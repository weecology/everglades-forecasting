# =============================================================================
# PLOTTING.R - Memory-efficient plotting for disk-backed forecasts
#
# Expected results structure:
#   results$mvgam$metrics
#   results$fable$metrics
#   results$<framework>$window_objects
#
# Each disk-backed window record contains:
#   train_file, test_file, model_files, test_start, test_end
#
# Raw forecast files are loaded ONE MODEL AT A TIME.
# Native plots are written beside each saved forecast file:
#   windows/<location>_<year>/<framework>/<model>/plots/
#
# Skill plots are split into batches of series.
# No model fitting or regional interval aggregation is performed here.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# GENERAL HELPERS
# =============================================================================

plot_make_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(path)) {
    stop("Could not create directory: ", path)
  }
  
  invisible(path)
}

plot_safe_name <- function(x) {
  gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
}

plot_has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0L
}

plot_attempt <- function(label, expr) {
  tryCatch(
    force(expr),
    error = function(e) {
      warning(
        label, ": ", conditionMessage(e),
        call. = FALSE
      )
      invisible(NULL)
    }
  )
}

plot_file_valid <- function(path) {
  if (!file.exists(path)) return(FALSE)
  
  size <- file.info(path)$size
  !is.na(size) && size > 0
}

plot_batch_filename <- function(stem, batch, total_batches) {
  if (total_batches == 1L) {
    paste0(stem, ".png")
  } else {
    sprintf("%s_batch_%03d.png", stem, batch)
  }
}

# =============================================================================
# WINDOW RECORDS AND DISK LOADING
# =============================================================================

get_plot_windows <- function(results, framework) {
  x <- results[[framework]]
  
  if (!is.null(x) && length(x$window_objects)) {
    return(x$window_objects)
  }
  
  # Fallback for results retained only under by_region.
  windows <- list()
  
  for (region_name in names(results$by_region %||% list())) {
    regional <- results$by_region[[region_name]][[framework]]
    
    for (window_name in names(regional$window_objects %||% list())) {
      record <- regional$window_objects[[window_name]]
      record$region <- region_name
      
      key <- paste(region_name, window_name, sep = "::")
      windows[[key]] <- record
    }
  }
  
  windows
}

plot_resolve_file <- function(results_dir, relative_file) {
  if (is.null(relative_file) ||
      length(relative_file) != 1L ||
      is.na(relative_file) ||
      !nzchar(relative_file)) {
    stop("Missing saved-file path in window metadata.")
  }
  
  filename <- file.path(results_dir, relative_file)
  
  if (!file.exists(filename)) {
    stop("Saved file not found: ", filename)
  }
  
  filename
}

# =============================================================================
# NATIVE MVGAM PLOTTING
# =============================================================================

plot_mvgam_labels <- function(fc) {
  labels <- as.character(fc$series_names)
  
  if (!length(labels)) {
    labels <- names(fc$forecasts)
  }
  
  if (!length(labels)) {
    stop("Cannot identify series in the mvgam forecast object.")
  }
  
  labels
}

plot_mvgam_image_names <- function(labels) {
  vapply(seq_along(labels), function(i) {
    sprintf(
      "series_%03d_%s.png",
      i,
      plot_safe_name(labels[i])
    )
  }, character(1))
}

plot_saved_mvgam_complete <- function(directory) {
  manifest_file <- file.path(directory, "manifest.csv")
  
  if (!file.exists(manifest_file)) return(FALSE)
  
  previous <- tryCatch(
    utils::read.csv(manifest_file),
    error = function(e) NULL
  )
  
  if (is.null(previous) ||
      !nrow(previous) ||
      !all(c("filename", "success") %in% names(previous))) {
    return(FALSE)
  }
  
  all(previous$success %in% TRUE) &&
    all(vapply(
      file.path(directory, previous$filename),
      plot_file_valid,
      logical(1)
    ))
}

plot_draw_mvgam <- function(fc, series_index, filename) {
  if (!requireNamespace("mvgam", quietly = TRUE)) {
    stop("The mvgam package is required.")
  }
  
  grDevices::png(
    filename,
    width = 1600,
    height = 900,
    res = 150
  )
  
  device <- grDevices::dev.cur()
  
  on.exit({
    devices <- grDevices::dev.list()
    
    if (!is.null(devices) && device %in% devices) {
      grDevices::dev.off(which = device)
    }
  }, add = TRUE)
  
  # Native S3 plotting method for mvgam_forecast.
  # Uses the saved fitted-period draws, forecast draws, and observations.
  graphics::plot(fc, series = series_index)
  
  invisible(NULL)
}

plot_one_mvgam_file <- function(filename, overwrite = FALSE) {
  directory <- file.path(dirname(filename), "plots")
  plot_make_dir(directory)
  
  # Avoid loading posterior arrays when previous plotting succeeded.
  if (!isTRUE(overwrite) && plot_saved_mvgam_complete(directory)) {
    return("existing")
  }
  
  # Local function scope: this object is released when the function returns.
  fc <- readRDS(filename)
  
  if (!inherits(fc, "mvgam_forecast")) {
    stop("Saved object is not an mvgam_forecast: ", filename)
  }
  
  labels <- plot_mvgam_labels(fc)
  image_names <- plot_mvgam_image_names(labels)
  rows <- vector("list", length(labels))
  
  for (i in seq_along(labels)) {
    image <- file.path(directory, image_names[i])
    error_text <- NA_character_
    
    success <- tryCatch({
      # Regenerate when no complete successful manifest was found.
      # This also retries series that failed in an earlier plotting pass.
      plot_draw_mvgam(fc, i, image)
      TRUE
    }, error = function(e) {
      error_text <<- conditionMessage(e)
      
      if (file.exists(image)) unlink(image)
      FALSE
    })
    
    rows[[i]] <- tibble::tibble(
      series = labels[i],
      series_index = i,
      filename = image_names[i],
      success = success,
      error = error_text
    )
  }
  
  manifest <- dplyr::bind_rows(rows)
  
  utils::write.csv(
    manifest,
    file.path(directory, "manifest.csv"),
    row.names = FALSE
  )
  
  if (all(manifest$success)) "saved" else "partial_or_failed"
}

# =============================================================================
# NATIVE FABLE PLOTTING
# =============================================================================

plot_one_fable_file <- function(filename, record, results_dir,
                                levels = c(80, 95),
                                overwrite = FALSE) {
  if (!requireNamespace("fabletools", quietly = TRUE) ||
      !requireNamespace("tsibble", quietly = TRUE)) {
    stop("fabletools and tsibble are required for fable plotting.")
  }
  
  directory <- file.path(dirname(filename), "plots")
  plot_make_dir(directory)
  
  image <- file.path(directory, "forecast.png")
  
  if (!isTRUE(overwrite) && plot_file_valid(image)) {
    return("existing")
  }
  
  fc <- readRDS(filename)
  
  if (!inherits(fc, "fbl_ts")) {
    stop("Saved object is not a native fable: ", filename)
  }
  
  train <- readRDS(
    plot_resolve_file(results_dir, record$train_file)
  )
  
  test <- readRDS(
    plot_resolve_file(results_dir, record$test_file)
  )
  
  keys <- setdiff(tsibble::key_vars(fc), ".model")
  
  if (!all(keys %in% names(train))) {
    stop("Saved training data does not match the fable's series keys.")
  }
  
  history <- tsibble::as_tsibble(
    tibble::as_tibble(train),
    key = tidyselect::all_of(keys),
    index = year
  )
  
  # Native fable plotting retains the forecast distributions.
  p <- ggplot2::autoplot(
    fc,
    history,
    level = levels
  ) +
    ggplot2::geom_point(
      data = tibble::as_tibble(test),
      mapping = ggplot2::aes(x = year, y = count),
      inherit.aes = FALSE,
      colour = "black",
      size = 1.8
    ) +
    ggplot2::labs(
      subtitle = paste0(
        "Training: ", record$train_start, "-", record$train_end,
        " | Forecast: ", record$test_start, "-", record$test_end
      ),
      x = "Year",
      y = "Count"
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  
  tryCatch(
    ggplot2::ggsave(
      image,
      plot = p,
      width = 12,
      height = 8,
      dpi = 150
    ),
    error = function(e) {
      if (file.exists(image)) unlink(image)
      stop(e)
    }
  )
  
  "saved"
}

# =============================================================================
# DISK-BACKED FORECAST ORCHESTRATION
# =============================================================================

replot_disk_windows <- function(results, framework,
                                results_dir = "results",
                                levels = c(80, 95),
                                overwrite = FALSE) {
  windows <- get_plot_windows(results, framework)
  
  if (!length(windows)) {
    message("No saved window records for ", framework, ".")
    return(invisible(NULL))
  }
  
  plot_make_dir(results_dir)
  rows <- list()
  
  for (window_key in names(windows)) {
    record <- windows[[window_key]]
    files <- record$model_files %||% list()
    
    if (!length(files)) {
      warning(
        window_key,
        ": no model_files found. This function expects disk-backed results.",
        call. = FALSE
      )
      next
    }
    
    for (model_name in names(files)) {
      error_text <- NA_character_
      
      status <- tryCatch({
        filename <- plot_resolve_file(
          results_dir,
          files[[model_name]]
        )
        
        if (framework == "mvgam") {
          plot_one_mvgam_file(
            filename,
            overwrite = overwrite
          )
        } else {
          plot_one_fable_file(
            filename,
            record,
            results_dir,
            levels = levels,
            overwrite = overwrite
          )
        }
      }, error = function(e) {
        error_text <<- conditionMessage(e)
        
        warning(
          window_key, " / ", model_name, ": ", error_text,
          call. = FALSE
        )
        
        "failed"
      })
      
      rows[[length(rows) + 1L]] <- tibble::tibble(
        window_key = window_key,
        location = record$location %||% record$region %||% "joint",
        framework = framework,
        model = model_name,
        test_start = record$test_start,
        test_end = record$test_end,
        forecast_file = files[[model_name]],
        status = status,
        error = error_text
      )
      
      # Raw forecast and plot objects were local to plot_one_*().
      # Collect them before loading the next saved forecast.
      gc(verbose = FALSE)
    }
  }
  
  manifest <- dplyr::bind_rows(rows)
  
  if (nrow(manifest)) {
    utils::write.csv(
      manifest,
      file.path(
        results_dir,
        paste0(framework, "_replot_manifest.csv")
      ),
      row.names = FALSE
    )
  }
  
  invisible(manifest)
}

plot_mvgam_windows <- function(results, results_dir = "results",
                               overwrite = FALSE) {
  replot_disk_windows(
    results,
    framework = "mvgam",
    results_dir = results_dir,
    overwrite = overwrite
  )
}

plot_fable_windows <- function(results, results_dir = "results",
                               levels = c(80, 95),
                               overwrite = FALSE) {
  replot_disk_windows(
    results,
    framework = "fable",
    results_dir = results_dir,
    levels = levels,
    overwrite = overwrite
  )
}

# =============================================================================
# SKILL-PLOT DATA
# =============================================================================

prepare_skill_plot_data <- function(metrics, framework) {
  if (!plot_has_rows(metrics)) return(tibble::tibble())
  
  model_column <- if (framework == "mvgam") "model" else ".model"
  required <- c(model_column, "species", "test_start")
  
  if (!all(required %in% names(metrics))) {
    stop(
      "Missing metric columns: ",
      paste(setdiff(required, names(metrics)), collapse = ", ")
    )
  }
  
  skill_columns <- intersect(
    c("crps_skill", "rmse_skill", "rps_skill"),
    names(metrics)
  )
  
  if (!length(skill_columns)) return(tibble::tibble())
  
  df <- tibble::as_tibble(metrics)
  df$plot_model <- as.character(df[[model_column]])
  
  df$plot_series <- if ("region" %in% names(df)) {
    paste(df$species, df$region, sep = " / ")
  } else {
    as.character(df$species)
  }
  
  df |>
    dplyr::filter(plot_model != "baseline") |>
    dplyr::select(
      plot_model, plot_series, test_start,
      all_of(skill_columns)
    ) |>
    tidyr::pivot_longer(
      cols = all_of(skill_columns),
      names_to = "metric",
      values_to = "skill"
    ) |>
    dplyr::filter(is.finite(skill))
}

skill_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

# =============================================================================
# SKILL FIGURES - BOUNDED-SIZE BATCHES
# =============================================================================

save_skill_batch <- function(df, framework, results_dir,
                             batch, total_batches) {
  labels <- c(
    crps_skill = "CRPS Skill",
    rmse_skill = "RMSE Skill",
    rps_skill = "RPS Skill"
  )
  
  # Individual metrics.
  for (metric_name in unique(df$metric)) {
    part <- df |>
      dplyr::filter(metric == metric_name)
    
    p <- ggplot2::ggplot(
      part,
      ggplot2::aes(
        x = test_start,
        y = skill,
        colour = plot_model,
        group = plot_model
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = "grey45"
      ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_point(size = 1.7) +
      ggplot2::facet_wrap(~plot_series, scales = "free_y", ncol = 3) +
      ggplot2::labs(
        title = paste(labels[[metric_name]], "Over Time -", framework),
        x = "Test Start Year",
        y = labels[[metric_name]],
        colour = "Model"
      ) +
      skill_plot_theme()
    
    stem <- paste0(
      framework, "_",
      sub("_skill$", "", metric_name),
      "_skill_over_time"
    )
    
    ggplot2::ggsave(
      file.path(
        results_dir,
        plot_batch_filename(stem, batch, total_batches)
      ),
      plot = p,
      width = 12,
      height = 7,
      dpi = 150
    )
    
    rm(p, part)
  }
  
  # Combined metrics: each series has its own column.
  df$metric_label <- unname(labels[df$metric])
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = test_start,
      y = skill,
      colour = plot_model,
      group = plot_model
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey45"
    ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.3) +
    ggplot2::facet_wrap(
      ~metric_label + plot_series,
      scales = "free_y",
      ncol = 3
    ) +
    ggplot2::labs(
      title = paste("Baseline-relative skill -", framework),
      x = "Test Start Year",
      y = "Skill",
      colour = "Model"
    ) +
    skill_plot_theme()
  
  ggplot2::ggsave(
    file.path(
      results_dir,
      plot_batch_filename(
        paste0(framework, "_all_skills_over_time"),
        batch,
        total_batches
      )
    ),
    plot = p,
    width = 12,
    height = 10,
    dpi = 150
  )
  
  rm(p)
  
  # Best-model counts within each series, window, and metric.
  winners <- df |>
    dplyr::group_by(plot_series, test_start, metric_label) |>
    dplyr::slice_max(skill, n = 1, with_ties = TRUE) |>
    dplyr::ungroup() |>
    dplyr::count(
      plot_series, plot_model, metric_label,
      name = "wins"
    )
  
  p <- ggplot2::ggplot(
    winners,
    ggplot2::aes(
      x = plot_series,
      y = wins,
      fill = plot_model
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~metric_label, scales = "free_y") +
    ggplot2::labs(
      title = paste("Windows with highest skill -", framework),
      subtitle = "Baseline excluded; ties count for every tied model",
      x = "Series",
      y = "Window wins",
      fill = "Model"
    ) +
    skill_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  ggplot2::ggsave(
    file.path(
      results_dir,
      plot_batch_filename(
        paste0(framework, "_best_model_counts"),
        batch,
        total_batches
      )
    ),
    plot = p,
    width = 12,
    height = 7,
    dpi = 150
  )
  
  invisible(NULL)
}

plot_framework_skills <- function(metrics, framework,
                                  results_dir = "results",
                                  series_per_batch = 6L) {
  if (length(series_per_batch) != 1L ||
      !is.numeric(series_per_batch) ||
      !is.finite(series_per_batch) ||
      series_per_batch < 1 ||
      series_per_batch != floor(series_per_batch)) {
    stop("series_per_batch must be a positive integer.")
  }
  
  plot_make_dir(results_dir)
  df <- prepare_skill_plot_data(metrics, framework)
  
  if (!nrow(df)) {
    message("No finite non-baseline skill scores for ", framework, ".")
    return(invisible(NULL))
  }
  
  keys <- c("plot_model", "plot_series", "test_start", "metric")
  
  if (anyDuplicated(as.data.frame(df[keys]))) {
    stop(
      "Duplicate model/series/window/metric rows found. ",
      "Resolve duplicate accuracy rows before plotting."
    )
  }
  
  series <- sort(unique(df$plot_series))
  
  batches <- split(
    series,
    ceiling(seq_along(series) / as.integer(series_per_batch))
  )
  
  for (i in seq_along(batches)) {
    part <- df |>
      dplyr::filter(plot_series %in% batches[[i]])
    
    plot_attempt(
      paste(framework, "skill batch", i),
      save_skill_batch(
        part,
        framework,
        results_dir,
        batch = i,
        total_batches = length(batches)
      )
    )
    
    rm(part)
    gc(verbose = FALSE)
  }
  
  invisible(NULL)
}

plot_mvgam_results <- function(metrics, results_dir = "results",
                               series_per_batch = 6L) {
  plot_framework_skills(
    metrics,
    framework = "mvgam",
    results_dir = results_dir,
    series_per_batch = series_per_batch
  )
}

plot_fable_results <- function(metrics, results_dir = "results",
                               series_per_batch = 6L) {
  plot_framework_skills(
    metrics,
    framework = "fable",
    results_dir = results_dir,
    series_per_batch = series_per_batch
  )
}

# =============================================================================
# MAIN ENTRY POINT - COMPATIBLE WITH main.R
# =============================================================================

generate_plots <- function(results, config, data = NULL,
                           results_dir = "results") {
  plot_make_dir(results_dir)
  
  options <- config$plots %||% list()
  series_per_batch <- options$series_per_batch %||% 6L
  overwrite <- isTRUE(options$overwrite %||% FALSE)
  
  for (framework in c("mvgam", "fable")) {
    metrics <- results[[framework]]$metrics
    
    if (plot_has_rows(metrics)) {
      plot_attempt(
        paste(framework, "skill plotting"),
        plot_framework_skills(
          metrics,
          framework,
          results_dir,
          series_per_batch = series_per_batch
        )
      )
    }
  }
  
  # The latest main.R passes FALSE for both options below because
  # evaluation.R already creates native plots while fitting each window.
  #
  # Enable these options for a separate plotting-only regeneration pass.
  if (isTRUE(options$native_mvgam %||% FALSE)) {
    plot_attempt(
      "Native mvgam replotting",
      plot_mvgam_windows(
        results,
        results_dir,
        overwrite = overwrite
      )
    )
  }
  
  if (isTRUE(options$native_fable %||% FALSE)) {
    plot_attempt(
      "Native fable replotting",
      plot_fable_windows(
        results,
        results_dir,
        levels = options$interval_levels %||% c(80, 95),
        overwrite = overwrite
      )
    )
  }
  
  cat(
    "\nPlotting finished. Individual failures appear in warnings/manifests.\n",
    "Output directory: ", results_dir, "\n",
    sep = ""
  )
  
  invisible(NULL)
}

cat("✓ Memory-efficient disk-backed plotting.R loaded\n")