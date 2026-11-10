# =============================================================================
# PLOTTING.R
#
# Compatible with rewritten evaluation.R and main.R.
#
# Forecast inputs:
#   results$mvgam$window_objects
#   results$fable$window_objects
#
# Regional fallback:
#   results$by_region[[region]][[framework]]$window_objects
#
# Outputs:
#   forecasts_native/...     native mvgam window plots
#   fable_window_forecasts/... native fable autoplot figures
#   *_skill_over_time.png
#   *_all_skills_over_time.png
#   *_best_model_counts.png
#
# No model fitting is performed here.
# No prediction intervals are summed across regions.
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

plot_safe_name <- function(x) {
  gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
}

plot_make_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Could not create directory: ", path)
  invisible(path)
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

# =============================================================================
# RETRIEVE ORIGINAL PER-WINDOW FORECAST OBJECTS
# =============================================================================

get_plot_windows <- function(results, framework) {
  framework_result <- results[[framework]]
  
  if (!is.null(framework_result) &&
      length(framework_result$window_objects)) {
    return(framework_result$window_objects)
  }
  
  # Support results retaining raw objects only under by_region.
  windows <- list()
  
  for (region_name in names(results$by_region %||% list())) {
    regional <- results$by_region[[region_name]][[framework]]
    regional_windows <- regional$window_objects %||% list()
    
    for (window_name in names(regional_windows)) {
      record <- regional_windows[[window_name]]
      record$region <- region_name
      
      windows[[paste(region_name, window_name, sep = "::")]] <- record
    }
  }
  
  windows
}

plot_window_directory <- function(root, record, model_name) {
  file.path(
    root,
    plot_safe_name(record$region %||% "joint"),
    paste0(
      "window_", record$window,
      "_test_", record$test_start, "-", record$test_end
    ),
    plot_safe_name(model_name)
  )
}

plot_existing_image <- function(filename, overwrite) {
  if (isTRUE(overwrite) || !file.exists(filename)) return(FALSE)
  size <- file.info(filename)$size
  !is.na(size) && size > 0
}

# =============================================================================
# NATIVE MVGAM FORECAST PLOTS
# =============================================================================

draw_mvgam_native <- function(fc, series_index = 1L) {
  if (!requireNamespace("mvgam", quietly = TRUE)) {
    stop("Install/load mvgam to draw native forecast plots.")
  }
  
  ns <- asNamespace("mvgam")
  
  native_function <- if (exists(
    "plot_mvgam_forecast",
    envir = ns,
    inherits = FALSE
  )) {
    get(
      "plot_mvgam_forecast",
      envir = ns,
      inherits = FALSE
    )
  } else {
    NULL
  }
  
  if (is.function(native_function) &&
      "series" %in% names(formals(native_function))) {
    native_function(fc, series = series_index)
  } else {
    # Use the installed S3 plotting method.
    graphics::plot(fc, series = series_index)
  }
  
  invisible(NULL)
}

save_mvgam_native <- function(fc, series_index, filename,
                              width = 1600, height = 900, res = 150) {
  grDevices::png(
    filename,
    width = width,
    height = height,
    res = res
  )
  
  device_id <- grDevices::dev.cur()
  
  on.exit({
    devices <- grDevices::dev.list()
    
    if (!is.null(devices) && device_id %in% devices) {
      grDevices::dev.off(which = device_id)
    }
  }, add = TRUE)
  
  draw_mvgam_native(fc, series_index)
  invisible(filename)
}

mvgam_series_count <- function(fc, record) {
  # Prefer information from the actual forecast object.
  summary_df <- tryCatch(
    tibble::as_tibble(summary(fc)),
    error = function(e) NULL
  )
  
  if (!is.null(summary_df) && "series" %in% names(summary_df)) {
    labels <- unique(as.character(summary_df$series))
    labels <- labels[!is.na(labels)]
    
    if (length(labels)) return(length(labels))
  }
  
  # Fallback to the exact training-series combinations.
  train <- record$train_data
  keys <- intersect(c("species", "region"), names(train))
  
  if (!length(keys) || !plot_has_rows(train)) {
    stop("Cannot determine the number of forecast series.")
  }
  
  nrow(dplyr::distinct(train, across(all_of(keys))))
}

plot_mvgam_windows <- function(results, results_dir = "results",
                               overwrite = FALSE) {
  windows <- get_plot_windows(results, "mvgam")
  
  if (!length(windows)) {
    message(
      "No raw mvgam window objects available. ",
      "Native forecast plots require results from the rewritten evaluation.R."
    )
    return(invisible(NULL))
  }
  
  root <- file.path(results_dir, "forecasts_native")
  plot_make_dir(root)
  
  manifest <- list()
  
  for (window_key in names(windows)) {
    record <- windows[[window_key]]
    
    for (model_name in names(record$models %||% list())) {
      fc <- record$models[[model_name]]
      if (is.null(fc)) next
      
      n_series <- tryCatch(
        mvgam_series_count(fc, record),
        error = function(e) {
          warning(
            window_key, " / ", model_name, ": ",
            conditionMessage(e),
            call. = FALSE
          )
          0L
        }
      )
      
      destination <- plot_window_directory(root, record, model_name)
      plot_make_dir(destination)
      
      for (series_index in seq_len(n_series)) {
        filename <- file.path(
          destination,
          sprintf("series_%03d.png", series_index)
        )
        
        status <- "existing"
        error_text <- NA_character_
        
        if (!plot_existing_image(filename, overwrite)) {
          status <- tryCatch({
            save_mvgam_native(fc, series_index, filename)
            "saved"
          }, error = function(e) {
            error_text <<- conditionMessage(e)
            if (file.exists(filename)) unlink(filename)
            
            warning(
              "Native mvgam plot failed: ", window_key, " / ",
              model_name, " / series ", series_index, ": ",
              error_text,
              call. = FALSE
            )
            
            "failed"
          })
        }
        
        manifest[[length(manifest) + 1L]] <- tibble::tibble(
          window_key = window_key,
          region = as.character(record$region %||% "joint"),
          model = model_name,
          series_index = series_index,
          test_start = record$test_start,
          test_end = record$test_end,
          filename = filename,
          status = status,
          error = error_text
        )
      }
    }
  }
  
  manifest <- dplyr::bind_rows(manifest)
  
  if (nrow(manifest)) {
    utils::write.csv(
      manifest,
      file.path(root, "plotting_manifest.csv"),
      row.names = FALSE
    )
  }
  
  invisible(manifest)
}

# =============================================================================
# NATIVE FABLE FORECAST PLOTS
# =============================================================================

plot_fable_windows <- function(results, results_dir = "results",
                               levels = c(80, 95),
                               overwrite = FALSE) {
  if (!requireNamespace("fabletools", quietly = TRUE) ||
      !requireNamespace("tsibble", quietly = TRUE)) {
    stop("fabletools and tsibble are required for fable plotting.")
  }
  
  windows <- get_plot_windows(results, "fable")
  
  if (!length(windows)) {
    message(
      "No raw fable window objects available. ",
      "Use results from the rewritten evaluation.R."
    )
    return(invisible(NULL))
  }
  
  root <- file.path(results_dir, "fable_window_forecasts")
  plot_make_dir(root)
  
  manifest <- list()
  
  for (window_key in names(windows)) {
    record <- windows[[window_key]]
    fc <- record$models$fable
    
    if (is.null(fc) || !nrow(fc)) next
    
    if (!inherits(fc, "fbl_ts")) {
      warning(
        window_key,
        ": saved forecast is not a native fable; skipping.",
        call. = FALSE
      )
      next
    }
    
    keys <- tsibble::key_vars(fc)
    series_keys <- setdiff(keys, ".model")
    
    if (!all(series_keys %in% names(record$train_data))) {
      warning(
        window_key, ": training data does not match fable keys.",
        call. = FALSE
      )
      next
    }
    
    # Reconstruct the training tsibble with the forecast's series keys.
    history <- tsibble::as_tsibble(
      tibble::as_tibble(record$train_data),
      key = tidyselect::all_of(series_keys),
      index = year
    )
    
    combinations <- tibble::as_tibble(fc) |>
      dplyr::select(all_of(c(".model", series_keys))) |>
      dplyr::distinct()
    
    for (i in seq_len(nrow(combinations))) {
      selected <- combinations[i, , drop = FALSE]
      model_name <- as.character(selected$.model)
      
      # Filter without converting fc to a plain tibble.
      keep <- rep(TRUE, nrow(fc))
      keep_history <- rep(TRUE, nrow(history))
      
      for (key in names(selected)) {
        keep <- keep &
          as.character(fc[[key]]) == as.character(selected[[key]][1])
      }
      
      for (key in series_keys) {
        keep_history <- keep_history &
          as.character(history[[key]]) ==
          as.character(selected[[key]][1])
      }
      
      fc_one <- fc[keep, ]
      history_one <- history[keep_history, ]
      
      label <- paste(
        vapply(
          series_keys,
          function(key) as.character(selected[[key]][1]),
          character(1)
        ),
        collapse = "_"
      )
      
      if (!nzchar(label)) label <- "series"
      
      destination <- plot_window_directory(root, record, model_name)
      plot_make_dir(destination)
      
      filename <- file.path(
        destination,
        paste0(plot_safe_name(label), ".png")
      )
      
      status <- "existing"
      error_text <- NA_character_
      
      if (!plot_existing_image(filename, overwrite)) {
        status <- tryCatch({
          p <- ggplot2::autoplot(
            fc_one,
            history_one,
            level = levels
          ) +
            ggplot2::labs(
              title = paste(label, "\u2014", model_name),
              subtitle = paste0(
                "Training: ", record$train_start, "\u2013",
                record$train_end,
                " | Forecast: ", record$test_start, "\u2013",
                record$test_end
              ),
              x = "Year",
              y = "Count"
            ) +
            ggplot2::theme_classic(base_size = 13) +
            ggplot2::theme(legend.position = "bottom")
          
          # Overlay held-out observations only within this test window.
          observations <- tibble::as_tibble(record$test_data)
          keep_observations <- rep(TRUE, nrow(observations))
          
          for (key in series_keys) {
            keep_observations <- keep_observations &
              as.character(observations[[key]]) ==
              as.character(selected[[key]][1])
          }
          
          observations <- observations[keep_observations, ]
          
          if (nrow(observations)) {
            p <- p +
              ggplot2::geom_point(
                data = observations,
                mapping = ggplot2::aes(x = year, y = count),
                inherit.aes = FALSE,
                colour = "black",
                size = 2
              )
          }
          
          ggplot2::ggsave(
            filename,
            plot = p,
            width = 10,
            height = 5.5,
            dpi = 150
          )
          
          "saved"
        }, error = function(e) {
          error_text <<- conditionMessage(e)
          if (file.exists(filename)) unlink(filename)
          
          warning(
            "Fable plot failed: ", window_key, " / ",
            model_name, " / ", label, ": ", error_text,
            call. = FALSE
          )
          
          "failed"
        })
      }
      
      manifest[[length(manifest) + 1L]] <- tibble::tibble(
        window_key = window_key,
        model = model_name,
        series = label,
        filename = filename,
        status = status,
        error = error_text
      )
    }
  }
  
  manifest <- dplyr::bind_rows(manifest)
  
  if (nrow(manifest)) {
    utils::write.csv(
      manifest,
      file.path(root, "plot_manifest.csv"),
      row.names = FALSE
    )
  }
  
  invisible(manifest)
}

# =============================================================================
# SKILL PLOTS: COMMON IMPLEMENTATION FOR BOTH FRAMEWORKS
# =============================================================================

prepare_skill_plot_data <- function(metrics, framework) {
  if (!plot_has_rows(metrics)) return(tibble::tibble())
  
  model_col <- if (framework == "mvgam") "model" else ".model"
  required <- c(model_col, "species", "test_start")
  
  if (!all(required %in% names(metrics))) {
    stop(
      "Metrics missing columns: ",
      paste(setdiff(required, names(metrics)), collapse = ", ")
    )
  }
  
  skill_cols <- intersect(
    c("crps_skill", "rmse_skill", "rps_skill"),
    names(metrics)
  )
  
  if (!length(skill_cols)) return(tibble::tibble())
  
  df <- tibble::as_tibble(metrics)
  df$plot_model <- as.character(df[[model_col]])
  
  # Preserve spatial units instead of averaging different regions together.
  df$plot_region <- if ("region" %in% names(df)) {
    as.character(df$region)
  } else {
    "system"
  }
  
  df$plot_series <- if ("region" %in% names(df)) {
    paste(df$species, df$plot_region, sep = " / ")
  } else {
    as.character(df$species)
  }
  
  df |>
    dplyr::filter(plot_model != "baseline") |>
    dplyr::select(
      plot_model, species, plot_region, plot_series,
      test_start, all_of(skill_cols)
    ) |>
    tidyr::pivot_longer(
      cols = all_of(skill_cols),
      names_to = "metric",
      values_to = "skill"
    ) |>
    dplyr::filter(is.finite(skill))
}

skill_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_framework_skills <- function(metrics, framework,
                                  results_dir = "results") {
  plot_make_dir(results_dir)
  df <- prepare_skill_plot_data(metrics, framework)
  
  if (!nrow(df)) {
    message("No finite non-baseline skill scores for ", framework, ".")
    return(invisible(NULL))
  }
  
  # Prevent accidental duplicate metric rows from being plotted as one line.
  uniqueness_keys <- c(
    "plot_model", "plot_series", "test_start", "metric"
  )
  
  if (anyDuplicated(as.data.frame(df[uniqueness_keys]))) {
    stop(
      "Duplicate model/series/window/metric rows found. ",
      "Resolve duplicate accuracy rows before plotting."
    )
  }
  
  labels <- c(
    crps_skill = "CRPS Skill",
    rmse_skill = "RMSE Skill",
    rps_skill = "RPS Skill"
  )
  
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
      ggplot2::geom_point(size = 1.8) +
      ggplot2::facet_wrap(~plot_series, scales = "free_y") +
      ggplot2::labs(
        title = paste(labels[[metric_name]], "Over Time", "-", framework),
        x = "Test Start Year",
        y = labels[[metric_name]],
        colour = "Model"
      ) +
      skill_plot_theme()
    
    filename <- paste0(
      framework, "_",
      sub("_skill$", "", metric_name),
      "_skill_over_time.png"
    )
    
    ggplot2::ggsave(
      file.path(results_dir, filename),
      plot = p,
      width = 12,
      height = max(6, 2.2 * ceiling(length(unique(part$plot_series)) / 3)),
      dpi = 150,
      limitsize = FALSE
    )
  }
  
  # Combined metrics plot.
  p_combined <- ggplot2::ggplot(
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
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(
      ~metric + plot_series,
      scales = "free_y",
      ncol = 3
    ) +
    ggplot2::labs(
      title = paste("Baseline-relative skill", "-", framework),
      x = "Test Start Year",
      y = "Skill",
      colour = "Model"
    ) +
    skill_plot_theme()
  
  panel_count <- nrow(
    dplyr::distinct(df, metric, plot_series)
  )
  
  ggplot2::ggsave(
    file.path(results_dir, paste0(framework, "_all_skills_over_time.png")),
    plot = p_combined,
    width = 13,
    height = max(6, 2.2 * ceiling(panel_count / 3)),
    dpi = 150,
    limitsize = FALSE
  )
  
  # Winners within each spatial series, test window, and metric.
  # Ties count as wins for every tied model.
  winners <- df |>
    dplyr::group_by(plot_series, test_start, metric) |>
    dplyr::slice_max(skill, n = 1, with_ties = TRUE) |>
    dplyr::ungroup() |>
    dplyr::count(plot_series, plot_model, metric, name = "wins")
  
  p_winners <- ggplot2::ggplot(
    winners,
    ggplot2::aes(
      x = plot_series,
      y = wins,
      fill = plot_model
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::labs(
      title = paste("Windows with highest skill", "-", framework),
      subtitle = "Baseline excluded; ties count for each tied model",
      x = "Series",
      y = "Window wins",
      fill = "Model"
    ) +
    skill_plot_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  ggplot2::ggsave(
    file.path(results_dir, paste0(framework, "_best_model_counts.png")),
    plot = p_winners,
    width = 13,
    height = 7,
    dpi = 150
  )
  
  invisible(df)
}

# Preserve these entry points for existing callers.
plot_mvgam_results <- function(metrics, results_dir = "results") {
  plot_framework_skills(metrics, "mvgam", results_dir)
}

plot_fable_results <- function(metrics, results_dir = "results") {
  plot_framework_skills(metrics, "fable", results_dir)
}

# =============================================================================
# MAIN ENTRY POINT: COMPATIBLE WITH main.R
# =============================================================================

generate_plots <- function(results, config, data = NULL,
                           results_dir = "results") {
  plot_make_dir(results_dir)
  
  options <- config$plots %||% list()
  overwrite <- isTRUE(options$overwrite %||% FALSE)
  
  outputs <- list()
  
  if (plot_has_rows(results$mvgam$metrics)) {
    outputs$mvgam_skills <- plot_attempt(
      "mvgam skill plotting",
      plot_mvgam_results(results$mvgam$metrics, results_dir)
    )
  }
  
  if (plot_has_rows(results$fable$metrics)) {
    outputs$fable_skills <- plot_attempt(
      "fable skill plotting",
      plot_fable_results(results$fable$metrics, results_dir)
    )
  }
  
  if (isTRUE(options$native_mvgam %||% TRUE) &&
      length(get_plot_windows(results, "mvgam"))) {
    outputs$mvgam_forecasts <- plot_attempt(
      "Native mvgam window plotting",
      plot_mvgam_windows(
        results,
        results_dir = results_dir,
        overwrite = overwrite
      )
    )
  }
  
  if (isTRUE(options$native_fable %||% TRUE) &&
      length(get_plot_windows(results, "fable"))) {
    outputs$fable_forecasts <- plot_attempt(
      "Native fable window plotting",
      plot_fable_windows(
        results,
        results_dir = results_dir,
        levels = options$interval_levels %||% c(80, 95),
        overwrite = overwrite
      )
    )
  }
  
  for (framework in c("mvgam", "fable")) {
    if (plot_has_rows(results[[framework]]$forecasts) &&
        !length(get_plot_windows(results, framework))) {
      message(
        framework,
        ": summarised forecasts exist, but original window objects are missing. ",
        "Native window plots were not generated."
      )
    }
  }
  
  cat(
    "\nPlotting finished. Check manifests for individual failures.\n",
    "Output directory: ", results_dir, "\n",
    sep = ""
  )
  
  invisible(outputs)
}

cat("✓ plotting.R loaded\n")