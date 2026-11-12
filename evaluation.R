# =============================================================================
# EVALUATION.R - Memory-efficient, disk-backed cross-validation
#
# Data flow:
#   data -> annual window -> save inputs -> fit model -> save raw forecast
#        -> native plot -> small predictions/metrics -> release raw object
#
# Returned outputs:
#   forecasts      : small tidy prediction table
#   metrics        : small metric table
#   cv_info        : CV settings
#   window_objects : lightweight metadata and relative file paths ONLY
#
# Model functions remain unchanged:
#   list(fc = fc, crps = crps)
#
# Windows are sequential. Outer model parallelism remains the caller's choice.
# =============================================================================

library(dplyr)
library(tidyr)
library(distributional)
library(future)

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# COMMON UTILITIES
# =============================================================================

guard_config_init <- function() {
  if (!exists("CONFIG", inherits = TRUE)) {
    assign("CONFIG", config::get(), envir = .GlobalEnv)
  }
  invisible(NULL)
}

get_evaluation_config <- function() {
  if (!exists("CONFIG", inherits = TRUE)) {
    stop("CONFIG must be initialized before evaluation.")
  }
  get("CONFIG", inherits = TRUE)
}

positive_integer <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) ||
      !is.finite(x) || x < 1 || x != floor(x)) {
    stop(name, " must be a positive integer.")
  }
  as.integer(x)
}

finite_mean <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  # Do not silently discard infinite errors/scores.
  mean(x, na.rm = TRUE)
}

safe_skill <- function(value, baseline) {
  out <- rep(NA_real_, length(value))
  valid <- is.finite(value) & is.finite(baseline) & baseline > 0
  out[valid] <- 1 - value[valid] / baseline[valid]
  out
}

evaluation_keys <- function(df) {
  c("species", intersect("region", names(df)))
}

assert_unique_rows <- function(df, keys, label) {
  if (!all(keys %in% names(df))) {
    stop(label, " is missing required keys.")
  }
  if (anyDuplicated(as.data.frame(df[keys]))) {
    stop(label, " has duplicate rows for: ", paste(keys, collapse = ", "))
  }
  invisible(NULL)
}

add_baseline_skill <- function(df, model_col, keys, metric) {
  if (!nrow(df)) return(df)
  
  baseline_col <- paste0(metric, "_baseline")
  skill_col <- paste0(metric, "_skill")
  
  df <- df |> dplyr::select(-any_of(c(baseline_col, skill_col)))
  
  baseline <- df |>
    dplyr::filter(.data[[model_col]] == "baseline") |>
    dplyr::select(all_of(c(keys, metric)))
  
  assert_unique_rows(baseline, keys, paste(metric, "baseline"))
  names(baseline)[names(baseline) == metric] <- baseline_col
  
  df <- dplyr::left_join(df, baseline, by = keys)
  df[[skill_col]] <- safe_skill(df[[metric]], df[[baseline_col]])
  df
}

empty_window_result <- function() {
  list(
    predictions = tibble(),
    metrics = tibble(),
    model_files = list(),
    errors = character()
  )
}

disk_safe_name <- function(x) {
  gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
}

disk_create_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Could not create directory: ", path)
  invisible(path)
}

# Write through a temporary file; do not silently continue after save failure.
disk_save_rds <- function(object, path) {
  disk_create_dir(dirname(path))
  temporary <- tempfile(pattern = ".writing_", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  
  saveRDS(object, temporary, compress = TRUE)
  
  if (file.exists(path) && !file.remove(path)) {
    stop("Could not replace: ", path)
  }
  if (!file.rename(temporary, path)) {
    stop("Could not finish saving: ", path)
  }
  invisible(path)
}

evaluation_output_dir <- function(config) {
  path <- config$output_dir
  
  if (is.null(path) &&
      exists("run_folder", envir = .GlobalEnv, inherits = FALSE)) {
    path <- get("run_folder", envir = .GlobalEnv, inherits = FALSE)
  }
  
  if (is.null(path)) {
    stop(
      "Set CONFIG$output_dir to the current run directory ",
      "before calling fit_sliding_window()."
    )
  }
  
  disk_create_dir(path)
  path
}

# =============================================================================
# PARALLEL UTILITIES
# =============================================================================

setup_parallel <- function(enabled = TRUE, workers = NULL) {
  if (!isTRUE(enabled)) {
    future::plan(future::sequential)
    return(list(enabled = FALSE, workers = 1L))
  }
  
  if (is.null(workers)) {
    workers <- max(1L, as.integer(parallelly::availableCores()) - 1L)
  }
  
  workers <- positive_integer(workers, "workers")
  future::plan(future::multisession, workers = workers)
  list(enabled = TRUE, workers = workers)
}

teardown_parallel <- function() {
  future::plan(future::sequential)
  invisible(NULL)
}

# =============================================================================
# NATIVE PLOTTING - ONE RAW OBJECT AT A TIME
# =============================================================================

save_native_mvgam_object <- function(fc, directory) {
  disk_create_dir(directory)
  
  labels <- as.character(fc$series_names)
  if (!length(labels)) labels <- names(fc$forecasts)
  if (!length(labels)) stop("Cannot identify mvgam forecast series.")
  
  draw_one <- function(index, filename) {
    grDevices::png(filename, width = 1600, height = 900, res = 150)
    device <- grDevices::dev.cur()
    
    on.exit({
      devices <- grDevices::dev.list()
      if (!is.null(devices) && device %in% devices) {
        grDevices::dev.off(which = device)
      }
    }, add = TRUE)
    
    # Native S3 method for the mvgam_forecast object.
    # Uses its historical draws, forecast draws, and observations.
    graphics::plot(fc, series = index)
    invisible(NULL)
  }
  
  rows <- lapply(seq_along(labels), function(index) {
    filename <- file.path(
      directory,
      sprintf(
        "series_%03d_%s.png",
        index,
        disk_safe_name(labels[index])
      )
    )
    
    error <- NA_character_
    success <- tryCatch({
      draw_one(index, filename)
      TRUE
    }, error = function(e) {
      error <<- conditionMessage(e)
      if (file.exists(filename)) unlink(filename)
      FALSE
    })
    
    tibble(
      series = labels[index],
      series_index = index,
      filename = basename(filename),
      success = success,
      error = error
    )
  })
  
  utils::write.csv(
    dplyr::bind_rows(rows),
    file.path(directory, "manifest.csv"),
    row.names = FALSE
  )
  
  invisible(NULL)
}

save_native_fable_object <- function(fc, train_data, test_data,
                                     directory, levels = c(80, 95)) {
  disk_create_dir(directory)
  
  if (!inherits(fc, "fbl_ts")) stop("Expected a native fable object.")
  
  keys <- setdiff(tsibble::key_vars(fc), ".model")
  
  history <- tsibble::as_tsibble(
    tibble::as_tibble(train_data),
    key = tidyselect::all_of(keys),
    index = year
  )
  
  p <- ggplot2::autoplot(fc, history, level = levels) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "Year", y = "Count")
  
  # Retain the original key columns for native faceting.
  p <- p + ggplot2::geom_point(
    data = tibble::as_tibble(test_data),
    mapping = ggplot2::aes(x = year, y = count),
    inherit.aes = FALSE,
    colour = "black",
    size = 1.8
  )
  
  ggplot2::ggsave(
    file.path(directory, "forecast.png"),
    plot = p,
    width = 12,
    height = 8,
    dpi = 150
  )
  
  invisible(NULL)
}

# =============================================================================
# SLIDING-WINDOW CROSS-VALIDATION
# =============================================================================

fit_sliding_window <- function(data,
                               make_forecast,
                               train_years,
                               test_years,
                               cv_windows = NULL,
                               parallel = FALSE,
                               workers = NULL,
                               framework = NULL,
                               ...) {
  dots <- list(...)
  config <- dots$config %||% get_evaluation_config()
  
  train_years <- positive_integer(train_years, "train_years")
  test_years <- positive_integer(test_years, "test_years")
  root <- evaluation_output_dir(config)
  
  if (!is.data.frame(data) || !nrow(data) || !"year" %in% names(data)) {
    stop("data must contain observations and a year column.")
  }
  
  years <- sort(unique(data$year))
  
  if (anyNA(years) || !is.numeric(years) ||
      any(!is.finite(years)) || any(years != floor(years))) {
    stop("year must contain finite, non-missing integer years.")
  }
  
  last_start <- max(years) - train_years - test_years + 1L
  
  if (last_start < min(years)) {
    stop("Insufficient calendar years for this CV configuration.")
  }
  
  starts <- seq.int(min(years), last_start)
  
  if (!is.null(cv_windows)) {
    starts <- tail(starts, positive_integer(cv_windows, "cv_windows"))
  }
  
  if (is.null(framework)) {
    framework <- if (identical(make_forecast, make_mvgam_forecasts)) {
      "mvgam"
    } else if (exists("make_fable_forecasts", mode = "function") &&
               identical(make_forecast, get("make_fable_forecasts"))) {
      "fable"
    } else {
      stop("For a custom forecast wrapper, supply framework='mvgam' or 'fable'.")
    }
  }
  
  if (!framework %in% c("mvgam", "fable")) stop("Unknown framework.")
  
  # CV remains sequential and does not alter the caller's future plan.
  if (isTRUE(parallel)) {
    message("Windows are sequential to limit peak memory.")
  }
  
  regions <- unique(as.character(data$region))
  regions <- regions[!is.na(regions)]
  
  location <- if (length(regions) == 1L) {
    regions
  } else if (length(regions) > 1L) {
    paste0(config$spatial$level %||% "spatial", "_joint")
  } else {
    "system"
  }
  
  forecast_parts <- vector("list", length(starts))
  metric_parts <- vector("list", length(starts))
  records <- vector("list", length(starts))
  
  for (i in seq_along(starts)) {
    train_start <- starts[i]
    test_start <- train_start + train_years
    test_end <- test_start + test_years - 1L
    
    relative_directory <- file.path(
      "windows",
      paste0(disk_safe_name(location), "_", test_start)
    )
    
    window_directory <- file.path(root, relative_directory)
    framework_directory <- file.path(window_directory, framework)
    disk_create_dir(framework_directory)
    
    train <- data |>
      tibble::as_tibble() |>
      dplyr::filter(year >= train_start, year < test_start)
    
    test <- data |>
      tibble::as_tibble() |>
      dplyr::filter(year >= test_start, year <= test_end)
    
    if (!setequal(unique(train$year), seq.int(train_start, test_start - 1L)) ||
        !setequal(unique(test$year), seq.int(test_start, test_end))) {
      stop("Missing calendar years in window ", location, "_", test_start)
    }
    
    # Save the exact inputs before fitting.
    disk_save_rds(train, file.path(window_directory, "train_data.rds"))
    disk_save_rds(test, file.path(window_directory, "test_data.rds"))
    
    # Read saved inputs: these are the inputs actually passed to models.
    rm(train, test)
    
    train <- readRDS(file.path(window_directory, "train_data.rds"))
    test <- readRDS(file.path(window_directory, "test_data.rds"))
    
    window_config <- config
    window_config$.window_dir <- framework_directory
    window_config$.window_relative_dir <- file.path(
      relative_directory, framework
    )
    
    args <- dots
    args$config <- window_config
    
    cat(
      "\nWindow", i, "/", length(starts), ":",
      location, "| Train", train_start, "-", test_start - 1L,
      "| Test", test_start, "-", test_end, "\n"
    )
    
    error_text <- NULL
    
    # Persistence failures are fatal: never continue silently without raw files.
    result <- tryCatch(
      do.call(make_forecast, c(list(train, test), args)),
      error = function(e) {
        if (inherits(e, "forecast_disk_error")) stop(e)
        error_text <<- conditionMessage(e)
        warning("Window failed: ", error_text, call. = FALSE)
        empty_window_result()
      }
    )
    
    if (is.null(result) || length(result) < 2L) {
      error_text <- error_text %||% "Forecast function returned no result."
      result <- empty_window_result()
    }
    
    fc_table <- result[[1]]
    metrics <- result[[2]]
    model_files <- result$model_files %||% list()
    
    if (framework == "fable" &&
        is.data.frame(fc_table) && nrow(fc_table)) {
      raw_path <- file.path(framework_directory, "forecast.rds")
      
      disk_save_rds(fc_table, raw_path)
      
      model_files <- list(
        fable = file.path(relative_directory, framework, "forecast.rds")
      )
      
      if (isTRUE(config$plots$native_fable %||% TRUE)) {
        tryCatch(
          save_native_fable_object(
            fc_table, train, test,
            file.path(framework_directory, "plots"),
            config$plots$interval_levels %||% c(80, 95)
          ),
          error = function(e) {
            warning("Fable plot failed: ", conditionMessage(e), call. = FALSE)
          }
        )
      }
      
      metrics <- tryCatch(
        make_fable_evaluation(
          metrics, fc_table, test, train, window_config,
          args$precomputed_breaks,
          args$use_ordinal %||% FALSE
        ),
        error = function(e) {
          warning("Fable evaluation failed: ", conditionMessage(e), call. = FALSE)
          tibble()
        }
      )
    }
    
    # Avoid dplyr data-mask ambiguity between column and local variable names.
    tag_window <- function(x) {
      if (is.null(x) || !is.data.frame(x) || !nrow(x)) return(tibble())
      out <- tibble::as_tibble(x)
      out$test_start <- test_start
      out$window <- i
      out
    }
    
    forecast_parts[[i]] <- tag_window(fc_table)
    metric_parts[[i]] <- tag_window(metrics)
    
    record <- list(
      window = i,
      location = location,
      region = if (length(regions) == 1L) location else NULL,
      train_start = train_start,
      train_end = test_start - 1L,
      test_start = test_start,
      test_end = test_end,
      framework = framework,
      directory = relative_directory,
      train_file = file.path(relative_directory, "train_data.rds"),
      test_file = file.path(relative_directory, "test_data.rds"),
      model_files = model_files,
      model_names = names(model_files),
      error = error_text,
      model_errors = result$errors %||% character()
    )
    
    records[[i]] <- record
    
    disk_save_rds(
      forecast_parts[[i]],
      file.path(framework_directory, "predictions.rds")
    )
    disk_save_rds(
      metric_parts[[i]],
      file.path(framework_directory, "metrics.rds")
    )
    disk_save_rds(record, file.path(framework_directory, "metadata.rds"))
    
    rm(result, fc_table, metrics, train, test, record, args, window_config)
    gc(verbose = FALSE)
  }
  
  names(records) <- paste0("window_", seq_along(records))
  
  list(
    forecasts = dplyr::bind_rows(forecast_parts),
    metrics = dplyr::bind_rows(metric_parts),
    cv_info = list(
      n_windows = length(starts),
      train_years = train_years,
      test_years = test_years,
      train_starts = starts,
      test_starts = starts + train_years
    ),
    window_objects = records
  )
}

# =============================================================================
# MVGAM CRPS AND DATA PREPARATION
# =============================================================================

extract_crps_mvgam <- function(forecast_obj, model_name) {
  raw <- mvgam::score(forecast_obj, score = "crps")
  raw <- raw[names(raw) != "all_series"]
  
  dplyr::bind_rows(lapply(names(raw), function(s) {
    entry <- raw[[s]]
    if (!length(entry$score)) return(tibble())
    
    scores <- as.numeric(entry$score)
    horizon <- entry$eval_horizon %||% seq_along(scores)
    
    if (length(horizon) != length(scores)) {
      stop("Unexpected CRPS horizon structure for ", s)
    }
    
    tibble(
      series = s,
      score = scores,
      eval_horizon = horizon,
      model = model_name
    )
  }))
}

prepare_mvgam_data <- function(train_data, test_data) {
  train <- tibble::as_tibble(train_data)
  test <- tibble::as_tibble(test_data)
  keys <- evaluation_keys(train)
  
  for (df in list(train, test)) {
    if (!all(c(keys, "year", "count") %in% names(df))) {
      stop("Training/test data must contain matching keys, year, and count.")
    }
    if (anyNA(df[keys])) stop("Missing series keys.")
  }
  
  assert_unique_rows(train, c(keys, "year"), "Training data")
  assert_unique_rows(test, c(keys, "year"), "Test data")
  
  labels <- function(df) {
    if ("region" %in% keys) {
      paste(df$species, df$region, sep = "_")
    } else {
      as.character(df$species)
    }
  }
  
  train$series_id <- labels(train)
  test$series_id <- labels(test)
  
  series_map <- dplyr::bind_rows(
    train |> dplyr::select(series_id, all_of(keys)),
    test |> dplyr::select(series_id, all_of(keys))
  ) |> dplyr::distinct()
  
  assert_unique_rows(series_map, "series_id", "Series labels")
  
  levels <- unique(train$series_id)
  if (length(setdiff(test$series_id, levels))) {
    stop("Test series absent from training.")
  }
  
  origin <- min(train$year)
  
  convert <- function(df) {
    df$series <- factor(df$series_id, levels = levels)
    df$time <- as.integer(df$year - origin + 1L)
    df$series_id <- NULL
    
    expected <- seq.int(min(df$time), max(df$time))
    for (s in levels) {
      if (!setequal(df$time[as.character(df$series) == s], expected)) {
        stop("Missing annual rows for series ", s)
      }
    }
    
    as.data.frame(dplyr::arrange(df, series, time))
  }
  
  list(
    train = convert(train),
    test = convert(test),
    keys = keys,
    series_map = series_map
  )
}

extract_mvgam_predictions <- function(fc, model_name, prepared) {
  df <- tibble::as_tibble(summary(fc))
  required <- c("series", "time", "predQ50", "predQ2.5", "predQ97.5")
  
  if (!all(required %in% names(df))) {
    stop("Unexpected mvgam summary columns; inspect names(summary(fc)).")
  }
  
  rows <- lapply(prepared$series_map$series_id, function(s) {
    part <- df |>
      dplyr::filter(as.character(series) == s) |>
      dplyr::arrange(time)
    
    expected <- prepared$test |>
      tibble::as_tibble() |>
      dplyr::filter(as.character(series) == s) |>
      dplyr::arrange(time)
    
    times <- as.integer(part$time)
    
    if (anyDuplicated(times)) stop("Duplicate summary times for ", s)
    
    if (all(expected$time %in% times)) {
      index <- match(expected$time, times)
    } else if (nrow(part) == nrow(expected) &&
               setequal(times, seq_len(nrow(expected)))) {
      index <- match(seq_len(nrow(expected)), times)
    } else {
      stop("Cannot map forecast years for ", s)
    }
    
    part <- part[index, , drop = FALSE]
    
    out <- tibble(
      series = s,
      time = expected$time,
      year = expected$year,
      model = model_name,
      Estimate = part$predQ50,
      Q2.5 = part$predQ2.5,
      Q97.5 = part$predQ97.5
    )
    
    for (q in c("10", "25", "75", "90")) {
      col <- paste0("predQ", q)
      if (col %in% names(part)) out[[paste0("Q", q)]] <- part[[col]]
    }
    out
  })
  
  dplyr::bind_rows(rows) |>
    dplyr::left_join(prepared$series_map, by = c("series" = "series_id"))
}

# =============================================================================
# MVGAM - FIT, SAVE, PLOT, SUMMARISE, RELEASE EACH MODEL
# =============================================================================

make_mvgam_forecasts <- function(train_data, test_data, models_to_run,
                                 use_ordinal = FALSE,
                                 precomputed_breaks = NULL,
                                 config = get_evaluation_config()) {
  if (is.null(config$.window_dir)) {
    stop("Call make_mvgam_forecasts through fit_sliding_window().")
  }
  
  prepared <- prepare_mvgam_data(train_data, test_data)
  keys <- prepared$keys
  groups <- c("model", keys)
  
  prediction_parts <- list()
  crps_parts <- list()
  model_files <- list()
  errors <- character()
  
  for (model_name in unique(models_to_run)) {
    function_name <- paste0("fit_mvgam_", model_name)
    
    if (!exists(function_name, mode = "function", inherits = TRUE)) {
      errors[model_name] <- paste("Missing function:", function_name)
      next
    }
    
    cat("Fitting mvgam:", model_name, "\n")
    
    fit_function <- get(function_name, mode = "function", inherits = TRUE)
    
    result <- tryCatch(
      fit_function(prepared$train, prepared$test, config),
      error = function(e) {
        errors[model_name] <<- conditionMessage(e)
        NULL
      }
    )
    
    if (is.null(result) || is.null(result$fc)) {
      errors[model_name] <- errors[model_name] %||% "Model returned no forecast."
      rm(result)
      gc(verbose = FALSE)
      next
    }
    
    model_directory <- file.path(
      config$.window_dir,
      disk_safe_name(model_name)
    )
    raw_file <- file.path(model_directory, "forecast.rds")
    
    # Fatal if raw-object persistence fails.
    tryCatch(
      disk_save_rds(result$fc, raw_file),
      error = function(e) {
        condition <- structure(
          list(message = conditionMessage(e), call = NULL),
          class = c("forecast_disk_error", "error", "condition")
        )
        stop(condition)
      }
    )
    
    model_files[[model_name]] <- file.path(
      config$.window_relative_dir,
      disk_safe_name(model_name),
      "forecast.rds"
    )
    
    if (isTRUE(config$plots$native_mvgam %||% TRUE)) {
      tryCatch(
        save_native_mvgam_object(
          result$fc,
          file.path(model_directory, "plots")
        ),
        error = function(e) {
          warning("Native plot failed: ", conditionMessage(e), call. = FALSE)
        }
      )
    }
    
    prediction_parts[[model_name]] <- tryCatch(
      extract_mvgam_predictions(result$fc, model_name, prepared),
      error = function(e) {
        errors[paste0(model_name, "_predictions")] <<- conditionMessage(e)
        tibble()
      }
    )
    
    crps_parts[[model_name]] <- tryCatch({
      scores <- result$crps
      if (is.null(scores) || !nrow(scores)) {
        scores <- extract_crps_mvgam(result$fc, model_name)
      }
      
      scores |>
        tibble::as_tibble() |>
        dplyr::mutate(series = as.character(series), model = model_name) |>
        dplyr::left_join(
          prepared$series_map,
          by = c("series" = "series_id")
        )
    }, error = function(e) {
      errors[paste0(model_name, "_crps")] <<- conditionMessage(e)
      tibble()
    })
    
    # No raw forecast list is accumulated.
    rm(result, fit_function)
    gc(verbose = FALSE)
  }
  
  predictions <- dplyr::bind_rows(prediction_parts)
  crps_rows <- dplyr::bind_rows(crps_parts)
  metric_parts <- list()
  
  if (nrow(crps_rows)) {
    if (anyNA(crps_rows[keys])) stop("Unmatched CRPS series labels.")
    
    metric_parts$crps <- crps_rows |>
      dplyr::group_by(across(all_of(groups))) |>
      dplyr::summarise(
        crps = finite_mean(score),
        n_forecasts = sum(!is.na(score)),
        .groups = "drop"
      ) |>
      add_baseline_skill("model", keys, "crps")
  }
  
  if (nrow(predictions)) {
    obs_keys <- c(keys, "year")
    
    metric_parts$rmse <- predictions |>
      dplyr::left_join(
        tibble::as_tibble(test_data) |>
          dplyr::select(all_of(c(obs_keys, "count"))),
        by = obs_keys
      ) |>
      dplyr::group_by(across(all_of(groups))) |>
      dplyr::summarise(
        rmse = sqrt(finite_mean((Estimate - count)^2)),
        n_rmse = sum(!is.na(Estimate) & !is.na(count)),
        .groups = "drop"
      ) |>
      add_baseline_skill("model", keys, "rmse")
    
    if (isTRUE(use_ordinal)) {
      metric_parts$rps <- tryCatch(
        calculate_rps_mvgam(
          predictions, test_data, train_data, config, precomputed_breaks
        ),
        error = function(e) {
          warning("RPS failed: ", conditionMessage(e), call. = FALSE)
          tibble()
        }
      )
    }
  }
  
  metric_parts <- Filter(function(x) nrow(x) > 0L, metric_parts)
  
  metrics <- if (length(metric_parts)) {
    Reduce(function(a, b) dplyr::full_join(a, b, by = groups), metric_parts)
  } else {
    tibble()
  }
  
  if (nrow(metrics)) {
    for (col in c(
      "crps", "crps_baseline", "crps_skill",
      "rmse", "rmse_baseline", "rmse_skill"
    )) {
      if (!col %in% names(metrics)) metrics[[col]] <- NA_real_
    }
  }
  
  list(
    predictions = predictions,
    metrics = metrics,
    model_files = model_files,
    errors = errors
  )
}

# =============================================================================
# ORDINAL THRESHOLDS AND RPS
# =============================================================================

filter_ordinal_years <- function(df, ordinal_years = "All") {
  if (is.null(ordinal_years) ||
      identical(tolower(as.character(ordinal_years)), "all")) {
    return(df)
  }
  
  years <- positive_integer(
    suppressWarnings(as.numeric(ordinal_years)), "ordinal_years"
  )
  
  dplyr::filter(df, year >= max(year, na.rm = TRUE) - years + 1L)
}

fix_zero_breaks <- function(quantiles_df) {
  out <- tibble::as_tibble(quantiles_df)
  
  for (i in seq_len(nrow(out))) {
    # Explicitly unlist the row: a tibble is not a numeric vector.
    values <- as.numeric(unlist(
      out[i, c("low", "medium", "high")],
      use.names = FALSE
    ))
    
    if (!all(is.finite(values))) next
    values <- sort(values)
    eps <- 1e-8 * max(1, abs(values))
    
    for (j in 2:3) {
      if (values[j] <= values[j - 1L]) {
        values[j] <- values[j - 1L] + eps
      }
    }
    
    out$low[i] <- values[1]
    out$medium[i] <- values[2]
    out$high[i] <- values[3]
  }
  
  out
}

ordinal_thresholds <- function(train_data, config,
                               precomputed_breaks = NULL) {
  if (!is.null(precomputed_breaks)) {
    return(fix_zero_breaks(precomputed_breaks))
  }
  
  probs <- unlist(config$ordinal_breaks %||% c(0.33, 0.50, 0.67))
  
  if (length(probs) != 3L || any(!is.finite(probs)) ||
      any(probs <= 0 | probs >= 1) || any(diff(probs) <= 0)) {
    stop("ordinal_breaks must contain three increasing probabilities.")
  }
  
  q <- function(x, p) {
    x <- x[is.finite(x)]
    if (length(x)) {
      as.numeric(stats::quantile(x, p, names = FALSE))
    } else {
      NA_real_
    }
  }
  
  train_data |>
    tibble::as_tibble() |>
    filter_ordinal_years(config$ordinal_years %||% "All") |>
    dplyr::group_by(across(all_of(evaluation_keys(train_data)))) |>
    dplyr::summarise(
      low = q(count, probs[1]),
      medium = q(count, probs[2]),
      high = q(count, probs[3]),
      .groups = "drop"
    ) |>
    fix_zero_breaks()
}

join_ordinal_data <- function(predictions, test_data, thresholds) {
  keys <- evaluation_keys(test_data)
  threshold_keys <- intersect(keys, names(thresholds))
  if (!"species" %in% threshold_keys) stop("Missing threshold species key.")
  
  assert_unique_rows(thresholds, threshold_keys, "Ordinal thresholds")
  obs_keys <- c(keys, "year")
  
  obs <- tibble::as_tibble(test_data) |>
    dplyr::select(all_of(c(obs_keys, "count"))) |>
    dplyr::rename(observed_count = count)
  
  assert_unique_rows(obs, obs_keys, "Ordinal observations")
  
  predictions |>
    tibble::as_tibble() |>
    dplyr::left_join(thresholds, by = threshold_keys) |>
    dplyr::left_join(obs, by = obs_keys)
}

aggregate_rps <- function(df, model_col, keys) {
  df$rps_row <-
    (df$cdf_low - (df$observed_count <= df$low))^2 +
    (df$cdf_medium - (df$observed_count <= df$medium))^2 +
    (df$cdf_high - (df$observed_count <= df$high))^2
  
  invalid <- !is.finite(df$observed_count) |
    !is.finite(df$low) | !is.finite(df$medium) | !is.finite(df$high)
  
  df$rps_row[invalid] <- NA_real_
  
  df |>
    dplyr::group_by(across(all_of(c(model_col, keys)))) |>
    dplyr::summarise(
      rps = finite_mean(rps_row),
      n_rps = sum(!is.na(rps_row)),
      .groups = "drop"
    ) |>
    add_baseline_skill(model_col, keys, "rps")
}

calculate_rps_mvgam <- function(predictions, test_data, train_data, config,
                                precomputed_breaks = NULL) {
  # Preserve the previous Normal-approximation RPS.
  # This is not exact posterior-draw RPS for skewed count distributions.
  thresholds <- ordinal_thresholds(train_data, config, precomputed_breaks)
  df <- join_ordinal_data(predictions, test_data, thresholds)
  sd <- pmax((df$Q97.5 - df$Q2.5) / (2 * 1.96), 0.1)
  
  df$cdf_low <- stats::pnorm(df$low, df$Estimate, sd)
  df$cdf_medium <- stats::pnorm(df$medium, df$Estimate, sd)
  df$cdf_high <- stats::pnorm(df$high, df$Estimate, sd)
  
  aggregate_rps(df, "model", evaluation_keys(test_data))
}

calculate_rps_fable <- function(forecasts, test_data, train_data, config,
                                precomputed_breaks = NULL) {
  thresholds <- ordinal_thresholds(train_data, config, precomputed_breaks)
  df <- join_ordinal_data(forecasts, test_data, thresholds)
  
  cdf_at <- function(distributions, thresholds) {
    vapply(seq_along(thresholds), function(i) {
      if (!is.finite(thresholds[i])) return(NA_real_)
      as.numeric(unlist(
        distributional::cdf(distributions[i], thresholds[i])
      ))[1]
    }, numeric(1))
  }
  
  df$cdf_low <- cdf_at(df$count, df$low)
  df$cdf_medium <- cdf_at(df$count, df$medium)
  df$cdf_high <- cdf_at(df$count, df$high)
  
  aggregate_rps(df, ".model", evaluation_keys(test_data))
}

# =============================================================================
# FABLE METRICS
# =============================================================================

make_fable_evaluation <- function(raw_metrics, forecasts, test_data,
                                  train_data, config,
                                  precomputed_breaks = NULL,
                                  use_ordinal = FALSE) {
  if (is.null(raw_metrics) || !nrow(raw_metrics)) return(tibble())
  
  metrics <- tibble::as_tibble(raw_metrics)
  required <- c(".model", "species", "crps", "rmse")
  
  if (!all(required %in% names(metrics))) {
    stop("Missing required fable metric columns.")
  }
  
  keys <- intersect(evaluation_keys(test_data), names(metrics))
  baseline_keys <- c(keys, intersect(".type", names(metrics)))
  
  assert_unique_rows(
    metrics, c(".model", baseline_keys), "Fable accuracy metrics"
  )
  
  metrics <- metrics |>
    add_baseline_skill(".model", baseline_keys, "crps") |>
    add_baseline_skill(".model", baseline_keys, "rmse")
  
  if (isTRUE(use_ordinal)) {
    rps <- tryCatch(
      calculate_rps_fable(
        forecasts, test_data, train_data, config, precomputed_breaks
      ),
      error = function(e) {
        warning("Fable RPS failed: ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
    
    if (nrow(rps)) {
      metrics <- dplyr::left_join(metrics, rps, by = c(".model", keys))
    }
  }
  
  metrics
}

# =============================================================================
# LOAD SAVED WINDOW INPUTS / ONE FORECAST OBJECT
# =============================================================================

load_window_data <- function(record, results_dir) {
  list(
    train_data = readRDS(file.path(results_dir, record$train_file)),
    test_data = readRDS(file.path(results_dir, record$test_file))
  )
}

load_window_forecast <- function(record, model_name, results_dir) {
  relative_file <- record$model_files[[model_name]]
  
  if (is.null(relative_file)) {
    stop("No saved forecast for model: ", model_name)
  }
  
  filename <- file.path(results_dir, relative_file)
  if (!file.exists(filename)) stop("Missing forecast file: ", filename)
  
  readRDS(filename)
}

# =============================================================================
# SUMMARY
# =============================================================================

print_cv_summary <- function(cv_results) {
  cat("\n=== Cross-validation summary ===\n")
  info <- cv_results$cv_info
  
  if (!is.null(info)) {
    cat("Windows:", info$n_windows %||% "regional", "\n")
    cat("Training years:", info$train_years, "\n")
    cat("Test years:", info$test_years, "\n")
  }
  
  metrics <- cv_results$metrics
  if (is.null(metrics) || !nrow(metrics)) {
    cat("No metrics available.\n")
    return(invisible(NULL))
  }
  
  model_col <- if ("model" %in% names(metrics)) {
    "model"
  } else if (".model" %in% names(metrics)) {
    ".model"
  } else {
    return(invisible(NULL))
  }
  
  if (!"crps" %in% names(metrics)) return(invisible(NULL))
  if (!"crps_skill" %in% names(metrics)) metrics$crps_skill <- NA_real_
  
  out <- metrics |>
    dplyr::group_by(.data[[model_col]]) |>
    dplyr::summarise(
      mean_crps = finite_mean(crps),
      mean_skill = finite_mean(crps_skill),
      metric_rows = n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(mean_crps)
  
  print(out, n = Inf)
  invisible(out)
}

cat("✓ Disk-backed evaluation.R loaded\n")