# =============================================================================
# EVALUATION.R
#
# Outputs:
#   forecasts     : tidy predictions for existing plots
#   metrics       : CRPS / RMSE / optional RPS and baseline-relative skills
#   cv_info       : cross-validation settings
#   window_objects: original forecast objects and data for every window
#
# Existing model return format remains:
#   list(fc = fc, crps = crps)
#
# Window fits remain sequential, matching the original implementation.
# Do not truncate test_data to control a multi-series forecast horizon.
# =============================================================================

library(dplyr)
library(tidyr)
library(distributional)
library(future)

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# CONFIGURATION AND COMMON UTILITIES
# =============================================================================

guard_config_init <- function() {
  # Never overwrite a CONFIG supplied by run_all_scales.R.
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
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

safe_skill <- function(value, baseline) {
  out <- rep(NA_real_, length(value))
  valid <- is.finite(value) & is.finite(baseline) & baseline > 0
  out[valid] <- 1 - value[valid] / baseline[valid]
  out
}

evaluation_keys <- function(df) {
  keys <- "species"
  if ("region" %in% names(df)) keys <- c(keys, "region")
  keys
}

assert_unique_rows <- function(df, keys, label) {
  if (anyDuplicated(as.data.frame(df[keys]))) {
    stop(label, " has duplicate rows for: ", paste(keys, collapse = ", "))
  }
  invisible(NULL)
}

add_baseline_skill <- function(df, model_col, keys, metric) {
  baseline_col <- paste0(metric, "_baseline")
  skill_col <- paste0(metric, "_skill")
  
  # Allow this helper to be called again safely.
  df <- df |>
    dplyr::select(-any_of(c(baseline_col, skill_col)))
  
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
    fc_objects = list()
  )
}

# =============================================================================
# PARALLEL PLAN UTILITIES
# =============================================================================

setup_parallel <- function(enabled = TRUE, workers = NULL) {
  if (!isTRUE(enabled)) {
    future::plan(future::sequential)
    return(list(enabled = FALSE, workers = 1L))
  }
  
  if (is.null(workers)) {
    workers <- parallelly::availableCores()
    workers <- max(1L, as.integer(workers) - 1L)
  }
  
  workers <- positive_integer(workers, "workers")
  future::plan(future::multisession, workers = workers)
  
  cat("Parallel plan configured:", workers, "workers\n")
  list(enabled = TRUE, workers = workers)
}

teardown_parallel <- function() {
  future::plan(future::sequential)
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
                               ...) {
  config <- get_evaluation_config()
  train_years <- positive_integer(train_years, "train_years")
  test_years <- positive_integer(test_years, "test_years")
  
  if (!nrow(data) || !"year" %in% names(data)) {
    stop("data must contain observations and a year column.")
  }
  
  years <- sort(unique(data$year))
  
  if (anyNA(years) || !is.numeric(years) ||
      any(!is.finite(years)) || any(years != floor(years))) {
    stop("year must contain finite, non-missing integer years.")
  }
  
  year_min <- min(years)
  year_max <- max(years)
  last_start <- year_max - train_years - test_years + 1L
  
  if (last_start < year_min) {
    stop(
      "Insufficient years: need at least ",
      train_years + test_years, " calendar years."
    )
  }
  
  train_starts <- seq.int(year_min, last_start)
  
  if (!is.null(cv_windows)) {
    cv_windows <- positive_integer(cv_windows, "cv_windows")
    train_starts <- tail(train_starts, cv_windows)
  }
  
  test_starts <- train_starts + train_years
  n_windows <- length(train_starts)
  dots <- list(...)
  
  if (isTRUE(parallel)) {
    message(
      "CV windows are fitted sequentially in this implementation. ",
      "The parallel/workers arguments are retained for main.R compatibility."
    )
  }
  
  cat("\n=== Cross-validation ===\n")
  cat("Years:", year_min, "-", year_max, "\n")
  cat("Training years:", train_years, "\n")
  cat("Forecast/test years:", test_years, "\n")
  cat("Windows:", n_windows, "\n\n")
  
  results_list <- lapply(seq_along(train_starts), function(i) {
    train_start <- train_starts[i]
    test_start <- test_starts[i]
    test_end <- test_start + test_years - 1L
    
    train_data <- data |>
      dplyr::filter(year >= train_start, year < test_start)
    
    test_data <- data |>
      dplyr::filter(year >= test_start, year <= test_end)
    
    cat(
      "Window", i, "/", n_windows,
      "| Train", train_start, "-", test_start - 1L,
      "| Test", test_start, "-", test_end, "\n"
    )
    
    error_text <- NULL
    
    result <- tryCatch({
      expected_train <- seq.int(train_start, test_start - 1L)
      expected_test <- seq.int(test_start, test_end)
      
      if (!setequal(unique(train_data$year), expected_train) ||
          !setequal(unique(test_data$year), expected_test)) {
        stop("Missing calendar years in this window.")
      }
      
      do.call(make_forecast, c(list(train_data, test_data), dots))
    }, error = function(e) {
      error_text <<- conditionMessage(e)
      warning("Window ", i, " failed: ", error_text, call. = FALSE)
      empty_window_result()
    })
    
    if (is.null(result) || length(result) < 2L) {
      error_text <- error_text %||% "Forecast function returned no usable result."
      result <- empty_window_result()
    }
    
    # First two elements preserve the existing calling contract.
    fc_raw <- result[[1]]
    met_raw <- result[[2]]
    raw_models <- result$fc_objects %||% list()
    
    is_fable <- inherits(fc_raw, "fbl_ts") ||
      inherits(fc_raw, "fable") ||
      (
        is.data.frame(fc_raw) &&
          ".model" %in% names(fc_raw) &&
          "count" %in% names(fc_raw) &&
          inherits(fc_raw$count, "distribution")
      )
    
    if (is_fable) {
      # Preserve the original fable before conversion to an ordinary tibble.
      raw_models <- list(fable = fc_raw)
      
      met_raw <- tryCatch({
        make_fable_evaluation(
          raw_metrics = met_raw,
          forecasts = fc_raw,
          test_data = test_data,
          train_data = train_data,
          config = config,
          precomputed_breaks = dots$precomputed_breaks,
          use_ordinal = dots$use_ordinal %||% FALSE
        )
      }, error = function(e) {
        warning(
          "Fable evaluation failed in window ", i, ": ",
          conditionMessage(e), call. = FALSE
        )
        tibble()
      })
    }
    
    tag_window <- function(x) {
      if (is.null(x) || !nrow(x)) return(tibble())
      tibble::as_tibble(x) |>
        dplyr::mutate(test_start = test_start, window = i)
    }
    
    list(
      forecasts = tag_window(fc_raw),
      metrics = tag_window(met_raw),
      object = list(
        window = i,
        train_start = train_start,
        train_end = test_start - 1L,
        test_start = test_start,
        test_end = test_end,
        train_data = tibble::as_tibble(train_data),
        test_data = tibble::as_tibble(test_data),
        models = raw_models,
        error = error_text
      )
    )
  })
  
  forecasts <- dplyr::bind_rows(lapply(results_list, `[[`, "forecasts"))
  metrics <- dplyr::bind_rows(lapply(results_list, `[[`, "metrics"))
  
  window_objects <- setNames(
    lapply(results_list, `[[`, "object"),
    paste0("window_", seq_along(results_list))
  )
  
  cat(
    "\nComplete:", nrow(forecasts), "forecast rows;",
    nrow(metrics), "metric rows\n"
  )
  
  list(
    forecasts = forecasts,
    metrics = metrics,
    cv_info = list(
      n_windows = n_windows,
      train_years = train_years,
      test_years = test_years,
      train_starts = train_starts,
      test_starts = test_starts
    ),
    window_objects = window_objects
  )
}

# =============================================================================
# MVGAM CRPS
# =============================================================================

extract_crps_mvgam <- function(forecast_obj, model_name) {
  raw <- mvgam::score(forecast_obj, score = "crps")
  raw <- raw[names(raw) != "all_series"]
  
  dplyr::bind_rows(lapply(names(raw), function(series_name) {
    entry <- raw[[series_name]]
    if (is.null(entry$score) || !length(entry$score)) return(tibble())
    
    scores <- as.numeric(entry$score)
    horizon <- entry$eval_horizon %||% seq_along(scores)
    
    if (length(horizon) != length(scores)) {
      stop("Unexpected CRPS horizon structure for ", series_name)
    }
    
    tibble(
      series = series_name,
      score = scores,
      eval_horizon = horizon,
      model = model_name
    )
  }))
}

# =============================================================================
# MVGAM DATA AND SUMMARY EXTRACTION
# =============================================================================

prepare_mvgam_data <- function(train_data, test_data) {
  train <- tibble::as_tibble(train_data)
  test <- tibble::as_tibble(test_data)
  
  keys <- evaluation_keys(train)
  
  if (!all(c(keys, "year", "count") %in% names(test))) {
    stop("Training and test data must have matching series keys.")
  }
  
  assert_unique_rows(train, c(keys, "year"), "Training data")
  assert_unique_rows(test, c(keys, "year"), "Test data")
  
  if (anyNA(train[keys]) || anyNA(test[keys])) {
    stop("Species/region keys must not be missing.")
  }
  
  # Keep the original naming convention without decoding labels with regex.
  # Metadata is recovered by joining against series_map.
  make_label <- function(df) {
    if ("region" %in% keys) {
      paste(df$species, df$region, sep = "_")
    } else {
      as.character(df$species)
    }
  }
  
  train$series_id <- make_label(train)
  test$series_id <- make_label(test)
  
  series_map <- dplyr::bind_rows(
    train |> dplyr::select(series_id, all_of(keys)),
    test |> dplyr::select(series_id, all_of(keys))
  ) |>
    dplyr::distinct()
  
  assert_unique_rows(series_map, "series_id", "Series labels")
  
  if (length(setdiff(test$series_id, train$series_id))) {
    stop("Test data contains series absent from training.")
  }
  
  levels <- unique(train$series_id)
  origin <- min(train$year)
  
  convert <- function(df) {
    df$series <- factor(df$series_id, levels = levels)
    df$time <- as.integer(df$year - origin + 1L)
    df$series_id <- NULL
    
    df <- df |> dplyr::arrange(series, time)
    as.data.frame(df)
  }
  
  train <- convert(train)
  test <- convert(test)
  
  # Require a complete annual grid for each series; do not silently invent zeros.
  check_grid <- function(df, label) {
    expected <- seq.int(min(df$time), max(df$time))
    for (s in levels) {
      times <- df$time[as.character(df$series) == s]
      if (!setequal(times, expected)) {
        stop(label, " is missing annual rows for series ", s)
      }
    }
  }
  
  check_grid(train, "Training data")
  check_grid(test, "Test data")
  
  list(
    train = train,
    test = test,
    keys = keys,
    series_map = series_map
  )
}

extract_mvgam_predictions <- function(fc, model_name, prepared) {
  summary_df <- tibble::as_tibble(summary(fc))
  
  required <- c("series", "time", "predQ50", "predQ2.5", "predQ97.5")
  missing <- setdiff(required, names(summary_df))
  
  if (length(missing)) {
    stop(
      "Unexpected mvgam summary columns: missing ",
      paste(missing, collapse = ", "),
      ". Inspect names(summary(fc)) for your installed version."
    )
  }
  
  test <- prepared$test
  series_map <- prepared$series_map
  
  rows <- lapply(series_map$series_id, function(s) {
    part <- summary_df |>
      dplyr::filter(as.character(series) == s) |>
      dplyr::arrange(time)
    
    expected <- test |>
      tibble::as_tibble() |>
      dplyr::filter(as.character(series) == s) |>
      dplyr::arrange(time)
    
    absolute_times <- expected$time
    relative_times <- seq_len(nrow(expected))
    summary_times <- as.integer(part$time)
    
    # Accept either absolute model times or forecast-relative times.
    # Do not use slice(1:n) on a summary that may include fitted values.
    if (all(absolute_times %in% summary_times)) {
      selected <- match(absolute_times, summary_times)
    } else if (
      nrow(part) == nrow(expected) &&
      setequal(summary_times, relative_times)
    ) {
      selected <- match(relative_times, summary_times)
    } else {
      stop("Cannot reliably map forecast times to years for series ", s)
    }
    
    if (anyDuplicated(summary_times)) {
      stop("Duplicate summary times for series ", s)
    }
    
    part <- part[selected, , drop = FALSE]
    
    out <- tibble(
      series = s,
      time = absolute_times,
      year = expected$year,
      model = model_name,
      Estimate = part$predQ50,
      Q2.5 = part$predQ2.5,
      Q97.5 = part$predQ97.5
    )
    
    # Preserve genuine quantiles when supplied by mvgam.
    # Never approximate an 80% interval from a 95% interval here.
    for (q in c("10", "25", "75", "90")) {
      source <- paste0("predQ", q)
      if (source %in% names(part)) {
        out[[paste0("Q", q)]] <- part[[source]]
      }
    }
    
    out
  })
  
  dplyr::bind_rows(rows) |>
    dplyr::left_join(series_map, by = c("series" = "series_id"))
}

# =============================================================================
# MVGAM FORECASTING AND METRICS
# =============================================================================

make_mvgam_forecasts <- function(train_data,
                                 test_data,
                                 models_to_run,
                                 use_ordinal = FALSE,
                                 precomputed_breaks = NULL,
                                 config = get_evaluation_config()) {
  prepared <- prepare_mvgam_data(train_data, test_data)
  keys <- prepared$keys
  group_vars <- c("model", keys)
  
  objects <- list()
  prediction_parts <- list()
  crps_parts <- list()
  
  for (model_name in unique(models_to_run)) {
    function_name <- paste0("fit_mvgam_", model_name)
    
    if (!exists(function_name, mode = "function", inherits = TRUE)) {
      warning("Model function not found: ", function_name, call. = FALSE)
      next
    }
    
    fit_function <- get(function_name, mode = "function", inherits = TRUE)
    
    cat("\nFitting mvgam model:", model_name, "\n")
    
    result <- tryCatch(
      fit_function(prepared$train, prepared$test, config),
      error = function(e) {
        warning(
          model_name, " failed: ", conditionMessage(e), call. = FALSE
        )
        NULL
      }
    )
    
    if (is.null(result) || is.null(result$fc)) next
    
    # Preserve the existing raw object, even if summary extraction fails.
    objects[[model_name]] <- result$fc
    
    prediction_parts[[model_name]] <- tryCatch(
      extract_mvgam_predictions(result$fc, model_name, prepared),
      error = function(e) {
        warning(
          "Prediction extraction failed for ", model_name, ": ",
          conditionMessage(e), call. = FALSE
        )
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
        dplyr::mutate(
          series = as.character(series),
          model = model_name
        ) |>
        dplyr::left_join(
          prepared$series_map,
          by = c("series" = "series_id")
        )
    }, error = function(e) {
      warning(
        "CRPS extraction failed for ", model_name, ": ",
        conditionMessage(e), call. = FALSE
      )
      tibble()
    })
  }
  
  predictions <- dplyr::bind_rows(prediction_parts)
  crps_rows <- dplyr::bind_rows(crps_parts)
  
  if (!length(objects)) {
    warning("All mvgam models failed.", call. = FALSE)
    return(empty_window_result())
  }
  
  crps_metrics <- tibble()
  
  if (nrow(crps_rows)) {
    if (anyNA(crps_rows[keys])) {
      stop("CRPS series labels do not match the prepared series map.")
    }
    
    crps_metrics <- crps_rows |>
      dplyr::group_by(across(all_of(group_vars))) |>
      dplyr::summarise(
        crps = finite_mean(score),
        n_forecasts = sum(is.finite(score)),
        .groups = "drop"
      ) |>
      add_baseline_skill("model", keys, "crps")
  }
  
  rmse_metrics <- tibble()
  
  if (nrow(predictions)) {
    obs_keys <- c(keys, "year")
    
    rmse_metrics <- predictions |>
      dplyr::left_join(
        tibble::as_tibble(test_data) |>
          dplyr::select(all_of(c(obs_keys, "count"))),
        by = obs_keys
      ) |>
      dplyr::group_by(across(all_of(group_vars))) |>
      dplyr::summarise(
        rmse = sqrt(finite_mean((Estimate - count)^2)),
        n_rmse = sum(is.finite(Estimate) & is.finite(count)),
        .groups = "drop"
      ) |>
      add_baseline_skill("model", keys, "rmse")
  }
  
  if (nrow(crps_metrics) && nrow(rmse_metrics)) {
    metrics <- dplyr::full_join(
      crps_metrics, rmse_metrics, by = group_vars
    )
  } else if (nrow(crps_metrics)) {
    metrics <- crps_metrics
  } else {
    metrics <- rmse_metrics
  }
  
  # Preserve expected metric columns even when scoring fails.
  if (nrow(metrics)) {
    for (col in c(
      "crps", "crps_baseline", "crps_skill",
      "rmse", "rmse_baseline", "rmse_skill"
    )) {
      if (!col %in% names(metrics)) metrics[[col]] <- NA_real_
    }
  }
  
  if (isTRUE(use_ordinal) && nrow(predictions)) {
    rps_metrics <- tryCatch(
      calculate_rps_mvgam(
        predictions, test_data, train_data, config, precomputed_breaks
      ),
      error = function(e) {
        warning("mvgam RPS failed: ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
    
    if (nrow(rps_metrics)) {
      if (nrow(metrics)) {
        metrics <- dplyr::full_join(
          metrics, rps_metrics, by = group_vars
        )
      } else {
        metrics <- rps_metrics
      }
    }
  }
  
  list(
    predictions = predictions,
    metrics = metrics,
    fc_objects = objects
  )
}

# =============================================================================
# ORDINAL THRESHOLDS AND RPS
# =============================================================================

filter_ordinal_years <- function(df, ordinal_years = "All") {
  if (is.null(ordinal_years) ||
      tolower(as.character(ordinal_years)) == "all") {
    return(df)
  }
  
  years <- suppressWarnings(as.numeric(ordinal_years))
  years <- positive_integer(years, "ordinal_years")
  
  df |>
    dplyr::filter(year >= max(year, na.rm = TRUE) - years + 1L)
}

fix_zero_breaks <- function(quantiles_df) {
  # Preserve valid quantiles.
  # Resolve repeated finite thresholds using a small numerical increment.
  # Missing thresholds remain NA rather than being invented.
  out <- tibble::as_tibble(quantiles_df)
  
  if (!nrow(out)) return(out)
  
  for (i in seq_len(nrow(out))) {
    values <- as.numeric(out[i, c("low", "medium", "high")])
    
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
  
  keys <- evaluation_keys(train_data)
  
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
    dplyr::group_by(across(all_of(keys))) |>
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
  
  if (!"species" %in% threshold_keys) {
    stop("Ordinal thresholds must identify species.")
  }
  
  assert_unique_rows(thresholds, threshold_keys, "Ordinal thresholds")
  
  obs_keys <- c(keys, "year")
  observations <- tibble::as_tibble(test_data) |>
    dplyr::select(all_of(c(obs_keys, "count"))) |>
    dplyr::rename(observed_count = count)
  
  assert_unique_rows(observations, obs_keys, "Ordinal observations")
  
  predictions |>
    tibble::as_tibble() |>
    dplyr::left_join(thresholds, by = threshold_keys) |>
    dplyr::left_join(observations, by = obs_keys)
}

aggregate_rps <- function(df, model_col, keys) {
  # RPS = sum of squared cumulative-probability errors.
  # Category convention: (-Inf, low], (low, medium], etc.
  # Observations and predictions are matched explicitly by year and series.
  row_score <- (df$cdf_low - (df$observed_count <= df$low))^2 +
    (df$cdf_medium - (df$observed_count <= df$medium))^2 +
    (df$cdf_high - (df$observed_count <= df$high))^2
  
  invalid <- !is.finite(df$observed_count) |
    !is.finite(df$low) |
    !is.finite(df$medium) |
    !is.finite(df$high)
  
  row_score[invalid] <- NA_real_
  df$rps_row <- row_score
  
  df |>
    dplyr::group_by(across(all_of(c(model_col, keys)))) |>
    dplyr::summarise(
      rps = finite_mean(rps_row),
      n_rps = sum(is.finite(rps_row)),
      .groups = "drop"
    ) |>
    add_baseline_skill(model_col, keys, "rps")
}

calculate_rps_mvgam <- function(predictions, test_data, train_data, config,
                                precomputed_breaks = NULL) {
  # APPROXIMATION retained from the original implementation.
  # For skewed count forecasts, use posterior-draw category probabilities
  # in a subsequent revision rather than treating this as exact RPS.
  thresholds <- ordinal_thresholds(
    train_data, config, precomputed_breaks
  )
  
  df <- join_ordinal_data(predictions, test_data, thresholds)
  
  pred_sd <- pmax((df$Q97.5 - df$Q2.5) / (2 * 1.96), 0.1)
  
  df$cdf_low <- stats::pnorm(df$low, df$Estimate, pred_sd)
  df$cdf_medium <- stats::pnorm(df$medium, df$Estimate, pred_sd)
  df$cdf_high <- stats::pnorm(df$high, df$Estimate, pred_sd)
  
  aggregate_rps(df, "model", evaluation_keys(test_data))
}

calculate_rps_fable <- function(forecasts, test_data, train_data, config,
                                precomputed_breaks = NULL) {
  thresholds <- ordinal_thresholds(
    train_data, config, precomputed_breaks
  )
  
  df <- join_ordinal_data(forecasts, test_data, thresholds)
  
  # Evaluate the actual forecast distribution, not a Normal approximation.
  cdf_at <- function(distributions, thresholds) {
    vapply(seq_along(thresholds), function(i) {
      if (!is.finite(thresholds[i])) return(NA_real_)
      value <- distributional::cdf(
        distributions[i], thresholds[i]
      )
      as.numeric(unlist(value))[1]
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

make_fable_evaluation <- function(raw_metrics,
                                  forecasts,
                                  test_data,
                                  train_data,
                                  config,
                                  precomputed_breaks = NULL,
                                  use_ordinal = FALSE) {
  if (is.null(raw_metrics) || !nrow(raw_metrics)) return(tibble())
  
  metrics <- tibble::as_tibble(raw_metrics)
  required <- c(".model", "species", "crps", "rmse")
  
  if (!all(required %in% names(metrics))) {
    stop(
      "Fable metrics are missing: ",
      paste(setdiff(required, names(metrics)), collapse = ", ")
    )
  }
  
  keys <- intersect(evaluation_keys(test_data), names(metrics))
  baseline_keys <- c(keys, intersect(".type", names(metrics)))
  
  assert_unique_rows(
    metrics, c(".model", baseline_keys), "Fable accuracy metrics"
  )
  
  metrics <- metrics |>
    add_baseline_skill(".model", baseline_keys, "crps") |>
    add_baseline_skill(".model", baseline_keys, "rmse")
  
  # Missing baselines yield NA skills without dropping raw scores.
  if (!any(metrics$.model == "baseline")) {
    warning("No fable baseline; skill scores are NA.", call. = FALSE)
  }
  
  if (isTRUE(use_ordinal)) {
    rps_metrics <- tryCatch(
      calculate_rps_fable(
        forecasts, test_data, train_data, config, precomputed_breaks
      ),
      error = function(e) {
        warning("Fable RPS failed: ", conditionMessage(e), call. = FALSE)
        tibble()
      }
    )
    
    if (nrow(rps_metrics)) {
      metrics <- dplyr::left_join(
        metrics,
        rps_metrics,
        by = c(".model", keys)
      )
    }
  }
  
  metrics
}

# =============================================================================
# SUMMARY
# =============================================================================

print_cv_summary <- function(cv_results) {
  cat("\n=== Cross-validation summary ===\n")
  
  info <- cv_results$cv_info
  
  if (!is.null(info)) {
    cat("Windows:", info$n_windows, "\n")
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
    cat("No model column available.\n")
    return(invisible(NULL))
  }
  
  if (!"crps" %in% names(metrics)) {
    cat("No CRPS scores available.\n")
    return(invisible(NULL))
  }
  
  if (!"crps_skill" %in% names(metrics)) {
    metrics$crps_skill <- NA_real_
  }
  
  if (!"n_forecasts" %in% names(metrics)) {
    metrics$n_forecasts <- NA_integer_
  }
  
  summary_table <- metrics |>
    dplyr::group_by(.data[[model_col]]) |>
    dplyr::summarise(
      mean_crps = finite_mean(crps),
      mean_skill = finite_mean(crps_skill),
      n_forecasts = if (all(is.na(n_forecasts))) {
        NA_integer_
      } else {
        sum(n_forecasts, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    dplyr::arrange(mean_crps)
  
  print(summary_table, n = Inf)
  invisible(summary_table)
}

cat("✓ evaluation.R loaded\n")