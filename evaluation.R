# =============================================================================
# EVALUATION.R - Cross-validation and forecast evaluation
# Handles both system-wide (species only) and subregional (species x region)
# Consistent evaluation for both mvgam and fable frameworks
# =============================================================================

library(dplyr)
library(tidyr)
library(verification)
library(distributional)
library(future)
library(furrr)
library(progressr)

# =============================================================================
# PARALLEL PROCESSING
# =============================================================================

setup_parallel <- function(enabled = TRUE, workers = NULL) {
  if (!enabled) {
    plan(sequential)
    return(list(enabled = FALSE, workers = 1))
  }
  
  if (is.null(workers)) {
    workers <- max(1, parallel::detectCores() - 1)
  }
  
  plan(multisession, workers = workers)
  cat(glue::glue("✓ Parallel processing enabled: {workers} workers\n"))
  
  return(list(enabled = TRUE, workers = workers))
}

# =============================================================================
# SLIDING WINDOW CROSS-VALIDATION
# =============================================================================

fit_sliding_window <- function(data,
                               make_forecast,
                               train_years,
                               test_years,
                               cv_windows = NULL,
                               parallel   = FALSE,
                               workers    = NULL,
                               ...) {
  
  year_min <- min(data$year)
  year_max <- max(data$year)
  
  train_starts <- year_min:(year_max - train_years - test_years + 1)
  test_starts  <- train_starts + train_years
  
  if (!is.null(cv_windows) && cv_windows < length(train_starts)) {
    train_starts <- tail(train_starts, cv_windows)
    test_starts  <- tail(test_starts,  cv_windows)
    cat(glue::glue("ℹ Using last {cv_windows} CV windows\n"))
  }
  
  n_windows <- length(train_starts)
  
  cat(glue::glue("\n=== Cross-Validation Setup ===\n"))
  cat(glue::glue("  Total years: {year_min}-{year_max}\n"))
  cat(glue::glue("  Train years: {train_years}\n"))
  cat(glue::glue("  Test years:  {test_years}\n"))
  cat(glue::glue("  CV windows:  {n_windows}\n\n"))
  cat(glue::glue("=== Fitting models across {n_windows} windows ===\n\n"))
  
  # Capture extra args for passing to make_fable_evaluation
  dots <- list(...)
  
  results_list <- lapply(seq_along(train_starts), function(i) {
    cat(glue::glue(
      "Window {i}/{n_windows}: ",
      "Train {train_starts[i]}-{test_starts[i]-1}, ",
      "Test {test_starts[i]}-{test_starts[i]+test_years-1}\n"
    ))
    
    train_data <- data |> filter(year >= train_starts[i] & year < test_starts[i])
    test_data  <- data |> filter(year >= test_starts[i]  & year < test_starts[i] + test_years)
    
    # Run forecast function
    forecast_and_metrics <- tryCatch({
      make_forecast(train_data, test_data, ...)
    }, error = function(e) {
      warning(glue::glue("Window {i} failed: {e$message}"))
      list(tibble(), tibble())
    })
    
    fc_raw  <- forecast_and_metrics[[1]]
    met_raw <- forecast_and_metrics[[2]]
    
    # -----------------------------------------------------------------------
    # FABLE: compute skill scores here (raw metrics come from fable_models.R)
    # -----------------------------------------------------------------------
    is_fable <- inherits(fc_raw, "fable") ||
      (is.data.frame(fc_raw) && ".model" %in% names(fc_raw) &&
         "count" %in% names(fc_raw) && inherits(fc_raw$count, "distribution"))
    
    if (is_fable && !is.null(met_raw) && nrow(met_raw) > 0) {
      met_out <- tryCatch({
        make_fable_evaluation(
          raw_metrics        = met_raw,
          forecasts          = fc_raw,
          test_data          = test_data,
          train_data         = train_data,
          config             = CONFIG,
          precomputed_breaks = dots$precomputed_breaks,
          use_ordinal        = dots$use_ordinal %||% FALSE
        ) |>
          as_tibble() |>
          mutate(test_start = test_starts[i], window = i)
      }, error = function(e) {
        warning(glue::glue("Fable evaluation window {i}: {e$message}"))
        tibble()
      })
    } else {
      # mvgam or empty - metrics already computed
      met_out <- tryCatch({
        met_raw |>
          as_tibble() |>
          mutate(test_start = test_starts[i], window = i)
      }, error = function(e) tibble())
    }
    
    # Safe forecast conversion
    fc_out <- tryCatch({
      fc_raw |>
        as_tibble() |>
        mutate(test_start = test_starts[i], window = i)
    }, error = function(e) {
      warning(glue::glue("Could not convert forecasts window {i}: {e$message}"))
      tibble()
    })
    
    list(forecasts = fc_out, metrics = met_out)
  })
  
  cat("\n=== Combining results ===\n")
  
  forecasts <- bind_rows(lapply(results_list, function(x) x$forecasts))
  metrics   <- bind_rows(lapply(results_list, function(x) x$metrics))
  
  cat(glue::glue(
    "✓ Complete! {nrow(forecasts)} forecasts, {nrow(metrics)} metric rows\n\n"
  ))
  
  return(list(
    forecasts = forecasts,
    metrics   = metrics,
    cv_info   = list(
      n_windows   = n_windows,
      train_years = train_years,
      test_years  = test_years
    )
  ))
}

# =============================================================================
# MVGAM EVALUATION - CENTRALIZED EXTRACTION
# =============================================================================

#' Extract CRPS scores from mvgam forecast object
extract_crps_mvgam <- function(forecast_obj, model_name) {
  crps_raw  <- score(forecast_obj, score = "crps")
  crps_list <- crps_raw[names(crps_raw) != "all_series"]
  
  bind_rows(lapply(names(crps_list), function(sp) {
    data.frame(
      series       = sp,
      score        = crps_list[[sp]]$score,
      eval_horizon = crps_list[[sp]]$eval_horizon,
      model        = model_name,
      stringsAsFactors = FALSE
    )
  }))
}

#' Calculate RPS for mvgam predictions
calculate_rps_mvgam <- function(predictions, test_data, train_data, config,
                                precomputed_breaks = NULL) {
  cat("  Calculating mvgam RPS...\n")
  
  has_region <- "region" %in% names(test_data) &&
    length(unique(test_data$region)) > 1
  
  group_vars <- if (has_region) c("species", "region") else "species"
  
  if (!is.null(precomputed_breaks)) {
    quantiles_by_group <- precomputed_breaks
  } else {
    quantiles_by_group <- train_data |>
      as_tibble() |>
      filter_ordinal_years(config$ordinal_years) |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        low    = quantile(count, config$ordinal_breaks[1], na.rm = TRUE),
        medium = quantile(count, config$ordinal_breaks[2], na.rm = TRUE),
        high   = quantile(count, config$ordinal_breaks[3], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  join_vars <- if (has_region && "region" %in% names(quantiles_by_group)) {
    c("species", "region")
  } else {
    "species"
  }
  
  test_data_ordinal <- test_data |>
    as_tibble() |>
    left_join(quantiles_by_group, by = join_vars) |>
    rowwise() |>
    mutate(
      count_category = cut(
        count,
        breaks = c(-Inf, low, medium, high, Inf),
        labels = c("Low", "Medium", "High", "Very High"),
        ordered = TRUE
      )
    ) |>
    ungroup()
  
  forecasts_probs <- predictions |>
    left_join(quantiles_by_group, by = join_vars) |>
    rowwise() |>
    mutate(
      pred_sd        = pmax((Q97.5 - Q2.5) / (2 * 1.96), 0.1),
      prob_low       = pnorm(low,    mean = Estimate, sd = pred_sd),
      prob_medium    = pnorm(medium, mean = Estimate, sd = pred_sd) - prob_low,
      prob_high      = pnorm(high,   mean = Estimate, sd = pred_sd) -
        pnorm(medium, mean = Estimate, sd = pred_sd),
      prob_very_high = 1 - pnorm(high, mean = Estimate, sd = pred_sd)
    ) |>
    ungroup()
  
  rps_group_vars <- if (has_region) c("model", "species", "region") else c("model", "species")
  
  rps_by_model <- forecasts_probs |>
    group_by(across(all_of(rps_group_vars))) |>
    summarise(
      rps = {
        cur_species <- unique(species)
        
        obs_filtered <- if (has_region) {
          cur_region <- unique(region)
          test_data_ordinal |>
            filter(species == cur_species, region == cur_region)
        } else {
          test_data_ordinal |>
            filter(species == cur_species)
        }
        
        obs_cat  <- obs_filtered |> pull(count_category) |> as.numeric()
        prob_mat <- pick(prob_low, prob_medium, prob_high, prob_very_high) |>
          as.matrix()
        
        mean(rps(obs_cat, prob_mat)$rps, na.rm = TRUE)
      },
      n_forecasts = n(),
      .groups = "drop"
    )
  
  baseline_join_vars <- if (has_region) c("species", "region") else "species"
  
  baseline_rps <- rps_by_model |>
    filter(model == "baseline") |>
    dplyr::select(all_of(c(baseline_join_vars, "rps"))) |>
    rename(rps_baseline = rps)
  
  rps_by_model |>
    left_join(baseline_rps, by = baseline_join_vars) |>
    mutate(
      rps_skill = if_else(
        is.na(rps_baseline) | is.nan(rps_baseline) | rps_baseline == 0,
        NA_real_,
        1 - rps / rps_baseline
      )
    )
}

#' Main mvgam forecasting and evaluation function
make_mvgam_forecasts <- function(train_data, test_data, models_to_run,
                                 use_ordinal = FALSE, precomputed_breaks = NULL) {
  
  # =========================================================================
  # LOAD MODEL FUNCTIONS
  # =========================================================================
  
  cat(glue::glue("\n  Checking {length(models_to_run)} mvgam model functions...\n"))
  
  for (model_name in models_to_run) {
    fit_fn <- paste0("fit_mvgam_", model_name)
    if (exists(fit_fn, envir = .GlobalEnv, inherits = TRUE)) {
      cat(glue::glue("    ✓ {model_name}\n"))
    } else {
      cat(glue::glue("    ✗ {model_name} - not found (should be loaded by main.R)\n"))
    }
  }
  
  # =========================================================================
  # DETECT SPATIAL LEVEL
  # =========================================================================
  
  has_region <- "region" %in% names(train_data) &&
    length(unique(train_data$region)) > 1
  min_year   <- min(train_data$year)
  
  cat(glue::glue(
    "  Spatial level: {ifelse(has_region, 'subregion/colony', 'system-wide')}\n"
  ))
  
  # =========================================================================
  # PREPARE DATA
  # =========================================================================
  
  if (has_region) {
    all_series <- unique(c(
      paste(train_data$species, train_data$region, sep = "_"),
      paste(test_data$species,  test_data$region,  sep = "_")
    ))
    
    train_data <- train_data |>
      mutate(
        time   = as.integer(year - min_year + 1),
        series = factor(paste(species, region, sep = "_"), levels = all_series)
      ) |>
      as_tibble() |>
      as.data.frame()
    
    test_data <- test_data |>
      mutate(
        time   = as.integer(year - min_year + 1),
        series = factor(paste(species, region, sep = "_"), levels = all_series)
      ) |>
      as_tibble() |>
      as.data.frame()
    
  } else {
    all_series <- unique(c(train_data$species, test_data$species))
    
    train_data <- train_data |>
      mutate(
        time   = as.integer(year - min_year + 1),
        series = factor(species, levels = all_series)
      ) |>
      as_tibble() |>
      as.data.frame()
    
    test_data <- test_data |>
      mutate(
        time   = as.integer(year - min_year + 1),
        series = factor(species, levels = all_series)
      ) |>
      as_tibble() |>
      as.data.frame()
  }
  
  train_data_original <- train_data
  
  cat(glue::glue("  N series: {length(all_series)}\n"))
  
  # =========================================================================
  # YEAR MAPPING FOR FORECAST EXTRACTION
  # =========================================================================
  
  n_test_years <- length(unique(test_data$year))
  
  if (has_region) {
    test_years_by_series <- test_data |>
      as_tibble() |>
      mutate(series_id = paste(species, region, sep = "_")) |>
      dplyr::select(series_id, year) |>
      distinct() |>
      group_by(series_id) |>
      arrange(year) |>
      mutate(forecast_index = row_number()) |>
      ungroup()
  } else {
    test_years_by_series <- test_data |>
      as_tibble() |>
      dplyr::select(species, year) |>
      distinct() |>
      group_by(species) |>
      arrange(year) |>
      mutate(forecast_index = row_number()) |>
      ungroup() |>
      rename(series_id = species)
  }
  
  cat(glue::glue("  Test years: {n_test_years} per series\n"))
  
  # =========================================================================
  # FIT ALL MODELS
  # =========================================================================
  
  cat(glue::glue("\n  Fitting {length(models_to_run)} mvgam models...\n"))
  
  results <- list()
  
  for (model_name in models_to_run) {
    cat(glue::glue("    • {model_name}"))
    
    fit_fn <- paste0("fit_mvgam_", model_name)
    
    if (!exists(fit_fn, envir = .GlobalEnv, inherits = TRUE)) {
      cat(" ✗ (not found)\n")
      next
    }
    
    result <- tryCatch({
      get(fit_fn, envir = .GlobalEnv)(train_data, test_data, CONFIG)
    }, error = function(e) {
      cat(" ✗\n")
      warning(glue::glue("    {model_name} failed: {e$message}"))
      NULL
    })
    
    if (!is.null(result)) {
      results[[model_name]] <- result
      cat(" ✓\n")
    }
  }
  
  # =========================================================================
  # EXTRACT AND FORMAT ALL FORECASTS
  # =========================================================================
  
  if (length(results) == 0) {
    warning("All mvgam models failed")
    return(list(predictions = tibble(), metrics = tibble()))
  }
  
  cat("\n  Extracting forecasts...\n")
  
  all_preds <- bind_rows(lapply(names(results), function(model_name) {
    result <- results[[model_name]]
    if (is.null(result$fc)) return(tibble())
    
    fc_summary <- summary(result$fc)
    
    fc_only <- fc_summary |>
      as_tibble() |>
      group_by(series) |>
      slice(1:n_test_years) |>
      ungroup()
    
    fc_out <- fc_only |>
      rename(
        Estimate = predQ50,
        Q2.5     = predQ2.5,
        Q97.5    = predQ97.5
      ) |>
      mutate(
        series_id      = as.character(series),
        forecast_index = time
      ) |>
      left_join(test_years_by_series, by = c("series_id", "forecast_index"))
    
    if (has_region) {
      fc_out <- fc_out |>
        mutate(
          species = sub("_[^_]+$", "", series_id),
          region  = sub(".*_",     "", series_id)
        )
    } else {
      fc_out <- fc_out |>
        mutate(species = series_id) |>
        dplyr::select(-any_of("region"))
    }
    
    fc_out |>
      mutate(model = model_name) |>
      dplyr::select(Estimate, Q2.5, Q97.5, species, any_of("region"), year, model)
  }))
  
  all_crps <- bind_rows(lapply(results, function(x) x$crps))
  
  expected_n <- length(results) * length(all_series) * n_test_years
  cat(glue::glue("  ✓ Extracted {nrow(all_preds)} predictions\n"))
  cat(glue::glue("  Expected:  {expected_n}\n"))
  
  na_count <- sum(is.na(all_preds$year))
  if (na_count > 0) {
    warning(glue::glue("  ⚠️  {na_count} predictions have NA years"))
  } else {
    cat("  ✓ All forecasts have valid years\n")
  }
  
  if (nrow(all_crps) == 0) {
    return(list(predictions = all_preds, metrics = tibble()))
  }
  
  # =========================================================================
  # DECODE CRPS SERIES LABELS
  # =========================================================================
  
  if (has_region) {
    all_crps <- all_crps |>
      mutate(
        species = sub("_[^_]+$", "", series),
        region  = sub(".*_",     "", series)
      )
  } else {
    all_crps <- all_crps |>
      mutate(species = series) |>
      dplyr::select(-any_of("region"))
  }
  
  # =========================================================================
  # CRPS SKILL SCORES
  # =========================================================================
  
  has_region_crps <- has_region && "region" %in% names(all_crps)
  group_vars      <- if (has_region_crps) c("model", "species", "region") else c("model", "species")
  baseline_vars   <- if (has_region_crps) c("species", "region")           else "species"
  
  baseline_summary <- all_crps |>
    filter(model == "baseline") |>
    group_by(across(all_of(baseline_vars))) |>
    summarize(crps_baseline = mean(score, na.rm = TRUE), .groups = "drop")
  
  skills <- all_crps |>
    left_join(baseline_summary, by = baseline_vars) |>
    group_by(across(all_of(group_vars))) |>
    summarize(
      crps          = mean(score, na.rm = TRUE),
      crps_baseline = first(crps_baseline),
      crps_skill    = if_else(
        is.na(crps_baseline) | is.nan(crps_baseline) | crps_baseline == 0,
        NA_real_,
        1 - (crps / crps_baseline)
      ),
      n_forecasts = n(),
      .groups = "drop"
    )
  
  # =========================================================================
  # RPS
  # =========================================================================
  
  if (use_ordinal) {
    cat("\n  Adding ordinal evaluation (RPS)...\n")
    rps_scores <- tryCatch({
      calculate_rps_mvgam(
        all_preds, test_data, train_data_original, CONFIG, precomputed_breaks
      )
    }, error = function(e) {
      warning(glue::glue("RPS failed: {e$message}"))
      NULL
    })
    
    if (!is.null(rps_scores)) {
      rps_join_vars <- if (has_region_crps) c("model", "species", "region") else c("model", "species")
      rps_select    <- c(rps_join_vars, "rps", "rps_skill")
      skills <- skills |>
        left_join(
          rps_scores |> dplyr::select(all_of(rps_select)),
          by = rps_join_vars
        )
    }
  }
  
  cat("\n")
  return(list(predictions = all_preds, metrics = skills))
}

# =============================================================================
# FABLE EVALUATION - SKILL SCORES + RPS
# =============================================================================

#' Calculate RPS for fable predictions
calculate_rps_fable <- function(forecasts, test_data, train_data, config,
                                precomputed_breaks = NULL) {
  
  has_region <- "region" %in% names(as_tibble(test_data)) &&
    length(unique(as_tibble(test_data)$region)) > 1
  
  group_vars <- if (has_region) c("species", "region") else "species"
  
  # Ordinal breaks
  if (!is.null(precomputed_breaks)) {
    quantiles_by_group <- precomputed_breaks
  } else {
    quantiles_by_group <- train_data |>
      as_tibble() |>
      filter_ordinal_years(config$ordinal_years) |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        low    = quantile(count, config$ordinal_breaks[1], na.rm = TRUE),
        medium = quantile(count, config$ordinal_breaks[2], na.rm = TRUE),
        high   = quantile(count, config$ordinal_breaks[3], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Forecast probabilities
  forecasts_probs <- forecasts |>
    as_tibble() |>
    left_join(quantiles_by_group, by = group_vars) |>
    rowwise() |>
    mutate(
      pred_sd        = sqrt(distributional::variance(count)),
      prob_low       = pnorm(low,    mean = .mean, sd = pred_sd),
      prob_medium    = pnorm(medium, mean = .mean, sd = pred_sd) - prob_low,
      prob_high      = pnorm(high,   mean = .mean, sd = pred_sd) -
        pnorm(medium, mean = .mean, sd = pred_sd),
      prob_very_high = 1 - pnorm(high, mean = .mean, sd = pred_sd)
    ) |>
    ungroup()
  
  # Ordinal observations
  test_data_ordinal <- test_data |>
    as_tibble() |>
    left_join(quantiles_by_group, by = group_vars) |>
    rowwise() |>
    mutate(
      count_category = cut(
        count,
        breaks = c(-Inf, low, medium, high, Inf),
        labels = c("Low", "Medium", "High", "Very High"),
        ordered = TRUE
      )
    ) |>
    ungroup()
  
  rps_group_vars <- c(".model", group_vars)
  
  rps_by_model <- forecasts_probs |>
    group_by(across(all_of(rps_group_vars))) |>
    summarise(
      rps = {
        cur_species <- unique(species)
        
        obs_filtered <- if (has_region) {
          cur_region <- unique(region)
          test_data_ordinal |>
            filter(species == cur_species, region == cur_region)
        } else {
          test_data_ordinal |>
            filter(species == cur_species)
        }
        
        obs_cat  <- obs_filtered |> pull(count_category) |> as.numeric()
        prob_mat <- pick(prob_low, prob_medium, prob_high, prob_very_high) |>
          base::as.matrix()
        
        mean(rps(obs_cat, prob_mat)$rps, na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  # Skill scores
  baseline_rps <- rps_by_model |>
    filter(.model == "baseline") |>
    dplyr::select(all_of(c(group_vars, "rps"))) |>
    rename(rps_baseline = rps)
  
  rps_by_model |>
    left_join(baseline_rps, by = group_vars) |>
    mutate(
      rps_skill = if_else(
        is.na(rps_baseline) | is.nan(rps_baseline) | rps_baseline == 0,
        NA_real_,
        1 - rps / rps_baseline
      )
    )
}

#' Compute skill scores from raw fable accuracy metrics
make_fable_evaluation <- function(raw_metrics, forecasts, test_data,
                                  train_data, config, precomputed_breaks,
                                  use_ordinal) {
  
  has_region <- "region" %in% names(as_tibble(test_data)) &&
    length(unique(as_tibble(test_data)$region)) > 1
  
  key_cols  <- if (has_region) c("species", "region") else "species"
  join_cols <- c(intersect(key_cols, names(raw_metrics)), ".type")
  
  # =========================================================================
  # CRPS AND RMSE SKILL SCORES
  # =========================================================================
  
  baselines <- raw_metrics |> filter(.model == "baseline")
  
  if (nrow(baselines) == 0) {
    warning("No fable baseline found - returning raw metrics")
    return(raw_metrics)
  }
  
  metrics <- raw_metrics |>
    left_join(baselines, by = join_cols, suffix = c("", "_baseline")) |>
    mutate(
      crps_skill = if_else(
        is.na(crps_baseline) | is.nan(crps_baseline) | crps_baseline == 0,
        NA_real_,
        1 - crps / crps_baseline
      ),
      rmse_skill = if_else(
        is.na(rmse_baseline) | is.nan(rmse_baseline) | rmse_baseline == 0,
        NA_real_,
        1 - rmse / rmse_baseline
      )
    ) |>
    dplyr::select(-.model_baseline)
  
  # =========================================================================
  # RPS
  # =========================================================================
  
  if (isTRUE(use_ordinal)) {
    rps_metrics <- tryCatch({
      calculate_rps_fable(
        forecasts, test_data, train_data,
        config             = config,
        precomputed_breaks = precomputed_breaks
      )
    }, error = function(e) {
      warning(glue::glue("Fable RPS failed: {e$message}"))
      NULL
    })
    
    if (!is.null(rps_metrics) && nrow(rps_metrics) > 0) {
      rps_join <- intersect(
        c(".model", "species", "region"),
        intersect(names(metrics), names(rps_metrics))
      )
      metrics <- metrics |>
        left_join(
          rps_metrics |>
            dplyr::select(all_of(c(rps_join, "rps", "rps_skill"))),
          by = rps_join
        )
    }
  }
  
  return(metrics)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Filter data to recent years for ordinal break calculation
filter_ordinal_years <- function(df, ordinal_years) {
  if (!identical(ordinal_years, "All")) {
    n_years <- as.integer(ordinal_years)
    df <- df |>
      filter(year >= max(year, na.rm = TRUE) - n_years + 1)
  }
  df
}

#' Print cross-validation summary
print_cv_summary <- function(cv_results) {
  cat("\n=== Cross-Validation Summary ===\n")
  
  if (!is.null(cv_results$cv_info)) {
    cat(glue::glue("Windows:     {cv_results$cv_info$n_windows}\n"))
    cat(glue::glue("Train years: {cv_results$cv_info$train_years}\n"))
    cat(glue::glue("Test years:  {cv_results$cv_info$test_years}\n\n"))
  }
  
  if (!is.null(cv_results$metrics) && nrow(cv_results$metrics) > 0) {
    cat("=== Model Performance ===\n")
    
    metrics <- cv_results$metrics
    
    # Handle both mvgam ("model") and fable (".model")
    if ("model" %in% names(metrics)) {
      model_col <- "model"
    } else if (".model" %in% names(metrics)) {
      model_col <- ".model"
    } else {
      cat("⚠️  No model column found\n")
      return(invisible(NULL))
    }
    
    if (!"crps" %in% names(metrics)) {
      cat("⚠️  No CRPS column found\n")
      return(invisible(NULL))
    }
    
    summary_by_model <- metrics |>
      group_by(.data[[model_col]]) |>
      summarise(
        mean_crps   = mean(crps, na.rm = TRUE),
        mean_skill  = mean(crps_skill, na.rm = TRUE),
        n_forecasts = if ("n_forecasts" %in% names(metrics)) {
          sum(n_forecasts, na.rm = TRUE)
        } else {
          n()
        },
        .groups = "drop"
      ) |>
      arrange(mean_crps)
    
    print(summary_by_model, n = Inf)
  }
  
  cat("\n")
}

cat("✓ evaluation.R loaded\n")