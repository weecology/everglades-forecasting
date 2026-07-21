# =============================================================================
# FABLE MODELS - Classical and regression time series models
# Fits models and returns raw forecasts + basic accuracy metrics
# Skill scores and RPS are computed in evaluation.R (consistent with mvgam)
# =============================================================================

library(fable)
library(fable.gam)
library(feasts)
library(tsibble)
library(dplyr)

# =============================================================================
# HELPER - Ensure correct tsibble structure
# =============================================================================

ensure_tsibble <- function(data, has_region) {
  if (is_tsibble(data)) {
    current_keys  <- key_vars(data)
    expected_keys <- if (has_region) c("species", "region") else "species"
    if (setequal(current_keys, expected_keys)) return(data)
  }
  
  df <- as_tibble(data)
  
  if (has_region) {
    as_tsibble(df, key = c(species, region), index = year)
  } else {
    as_tsibble(df, key = species, index = year)
  }
}

# =============================================================================
# MAIN FABLE FORECASTING FUNCTION
# =============================================================================

make_fable_forecasts <- function(train_data, test_data, models_to_run = NULL,
                                 use_ordinal = TRUE, config = CONFIG,
                                 precomputed_breaks = NULL) {
  
  # =========================================================================
  # SETUP
  # =========================================================================
  
  if (is.null(models_to_run)) {
    models_to_run <- c("baseline", "arima", "tslm", "arima_exog", "gam")
  }
  
  # Detect spatial level
  has_region <- "region" %in% names(as_tibble(train_data)) &&
    length(unique(as_tibble(train_data)$region)) > 1
  
  cat(glue::glue(
    "  Fable spatial: {ifelse(has_region, 'subregion/colony', 'system-wide')}\n"
  ))
  
  # Ensure correct tsibble keys
  train_data <- ensure_tsibble(train_data, has_region)
  test_data  <- ensure_tsibble(test_data,  has_region)
  
  cat(glue::glue(
    "  Keys: {paste(key_vars(train_data), collapse = ', ')} | ",
    "N series: {n_keys(train_data)}\n"
  ))
  
  # =========================================================================
  # BUILD MODEL SPECIFICATIONS
  # =========================================================================
  
  model_specs <- character()
  
  if ("baseline" %in% models_to_run) {
    model_specs <- c(model_specs, "baseline = RW(count)")                                     #"The future will be like today" (Random Walk)
  }
  if ("arima" %in% models_to_run) {
    model_specs <- c(model_specs, "arima = ARIMA(count)")
  }
  if ("tslm" %in% models_to_run) {
    model_specs <- c(model_specs,
                     "tslm = TSLM(count ~ breed_season_depth + I(breed_season_depth^2) +
         pre_recession + post_recession + recession + dry_days + trend())")
  }
  if ("arima_exog" %in% models_to_run) {
    model_specs <- c(model_specs,
                     "arima_exog = ARIMA(count ~ breed_season_depth + I(breed_season_depth^2) +
         pre_recession + post_recession + recession + dry_days)")
  }
  if ("gam" %in% models_to_run) {
    model_specs <- c(model_specs,
                     "gam = fable.gam::GAM(count ~ trend2(k = 4, bs = 'gp') +
         xreg(breed_season_depth, smooth = TRUE, k = 5) +
         xreg(dry_days, smooth = TRUE, k = 5) +
         xreg(recession, smooth = TRUE, k = 4))")
  }
  
  if (length(model_specs) == 0) {
    warning("No valid fable models specified")
    return(list(tibble(), tibble()))
  }
  
  # =========================================================================
  # FIT MODELS
  # =========================================================================
  
  model_call_str <- paste0(
    "model(train_data, ",
    paste(model_specs, collapse = ", "),
    ")"
  )
  
  cat(glue::glue("  Fitting {length(model_specs)} fable models...\n"))
  
  fitted_models <- tryCatch({
    eval(parse(text = model_call_str))
  }, error = function(e) {
    warning("Fable model fitting failed: ", e$message)
    cat("\nFailed model specs:\n", model_call_str, "\n")
    NULL
  })
  
  if (is.null(fitted_models)) {
    return(list(tibble(), tibble()))
  }
  
  cat("  ✓ Models fitted\n")
  
  # =========================================================================
  # GENERATE FORECASTS
  # =========================================================================
  
  forecasts_raw <- tryCatch({
    forecast(fitted_models, new_data = test_data)
  }, error = function(e) {
    warning("Fable forecasting failed: ", e$message)
    NULL
  })
  
  if (is.null(forecasts_raw)) {
    return(list(tibble(), tibble()))
  }
  
  cat(glue::glue("  ✓ Forecasts: {nrow(forecasts_raw)} rows\n"))
  
  # =========================================================================
  # BASIC ACCURACY METRICS (CRPS, RMSE)
  # Skill scores and RPS computed in evaluation.R
  # =========================================================================
  
  raw_metrics <- tryCatch({
    accuracy(forecasts_raw, test_data, list(crps = CRPS, rmse = RMSE))
  }, error = function(e) {
    warning("Fable accuracy() failed: ", e$message)
    NULL
  })
  
  if (is.null(raw_metrics) || nrow(raw_metrics) == 0) {
    warning("No fable metrics computed")
    return(list(forecasts_raw, tibble()))
  }
  
  cat(glue::glue("  ✓ Metrics: {nrow(raw_metrics)} rows\n"))
  
  return(list(forecasts_raw, raw_metrics))
}



cat("✓ fable_models.R loaded\n")




# something to experiment with later --------------------------------------

# 1. Covariate uncertainty — your exogenous variables (breed_season_depth, dry_days, recession) are themselves uncertain in the forecast period. 
# You can propagate this by estimating the variance of each covariate in the test window and adding a weighted contribution to σ.

# 2. Out-of-sample distance — how far the test covariates are from the training distribution. A natural measure is Mahalanobis distance from 
# the training covariate centroid, which inflates σ smoothly as forecasts venture into unfamiliar covariate space.

# Lambda is your key tuning parameter — it controls how aggressively uncertainty inflates with distance. You could treat it as a hyperparameter and
# tune it against your RPS scores on a validation window, which would tie it back neatly into your existing evaluation framework.

# Per-row vs. global covariate uncertainty — right now cov_uncertainty is a single scalar per forecast window. If your test period is long, you might 
# want a rolling window version that captures how covariate uncertainty evolves over time rather than summarizing the whole test period at once.
 
# Species-specific inflation — since you're already grouping by species, it's worth asking whether the same λ should apply across species or whether 
# some are more sensitive to covariate excursions. Do your species share the same covariate dynamics, or do some respond more nonlinearly?








# compute_fuzzy_sigma <- function(train_data, test_data, base_sigma,
#                                 covariate_cols = c("breed_season_depth", "dry_days", "recession"),
#                                 lambda = 0.5) {
#   train_mat <- train_data |>
#     as_tibble() |>
#     dplyr::select(all_of(covariate_cols)) |>
#     as.matrix()
#   
#   test_mat <- test_data |>
#     as_tibble() |>
#     dplyr::select(all_of(covariate_cols)) |>
#     as.matrix()
#   
#   # Covariate uncertainty: variance of each covariate in test window
#   test_cov_var <- apply(test_mat, 2, var, na.rm = TRUE)
#   
#   # Weighted covariate uncertainty contribution
#   cov_uncertainty <- sqrt(sum(test_cov_var))
#   
#   # Out-of-sample distance: Mahalanobis from training centroid
#   train_center <- colMeans(train_mat, na.rm = TRUE)
#   train_cov    <- cov(train_mat)
#   
#   mahal_dist <- apply(test_mat, 1, function(x) {
#     tryCatch(
#       sqrt(mahalanobis(x, center = train_center, cov = train_cov)),
#       error = function(e) NA_real_
#     )
#   })
#   
#   # Normalize distance to [0, 1] range using training distances as reference
#   train_mahal <- apply(train_mat, 1, function(x) {
#     tryCatch(
#       sqrt(mahalanobis(x, center = train_center, cov = train_cov)),
#       error = function(e) NA_real_
#     )
#   })
#   
#   max_train_dist <- quantile(train_mahal, 0.95, na.rm = TRUE)
#   norm_dist      <- pmin(mahal_dist / max_train_dist, 2)  # cap at 2x training range
#   
#   # Inflate sigma: base * (1 + lambda * norm_dist) + covariate uncertainty term
#   inflated_sigma <- (base_sigma * (1 + lambda * norm_dist)) + (lambda * cov_uncertainty)
#   
#   tibble(
#     inflated_sigma  = inflated_sigma,
#     mahal_dist      = mahal_dist,
#     norm_dist       = norm_dist,
#     cov_uncertainty = cov_uncertainty
#   )
# }
# 
# make_fuzzy_arima_forecasts <- function(train_data, test_data,
#                                        alpha_cuts = c(0.1, 0.5, 0.9),
#                                        lambda = 0.5) {
#   model_fit <- train_data |>
#     model(arima = ARIMA(count))
#   
#   fc <- forecast(model_fit, new_data = test_data)
#   
#   base_fc <- fc |>
#     as_tibble() |>
#     mutate(
#       m          = .mean,
#       base_sigma = sqrt(variance(count))
#     )
#   
#   sigma_inflation <- compute_fuzzy_sigma(
#     train_data, test_data,
#     base_sigma = mean(base_fc$base_sigma),
#     lambda     = lambda
#   )
#   
#   fuzzy_fc <- base_fc |>
#     bind_cols(sigma_inflation) |>
#     rowwise() |>
#     mutate(
#       fuzzy_intervals = list(
#         tibble(
#           alpha = alpha_cuts,
#           z     = sqrt(-2 * log(alpha_cuts)),
#           lower = m - z * inflated_sigma,
#           upper = m + z * inflated_sigma
#         )
#       )
#     ) |>
#     ungroup()
#   
#   fuzzy_fc
# }