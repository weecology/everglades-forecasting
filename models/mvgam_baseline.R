fit_mvgam_baseline <- function(train_data, test_data, config) {
  cat("  Fitting baseline model...\n")
  
  model_family <- if (is.null(config$family) || is.na(config$family)) {
    nb()  
  } else if (config$family == "poisson") {
    poisson()
  } else if (config$family == "nb") {
    nb()
  } else if (config$family == "gaussian") {
    gaussian()
  } else {
    nb()
  }
  
  n_series <- length(unique(train_data$series))
  
  baseline_formula <- if (CONFIG$spatial$forecast_totals == TRUE) {                  # this needs to be different.... if species >1 then have +series but not if running individual species... 
    count ~ -1 + series                                                              # this goes in formula, if I can figure it out.... 
  } else {
    count ~ 1
  }
  
  cat(glue::glue("    Using formula: {deparse(baseline_formula)}\n"))
  cat(glue::glue("    N series: {n_series}\n"))
  # saveRDS(train_data, file = glue::glue("debug_train_{unique(train_data$series)[1]}.rds"))
  # saveRDS(test_data,  file = glue::glue("debug_test_{unique(train_data$series)[1]}.rds"))
  # saveRDS(config, file = "debug_config.rds")
  
  
  tryCatch({
    model <- mvgam(
      formula  = count ~ 1,
      data     = train_data,
      #newdata  = test_data,
      family   = model_family,
      silent   = 2,
      control  = list(adapt_delta = 0.99, max_treedepth = 12),
      chains   = config$chains,
      burnin   = config$burnin,
      samples  = config$samples
    )

    fc   <- forecast(model, newdata = test_data, type = "response")   
    crps <- extract_crps_mvgam(fc, model_name = "baseline")

    return(list(fc = fc, crps = crps))

  }, error = function(e) {
    cat("  ✗ Baseline model failed\n")
    cat(glue::glue("    Error: {e$message}\n"))
    warning("Baseline model failed: ", e$message)
    return(NULL)
  })
}




# type
# 
# When this has the value link (default) the linear predictor is calculated on the 
# link scale. If expected is used, predictions reflect the expectation of the response 
# (the mean) but ignore uncertainty in the observation process. When response is used, 
# the predictions take uncertainty in the observation process into account to return 
# predictions on the outcome scale. When variance is used, the variance of the response 
# with respect to the mean (mean-variance relationship) is returned. When type = "terms", 
# each component of the linear predictor is returned separately in the form of a list 
# (possibly with standard errors, if summary = TRUE): this includes parametric model 
# components, followed by each smooth component, but excludes any offset and any intercept. 
# Two special cases are also allowed: type latent_N will return the estimated latent 
# abundances from an N-mixture distribution, while type detection will return the 
# estimated detection probability from an N-mixture distribution
