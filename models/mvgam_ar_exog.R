fit_mvgam_ar_exog <- function(train_data, test_data, config) {
  cat("  Fitting AR exog model...\n")
  
  
  model_family <- if (is.null(config$family)) {
    NA
  } else if (config$family == "poisson") {
    poisson()
  } else if (config$family == "nb") {
    nb()
  } else if (config$family == "gaussian") {
    gaussian()
  } else {
    NA
  }
  
  
  tryCatch({
    model <- mvgam(
      formula = count ~ 1,
      trend_formula = ~ breed_season_depth + I(breed_season_depth^2) +
        recession + dry_days,
      trend_model = mvgam::AR(p = 1),
      data = train_data,
      family = model_family,
      noncentred = TRUE,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      chains = config$chains,
      burnin = config$burnin,
      samples = config$samples
    )
    
    fc <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "ar_exog")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ AR exog model failed\n")
    warning("AR exog model failed: ", e$message)
    return(NULL)
  })
}