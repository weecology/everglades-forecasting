fit_mvgam_ar_exog_plus <- function(train_data, test_data, config) {
  cat("  Fitting AR exog plus model...\n")
  
  
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
      trend_formula = ~  
        s(breed_season_depth, bs = 'cr', k = 8) +
        s(dry_days, bs = 'cr', k = 8) +
        s(recession, bs = 'cr', k = 6) +
        s(init_depth, bs = 'cr', k = 8) +
        ti(breed_season_depth, dry_days, bs = 'cr', k = 5) +
        ti(breed_season_depth, recession, bs = 'cr', k = 5) +
        ti(init_depth, dry_days, bs = 'cr', k = 5),
      trend_model = mvgam::AR(),
      data = train_data,
      family = model_family,
      noncentred = TRUE,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      chains = config$chains,
      burnin = config$burnin,
      samples = config$samples
    )
    
    fc <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "ar_exog_plus")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ AR exog plus model failed\n")
    warning("AR exog plus model failed: ", e$message)
    return(NULL)
  })
}