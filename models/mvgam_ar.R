fit_mvgam_ar <- function(train_data, test_data, config) {
  cat("  Fitting AR model...\n")
  
  # =========================================================================
  # FAMILY
  # =========================================================================
  
  model_family <- if (is.null(config$family)) {
    NA
  } else if (config$family == "poisson") {
    poisson()
  } else if (config$family == "nb") {
    nb()
  } else if (config$family == "gaussian") {
    gaussian()
  } else {
    nb()
  }
  
  # =========================================================================
  # GUARD: ensure series factor levels are consistent
  # =========================================================================
  
  if (!is.null(train_data$series) && !is.null(test_data$series)) {
    all_levels <- union(levels(train_data$series), levels(test_data$series))
    train_data$series <- factor(train_data$series, levels = all_levels)
    test_data$series  <- factor(test_data$series,  levels = all_levels)
  }
  
  # =========================================================================
  # FIT
  # =========================================================================
  
  tryCatch({
    model <- mvgam(
      formula      = count ~ 1,
      trend_formula = ~ s(breed_season_depth, bs = 'cr', k = 5) +
        s(dry_days,           bs = 'cr', k = 5) +
        s(recession,          bs = 'cr', k = 5),
      trend_model  = mvgam::AR(p = 1),
      data         = train_data,
      family       = model_family,
      noncentred   = TRUE,
      control      = list(adapt_delta = 0.99, max_treedepth = 12),
      chains       = config$chains,
      burnin       = config$burnin,
      samples      = config$samples
    )
    
    fc   <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "ar")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ AR model failed\n")
    cat(glue::glue("    Error: {e$message}\n"))
    warning("AR model failed: ", e$message)
    return(NULL)
  })
}