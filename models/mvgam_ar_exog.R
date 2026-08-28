fit_mvgam_ar_exog <- function(train_data, test_data, config) {
  cat("  Fitting AR exog model...\n")
  
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
  # SCALE COVARIATES using training data statistics
  # =========================================================================
  
  covars <- c("breed_season_depth", "recession", "dry_days")
  
  scale_params <- lapply(covars, function(v) {
    list(mean = mean(train_data[[v]], na.rm = TRUE),
         sd   = sd(train_data[[v]],   na.rm = TRUE))
  })
  names(scale_params) <- covars
  
  scale_df <- function(df) {
    for (v in covars) {
      s <- scale_params[[v]]
      df[[v]] <- if (s$sd > 0) (df[[v]] - s$mean) / s$sd else df[[v]] - s$mean
    }
    df
  }
  
  train_data <- scale_df(train_data)
  test_data  <- scale_df(test_data)
  
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
      formula       = count ~ 1,
      trend_formula = ~ breed_season_depth + I(breed_season_depth^2) +
        recession + dry_days,
      trend_model   = mvgam::AR(p = 1),
      data          = train_data,
      family        = model_family,
      noncentred    = TRUE,
      control       = list(adapt_delta = 0.99, max_treedepth = 12),
      chains        = config$chains,
      burnin        = config$burnin,
      samples       = config$samples
    )
    
    fc   <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "ar_exog")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ AR exog model failed\n")
    cat(glue::glue("    Error: {e$message}\n"))
    warning("AR exog model failed: ", e$message)
    return(NULL)
  })
}