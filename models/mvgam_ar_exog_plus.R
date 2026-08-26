fit_mvgam_ar_exog_plus <- function(train_data, test_data, config) {
  cat("  Fitting AR exog plus model...\n")
  
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
  # ADAPTIVE KNOTS: cap k at number of unique values in training data
  # =========================================================================
  
  safe_k <- function(var, k_desired, k_min = 3) {
    n_unique <- length(unique(train_data[[var]]))
    max(k_min, min(k_desired, n_unique - 1))
  }
  
  k_depth    <- safe_k("breed_season_depth", 8)
  k_dry      <- safe_k("dry_days",           8)
  k_recession <- safe_k("recession",         6)
  k_init     <- safe_k("init_depth",         8)
  k_ti       <- max(3, min(5, k_depth - 1))
  
  cat(glue::glue("    Adaptive k: depth={k_depth}, dry={k_dry}, recession={k_recession}, init={k_init}, ti={k_ti}\n"))
  
  # Bake the numeric k values directly into the formula expression
  trend_form <- bquote(~
                         s(breed_season_depth, bs = 'cr', k = .(k_depth)) +
                         s(dry_days,           bs = 'cr', k = .(k_dry)) +
                         s(recession,          bs = 'cr', k = .(k_recession)) +
                         s(init_depth,         bs = 'cr', k = .(k_init)) +
                         ti(breed_season_depth, dry_days,  bs = 'cr', k = .(k_ti)) +
                         ti(breed_season_depth, recession, bs = 'cr', k = .(k_ti)) +
                         ti(init_depth,         dry_days,  bs = 'cr', k = .(k_ti))
  )
  
  # =========================================================================
  # FIT
  # =========================================================================
  
  tryCatch({
    model <- mvgam(
      formula       = count ~ 1,
      trend_formula = trend_form,
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
    crps <- extract_crps_mvgam(fc, model_name = "ar_exog_plus")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ AR exog plus model failed\n")
    cat(glue::glue("    Error: {e$message}\n"))
    warning("AR exog plus model failed: ", e$message)
    return(NULL)
  })
}