fit_mvgam_species_specific <- function(train_data, test_data, config) {
  cat("  Fitting species-specific model...\n")
  
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
  # ADAPTIVE KNOTS
  # =========================================================================
  
  safe_k <- function(var, k_desired, k_min = 3) {
    n_unique <- length(unique(train_data[[var]]))
    max(k_min, min(k_desired, n_unique - 1))
  }
  
  n_series <- length(unique(train_data$series))
  k_depth  <- safe_k("breed_season_depth", 5)
  k_dry    <- safe_k("dry_days", 5)
  
  cat(glue::glue("    Adaptive k: depth={k_depth}, dry={k_dry}, n_series={n_series}\n"))
  
  # =========================================================================
  # BUILD TREND FORMULA via bquote() — observation formula stays static
  # =========================================================================
  
  trend_form <- if (n_series >= 4) {
    k_sz <- max(3, min(4, n_series - 1, k_depth - 1, k_dry - 1))
    cat(glue::glue("    Using sz smooths, k_sz={k_sz}\n"))
    bquote(~
             s(breed_season_depth, bs = 'cr', k = .(k_depth)) +
             s(dry_days,           bs = 'cr', k = .(k_dry)) +
             s(breed_season_depth, trend, bs = 'sz', xt = list(bs = 'cr'), k = .(k_sz)) +
             s(dry_days,           trend, bs = 'sz', xt = list(bs = 'cr'), k = .(k_sz))
    )
  } else {
    cat(glue::glue("    Using shared smooths only in trend_formula (n_series={n_series})\n"))
    bquote(~
             s(breed_season_depth, bs = 'cr', k = .(k_depth)) +
             s(dry_days,           bs = 'cr', k = .(k_dry))
    )
  }
  
  # =========================================================================
  # FIT
  # =========================================================================
  
  tryCatch({
    model <- mvgam(
      formula       = count ~ series,   # static — avoids "no terms component" error
      trend_formula = trend_form,
      trend_model   = mvgam::AR(p = 1),
      noncentred    = TRUE,
      control       = list(adapt_delta = 0.99, max_treedepth = 12),
      data          = train_data,
      family        = model_family,
      chains        = config$chains,
      burnin        = config$burnin,
      samples       = config$samples
    )
    
    # ------------------------------------------------------------------
    # GUARD: forecast() can return non-conformable matrices when
    # n_test_timepoints == 1.  Pad test_data with a duplicate row so
    # the posterior draw matrix is always [n_samples × ≥2], then drop
    # the extra timepoint from the forecast object afterwards.
    # ------------------------------------------------------------------
    n_test_times <- length(unique(test_data$time))
    
    if (n_test_times == 1) {
      # Duplicate every row with time + 1 so mvgam sees 2 timepoints
      pad <- test_data
      pad$time  <- pad$time  + 1L
      pad$count <- NA_real_
      test_data_fc <- rbind(test_data, pad)
    } else {
      test_data_fc <- test_data
    }
    
    fc_full <- forecast(model, newdata = test_data_fc)
    
    # Trim back to the real test timepoints if we padded
    if (n_test_times == 1) {
      real_times <- unique(test_data$time)
      fc_full$forecasts <- lapply(fc_full$forecasts, function(m) {
        if (is.matrix(m)) m[, 1, drop = FALSE] else m
      })
      fc_full$test_observations <- lapply(fc_full$test_observations, function(v) v[1])
    }
    
    fc   <- fc_full
    crps <- extract_crps_mvgam(fc, model_name = "species_specific")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ Species-specific model failed\n")
    cat(glue::glue("    Error: {e$message}\n"))
    warning("Species-specific model failed: ", e$message)
    return(NULL)
  })
}