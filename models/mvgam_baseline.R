fit_mvgam_baseline <- function(train_data, test_data, config) {
  cat("  Fitting baseline model...\n")
  
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
  
  n_series <- length(unique(train_data$series))
  
  baseline_formula <- if (n_series > 1) {
    count ~ -1 + series
  } else {
    count ~ 1
  }
  
  cat(glue::glue("    Using formula: {deparse(baseline_formula)}\n"))
  cat(glue::glue("    N series: {n_series}\n"))
  
  tryCatch({
    model <- mvgam(
      formula  = baseline_formula,
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


