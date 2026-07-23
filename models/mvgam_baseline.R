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
    NA
  }
  
  tryCatch({
    model <- mvgam(
      formula = count ~ 1,           
      trend_model = RW(),            
      #trend_model = RW() — This makes it a proper random walk baseline (the time-series equivalent of "just predict tomorrow will be like today")
      #this might want to be modified? 
      data = train_data,
      family = model_family,
      noncentred = TRUE,
      control = list(adapt_delta = 0.99, max_treedepth = 12),   
      chains = config$chains,
      burnin = config$burnin,
      samples = config$samples
    )
    
    fc <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "baseline")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ Baseline model failed\n")
    warning("Baseline model failed: ", e$message)
    return(NULL)
  })
}