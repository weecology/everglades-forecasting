fit_mvgam_species_specific <- function(train_data, test_data, config) {
  cat("  Fitting species-specific model...\n")
  
  
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
      formula = count ~ series,
      trend_formula = ~
        s(breed_season_depth, bs = 'cr', k = 5) +
        s(dry_days, bs = 'cr', k = 5) +
        s(breed_season_depth, by = trend, bs = 'sz', xt = list(bs = 'cr'),, k = 4) +
        s(dry_days, by = trend, bs = 'fs', k = 4),
      trend_model = mvgam::AR(),
      noncentred = TRUE,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      data = train_data,
      family = model_family,
      chains = config$chains,
      burnin = config$burnin,
      samples = config$samples
    )
    
    fc <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "species_specific")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ Species-specific model failed\n")
    warning("Species-specific model failed: ", e$message)
    return(NULL)
  })
}







# from copilot ------------------------------------------------------------

# bs = 'fs' is a factor-smooth interaction basis in mgcv and requires a factor 
# by variable. In mvgam, trend is a (numeric) trend index, so 
# s(..., by = trend, bs = 'fs') is likely to error or behave unexpectedly. 
# If the intent is a time-varying smooth coefficient, use the sz smooth with 
# a reduced k (to keep the model smaller) instead.

## sugestion 
# trend_formula = ~
#   s(breed_season_depth, bs = 'cr', k = 5) +
#   s(dry_days, bs = 'cr', k = 5) +
# s(breed_season_depth, trend, bs = 'sz', xt = list(bs = 'cr'), k = 4) +
#   s(dry_days, trend, bs = 'sz', xt = list(bs = 'cr'), k = 4),
# trend_model = mvgam::AR(),
# noncentred = TRUE,
# data = train_data,