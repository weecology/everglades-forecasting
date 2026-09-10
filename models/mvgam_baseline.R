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
      newdata  = test_data,
      family   = model_family,
      silent   = 2,
      control  = list(adapt_delta = 0.99, max_treedepth = 12),
      chains   = config$chains,
      burnin   = config$burnin,
      samples  = config$samples
    )

    fc   <- forecast(model, newdata = test_data)
    crps <- extract_crps_mvgam(fc, model_name = "baseline")

    return(list(fc = fc, crps = crps))

  }, error = function(e) {
    cat("  ✗ Baseline model failed\n")
    cat(glue::glue("    Error: {e$message}\n"))
    warning("Baseline model failed: ", e$message)
    return(NULL)
  })
}




# #copilot notes
# 
# trend_model controls whether a latent temporal process is fitted and how it is 
# projected forward. Setting it to "None" leaves an intercept-only model with no 
# latent state to propagate; forecast() (and internal code expecting a latent 
#                                        state for multi-step forecasts) can fail 
# or produce nonsensical zero-length/empty-state errors ("argument of length 0"). 
# Adding mvgam::RW() supplies an explicit random-walk latent trend that the model 
# can estimate and the forecast routine can propagate, so forecast() succeeds.
# 
# Trade-offs to be aware of
# 
# Pros
# Better multi-step forecasts: the RW trend captures autocorrelation / nonstationary 
# structure so forecasts evolve through time rather than staying flat at the intercept.
# More realistic uncertainty: latent-state uncertainty is propagated forward, 
# giving predictive intervals that widen appropriately with horizon.
# Cons / costs
# More parameters / latent states to estimate → slower fits and higher computation 
# (longer sampling / more memory).
# Potential mixing/convergence issues on very short time series; you may need more 
# iterations, noncentred parametrization (you already set noncentred = TRUE), or 
# stronger priors.
# Risk of overfitting for tiny training windows: an RW can chase noise if the 
# training window is very short.
# If you also want to model cross-series differences, you may prefer series-specific 
# intercepts (formula including series) in addition to or instead of RW, 
# depending on goals.
# 
# Alternatives
# 
# If the intention is a pure intercept baseline across multiple series, use 
# series-specific intercepts (e.g., formula = y ~ -1 + series) so means differ 
# by series; then you can choose whether to add trend_model = mvgam::RW() for 
# time dynamics.
# If some runs have extremely short windows where RW causes poor sampling, you 
# can conditionally set trend_model based on train length (but that makes 
#                                                          behavior nonuniform 
#                                                          across experiments).
# 
# Test
# 
# After making the change, re-run your test harness (e.g., 
#                                                    test_single_model('baseline') 
#                                                    or test_single_window('baseline', ...)). 
# If you see slow sampling or warnings about convergence, increase burnin/samples 
# or inspect diagnostics (rhat, effective size) and consider tuning control/adapt_delta.
# 
# If you want, I can:
#   
#   Propose a small guarded patch that sets trend_model = mvgam::RW() only for 
# time series with sufficient length, or
# Create a PR patch that makes the change and adds a short comment explaining 
# why. Which would you prefer?
  