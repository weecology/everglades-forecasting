fit_mvgam_trait <- function(train_data, test_data, config) {
  cat("  Fitting trait model...\n")
  
  # =========================================================================
  # DEFINE SPECIES TRAITS
  # =========================================================================
  
  trait_data <- data.frame(
    species = c("wost", "rosp", "whib", "sneg", "greg", "gbhe"),
    recession_weight = c(1.0, 0.2, 0.7, 0.5, 0.5, 0.1),
    optimal_depth = c(25, 15, 10, 12, 18, 40),
    stringsAsFactors = FALSE
  )
  
  # =========================================================================
  # ENRICH DATA WITH TRAIT-BASED FEATURES
  # =========================================================================
  
  train_enriched <- train_data %>%
    left_join(trait_data, by = "species") %>%
    mutate(
      recession_effect = recession * recession_weight,
      depth_deviation = abs(breed_season_depth - optimal_depth)
    )
  
  test_enriched <- test_data %>%
    left_join(trait_data, by = "species") %>%
    mutate(
      recession_effect = recession * recession_weight,
      depth_deviation = abs(breed_season_depth - optimal_depth)
    )
  
  # =========================================================================
  # FIT MODEL
  # =========================================================================
  
  model_family <- if (is.null(config$family)) {
    NA
  } else if (config$family == "poisson") {
    poisson()
  } else if (config$family == "nb") {
    nb()
  } else {
    NA
  }
  
  tryCatch({
    model <- mvgam(
      formula = count ~ series,
      trend_formula = ~ 
        recession_effect + 
        depth_deviation + 
        dry_days + 
        reversals,
      trend_model = mvgam::AR(p = 1),
      data = train_enriched,
      family = model_family,
      chains = config$chains,
      burnin = config$burnin,
      samples = config$samples,
      noncentred = TRUE,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
    )
    
    fc <- forecast(model, newdata = test_enriched)
    crps <- extract_crps_mvgam(fc, model_name = "trait")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ Trait model failed\n")
    warning("Trait model failed: ", e$message)
    return(NULL)
  })
}