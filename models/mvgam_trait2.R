fit_mvgam_trait2 <- function(train_data, test_data, config) {
  cat("  Fitting trait2 model (enhanced)...\n")
  
  # =========================================================================
  # DEFINE SPECIES TRAITS
  # =========================================================================
  
  trait_data <- tibble::tribble(
    ~species, ~foraging_guild,    ~optimal_depth_cm, ~recession_sensitivity, ~reversal_sensitivity, ~body_size,
    "wost",   "tactile_deep",     25,                1.00,                   1.00,                  "large",
    "rosp",   "tactile_shallow",  15,                0.20,                   0.80,                  "large",
    "whib",   "tactile_shallow",  10,                0.70,                   0.90,                  "medium",
    "sneg",   "visual_shallow",   12,                0.50,                   0.70,                  "small",
    "greg",   "visual_shallow",   18,                0.50,                   0.70,                  "medium",
    "gbhe",   "visual_deep",      40,                0.10,                   0.30,                  "large"
  )
  
  # =========================================================================
  # ENRICH DATA WITH TRAIT-BASED FEATURES
  # =========================================================================
  
  train_enriched <- train_data %>%
    as_tibble() %>%
    left_join(trait_data, by = "species") %>%
    mutate(
      depth_deviation = abs(breed_season_depth - optimal_depth_cm),
      recession_effect = recession * recession_sensitivity,
      reversal_effect = reversals * reversal_sensitivity,
      stress_index = depth_deviation * recession_effect,
      foraging_guild = factor(foraging_guild),
      body_size = factor(body_size, levels = c("small", "medium", "large")),
      series = factor(species)
    ) %>%
    as.data.frame()
  
  test_enriched <- test_data %>%
    as_tibble() %>%
    left_join(trait_data, by = "species") %>%
    mutate(
      depth_deviation = abs(breed_season_depth - optimal_depth_cm),
      recession_effect = recession * recession_sensitivity,
      reversal_effect = reversals * reversal_sensitivity,
      stress_index = depth_deviation * recession_effect,
      foraging_guild = factor(foraging_guild),
      body_size = factor(body_size, levels = c("small", "medium", "large")),
      series = factor(species)
    ) %>%
    as.data.frame()
  
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
      formula = count ~
        s(series, bs = 're') +
        s(foraging_guild, bs = 're'),
      
      trend_formula = ~
        depth_deviation + I(depth_deviation^2) +
        recession_effect +
        dry_days +
        reversal_effect + I(reversals > 2) +
        stress_index +
        s(foraging_guild, by = depth_deviation, bs = 're') +
        s(foraging_guild, by = dry_days, bs = 're') +
        s(body_size, bs = 're') +
        s(body_size, by = stress_index, bs = 're') +
        s(breed_season_depth, by = trend, bs = 'fs', k = 4) +  
        s(recession, by = trend, bs = 'fs', k = 4),
      
      trend_model = mvgam::AR(p = 1),
      data = train_enriched,
      family = model_family,
      chains = config$chains,
      burnin = config$burnin,
      samples = config$samples,
      priors = prior(normal(0, 2), class = 'b'),
      
      noncentred = TRUE,
      control = list(adapt_delta = 0.99, max_treedepth = 12)
    )
    
    fc <- forecast(model, newdata = test_enriched)
    crps <- extract_crps_mvgam(fc, model_name = "trait2")
    
    return(list(fc = fc, crps = crps))
    
  }, error = function(e) {
    cat("  ✗ Trait2 model failed\n")
    warning("Trait2 model failed: ", e$message)
    return(NULL)
  })
}