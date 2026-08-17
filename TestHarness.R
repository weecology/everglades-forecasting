# =============================================================================
# TEST_HARNESS.R - Isolated testing of pipeline components
# =============================================================================

library(dplyr)
library(mvgam)
library(glue)

# Load necessary functions
source("data_functions.R")
source("evaluation.R")

# =============================================================================
# CONFIGURATION FOR TESTING
# =============================================================================

# Minimal test configuration
TEST_CONFIG <- list(
  # Data settings
  spatial = list(
    level = "system",  # or "subregion", "colony"
    include_species = c("gbhe", "greg"),  # Just 2 species for speed
    include_unknowns = FALSE,
    forecast_totals = FALSE,
    run_by_region = FALSE
  ),
  
  # Minimal MCMC for fast testing
  chains = 2,
  burnin = 200,
  samples = 200,
  
  # CV settings
  train_years = 5,
  test_years = 1,
  cv_windows = 1,  # Just test ONE window
  
  # Evaluation
  use_ordinal = FALSE,  # Disable for speed
  ordinal_years = "All",
  ordinal_breaks = c(0.33, 0.67, 0.90),
  sliding_window_breaks = FALSE,
  
  # Model settings
  family = "nb",
  
  # Parallel (disable for easier debugging)
  parallel = list(
    enabled = FALSE,
    workers = NULL
  ),
  
  # Cache
  cache = list(
    data = TRUE,
    models = FALSE
  ),
  
  # Data type
  data_type = "observed"
)

# Make it globally available
CONFIG <- TEST_CONFIG

# =============================================================================
# TEST FUNCTIONS
# =============================================================================

#' Test a single model in isolation
#' @param model_name Name of the model (e.g., "baseline", "ar", "trait")
#' @param framework Either "mvgam" or "fable"
#' @param use_full_data If FALSE, uses only recent years
test_single_model <- function(model_name, 
                              framework = "mvgam",
                              use_full_data = FALSE) {
  

  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat(glue("TESTING: {framework}_{model_name}\n"))
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  # Load data
  cat("Loading data...\n")
  data <- get_wading_bird_data(config = CONFIG, cache = TRUE)
  
  # Optionally subset to recent years for speed
  if (!use_full_data) {
    recent_years <- 10
    min_year <- max(data$year) - recent_years + 1
    data <- data |> filter(year >= min_year)
    cat(glue("Using recent {recent_years} years: {min_year}-{max(data$year)}\n"))
  }
  
  cat(glue("Data: {nrow(data)} rows, {n_distinct(data$species)} species\n\n"))
  
  # Load the specific model file
  if (framework == "mvgam") {
    model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
    if (!file.exists(model_file)) {
      stop("Model file not found: ", model_file)
    }
    source(model_file)
    cat(glue("✓ Loaded {model_file}\n\n"))
    
    # Run the model through CV
    results <- fit_sliding_window(
      data = data,
      make_forecast = make_mvgam_forecasts,
      train_years = CONFIG$train_years,
      test_years = CONFIG$test_years,
      cv_windows = CONFIG$cv_windows,
      parallel = FALSE,
      workers = NULL,
      models_to_run = model_name,
      use_ordinal = CONFIG$use_ordinal,
      precomputed_breaks = NULL
    )
    
  } else if (framework == "fable") {
    source("models/fable_models.R")
    cat("✓ Loaded fable models\n\n")
    
    results <- fit_sliding_window(
      data = data,
      make_forecast = make_fable_forecasts,
      train_years = CONFIG$train_years,
      test_years = CONFIG$test_years,
      cv_windows = CONFIG$cv_windows,
      parallel = FALSE,
      workers = NULL,
      models_to_run = model_name,
      use_ordinal = CONFIG$use_ordinal
    )
  }
  
  # Print results
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("RESULTS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  cat("Forecasts:", nrow(results$forecasts), "rows\n")
  cat("Metrics:", nrow(results$metrics), "rows\n\n")
  
  if (nrow(results$metrics) > 0) {
    print(results$metrics)
  }
  
  return(results)
}

#' Test with a single CV window and custom train/test split
#' @param train_start First year of training data
#' @param test_start First year of test data
test_single_window <- function(model_name,
                               train_start = 2010,
                               test_start = 2015,
                               framework = "mvgam") {
  

  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat(glue("TESTING SINGLE WINDOW: {train_start}-{test_start-1} → {test_start}\n"))
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  # Load data
  data <- get_wading_bird_data(config = CONFIG, cache = TRUE)
  
  # Split into train/test
  train_data <- data |> filter(year >= train_start & year < test_start)
  test_data <- data |> filter(year == test_start)
  
  cat(glue("Train: {nrow(train_data)} obs ({min(train_data$year)}-{max(train_data$year)})\n"))
  cat(glue("Test:  {nrow(test_data)} obs (year {test_start})\n\n"))
  
  # Load model
  if (framework == "mvgam") {
    model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
    source(model_file)
    
    # Prepare data for mvgam
    all_series <- unique(c(train_data$species, test_data$species))
    min_year <- min(train_data$year)
    
    train_data <- train_data |>
      mutate(
        time = as.integer(year - min_year + 1),
        series = factor(species, levels = all_series)
      ) |>
      as.data.frame()
    
    test_data <- test_data |>
      mutate(
        time = as.integer(year - min_year + 1),
        series = factor(species, levels = all_series)
      ) |>
      as.data.frame()
    
    # Run the forecast
    result <- make_mvgam_forecasts(
      train_data = train_data,
      test_data = test_data,
      models_to_run = model_name,
      use_ordinal = FALSE
    )
  }
  
  cat("\nResults:\n")
  print(result$metrics)
  
  return(result)
}

#' Test data loading with different configurations
test_data_loading <- function() {
 
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("TESTING DATA LOADING\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  configs_to_test <- list(
    system = list(level = "system", include_species = "top6"),
    subregion = list(level = "subregion", include_species = "top6"),
    colony = list(level = "colony", include_species = c("gbhe", "greg"))
  )
  
  for (name in names(configs_to_test)) {
    cat(glue("\n--- Testing: {name} ---\n"))
    
    test_config <- CONFIG
    test_config$spatial <- modifyList(CONFIG$spatial, configs_to_test[[name]])
    
    data <- get_wading_bird_data(config = test_config, cache = FALSE)
    
    cat(glue("  Rows: {nrow(data)}\n"))
    cat(glue("  Species: {paste(unique(data$species), collapse = ', ')}\n"))
    if ("region" %in% names(data)) {
      cat(glue("  Regions: {paste(unique(data$region), collapse = ', ')}\n"))
    }
    cat(glue("  Years: {min(data$year)}-{max(data$year)}\n"))
  }
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# Uncomment the test you want to run:

# Test baseline model quickly
# test_single_model("baseline", framework = "mvgam", use_full_data = FALSE)

# Test trait model with full data
# test_single_model("trait", framework = "mvgam", use_full_data = TRUE)

# Test a specific window
# test_single_window("baseline", train_start = 2010, test_start = 2018)

# Test data loading
# test_data_loading()

cat("\n✓ Test harness loaded. Run test functions as needed.\n")
cat("Examples:\n")
cat("  test_single_model('baseline')\n")
cat("  test_single_model('ar', use_full_data = TRUE)\n")
cat("  test_single_window('baseline', train_start = 2010, test_start = 2018)\n")
cat("  test_data_loading()\n\n")