# =============================================================================
# TestHarness.R — isolated testing of pipeline components
# Works on a local laptop and HiPerGator
# Run from the project root directory.
# =============================================================================

# --- Environment-specific setup ----------------------------------------------

is_hipergator <- dir.exists("/apps")

if (is_hipergator) {
  blue_library <- "/blue/ewhite/alexanderblochel/R/libs"
  
  if (!dir.exists(blue_library)) {
    stop("HiPerGator R library not found: ", blue_library)
  }
  
  # Put the Blue library first; retain R's standard libraries.
  .libPaths(c(blue_library, .libPaths()))
  
  # Required before loading units/sf/stars on this HiPerGator setup.
  udunits_library <- "/apps/udunits/2.2.17/lib/libudunits2.so.0"
  if (!file.exists(udunits_library)) {
    stop("UDUNITS library not found: ", udunits_library)
  }
  dyn.load(udunits_library)
}

# --- Check packages before attaching them -------------------------------------

required_packages <- c(
  "units", "sf", "stars", "dplyr", "mvgam", "glue",
  "future", "conflicted", "digest", "edenR", "readr",
  "tidyr", "tsibble", "wader", "zoo"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Required packages are not available in the active R library: ",
    paste(missing_packages, collapse = ", "),
    "\nActive libraries:\n",
    paste(.libPaths(), collapse = "\n")
  )
}

# --- Attach packages -----------------------------------------------------------

library(units)
library(sf)
library(stars)
library(dplyr)
library(mvgam)
library(glue)
library(future)
library(conflicted)

conflicted::conflicts_prefer(dplyr::filter,   .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select,   .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::AR,       .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::VAR,      .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::RW,       .quiet = TRUE)
conflicted::conflicts_prefer(base::as.matrix, .quiet = TRUE)
conflicted::conflicts_prefer(base::get,       .quiet = TRUE)

# --- Load project functions ----------------------------------------------------

if (!file.exists("data_functions.R")) {
  stop("Run this script from the project root; data_functions.R was not found.")
}
if (!file.exists("evaluation.R")) {
  stop("Run this script from the project root; evaluation.R was not found.")
}

source("data_functions.R")
source("evaluation.R")

# =============================================================================
# Configuration for testing
# =============================================================================

TEST_CONFIG <- list(
  spatial = list(
    level = "subregion",
    include_species = "all",
    include_unknowns = FALSE,
    forecast_totals = TRUE,
    run_by_region = FALSE,
    exclude_regions = c("bigcypress", "coastal")
  ),
  
  chains = 2,
  burnin = 200,
  samples = 200,
  
  train_years = 5,
  test_years = 1,
  cv_windows = 1,
  
  use_ordinal = TRUE,
  ordinal_years = "All",
  ordinal_breaks = c(0.33, 0.67, 0.90),
  sliding_window_breaks = FALSE,
  
  family = "nb",
  
  # Sequential by default. Use parallel=TRUE only when your laptop or
  # HiPerGator allocation has enough CPUs for the requested workers.
  parallel = list(
    enabled = FALSE,
    workers = 1
  ),
  
  cache = list(
    data = TRUE,
    models = FALSE
  ),
  
  data_type = "observed"
)

CONFIG <- TEST_CONFIG

# =============================================================================
# Test functions
# =============================================================================

test_single_model <- function(model_name,
                              framework = "mvgam",
                              use_full_data = FALSE,
                              parallel = FALSE,
                              workers = NULL) {
  if (!framework %in% c("mvgam", "fable")) {
    stop("framework must be either 'mvgam' or 'fable'.")
  }
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat(glue("Testing {framework}_{model_name}; parallel={parallel}\n"))
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  cat("Loading data...\n")
  data <- get_wading_bird_data(config = CONFIG, cache = TRUE)
  
  if (!use_full_data) {
    recent_years <- 10
    min_year <- max(data$year, na.rm = TRUE) - recent_years + 1
    data <- data |> filter(year >= min_year)
    cat(glue("Using recent years from {min_year} to {max(data$year)}\n"))
  }
  
  cat(glue(
    "Data: {nrow(data)} rows; ",
    "{dplyr::n_distinct(data$species)} species\n\n"
  ))
  
  if (framework == "mvgam") {
    model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
    if (!file.exists(model_file)) {
      stop("Model file not found: ", model_file)
    }
    
    source(model_file)
    
    make_forecast_fn <- function(train, test, ...) {
      source("evaluation.R", local = FALSE)
      source(file.path("models", "mvgam_baseline.R"), local = FALSE)
      source(file.path("models", paste0("mvgam_", model_name, ".R")),
             local = FALSE)
      make_mvgam_forecasts(train, test, ...)
    }
    
    results <- fit_sliding_window(
      data = data,
      make_forecast = make_forecast_fn,
      train_years = CONFIG$train_years,
      test_years = CONFIG$test_years,
      cv_windows = CONFIG$cv_windows,
      parallel = parallel,
      workers = workers,
      models_to_run = model_name,
      use_ordinal = CONFIG$use_ordinal,
      precomputed_breaks = NULL
    )
    
  } else {
    model_file <- file.path("models", "fable_models.R")
    if (!file.exists(model_file)) {
      stop("Model file not found: ", model_file)
    }
    
    source(model_file)
    
    make_forecast_fn <- function(train, test, ...) {
      source("evaluation.R", local = FALSE)
      source("models/fable_models.R", local = FALSE)
      make_fable_forecasts(train, test, ...)
    }
    
    results <- fit_sliding_window(
      data = data,
      make_forecast = make_forecast_fn,
      train_years = CONFIG$train_years,
      test_years = CONFIG$test_years,
      cv_windows = CONFIG$cv_windows,
      parallel = parallel,
      workers = workers,
      models_to_run = model_name,
      use_ordinal = CONFIG$use_ordinal
    )
  }
  
  cat("\nResults\n")
  if (!is.null(results$forecasts)) {
    cat("Forecast rows:", nrow(results$forecasts), "\n")
  }
  if (!is.null(results$metrics)) {
    cat("Metric rows:", nrow(results$metrics), "\n")
    if (nrow(results$metrics) > 0) print(results$metrics)
  }
  
  invisible(results)
}

test_single_window <- function(model_name,
                               train_start = 2010,
                               test_start = 2015,
                               framework = "mvgam") {
  if (framework != "mvgam") {
    stop("test_single_window currently supports framework='mvgam' only.")
  }
  
  cat(glue(
    "\nTesting window: {train_start}-{test_start - 1} -> {test_start}\n"
  ))
  
  data <- get_wading_bird_data(config = CONFIG, cache = TRUE)
  
  train_data <- data |> filter(year >= train_start, year < test_start)
  test_data <- data |> filter(year == test_start)
  
  if (nrow(train_data) == 0) stop("Training data is empty for this split.")
  if (nrow(test_data) == 0) stop("Test data is empty for this split.")
  
  model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
  if (!file.exists(model_file)) {
    stop("Model file not found: ", model_file)
  }
  source(model_file)
  
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
  
  result <- make_mvgam_forecasts(
    train_data = train_data,
    test_data = test_data,
    models_to_run = model_name,
    use_ordinal = FALSE
  )
  
  print(result$metrics)
  invisible(result)
}

test_data_loading <- function() {
  configs_to_test <- list(
    system = list(level = "system", include_species = "top6"),
    subregion = list(level = "subregion", include_species = "top6"),
    colony = list(level = "colony", include_species = c("gbhe", "greg"))
  )
  
  for (test_name in names(configs_to_test)) {
    cat(glue("\n--- Testing {test_name} ---\n"))
    
    test_config <- CONFIG
    test_config$spatial <- modifyList(
      CONFIG$spatial,
      configs_to_test[[test_name]]
    )
    
    data <- get_wading_bird_data(config = test_config, cache = FALSE)
    
    cat("Rows:", nrow(data), "\n")
    cat("Species:", paste(unique(data$species), collapse = ", "), "\n")
    if ("region" %in% names(data)) {
      cat("Regions:", paste(unique(data$region), collapse = ", "), "\n")
    }
    cat("Years:", min(data$year), "-", max(data$year), "\n")
  }
  
  invisible(NULL)
}

cat("\nTest harness loaded.\n")
cat("Examples:\n")
cat("  test_single_model('baseline')\n")
cat("  test_single_model('ar', use_full_data = TRUE)\n")
cat("  test_single_window('baseline', train_start = 2010, test_start = 2018)\n")
cat("  test_data_loading()\n\n")