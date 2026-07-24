# =============================================================================
# MAIN.R - Wading Bird Forecasting Pipeline ----
# Optimized for parallel processing and organized output folders
# =============================================================================
start_time <- Sys.time()

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================
`%||%` <- function(x, y) if (is.null(x)) y else x

# Print banner
print_banner <- function(text, char = "=", width = 80) {
  border <- paste(rep(char, width), collapse = "")
  cat("\n", border, "\n", text, "\n", border, "\n", sep = "")
}

# Print section header
print_section <- function(text, char = "-") {
  cat("\n", paste(rep(char, 60), collapse = ""), "\n", sep = "")
  cat(text, "\n")
  cat(paste(rep(char, 60), collapse = ""), "\n\n", sep = "")
}

# =============================================================================
# LIBRARY LOADING
# =============================================================================
print_banner("WADING BIRD FORECASTING PIPELINE")
cat("Starting at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

suppressPackageStartupMessages({
  library(config)
  library(conflicted)
  library(distributional)
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(tidyr)
  library(tsibble)
  library(wader)
  library(mvgam)
  library(verification)
  library(future)
  library(furrr)
  library(progressr)
})

# Handle namespace conflicts
conflict_prefer("filter",    "dplyr")
conflict_prefer("select",    "dplyr")
conflict_prefer("AR",        "mvgam")
conflict_prefer("VAR",       "mvgam")
conflict_prefer("RW",        "mvgam")
conflict_prefer("get",       "base")
conflict_prefer("as.matrix", "base")

# =============================================================================
# LOAD CONFIGURATION
# =============================================================================
print_section("CONFIGURATION")

if (!exists("CONFIG")) {
  active_config <- Sys.getenv("R_CONFIG_ACTIVE", "default")
  cat("Loading configuration:", active_config, "\n")
  CONFIG <- config::get()
} else {
  cat("Using pre-set CONFIG (", attr(CONFIG, "config") %||% "custom", ")\n")
}

# For totals
if (!is.null(CONFIG$spatial$forecast_totals) && isTRUE(CONFIG$spatial$forecast_totals)) {
  incompatible_models <- c("species_specific", "trait", "trait2")
  
  if (any(incompatible_models %in% CONFIG$models$mvgam)) {
    CONFIG$models$mvgam <- setdiff(CONFIG$models$mvgam, incompatible_models)
  }
  
  # Document which species will be aggregated
  species_to_aggregate <- if (CONFIG$spatial$include_species == "top6") {
    "gbhe, greg, rosp, sneg, wost, whib"
  } else if (CONFIG$spatial$include_species == "all") {
    if (CONFIG$spatial$include_unknowns) "all species (including unknowns)" else "all identified species"
  } else {
    paste(CONFIG$spatial$include_species, collapse = ", ")
  }
  
  cat("  • Aggregating to Total from:", species_to_aggregate, "\n")
}

# Print key configuration settings
cat("\n📋 Configuration Summary:\n")
cat("  • Environment:", Sys.getenv("R_CONFIG_ACTIVE", "default"), "\n")
cat("  • Spatial level:", CONFIG$spatial$level, "\n")
cat("  • Forecast totals:", isTRUE(CONFIG$spatial$forecast_totals), "\n")
cat("  • Run by region:", CONFIG$spatial$run_by_region, "\n")
cat("  • mvgam models:", ifelse(CONFIG$run_mvgam,
                                paste(CONFIG$models$mvgam, collapse = ", "), "disabled"), "\n")
cat("  • fable models:", ifelse(CONFIG$run_fable,
                                paste(CONFIG$models$fable, collapse = ", "), "disabled"), "\n")
cat("  • MCMC: chains =", CONFIG$chains,
    "| burnin =", CONFIG$burnin,
    "| samples =", CONFIG$samples, "\n")
cat("  • Train years:", CONFIG$train_years, "| Test years:", CONFIG$test_years, "\n")
cat("  • CV windows:", CONFIG$cv_windows %||% "all", "\n")
cat("  • Parallel:", CONFIG$parallel$enabled %||% FALSE, "\n")
if ((CONFIG$parallel$enabled %||% FALSE)) {
  cat("    Workers:", CONFIG$parallel$workers %||% "auto-detect", "\n")
}
cat("  • Ordinal evaluation:", CONFIG$use_ordinal, "\n")
cat("\n")

# =============================================================================
# CREATE TIMESTAMPED RUN FOLDER
# =============================================================================
print_section("CREATING RUN FOLDER")

# Generate timestamp
timestamp <- format(Sys.time(), "%Y%m%d-%H%M")

# Create run folder name with spatial level and timestamp
run_folder <- file.path("results", paste0("run_", CONFIG$spatial$level, "_", timestamp))

# Create directory structure
dir.create(run_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_folder, "forecasts"), recursive = TRUE, showWarnings = FALSE)

cat("✓ Run folder created:", run_folder, "\n")
cat("  All outputs will be saved to this folder\n")

# =============================================================================
# LOAD FUNCTIONS
# =============================================================================
print_section("LOADING FUNCTIONS")

source("data_functions.R")
cat("✓ data_functions.R loaded\n")

source("evaluation.R")
cat("✓ evaluation.R loaded\n")

source("plotting.R")
cat("✓ plotting.R loaded\n")

# Load fable if needed
if (CONFIG$run_fable) {
  cat("\nLoading fable libraries...\n")
  suppressPackageStartupMessages({
    library(fable)
    library(fable.gam)
    library(fabletools)
    library(feasts)
  })
  
  # Re-apply conflicts after fable
  conflict_prefer("AR", "mvgam")
  conflict_prefer("VAR", "mvgam")
  conflict_prefer("RW", "mvgam")
  
  source("models/fable_models.R")
  cat("✓ fable models loaded\n")
}

# Load mvgam models
if (CONFIG$run_mvgam) {
  cat("\nLoading mvgam models:\n")
  for (model in CONFIG$models$mvgam) {
    model_file <- file.path("models", paste0("mvgam_", model, ".R"))
    if (file.exists(model_file)) {
      source(model_file)
      cat("  ✓", model, "\n")
    } else {
      warning("  ✗ Model file not found: ", model_file)
    }
  }
}

# =============================================================================
# CLEANUP
# =============================================================================
print_section("INITIALIZATION")

# Clean up Stan temp files
stale_xt <- list.files(tempdir(), pattern = "\\.xt$", full.names = TRUE)
if (length(stale_xt) > 0) {
  cat("Removing", length(stale_xt), "leftover Stan temp files...\n")
  file.remove(stale_xt)
}

# Ensure wader data directory exists
if (!dir.exists("SiteandMethods")) {
  cat("Downloading wader observation data...\n")
  download_observations(".")
}

# Create cache directory if needed
if ((CONFIG$cache$data %||% FALSE) || (CONFIG$cache$models %||% FALSE)) {
  dir.create("cache", showWarnings = FALSE, recursive = TRUE)
  cat("✓ Cache directory ready\n")
}

# =============================================================================
# LOAD DATA
# =============================================================================
print_section("DATA LOADING")

data <- get_wading_bird_data(
  config = CONFIG,
  cache = CONFIG$cache$data %||% TRUE
)

cat("\n📊 Data Summary:\n")
cat("  • Years:", min(data$year), "-", max(data$year),
    glue("({max(data$year) - min(data$year) + 1} years)"), "\n")
cat("  • Species:", paste(unique(data$species), collapse = ", "), "\n")
cat("  • Observations:", nrow(data), "\n")

if (CONFIG$spatial$level != "system") {
  regions <- unique(data$region)
  cat("  • Regions:", paste(regions, collapse = ", "), "\n")
  cat("  • N regions:", length(regions), "\n")
  
  # Region-level summary
  region_summary <- data |>
    as_tibble() |>
    group_by(region) |>
    summarise(
      n_obs = n(),
      n_years = n_distinct(year),
      year_range = paste(min(year), max(year), sep = "-"),
      .groups = "drop"
    )
  
  cat("\n  Region details:\n")
  print(region_summary, n = Inf)
}

cat("\n")

# =============================================================================
# PRE-COMPUTE ORDINAL BREAKS (if using fixed breaks)
# =============================================================================
if (CONFIG$use_ordinal && !CONFIG$sliding_window_breaks) {
  print_section("ORDINAL BREAK CALCULATION")
  
  cat("Computing ordinal breaks (fixed)...\n")
  cat("  Using data from:", CONFIG$ordinal_years, "\n")
  cat("  Break quantiles:", paste(CONFIG$ordinal_breaks, collapse = ", "), "\n\n")
  
  # Check if spatial grouping needed
  has_regions <- CONFIG$spatial$level != "system" && "region" %in% names(data)
  
  if (has_regions) {
    # Breaks by region AND species
    precomputed_breaks <- data |>
      as_tibble() |>
      filter_ordinal_years(CONFIG$ordinal_years) |>
      group_by(region, species) |>
      summarise(
        low    = quantile(count, CONFIG$ordinal_breaks[1], na.rm = TRUE),
        medium = quantile(count, CONFIG$ordinal_breaks[2], na.rm = TRUE),
        high   = quantile(count, CONFIG$ordinal_breaks[3], na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    # Breaks by species only (system-wide)
    precomputed_breaks <- data |>
      as_tibble() |>
      filter_ordinal_years(CONFIG$ordinal_years) |>
      group_by(species) |>
      summarise(
        low    = quantile(count, CONFIG$ordinal_breaks[1], na.rm = TRUE),
        medium = quantile(count, CONFIG$ordinal_breaks[2], na.rm = TRUE),
        high   = quantile(count, CONFIG$ordinal_breaks[3], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  cat("✓ Ordinal breaks computed for", nrow(precomputed_breaks), "groups\n")
  
  # Preview breaks
  if (nrow(precomputed_breaks) <= 10) {
    print(precomputed_breaks)
  } else {
    cat("\n  Preview (first 6 rows):\n")
    print(head(precomputed_breaks))
  }
  
} else {
  precomputed_breaks <- NULL
  if (CONFIG$use_ordinal) {
    cat("ℹ Using sliding window ordinal breaks (computed per CV fold)\n")
  }
}

# =============================================================================
# MODEL FITTING FUNCTION (with parallel/cv_windows support)
# =============================================================================
run_models <- function(data, framework = "mvgam", models_to_run, ...) {
  
  make_forecast_fn <- switch(framework,
                             "mvgam" = make_mvgam_forecasts,
                             "fable" = make_fable_forecasts,
                             stop("Unknown framework: ", framework)
  )
  
  fit_sliding_window(
    data               = data,
    make_forecast      = make_forecast_fn,
    train_years        = CONFIG$train_years,
    test_years         = CONFIG$test_years,
    models_to_run      = models_to_run,
    use_ordinal        = CONFIG$use_ordinal,
    precomputed_breaks = precomputed_breaks,
    cv_windows         = CONFIG$cv_windows %||% NULL,
    parallel           = CONFIG$parallel$enabled %||% FALSE,
    workers            = CONFIG$parallel$workers %||% NULL,
    ...
  )
}

# =============================================================================
# RUN FORECASTS
# =============================================================================
print_banner("MODEL FITTING AND FORECASTING")

results <- list()

# Determine if running by region
run_by_region <- !is.null(CONFIG$spatial$run_by_region) &&
  isTRUE(CONFIG$spatial$run_by_region) &&
  CONFIG$spatial$level != "system"

# ---------------------------------------------------------------------------
# OPTION 1: SEPARATE MODELS PER REGION (slower)
# ---------------------------------------------------------------------------
if (run_by_region) {
  
  # Get unique regions
  regions <- unique(as_tibble(data)$region)
  
  print_section(glue("RUNNING MODELS SEPARATELY BY REGION ({length(regions)} regions)"))
  cat("⚠️  Note: Running hierarchical models (run_by_region: false) is faster!\n\n")
  
  # Loop over regions
  region_results <- list()
  
  for (i in seq_along(regions)) {
    reg <- regions[i]
    
    cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
    cat(glue("REGION {i}/{length(regions)}: {reg}"), "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
    
    # Subset data for this region
    data_region <- data |> filter(region == reg)
    cat("  • Years:", min(data_region$year), "-", max(data_region$year), "\n")
    cat("  • Observations:", nrow(data_region), "\n")
    cat("  • Species:", paste(unique(data_region$species), collapse = ", "), "\n\n")
    
    # Compute region-specific ordinal breaks if needed
    if (CONFIG$use_ordinal && !CONFIG$sliding_window_breaks) {
      precomputed_breaks_region <- data_region |>
        as_tibble() |>
        filter_ordinal_years(CONFIG$ordinal_years) |>
        group_by(species) |>
        summarise(
          low    = quantile(count, CONFIG$ordinal_breaks[1], na.rm = TRUE),
          medium = quantile(count, CONFIG$ordinal_breaks[2], na.rm = TRUE),
          high   = quantile(count, CONFIG$ordinal_breaks[3], na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      precomputed_breaks_region <- precomputed_breaks
    }
    
    region_result <- list()
    
    # Run mvgam models for this region
    if (CONFIG$run_mvgam) {
      cat("Running mvgam models for", reg, "...\n")
      region_result$mvgam <- tryCatch({
        run_models(
          data = data_region,
          framework = "mvgam",
          models_to_run = CONFIG$models$mvgam,
          precomputed_breaks = precomputed_breaks_region
        )
      }, error = function(e) {
        warning(glue("mvgam failed for region {reg}: {e$message}"))
        NULL
      })
    }
    
    # Run fable models for this region
    if (CONFIG$run_fable) {
      cat("Running fable models for", reg, "...\n")
      region_result$fable <- tryCatch({
        run_models(
          data = data_region,
          framework = "fable",
          models_to_run = CONFIG$models$fable,
          precomputed_breaks = precomputed_breaks_region
        )
      }, error = function(e) {
        warning(glue("fable failed for region {reg}: {e$message}"))
        NULL
      })
    }
    
    region_results[[reg]] <- region_result
    cat("\n✓ Region", reg, "complete!\n")
  }
  
  # Combine results from all regions
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("COMBINING RESULTS FROM ALL REGIONS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  results$by_region <- region_results
  
  # Combine mvgam results
  if (CONFIG$run_mvgam) {
    results$mvgam <- list(
      forecasts = bind_rows(lapply(names(region_results), function(reg) {
        if (!is.null(region_results[[reg]]$mvgam)) {
          region_results[[reg]]$mvgam$forecasts |> mutate(region = reg)
        }
      })),
      metrics = bind_rows(lapply(names(region_results), function(reg) {
        if (!is.null(region_results[[reg]]$mvgam)) {
          region_results[[reg]]$mvgam$metrics |> mutate(region = reg)
        }
      }))
    )
    cat("✓ mvgam results combined:", nrow(results$mvgam$forecasts), "forecasts\n")
  }
  
  # Combine fable results
  if (CONFIG$run_fable) {
    results$fable <- list(
      forecasts = bind_rows(lapply(names(region_results), function(reg) {
        if (!is.null(region_results[[reg]]$fable)) {
          region_results[[reg]]$fable$forecasts |> mutate(region = reg)
        }
      })),
      metrics = bind_rows(lapply(names(region_results), function(reg) {
        if (!is.null(region_results[[reg]]$fable)) {
          region_results[[reg]]$fable$metrics |> mutate(region = reg)
        }
      }))
    )
    cat("✓ fable results combined:", nrow(results$fable$forecasts), "forecasts\n")
  }
  
  # ---------------------------------------------------------------------------
  # OPTION 2: HIERARCHICAL MODEL (faster, recommended)
  # ---------------------------------------------------------------------------
  
} else {
  
  print_section("RUNNING HIERARCHICAL MODELS")
  cat("✓ Using efficient hierarchical approach\n\n")
  
  # Run mvgam models
  if (CONFIG$run_mvgam) {
    cat("=== Running mvgam models ===\n")
    cat("Models:", paste(CONFIG$models$mvgam, collapse = ", "), "\n\n")
    
    results$mvgam <- run_models(
      data = data,
      framework = "mvgam",
      models_to_run = CONFIG$models$mvgam
    )
    
    if (!is.null(results$mvgam)) {
      cat("\n✓ mvgam forecasts complete!\n")
      print_cv_summary(results$mvgam)
    }
  }
  
  # Run fable models
  if (CONFIG$run_fable) {
    cat("\n=== Running fable models ===\n")
    cat("Models:", paste(CONFIG$models$fable, collapse = ", "), "\n\n")
    
    results$fable <- run_models(
      data = data,
      framework = "fable",
      models_to_run = CONFIG$models$fable
    )
    
    if (!is.null(results$fable)) {
      cat("\n✓ fable forecasts complete!\n")
      print_cv_summary(results$fable)
    }
  }
}

# =============================================================================
# SAVE RESULTS TO RUN FOLDER
# =============================================================================
print_section("SAVING RESULTS")

# Save results
results_filename <- file.path(run_folder, "forecast_results.rds")
saveRDS(results, results_filename)
cat("✓ Results saved to:", results_filename, "\n")

# Save config
config_filename <- file.path(run_folder, "config.rds")
saveRDS(CONFIG, config_filename)
cat("✓ Config saved to:", config_filename, "\n")

# =============================================================================
# GENERATE PLOTS (using run folder)
# =============================================================================
print_section("GENERATING PLOTS")

tryCatch({
  generate_plots(results, CONFIG, data = data, results_dir = run_folder)
  cat("✓ All plots saved to:", run_folder, "\n")
}, error = function(e) {
  warning("Plotting failed: ", e$message)
})

# =============================================================================
# FINAL SUMMARY
# =============================================================================
print_banner("ANALYSIS COMPLETE")

cat("📂 Run Folder:", run_folder, "\n\n")

cat("📊 Configuration:\n")
cat("  • Spatial level:", CONFIG$spatial$level, "\n")
cat("  • Evaluation:", ifelse(CONFIG$use_ordinal,
                              "Numeric + Ordinal (RPS)", "Numeric only (CRPS)"), "\n")
cat("  • Data type:", CONFIG$data_type, "\n\n")

if (CONFIG$run_mvgam && !is.null(results$mvgam)) {
  cat("🔵 mvgam Results:\n")
  cat("  • Models:", paste(CONFIG$models$mvgam, collapse = ", "), "\n")
  cat("  • Forecasts:", nrow(results$mvgam$forecasts), "\n")
  cat("  • Metric rows:", nrow(results$mvgam$metrics), "\n")
  
  # Best model by CRPS
  if (nrow(results$mvgam$metrics) > 0 && "crps" %in% names(results$mvgam$metrics)) {
    best_model <- results$mvgam$metrics |>
      group_by(model) |>
      summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop") |>
      arrange(mean_crps) |>
      slice(1)
    
    cat("  • Best model:", best_model$model,
        glue("(CRPS = {round(best_model$mean_crps, 2)})"), "\n")
  }
  cat("\n")
}

if (CONFIG$run_fable && !is.null(results$fable)) {
  cat("🟢 fable Results:\n")
  cat("  • Models:", paste(CONFIG$models$fable, collapse = ", "), "\n")
  cat("  • Forecasts:", nrow(results$fable$forecasts), "\n")
  cat("  • Metric rows:", nrow(results$fable$metrics), "\n\n")
}

end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "mins")

cat("⏱️  Runtime:", round(runtime, 1), "minutes\n")
cat("💾 All results saved to:", run_folder, "\n")
cat("✅ Analysis complete at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================================
# CLEANUP
# =============================================================================
# Reset to sequential processing (in case parallel was used)
plan(sequential)

cat("🎉 Done!\n\n")