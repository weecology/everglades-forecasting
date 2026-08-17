# =============================================================================
# WRAPPER SCRIPT: RUN EACH MODEL SEPARATELY ACROSS ALL SPATIAL SCALES
# Compares each model individually to baseline across system/subregion/colony
# =============================================================================

library(config)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(glue)

# =============================================================================
# CONFIGURATION - EDIT THIS SECTION
# =============================================================================

# Species to include (examples: c("gbhe", "greg", "rosp") or "top6" or "all")
SPECIES_TO_RUN <- c("greg")  # Specify individual species
# SPECIES_TO_RUN <- "top6"           # Or use "top6" for all top 6 species
# SPECIES_TO_RUN <- "all"            # Or use "all" for all species

# Spatial scales to compare
SCALES_TO_RUN <- c("system", "subregion")  # Choose from: "system", "subregion", "colony"
# SCALES_TO_RUN <- c("system", "colony")   # Example: skip subregion
# SCALES_TO_RUN <- c("colony")             # Example: just colony level

# mvgam models to test (baseline is always included automatically)
MVGAM_MODELS <- c("ar_exog")  # Choose from: "ar", "ar_exog", "ar_exog_plus", "species_specific", "trait", "trait2"
# MVGAM_MODELS <- c()                   # Set to empty vector to skip mvgam

# fable models to test (baseline is always included automatically)
FABLE_MODELS <- c()  # Choose from: "arima", "tslm", "arima_exog", "gam"
# FABLE_MODELS <- c("arima", "tslm")   # Example: test multiple fable models

# =============================================================================
# END USER CONFIGURATION
# =============================================================================

# =============================================================================
# CREATE TIMESTAMPED SCALE_RUN FOLDER
# =============================================================================

timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
scale_run_folder <- file.path("results", paste0("model_scale_run_", timestamp))
dir.create(scale_run_folder, recursive = TRUE, showWarnings = FALSE)

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("MODEL-BY-MODEL SPATIAL SCALE COMPARISON\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Results folder:", scale_run_folder, "\n\n")

# =============================================================================
# LOAD BASE CONFIGURATION AND APPLY USER SETTINGS
# =============================================================================

base_profile <- "run_all_scales_all"
Sys.setenv(R_CONFIG_ACTIVE = base_profile)
base_config <- config::get()

# Override with user-specified species
base_config$spatial$include_species <- SPECIES_TO_RUN

# Override with user-specified models
base_config$models$mvgam <- MVGAM_MODELS
base_config$models$fable <- FABLE_MODELS

# Set framework flags
base_config$run_mvgam <- length(MVGAM_MODELS) > 0
base_config$run_fable <- length(FABLE_MODELS) > 0

# Save the configuration
saveRDS(base_config, file.path(scale_run_folder, "base_config.rds"))

cat("📋 Configuration:\n")
cat("  • Species:", paste(SPECIES_TO_RUN, collapse = ", "), "\n")
cat("  • Scales:", paste(SCALES_TO_RUN, collapse = ", "), "\n")
cat("  • mvgam models:", if(length(MVGAM_MODELS) > 0) paste(MVGAM_MODELS, collapse = ", ") else "none", "\n")
cat("  • fable models:", if(length(FABLE_MODELS) > 0) paste(FABLE_MODELS, collapse = ", ") else "none", "\n")
cat("✓ Base configuration saved\n\n")

# =============================================================================
# BUILD MODEL LIST
# =============================================================================

models_to_test <- list()

if (base_config$run_mvgam) {
  for (m in MVGAM_MODELS) {
    models_to_test[[paste0("mvgam_", m)]] <- list(
      framework = "mvgam",
      model = m
    )
  }
}

if (base_config$run_fable) {
  for (m in FABLE_MODELS) {
    models_to_test[[paste0("fable_", m)]] <- list(
      framework = "fable",
      model = m
    )
  }
}

if (length(models_to_test) == 0) {
  stop("❌ No models specified! Please add models to MVGAM_MODELS or FABLE_MODELS")
}

cat("📋 Models to test:", length(models_to_test), "\n")
for (model_key in names(models_to_test)) {
  cat("  •", model_key, "\n")
}
cat("\n")

# Storage for all results
all_model_results <- list()

# =============================================================================
# PART 1: RUN EACH MODEL ACROSS ALL SCALES
# =============================================================================

for (model_key in names(models_to_test)) {
  
  model_info <- models_to_test[[model_key]]
  framework <- model_info$framework
  model_name <- model_info$model
  
  cat("\n")
  cat(paste(rep("█", 80), collapse = ""), "\n")
  cat(glue("🔬 TESTING MODEL: {model_key}"), "\n")
  cat(paste(rep("█", 80), collapse = ""), "\n\n")
  
  # Create folder for this model
  model_folder <- file.path(scale_run_folder, model_key)
  dir.create(model_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Storage for this model's results across scales
  model_scale_results <- list()
  
  # Run this model at each scale
  for (current_scale in SCALES_TO_RUN) {
    
    cat("\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    cat(glue("  Scale: {toupper(current_scale)}"), "\n")
    cat(paste(rep("-", 70), collapse = ""), "\n\n")
    
    # Create CONFIG for this model + scale
    CONFIG <- base_config
    CONFIG$spatial$level <- current_scale
    
    # Set run_by_region
    if (current_scale == "system") {
      CONFIG$spatial$run_by_region <- FALSE
    }
    
    # Configure to run only baseline + this model
    if (framework == "mvgam") {
      CONFIG$models$mvgam <- c("baseline", model_name)
      CONFIG$models$fable <- c()
      CONFIG$run_mvgam <- TRUE
      CONFIG$run_fable <- FALSE
    } else {
      CONFIG$models$fable <- c("baseline", model_name)
      CONFIG$models$mvgam <- c()
      CONFIG$run_mvgam <- FALSE
      CONFIG$run_fable <- TRUE
    }
    
    # Load model functions into global environment
    if (framework == "mvgam") {
      model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
      if (file.exists(model_file)) {
        source(model_file, local = FALSE)  # local = FALSE ensures global environment
        cat(glue("  ✓ Pre-loaded {model_name}\n"))
      }
      
      # Also load baseline
      baseline_file <- file.path("models", "mvgam_baseline.R")
      if (file.exists(baseline_file)) {
        source(baseline_file, local = FALSE)
        cat("  ✓ Pre-loaded baseline\n")
      }
    }
    
    # Run the pipeline
    tryCatch({
      source("main.R")
      
      if (exists("run_folder") && !is.null(run_folder)) {
        # Move results to organized structure
        dest_folder <- file.path(model_folder, current_scale)
        
        files_to_copy <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
        
        for (src_file in files_to_copy) {
          rel_path <- sub(paste0(run_folder, "/"), "", src_file)
          dest_file <- file.path(dest_folder, rel_path)
          dest_dir <- dirname(dest_file)
          
          dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
          file.copy(src_file, dest_file, overwrite = TRUE)
        }
        
        model_scale_results[[current_scale]] <- dest_folder
        
        # Clean up
        unlink(run_folder, recursive = TRUE)
        
        cat("  ✓ Results saved\n")
      } else {
        warning(glue("  ✗ {model_key} at {current_scale} failed to create run_folder"))
        model_scale_results[[current_scale]] <- NULL
      }
      
    }, error = function(e) {
      warning(glue("  ✗ {model_key} at {current_scale} failed: {e$message}"))
      model_scale_results[[current_scale]] <- NULL
    })
    
    gc()
  }
  
  all_model_results[[model_key]] <- model_scale_results
  
  cat(glue("\n✓ {model_key} complete across all scales\n"))
}

cat("\n✅ ALL MODEL RUNS COMPLETE!\n\n")

# =============================================================================
# PART 2: EXTRACT AND COMPARE EACH MODEL TO BASELINE
# =============================================================================

cat("📊 Extracting and comparing metrics...\n\n")

# Function to extract metrics for a specific model
extract_model_metrics <- function(folder_path, scale_name, framework) {
  file_path <- file.path(folder_path, "forecast_results.rds")
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  res <- readRDS(file_path)
  
  if (framework == "mvgam") {
    if (is.null(res$mvgam) || is.null(res$mvgam$metrics)) return(NULL)
    
    metrics <- res$mvgam$metrics |>
      select(model, species,
             crps_skill,
             any_of(c("rps_skill", "rmse_skill"))) |>
      mutate(
        framework = "mvgam",
        scale = scale_name
      )
    
  } else {
    if (is.null(res$fable) || is.null(res$fable$metrics)) return(NULL)
    
    metrics <- res$fable$metrics |>
      rename(model = .model) |>
      select(model, species,
             crps_skill,
             any_of(c("rps_skill", "rmse_skill"))) |>
      mutate(
        framework = "fable",
        scale = scale_name
      )
  }
  
  return(metrics)
}

# Scale colors (adjust based on which scales are included)
all_scale_colors <- c("colony" = "#00B050", "subregion" = "#FF0000", "system" = "#0000FF")
scale_colors <- all_scale_colors[SCALES_TO_RUN]

# Process each model
for (model_key in names(all_model_results)) {
  
  model_info <- models_to_test[[model_key]]
  framework <- model_info$framework
  model_name <- model_info$model
  model_folders <- all_model_results[[model_key]]
  
  cat(glue("\n📈 Generating plots for {model_key}...\n"))
  
  # Extract metrics from all scales
  model_data <- bind_rows(lapply(names(model_folders), function(scale) {
    folder <- model_folders[[scale]]
    if (is.null(folder)) return(NULL)
    extract_model_metrics(folder, scale, framework)
  }))
  
  if (is.null(model_data) || nrow(model_data) == 0) {
    cat(glue("No data for {model_key}\n"))
    next
  }
  
  # Filter to only the test model (exclude baseline)
  model_only <- model_data |> filter(model == model_name)
  
  if (nrow(model_only) == 0) {
    cat(glue("No results for {model_name}\n"))
    next
  }
  
  # Convert to long format for plotting
  skill_metrics <- intersect(c("crps_skill", "rps_skill", "rmse_skill"), names(model_only))
  
  model_long <- model_only |>
    select(species, scale, all_of(skill_metrics)) |>
    pivot_longer(cols = all_of(skill_metrics),
                 names_to = "metric",
                 values_to = "skill_score") |>
    filter(!is.na(skill_score)) |>
    mutate(
      skill_score = pmax(skill_score, -1),
      metric_label = case_when(
        metric == "crps_skill" ~ "CRPS Skill",
        metric == "rps_skill" ~ "RPS Skill",
        metric == "rmse_skill" ~ "RMSE Skill",
        TRUE ~ metric
      ),
      scale = factor(scale, levels = SCALES_TO_RUN)
    )
  
  # ---------------------------------------------------------------------
  # PLOT 1: Density plot (all metrics faceted)
  # ---------------------------------------------------------------------
  p_density <- ggplot(model_long, aes(x = skill_score, fill = scale, color = scale)) +
    geom_density(alpha = 0.2, linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = scale_colors) +
    scale_color_manual(values = scale_colors) +
    theme_classic(base_size = 14) +
    labs(
      title = glue("{model_key} Skill Across Spatial Scales"),
      subtitle = "Compared to Baseline",
      x = "Skill Score (vs Baseline)",
      y = "Density",
      fill = "Scale",
      color = "Scale"
    ) +
    theme(
      legend.position = "bottom",
      axis.line = element_line(linewidth = 1),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray40")
    )
  
  ggsave(file.path(model_folder, "density_across_scales.png"),
         p_density, width = 10, height = 12, dpi = 300)
  
  # ---------------------------------------------------------------------
  # PLOT 2: Jitter plot (all metrics)
  # ---------------------------------------------------------------------
  p_jitter <- ggplot(model_long, aes(x = scale, y = skill_score, color = scale)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.8) +
    geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
    facet_wrap(~metric_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = scale_colors) +
    theme_classic(base_size = 14) +
    labs(
      title = glue("{model_key} Skill by Spatial Scale"),
      subtitle = "Compared to Baseline",
      x = NULL,
      y = "Skill Score (vs Baseline)",
      color = "Scale"
    ) +
    theme(
      legend.position = "bottom",
      axis.line = element_line(linewidth = 1),
      axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray40")
    )
  
  ggsave(file.path(model_folder, "jitter_across_scales.png"),
         p_jitter, width = 12, height = 8, dpi = 300)
  
  # ---------------------------------------------------------------------
  # PLOT 3: Summary statistics table
  # ---------------------------------------------------------------------
  summary_stats <- model_long |>
    group_by(scale, metric_label) |>
    summarise(
      n = n(),
      mean = mean(skill_score, na.rm = TRUE),
      median = median(skill_score, na.rm = TRUE),
      sd = sd(skill_score, na.rm = TRUE),
      min = min(skill_score, na.rm = TRUE),
      max = max(skill_score, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(where(is.numeric) & !n, ~round(.x, 3)))
  
  write.csv(summary_stats,
            file.path(model_folder, "summary_statistics.csv"),
            row.names = FALSE)
  
  cat(glue("  ✓ Saved: density_across_scales.png\n"))
  cat(glue("  ✓ Saved: jitter_across_scales.png\n"))
  cat(glue("  ✓ Saved: summary_statistics.csv\n"))
  
  # Print summary to console
  cat("\n  Summary Statistics:\n")
  print(summary_stats, n = Inf)
}

# =============================================================================
# PART 3: GENERATE CROSS-MODEL COMPARISON
# =============================================================================

cat("\n📊 Generating cross-model comparison...\n")

# Extract all metrics for comparison
all_metrics <- bind_rows(lapply(names(all_model_results), function(model_key) {
  model_info <- models_to_test[[model_key]]
  framework <- model_info$framework
  model_name <- model_info$model
  model_folders <- all_model_results[[model_key]]
  
  bind_rows(lapply(names(model_folders), function(scale) {
    folder <- model_folders[[scale]]
    if (is.null(folder)) return(NULL)
    
    metrics <- extract_model_metrics(folder, scale, framework)
    if (is.null(metrics)) return(NULL)
    
    metrics |>
      filter(model == model_name) |>
      mutate(model_key = model_key)
  }))
}))

if (!is.null(all_metrics) && nrow(all_metrics) > 0) {
  
  # Convert to long format
  skill_metrics <- intersect(c("crps_skill", "rps_skill", "rmse_skill"), names(all_metrics))
  
  comparison_long <- all_metrics |>
    select(model_key, species, scale, all_of(skill_metrics)) |>
    pivot_longer(cols = all_of(skill_metrics),
                 names_to = "metric",
                 values_to = "skill_score") |>
    filter(!is.na(skill_score)) |>
    mutate(
      skill_score = pmax(skill_score, -1),
      metric_label = case_when(
        metric == "crps_skill" ~ "CRPS Skill",
        metric == "rps_skill" ~ "RPS Skill",
        metric == "rmse_skill" ~ "RMSE Skill",
        TRUE ~ metric
      ),
      scale = factor(scale, levels = SCALES_TO_RUN)
    )
  
  # Plot: Model comparison across scales
  p_model_comparison <- ggplot(comparison_long,
                               aes(x = model_key, y = skill_score, fill = scale)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = scale_colors) +
    theme_classic(base_size = 12) +
    labs(
      title = "Model Performance Comparison Across Spatial Scales",
      subtitle = "All models vs. Baseline",
      x = "Model",
      y = "Skill Score (vs Baseline)",
      fill = "Scale"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray40")
    )
  
  ggsave(file.path(scale_run_folder, "all_models_comparison.png"),
         p_model_comparison, width = 14, height = 12, dpi = 300)
  
  cat("  ✓ Saved: all_models_comparison.png\n")
  
  # Overall summary
  overall_summary <- comparison_long |>
    group_by(model_key, scale, metric_label) |>
    summarise(
      mean_skill = mean(skill_score, na.rm = TRUE),
      median_skill = median(skill_score, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(across(c(mean_skill, median_skill), ~round(.x, 3)))
  
  write.csv(overall_summary,
            file.path(scale_run_folder, "all_models_summary.csv"),
            row.names = FALSE)
  
  cat("  ✓ Saved: all_models_summary.csv\n")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("✅ MODEL-BY-MODEL SPATIAL SCALE COMPARISON COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
cat("📂 All results saved to:", scale_run_folder, "\n\n")
cat("📁 Folder Structure:\n")
cat("  • base_config.rds - Base configuration used\n")
cat("  • all_models_comparison.png - Cross-model comparison plot\n")
cat("  • all_models_summary.csv - Overall performance summary\n\n")

for (model_key in names(all_model_results)) {
  cat(glue("  • {model_key}/\n"))
  for (scale in SCALES_TO_RUN) {
    cat(glue("    ├── {scale}/\n"))
  }
  cat(glue("    ├── density_across_scales.png\n"))
  cat(glue("    ├── jitter_across_scales.png\n"))
  cat(glue("    └── summary_statistics.csv\n\n"))
}

cat("🎉 Done!\n\n")