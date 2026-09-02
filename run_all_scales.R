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
#SPECIES_TO_RUN <- c("wost")  # Specify individual species
SPECIES_TO_RUN <- "top6"           # Or use "top6" for all top 6 species
# SPECIES_TO_RUN <- "all"            # Or use "all" for all species

FORECAST_TOTALS <- FALSE           #TRUE/FALSE

# Spatial scales to compare
SCALES_TO_RUN <- c("system", "subregion")  # Choose from: "system", "subregion", "colony"
# SCALES_TO_RUN <- c("system", "colony")
# SCALES_TO_RUN <- c("colony")

# mvgam models to test (baseline is always included automatically)
#MVGAM_MODELS <- c("ar", "ar_exog", "ar_exog_plus")
MVGAM_MODELS <- c()

# fable models to test (baseline is always included automatically)
#FABLE_MODELS <- c()
FABLE_MODELS <- c("arima", "tslm", "arima_exog", "gam")

# =============================================================================
# PARALLEL PROCESSING
# =============================================================================
#CONFIG$parallel$enabled <- TRUE

# =============================================================================
# CREATE TIMESTAMPED SCALE_RUN FOLDER
# =============================================================================

timestamp       <- format(Sys.time(), "%Y%m%d-%H%M")
all_models_str  <- paste(c(MVGAM_MODELS, FABLE_MODELS), collapse = "-")
all_scales_str  <- paste(SCALES_TO_RUN, collapse = "-")
species_str     <- paste(SPECIES_TO_RUN, collapse = "-")

scale_run_folder <- file.path("results",
                              paste0(species_str,
                                     "_model_scale_run_",
                                     all_models_str,
                                     "_",
                                     all_scales_str,
                                     "-",
                                     timestamp))
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

base_config$spatial$include_species <- SPECIES_TO_RUN
base_config$spatial$forecast_totals <- FORECAST_TOTALS
base_config$models$mvgam            <- MVGAM_MODELS
base_config$models$fable            <- FABLE_MODELS
base_config$run_mvgam               <- length(MVGAM_MODELS) > 0
base_config$run_fable               <- length(FABLE_MODELS) > 0

saveRDS(base_config, file.path(scale_run_folder, "base_config.rds"))

cat("📋 Configuration:\n")
cat("  • Species:", paste(SPECIES_TO_RUN, collapse = ", "), "\n")
cat("  • Scales:",  paste(SCALES_TO_RUN,  collapse = ", "), "\n")
cat("  • mvgam models:", if (length(MVGAM_MODELS) > 0) paste(MVGAM_MODELS, collapse = ", ") else "none", "\n")
cat("  • fable models:", if (length(FABLE_MODELS) > 0) paste(FABLE_MODELS, collapse = ", ") else "none", "\n")
cat("✓ Base configuration saved\n\n")

# =============================================================================
# BUILD MODEL LIST
# =============================================================================

models_to_test <- list()

if (base_config$run_mvgam) {
  for (m in MVGAM_MODELS) {
    models_to_test[[paste0("mvgam_", m)]] <- list(framework = "mvgam", model = m)
  }
}

if (base_config$run_fable) {
  for (m in FABLE_MODELS) {
    models_to_test[[paste0("fable_", m)]] <- list(framework = "fable", model = m)
  }
}

if (length(models_to_test) == 0) {
  stop("!!!! - No models specified! Please add models to MVGAM_MODELS or FABLE_MODELS")
}

cat("📋 Models to test:", length(models_to_test), "\n")
for (model_key in names(models_to_test)) {
  cat("  •", model_key, "\n")
}
cat("\n")

all_model_results <- list()

# =============================================================================
# PART 1: RUN EACH MODEL ACROSS ALL SCALES
# =============================================================================

for (model_key in names(models_to_test)) {
  
  model_info  <- models_to_test[[model_key]]
  framework   <- model_info$framework
  model_name  <- model_info$model
  
  cat("\n")
  cat(paste(rep("█", 80), collapse = ""), "\n")
  cat(glue("🔬 TESTING MODEL: {model_key}"), "\n")
  cat(paste(rep("█", 80), collapse = ""), "\n\n")
  
  model_folder <- file.path(scale_run_folder, model_key)
  dir.create(model_folder, recursive = TRUE, showWarnings = FALSE)
  
  model_scale_results <- list()
  
  for (current_scale in SCALES_TO_RUN) {
    
    cat("\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    cat(glue("  Scale: {toupper(current_scale)}"), "\n")
    cat(paste(rep("-", 70), collapse = ""), "\n\n")
    
    CONFIG <- base_config
    CONFIG$spatial$level <- current_scale
    
    if (current_scale == "system") {
      CONFIG$spatial$run_by_region <- FALSE
    } else if (current_scale == "subregion") {
      CONFIG$spatial$run_by_region <- TRUE
    }
    
    if (framework == "mvgam") {
      CONFIG$models$mvgam <- c("baseline", model_name)
      CONFIG$models$fable <- c()
      CONFIG$run_mvgam    <- TRUE
      CONFIG$run_fable    <- FALSE
    } else {
      CONFIG$models$fable <- c("baseline", model_name)
      CONFIG$models$mvgam <- c()
      CONFIG$run_mvgam    <- FALSE
      CONFIG$run_fable    <- TRUE
    }
    
    if (framework == "mvgam") {
      model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
      if (file.exists(model_file)) {
        source(model_file, local = FALSE)
        cat(glue("  ✓ Pre-loaded {model_name}\n"))
      }
      baseline_file <- file.path("models", "mvgam_baseline.R")
      if (file.exists(baseline_file)) {
        source(baseline_file, local = FALSE)
        cat("  ✓ Pre-loaded baseline\n")
      }
    }
    
    tryCatch({
      CONFIG$parallel$enabled  <- FALSE
      CONFIG$.skip_config_init <- TRUE
      source("main.R")
      
      if (exists("run_folder") && !is.null(run_folder)) {
        dest_folder    <- file.path(model_folder, current_scale)
        files_to_copy  <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
        
        for (src_file in files_to_copy) {
          rel_path  <- sub(paste0(run_folder, "/"), "", src_file)
          dest_file <- file.path(dest_folder, rel_path)
          dest_dir  <- dirname(dest_file)
          dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
          file.copy(src_file, dest_file, overwrite = TRUE)
        }
        
        model_scale_results[[current_scale]] <- dest_folder
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
# PART 2: EXTRACT PER-WINDOW SKILL AND COMPUTE SCALE DELTA
# delta_skill = skill_system_t - skill_subregion_t (per window t, species, model)
# =============================================================================

cat("📊 Extracting per-window skill scores and computing scale deltas...\n\n")

# -- Helper: extract window-level metrics (keeps window + test_start columns) --
extract_model_metrics <- function(folder_path, scale_name, framework) {
  file_path <- file.path(folder_path, "forecast_results.rds")
  if (!file.exists(file_path)) return(NULL)
  
  res <- readRDS(file_path)
  
  if (framework == "mvgam") {
    if (is.null(res$mvgam) || is.null(res$mvgam$metrics)) return(NULL)
    metrics <- res$mvgam$metrics |>
      select(model, species, any_of("region"),
             window, any_of("test_start"),
             crps_skill, any_of(c("rps_skill", "rmse_skill"))) |>
      mutate(framework = "mvgam", scale = scale_name)
    
  } else {
    if (is.null(res$fable) || is.null(res$fable$metrics)) return(NULL)
    metrics <- res$fable$metrics |>
      rename(model = .model) |>
      select(model, species, any_of("region"),
             window, any_of("test_start"),
             crps_skill, any_of(c("rps_skill", "rmse_skill"))) |>
      mutate(framework = "fable", scale = scale_name)
  }
  
  return(metrics)
}

# Scale colors
all_scale_colors <- c("colony" = "#00B050", "subregion" = "#FF0000", "system" = "#0000FF")
scale_colors     <- all_scale_colors[SCALES_TO_RUN]

# -- Collect window-level metrics across all models and scales --
all_window_metrics <- bind_rows(lapply(names(all_model_results), function(model_key) {
  model_info    <- models_to_test[[model_key]]
  framework     <- model_info$framework
  model_name    <- model_info$model
  model_folders <- all_model_results[[model_key]]
  
  bind_rows(lapply(names(model_folders), function(scale) {
    folder <- model_folders[[scale]]
    if (is.null(folder)) return(NULL)
    
    extract_model_metrics(folder, scale, framework) |>
      filter(model == model_name) |>
      mutate(model_key = model_key)
  }))
}))

if (is.null(all_window_metrics) || nrow(all_window_metrics) == 0) {
  warning("No window-level metrics found. Skipping delta computation.")
} else {
  
  skill_metrics <- intersect(
    c("crps_skill", "rps_skill", "rmse_skill"),
    names(all_window_metrics)
  )
  
  # -- Pivot to long format --
  window_long <- all_window_metrics |>
    pivot_longer(
      cols      = all_of(skill_metrics),
      names_to  = "metric",
      values_to = "skill_score"
    ) |>
    filter(!is.na(skill_score)) |>
    mutate(
      skill_score = pmax(skill_score, -1),
      metric_label = case_when(
        metric == "crps_skill" ~ "CRPS Skill",
        metric == "rps_skill"  ~ "RPS Skill",
        metric == "rmse_skill" ~ "RMSE Skill",
        TRUE ~ metric
      )
    )
  
  # Save full window-level skill table
  write.csv(window_long,
            file.path(scale_run_folder, "window_skill_all_scales.csv"),
            row.names = FALSE)
  cat("  ✓ Saved: window_skill_all_scales.csv\n")
  
  # -- Per-model window plots --
  for (model_key in names(all_model_results)) {
    
    model_folder  <- file.path(scale_run_folder, model_key)
    model_info    <- models_to_test[[model_key]]
    framework     <- model_info$framework
    model_name    <- model_info$model
    model_folders <- all_model_results[[model_key]]
    
    cat(glue("\n📈 Generating window plots for {model_key}...\n"))
    
    model_long <- window_long |>
      filter(model_key == model_key) |>
      mutate(
        scale  = factor(scale, levels = SCALES_TO_RUN),
        window = factor(window)
      )
    
    if (nrow(model_long) == 0) {
      cat(glue("No data for {model_key}\n"))
      next
    }
    
    # -- Plot: skill score per window, colored by scale --
    p_window <- ggplot(model_long,
                       aes(x = window, y = skill_score,
                           color = scale, group = scale)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.7) +
      geom_line(linewidth = 0.9, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.9) +
      facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
      scale_color_manual(values = scale_colors) +
      theme_classic(base_size = 13) +
      labs(
        title    = glue("{model_key}: Skill Score per Sliding Window"),
        subtitle = "Each point = one CV window (10yr train / 2yr test)",
        x        = "CV Window (t)",
        y        = "Skill Score (vs Baseline)",
        color    = "Scale"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 11),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(model_folder, "window_skill_by_scale.png"),
           p_window, width = 12, height = 10, dpi = 300)
    cat(glue("  ✓ Saved: {model_key}/window_skill_by_scale.png\n"))
  }
  
  # -- Compute delta: skill_system_t - skill_subregion_t --
  if (all(c("system", "subregion") %in% SCALES_TO_RUN)) {
    
    group_keys <- intersect(
      c("model_key", "species", "region", "window", "test_start", "metric", "metric_label"),
      names(window_long)
    )
    
    delta_df <- window_long |>
      filter(scale %in% c("system", "subregion")) |>
      pivot_wider(
        id_cols     = all_of(group_keys),
        names_from  = scale,
        values_from = skill_score,
        values_fn   = mean          # removes duplicates
      ) |>
      filter(!is.na(system), !is.na(subregion)) |>
      mutate(
        delta_skill = system - subregion,
        window      = factor(window)
      )
    
    write.csv(delta_df,
              file.path(scale_run_folder, "window_delta_skill.csv"),
              row.names = FALSE)
    cat("  ✓ Saved: window_delta_skill.csv\n")
    
    # -- Cross-model delta plot --
    p_delta <- ggplot(delta_df,
                      aes(x = window, y = delta_skill,
                          color = model_key, group = model_key)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.7) +
      geom_line(linewidth = 0.9, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.9) +
      facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
      theme_classic(base_size = 13) +
      labs(
        title    = "Skill Delta: System − Subregion per Sliding Window",
        subtitle = "Positive = system scale outperforms subregion; Negative = subregion wins",
        x        = "CV Window (t)",
        y        = "Δ Skill Score (system − subregion)",
        color    = "Model"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 11),
        plot.title       = element_text(face = "bold", size = 15),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(scale_run_folder, "window_delta_skill.png"),
           p_delta, width = 12, height = 10, dpi = 300)
    cat("  ✓ Saved: window_delta_skill.png\n")
    
    # -- Per-model delta plots --
    for (mk in unique(delta_df$model_key)) {
      model_folder <- file.path(scale_run_folder, mk)
      
      p_model_delta <- delta_df |>
        filter(model_key == mk) |>
        ggplot(aes(x = window, y = delta_skill,
                   color = metric_label, group = metric_label)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray50", linewidth = 0.7) +
        geom_line(linewidth = 1, alpha = 0.8) +
        geom_point(size = 2.5) +
        scale_color_brewer(palette = "Dark2") +
        theme_classic(base_size = 13) +
        labs(
          title    = glue("{mk}: Skill Delta per Window"),
          subtitle = "system − subregion  |  positive = system wins",
          x        = "CV Window (t)",
          y        = "Δ Skill Score",
          color    = "Metric"
        ) +
        theme(
          legend.position  = "bottom",
          axis.line        = element_line(linewidth = 1),
          plot.title       = element_text(face = "bold", size = 14),
          plot.subtitle    = element_text(size = 11, color = "gray40")
        )
      
      ggsave(file.path(model_folder, "window_delta_skill.png"),
             p_model_delta, width = 10, height = 6, dpi = 300)
      cat(glue("  ✓ Saved: {mk}/window_delta_skill.png\n"))
    }
    
  } else {
    cat("  ⚠ Both 'system' and 'subregion' needed for delta — skipping.\n")
  }
}

# =============================================================================
# PART 3: CROSS-MODEL SUMMARY (optional — aggregated across windows)
# =============================================================================

cat("\n📊 Generating cross-model summary...\n")

if (exists("all_window_metrics") && !is.null(all_window_metrics) && nrow(all_window_metrics) > 0) {
  
  skill_metrics <- intersect(
    c("crps_skill", "rps_skill", "rmse_skill"),
    names(all_window_metrics)
  )
  
  overall_summary <- all_window_metrics |>
    pivot_longer(cols = all_of(skill_metrics),
                 names_to  = "metric",
                 values_to = "skill_score") |>
    filter(!is.na(skill_score)) |>
    mutate(skill_score = pmax(skill_score, -1)) |>
    group_by(model_key, scale, metric) |>
    summarise(
      mean_skill   = mean(skill_score,   na.rm = TRUE),
      median_skill = median(skill_score, na.rm = TRUE),
      n            = n(),
      .groups      = "drop"
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
cat("  • base_config.rds             - Base configuration\n")
cat("  • window_skill_all_scales.csv - Per-window skill scores\n")
cat("  • window_delta_skill.csv      - system − subregion delta per window\n")
cat("  • window_delta_skill.png      - Cross-model delta plot\n")
cat("  • all_models_summary.csv      - Aggregated summary\n\n")

for (model_key in names(all_model_results)) {
  cat(glue("  • {model_key}/\n"))
  cat(glue("    ├── window_skill_by_scale.png\n"))
  cat(glue("    ├── window_delta_skill.png\n"))
  for (scale in SCALES_TO_RUN) {
    cat(glue("    ├── {scale}/\n"))
  }
}