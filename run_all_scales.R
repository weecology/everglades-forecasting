# =============================================================================
# WRAPPER SCRIPT: RUN EACH MODEL SEPARATELY ACROSS ALL SPATIAL SCALES
# Compares each model individually to baseline across system/subregion/colony
# Computes per-window skill delta: skill_system_t - skill_subregion_t
# Works for both species-level and total count forecasting modes
# Copilot fixes:
#   - FIX 1: Redundant pmax() floor applied only once in window_long
#   - FIX 2: CONFIG$.skip_config_init explicitly guarded via guard_config_init()
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

#SPECIES_TO_RUN <- c("wost")
SPECIES_TO_RUN <- "top6"
# SPECIES_TO_RUN <- "all"

FORECAST_TOTALS <- TRUE           # TRUE/FALSE

# Spatial scales to compare
SCALES_TO_RUN <- c("system", "subregion")  # "system", "subregion", "colony"
# SCALES_TO_RUN <- c("system", "colony")
# SCALES_TO_RUN <- c("colony")

# mvgam models to test (baseline always included automatically)
#MVGAM_MODELS <- c("ar", "ar_exog", "ar_exog_plus")
MVGAM_MODELS <- c()

# fable models to test (baseline always included automatically)
#FABLE_MODELS <- c()
FABLE_MODELS <- c("arima", "tslm", "arima_exog", "gam")

# =============================================================================
# PARALLEL PROCESSING
# =============================================================================
#CONFIG$parallel$enabled <- TRUE

# =============================================================================
# CREATE TIMESTAMPED SCALE_RUN FOLDER
# =============================================================================

timestamp      <- format(Sys.time(), "%Y%m%d-%H%M")
all_models_str <- paste(c(MVGAM_MODELS, FABLE_MODELS), collapse = "-")
all_scales_str <- paste(SCALES_TO_RUN, collapse = "-")
species_str    <- paste(SPECIES_TO_RUN, collapse = "-")

scale_run_folder <- file.path(
  "results",
  paste0(species_str, "_model_scale_run_", all_models_str, "_", all_scales_str, "-", timestamp)
)
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
cat("  • Species:",         paste(SPECIES_TO_RUN, collapse = ", "), "\n")
cat("  • Forecast totals:", FORECAST_TOTALS, "\n")
cat("  • Scales:",          paste(SCALES_TO_RUN,  collapse = ", "), "\n")
cat("  • mvgam models:",    if (length(MVGAM_MODELS) > 0) paste(MVGAM_MODELS, collapse = ", ") else "none", "\n")
cat("  • fable models:",    if (length(FABLE_MODELS) > 0) paste(FABLE_MODELS, collapse = ", ") else "none", "\n")
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
for (model_key in names(models_to_test)) cat("  •", model_key, "\n")
cat("\n")

all_model_results <- list()

# =============================================================================
# PART 1: RUN EACH MODEL ACROSS ALL SCALES
# =============================================================================

for (model_key in names(models_to_test)) {
  
  model_info <- models_to_test[[model_key]]
  framework  <- model_info$framework
  model_name <- model_info$model
  
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
    
    # Build per-scale CONFIG from base — prevents main.R overwriting it
    CONFIG                       <- base_config
    CONFIG$spatial$level         <- current_scale
    CONFIG$spatial$run_by_region <- current_scale != "system"
    
    # FIX 2 (Copilot): explicitly flag to guard against config::get() re-init in main.R
    # main.R must call guard_config_init() (defined in evaluation.R) at its top
    CONFIG$.skip_config_init <- TRUE
    CONFIG$parallel$enabled  <- FALSE
    
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
    
    # Pre-load model functions into global environment
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
      source("main.R")
      
      if (exists("run_folder") && !is.null(run_folder)) {
        dest_folder   <- file.path(model_folder, current_scale)
        files_to_copy <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
        
        for (src_file in files_to_copy) {
          rel_path  <- sub(paste0(run_folder, "/"), "", src_file)
          dest_file <- file.path(dest_folder, rel_path)
          dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
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
# PART 2: EXTRACT PER-WINDOW SKILL AND COMPUTE SCALE DELTAS
# delta_skill = skill_system_t - skill_subregion_t
# Works for both FORECAST_TOTALS = TRUE and FALSE
# =============================================================================

cat("📊 Extracting per-window skill scores and computing scale deltas...\n\n")

# -----------------------------------------------------------------------------
# HELPER: extract window-level metrics from a results folder
# -----------------------------------------------------------------------------

extract_model_metrics <- function(folder_path, scale_name, framework) {
  file_path <- file.path(folder_path, "forecast_results.rds")
  if (!file.exists(file_path)) return(NULL)
  
  res <- readRDS(file_path)
  
  if (framework == "mvgam") {
    if (is.null(res$mvgam) || is.null(res$mvgam$metrics)) return(NULL)
    metrics <- res$mvgam$metrics
    
  } else {
    if (is.null(res$fable) || is.null(res$fable$metrics)) return(NULL)
    metrics <- res$fable$metrics |> rename(model = .model)
  }
  
  if (!"window" %in% names(metrics)) {
    warning(glue("No 'window' column in metrics for scale={scale_name}. Skipping."))
    return(NULL)
  }
  
  metrics |>
    as_tibble() |>
    select(
      model,
      any_of(c("species", "region")),
      window,
      any_of("test_start"),
      any_of(c("crps_skill", "rps_skill", "rmse_skill"))
    ) |>
    mutate(framework = framework, scale = scale_name)
}

# -----------------------------------------------------------------------------
# Collect window-level metrics across all models and scales
# -----------------------------------------------------------------------------

all_window_metrics <- bind_rows(lapply(names(all_model_results), function(model_key) {
  model_info    <- models_to_test[[model_key]]
  framework     <- model_info$framework
  model_name    <- model_info$model
  model_folders <- all_model_results[[model_key]]
  
  bind_rows(lapply(names(model_folders), function(scale) {
    folder <- model_folders[[scale]]
    if (is.null(folder)) return(NULL)
    
    m <- extract_model_metrics(folder, scale, framework)
    if (is.null(m)) return(NULL)
    
    m |>
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
  
  # FIX 1 (Copilot): pmax floor applied exactly once here, not repeated downstream
  window_long <- all_window_metrics |>
    pivot_longer(
      cols      = all_of(skill_metrics),
      names_to  = "metric",
      values_to = "skill_score"
    ) |>
    filter(!is.na(skill_score)) |>
    mutate(
      skill_score  = pmax(skill_score, -1),
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
  
  # Scale colors
  all_scale_colors <- c("colony" = "#00B050", "subregion" = "#FF0000", "system" = "#0000FF")
  scale_colors     <- all_scale_colors[SCALES_TO_RUN]
  
  # ===========================================================================
  # PER-MODEL: window skill plot + delta plot
  # ===========================================================================
  
  for (mk in names(all_model_results)) {
    
    model_folder <- file.path(scale_run_folder, mk)
    
    cat(glue("\n📈 Generating window plots for {mk}...\n"))
    
    model_long <- window_long |>
      filter(model_key == mk) |>
      mutate(
        scale  = factor(scale, levels = SCALES_TO_RUN),
        window = factor(window)
      )
    
    if (nrow(model_long) == 0) {
      cat(glue("  ⚠ No data for {mk}, skipping.\n"))
      next
    }
    
    # Add entity label: species name or "Total"
    if (!FORECAST_TOTALS && "species" %in% names(model_long)) {
      model_long <- model_long |> mutate(entity = species)
    } else {
      model_long <- model_long |> mutate(entity = "Total")
    }
    
    # -------------------------------------------------------------------------
    # PLOT A: Skill score per window, colored by scale, faceted by metric x entity
    # -------------------------------------------------------------------------
    
    p_window <- ggplot(model_long,
                       aes(x = window, y = skill_score,
                           color = scale, group = scale)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.7) +
      geom_line(linewidth = 0.9, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.9) +
      facet_grid(entity ~ metric_label, scales = "free_y") +
      scale_color_manual(values = scale_colors) +
      theme_classic(base_size = 12) +
      labs(
        title    = glue("{mk}: Skill Score per Sliding Window"),
        subtitle = "Each point = one CV window | colored by spatial scale",
        x        = "CV Window (t)",
        y        = "Skill Score (vs Baseline)",
        color    = "Scale"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 10),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(model_folder, "window_skill_by_scale.png"),
           p_window,
           width  = max(10, length(unique(model_long$window)) * 0.5),
           height = max(6,  length(unique(model_long$entity)) * 2.5),
           dpi    = 300)
    cat(glue("  ✓ Saved: {mk}/window_skill_by_scale.png\n"))
    
    # -------------------------------------------------------------------------
    # DELTA: system - subregion per window
    # -------------------------------------------------------------------------
    
    available_scales <- as.character(unique(model_long$scale))
    
    if (!all(c("system", "subregion") %in% available_scales)) {
      cat(glue("  ⚠ Both system and subregion needed for delta — skipping {mk}.\n"))
      next
    }
    
    # Build id cols — exclude region (averaged out below), no pmax re-applied
    delta_id_cols <- intersect(
      c("model_key",
        if (!FORECAST_TOTALS) "species",
        "entity", "window", "test_start", "metric", "metric_label"),
      names(model_long)
    )
    
    # Average across regions first so subregion has one row per (entity x window x metric)
    delta_df <- model_long |>
      filter(scale %in% c("system", "subregion")) |>
      group_by(across(all_of(delta_id_cols)), scale) |>
      summarise(skill_score = mean(skill_score, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(
        id_cols     = all_of(delta_id_cols),
        names_from  = scale,
        values_from = skill_score,
        values_fn   = mean       # safety net for any residual duplicates
      ) |>
      filter(!is.na(system), !is.na(subregion)) |>
      mutate(
        delta_skill = system - subregion,   # positive = system better
        window      = factor(window)
      )
    
    if (nrow(delta_df) == 0) {
      cat(glue("  ⚠ Delta table empty for {mk} after filtering — skipping delta plot.\n"))
      next
    }
    
    cat(glue("  Delta rows: {nrow(delta_df)} | ",
             "windows: {length(unique(delta_df$window))} | ",
             "metrics: {paste(unique(delta_df$metric_label), collapse=', ')}\n"))
    
    # -------------------------------------------------------------------------
    # PLOT B: Delta per window, faceted by entity
    # -------------------------------------------------------------------------
    
    p_delta <- ggplot(delta_df,
                      aes(x = window, y = delta_skill,
                          color = metric_label, group = metric_label)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.7) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2.5) +
      facet_wrap(~entity, ncol = 2, scales = "free_y") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic(base_size = 12) +
      labs(
        title    = glue("{mk}: Skill Delta per Window (System − Subregion)"),
        subtitle = "Positive = system outperforms subregion | Negative = subregion wins",
        x        = "CV Window (t)",
        y        = "Δ Skill Score (system − subregion)",
        color    = "Metric"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 10),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(model_folder, "window_delta_skill.png"),
           p_delta,
           width  = max(10, length(unique(delta_df$window)) * 0.5),
           height = max(6,  length(unique(delta_df$entity))  * 2.5),
           dpi    = 300)
    cat(glue("  ✓ Saved: {mk}/window_delta_skill.png\n"))
    
    write.csv(delta_df,
              file.path(model_folder, "window_delta_skill.csv"),
              row.names = FALSE)
    cat(glue("  ✓ Saved: {mk}/window_delta_skill.csv\n"))
  }
  
  # ===========================================================================
  # CROSS-MODEL DELTA PLOTS: Density, ECDF, Violin
  # Uses cross_delta — already computed above
  # x-axis = delta_skill (positive = system better, negative = subregion better)
  # each model = one line/fill (density + ECDF) or one violin
  # ===========================================================================
  
  
  
  if (all(c("system", "subregion") %in% SCALES_TO_RUN)) {
    
    cat("\n📊 Generating cross-model delta plot...\n")
    
    cross_id_cols <- intersect(
      c("model_key",
        if (!FORECAST_TOTALS) "species",
        "window", "test_start", "metric", "metric_label"),
      names(window_long)
    )
    
    # Average across regions, no pmax re-applied (already done in window_long)
    cross_delta <- window_long |>
      filter(scale %in% c("system", "subregion")) |>
      group_by(across(all_of(cross_id_cols)), scale) |>
      summarise(skill_score = mean(skill_score, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(
        id_cols     = all_of(cross_id_cols),
        names_from  = scale,
        values_from = skill_score,
        values_fn   = mean
      ) |>
      filter(!is.na(system), !is.na(subregion)) |>
      mutate(
        delta_skill = system - subregion,
        window      = factor(window)
      )
    
    if (nrow(cross_delta) > 0) {
      
      if (!FORECAST_TOTALS && "species" %in% names(cross_delta)) {
        cross_delta <- cross_delta |> mutate(entity = species)
      } else {
        cross_delta <- cross_delta |> mutate(entity = "Total")
      }
      
      p_cross <- ggplot(cross_delta,
                        aes(x = window, y = delta_skill,
                            color = model_key, group = model_key)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray50", linewidth = 0.7) +
        geom_line(linewidth = 0.9, alpha = 0.8) +
        geom_point(size = 2, alpha = 0.9) +
        facet_grid(entity ~ metric_label, scales = "free_y") +
        theme_classic(base_size = 12) +
        labs(
          title    = "All Models: Skill Delta per Window (System − Subregion)",
          subtitle = "Positive = system outperforms subregion | Negative = subregion wins",
          x        = "CV Window (t)",
          y        = "Δ Skill Score (system − subregion)",
          color    = "Model"
        ) +
        theme(
          legend.position  = "bottom",
          axis.line        = element_line(linewidth = 1),
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
          strip.background = element_rect(fill = "grey90", color = NA),
          strip.text       = element_text(face = "bold", size = 10),
          plot.title       = element_text(face = "bold", size = 14),
          plot.subtitle    = element_text(size = 11, color = "gray40")
        )
      
      ggsave(file.path(scale_run_folder, "all_models_window_delta.png"),
             p_cross,
             width  = max(12, length(unique(cross_delta$window)) * 0.5),
             height = max(8,  length(unique(cross_delta$entity))  * 2.5),
             dpi    = 300)
      cat("  ✓ Saved: all_models_window_delta.png\n")
      
      write.csv(cross_delta,
                file.path(scale_run_folder, "all_models_window_delta.csv"),
                row.names = FALSE)
      cat("  ✓ Saved: all_models_window_delta.csv\n")
    }
  }
  
  
  # ===========================================================================
  # CROSS-MODEL DELTA PLOTS: Density, ECDF, Violin
  # Uses cross_delta — already computed above
  # x-axis = delta_skill (positive = system better, negative = subregion better)
  # each model = one line/fill (density + ECDF) or one violin
  # ===========================================================================
  
  if (nrow(cross_delta) > 0) {
    
    # -------------------------------------------------------------------------
    # PLOT 1: Density — delta_skill on x, one line per model, faceted by metric
    # -------------------------------------------------------------------------
    
    p_delta_density <- ggplot(cross_delta,
                              aes(x = delta_skill, fill = model_key, color = model_key)) +
      geom_density(alpha = 0.15, linewidth = 1.1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
      annotate("text", x = -0.05, y = Inf, label = "← Subregion better",
               hjust = 1, vjust = 1.5, size = 3.5, color = "gray40") +
      annotate("text", x =  0.05, y = Inf, label = "System better →",
               hjust = 0, vjust = 1.5, size = 3.5, color = "gray40") +
      facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
      scale_fill_brewer(palette  = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic(base_size = 13) +
      labs(
        title    = "All Models: Distribution of Skill Delta",
        subtitle = "System − Subregion | Positive = system wins | Negative = subregion wins",
        x        = "Δ Skill Score (system − subregion)",
        y        = "Density",
        fill     = "Model",
        color    = "Model"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 11),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(scale_run_folder, "all_models_delta_density.png"),
           p_delta_density, width = 10, height = 10, dpi = 300)
    cat("  ✓ Saved: all_models_delta_density.png\n")
    
    # -------------------------------------------------------------------------
    # PLOT 2: ECDF — delta_skill on x (coord_flip mirrors your existing ECDF [1]),
    # one line per model, faceted by metric
    # -------------------------------------------------------------------------
    
    p_delta_ecdf <- ggplot(cross_delta,
                           aes(x = delta_skill, color = model_key)) +
      stat_ecdf(linewidth = 1.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
      annotate("text", x = -0.05, y = 0.5, label = "← Subregion better",
               hjust = 1, size = 3.5, color = "gray40") +
      annotate("text", x =  0.05, y = 0.5, label = "System better →",
               hjust = 0, size = 3.5, color = "gray40") +
      facet_wrap(~metric_label, ncol = 1, scales = "free_x") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic(base_size = 13) +
      labs(
        title    = "All Models: ECDF of Skill Delta",
        subtitle = "System − Subregion | Positive = system wins | Negative = subregion wins",
        x        = "Δ Skill Score (system − subregion)",
        y        = "Cumulative Probability",
        color    = "Model"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 11),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(scale_run_folder, "all_models_delta_ecdf.png"),
           p_delta_ecdf, width = 10, height = 10, dpi = 300)
    cat("  ✓ Saved: all_models_delta_ecdf.png\n")
    
    # -------------------------------------------------------------------------
    # PLOT 3: Violin — replaces the scatter/jitter plot [1]
    # x = model, y = delta_skill, one violin per model, faceted by metric
    # -------------------------------------------------------------------------
    
    p_delta_violin <- ggplot(cross_delta,
                             aes(x = model_key, y = delta_skill,
                                 fill = model_key, color = model_key)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
      geom_violin(alpha = 0.3, linewidth = 0.9, trim = FALSE) +
      geom_boxplot(width = 0.08, alpha = 0.8, outlier.shape = NA, color = "gray20") +
      facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
      scale_fill_brewer(palette  = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic(base_size = 13) +
      labs(
        title    = "All Models: Skill Delta Distribution (System − Subregion)",
        subtitle = "Positive = system wins | Negative = subregion wins | Line = median",
        x        = NULL,
        y        = "Δ Skill Score (system − subregion)",
        fill     = "Model",
        color    = "Model"
      ) +
      theme(
        legend.position  = "bottom",
        axis.line        = element_line(linewidth = 1),
        axis.text.x      = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text       = element_text(face = "bold", size = 11),
        plot.title       = element_text(face = "bold", size = 14),
        plot.subtitle    = element_text(size = 11, color = "gray40")
      )
    
    ggsave(file.path(scale_run_folder, "all_models_delta_violin.png"),
           p_delta_violin, width = 12, height = 10, dpi = 300)
    cat("  ✓ Saved: all_models_delta_violin.png\n")
  }
  
  
  
  # ===========================================================================
  # PART 3: AGGREGATED CROSS-MODEL SUMMARY (across windows)
  # ===========================================================================
  
  cat("\n📊 Generating aggregated cross-model summary...\n")
  
  overall_summary <- window_long |>
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
cat("  • base_config.rds                - Base configuration\n")
cat("  • window_skill_all_scales.csv    - Per-window skill scores (all models/scales)\n")
cat("  • all_models_window_delta.png    - Cross-model delta plot\n")
cat("  • all_models_window_delta.csv    - Cross-model delta table\n")
cat("  • all_models_summary.csv         - Aggregated summary across windows\n\n")

for (model_key in names(all_model_results)) {
  cat(glue("  • {model_key}/\n"))
  cat(glue("    ├── window_skill_by_scale.png  - Skill per window by scale\n"))
  cat(glue("    ├── window_delta_skill.png     - Delta per window\n"))
  cat(glue("    ├── window_delta_skill.csv     - Delta table\n"))
  for (scale in SCALES_TO_RUN) {
    cat(glue("    ├── {scale}/\n"))
  }
}