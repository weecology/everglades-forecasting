# =============================================================================
# WRAPPER SCRIPT: RUN EACH MODEL SEPARATELY ACROSS ALL SPATIAL SCALES
# Compares each model individually to baseline across system/subregion/colony
# Computes per-window skill delta: skill_system_t - skill_subregion_t
# Works for both species-level and total count forecasting modes
# =============================================================================

# config loaded via config::get() directly — do not use library(config)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(glue)
library(stringr)

# =============================================================================
# CONFIGURATION - EDIT THIS SECTION
# =============================================================================
SPECIES_TO_RUN   <- "top6"
FORECAST_TOTALS  <- FALSE
SCALES_TO_RUN    <- c("system", "subregion")


# FABLE_MODELS     <- c("arima", "tslm", "arima_exog", "gam")
FABLE_MODELS     <- c()
MVGAM_MODELS     <- c("ar", "ar_exog", "ar_exog_plus")
# MVGAM_MODELS     <- c()


PARALLEL         <- TRUE   # Set to FALSE to run sequentially
PARALLEL_WORKERS <- 3      # Number of parallel workers (ignored if PARALLEL = FALSE)

# =============================================================================
# PRE-DOWNLOAD DATA TO PREVENT PARALLEL RACE CONDITIONS
# =============================================================================
if (!dir.exists("SiteandMethods") || difftime(Sys.time(), file.info("SiteandMethods")$mtime, units = "days") > 7) {
  cat("Pre-downloading wader observation data...\n")
  wader::download_observations(".")
}

# =============================================================================
# SOURCE PARALLEL UTILITIES
# =============================================================================
source("parallel_utils.R")

# =============================================================================
# HELPER: extract start year from window label or test_start
# Handles formats: "2003-2007", "2003_2007", "W01", integer years, etc.
# =============================================================================
extract_window_start_year <- function(window_vec, test_start_vec = NULL) {
  if (!is.null(test_start_vec)) {
    yr <- suppressWarnings(as.integer(str_extract(as.character(test_start_vec), "\\d{4}")))
    if (any(!is.na(yr))) return(yr)
  }
  yr <- suppressWarnings(as.integer(str_extract(as.character(window_vec), "\\d{4}")))
  if (any(!is.na(yr))) return(yr)
  suppressWarnings(as.integer(as.character(window_vec)))
}

# =============================================================================
# CREATE TIMESTAMPED SCALE_RUN FOLDER
# =============================================================================
timestamp        <- format(Sys.time(), "%Y%m%d-%H%M")
all_models_str   <- paste(c(MVGAM_MODELS, FABLE_MODELS), collapse = "-")
all_scales_str   <- paste(SCALES_TO_RUN,  collapse = "-")
species_str      <- paste(SPECIES_TO_RUN, collapse = "-")
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
cat("  • Parallel:",        PARALLEL, "\n")
if (PARALLEL) cat("  • Workers:", PARALLEL_WORKERS, "\n")
cat("✓ Base configuration saved\n\n")

# =============================================================================
# BUILD MODEL LIST
# =============================================================================
models_to_test <- list()
if (base_config$run_mvgam) {
  for (m in MVGAM_MODELS)
    models_to_test[[paste0("mvgam_", m)]] <- list(framework = "mvgam", model = m)
}
if (base_config$run_fable) {
  for (m in FABLE_MODELS)
    models_to_test[[paste0("fable_", m)]] <- list(framework = "fable", model = m)
}
if (length(models_to_test) == 0)
  stop("!!!! - No models specified! Please add models to MVGAM_MODELS or FABLE_MODELS")

cat("📋 Models to test:", length(models_to_test), "\n")
for (model_key in names(models_to_test)) cat("  •", model_key, "\n")
cat("\n")

# =============================================================================
# PART 1: RUN EACH MODEL ACROSS ALL SCALES
# =============================================================================

# -----------------------------------------------------------------------------
# Refactored model runner — one model across all scales
# NOTE: CONFIG$parallel$enabled stays FALSE inside each main.R call
#       to avoid nested parallelism with Stan
# -----------------------------------------------------------------------------
run_single_model <- function(model_key) {
  
  assign("base_profile", "run_all_scales_all", envir = .GlobalEnv)
  
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
    
    CONFIG                       <- base_config
    CONFIG$spatial$level         <- current_scale
    CONFIG$spatial$run_by_region <- current_scale != "system"
    CONFIG$.skip_config_init     <- TRUE
    CONFIG$parallel$enabled      <- FALSE
    
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
    
    assign("CONFIG", CONFIG, envir = .GlobalEnv)
    
    if (framework == "mvgam") {
      model_file <- file.path("models", paste0("mvgam_", model_name, ".R"))
      if (file.exists(model_file)) { source(model_file, local = FALSE); cat(glue("  ✓ Pre-loaded {model_name}\n")) }
      baseline_file <- file.path("models", "mvgam_baseline.R")
      if (file.exists(baseline_file)) { source(baseline_file, local = FALSE); cat("  ✓ Pre-loaded baseline\n") }
    }
    
    # -------------------------------------------------------------------------
    # Run with tryCatch — capture both errors and the run_folder result
    # -------------------------------------------------------------------------
    run_succeeded <- FALSE
    run_folder    <- NULL
    
    tryCatch({
      source("main.R")
      
      # Verify run_folder was actually created and contains results
      if (!exists("run_folder") || is.null(run_folder)) {
        stop("main.R completed but run_folder was not set")
      }
      if (!dir.exists(run_folder)) {
        stop(glue("run_folder was set to '{run_folder}' but directory does not exist"))
      }
      rds_path <- file.path(run_folder, "forecast_results.rds")
      if (!file.exists(rds_path)) {
        stop(glue("run_folder exists but forecast_results.rds is missing: {rds_path}"))
      }
      
      run_succeeded <- TRUE
      
    }, error = function(e) {
      cat(glue("\n  ✗ {model_key} at {current_scale} FAILED\n"))
      cat(glue("    Reason: {e$message}\n"))
      cat(glue("    run_folder at time of error: {if (exists('run_folder') && !is.null(run_folder)) run_folder else 'not set'}\n"))
    })
    
    # -------------------------------------------------------------------------
    # Only copy results if run genuinely succeeded
    # -------------------------------------------------------------------------
    if (run_succeeded && !is.null(run_folder)) {
      
      dest_folder   <- file.path(model_folder, current_scale)
      files_to_copy <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
      
      if (length(files_to_copy) == 0) {
        cat(glue("  ⚠ run_folder exists but is empty: {run_folder}\n"))
        model_scale_results[[current_scale]] <- NULL
      } else {
        for (src_file in files_to_copy) {
          rel_path  <- sub(paste0(run_folder, "/"), "", src_file)
          dest_file <- file.path(dest_folder, rel_path)
          dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
          file.copy(src_file, dest_file, overwrite = TRUE)
        }
        model_scale_results[[current_scale]] <- dest_folder
        unlink(run_folder, recursive = TRUE)
        cat(glue("  ✓ Results saved to: {dest_folder}\n"))
      }
      
    } else {
      
      # Log a diagnostic summary to a file so failures aren't lost silently
      fail_log <- file.path(model_folder, glue("FAILED_{current_scale}.txt"))
      writeLines(c(
        glue("Model:  {model_key}"),
        glue("Scale:  {current_scale}"),
        glue("Time:   {format(Sys.time())}"),
        glue("run_folder: {if (exists('run_folder') && !is.null(run_folder)) run_folder else 'not set'}"),
        "",
        "Check that main.R completes without error for this scale and saves forecast_results.rds."
      ), fail_log)
      
      cat(glue("  ✗ Failure logged to: {fail_log}\n"))
      model_scale_results[[current_scale]] <- NULL
    }
    
    gc()
  }
  
  # Report which scales succeeded and which failed
  cat(glue("\n✓ {model_key} complete\n"))
  succeeded <- names(Filter(Negate(is.null), model_scale_results))
  failed    <- setdiff(SCALES_TO_RUN, succeeded)
  if (length(succeeded) > 0) cat(glue("  ✓ Succeeded: {paste(succeeded, collapse = ', ')}\n"))
  if (length(failed)    > 0) cat(glue("  ✗ Failed:    {paste(failed,    collapse = ', ')}\n"))
  
  return(model_scale_results)
}

# -----------------------------------------------------------------------------
# Run models — parallel or sequential based on PARALLEL flag
# -----------------------------------------------------------------------------
if (PARALLEL) {
  all_model_results <- run_models_parallel(
    model_keys = names(models_to_test),
    run_fn     = run_single_model,
    workers    = PARALLEL_WORKERS
  )
} else {
  all_model_results <- lapply(names(models_to_test), run_single_model)
  names(all_model_results) <- names(models_to_test)
}

cat("\n✅ ALL MODEL RUNS COMPLETE!\n\n")

# =============================================================================
# PART 2: EXTRACT PER-WINDOW SKILL AND COMPUTE SCALE DELTAS
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
    m |> filter(model == model_name) |> mutate(model_key = model_key)
  }))
}))

if (is.null(all_window_metrics) || nrow(all_window_metrics) == 0) {
  warning("No window-level metrics found. Skipping delta computation.")
} else {
  
  skill_metrics <- intersect(
    c("crps_skill", "rps_skill", "rmse_skill"),
    names(all_window_metrics)
  )
  
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
      ),
      window_start_year = extract_window_start_year(
        window,
        if ("test_start" %in% names(pick(everything()))) test_start else NULL
      )
    )
  
  write.csv(window_long,
            file.path(scale_run_folder, "window_skill_all_scales.csv"),
            row.names = FALSE)
  cat("  ✓ Saved: window_skill_all_scales.csv\n")
  
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
      mutate(scale = factor(scale, levels = SCALES_TO_RUN))
    
    if (nrow(model_long) == 0) { cat(glue("  ⚠ No data for {mk}, skipping.\n")); next }
    
    if (!FORECAST_TOTALS && "species" %in% names(model_long)) {
      model_long <- model_long |> mutate(entity = species)
    } else {
      model_long <- model_long |> mutate(entity = "Total")
    }
    
    year_breaks <- sort(unique(model_long$window_start_year))
    
    p_window <- ggplot(model_long,
                       aes(x = window_start_year, y = skill_score,
                           color = scale, group = scale)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7) +
      geom_line(linewidth = 0.9, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.9) +
      facet_grid(entity ~ metric_label, scales = "free_y") +
      scale_color_manual(values = scale_colors) +
      scale_x_continuous(name = "Forecast Window Start Year", breaks = year_breaks) +
      theme_classic(base_size = 12) +
      labs(
        title    = glue("{mk}: Skill Score per Sliding Window"),
        subtitle = "Each point = one CV window | colored by spatial scale",
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
           width  = max(10, length(year_breaks) * 0.7),
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
    
    delta_id_cols <- intersect(
      c("model_key",
        if (!FORECAST_TOTALS) "species",
        "entity", "window", "window_start_year", "test_start", "metric", "metric_label"),
      names(model_long)
    )
    
    delta_df <- model_long |>
      filter(scale %in% c("system", "subregion")) |>
      group_by(across(all_of(delta_id_cols)), scale) |>
      summarise(skill_score = mean(skill_score, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(
        id_cols     = all_of(delta_id_cols),
        names_from  = scale,
        values_from = skill_score,
        values_fn   = mean
      ) |>
      filter(!is.na(system), !is.na(subregion)) |>
      mutate(delta_skill = system - subregion)
    
    if (nrow(delta_df) == 0) {
      cat(glue("  ⚠ Delta table empty for {mk} — skipping delta plot.\n"))
      next
    }
    
    cat(glue("  Delta rows: {nrow(delta_df)} | ",
             "windows: {length(unique(delta_df$window_start_year))} | ",
             "metrics: {paste(unique(delta_df$metric_label), collapse=', ')}\n"))
    
    year_breaks_delta <- sort(unique(delta_df$window_start_year))
    
    p_delta <- ggplot(delta_df,
                      aes(x = window_start_year, y = delta_skill,
                          color = metric_label, group = metric_label)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2.5) +
      facet_wrap(~entity, ncol = 2, scales = "free_y") +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(name = "Forecast Window Start Year", breaks = year_breaks_delta) +
      theme_classic(base_size = 12) +
      labs(
        title    = glue("{mk}: Skill Delta per Window (System − Subregion)"),
        subtitle = "Positive = system outperforms subregion | Negative = subregion wins",
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
           width  = max(10, length(year_breaks_delta) * 0.7),
           height = max(6,  length(unique(delta_df$entity)) * 2.5),
           dpi    = 300)
    cat(glue("  ✓ Saved: {mk}/window_delta_skill.png\n"))
    
    write.csv(delta_df,
              file.path(model_folder, "window_delta_skill.csv"),
              row.names = FALSE)
    cat(glue("  ✓ Saved: {mk}/window_delta_skill.csv\n"))
  }
  
  # ===========================================================================
  # CROSS-MODEL DELTA PLOT (all_models_window_delta.png)
  # ===========================================================================
  if (all(c("system", "subregion") %in% SCALES_TO_RUN)) {
    
    cat("\n📊 Generating cross-model delta plot...\n")
    
    cross_id_cols <- intersect(
      c("model_key",
        if (!FORECAST_TOTALS) "species",
        "window", "window_start_year", "test_start", "metric", "metric_label"),
      names(window_long)
    )
    
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
      mutate(delta_skill = system - subregion)
    
    if (nrow(cross_delta) > 0) {
      
      if (!FORECAST_TOTALS && "species" %in% names(cross_delta)) {
        cross_delta <- cross_delta |> mutate(entity = species)
      } else {
        cross_delta <- cross_delta |> mutate(entity = "Total")
      }
      
      cross_year_breaks <- sort(unique(cross_delta$window_start_year))
      
      p_cross <- ggplot(cross_delta,
                        aes(x = window_start_year, y = delta_skill,
                            color = model_key, group = model_key)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.7) +
        geom_line(linewidth = 0.9, alpha = 0.8) +
        geom_point(size = 2, alpha = 0.9) +
        facet_grid(entity ~ metric_label, scales = "free_y") +
        scale_x_continuous(name = "Forecast Window Start Year", breaks = cross_year_breaks) +
        theme_classic(base_size = 12) +
        labs(
          title    = "All Models: Skill Delta per Window (System − Subregion)",
          subtitle = "Positive = system outperforms subregion | Negative = subregion wins",
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
             width  = max(12, length(cross_year_breaks) * 0.7),
             height = max(8,  length(unique(cross_delta$entity)) * 2.5),
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
  # ===========================================================================
  if (exists("cross_delta") && nrow(cross_delta) > 0) {
    
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
        y        = "Density", fill = "Model", color = "Model"
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
        y        = "Cumulative Probability", color = "Model"
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
        fill     = "Model", color = "Model"
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
  # PART 3: AGGREGATED CROSS-MODEL SUMMARY
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
cat("  • all_models_window_delta.png    - Cross-model delta plot (x = start year)\n")
cat("  • all_models_window_delta.csv    - Cross-model delta table\n")
cat("  • all_models_summary.csv         - Aggregated summary across windows\n\n")
for (model_key in names(all_model_results)) {
  cat(glue("  • {model_key}/\n"))
  cat(glue("    ├── window_skill_by_scale.png  - Skill per window by scale\n"))
  cat(glue("    ├── window_delta_skill.png     - Delta per window\n"))
  cat(glue("    ├── window_delta_skill.csv     - Delta table\n"))
  for (scale in SCALES_TO_RUN) cat(glue("    ├── {scale}/\n"))
}