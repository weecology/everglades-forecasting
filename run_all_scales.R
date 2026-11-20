# =============================================================================
# WRAPPER SCRIPT: RUN EACH MODEL SEPARATELY ACROSS ALL SPATIAL SCALES
# Compares each model individually to baseline across system/subregion/colony
# Computes per-window skill delta: skill_system_t - skill_subregion_t
# Works for both species-level and total count forecasting modes
# =============================================================================
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(ggExtra)
library(ggside)
library(glue)
library(stringr)
library(mvgam)
library(fabletools)
library(conflicted)
library(pdftools)
conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
source("data_functions.R")
# =============================================================================
# CONFIGURATION - EDIT THIS SECTION
# =============================================================================
SPECIES_TO_RUN   <- c('wost') #top6
FORECAST_TOTALS  <- FALSE
SCALES_TO_RUN    <- c("system", 'subregion') #c("system", "subregion", "colony")
FABLE_MODELS     <- c()
# c("arima", "tslm", "arima_exog", "gam")
MVGAM_MODELS     <- c('ar', 'ar_exog')
# c('ar', 'ar_exog', 'ar_exog_plus', 'trait', 'species_specific', 'trait2')
PARALLEL         <- FALSE
PARALLEL_WORKERS <- 3
FORECAST_HORIZON  <- 2 # 2 seems like the most reasonable given the data
CV_WINDOWS       <- 1
#NULL equals max, can chose 1,2,3 windows 
# =============================================================================
# SERIAL PRECOMPUTE OF ALL SHARED ARTIFACTS
# =============================================================================
cat("Precomputing shared data artifacts...\n")
needs_download <- !dir.exists("SiteandMethods") || {
  stamp <- file.info("SiteandMethods")$mtime
  is.na(stamp) || difftime(Sys.time(), stamp, units = "days") > 7
}
if (needs_download) {
  cat("  Downloading wader observation data...\n")
  wader::download_observations(".")
  cat("  ✓ Wader observation data downloaded\n")
} else {
  cat("  ✓ Wader observation data current — skipping download\n")
}
cat("  Pre-caching water covariate data...\n")
tryCatch({
  get_data_water()
  cat("  ✓ Water covariate data cached\n")
}, error = function(e) warning("Water data pre-cache failed: ", e$message))
cat("  Pre-caching wading bird data...\n")
tryCatch({
  tmp_cfg <- config::get(config = "run_all_scales_all")
  tmp_cfg$spatial$include_species <- SPECIES_TO_RUN
  tmp_cfg$spatial$forecast_totals <- FORECAST_TOTALS
  for (sc in SCALES_TO_RUN) {
    tmp_cfg$spatial$level <- sc
    get_wading_bird_data(config = tmp_cfg, cache = TRUE)
    cat(glue("  ✓ Wading bird data cached for scale: {sc}\n"))
  }
}, error = function(e) warning("Wading bird data pre-cache failed: ", e$message))
dir.create("cache", showWarnings = FALSE, recursive = TRUE)
cat("✓ All shared artifacts precomputed — safe to start parallel workers\n\n")
# =============================================================================
# SOURCE PARALLEL UTILITIES
# =============================================================================
source("parallel_utils.R")
# =============================================================================
# HELPER: extract start year from window label or test_start
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
base_config$forecast_horizon        <- FORECAST_HORIZON
base_config$cv_windows              <- CV_WINDOWS
# Disable individual PDF generation at the main.R level - aggregated 
base_config$plots$generate_forecast_pdfs <- FALSE 
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
for (mk in names(models_to_test)) cat("  •", mk, "\n")
cat("\n")
# =============================================================================
# PART 1: RUN EACH MODEL ACROSS ALL SCALES
# =============================================================================
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
    CONFIG$model_key             <- model_key
    
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
    
    run_succeeded <- FALSE
    run_folder    <- NULL
    
    tryCatch({
      source("main.R")
      run_folder <- base::get("run_folder", envir = .GlobalEnv, inherits = FALSE)
      if (is.null(run_folder))
        stop("main.R completed but run_folder was not set in .GlobalEnv")
      if (!dir.exists(run_folder))
        stop(glue("run_folder '{run_folder}' does not exist"))
      rds_path <- file.path(run_folder, "forecast_results.rds")
      if (!file.exists(rds_path))
        stop(glue("forecast_results.rds missing from: {rds_path}"))
      run_succeeded <- TRUE
    }, error = function(e) {
      cat(glue("\n  ✗ {model_key} at {current_scale} FAILED\n"))
      cat(glue("    Reason: {e$message}\n"))
      rf <- tryCatch(
        base::get("run_folder", envir = .GlobalEnv, inherits = FALSE),
        error = function(e2) NULL
      )
      cat(glue("    run_folder at error: {if (!is.null(rf)) rf else 'not set'}\n"))
    })
    
    if (run_succeeded && !is.null(run_folder)) {
      
      dest_folder   <- file.path(model_folder, current_scale)
      files_to_copy <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
      
      if (length(files_to_copy) == 0) {
        cat(glue("  ⚠ run_folder exists but is empty: {run_folder}\n"))
        model_scale_results[[current_scale]] <- NULL
      } else {
        for (src_file in files_to_copy) {
          rel_path  <- fs::path_rel(src_file, start = run_folder)
          dest_file <- file.path(dest_folder, rel_path)
          dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)
          file.copy(src_file, dest_file, overwrite = TRUE)
        }
        model_scale_results[[current_scale]] <- dest_folder
        unlink(run_folder, recursive = TRUE)
        cat(glue("  ✓ Results saved to: {dest_folder}\n"))
      }
      
    } else {
      
      fail_log <- file.path(model_folder, glue("FAILED_{current_scale}.txt"))
      rf_val   <- tryCatch(
        base::get("run_folder", envir = .GlobalEnv, inherits = FALSE),
        error = function(e) NULL
      )
      writeLines(c(
        glue("Model:      {model_key}"),
        glue("Scale:      {current_scale}"),
        glue("Time:       {format(Sys.time())}"),
        glue("run_folder: {if (!is.null(rf_val)) rf_val else 'not set'}"),
        "",
        "Check that main.R completes without error and saves forecast_results.rds."
      ), fail_log)
      cat(glue("  ✗ Failure logged to: {fail_log}\n"))
      model_scale_results[[current_scale]] <- NULL
    }
    
    gc()
  }
  
  cat(glue("\n✓ {model_key} complete\n"))
  succeeded <- names(Filter(Negate(is.null), model_scale_results))
  failed    <- setdiff(SCALES_TO_RUN, succeeded)
  if (length(succeeded) > 0) cat(glue("  ✓ Succeeded: {paste(succeeded, collapse = ', ')}\n"))
  if (length(failed)    > 0) cat(glue("  ✗ Failed:    {paste(failed,    collapse = ', ')}\n"))
  
  return(model_scale_results)
}
# -----------------------------------------------------------------------------
# Run models — parallel or sequential
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
  
  # FIX: guard against metrics with no "model" column (every CV window failed)
  if (!"model" %in% names(metrics)) {
    warning(glue("No 'model' column in metrics for scale={scale_name}. All CV windows likely failed. Skipping."))
    return(NULL)
  }
  
  if (!"window" %in% names(metrics)) {
    warning(glue("No 'window' column in metrics for scale={scale_name}. Skipping."))
    return(NULL)
  }
  
  metrics |>
    as_tibble() |>
    dplyr::select(
      any_of("model"),
      any_of(c("species", "region")),
      any_of("window"),
      any_of("test_start"),
      any_of(c("crps_skill", "rps_skill", "rmse_skill"))
    ) |>
    mutate(framework = framework, scale = scale_name)
}
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
    names(all_window_metrics)  )
  has_test_start <- "test_start" %in% names(all_window_metrics)
  
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
        # metric == "log_skill"  ~ "Log Score Skill",
        TRUE ~ metric
      ),
      window_start_year = extract_window_start_year(
        window,
        if (has_test_start) test_start else NULL
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
    
    if (nrow(model_long) == 0) {
      cat(glue("  ⚠ No data for {mk}, skipping.\n"))
      next
    }
    
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
        title    = glue("{mk}: Skill Delta per Window (System \u2212 Subregion)"),
        subtitle = "Positive = system outperforms subregion | Negative = subregion wins",
        y        = "\u0394 Skill Score (system \u2212 subregion)",
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
  # CROSS-MODEL DELTA PLOTS
  # ===========================================================================
  cross_delta <- NULL
  
  if (all(c("system", "subregion") %in% SCALES_TO_RUN)) {
    
    cat("\n📊 Generating cross-model delta plots...\n")
    
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
      
      # -----------------------------------------------------------------------
      # PLOT 1: ggside — faceted figure with y-axis marginal density
      # -----------------------------------------------------------------------
      p_cross <- ggplot(cross_delta,
                        aes(x = window_start_year, y = delta_skill,
                            color = model_key, group = model_key)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray50", linewidth = 0.7) +
        geom_line(linewidth = 0.9, alpha = 0.8) +
        geom_point(size = 2, alpha = 0.9) +
        geom_ysidedensity(
          aes(x = after_stat(density), fill = model_key),
          alpha     = 0.20,
          linewidth = 0.7,
          position  = "identity"
        ) +
        facet_grid(entity ~ metric_label, scales = "free_y") +
        ggside(scales = "free", y.pos = "right") +
        scale_x_continuous(name   = "Forecast Window Start Year",
                           breaks = cross_year_breaks) +
        scale_color_brewer(palette = "Dark2") +
        scale_fill_brewer(palette  = "Dark2") +
        theme_classic(base_size = 12) +
        labs(
          title    = "All Models: Skill Delta per Window (System \u2212 Subregion)",
          subtitle = paste0("Positive = system outperforms subregion | ",
                            "Negative = subregion wins\n",
                            "Right strip = marginal distribution of \u0394 skill"),
          y        = "\u0394 Skill Score (system \u2212 subregion)",
          color    = "Model",
          fill     = "Model"
        ) +
        theme(
          legend.position    = "bottom",
          axis.line          = element_line(linewidth = 1),
          axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
          strip.background   = element_rect(fill = "grey90", color = NA),
          strip.text         = element_text(face = "bold", size = 10),
          plot.title         = element_text(face = "bold", size = 14),
          plot.subtitle      = element_text(size = 11, color = "gray40"),
          ggside.panel.scale = 0.20
        ) +
        theme_ggside_void()  # must be last — hides marginal axis labels/ticks
      
      ggsave(file.path(scale_run_folder, "all_models_window_delta.png"),
             p_cross,
             width  = max(14, length(cross_year_breaks) * 0.7),
             height = max(8,  length(unique(cross_delta$entity)) * 2.5),
             dpi    = 300)
      cat("  ✓ Saved: all_models_window_delta.png\n")
      
      write.csv(cross_delta,
                file.path(scale_run_folder, "all_models_window_delta.csv"),
                row.names = FALSE)
      cat("  ✓ Saved: all_models_window_delta.csv\n")
      
      # -----------------------------------------------------------------------
      # PLOT 2: ggExtra — one PNG per entity x metric with y-axis marginal
      # -----------------------------------------------------------------------
      cat("\n  📈 Generating ggExtra per-panel marginal plots...\n")
      
      marginal_folder <- file.path(scale_run_folder, "window_delta_marginal")
      dir.create(marginal_folder, recursive = TRUE, showWarnings = FALSE)
      
      for (ent in sort(unique(cross_delta$entity))) {
        for (met in sort(unique(cross_delta$metric_label))) {
          
          df_sub <- cross_delta |>
            filter(entity == ent, metric_label == met)
          
          if (nrow(df_sub) == 0) next
          
          p_sub <- ggplot(df_sub,
                          aes(x = window_start_year, y = delta_skill,
                              color = model_key, group = model_key)) +
            geom_hline(yintercept = 0, linetype = "dashed",
                       color = "gray50", linewidth = 0.7) +
            geom_line(linewidth = 0.9, alpha = 0.8) +
            geom_point(size = 2.5, alpha = 0.9) +
            scale_x_continuous(
              name   = "Forecast Window Start Year",
              breaks = sort(unique(df_sub$window_start_year))
            ) +
            scale_color_brewer(palette = "Dark2") +
            theme_classic(base_size = 12) +
            labs(
              title    = glue("Skill Delta: {ent} | {met}"),
              subtitle = "Positive = system wins | Negative = subregion wins",
              y        = "\u0394 Skill Score (system \u2212 subregion)",
              color    = "Model"
            ) +
            theme(
              legend.position = "bottom",
              axis.line       = element_line(linewidth = 1),
              axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
              plot.title      = element_text(face = "bold", size = 13),
              plot.subtitle   = element_text(size = 10, color = "gray40")
            )
          
          p_marginal <- ggMarginal(
            p_sub,
            margins     = "y",
            type        = "density",
            groupColour = TRUE,
            groupFill   = TRUE,
            size        = 4
          )
          
          fname <- glue("{ent}_{met}.png") |>
            stringr::str_replace_all(" ", "_") |>
            stringr::str_to_lower()
          
          ggsave(file.path(marginal_folder, fname),
                 p_marginal,
                 width  = 12,
                 height = 5,
                 dpi    = 300)
          
          cat(glue("    ✓ Saved: window_delta_marginal/{fname}\n"))
        }
      }
      
      cat("  ✓ All ggExtra marginal plots saved to: window_delta_marginal/\n")
      
    } else {
      cross_delta <- NULL
    }
  }
  
  # ===========================================================================
  # CROSS-MODEL DELTA PLOTS: Density, ECDF, Violin
  # ===========================================================================
  if (!is.null(cross_delta) && nrow(cross_delta) > 0) {
    
    p_delta_density <- ggplot(cross_delta,
                              aes(x = delta_skill, fill = model_key, color = model_key)) +
      geom_density(alpha = 0.15, linewidth = 1.1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
      annotate("text", x = -0.05, y = Inf, label = "\u2190 Subregion better",
               hjust = 1, vjust = 1.5, size = 3.5, color = "gray40") +
      annotate("text", x =  0.05, y = Inf, label = "System better \u2192",
               hjust = 0, vjust = 1.5, size = 3.5, color = "gray40") +
      facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
      scale_fill_brewer(palette  = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic(base_size = 13) +
      labs(
        title    = "All Models: Distribution of Skill Delta",
        subtitle = "System \u2212 Subregion | Positive = system wins | Negative = subregion wins",
        x        = "\u0394 Skill Score (system \u2212 subregion)",
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
      annotate("text", x = -0.05, y = 0.5, label = "\u2190 Subregion better",
               hjust = 1, size = 3.5, color = "gray40") +
      annotate("text", x =  0.05, y = 0.5, label = "System better \u2192",
               hjust = 0, size = 3.5, color = "gray40") +
      facet_wrap(~metric_label, ncol = 1, scales = "free_x") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic(base_size = 13) +
      labs(
        title    = "All Models: ECDF of Skill Delta",
        subtitle = "System \u2212 Subregion | Positive = system wins | Negative = subregion wins",
        x        = "\u0394 Skill Score (system \u2212 subregion)",
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
        title    = "All Models: Skill Delta Distribution (System \u2212 Subregion)",
        subtitle = "Positive = system wins | Negative = subregion wins | Line = median",
        x        = NULL,
        y        = "\u0394 Skill Score (system \u2212 subregion)",
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

# ===========================================================================
# PART 4: GENERATE COMBINED FORECAST PDF & CLEANUP
# ===========================================================================
# =============================================================================
#
# Output:
#   <scale_run_folder>/all_models_forecasts.pdf
#   <scale_run_folder>/forecast_pdf_manifest.csv
#   <scale_run_folder>/forecast_cleanup_manifest.csv
#
# Requires: pdftools
#
# Deletes ONLY:
#   - Successfully processed forecast.rds files
#   - Associated train_data.rds and test_data.rds files
#
# Keeps metrics, configurations, summaries, and comparison figures.
# =============================================================================

cat("\nGenerating combined forecast PDF...\n")

# Set FALSE for the first test if you want to inspect the PDF before deletion.
DELETE_RAW_AFTER_VALIDATION <- FALSE

build_combined_forecast_pdf <- function(root, delete_raw = FALSE) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop(
      "Install pdftools before running PDF generation. ",
      "No raw forecast files have been deleted."
    )
  }
  
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  
  files <- sort(list.files(
    root,
    pattern = "^forecast\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  ))
  
  if (!length(files)) {
    stop(
      "No forecast.rds files found under: ", root,
      ". They may already have been deleted by the previous PDF code."
    )
  }
  
  final_pdf <- file.path(root, "all_models_forecasts.pdf")
  temporary_pdf <- tempfile(
    pattern = "all_models_forecasts_build_",
    tmpdir = root,
    fileext = ".pdf"
  )
  
  device_id <- NULL
  
  close_pdf_device <- function() {
    devices <- grDevices::dev.list()
    
    if (!is.null(device_id) &&
        !is.null(devices) &&
        device_id %in% devices) {
      grDevices::dev.off(which = device_id)
    }
    
    device_id <<- NULL
    invisible(NULL)
  }
  
  on.exit(close_pdf_device(), add = TRUE)
  
  # Locate metadata using the windows directory rather than fixed depths.
  parse_forecast_path <- function(filename) {
    path <- normalizePath(filename, winslash = "/", mustWork = TRUE)
    
    relative <- substring(path, nchar(root) + 2L)
    parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
    positions <- which(parts == "windows")
    
    if (length(positions) != 1L) {
      stop("Cannot identify the windows directory in: ", path)
    }
    
    w <- positions[1]
    
    if (w < 2L || length(parts) < w + 3L) {
      stop("Unexpected forecast directory structure: ", path)
    }
    
    framework <- parts[w + 2L]
    
    # mvgam: windows/location_year/mvgam/model/forecast.rds
    # fable: windows/location_year/fable/forecast.rds
    model <- if (length(parts) == w + 4L) {
      parts[w + 3L]
    } else if (
      framework == "fable" &&
      length(parts) == w + 3L
    ) {
      "fable"
    } else {
      stop("Unexpected forecast file layout: ", path)
    }
    
    window_directory <- file.path(
      root,
      paste(parts[seq_len(w + 1L)], collapse = "/")
    )
    
    list(
      relative_file = relative,
      scale = parts[w - 1L],
      window = parts[w + 1L],
      framework = framework,
      model = model,
      window_directory = window_directory,
      run_label = if (w > 2L) {
        paste(parts[seq_len(w - 2L)], collapse = "/")
      } else {
        ""
      }
    )
  }
  
  # Handles either returned ggplot/patchwork objects or direct base graphics.
  draw_mvgam_page <- function(fc, series_index, heading) {
    p <- graphics::plot(fc, series = series_index)
    
    if (inherits(p, "patchwork")) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("patchwork is required to render this returned plot.")
      }
      
      p <- p + patchwork::plot_annotation(title = heading)
      print(p)
      
    } else if (inherits(p, "ggplot")) {
      p <- p +
        ggplot2::labs(title = heading) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 10,
            face = "bold"
          )
        )
      
      print(p)
      
    } else if (is.null(p) || is.list(p)) {
      # Base plotting methods commonly return NULL or an invisible list.
      # The PDF page-count check below catches missing/extra pages.
      graphics::mtext(
        heading,
        side = 3,
        line = 0.5,
        cex = 0.7,
        adj = 0
      )
      
    } else {
      stop(
        "Unrecognised plot return class: ",
        paste(class(p), collapse = ", ")
      )
    }
    
    invisible(NULL)
  }
  
  # Each invocation holds only one raw forecast object.
  process_file <- function(filename) {
    metadata <- parse_forecast_path(filename)
    fc <- readRDS(filename)
    page_rows <- list()
    
    add_page_record <- function(model, series) {
      tibble::tibble(
        source_file = metadata$relative_file,
        run_label = metadata$run_label,
        scale = metadata$scale,
        window = metadata$window,
        framework = metadata$framework,
        model = model,
        series = series
      )
    }
    
    heading_for <- function(model, series) {
      paste0(
        "Scale: ", metadata$scale,
        " | Window: ", metadata$window,
        "\nModel: ", model,
        " | Series: ", series
      )
    }
    
    if (inherits(fc, "mvgam_forecast")) {
      if (!requireNamespace("mvgam", quietly = TRUE)) {
        stop("mvgam is required to plot saved forecasts.")
      }
      
      labels <- as.character(fc$series_names)
      if (!length(labels)) labels <- names(fc$forecasts)
      
      if (!length(labels) || anyNA(labels)) {
        stop("Cannot identify forecast series in: ", filename)
      }
      
      for (s in seq_along(labels)) {
        draw_mvgam_page(
          fc,
          series_index = s,
          heading = heading_for(metadata$model, labels[s])
        )
        
        page_rows[[length(page_rows) + 1L]] <-
          add_page_record(metadata$model, labels[s])
      }
      
    } else if (inherits(fc, "fbl_ts")) {
      if (!requireNamespace("fabletools", quietly = TRUE) ||
          !requireNamespace("tsibble", quietly = TRUE)) {
        stop("fabletools and tsibble are required for fable forecasts.")
      }
      
      train_file <- file.path(
        metadata$window_directory, "train_data.rds"
      )
      
      test_file <- file.path(
        metadata$window_directory, "test_data.rds"
      )
      
      if (!file.exists(train_file) || !file.exists(test_file)) {
        stop("Missing fable window inputs for: ", filename)
      }
      
      train <- tibble::as_tibble(readRDS(train_file))
      test <- tibble::as_tibble(readRDS(test_file))
      
      series_keys <- setdiff(tsibble::key_vars(fc), ".model")
      
      combinations <- tibble::as_tibble(fc) |>
        dplyr::select(tidyselect::all_of(c(".model", series_keys))) |>
        dplyr::distinct()
      
      if (!nrow(combinations)) {
        stop("Fable contains no forecast series: ", filename)
      }
      
      for (j in seq_len(nrow(combinations))) {
        selected <- combinations[j, , drop = FALSE]
        
        keep_fc <- rep(TRUE, nrow(fc))
        keep_train <- rep(TRUE, nrow(train))
        keep_test <- rep(TRUE, nrow(test))
        
        for (key in names(selected)) {
          keep_fc <- keep_fc &
            as.character(fc[[key]]) ==
            as.character(selected[[key]][1])
        }
        
        for (key in series_keys) {
          keep_train <- keep_train &
            as.character(train[[key]]) ==
            as.character(selected[[key]][1])
          
          keep_test <- keep_test &
            as.character(test[[key]]) ==
            as.character(selected[[key]][1])
        }
        
        fc_one <- fc[which(keep_fc), ]
        
        history <- tsibble::as_tsibble(
          train[which(keep_train), ],
          key = tidyselect::all_of(series_keys),
          index = year
        )
        
        observations <- test[which(keep_test), ]
        
        model <- as.character(selected$.model[1])
        
        series <- paste(vapply(
          series_keys,
          function(key) as.character(selected[[key]][1]),
          character(1)
        ), collapse = " / ")
        
        if (!nzchar(series)) series <- "series"
        
        p <- ggplot2::autoplot(
          fc_one,
          history,
          level = c(80, 95)
        ) +
          ggplot2::geom_point(
            data = observations,
            mapping = ggplot2::aes(x = year, y = count),
            inherit.aes = FALSE,
            colour = "black",
            size = 1.8
          ) +
          ggplot2::labs(
            title = heading_for(model, series),
            x = "Year",
            y = "Count"
          ) +
          ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(
            legend.position = "bottom",
            plot.title = ggplot2::element_text(size = 10)
          )
        
        print(p)
        
        page_rows[[length(page_rows) + 1L]] <-
          add_page_record(model, series)
      }
      
    } else {
      stop(
        "Unsupported saved forecast class in ", filename, ": ",
        paste(class(fc), collapse = ", ")
      )
    }
    
    list(
      pages = dplyr::bind_rows(page_rows),
      input_files = c(
        file.path(metadata$window_directory, "train_data.rds"),
        file.path(metadata$window_directory, "test_data.rds")
      )
    )
  }
  
  grDevices::pdf(
    temporary_pdf,
    width = 12,
    height = 8,
    onefile = TRUE,
    title = "All Models: Hindcasts and Forecasts"
  )
  
  device_id <- grDevices::dev.cur()
  
  page_records <- vector("list", length(files))
  input_files <- character()
  
  for (i in seq_along(files)) {
    cat(sprintf(
      "  [%d/%d] %s\n",
      i, length(files),
      substring(files[i], nchar(root) + 2L)
    ))
    
    # Any error stops the build. No sources are deleted.
    output <- process_file(files[i])
    
    page_records[[i]] <- output$pages
    input_files <- unique(c(input_files, output$input_files))
    
    rm(output)
    gc(verbose = FALSE)
  }
  
  close_pdf_device()
  
  manifest <- dplyr::bind_rows(page_records)
  expected_pages <- nrow(manifest)
  
  info <- pdftools::pdf_info(temporary_pdf)
  actual_pages <- info$pages
  
  if (expected_pages < 1L || actual_pages != expected_pages) {
    stop(
      "PDF verification failed: expected ", expected_pages,
      " pages, found ", actual_pages,
      ". Raw files retained. Build file: ", temporary_pdf
    )
  }
  
  # Verify that each page has an extractable label.
  # This checks labels/page presence, not scientific correctness of the plots.
  page_text <- pdftools::pdf_text(temporary_pdf)
  
  if (length(page_text) != expected_pages ||
      any(!grepl("Scale:", page_text, fixed = TRUE))) {
    stop(
      "PDF page-label verification failed. Raw files retained. ",
      "Build file: ", temporary_pdf
    )
  }
  
  # Do not silently overwrite an existing combined report.
  if (file.exists(final_pdf)) {
    stop(
      "A combined PDF already exists: ", final_pdf,
      ". Rename it before rebuilding. New validated build: ",
      temporary_pdf
    )
  }
  
  if (!file.rename(temporary_pdf, final_pdf)) {
    stop(
      "Could not publish the combined PDF. ",
      "Raw files retained. Build file: ", temporary_pdf
    )
  }
  
  manifest$page <- seq_len(expected_pages)
  
  utils::write.csv(
    manifest,
    file.path(root, "forecast_pdf_manifest.csv"),
    row.names = FALSE
  )
  
  cat(
    "\nVerified combined PDF:", final_pdf,
    "\nPages:", actual_pages, "\n"
  )
  
  if (isTRUE(delete_raw)) {
    # Delete only the sources processed above and their window inputs.
    candidates <- unique(c(files, input_files))
    candidates <- candidates[file.exists(candidates)]
    
    cleanup <- data.frame(
      file = substring(candidates, nchar(root) + 2L),
      bytes = file.info(candidates)$size,
      deleted = FALSE,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(candidates)) {
      cleanup$deleted[i] <- file.remove(candidates[i])
    }
    
    utils::write.csv(
      cleanup,
      file.path(root, "forecast_cleanup_manifest.csv"),
      row.names = FALSE
    )
    
    if (any(!cleanup$deleted)) {
      warning(
        "Some source files could not be deleted. ",
        "See forecast_cleanup_manifest.csv.",
        call. = FALSE
      )
    }
    
    cat(
      "Deleted", sum(cleanup$deleted), "source files;",
      "reclaimed approximately",
      round(sum(cleanup$bytes[cleanup$deleted], na.rm = TRUE) / 1024^3, 2),
      "GiB.\n"
    )
  } else {
    cat("Raw files retained for visual inspection.\n")
  }
  
  invisible(manifest)
}

forecast_pdf_manifest <- build_combined_forecast_pdf(
  root = scale_run_folder
  # delete_raw = DELETE_RAW_AFTER_VALIDATION
)


# =============================================================================
# FINAL SUMMARY
# =============================================================================
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("✅ MODEL-BY-MODEL SPATIAL SCALE COMPARISON COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
cat("📂 All results saved to:", scale_run_folder, "\n\n")
cat("📁 Folder Structure:\n")
cat("  • base_config.rds                   - Base configuration\n")
cat("  • window_skill_all_scales.csv       - Per-window skill scores (all models/scales)\n")
cat("  • all_models_window_delta.png       - Cross-model delta plot with ggside marginal\n")
cat("  • all_models_window_delta.csv       - Cross-model delta table\n")
cat("  • all_models_summary.csv            - Aggregated summary across windows\n")
cat("  • window_delta_marginal/            - ggExtra per-panel marginal plots\n")
cat("  • all_models_forecasts.pdf          - Single PDF containing all forecasting plots\n\n")

for (model_key in names(all_model_results)) {
  cat(glue("  • {model_key}/\n"))
  cat(glue("    \u251c\u2500\u2500 window_skill_by_scale.png  - Skill per window by scale\n"))
  cat(glue("    \u251c\u2500\u2500 window_delta_skill.png     - Delta per window\n"))
  cat(glue("    \u251c\u2500\u2500 window_delta_skill.csv     - Delta table\n"))
  for (scale in SCALES_TO_RUN) cat(glue("    \u251c\u2500\u2500 {scale}/\n"))
}
# =============================================================================
# ZIP results folder if on HiPerGator blue
# =============================================================================
on_hipergator <- dir.exists("/blue/ewhite")
if (on_hipergator) {
  zip_path <- paste0(scale_run_folder, ".zip")
  cat("\n📦 Zipping results folder...\n")
  zip(zipfile = zip_path, files = scale_run_folder)
  cat(glue("  ✓ Zipped: {zip_path}\n"))
  unlink(scale_run_folder, recursive = TRUE)
  cat(glue("  ✓ Removed original folder: {scale_run_folder}\n"))
}