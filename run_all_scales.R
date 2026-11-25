# =============================================================================
# RUN_ALL_SCALES.R
# Run each model separately across all spatial scales, compare skill deltas.
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
library(fs)

conflicted::conflicts_prefer(dplyr::filter,   .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select,   .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::AR,       .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::VAR,      .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::RW,       .quiet = TRUE)

source("data_functions.R")
source("evaluation.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

SPECIES_TO_RUN   <- c("top6")
FORECAST_TOTALS  <- FALSE
SCALES_TO_RUN    <- c("system", "subregion")
FABLE_MODELS     <- c()
MVGAM_MODELS     <- c("ar", "ar_exog")
PARALLEL         <- FALSE
PARALLEL_WORKERS <- 3
FORECAST_HORIZON <- 2
CV_WINDOWS       <- NULL

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

# Load base config early so remote_url is available for water precompute.
base_config <- config::get(config = "run_all_scales")

cat("  Pre-caching water covariate data...\n")
tryCatch({
  get_data_water(remote_url = base_config$water$remote_url %||% NULL)
  cat("  ✓ Water covariate data cached\n")
}, error = function(e) warning("Water data pre-cache failed: ", e$message))

cat("  Pre-caching wading bird data...\n")
tryCatch({
  tmp_cfg                          <- base_config
  tmp_cfg$spatial$include_species  <- SPECIES_TO_RUN
  tmp_cfg$spatial$forecast_totals  <- FORECAST_TOTALS
  for (sc in SCALES_TO_RUN) {
    tmp_cfg$spatial$level <- sc
    get_wading_bird_data(config = tmp_cfg, cache = TRUE)
    cat(glue("  ✓ Wading bird data cached for scale: {sc}\n"))
  }
}, error = function(e) warning("Wading bird data pre-cache failed: ", e$message))

dir.create("cache", showWarnings = FALSE, recursive = TRUE)
cat("✓ All shared artifacts precomputed\n\n")

# =============================================================================
# PARALLEL UTILITIES
# =============================================================================

if (PARALLEL) source("parallel_utils.R")

# =============================================================================
# HELPERS
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

extract_window_start_year <- function(window_vec, test_start_vec = NULL) {
  if (!is.null(test_start_vec)) {
    yr <- suppressWarnings(
      as.integer(stringr::str_extract(as.character(test_start_vec), "\\d{4}"))
    )
    if (any(!is.na(yr))) return(yr)
  }
  yr <- suppressWarnings(
    as.integer(stringr::str_extract(as.character(window_vec), "\\d{4}"))
  )
  if (any(!is.na(yr))) return(yr)
  suppressWarnings(as.integer(as.character(window_vec)))
}

# =============================================================================
# OUTPUT DIRECTORY
# =============================================================================

timestamp        <- format(Sys.time(), "%Y%m%d-%H%M")
all_models_str   <- paste(c(MVGAM_MODELS, FABLE_MODELS), collapse = "-")
all_scales_str   <- paste(SCALES_TO_RUN,  collapse = "-")
species_str      <- paste(SPECIES_TO_RUN, collapse = "-")

scale_run_folder <- file.path(
  "results",
  paste0(species_str, "_scale_run_", all_models_str, "_",
         all_scales_str, "_", timestamp)
)
dir.create(scale_run_folder, recursive = TRUE, showWarnings = FALSE)

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("MODEL-BY-MODEL SPATIAL SCALE COMPARISON\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Results folder:", scale_run_folder, "\n\n")

# =============================================================================
# APPLY USER SETTINGS TO BASE CONFIG
# =============================================================================

base_config$spatial$include_species      <- SPECIES_TO_RUN
base_config$spatial$forecast_totals      <- FORECAST_TOTALS
base_config$models$mvgam                 <- MVGAM_MODELS
base_config$models$fable                 <- FABLE_MODELS
base_config$run_mvgam                    <- length(MVGAM_MODELS) > 0L
base_config$run_fable                    <- length(FABLE_MODELS) > 0L
base_config$forecast_horizon             <- FORECAST_HORIZON
base_config$cv_windows                   <- CV_WINDOWS
base_config$plots$generate_forecast_pdfs <- FALSE

saveRDS(base_config, file.path(scale_run_folder, "base_config.rds"))

cat("Configuration:\n")
cat("  Species        :", paste(SPECIES_TO_RUN, collapse = ", "), "\n")
cat("  Forecast totals:", FORECAST_TOTALS, "\n")
cat("  Scales         :", paste(SCALES_TO_RUN,  collapse = ", "), "\n")
cat("  mvgam models   :", if (length(MVGAM_MODELS) > 0L) paste(MVGAM_MODELS, collapse = ", ") else "none", "\n")
cat("  fable models   :", if (length(FABLE_MODELS) > 0L) paste(FABLE_MODELS, collapse = ", ") else "none", "\n")
cat("  Parallel       :", PARALLEL, "\n")
if (PARALLEL) cat("  Workers        :", PARALLEL_WORKERS, "\n")
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

if (!length(models_to_test)) {
  stop("No models specified. Add models to MVGAM_MODELS or FABLE_MODELS.")
}

cat("Models to test:", length(models_to_test), "\n")
for (mk in names(models_to_test)) cat("  •", mk, "\n")
cat("\n")

# =============================================================================
# PART 1: RUN EACH MODEL ACROSS ALL SCALES
# =============================================================================

run_single_model <- function(model_key) {
  
  model_info <- models_to_test[[model_key]]
  framework  <- model_info$framework
  model_name <- model_info$model
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat(glue("TESTING MODEL: {model_key}\n"))
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  model_folder <- file.path(scale_run_folder, model_key)
  dir.create(model_folder, recursive = TRUE, showWarnings = FALSE)
  
  model_scale_results <- list()
  
  for (current_scale in SCALES_TO_RUN) {
    
    cat("\n", paste(rep("-", 60), collapse = ""), "\n", sep = "")
    cat(glue("  Scale: {toupper(current_scale)}\n"))
    cat(paste(rep("-", 60), collapse = ""), "\n\n")
    
    # Build a fresh config for this model × scale combination.
    cfg                       <- base_config
    cfg$spatial$level         <- current_scale
    cfg$spatial$run_by_region <- current_scale != "system"
    cfg$parallel$enabled      <- FALSE
    cfg$model_key             <- model_key
    
    if (framework == "mvgam") {
      # Always include baseline so skill scores are computed.
      cfg$models$mvgam <- c("baseline", model_name)
      cfg$models$fable <- character()
      cfg$run_mvgam    <- TRUE
      cfg$run_fable    <- FALSE
    } else {
      cfg$models$fable <- c("baseline", model_name)
      cfg$models$mvgam <- character()
      cfg$run_mvgam    <- FALSE
      cfg$run_fable    <- TRUE
    }
    
    # Publish CONFIG so run_forecasting_pipeline() picks it up without
    # calling config::get() again.
    assign("CONFIG", cfg, envir = .GlobalEnv)
    
    run_succeeded <- FALSE
    run_folder    <- NULL
    
    tryCatch({
      # run_forecasting_pipeline() sources model files and evaluation.R
      # internally, then publishes run_folder to .GlobalEnv.
      run_forecasting_pipeline()
      
      run_folder <- tryCatch(
        base::get("run_folder", envir = .GlobalEnv, inherits = FALSE),
        error = function(e) NULL
      )
      
      if (is.null(run_folder)) {
        stop("run_forecasting_pipeline() completed but run_folder was not set.")
      }
      if (!dir.exists(run_folder)) {
        stop(glue("run_folder does not exist: {run_folder}"))
      }
      
      rds_path <- file.path(run_folder, "forecast_results.rds")
      if (!file.exists(rds_path)) {
        stop(glue("forecast_results.rds missing from: {run_folder}"))
      }
      
      run_succeeded <- TRUE
      
    }, error = function(e) {
      cat(glue("\n  ✗ {model_key} at {current_scale} FAILED\n"))
      cat(glue("    Reason: {e$message}\n"))
    })
    
    if (run_succeeded && !is.null(run_folder)) {
      
      dest_folder   <- file.path(model_folder, current_scale)
      files_to_copy <- list.files(run_folder, full.names = TRUE, recursive = TRUE)
      
      if (!length(files_to_copy)) {
        cat(glue("  ⚠  run_folder is empty: {run_folder}\n"))
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
      writeLines(c(
        glue("Model  : {model_key}"),
        glue("Scale  : {current_scale}"),
        glue("Time   : {format(Sys.time())}"),
        glue("Folder : {run_folder %||% 'not set'}"),
        "",
        "Check that run_forecasting_pipeline() completes without error."
      ), fail_log)
      
      cat(glue("  ✗ Failure logged to: {fail_log}\n"))
      model_scale_results[[current_scale]] <- NULL
    }
    
    gc(verbose = FALSE)
  }
  
  cat(glue("\n✓ {model_key} complete\n"))
  succeeded <- names(Filter(Negate(is.null), model_scale_results))
  failed    <- setdiff(SCALES_TO_RUN, succeeded)
  if (length(succeeded)) cat(glue("  ✓ Succeeded: {paste(succeeded, collapse = ', ')}\n"))
  if (length(failed))    cat(glue("  ✗ Failed   : {paste(failed,    collapse = ', ')}\n"))
  
  model_scale_results
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
  all_model_results           <- lapply(names(models_to_test), run_single_model)
  names(all_model_results)    <- names(models_to_test)
}

cat("\n✓ ALL MODEL RUNS COMPLETE\n\n")

# =============================================================================
# PART 2: EXTRACT PER-WINDOW SKILL AND COMPUTE SCALE DELTAS
# =============================================================================

cat("Extracting per-window skill scores...\n\n")

extract_model_metrics <- function(folder_path, scale_name, framework) {
  file_path <- file.path(folder_path, "forecast_results.rds")
  if (!file.exists(file_path)) return(NULL)
  
  res <- readRDS(file_path)
  
  if (framework == "mvgam") {
    if (is.null(res$mvgam) || is.null(res$mvgam$metrics)) return(NULL)
    metrics <- res$mvgam$metrics
  } else {
    if (is.null(res$fable) || is.null(res$fable$metrics)) return(NULL)
    # Fable uses .model; rename to match mvgam convention.
    metrics <- dplyr::rename(res$fable$metrics, model = .model)
  }
  
  if (!"model" %in% names(metrics)) {
    warning(glue(
      "No 'model' column in metrics for scale={scale_name}. ",
      "All CV windows likely failed. Skipping."
    ))
    return(NULL)
  }
  
  if (!"window" %in% names(metrics)) {
    warning(glue("No 'window' column in metrics for scale={scale_name}. Skipping."))
    return(NULL)
  }
  
  metrics |>
    tibble::as_tibble() |>
    dplyr::select(
      dplyr::any_of(c(
        "model", "species", "region",
        "window", "test_start",
        "crps_skill", "rps_skill", "rmse_skill"
      ))
    ) |>
    dplyr::mutate(framework = framework, scale = scale_name)
}

all_window_metrics <- dplyr::bind_rows(lapply(names(all_model_results), function(model_key) {
  model_info    <- models_to_test[[model_key]]
  framework     <- model_info$framework
  model_name    <- model_info$model
  model_folders <- all_model_results[[model_key]]
  
  dplyr::bind_rows(lapply(names(model_folders), function(scale) {
    folder <- model_folders[[scale]]
    if (is.null(folder)) return(NULL)
    m <- extract_model_metrics(folder, scale, framework)
    if (is.null(m)) return(NULL)
    dplyr::filter(m, model == model_name) |>
      dplyr::mutate(model_key = model_key)
  }))
}))

if (is.null(all_window_metrics) || !nrow(all_window_metrics)) {
  warning("No window-level metrics found. Skipping delta computation.")
} else {
  
  skill_metrics  <- intersect(
    c("crps_skill", "rps_skill", "rmse_skill"),
    names(all_window_metrics)
  )
  has_test_start <- "test_start" %in% names(all_window_metrics)
  
  window_long <- all_window_metrics |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(skill_metrics),
      names_to  = "metric",
      values_to = "skill_score"
    ) |>
    dplyr::filter(!is.na(skill_score)) |>
    dplyr::mutate(
      skill_score  = pmax(skill_score, -1),
      metric_label = dplyr::case_when(
        metric == "crps_skill" ~ "CRPS Skill",
        metric == "rps_skill"  ~ "RPS Skill",
        metric == "rmse_skill" ~ "RMSE Skill",
        TRUE ~ metric
      ),
      window_start_year = extract_window_start_year(
        window,
        if (has_test_start) test_start else NULL
      )
    )
  
  utils::write.csv(
    window_long,
    file.path(scale_run_folder, "window_skill_all_scales.csv"),
    row.names = FALSE
  )
  cat("  ✓ Saved: window_skill_all_scales.csv\n")
  
  all_scale_colors <- c(colony = "#00B050", subregion = "#FF0000", system = "#0000FF")
  scale_colors     <- all_scale_colors[SCALES_TO_RUN]
  
  # ===========================================================================
  # PER-MODEL: window skill plot + delta plot
  # ===========================================================================
  
  for (mk in names(all_model_results)) {
    
    model_folder <- file.path(scale_run_folder, mk)
    cat(glue("\nGenerating window plots for {mk}...\n"))
    
    model_long <- window_long |>
      dplyr::filter(model_key == mk) |>
      dplyr::mutate(scale = factor(scale, levels = SCALES_TO_RUN))
    
    if (!nrow(model_long)) {
      cat(glue("  ⚠  No data for {mk}, skipping.\n"))
      next
    }
    
    model_long <- dplyr::mutate(
      model_long,
      entity = if (!FORECAST_TOTALS && "species" %in% names(model_long)) {
        species
      } else {
        "Total"
      }
    )
    
    year_breaks <- sort(unique(model_long$window_start_year))
    
    p_window <- ggplot2::ggplot(
      model_long,
      ggplot2::aes(
        x = window_start_year, y = skill_score,
        color = scale, group = scale
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed",
        color = "gray50", linewidth = 0.7
      ) +
      ggplot2::geom_line(linewidth = 0.9, alpha = 0.8) +
      ggplot2::geom_point(size = 2.5, alpha = 0.9) +
      ggplot2::facet_grid(entity ~ metric_label, scales = "free_y") +
      ggplot2::scale_color_manual(values = scale_colors) +
      ggplot2::scale_x_continuous(
        name = "Forecast Window Start Year", breaks = year_breaks
      ) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::labs(
        title    = glue("{mk}: Skill Score per Sliding Window"),
        subtitle = "Each point = one CV window | coloured by spatial scale",
        y        = "Skill Score (vs Baseline)",
        color    = "Scale"
      ) +
      ggplot2::theme(
        legend.position  = "bottom",
        axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
        strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
        strip.text       = ggplot2::element_text(face = "bold", size = 10),
        plot.title       = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle    = ggplot2::element_text(size = 11, color = "gray40")
      )
    
    ggplot2::ggsave(
      file.path(model_folder, "window_skill_by_scale.png"),
      p_window,
      width  = max(10, length(year_breaks) * 0.7),
      height = max(6,  dplyr::n_distinct(model_long$entity) * 2.5),
      dpi    = 300
    )
    cat(glue("  ✓ Saved: {mk}/window_skill_by_scale.png\n"))
    
    # -------------------------------------------------------------------------
    # DELTA: system - subregion per window
    # -------------------------------------------------------------------------
    
    available_scales <- as.character(unique(model_long$scale))
    if (!all(c("system", "subregion") %in% available_scales)) {
      cat(glue("  ⚠  Both system and subregion needed for delta — skipping {mk}.\n"))
      next
    }
    
    delta_id_cols <- intersect(
      c("model_key",
        if (!FORECAST_TOTALS) "species",
        "entity", "window", "window_start_year", "test_start",
        "metric", "metric_label"),
      names(model_long)
    )
    
    delta_df <- model_long |>
      dplyr::filter(scale %in% c("system", "subregion")) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(delta_id_cols, "scale")))) |>
      dplyr::summarise(skill_score = mean(skill_score, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(delta_id_cols),
        names_from  = scale,
        values_from = skill_score,
        values_fn   = mean
      ) |>
      dplyr::filter(!is.na(system), !is.na(subregion)) |>
      dplyr::mutate(delta_skill = system - subregion)
    
    if (!nrow(delta_df)) {
      cat(glue("  ⚠  Delta table empty for {mk} — skipping.\n"))
      next
    }
    
    year_breaks_delta <- sort(unique(delta_df$window_start_year))
    
    p_delta <- ggplot2::ggplot(
      delta_df,
      ggplot2::aes(
        x = window_start_year, y = delta_skill,
        color = metric_label, group = metric_label
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed",
        color = "gray50", linewidth = 0.7
      ) +
      ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::facet_wrap(~entity, ncol = 2, scales = "free_y") +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::scale_x_continuous(
        name = "Forecast Window Start Year", breaks = year_breaks_delta
      ) +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::labs(
        title    = glue("{mk}: Skill Delta per Window (System \u2212 Subregion)"),
        subtitle = "Positive = system wins | Negative = subregion wins",
        y        = "\u0394 Skill Score (system \u2212 subregion)",
        color    = "Metric"
      ) +
      ggplot2::theme(
        legend.position  = "bottom",
        axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
        strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
        strip.text       = ggplot2::element_text(face = "bold", size = 10),
        plot.title       = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle    = ggplot2::element_text(size = 11, color = "gray40")
      )
    
    ggplot2::ggsave(
      file.path(model_folder, "window_delta_skill.png"),
      p_delta,
      width  = max(10, length(year_breaks_delta) * 0.7),
      height = max(6,  dplyr::n_distinct(delta_df$entity) * 2.5),
      dpi    = 300
    )
    cat(glue("  ✓ Saved: {mk}/window_delta_skill.png\n"))
    
    utils::write.csv(
      delta_df,
      file.path(model_folder, "window_delta_skill.csv"),
      row.names = FALSE
    )
    cat(glue("  ✓ Saved: {mk}/window_delta_skill.csv\n"))
  }
  
  # ===========================================================================
  # CROSS-MODEL DELTA PLOTS
  # ===========================================================================
  
  cross_delta <- NULL
  
  if (all(c("system", "subregion") %in% SCALES_TO_RUN)) {
    
    cat("\nGenerating cross-model delta plots...\n")
    
    cross_id_cols <- intersect(
      c("model_key",
        if (!FORECAST_TOTALS) "species",
        "window", "window_start_year", "test_start",
        "metric", "metric_label"),
      names(window_long)
    )
    
    cross_delta <- window_long |>
      dplyr::filter(scale %in% c("system", "subregion")) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(cross_id_cols, "scale")))) |>
      dplyr::summarise(skill_score = mean(skill_score, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(cross_id_cols),
        names_from  = scale,
        values_from = skill_score,
        values_fn   = mean
      ) |>
      dplyr::filter(!is.na(system), !is.na(subregion)) |>
      dplyr::mutate(
        delta_skill = system - subregion,
        entity      = if (!FORECAST_TOTALS && "species" %in% names(cross_delta %||% .)) {
          species
        } else {
          "Total"
        }
      )
    
    # Safer entity assignment after the pipe
    if (!"entity" %in% names(cross_delta)) {
      cross_delta$entity <- if (!FORECAST_TOTALS && "species" %in% names(cross_delta)) {
        cross_delta$species
      } else {
        "Total"
      }
    }
    
    if (nrow(cross_delta) > 0L) {
      
      cross_year_breaks <- sort(unique(cross_delta$window_start_year))
      
      # ggside version — faceted with marginal density
      p_cross <- ggplot2::ggplot(
        cross_delta,
        ggplot2::aes(
          x = window_start_year, y = delta_skill,
          color = model_key, group = model_key
        )
      ) +
        ggplot2::geom_hline(
          yintercept = 0, linetype = "dashed",
          color = "gray50", linewidth = 0.7
        ) +
        ggplot2::geom_line(linewidth = 0.9, alpha = 0.8) +
        ggplot2::geom_point(size = 2, alpha = 0.9) +
        ggside::geom_ysidedensity(
          ggplot2::aes(x = ggplot2::after_stat(density), fill = model_key),
          alpha     = 0.20,
          linewidth = 0.7,
          position  = "identity"
        ) +
        ggplot2::facet_grid(entity ~ metric_label, scales = "free_y") +
        ggside::ggside(scales = "free", y.pos = "right") +
        ggplot2::scale_x_continuous(
          name = "Forecast Window Start Year", breaks = cross_year_breaks
        ) +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::scale_fill_brewer(palette  = "Dark2") +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::labs(
          title    = "All Models: Skill Delta (System \u2212 Subregion)",
          subtitle = paste0(
            "Positive = system wins | Negative = subregion wins\n",
            "Right strip = marginal distribution of \u0394 skill"
          ),
          y     = "\u0394 Skill Score",
          color = "Model",
          fill  = "Model"
        ) +
        ggplot2::theme(
          legend.position    = "bottom",
          axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
          strip.background   = ggplot2::element_rect(fill = "grey90", color = NA),
          strip.text         = ggplot2::element_text(face = "bold", size = 10),
          plot.title         = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle      = ggplot2::element_text(size = 11, color = "gray40"),
          ggside.panel.scale = 0.20
        ) +
        ggside::theme_ggside_void()
      
      ggplot2::ggsave(
        file.path(scale_run_folder, "all_models_window_delta.png"),
        p_cross,
        width  = max(14, length(cross_year_breaks) * 0.7),
        height = max(8,  dplyr::n_distinct(cross_delta$entity) * 2.5),
        dpi    = 300
      )
      cat("  ✓ Saved: all_models_window_delta.png\n")
      
      utils::write.csv(
        cross_delta,
        file.path(scale_run_folder, "all_models_window_delta.csv"),
        row.names = FALSE
      )
      cat("  ✓ Saved: all_models_window_delta.csv\n")
      
      # ggExtra per-panel marginal plots
      cat("\n  Generating ggExtra per-panel marginal plots...\n")
      
      marginal_folder <- file.path(scale_run_folder, "window_delta_marginal")
      dir.create(marginal_folder, recursive = TRUE, showWarnings = FALSE)
      
      for (ent in sort(unique(cross_delta$entity))) {
        for (met in sort(unique(cross_delta$metric_label))) {
          
          df_sub <- dplyr::filter(
            cross_delta, entity == ent, metric_label == met
          )
          if (!nrow(df_sub)) next
          
          p_sub <- ggplot2::ggplot(
            df_sub,
            ggplot2::aes(
              x = window_start_year, y = delta_skill,
              color = model_key, group = model_key
            )
          ) +
            ggplot2::geom_hline(
              yintercept = 0, linetype = "dashed",
              color = "gray50", linewidth = 0.7
            ) +
            ggplot2::geom_line(linewidth = 0.9, alpha = 0.8) +
            ggplot2::geom_point(size = 2.5, alpha = 0.9) +
            ggplot2::scale_x_continuous(
              name   = "Forecast Window Start Year",
              breaks = sort(unique(df_sub$window_start_year))
            ) +
            ggplot2::scale_color_brewer(palette = "Dark2") +
            ggplot2::theme_classic(base_size = 12) +
            ggplot2::labs(
              title    = glue("Skill Delta: {ent} | {met}"),
              subtitle = "Positive = system wins | Negative = subregion wins",
              y        = "\u0394 Skill Score",
              color    = "Model"
            ) +
            ggplot2::theme(
              legend.position = "bottom",
              axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
              plot.title      = ggplot2::element_text(face = "bold", size = 13),
              plot.subtitle   = ggplot2::element_text(size = 10, color = "gray40")
            )
          
          p_marginal <- ggExtra::ggMarginal(
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
          
          ggplot2::ggsave(
            file.path(marginal_folder, fname),
            p_marginal,
            width  = 12,
            height = 5,
            dpi    = 300
          )
          cat(glue("    ✓ Saved: window_delta_marginal/{fname}\n"))
        }
      }
      
      cat("  ✓ All ggExtra marginal plots saved\n")
      
      # Density / ECDF / Violin
      p_density <- ggplot2::ggplot(
        cross_delta,
        ggplot2::aes(x = delta_skill, fill = model_key, color = model_key)
      ) +
        ggplot2::geom_density(alpha = 0.15, linewidth = 1.1) +
        ggplot2::geom_vline(
          xintercept = 0, linetype = "dashed",
          color = "gray40", linewidth = 0.7
        ) +
        ggplot2::facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
        ggplot2::scale_fill_brewer(palette  = "Dark2") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_classic(base_size = 13) +
        ggplot2::labs(
          title    = "All Models: Distribution of Skill Delta",
          subtitle = "System \u2212 Subregion",
          x        = "\u0394 Skill Score",
          y        = "Density",
          fill     = "Model", color = "Model"
        ) +
        ggplot2::theme(
          legend.position  = "bottom",
          strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
          strip.text       = ggplot2::element_text(face = "bold", size = 11),
          plot.title       = ggplot2::element_text(face = "bold", size = 14)
        )
      
      ggplot2::ggsave(
        file.path(scale_run_folder, "all_models_delta_density.png"),
        p_density, width = 10, height = 10, dpi = 300
      )
      cat("  ✓ Saved: all_models_delta_density.png\n")
      
      p_ecdf <- ggplot2::ggplot(
        cross_delta,
        ggplot2::aes(x = delta_skill, color = model_key)
      ) +
        ggplot2::stat_ecdf(linewidth = 1.2) +
        ggplot2::geom_vline(
          xintercept = 0, linetype = "dashed",
          color = "gray40", linewidth = 0.7
        ) +
        ggplot2::facet_wrap(~metric_label, ncol = 1, scales = "free_x") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_classic(base_size = 13) +
        ggplot2::labs(
          title    = "All Models: ECDF of Skill Delta",
          subtitle = "System \u2212 Subregion",
          x        = "\u0394 Skill Score",
          y        = "Cumulative Probability",
          color    = "Model"
        ) +
        ggplot2::theme(
          legend.position  = "bottom",
          strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
          strip.text       = ggplot2::element_text(face = "bold", size = 11),
          plot.title       = ggplot2::element_text(face = "bold", size = 14)
        )
      
      ggplot2::ggsave(
        file.path(scale_run_folder, "all_models_delta_ecdf.png"),
        p_ecdf, width = 10, height = 10, dpi = 300
      )
      cat("  ✓ Saved: all_models_delta_ecdf.png\n")
      
      p_violin <- ggplot2::ggplot(
        cross_delta,
        ggplot2::aes(
          x = model_key, y = delta_skill,
          fill = model_key, color = model_key
        )
      ) +
        ggplot2::geom_hline(
          yintercept = 0, linetype = "dashed",
          color = "gray40", linewidth = 0.7
        ) +
        ggplot2::geom_violin(alpha = 0.3, linewidth = 0.9, trim = FALSE) +
        ggplot2::geom_boxplot(
          width = 0.08, alpha = 0.8,
          outlier.shape = NA, color = "gray20"
        ) +
        ggplot2::facet_wrap(~metric_label, ncol = 1, scales = "free_y") +
        ggplot2::scale_fill_brewer(palette  = "Dark2") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_classic(base_size = 13) +
        ggplot2::labs(
          title    = "All Models: Skill Delta Distribution",
          subtitle = "System \u2212 Subregion | Positive = system wins",
          x        = NULL,
          y        = "\u0394 Skill Score",
          fill     = "Model", color = "Model"
        ) +
        ggplot2::theme(
          legend.position  = "bottom",
          axis.text.x      = ggplot2::element_text(
            angle = 45, hjust = 1, face = "bold", size = 10
          ),
          strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
          strip.text       = ggplot2::element_text(face = "bold", size = 11),
          plot.title       = ggplot2::element_text(face = "bold", size = 14)
        )
      
      ggplot2::ggsave(
        file.path(scale_run_folder, "all_models_delta_violin.png"),
        p_violin, width = 12, height = 10, dpi = 300
      )
      cat("  ✓ Saved: all_models_delta_violin.png\n")
      
    } else {
      cross_delta <- NULL
    }
  }
  
  # ===========================================================================
  # AGGREGATED CROSS-MODEL SUMMARY
  # ===========================================================================
  
  cat("\nGenerating aggregated cross-model summary...\n")
  
  overall_summary <- window_long |>
    dplyr::group_by(model_key, scale, metric) |>
    dplyr::summarise(
      mean_skill   = round(mean(skill_score,   na.rm = TRUE), 3),
      median_skill = round(median(skill_score, na.rm = TRUE), 3),
      n            = dplyr::n(),
      .groups      = "drop"
    )
  
  utils::write.csv(
    overall_summary,
    file.path(scale_run_folder, "all_models_summary.csv"),
    row.names = FALSE
  )
  cat("  ✓ Saved: all_models_summary.csv\n")
}

# =============================================================================
# PART 3: COMBINED FORECAST PDF
# =============================================================================

cat("\nGenerating combined forecast PDF...\n")

DELETE_RAW_AFTER_VALIDATION <- FALSE

build_combined_forecast_pdf <- function(root, delete_raw = FALSE) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Install pdftools before running PDF generation.")
  }
  
  root  <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- sort(list.files(
    root,
    pattern    = "^forecast\\.rds$",
    recursive  = TRUE,
    full.names = TRUE
  ))
  
  if (!length(files)) {
    stop("No forecast.rds files found under: ", root)
  }
  
  final_pdf     <- file.path(root, "all_models_forecasts.pdf")
  temporary_pdf <- tempfile(
    pattern = "all_models_forecasts_build_",
    tmpdir  = root,
    fileext = ".pdf"
  )
  
  device_id <- NULL
  
  close_pdf_device <- function() {
    devices <- grDevices::dev.list()
    if (!is.null(device_id) && !is.null(devices) && device_id %in% devices) {
      grDevices::dev.off(which = device_id)
    }
    device_id <<- NULL
    invisible(NULL)
  }
  
  on.exit(close_pdf_device(), add = TRUE)
  
  # Parse a forecast.rds path into metadata.
  # Structure (after copying):
  #   <root>/<model_key>/<scale>/windows/<location_year>/<framework>/[<model>/]forecast.rds
  parse_forecast_path <- function(filename) {
    path     <- normalizePath(filename, winslash = "/", mustWork = TRUE)
    relative <- substring(path, nchar(root) + 2L)
    parts    <- strsplit(relative, "/", fixed = TRUE)[[1]]
    
    positions <- which(parts == "windows")
    if (length(positions) != 1L) stop("Cannot find 'windows' in path: ", path)
    
    w <- positions[1L]
    
    # Parts before 'windows': model_key, scale
    if (w < 3L) stop("Unexpected path structure before 'windows': ", path)
    
    model_key <- parts[w - 2L]
    scale     <- parts[w - 1L]
    window    <- parts[w + 1L]
    framework <- parts[w + 2L]
    
    # mvgam: .../framework/model/forecast.rds  (5 parts after w)
    # fable: .../framework/forecast.rds         (4 parts after w — no model subdir)
    model <- if (length(parts) >= w + 4L) parts[w + 3L] else framework
    
    window_directory <- file.path(
      root, paste(parts[seq_len(w + 1L)], collapse = "/")
    )
    
    list(
      relative_file    = relative,
      model_key        = model_key,
      scale            = scale,
      window           = window,
      framework        = framework,
      model            = model,
      window_directory = window_directory
    )
  }
  
  draw_mvgam_page <- function(fc, series_index, heading) {
    p <- graphics::plot(fc, series = series_index)
    
    if (inherits(p, c("patchwork", "gg"))) {
      if (inherits(p, "patchwork")) {
        p <- p + patchwork::plot_annotation(title = heading)
      } else {
        p <- p + ggplot2::labs(title = heading) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 10, face = "bold")
          )
      }
      print(p)
    } else {
      graphics::mtext(heading, side = 3, line = 0.5, cex = 0.7, adj = 0)
    }
    invisible(NULL)
  }
  
  process_file <- function(filename) {
    metadata   <- parse_forecast_path(filename)
    fc         <- readRDS(filename)
    page_rows  <- list()
    input_files <- character()
    
    heading_for <- function(series) {
      paste0(
        "Scale: ", metadata$scale,
        " | Window: ", metadata$window,
        "\nModel: ", metadata$model,
        " | Series: ", series
      )
    }
    
    add_page <- function(series) {
      tibble::tibble(
        source_file = metadata$relative_file,
        model_key   = metadata$model_key,
        scale       = metadata$scale,
        window      = metadata$window,
        framework   = metadata$framework,
        model       = metadata$model,
        series      = series
      )
    }
    
    if (inherits(fc, "mvgam_forecast")) {
      labels <- as.character(fc$series_names)
      if (!length(labels)) labels <- names(fc$forecasts)
      if (!length(labels) || anyNA(labels)) {
        stop("Cannot identify forecast series in: ", filename)
      }
      
      for (s in seq_along(labels)) {
        draw_mvgam_page(fc, s, heading_for(labels[s]))
        page_rows[[length(page_rows) + 1L]] <- add_page(labels[s])
      }
      
    } else if (inherits(fc, "fbl_ts")) {
      train_file <- file.path(metadata$window_directory, "train_data.rds")
      test_file  <- file.path(metadata$window_directory, "test_data.rds")
      
      if (!file.exists(train_file) || !file.exists(test_file)) {
        stop("Missing window input files for: ", filename)
      }
      
      train       <- tibble::as_tibble(readRDS(train_file))
      test        <- tibble::as_tibble(readRDS(test_file))
      input_files <- c(train_file, test_file)
      
      series_keys  <- setdiff(tsibble::key_vars(fc), ".model")
      combinations <- tibble::as_tibble(fc) |>
        dplyr::select(dplyr::all_of(c(".model", series_keys))) |>
        dplyr::distinct()
      
      if (!nrow(combinations)) stop("Fable contains no series: ", filename)
      
      for (j in seq_len(nrow(combinations))) {
        sel     <- combinations[j, , drop = FALSE]
        keep_fc <- Reduce(`&`, lapply(names(sel), function(k) {
          as.character(fc[[k]]) == as.character(sel[[k]][1L])
        }), rep(TRUE, nrow(fc)))
        
        keep_data <- Reduce(`&`, lapply(series_keys, function(k) {
          as.character(train[[k]]) == as.character(sel[[k]][1L])
        }), rep(TRUE, nrow(train)))
        
        keep_test <- Reduce(`&`, lapply(series_keys, function(k) {
          as.character(test[[k]]) == as.character(sel[[k]][1L])
        }), rep(TRUE, nrow(test)))
        
        fc_one  <- fc[which(keep_fc), ]
        history <- tsibble::as_tsibble(
          train[which(keep_data), ],
          key   = tidyselect::all_of(series_keys),
          index = year
        )
        
        series_label <- paste(
          vapply(series_keys, function(k) as.character(sel[[k]][1L]), character(1)),
          collapse = " / "
        )
        if (!nzchar(series_label)) series_label <- "series"
        
        p <- ggplot2::autoplot(fc_one, history, level = c(80, 95)) +
          ggplot2::geom_point(
            data        = test[which(keep_test), ],
            mapping     = ggplot2::aes(x = year, y = count),
            inherit.aes = FALSE,
            colour      = "black",
            size        = 1.8
          ) +
          ggplot2::labs(
            title = heading_for(series_label),
            x = "Year", y = "Count"
          ) +
          ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(
            legend.position = "bottom",
            plot.title      = ggplot2::element_text(size = 10)
          )
        
        print(p)
        page_rows[[length(page_rows) + 1L]] <- add_page(series_label)
      }
      
    } else {
      stop(
        "Unsupported forecast class in ", filename, ": ",
        paste(class(fc), collapse = ", ")
      )
    }
    
    list(
      pages       = dplyr::bind_rows(page_rows),
      input_files = input_files
    )
  }
  
  grDevices::pdf(
    temporary_pdf,
    width   = 12,
    height  = 8,
    onefile = TRUE,
    title   = "All Models: Hindcasts and Forecasts"
  )
  device_id <- grDevices::dev.cur()
  
  page_records <- vector("list", length(files))
  all_inputs   <- character()
  
  for (i in seq_along(files)) {
    cat(sprintf(
      "  [%d/%d] %s\n", i, length(files),
      substring(files[i], nchar(root) + 2L)
    ))
    
    output           <- process_file(files[i])
    page_records[[i]] <- output$pages
    all_inputs        <- unique(c(all_inputs, output$input_files))
    
    rm(output)
    gc(verbose = FALSE)
  }
  
  close_pdf_device()
  
  manifest       <- dplyr::bind_rows(page_records)
  expected_pages <- nrow(manifest)
  actual_pages   <- pdftools::pdf_info(temporary_pdf)$pages
  
  if (expected_pages < 1L || actual_pages != expected_pages) {
    stop(
      "PDF verification failed: expected ", expected_pages,
      " pages, found ", actual_pages,
      ". Raw files retained. Build: ", temporary_pdf
    )
  }
  
  page_text <- pdftools::pdf_text(temporary_pdf)
  if (length(page_text) != expected_pages ||
      any(!grepl("Scale:", page_text, fixed = TRUE))) {
    stop(
      "PDF page-label check failed. Raw files retained. Build: ", temporary_pdf
    )
  }
  
  if (file.exists(final_pdf)) {
    stop(
      "Combined PDF already exists: ", final_pdf,
      ". Rename it before rebuilding. New build: ", temporary_pdf
    )
  }
  
  if (!file.rename(temporary_pdf, final_pdf)) {
    stop("Could not publish PDF. Build: ", temporary_pdf)
  }
  
  manifest$page <- seq_len(expected_pages)
  
  utils::write.csv(
    manifest,
    file.path(root, "forecast_pdf_manifest.csv"),
    row.names = FALSE
  )
  
  cat("\nVerified PDF:", final_pdf, "\nPages:", actual_pages, "\n")
  
  if (isTRUE(delete_raw)) {
    candidates <- unique(c(files, all_inputs))
    candidates <- candidates[file.exists(candidates)]
    
    cleanup <- data.frame(
      file    = substring(candidates, nchar(root) + 2L),
      bytes   = file.info(candidates)$size,
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
        "Some files could not be deleted. See forecast_cleanup_manifest.csv.",
        call. = FALSE
      )
    }
    
    cat(
      "Deleted", sum(cleanup$deleted), "source files; reclaimed ~",
      round(
        sum(cleanup$bytes[cleanup$deleted], na.rm = TRUE) / 1024^3, 2
      ),
      "GiB.\n"
    )
  } else {
    cat("Raw files retained.\n")
  }
  
  invisible(manifest)
}

forecast_pdf_manifest <- build_combined_forecast_pdf(
  root       = scale_run_folder,
  delete_raw = DELETE_RAW_AFTER_VALIDATION
)

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
cat("MODEL-BY-MODEL SPATIAL SCALE COMPARISON COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")
cat("Results saved to:", scale_run_folder, "\n\n")
cat("Output files:\n")
cat("  base_config.rds                 — base configuration\n")
cat("  window_skill_all_scales.csv     — per-window skill scores\n")
cat("  all_models_window_delta.png     — cross-model delta with ggside marginal\n")
cat("  all_models_window_delta.csv     — cross-model delta table\n")
cat("  all_models_delta_density.png    — density of delta distribution\n")
cat("  all_models_delta_ecdf.png       — ECDF of delta distribution\n")
cat("  all_models_delta_violin.png     — violin plot of delta distribution\n")
cat("  all_models_summary.csv          — aggregated summary\n")
cat("  window_delta_marginal/          — per-panel ggExtra marginal plots\n")
cat("  all_models_forecasts.pdf        — combined forecast PDF\n\n")

for (model_key in names(all_model_results)) {
  cat(glue("  {model_key}/\n"))
  cat(glue("    ├── window_skill_by_scale.png\n"))
  cat(glue("    ├── window_delta_skill.png\n"))
  cat(glue("    ├── window_delta_skill.csv\n"))
  for (scale in SCALES_TO_RUN) cat(glue("    └── {scale}/\n"))
}

# =============================================================================
# ZIP on HiPerGator
# =============================================================================

if (dir.exists("/blue/ewhite")) {
  zip_path <- paste0(scale_run_folder, ".zip")
  cat("\nZipping results folder...\n")
  zip(zipfile = zip_path, files = scale_run_folder)
  cat(glue("  ✓ Zipped: {zip_path}\n"))
  unlink(scale_run_folder, recursive = TRUE)
  cat(glue("  ✓ Removed original: {scale_run_folder}\n"))
}