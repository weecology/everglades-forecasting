# =============================================================================
# RUN_ALL_SCALES.R
#
# Sequential model-by-model comparisons across spatial scales.
#
# Requires:
#   config.yml profile "run_all_scales"
#   main.R defining run_forecasting_pipeline <- function() { ... }
#   data_functions.R
#   evaluation.R
#   selected model scripts
#
# Safety:
#   - Never deletes run directories or raw forecasts.
#   - Verifies copied files using checksums.
#   - Writes checkpoints after each model/scale run.
#   - Preserves original finite skill values without clipping.
#   - PDF failures do not invalidate completed forecasting runs.
#
# Interpretation:
#   Scale delta = system skill - mean subregional skill.
#   This is NOT skill from aggregating regional predictive distributions.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# USER SETTINGS
# =============================================================================

BASE_PROFILE <- "run_all_scales"

SPECIES_TO_RUN <- "top6"
FORECAST_TOTALS <- FALSE
SCALES_TO_RUN <- c("system", "subregion")

MVGAM_MODELS <- c("ar", "ar_exog")
FABLE_MODELS <- character()

FORECAST_HORIZON <- 2L
CV_WINDOWS <- 1L                  # Set NULL after the smoke test succeeds.

RUN_BY_REGION <- TRUE
EXCLUDE_REGIONS <- c("bigcypress", "coastal")

UPDATE_BIRDS <- FALSE
UPDATE_WATER <- FALSE

GENERATE_NATIVE_PNGS <- TRUE
GENERATE_FORECAST_PDF <- TRUE

SEED <- 1234L

# =============================================================================
# BASIC HELPERS
# =============================================================================

log_line <- function(...) {
  cat(..., "\n", sep = "")
}

safe_name <- function(x) {
  gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
}

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required package is not installed: ", package)
  }
}

source_required <- function(filename) {
  if (!file.exists(filename)) {
    stop("Required script not found: ", filename)
  }
  base::source(filename, local = .GlobalEnv)
}

prefer_namespaces <- function() {
  if (requireNamespace("conflicted", quietly = TRUE)) {
    conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
    conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
    conflicted::conflicts_prefer(base::intersect, .quiet = TRUE)
    conflicted::conflicts_prefer(base::union, .quiet = TRUE)
    conflicted::conflicts_prefer(base::setdiff, .quiet = TRUE)
    conflicted::conflicts_prefer(base::setequal, .quiet = TRUE)
    conflicted::conflicts_prefer(base::get, .quiet = TRUE)
    conflicted::conflicts_prefer(base::as.matrix, .quiet = TRUE)
  }
}

# Load only the pipeline function, not main.R's final automatic call.
load_pipeline_definition <- function(filename) {
  if (!file.exists(filename)) stop("Missing script: ", filename)
  
  expressions <- as.list(parse(file = filename))
  
  definitions <- Filter(function(expr) {
    if (!is.call(expr) || length(expr) != 3L) return(FALSE)
    
    operator <- expr[[1L]]
    
    (identical(operator, as.name("<-")) ||
        identical(operator, as.name("="))) &&
      identical(expr[[2L]], as.name("run_forecasting_pipeline")) &&
      is.call(expr[[3L]]) &&
      identical(expr[[3L]][[1L]], as.name("function"))
  }, expressions)
  
  if (length(definitions) != 1L) {
    stop(
      filename,
      " must define exactly one top-level ",
      "run_forecasting_pipeline <- function(...) { ... }."
    )
  }
  
  eval(definitions[[1L]], envir = .GlobalEnv)
}

# =============================================================================
# LOAD PROJECT HELPERS
# =============================================================================

require_package("config")

source_required("data_functions.R")
source_required("evaluation.R")
prefer_namespaces()
load_pipeline_definition("main.R")

required_helpers <- c(
  "get_data_water",
  "get_wading_bird_data",
  "disk_create_dir",
  "disk_save_rds",
  "positive_integer",
  "assert_unique_rows"
)

for (name in required_helpers) {
  if (!exists(name, mode = "function", envir = .GlobalEnv)) {
    stop("Missing project helper: ", name)
  }
}

# =============================================================================
# CONFIGURATION VALIDATION
# =============================================================================

SCALES_TO_RUN <- unique(as.character(SCALES_TO_RUN))
MVGAM_MODELS <- unique(as.character(MVGAM_MODELS))
FABLE_MODELS <- unique(as.character(FABLE_MODELS))

if (!length(SCALES_TO_RUN) ||
    anyNA(SCALES_TO_RUN) ||
    any(!SCALES_TO_RUN %in% c("system", "subregion", "colony"))) {
  stop("Select valid spatial scales: system, subregion, colony.")
}

if (!length(MVGAM_MODELS) && !length(FABLE_MODELS)) {
  stop("Select at least one forecasting model.")
}

selected_models <- c(MVGAM_MODELS, FABLE_MODELS)

if (anyNA(selected_models) || any(!nzchar(selected_models))) {
  stop("Model names must be non-missing, non-empty strings.")
}

if (FORECAST_TOTALS &&
    any(MVGAM_MODELS %in% c("species_specific", "trait", "trait2"))) {
  stop("Remove species-specific/trait models when forecasting totals.")
}

FORECAST_HORIZON <- positive_integer(
  FORECAST_HORIZON, "FORECAST_HORIZON"
)

if (!is.null(CV_WINDOWS)) {
  CV_WINDOWS <- positive_integer(CV_WINDOWS, "CV_WINDOWS")
}

base_config <- config::get(config = BASE_PROFILE)

for (name in c(
  "spatial", "models", "parallel", "cache",
  "plots", "water", "data_update"
)) {
  base_config[[name]] <- base_config[[name]] %||% list()
}

base_config$spatial$include_species <- SPECIES_TO_RUN
base_config$spatial$forecast_totals <- FORECAST_TOTALS
base_config$spatial$exclude_regions <- base::union(
  as.character(unlist(base_config$spatial$exclude_regions)),
  EXCLUDE_REGIONS
)

base_config$train_years <- positive_integer(
  base_config$train_years %||% 20L,
  "train_years"
)

base_config$forecast_horizon <- FORECAST_HORIZON
base_config$test_years <- FORECAST_HORIZON
base_config$cv_windows <- CV_WINDOWS

base_config$models$mvgam <- MVGAM_MODELS
base_config$models$fable <- FABLE_MODELS
base_config$run_mvgam <- length(MVGAM_MODELS) > 0L
base_config$run_fable <- length(FABLE_MODELS) > 0L

base_config$parallel$enabled <- FALSE

# Force fresh preparation in main.R rather than relying on old prepared caches.
# Source observations and the processed water CSV remain disk-backed.
base_config$cache$data <- FALSE
base_config$cache$models <- FALSE

# Training-only ordinal thresholds avoid held-out-data leakage.
base_config$sliding_window_breaks <- TRUE

# Disable main.R's end-of-run comparison plotting.
# The wrapper generates comparison plots below.
base_config$plots$enabled <- FALSE
base_config$plots$native_mvgam <- GENERATE_NATIVE_PNGS
base_config$plots$native_fable <- GENERATE_NATIVE_PNGS
base_config$plots$generate_forecast_pdfs <- FALSE

for (model in unique(c(
  if (length(MVGAM_MODELS)) "baseline",
  MVGAM_MODELS
))) {
  filename <- file.path("models", paste0("mvgam_", model, ".R"))
  if (!file.exists(filename)) stop("Missing model script: ", filename)
}

if (length(FABLE_MODELS) && !file.exists("models/fable_models.R")) {
  stop("Missing models/fable_models.R")
}

# =============================================================================
# OUTPUT DIRECTORY
# =============================================================================

disk_create_dir("results")
disk_create_dir("cache")

stem <- paste(
  safe_name(paste(SPECIES_TO_RUN, collapse = "-")),
  "scale_run",
  format(Sys.time(), "%Y%m%d-%H%M%S"),
  Sys.getpid(),
  sep = "_"
)

scale_run_folder <- file.path("results", stem)
suffix <- 0L

while (dir.exists(scale_run_folder)) {
  suffix <- suffix + 1L
  scale_run_folder <- file.path("results", paste0(stem, "_", suffix))
}

disk_create_dir(scale_run_folder)

log_line("Results directory: ", scale_run_folder)
log_line("Profile: ", BASE_PROFILE)
log_line("Scales: ", paste(SCALES_TO_RUN, collapse = ", "))
log_line("CV windows: ", CV_WINDOWS %||% "all")

# =============================================================================
# SHARED SOURCE-DATA PREPARATION
# =============================================================================

invalidate_prepared_cache <- function() {
  files <- list.files(
    "cache",
    pattern = "^data_.*\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) && !all(file.remove(files))) {
    stop("Could not invalidate prepared data caches.")
  }
}

log_line("\nPreparing shared source data...")

# Test the actual loader instead of assuming a download directory name.
check_bird_observations <- function(path = ".") {
  tryCatch({
    counts <- tibble::as_tibble(
      wader::max_counts(level = "all", path = path)
    )
    
    required <- c("year", "species", "count")
    missing <- base::setdiff(required, names(counts))
    
    if (length(missing)) {
      stop("Missing columns: ", paste(missing, collapse = ", "))
    }
    
    if (!nrow(counts)) {
      stop("The observation loader returned no rows.")
    }
    
    list(ok = TRUE, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
}

bird_check <- check_bird_observations(".")

if (UPDATE_BIRDS || !bird_check$ok) {
  if (!bird_check$ok) {
    log_line("Existing observations could not be read: ", bird_check$error)
  }
  
  log_line("Downloading bird observations...")
  wader::download_observations(".")
  invalidate_prepared_cache()
  
  bird_check <- check_bird_observations(".")
}

if (!bird_check$ok) {
  stop(
    "Bird observations remain unreadable after preparation.",
    "\nWorking directory: ", getwd(),
    "\nLoader error: ", bird_check$error,
    call. = FALSE
  )
}

log_line("Bird observations are readable.")

water <- get_data_water(
  remote_url = base_config$water$remote_url,
  update = UPDATE_WATER
)

if (!nrow(water)) stop("Water-data preparation returned no rows.")
rm(water)

if (UPDATE_WATER) invalidate_prepared_cache()

# Source updates were handled above. Do not repeat downloads in main.R.
base_config$data_update$birds <- FALSE
base_config$data_update$water <- FALSE
base_config$water$remote_url <- NULL

disk_save_rds(
  base_config,
  file.path(scale_run_folder, "base_config.rds")
)

# =============================================================================
# PREFLIGHT DATA VALIDATION
# =============================================================================

covars <- c("breed_season_depth", "recession", "dry_days")

uses_water <- length(base::setdiff(MVGAM_MODELS, "baseline")) > 0L ||
  any(FABLE_MODELS %in% c("arima_exog", "gam", "tslm"))

preflight_rows <- list()

for (scale in SCALES_TO_RUN) {
  cfg <- base_config
  cfg$spatial$level <- scale
  
  df <- tibble::as_tibble(
    get_wading_bird_data(config = cfg, cache = FALSE)
  )
  
  if (!nrow(df)) stop("No observations returned for scale: ", scale)
  
  keys <- c("species", if (scale != "system") "region")
  required <- c(keys, "year", "count")
  missing <- base::setdiff(required, names(df))
  
  if (length(missing)) {
    stop(scale, " data is missing: ", paste(missing, collapse = ", "))
  }
  
  assert_unique_rows(df, c(keys, "year"), paste(scale, "data"))
  
  if (anyNA(df[keys])) stop("Missing series keys at scale: ", scale)
  
  if (!is.numeric(df$count) || any(!is.finite(df$count))) {
    stop("Counts must be finite numeric values at scale: ", scale)
  }
  
  if (!is.numeric(df$year) ||
      any(!is.finite(df$year)) ||
      any(df$year != floor(df$year))) {
    stop("Years must be finite integers at scale: ", scale)
  }
  
  if (uses_water) {
    missing <- base::setdiff(covars, names(df))
    
    if (length(missing)) {
      stop(scale, " data is missing covariates: ",
           paste(missing, collapse = ", "))
    }
    
    for (v in covars) {
      if (!is.numeric(df[[v]]) || any(!is.finite(df[[v]]))) {
        stop(
          "Invalid water covariate ", v, " at scale ", scale,
          ". Check water coverage and region exclusions."
        )
      }
    }
  }
  
  # Check complete calendar panels before fitting.
  expected_years <- seq.int(min(df$year), max(df$year))
  
  panels <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      complete = base::setequal(year, expected_years),
      .groups = "drop"
    )
  
  if (any(!panels$complete)) {
    stop("Incomplete calendar panel at scale: ", scale)
  }
  
  if (length(expected_years) <
      base_config$train_years + FORECAST_HORIZON) {
    stop("Insufficient calendar years at scale: ", scale)
  }
  
  disk_save_rds(
    df,
    file.path(scale_run_folder, paste0("preflight_data_", scale, ".rds"))
  )
  
  preflight_rows[[scale]] <- tibble::tibble(
    scale = scale,
    rows = nrow(df),
    series = nrow(panels),
    first_year = min(df$year),
    last_year = max(df$year)
  )
  
  rm(df, panels)
  gc(verbose = FALSE)
}

preflight <- dplyr::bind_rows(preflight_rows)

utils::write.csv(
  preflight,
  file.path(scale_run_folder, "preflight_manifest.csv"),
  row.names = FALSE
)

# main.R determines windows from each dataset's full calendar range.
# Require identical ranges rather than silently matching unequal windows.
if (dplyr::n_distinct(preflight$first_year) != 1L ||
    dplyr::n_distinct(preflight$last_year) != 1L) {
  stop(
    "Selected scales have different calendar ranges. ",
    "Align them in data preparation before making scale comparisons."
  )
}

log_line("\nPreflight validation passed.")
print(preflight)

# =============================================================================
# CHECKED RESULT COPYING
# =============================================================================

copy_run_files <- function(from, to) {
  from <- normalizePath(from, winslash = "/", mustWork = TRUE)
  
  files <- list.files(
    from,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  
  files <- files[!file.info(files)$isdir]
  
  if (!length(files)) stop("Run directory is empty: ", from)
  if (dir.exists(to)) stop("Destination already exists: ", to)
  
  disk_create_dir(to)
  relative <- substring(files, nchar(from) + 2L)
  destinations <- file.path(to, relative)
  
  for (i in seq_along(files)) {
    disk_create_dir(dirname(destinations[i]))
    
    if (!isTRUE(file.copy(
      files[i], destinations[i], overwrite = FALSE
    ))) {
      stop("Failed to copy: ", files[i])
    }
  }
  
  if (!identical(
    unname(tools::md5sum(files)),
    unname(tools::md5sum(destinations))
  )) {
    stop("Checksum verification failed for copied run: ", to)
  }
  
  invisible(to)
}

# =============================================================================
# MODEL LIST AND METRIC READER
# =============================================================================

models_to_test <- list()

for (model in MVGAM_MODELS) {
  models_to_test[[paste0("mvgam_", model)]] <- list(
    framework = "mvgam",
    model = model
  )
}

for (model in FABLE_MODELS) {
  models_to_test[[paste0("fable_", model)]] <- list(
    framework = "fable",
    model = model
  )
}

read_framework_metrics <- function(folder, framework) {
  filename <- file.path(folder, "metrics_only.rds")
  
  if (file.exists(filename)) {
    metrics <- readRDS(filename)[[framework]]
  } else {
    filename <- file.path(folder, "forecast_results.rds")
    
    if (!file.exists(filename)) {
      stop("No metrics/results file found in: ", folder)
    }
    
    result <- readRDS(filename)
    metrics <- result[[framework]]$metrics
  }
  
  if (!is.data.frame(metrics)) return(tibble::tibble())
  
  metrics <- tibble::as_tibble(metrics)
  
  if (".model" %in% names(metrics) &&
      !"model" %in% names(metrics)) {
    metrics <- dplyr::rename(metrics, model = .model)
  }
  
  metrics
}

# =============================================================================
# RUN MODELS SEQUENTIALLY
# =============================================================================

all_model_results <- setNames(
  vector("list", length(models_to_test)),
  names(models_to_test)
)

run_records <- list()

for (model_key in names(models_to_test)) {
  info <- models_to_test[[model_key]]
  model_folder <- file.path(scale_run_folder, model_key)
  disk_create_dir(model_folder)
  
  all_model_results[[model_key]] <- list()
  
  for (scale in SCALES_TO_RUN) {
    log_line("\nRunning ", model_key, " at ", scale)
    
    cfg <- base_config
    cfg$spatial$level <- scale
    cfg$spatial$run_by_region <- RUN_BY_REGION && scale != "system"
    cfg$model_key <- model_key
    
    cfg$models$mvgam <- character()
    cfg$models$fable <- character()
    cfg$models[[info$framework]] <- unique(c("baseline", info$model))
    
    cfg$run_mvgam <- info$framework == "mvgam"
    cfg$run_fable <- info$framework == "fable"
    
    assign("CONFIG", cfg, envir = .GlobalEnv)
    
    # Remove stale globals from previous runs before launching main.R.
    for (name in c("run_folder", "results")) {
      if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = name, envir = .GlobalEnv)
      }
    }
    
    started <- Sys.time()
    failure <- NA_character_
    source_folder <- NA_character_
    destination <- NA_character_
    status <- "failed"
    
    set.seed(SEED)
    
    completed <- tryCatch({
      run_forecasting_pipeline()
      
      source_folder <- base::get(
        "run_folder", envir = .GlobalEnv, inherits = FALSE
      )
      
      if (!dir.exists(source_folder)) {
        stop("Pipeline output directory does not exist.")
      }
      
      metrics <- read_framework_metrics(source_folder, info$framework)
      
      if (!nrow(metrics) ||
          !"model" %in% names(metrics) ||
          !info$model %in% metrics$model) {
        stop("No metric rows returned for requested model: ", info$model)
      }
      
      TRUE
    }, error = function(e) {
      failure <<- conditionMessage(e)
      FALSE
    })
    
    # Recover this run's directory even if the pipeline failed.
    if (is.na(source_folder) &&
        exists("run_folder", envir = .GlobalEnv, inherits = FALSE)) {
      source_folder <- base::get(
        "run_folder", envir = .GlobalEnv, inherits = FALSE
      )
    }
    
    if (completed) {
      destination <- file.path(model_folder, scale)
      
      # Copy errors are fatal. Original files remain untouched.
      copy_run_files(source_folder, destination)
      
      all_model_results[[model_key]][[scale]] <- destination
      status <- "completed"
      
      log_line("Verified copy: ", destination)
      log_line("Original retained: ", source_folder)
    } else {
      writeLines(
        c(
          paste("Model:", model_key),
          paste("Scale:", scale),
          paste("Error:", failure),
          paste("Retained source directory:", source_folder)
        ),
        file.path(model_folder, paste0("FAILED_", scale, ".txt"))
      )
      
      log_line("FAILED: ", failure)
    }
    
    run_records[[length(run_records) + 1L]] <- tibble::tibble(
      model_key = model_key,
      scale = scale,
      status = status,
      source_directory = source_folder,
      result_directory = destination,
      elapsed_seconds = as.numeric(
        difftime(Sys.time(), started, units = "secs")
      ),
      error = failure
    )
    
    utils::write.csv(
      dplyr::bind_rows(run_records),
      file.path(scale_run_folder, "run_manifest.csv"),
      row.names = FALSE
    )
    
    disk_save_rds(
      all_model_results,
      file.path(scale_run_folder, "result_paths.rds")
    )
    
    gc(verbose = FALSE)
  }
}

# =============================================================================
# EXTRACT RAW WINDOW SKILL
# =============================================================================

metric_parts <- list()

for (model_key in names(all_model_results)) {
  info <- models_to_test[[model_key]]
  
  for (scale in names(all_model_results[[model_key]])) {
    folder <- all_model_results[[model_key]][[scale]]
    metrics <- read_framework_metrics(folder, info$framework)
    
    if (!nrow(metrics)) next
    
    required <- c("model", "species", "test_start")
    missing <- base::setdiff(required, names(metrics))
    
    if (length(missing)) {
      warning(
        "Skipping metrics missing required columns: ",
        paste(missing, collapse = ", "),
        " in ", folder,
        call. = FALSE
      )
      next
    }
    
    metrics <- metrics |>
      dplyr::filter(model == info$model)
    
    if (!nrow(metrics)) next
    
    skill_columns <- base::intersect(
      c("crps_skill", "rmse_skill", "rps_skill"),
      names(metrics)
    )
    
    if (!length(skill_columns)) next
    
    metric_parts[[length(metric_parts) + 1L]] <- metrics |>
      dplyr::select(
        dplyr::any_of(c(
          "model", "species", "region", "window",
          "test_start", ".type",
          skill_columns
        ))
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(skill_columns),
        names_to = "metric",
        values_to = "skill_score"
      ) |>
      dplyr::mutate(
        model_key = .env$model_key,
        framework = .env$info$framework,
        scale = .env$scale,
        metric_label = dplyr::case_when(
          metric == "crps_skill" ~ "CRPS Skill",
          metric == "rmse_skill" ~ "RMSE Skill",
          metric == "rps_skill" ~ "RPS Skill",
          TRUE ~ metric
        )
      )
  }
}

window_long <- dplyr::bind_rows(metric_parts)

utils::write.csv(
  window_long,
  file.path(scale_run_folder, "window_skill_all_scales.csv"),
  row.names = FALSE
)

# =============================================================================
# SCALE MEANS, DELTAS, AND SUMMARY
# =============================================================================

scale_means <- tibble::tibble()
cross_delta <- tibble::tibble()
overall_summary <- tibble::tibble()

if (nrow(window_long)) {
  # Retain invalid scores in the raw CSV; use only finite values in summaries.
  finite_rows <- window_long |>
    dplyr::filter(
      is.finite(skill_score),
      !is.na(test_start)
    )
  
  if (nrow(finite_rows)) {
    group_keys <- c(
      "model_key", "framework", "species",
      "test_start", "metric", "metric_label", "scale",
      base::intersect(".type", names(finite_rows))
    )
    
    scale_means <- finite_rows |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_keys))
      ) |>
      dplyr::summarise(
        skill_score = mean(skill_score),
        n_metric_rows = dplyr::n(),
        .groups = "drop"
      )
    
    # Coverage counts help identify comparisons affected by failed regions.
    utils::write.csv(
      scale_means,
      file.path(scale_run_folder, "window_skill_scale_means.csv"),
      row.names = FALSE
    )
    
    overall_summary <- scale_means |>
      dplyr::group_by(model_key, framework, scale, metric) |>
      dplyr::summarise(
        mean_skill = mean(skill_score),
        median_skill = stats::median(skill_score),
        n_window_rows = dplyr::n(),
        .groups = "drop"
      )
    
    utils::write.csv(
      overall_summary,
      file.path(scale_run_folder, "all_models_summary.csv"),
      row.names = FALSE
    )
    
    if (all(c("system", "subregion") %in% SCALES_TO_RUN)) {
      delta_keys <- base::setdiff(group_keys, "scale")
      
      cross_delta <- scale_means |>
        dplyr::filter(scale %in% c("system", "subregion")) |>
        dplyr::select(
          dplyr::all_of(delta_keys),
          scale, skill_score
        ) |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(delta_keys),
          names_from = scale,
          values_from = skill_score
        )
      
      for (scale in c("system", "subregion")) {
        if (!scale %in% names(cross_delta)) {
          cross_delta[[scale]] <- rep(NA_real_, nrow(cross_delta))
        }
      }
      
      cross_delta <- cross_delta |>
        dplyr::filter(is.finite(system), is.finite(subregion)) |>
        dplyr::mutate(delta_skill = system - subregion)
      
      utils::write.csv(
        cross_delta,
        file.path(scale_run_folder, "all_models_window_delta.csv"),
        row.names = FALSE
      )
    }
  }
} else {
  warning("No requested-model window metrics were available.", call. = FALSE)
}

# =============================================================================
# COMPARISON PLOTS
# =============================================================================

save_plot_safely <- function(plot, filename, width = 12, height = 8) {
  tryCatch({
    ggplot2::ggsave(
      filename,
      plot = plot,
      width = width,
      height = height,
      dpi = 200,
      limitsize = FALSE
    )
    TRUE
  }, error = function(e) {
    warning(
      "Plot failed: ", filename, ": ", conditionMessage(e),
      call. = FALSE
    )
    FALSE
  })
}

scale_colors <- c(
  system = "#2166AC",
  subregion = "#B2182B",
  colony = "#1B7837"
)

if (nrow(scale_means)) {
  for (model_key in unique(scale_means$model_key)) {
    plot_data <- scale_means |>
      dplyr::filter(model_key == .env$model_key)
    
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = test_start,
        y = skill_score,
        colour = scale,
        group = scale
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed", colour = "grey50"
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(species ~ metric_label, scales = "free_y") +
      ggplot2::scale_colour_manual(values = scale_colors) +
      ggplot2::theme_classic(base_size = 11) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(
        title = paste(model_key, "— skill by spatial scale"),
        subtitle = "Spatial-scale points are means of available regional metric rows",
        x = "Forecast start year",
        y = "Skill versus baseline",
        colour = "Scale"
      )
    
    save_plot_safely(
      p,
      file.path(scale_run_folder, model_key, "window_skill_by_scale.png"),
      height = max(6, dplyr::n_distinct(plot_data$species) * 2)
    )
  }
}

if (nrow(cross_delta)) {
  for (model_key in unique(cross_delta$model_key)) {
    plot_data <- cross_delta |>
      dplyr::filter(model_key == .env$model_key)
    
    utils::write.csv(
      plot_data,
      file.path(scale_run_folder, model_key, "window_delta_skill.csv"),
      row.names = FALSE
    )
    
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = test_start,
        y = delta_skill,
        colour = metric_label,
        group = metric_label
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed", colour = "grey50"
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_wrap(~species, scales = "free_y") +
      ggplot2::theme_classic(base_size = 11) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(
        title = paste(model_key, "— system minus mean subregional skill"),
        subtitle = "Positive: system skill is higher; negative: mean subregional skill is higher",
        x = "Forecast start year",
        y = "Skill difference",
        colour = "Metric"
      )
    
    save_plot_safely(
      p,
      file.path(scale_run_folder, model_key, "window_delta_skill.png")
    )
  }
  
  p_cross <- ggplot2::ggplot(
    cross_delta,
    ggplot2::aes(
      x = test_start,
      y = delta_skill,
      colour = model_key,
      group = model_key
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", colour = "grey50"
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(species ~ metric_label, scales = "free_y") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "System minus mean subregional skill",
      subtitle = "Unclipped scores; comparisons use matching forecast start years",
      x = "Forecast start year",
      y = "Skill difference",
      colour = "Model"
    )
  
  save_plot_safely(
    p_cross,
    file.path(scale_run_folder, "all_models_window_delta.png"),
    width = 14,
    height = max(7, dplyr::n_distinct(cross_delta$species) * 2)
  )
  
  p_ecdf <- ggplot2::ggplot(
    cross_delta,
    ggplot2::aes(x = delta_skill, colour = model_key)
  ) +
    ggplot2::stat_ecdf() +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed", colour = "grey50"
    ) +
    ggplot2::facet_wrap(~metric_label, scales = "free_x") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Distribution of scale skill differences",
      x = "System minus mean subregional skill",
      y = "Empirical cumulative probability",
      colour = "Model"
    )
  
  save_plot_safely(
    p_ecdf,
    file.path(scale_run_folder, "all_models_delta_ecdf.png")
  )
}

# =============================================================================
# OPTIONAL COMBINED FORECAST PDF
#
# Each forecast object is rendered into its own PDF fragment.
# A failed object cannot corrupt successful fragments.
# Raw files and fragments are retained regardless of success.
# =============================================================================

build_combined_forecast_pdf <- function(root) {
  require_package("pdftools")
  require_package("qpdf")
  
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  
  files <- sort(list.files(
    root,
    pattern = "^forecast\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  ))
  
  if (!length(files)) {
    warning("No forecast.rds files found; skipping PDF.", call. = FALSE)
    return(invisible(NULL))
  }
  
  fragment_dir <- file.path(root, "forecast_pdf_fragments")
  disk_create_dir(fragment_dir)
  
  parse_metadata <- function(filename) {
    relative <- substring(filename, nchar(root) + 2L)
    parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
    positions <- which(parts == "windows")
    
    if (length(positions) != 1L) {
      stop("Cannot identify windows directory: ", filename)
    }
    
    w <- positions[1L]
    
    if (w < 3L || length(parts) < w + 3L) {
      stop("Unexpected forecast path: ", filename)
    }
    
    list(
      source_file = relative,
      model_key = parts[w - 2L],
      scale = parts[w - 1L],
      window = parts[w + 1L],
      framework = parts[w + 2L],
      model = if (length(parts) == w + 4L) {
        parts[w + 3L]
      } else {
        parts[w + 2L]
      },
      window_directory = file.path(
        root,
        paste(parts[seq_len(w + 1L)], collapse = "/")
      )
    )
  }
  
  render_one <- function(filename, fragment) {
    metadata <- parse_metadata(filename)
    fc <- readRDS(filename)
    records <- list()
    
    grDevices::pdf(fragment, width = 12, height = 8, onefile = TRUE)
    device <- grDevices::dev.cur()
    
    on.exit({
      devices <- grDevices::dev.list()
      if (!is.null(devices) && device %in% devices) {
        grDevices::dev.off(which = device)
      }
    }, add = TRUE)
    
    heading <- function(model, series) {
      paste0(
        "Scale: ", metadata$scale,
        " | Window: ", metadata$window,
        "\nModel: ", model,
        " | Series: ", series
      )
    }
    
    add_record <- function(model, series) {
      tibble::tibble(
        source_file = metadata$source_file,
        model_key = metadata$model_key,
        scale = metadata$scale,
        window = metadata$window,
        framework = metadata$framework,
        model = model,
        series = series
      )
    }
    
    if (inherits(fc, "mvgam_forecast")) {
      require_package("mvgam")
      
      labels <- as.character(fc$series_names)
      if (!length(labels)) labels <- names(fc$forecasts)
      
      if (!length(labels) || anyNA(labels)) {
        stop("Cannot identify mvgam forecast series.")
      }
      
      for (i in seq_along(labels)) {
        p <- graphics::plot(fc, series = i)
        title <- heading(metadata$model, labels[i])
        
        if (inherits(p, "patchwork")) {
          require_package("patchwork")
          print(p + patchwork::plot_annotation(title = title))
        } else if (inherits(p, "ggplot")) {
          print(p + ggplot2::labs(title = title))
        } else {
          graphics::mtext(
            title, side = 3, line = 0.5, cex = 0.65, adj = 0
          )
        }
        
        records[[length(records) + 1L]] <- add_record(
          metadata$model, labels[i]
        )
      }
    } else if (inherits(fc, "fbl_ts")) {
      require_package("fabletools")
      require_package("tsibble")
      
      train <- tibble::as_tibble(readRDS(file.path(
        metadata$window_directory, "train_data.rds"
      )))
      
      test <- tibble::as_tibble(readRDS(file.path(
        metadata$window_directory, "test_data.rds"
      )))
      
      keys <- base::setdiff(tsibble::key_vars(fc), ".model")
      
      combinations <- tibble::as_tibble(fc) |>
        dplyr::select(dplyr::all_of(c(".model", keys))) |>
        dplyr::distinct()
      
      if (!nrow(combinations)) stop("No fable forecast combinations.")
      
      match_rows <- function(df, selected, columns) {
        keep <- rep(TRUE, nrow(df))
        
        for (key in columns) {
          if (!key %in% names(df)) stop("Missing fable key: ", key)
          
          keep <- keep &
            !is.na(df[[key]]) &
            as.character(df[[key]]) ==
            as.character(selected[[key]][1L])
        }
        
        which(keep)
      }
      
      for (i in seq_len(nrow(combinations))) {
        selected <- combinations[i, , drop = FALSE]
        
        fc_one <- fc[
          match_rows(fc, selected, c(".model", keys)), ,
          drop = FALSE
        ]
        
        history <- tsibble::as_tsibble(
          train[match_rows(train, selected, keys), , drop = FALSE],
          key = tidyselect::all_of(keys),
          index = year
        )
        
        observed <- test[
          match_rows(test, selected, keys), , drop = FALSE
        ]
        
        model <- as.character(selected$.model[1L])
        
        series <- paste(
          vapply(keys, function(key) {
            as.character(selected[[key]][1L])
          }, character(1)),
          collapse = " / "
        )
        
        if (!nzchar(series)) series <- "series"
        
        p <- ggplot2::autoplot(fc_one, history, level = c(80, 95)) +
          ggplot2::geom_point(
            data = observed,
            mapping = ggplot2::aes(x = year, y = count),
            inherit.aes = FALSE,
            colour = "black"
          ) +
          ggplot2::labs(
            title = heading(model, series),
            x = "Year",
            y = "Count"
          ) +
          ggplot2::theme_classic(base_size = 11)
        
        print(p)
        
        records[[length(records) + 1L]] <- add_record(model, series)
      }
    } else {
      stop("Unsupported forecast class: ", paste(class(fc), collapse = ", "))
    }
    
    dplyr::bind_rows(records)
  }
  
  fragments <- character()
  pages <- list()
  failures <- list()
  
  for (i in seq_along(files)) {
    log_line("PDF forecast ", i, "/", length(files))
    
    fragment <- file.path(fragment_dir, sprintf("forecast_%05d.pdf", i))
    
    result <- tryCatch({
      records <- render_one(files[i], fragment)
      
      if (pdftools::pdf_info(fragment)$pages != nrow(records) ||
          !nrow(records)) {
        stop("Rendered page count does not match the manifest.")
      }
      
      text <- pdftools::pdf_text(fragment)
      
      if (any(!grepl("Scale:", text, fixed = TRUE))) {
        stop("Rendered pages are missing expected labels.")
      }
      
      records
    }, error = function(e) {
      failures[[length(failures) + 1L]] <<- tibble::tibble(
        source_file = substring(files[i], nchar(root) + 2L),
        error = conditionMessage(e)
      )
      NULL
    })
    
    if (!is.null(result)) {
      fragments <- c(fragments, fragment)
      pages[[length(pages) + 1L]] <- result
    }
    
    gc(verbose = FALSE)
  }
  
  utils::write.csv(
    dplyr::bind_rows(failures),
    file.path(root, "forecast_pdf_failures.csv"),
    row.names = FALSE
  )
  
  if (!length(fragments)) {
    warning(
      "No forecasts rendered successfully. Raw forecasts retained.",
      call. = FALSE
    )
    return(invisible(NULL))
  }
  
  temporary <- tempfile(tmpdir = root, fileext = ".pdf")
  final <- file.path(root, "all_models_forecasts.pdf")
  
  qpdf::pdf_combine(input = fragments, output = temporary)
  
  manifest <- dplyr::bind_rows(pages)
  actual_pages <- pdftools::pdf_info(temporary)$pages
  
  if (actual_pages != nrow(manifest)) {
    stop("Combined PDF page-count verification failed: ", temporary)
  }
  
  if (file.exists(final)) {
    stop("Combined PDF already exists; new build retained at: ", temporary)
  }
  
  if (!file.rename(temporary, final)) {
    stop("Could not publish PDF; build retained at: ", temporary)
  }
  
  manifest$page <- seq_len(nrow(manifest))
  
  utils::write.csv(
    manifest,
    file.path(root, "forecast_pdf_manifest.csv"),
    row.names = FALSE
  )
  
  log_line("Combined PDF saved: ", final)
  log_line("Forecast objects omitted: ", length(failures))
  log_line("Raw forecasts and PDF fragments retained.")
  
  invisible(manifest)
}

if (GENERATE_FORECAST_PDF) {
  tryCatch(
    build_combined_forecast_pdf(scale_run_folder),
    error = function(e) {
      warning(
        "PDF generation failed: ", conditionMessage(e),
        ". Forecasts, metrics, and raw files remain available.",
        call. = FALSE
      )
    }
  )
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

run_manifest <- dplyr::bind_rows(run_records)

disk_save_rds(
  list(
    run_manifest = run_manifest,
    result_paths = all_model_results,
    preflight = preflight,
    completed_at = Sys.time(),
    session = utils::sessionInfo()
  ),
  file.path(scale_run_folder, "wrapper_metadata.rds")
)

log_line("\nMODEL/SCALE RUN SUMMARY")
print(run_manifest)

log_line("\nResults saved to: ", scale_run_folder)
log_line("Raw files and original pipeline directories were retained.")
log_line("No automatic cleanup or archive deletion was performed.")

if (!any(run_manifest$status == "completed")) {
  stop(
    "All model/scale runs failed. Inspect run_manifest.csv and FAILED_*.txt."
  )
}

if (any(run_manifest$status == "failed")) {
  warning(
    "Some runs failed; available outputs were saved. ",
    "Inspect run_manifest.csv before interpreting comparisons.",
    call. = FALSE
  )
}