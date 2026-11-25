# =============================================================================
# RUN_ALL_SCALES.R
#
# Requires:
#   main.R defining run_forecasting_pipeline()
#   data_functions.R
#   evaluation.R
#   models/mvgam_*.R and/or models/fable_models.R
#
# Outputs:
#   <root>/<model_key>/<scale>/... disk-backed pipeline outputs
#   run_manifest.csv
#   window_skill_all_scales.csv          unmodified regional/system skill
#   window_skill_scale_means.csv         mean regional skill per year
#   all_models_window_delta.csv          system minus mean subregional skill
#   comparison figures
#   optional combined forecast PDF and success/failure manifests
#
# Safety:
#   Sequential model/scale execution.
#   Failed runs and raw files are retained.
#   Copy failures are fatal.
#   No automatic deletion or zipping.
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
CV_WINDOWS <- 1L                     # Set NULL only after the smoke test works.

RUN_BY_REGION <- TRUE               # Separate fits outside system scale.
EXCLUDE_REGIONS <- c("bigcypress", "coastal")

UPDATE_BIRDS <- FALSE                # Download automatically if absent.
UPDATE_WATER <- FALSE
USE_DATA_CACHE <- TRUE

GENERATE_FORECAST_PDF <- TRUE
SEED <- 1234L

# =============================================================================
# HELPERS AND PIPELINE DEFINITION
# =============================================================================

required_scripts <- c("data_functions.R", "evaluation.R", "main.R")

for (filename in required_scripts) {
  if (!file.exists(filename)) {
    stop("Required script not found: ", filename)
  }
}

source("data_functions.R", local = .GlobalEnv)
source("evaluation.R", local = .GlobalEnv)

# Resolve set-operation conflicts after spatial packages have loaded.
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

# Load ONLY the function definition from main.R.
# This also works when main.R ends with an unconditional pipeline call.
load_pipeline_definition <- function(filename) {
  expressions <- parse(file = filename)
  
  definitions <- Filter(function(expr) {
    is.call(expr) &&
      as.character(expr[[1L]]) %in% c("<-", "=") &&
      identical(expr[[2L]], as.name("run_forecasting_pipeline")) &&
      is.call(expr[[3L]]) &&
      identical(expr[[3L]][[1L]], as.name("function"))
  }, as.list(expressions))
  
  if (length(definitions) != 1L) {
    stop(
      filename,
      " must contain one top-level ",
      "run_forecasting_pipeline <- function(...) definition."
    )
  }
  
  eval(definitions[[1L]], envir = .GlobalEnv)
}

load_pipeline_definition("main.R")

safe_name <- function(x) {
  gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
}

log_line <- function(...) {
  cat(..., "\n", sep = "")
}

invalidate_data_cache <- function() {
  files <- list.files(
    "cache",
    pattern = "^data_.*\\.rds$",
    full.names = TRUE
  )
  
  if (length(files) && !all(file.remove(files))) {
    stop("Could not invalidate prepared data caches.")
  }
}

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
  
  if (!length(files)) stop("Run directory contains no files: ", from)
  if (dir.exists(to)) stop("Destination already exists: ", to)
  
  disk_create_dir(to)
  
  for (src in files) {
    relative <- substring(src, nchar(from) + 2L)
    dest <- file.path(to, relative)
    disk_create_dir(dirname(dest))
    
    copied <- file.copy(src, dest, overwrite = FALSE)
    
    if (!isTRUE(copied) ||
        !file.exists(dest) ||
        file.info(src)$size != file.info(dest)$size) {
      stop("Copy failed or size mismatch: ", src, " -> ", dest)
    }
  }
  
  # Verify content before reporting a successful copy.
  relative <- substring(files, nchar(from) + 2L)
  destinations <- file.path(to, relative)
  
  if (!identical(
    unname(tools::md5sum(files)),
    unname(tools::md5sum(destinations))
  )) {
    stop("Checksum verification failed for copied run: ", to)
  }
  
  invisible(to)
}

# =============================================================================
# CONFIGURATION AND OUTPUT DIRECTORY
# =============================================================================

SCALES_TO_RUN <- unique(as.character(SCALES_TO_RUN))
MVGAM_MODELS <- unique(as.character(MVGAM_MODELS))
FABLE_MODELS <- unique(as.character(FABLE_MODELS))

if (!length(SCALES_TO_RUN) ||
    any(!SCALES_TO_RUN %in% c("system", "subregion", "colony"))) {
  stop("Select at least one valid spatial scale.")
}

if (!length(MVGAM_MODELS) && !length(FABLE_MODELS)) {
  stop("Select at least one model.")
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

base_config$spatial <- base_config$spatial %||% list()
base_config$plots <- base_config$plots %||% list()
base_config$parallel <- base_config$parallel %||% list()
base_config$cache <- base_config$cache %||% list()
base_config$data_update <- base_config$data_update %||% list()
base_config$water <- base_config$water %||% list()
base_config$models <- base_config$models %||% list()

base_config$spatial$include_species <- SPECIES_TO_RUN
base_config$spatial$forecast_totals <- FORECAST_TOTALS
base_config$spatial$exclude_regions <- base::union(
  as.character(unlist(base_config$spatial$exclude_regions)),
  EXCLUDE_REGIONS
)

base_config$models$mvgam <- MVGAM_MODELS
base_config$models$fable <- FABLE_MODELS
base_config$run_mvgam <- length(MVGAM_MODELS) > 0L
base_config$run_fable <- length(FABLE_MODELS) > 0L

base_config$forecast_horizon <- FORECAST_HORIZON
base_config$test_years <- FORECAST_HORIZON
base_config$cv_windows <- CV_WINDOWS
base_config$parallel$enabled <- FALSE
base_config$cache$data <- USE_DATA_CACHE
base_config$cache$models <- FALSE

# Use training-only ordinal thresholds for CV comparisons.
base_config$sliding_window_breaks <- TRUE

# Native plots happen during evaluation; wrapper makes comparison figures.
base_config$plots$enabled <- FALSE
base_config$plots$generate_forecast_pdfs <- FALSE

# Ensure model files exist before doing any expensive work.
for (model in unique(c(
  if (length(MVGAM_MODELS)) "baseline",
  MVGAM_MODELS
))) {
  filename <- file.path("models", paste0("mvgam_", model, ".R"))
  if (!file.exists(filename)) stop("Missing model file: ", filename)
}

if (length(FABLE_MODELS) && !file.exists("models/fable_models.R")) {
  stop("Missing models/fable_models.R")
}

disk_create_dir("results")
disk_create_dir("cache")

tag <- paste(
  safe_name(paste(SPECIES_TO_RUN, collapse = "-")),
  "scale_run",
  format(Sys.time(), "%Y%m%d-%H%M%S"),
  Sys.getpid(),
  sep = "_"
)

scale_run_folder <- file.path("results", tag)
if (dir.exists(scale_run_folder)) stop("Output directory already exists.")
disk_create_dir(scale_run_folder)

disk_save_rds(base_config, file.path(scale_run_folder, "base_config.rds"))

log_line("Results directory: ", scale_run_folder)
log_line("Profile: ", BASE_PROFILE)
log_line("Scales: ", paste(SCALES_TO_RUN, collapse = ", "))
log_line("CV windows: ", CV_WINDOWS %||% "all")

# =============================================================================
# SERIAL SHARED-DATA PRECOMPUTE
# =============================================================================

log_line("Preparing shared data...")

if (UPDATE_BIRDS || !dir.exists("SiteandMethods")) {
  wader::download_observations(".")
  invalidate_data_cache()
}

water_file <- file.path("WaterData", "eden_covariates.csv")
water_before <- if (file.exists(water_file)) {
  unname(tools::md5sum(water_file))
} else {
  NA_character_
}

water <- get_data_water(
  remote_url = base_config$water$remote_url,
  update = UPDATE_WATER
)
rm(water)

water_after <- unname(tools::md5sum(water_file))

if (!identical(water_before, water_after)) {
  invalidate_data_cache()
}

# Updates have been handled serially. Avoid repeated remote checks in main.R.
base_config$data_update$birds <- FALSE
base_config$data_update$water <- FALSE
base_config$water$remote_url <- NULL

uses_water <- any(base::setdiff(MVGAM_MODELS, "baseline") != "") ||
  any(FABLE_MODELS %in% c("arima_exog", "gam", "tslm"))

covars <- c("breed_season_depth", "recession", "dry_days")
shared_data <- list()

for (scale in SCALES_TO_RUN) {
  cfg <- base_config
  cfg$spatial$level <- scale
  
  # Rebuild once per scale to avoid using stale prepared caches.
  df <- tibble::as_tibble(
    get_wading_bird_data(config = cfg, cache = FALSE)
  )
  
  if (!nrow(df)) stop("No data returned for scale: ", scale)
  
  keys <- c("species", if (scale != "system") "region")
  assert_unique_rows(df, c(keys, "year"), paste(scale, "data"))
  
  if (anyNA(df[keys]) ||
      !is.numeric(df$count) ||
      any(!is.finite(df$count))) {
    stop("Missing keys or non-finite counts at scale: ", scale)
  }
  
  if (!is.numeric(df$year) ||
      any(!is.finite(df$year)) ||
      any(df$year != floor(df$year))) {
    stop("Invalid years at scale: ", scale)
  }
  
  if (uses_water) {
    missing <- base::setdiff(covars, names(df))
    if (length(missing)) {
      stop(scale, " is missing covariates: ", paste(missing, collapse = ", "))
    }
    
    bad <- vapply(covars, function(v) {
      !is.numeric(df[[v]]) || any(!is.finite(df[[v]]))
    }, logical(1))
    
    if (any(bad)) {
      stop(
        "Non-finite water covariates at scale ", scale, ": ",
        paste(covars[bad], collapse = ", "),
        ". Check regional water coverage before fitting."
      )
    }
  }
  
  shared_data[[scale]] <- df
}

# Limit all scales to the same calendar range for aligned CV windows.
first_year <- max(vapply(shared_data, function(x) min(x$year), numeric(1)))
last_year <- min(vapply(shared_data, function(x) max(x$year), numeric(1)))

if (last_year - first_year + 1L <
    base_config$train_years + FORECAST_HORIZON) {
  stop("Insufficient overlapping years across selected scales.")
}

for (scale in SCALES_TO_RUN) {
  df <- shared_data[[scale]] |>
    dplyr::filter(year >= first_year, year <= last_year)
  
  disk_save_rds(
    df,
    file.path(scale_run_folder, paste0("shared_data_", scale, ".rds"))
  )
}

rm(shared_data)
gc(verbose = FALSE)

log_line("Shared data validated. Calendar range: ", first_year, "-", last_year)

# =============================================================================
# MODEL × SCALE RUNS
# =============================================================================

models_to_test <- list()

for (model in MVGAM_MODELS) {
  models_to_test[[paste0("mvgam_", model)]] <- list(
    framework = "mvgam", model = model
  )
}

for (model in FABLE_MODELS) {
  models_to_test[[paste0("fable_", model)]] <- list(
    framework = "fable", model = model
  )
}

# Override only main.R's bird-data loader during wrapper runs.
# Every pipeline run reads the exact validated scale snapshot.
original_loader <- get_wading_bird_data

snapshot_loader <- local({
  root <- normalizePath(scale_run_folder, winslash = "/", mustWork = TRUE)
  
  function(config, path = ".", cache = TRUE) {
    readRDS(file.path(
      root,
      paste0("shared_data_", config$spatial$level, ".rds")
    ))
  }
})

# main.R re-sources data_functions.R. Replace its loader immediately after
# that source operation, then restore source() after the run.
run_with_snapshot <- function() {
  original_source <- base::source
  
  source_wrapper <- function(file, ...) {
    answer <- original_source(file, ...)
    
    if (is.character(file) && length(file) == 1L &&
        basename(file) == "data_functions.R") {
      assign(
        "get_wading_bird_data", snapshot_loader,
        envir = .GlobalEnv
      )
    }
    
    invisible(answer)
  }
  
  had_source <- exists("source", envir = .GlobalEnv, inherits = FALSE)
  previous_source <- if (had_source) {
    get("source", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  
  assign("source", source_wrapper, envir = .GlobalEnv)
  
  on.exit({
    if (had_source) {
      assign("source", previous_source, envir = .GlobalEnv)
    } else {
      rm(list = "source", envir = .GlobalEnv)
    }
    
    assign("get_wading_bird_data", original_loader, envir = .GlobalEnv)
  }, add = TRUE)
  
  run_forecasting_pipeline()
}

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
    
    # Prevent a failed startup from pointing to a previous successful run.
    for (name in c("run_folder", "results")) {
      if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = name, envir = .GlobalEnv)
      }
    }
    
    set.seed(SEED)
    started <- Sys.time()
    failure <- NA_character_
    
    completed <- tryCatch({
      run_with_snapshot()
      TRUE
    }, error = function(e) {
      failure <<- conditionMessage(e)
      FALSE
    })
    
    source_folder <- if (
      exists("run_folder", envir = .GlobalEnv, inherits = FALSE)
    ) {
      get("run_folder", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NA_character_
    }
    
    destination <- NA_character_
    status <- "failed"
    
    if (completed) {
      metrics_file <- file.path(source_folder, "metrics_only.rds")
      
      if (!file.exists(metrics_file)) {
        failure <- "Pipeline returned without metrics_only.rds."
      } else {
        metrics <- readRDS(metrics_file)[[info$framework]]
        model_col <- if (".model" %in% names(metrics)) ".model" else "model"
        
        if (!is.data.frame(metrics) ||
            !model_col %in% names(metrics) ||
            !info$model %in% metrics[[model_col]]) {
          failure <- paste("No metrics returned for requested model:", info$model)
        } else {
          destination <- file.path(model_folder, scale)
          
          # A persistence failure stops the wrapper; the source stays intact.
          copy_run_files(source_folder, destination)
          
          all_model_results[[model_key]][[scale]] <- destination
          status <- "completed"
        }
      }
    }
    
    if (status == "failed") {
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
    } else {
      log_line("Saved verified copy: ", destination)
      log_line("Original run retained: ", source_folder)
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
# METRIC EXTRACTION
# =============================================================================

metric_parts <- list()

for (model_key in names(all_model_results)) {
  info <- models_to_test[[model_key]]
  
  for (scale in names(all_model_results[[model_key]])) {
    folder <- all_model_results[[model_key]][[scale]]
    
    metrics <- readRDS(file.path(folder, "metrics_only.rds"))[
      [info$framework]
    ]
  }
}