# =============================================================================
# MAIN.R - Memory-efficient wading bird forecasting pipeline
#
# Requires the disk-backed evaluation.R from this conversation.
#
# Workflow:
#   configuration -> data -> CV windows -> per-model disk persistence/plots
#                 -> small combined results -> metrics-only file -> skill plots
#
# Raw forecasts and window inputs:
#   results/<run>/windows/<location>_<test_start_year>/...
#
# Wrapper-compatible global outputs:
#   CONFIG, run_folder, results, data
# =============================================================================

run_forecasting_pipeline <- function() {
  start_time <- Sys.time()
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  banner <- function(text) {
    border <- paste(rep("=", 70L), collapse = "")
    cat("\n", border, "\n", text, "\n", border, "\n", sep = "")
  }
  
  section <- function(text) {
    cat("\n--- ", text, " ---\n", sep = "")
  }
  
  safe_name <- function(x) {
    gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
  }
  
  source_required <- function(path) {
    if (!file.exists(path)) {
      stop("Required script not found: ", path)
    }
    source(path, local = .GlobalEnv)
  }
  
  banner("WADING BIRD FORECASTING PIPELINE")
  cat("Started:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # ===========================================================================
  # CONFIGURATION
  # ===========================================================================
  
  if (exists("CONFIG", envir = .GlobalEnv, inherits = FALSE)) {
    cfg <- get("CONFIG", envir = .GlobalEnv, inherits = FALSE)
    cat("Using configuration supplied by the calling script.\n")
  } else {
    cfg <- config::get()
    cat(
      "Loaded profile:",
      Sys.getenv("R_CONFIG_ACTIVE", "default"), "\n"
    )
  }
  
  cfg$spatial <- cfg$spatial %||% list()
  cfg$models <- cfg$models %||% list()
  cfg$cache <- cfg$cache %||% list()
  cfg$plots <- cfg$plots %||% list()
  cfg$parallel <- cfg$parallel %||% list()
  
  cfg$spatial$level <- cfg$spatial$level %||% "system"
  
  # data_functions.R uses "system"; wader translation happens there.
  if (identical(cfg$spatial$level, "all")) {
    cfg$spatial$level <- "system"
  }
  
  if (!cfg$spatial$level %in% c("system", "subregion", "colony")) {
    stop("spatial.level must be system, subregion, or colony.")
  }
  
  cfg$models$mvgam <- unique(as.character(
    unlist(cfg$models$mvgam %||% character())
  ))
  
  cfg$models$fable <- unique(as.character(
    unlist(cfg$models$fable %||% character())
  ))
  
  cfg$run_mvgam <- isTRUE(
    cfg$run_mvgam %||% (length(cfg$models$mvgam) > 0L)
  )
  
  cfg$run_fable <- isTRUE(
    cfg$run_fable %||% (length(cfg$models$fable) > 0L)
  )
  
  cfg$use_ordinal <- isTRUE(cfg$use_ordinal %||% FALSE)
  
  cfg$sliding_window_breaks <- isTRUE(
    cfg$sliding_window_breaks %||% TRUE
  )
  
  cfg$ordinal_years <- cfg$ordinal_years %||% "All"
  
  cfg$ordinal_breaks <- unlist(
    cfg$ordinal_breaks %||% c(0.33, 0.50, 0.67)
  )
  
  cfg$plots$enabled <- isTRUE(cfg$plots$enabled %||% TRUE)
  
  cfg$plots$native_mvgam <- isTRUE(
    cfg$plots$native_mvgam %||% TRUE
  )
  
  cfg$plots$native_fable <- isTRUE(
    cfg$plots$native_fable %||% TRUE
  )
  
  if (isTRUE(cfg$spatial$forecast_totals)) {
    removed <- intersect(
      cfg$models$mvgam,
      c("species_specific", "trait", "trait2")
    )
    
    cfg$models$mvgam <- setdiff(cfg$models$mvgam, removed)
    
    if (length(removed)) {
      cat(
        "Removed models incompatible with totals:",
        paste(removed, collapse = ", "), "\n"
      )
    }
  }
  
  if (cfg$run_mvgam && !length(cfg$models$mvgam)) {
    stop("run_mvgam is enabled, but no mvgam models are selected.")
  }
  
  if (cfg$run_fable && !length(cfg$models$fable)) {
    stop("run_fable is enabled, but no fable models are selected.")
  }
  
  if (!cfg$run_mvgam && !cfg$run_fable) {
    stop("Enable at least one forecasting framework.")
  }
  
  # ===========================================================================
  # OPTIONAL HIPERGATOR LIBRARY PRELOAD
  #
  # Retains the previous workaround. Paths may differ between installations.
  # Set system_library_preload: false to disable it.
  # ===========================================================================
  
  if (dir.exists("/apps") &&
      isTRUE(cfg$system_library_preload %||% TRUE)) {
    paths <- c(
      "/apps/udunits/2.2.17/lib/libudunits2.so.0",
      "/apps/gcc/12.2.0/gdal/3.9.0/lib/libgdal.so"
    )
    
    loaded <- vapply(
      getLoadedDLLs(),
      function(x) x[["path"]],
      character(1)
    )
    
    for (path in paths) {
      if (file.exists(path) && !path %in% loaded) {
        tryCatch(
          dyn.load(path),
          error = function(e) {
            warning(
              "Optional library preload failed: ",
              conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    }
  }
  
  # ===========================================================================
  # LIBRARIES AND PROJECT HELPERS
  # ===========================================================================
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(tsibble)
    library(ggplot2)
    library(glue)
    library(distributional)
    library(conflicted)
  })
  
  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
  conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
  conflicted::conflicts_prefer(base::get, .quiet = TRUE)
  conflicted::conflicts_prefer(base::as.matrix, .quiet = TRUE)
  
  assign("CONFIG", cfg, envir = .GlobalEnv)
  
  source_required("data_functions.R")
  source_required("evaluation.R")
  
  required_helpers <- c(
    "positive_integer",
    "ordinal_thresholds",
    "fit_sliding_window",
    "disk_save_rds",
    "save_native_mvgam_object",
    "load_window_forecast"
  )
  
  for (name in required_helpers) {
    if (!exists(
      name, mode = "function",
      envir = .GlobalEnv, inherits = FALSE
    )) {
      stop(
        "Missing helper: ", name,
        ". Use the latest disk-backed evaluation.R."
      )
    }
  }
  
  cfg$train_years <- positive_integer(
    cfg$train_years %||% 20L,
    "train_years"
  )
  
  # One annual horizon for all CV windows and selected frameworks.
  # An explicit forecast_horizon overrides test_years.
  cfg$forecast_horizon <- positive_integer(
    cfg$forecast_horizon %||% cfg$test_years %||% 2L,
    "forecast_horizon"
  )
  
  cfg$test_years <- cfg$forecast_horizon
  
  if (!is.null(cfg$cv_windows)) {
    cfg$cv_windows <- positive_integer(cfg$cv_windows, "cv_windows")
  }
  
  if (isTRUE(cfg$parallel$enabled)) {
    cat(
      "CV windows will run sequentially to limit memory.",
      "Outer wrapper parallelism is unchanged.\n"
    )
  }
  
  cfg$parallel$enabled <- FALSE
  
  # ===========================================================================
  # MODEL PACKAGES AND FUNCTIONS
  # ===========================================================================
  
  section("Loading model functions")
  
  if (cfg$run_mvgam) {
    suppressPackageStartupMessages({
      library(mvgam)
      library(cmdstanr)
    })
    
    conflicted::conflicts_prefer(mvgam::AR, .quiet = TRUE)
    conflicted::conflicts_prefer(mvgam::VAR, .quiet = TRUE)
    conflicted::conflicts_prefer(mvgam::RW, .quiet = TRUE)
    
    cmdstanr::check_cmdstan_toolchain(fix = FALSE)
    
    for (model_name in cfg$models$mvgam) {
      source_required(file.path(
        "models",
        paste0("mvgam_", model_name, ".R")
      ))
      
      function_name <- paste0("fit_mvgam_", model_name)
      
      if (!exists(
        function_name,
        mode = "function",
        envir = .GlobalEnv,
        inherits = FALSE
      )) {
        stop("Model script did not define ", function_name)
      }
    }
  }
  
  if (cfg$run_fable) {
    suppressPackageStartupMessages({
      library(fable)
      library(fabletools)
      library(fable.gam)
      library(feasts)
    })
    
    source_required("models/fable_models.R")
    
    if (!exists(
      "make_fable_forecasts",
      mode = "function",
      envir = .GlobalEnv
    )) {
      stop("Missing make_fable_forecasts().")
    }
  }
  
  if (cfg$run_mvgam) {
    conflicted::conflicts_prefer(mvgam::AR, .quiet = TRUE)
    conflicted::conflicts_prefer(mvgam::VAR, .quiet = TRUE)
    conflicted::conflicts_prefer(mvgam::RW, .quiet = TRUE)
  }
  
  # ===========================================================================
  # CREATE RUN DIRECTORY
  # ===========================================================================
  
  section("Creating output directory")
  
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  stem <- paste0(
    "run_", safe_name(cfg$spatial$level), "_",
    safe_name(cfg$model_key %||% "default"), "_", stamp
  )
  
  run_folder <- file.path("results", stem)
  suffix <- 0L
  
  while (dir.exists(run_folder)) {
    suffix <- suffix + 1L
    run_folder <- file.path("results", paste0(stem, "_", suffix))
  }
  
  disk_create_dir(run_folder)
  
  # Required by disk-backed fit_sliding_window().
  cfg$output_dir <- run_folder
  
  assign("CONFIG", cfg, envir = .GlobalEnv)
  assign("run_folder", run_folder, envir = .GlobalEnv)
  
  disk_save_rds(cfg, file.path(run_folder, "config.rds"))
  
  cat("Run directory:", run_folder, "\n")
  cat("Spatial level:", cfg$spatial$level, "\n")
  cat("Training years:", cfg$train_years, "\n")
  cat("Forecast/test years:", cfg$test_years, "\n")
  cat("CV windows:", cfg$cv_windows %||% "all", "\n")
  cat("Ordinal evaluation:", cfg$use_ordinal, "\n")
  
  # ===========================================================================
  # DATA PREPARATION
  # ===========================================================================
  
  section("Preparing data")
  
  disk_create_dir("cache")
  
  # Default: download observations only when missing.
  # Explicitly set data_update.birds: true to force a refresh.
  updates <- cfg$data_update %||% list()
  
  update_birds <- isTRUE(updates$birds %||% FALSE) ||
    !dir.exists("SiteandMethods")
  
  if (update_birds) {
    cat("Downloading bird observations...\n")
    wader::download_observations(".")
    
    # Invalidate prepared bird caches after refreshing source observations.
    stale <- list.files(
      "cache",
      pattern = "^data_.*\\.rds$",
      full.names = TRUE
    )
    
    if (length(stale)) unlink(stale)
  } else {
    cat("Using existing bird observations.\n")
  }
  
  # This call supports the original get_data_water() signature.
  # If your version requires remote_url, supply it in config.
  water_args <- list(
    update = isTRUE(updates$water %||% FALSE)
  )
  
  if (!is.null(updates$water_remote_url)) {
    water_args$remote_url <- updates$water_remote_url
  }
  
  water <- do.call(get_data_water, water_args)
  rm(water)
  
  # Force rebuilding prepared data after an explicit water refresh.
  if (isTRUE(updates$water)) {
    stale <- list.files(
      "cache",
      pattern = "^data_.*\\.rds$",
      full.names = TRUE
    )
    if (length(stale)) unlink(stale)
  }
  
  observations <- get_wading_bird_data(
    config = cfg,
    cache = cfg$cache$data %||% TRUE
  ) |>
    tibble::as_tibble()
  
  if (!nrow(observations)) {
    stop("Data preparation returned no observations.")
  }
  
  required_columns <- c("year", "species", "count")
  
  if (!all(required_columns %in% names(observations))) {
    stop("Prepared data must contain year, species, and count.")
  }
  
  if (cfg$spatial$level == "system") {
    observations <- observations |>
      dplyr::select(-any_of("region"))
  } else {
    if (!"region" %in% names(observations) ||
        anyNA(observations$region)) {
      stop("Spatial data requires non-missing region labels.")
    }
    
    observations$region <- as.character(observations$region)
  }
  
  observations <- observations |>
    dplyr::arrange(year, species)
  
  disk_save_rds(
    observations,
    file.path(run_folder, "data.rds")
  )
  
  cat(
    "Years:", min(observations$year), "-",
    max(observations$year), "\n"
  )
  cat("Observations:", nrow(observations), "\n")
  cat(
    "Species:",
    paste(unique(observations$species), collapse = ", "), "\n"
  )
  
  # ===========================================================================
  # ORDINAL THRESHOLDS
  # ===========================================================================
  
  precomputed_breaks <- NULL
  
  if (cfg$use_ordinal && !cfg$sliding_window_breaks) {
    warning(
      "Fixed ordinal thresholds use the full dataset, including held-out years. ",
      "Use sliding_window_breaks: true for training-only thresholds.",
      call. = FALSE
    )
    
    precomputed_breaks <- ordinal_thresholds(observations, cfg)
    
    disk_save_rds(
      precomputed_breaks,
      file.path(run_folder, "ordinal_breaks.rds")
    )
  }
  
  # ===========================================================================
  # FRAMEWORK RUNNER
  # ===========================================================================
  
  frameworks <- c(
    if (cfg$run_mvgam) "mvgam",
    if (cfg$run_fable) "fable"
  )
  
  run_models <- function(input_data, framework, breaks = NULL) {
    forecast_function <- switch(
      framework,
      mvgam = make_mvgam_forecasts,
      fable = make_fable_forecasts,
      stop("Unknown framework: ", framework)
    )
    
    fit_sliding_window(
      data = input_data,
      make_forecast = forecast_function,
      train_years = cfg$train_years,
      test_years = cfg$test_years,
      cv_windows = cfg$cv_windows,
      parallel = FALSE,
      framework = framework,
      models_to_run = cfg$models[[framework]],
      use_ordinal = cfg$use_ordinal,
      precomputed_breaks = breaks,
      config = cfg
    )
  }
  
  # Only small predictions, metrics, and file paths are stored here.
  results <- list()
  
  run_by_region <- isTRUE(cfg$spatial$run_by_region) &&
    cfg$spatial$level != "system"
  
  banner("MODEL FITTING AND FORECASTING")
  
  # ===========================================================================
  # SEPARATE REGIONAL FITS
  # ===========================================================================
  
  if (run_by_region) {
    regions <- sort(unique(observations$region))
    regional_results <- list()
    
    for (region_name in regions) {
      section(paste("Region:", region_name))
      
      region_data <- observations |>
        dplyr::filter(region == region_name)
      
      region_breaks <- precomputed_breaks
      
      if (!is.null(region_breaks) &&
          "region" %in% names(region_breaks)) {
        region_breaks <- region_breaks |>
          dplyr::filter(region == region_name)
      }
      
      regional_result <- list()
      
      for (framework in frameworks) {
        regional_result[[framework]] <- tryCatch(
          run_models(region_data, framework, region_breaks),
          error = function(e) {
            # Disk failures must stop the run, not masquerade as model failures.
            if (inherits(e, "forecast_disk_error")) stop(e)
            
            warning(
              framework, " failed for ", region_name, ": ",
              conditionMessage(e),
              call. = FALSE
            )
            
            list(
              forecasts = tibble(),
              metrics = tibble(),
              cv_info = NULL,
              window_objects = list(),
              error = conditionMessage(e)
            )
          }
        )
      }
      
      regional_results[[region_name]] <- regional_result
      
      # This checkpoint contains no raw posterior arrays.
      disk_save_rds(
        list(by_region = regional_results),
        file.path(run_folder, "regional_checkpoint.rds")
      )
      
      rm(region_data, region_breaks, regional_result)
      gc(verbose = FALSE)
    }
    
    results$by_region <- regional_results
    
    for (framework in frameworks) {
      get_table <- function(region_name, table_name) {
        x <- regional_results[[region_name]][[framework]][[table_name]]
        
        if (is.null(x) || !nrow(x)) return(tibble())
        
        x |>
          dplyr::mutate(region = region_name)
      }
      
      windows <- list()
      regional_info <- list()
      
      for (region_name in names(regional_results)) {
        x <- regional_results[[region_name]][[framework]]
        
        regional_info[region_name] <- list(x$cv_info)
        
        for (window_name in names(x$window_objects %||% list())) {
          record <- x$window_objects[[window_name]]
          record$region <- region_name
          
          windows[[paste(region_name, window_name, sep = "::")]] <- record
        }
      }
      
      results[[framework]] <- list(
        forecasts = dplyr::bind_rows(lapply(
          names(regional_results),
          get_table,
          table_name = "forecasts"
        )),
        metrics = dplyr::bind_rows(lapply(
          names(regional_results),
          get_table,
          table_name = "metrics"
        )),
        cv_info = list(
          train_years = cfg$train_years,
          test_years = cfg$test_years,
          by_region = regional_info
        ),
        window_objects = windows
      )
    }
    
    rm(regional_results)
    gc(verbose = FALSE)
    
    # ===========================================================================
    # JOINT SERIES FITS
    # ===========================================================================
    
  } else {
    section("Fitting selected series together")
    
    for (framework in frameworks) {
      results[[framework]] <- run_models(
        observations,
        framework,
        precomputed_breaks
      )
      
      # Save completed frameworks without waiting for the next framework.
      disk_save_rds(
        results,
        file.path(run_folder, "forecast_results.rds")
      )
      
      gc(verbose = FALSE)
    }
  }
  
  # ===========================================================================
  # SAVE SMALL COMBINED RESULTS
  # ===========================================================================
  
  section("Saving combined results")
  
  disk_save_rds(
    results,
    file.path(run_folder, "forecast_results.rds")
  )
  
  metrics_only <- lapply(
    results[intersect(frameworks, names(results))],
    function(x) x$metrics
  )
  
  disk_save_rds(
    metrics_only,
    file.path(run_folder, "metrics_only.rds")
  )
  
  disk_save_rds(
    list(
      started = start_time,
      saved = Sys.time(),
      session = utils::sessionInfo()
    ),
    file.path(run_folder, "run_metadata.rds")
  )
  
  rm(metrics_only)
  gc(verbose = FALSE)
  
  # Publish lightweight outputs before plotting.
  assign("CONFIG", cfg, envir = .GlobalEnv)
  assign("run_folder", run_folder, envir = .GlobalEnv)
  assign("results", results, envir = .GlobalEnv)
  assign("data", observations, envir = .GlobalEnv)
  
  total_forecasts <- sum(vapply(
    frameworks,
    function(framework) nrow(results[[framework]]$forecasts),
    integer(1)
  ))
  
  if (total_forecasts == 0L) {
    stop(
      "No forecast rows were produced. ",
      "Window files and failure metadata were saved to: ",
      run_folder
    )
  }
  
  # ===========================================================================
  # COMPARISON AND SKILL PLOTS ONLY
  #
  # Native forecasts were already plotted and saved during evaluation.
  # Disable end-of-run native plotting to avoid loading posterior arrays again.
  # ===========================================================================
  
  if (cfg$plots$enabled && file.exists("plotting.R")) {
    section("Generating skill and comparison plots")
    
    tryCatch({
      source_required("plotting.R")
      
      if (!exists(
        "generate_plots",
        mode = "function",
        envir = .GlobalEnv
      )) {
        stop("plotting.R does not define generate_plots().")
      }
      
      comparison_config <- cfg
      comparison_config$plots$native_mvgam <- FALSE
      comparison_config$plots$native_fable <- FALSE
      
      generate_plots(
        results = results,
        config = comparison_config,
        data = observations,
        results_dir = run_folder
      )
    }, error = function(e) {
      warning(
        "Comparison plotting failed: ", conditionMessage(e),
        ". Saved forecasts and metrics are unaffected.",
        call. = FALSE
      )
    })
  }
  
  # ===========================================================================
  # FINAL SUMMARY
  # ===========================================================================
  
  banner("ANALYSIS COMPLETE")
  
  for (framework in frameworks) {
    x <- results[[framework]]
    
    cat("\nFramework:", framework, "\n")
    cat("Forecast rows:", nrow(x$forecasts), "\n")
    cat("Metric rows:", nrow(x$metrics), "\n")
    cat("Disk-backed window records:", length(x$window_objects), "\n")
    
    if (!run_by_region) print_cv_summary(x)
  }
  
  elapsed <- as.numeric(difftime(
    Sys.time(), start_time, units = "mins"
  ))
  
  cat("\nRuntime:", round(elapsed, 1), "minutes\n")
  cat("Results directory:", run_folder, "\n")
  cat("Raw forecasts: windows/<location>_<year>/<framework>/<model>/\n")
  cat("Cross-scale metrics: metrics_only.rds\n")
  
  # Do not reset the wrapper's future plan here.
  gc(verbose = FALSE)
  
  invisible(list(run_folder = run_folder))
}

run_forecasting_pipeline()