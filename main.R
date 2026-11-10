# =============================================================================
# MAIN.R - Wading Bird Forecasting Pipeline
#
# Requires the rewritten evaluation.R containing:
#   fit_sliding_window()
#   make_mvgam_forecasts()
#   ordinal_thresholds()
#   positive_integer()
#
# Preserves:
#   results$<framework>$forecasts
#   results$<framework>$metrics
#   results$<framework>$window_objects
#
# Separate regional runs are retained under:
#   results$by_region[[region]]
#
# Native mvgam plots:
#   forecasts_native/<region>/<window>/<model>/series_*.png
#
# Intended usage:
#   Rscript main.R
# or:
#   source("main.R") from run_all_scales.R
# =============================================================================

run_forecasting_pipeline <- function() {
  start_time <- Sys.time()
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  banner <- function(text) {
    border <- paste(rep("=",  seventy <- 70L), collapse = "")
    cat("\n", border, "\n", text, "\n", border, "\n", sep = "")
  }
  
  section <- function(text) {
    cat("\n--- ", text, " ---\n", sep = "")
  }
  
  safe_name <- function(x) {
    gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
  }
  
  # Source project functions where existing model lookups can find them.
  source_required <- function(path) {
    if (!file.exists(path)) {
      stop("Required file not found: ", path)
    }
    source(path, local = .GlobalEnv)
  }
  
  banner("WADING BIRD FORECASTING PIPELINE")
  cat("Started:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # ===========================================================================
  # OPTIONAL SYSTEM LIBRARY PRELOAD
  # Preserve the existing HiPerGator workaround, but do not assume these
  # paths exist or that they are appropriate for every R installation.
  # ===========================================================================
  
  if (dir.exists("/apps")) {
    system_libraries <- c(
      "/apps/udunits/2.2.17/lib/libudunits2.so.0",
      "/apps/gcc/12.2.0/gdal/3.9.0/lib/libgdal.so"
    )
    
    loaded_paths <- vapply(
      getLoadedDLLs(),
      function(x) x[["path"]],
      character(1)
    )
    
    for (path in system_libraries) {
      if (file.exists(path) && !path %in% loaded_paths) {
        tryCatch(
          dyn.load(path),
          error = function(e) {
            warning(
              "Optional library preload failed: ", path, "\n",
              conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    }
  }
  
  # ===========================================================================
  # COMMON PACKAGES
  # ===========================================================================
  
  suppressPackageStartupMessages({
    library(units)
    library(sf)
    library(stars)
    library(distributional)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(glue)
    library(tsibble)
    library(wader)
    library(future)
    library(conflicted)
  })
  
  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
  conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
  conflicted::conflicts_prefer(base::get, .quiet = TRUE)
  conflicted::conflicts_prefer(base::as.matrix, .quiet = TRUE)
  
  # ===========================================================================
  # CONFIGURATION
  # ===========================================================================
  
  section("Configuration")
  
  if (exists("CONFIG", envir = .GlobalEnv, inherits = FALSE)) {
    cfg <- get("CONFIG", envir = .GlobalEnv, inherits = FALSE)
    cat("Using CONFIG supplied by the calling script.\n")
  } else {
    cfg <- config::get()
    cat(
      "Loaded configuration profile:",
      Sys.getenv("R_CONFIG_ACTIVE", "default"), "\n"
    )
  }
  
  cfg$spatial <- cfg$spatial %||% list()
  cfg$models <- cfg$models %||% list()
  cfg$cache <- cfg$cache %||% list()
  cfg$plots <- cfg$plots %||% list()
  
  cfg$spatial$level <- cfg$spatial$level %||% "system"
  
  # data_functions.R implements system, rather than all, as its branch.
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
  
  # Defaults: native mvgam window plots enabled.
  # Existing plotting.R is optional and can still generate comparison plots.
  cfg$plots$native_mvgam <- isTRUE(
    cfg$plots$native_mvgam %||% TRUE
  )
  
  cfg$plots$enabled <- isTRUE(cfg$plots$enabled %||% TRUE)
  
  if (isTRUE(cfg$spatial$forecast_totals)) {
    removed <- intersect(
      cfg$models$mvgam,
      c("species_specific", "trait", "trait2")
    )
    
    cfg$models$mvgam <- setdiff(cfg$models$mvgam, removed)
    
    if (length(removed)) {
      cat(
        "Removed species-dependent models for totals:",
        paste(removed, collapse = ", "), "\n"
      )
    }
  }
  
  if (cfg$run_mvgam && !length(cfg$models$mvgam)) {
    stop("run_mvgam is enabled but no mvgam models are selected.")
  }
  
  if (cfg$run_fable && !length(cfg$models$fable)) {
    stop("run_fable is enabled but no fable models are selected.")
  }
  
  if (!cfg$run_mvgam && !cfg$run_fable) {
    stop("Enable at least one model framework.")
  }
  
  # Publish before sourcing evaluation.R.
  assign("CONFIG", cfg, envir = .GlobalEnv)
  
  source_required("data_functions.R")
  source_required("evaluation.R")
  
  required_helpers <- c(
    "positive_integer",
    "ordinal_thresholds",
    "fit_sliding_window",
    "make_mvgam_forecasts"
  )
  
  for (helper in required_helpers) {
    if (!exists(helper, mode = "function", envir = .GlobalEnv)) {
      stop(
        "Missing evaluation helper: ", helper,
        ". Use the rewritten evaluation.R."
      )
    }
  }
  
  cfg$train_years <- positive_integer(
    cfg$train_years %||% 20L, "train_years"
  )
  
  # A supplied forecast_horizon overrides test_years.
  # Both refer to the number of held-out annual forecast steps per CV window.
  horizon <- positive_integer(
    cfg$forecast_horizon %||% cfg$test_years %||% 2L,
    "forecast_horizon"
  )
  
  cfg$forecast_horizon <- horizon
  cfg$test_years <- horizon
  
  if (!is.null(cfg$cv_windows)) {
    cfg$cv_windows <- positive_integer(cfg$cv_windows, "cv_windows")
  }
  
  # This matches the rewritten evaluation.R, whose CV loop is sequential.
  # Do not reset the calling wrapper's future plan here.
  cfg$parallel <- cfg$parallel %||% list()
  
  if (isTRUE(cfg$parallel$enabled)) {
    cat(
      "Note: this evaluation.R fits CV windows sequentially;",
      "outer wrapper parallelism is unchanged.\n"
    )
  }
  
  cfg$parallel$enabled <- FALSE
  
  assign("CONFIG", cfg, envir = .GlobalEnv)
  
  cat("Spatial level:", cfg$spatial$level, "\n")
  cat("Separate regional fits:", isTRUE(cfg$spatial$run_by_region), "\n")
  cat("Training years:", cfg$train_years, "\n")
  cat("Forecast/test years per window:", horizon, "\n")
  cat("CV windows:", cfg$cv_windows %||% "all", "\n")
  cat("Ordinal evaluation:", cfg$use_ordinal, "\n")
  
  # ===========================================================================
  # FRAMEWORK PACKAGES AND MODEL FUNCTIONS
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
    
    # Check only when Stan models are requested.
    # Do not attempt to modify the compiler setup from a batch job.
    cmdstanr::check_cmdstan_toolchain(fix = FALSE)
    
    for (model_name in cfg$models$mvgam) {
      source_required(
        file.path("models", paste0("mvgam_", model_name, ".R"))
      )
      
      function_name <- paste0("fit_mvgam_", model_name)
      
      if (!exists(
        function_name,
        mode = "function",
        envir = .GlobalEnv,
        inherits = FALSE
      )) {
        stop("Model file did not define ", function_name)
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
    
    if (cfg$run_mvgam) {
      conflicted::conflicts_prefer(mvgam::AR, .quiet = TRUE)
      conflicted::conflicts_prefer(mvgam::VAR, .quiet = TRUE)
      conflicted::conflicts_prefer(mvgam::RW, .quiet = TRUE)
    }
  }
  
  # ===========================================================================
  # OUTPUT DIRECTORY
  # ===========================================================================
  
  section("Creating output directory")
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  folder_stem <- paste0(
    "run_", safe_name(cfg$spatial$level), "_",
    safe_name(cfg$model_key %||% "default"), "_", timestamp
  )
  
  run_folder <- file.path("results", folder_stem)
  
  # Avoid overwriting an earlier run launched in the same second.
  suffix <- 0L
  while (dir.exists(run_folder)) {
    suffix <- suffix + 1L
    run_folder <- file.path(
      "results", paste0(folder_stem, "_", suffix)
    )
  }
  
  dir.create(run_folder, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(run_folder)) {
    stop("Could not create output directory: ", run_folder)
  }
  
  assign("run_folder", run_folder, envir = .GlobalEnv)
  
  # Save configuration immediately, even if fitting later fails.
  saveRDS(cfg, file.path(run_folder, "config.rds"))
  
  cat("Run folder:", run_folder, "\n")
  
  # ===========================================================================
  # SERIAL DATA PREPARATION
  # ===========================================================================
  
  section("Preparing shared data")
  
  dir.create("cache", recursive = TRUE, showWarnings = FALSE)
  
  needs_download <- !dir.exists("SiteandMethods")
  
  if (!needs_download) {
    modified <- file.info("SiteandMethods")$mtime
    
    needs_download <- is.na(modified) ||
      as.numeric(difftime(
        Sys.time(), modified, units = "days"
      )) > 7
  }
  
  if (needs_download) {
    wader::download_observations(".")
  } else {
    cat("Using existing observation download.\n")
  }
  
  # Stop on preparation failures rather than claiming all inputs are ready.
  get_data_water()
  
  data <- get_wading_bird_data(
    config = cfg,
    cache = cfg$cache$data %||% TRUE
  )
  
  data <- tibble::as_tibble(data)
  
  if (!nrow(data)) {
    stop("Data preparation returned no observations.")
  }
  
  if (!all(c("species", "year", "count") %in% names(data))) {
    stop("Prepared data must contain species, year, and count.")
  }
  
  # System-level data may carry a constant water-region column.
  # Remove it so system models use species-only keys.
  if (identical(cfg$spatial$level, "system")) {
    data <- data |> dplyr::select(-any_of("region"))
  } else {
    if (!"region" %in% names(data) || anyNA(data$region)) {
      stop("Spatial data must contain non-missing region labels.")
    }
    
    data$region <- as.character(data$region)
  }
  
  data <- data |>
    dplyr::arrange(year, species)
  
  if (anyNA(data$year)) {
    stop("Prepared data contains missing years.")
  }
  
  assign("data", data, envir = .GlobalEnv)
  
  # Save the exact observations used, allowing plotting-only reruns.
  saveRDS(data, file.path(run_folder, "data.rds"))
  
  cat("Years:", min(data$year), "-", max(data$year), "\n")
  cat("Species:", paste(unique(data$species), collapse = ", "), "\n")
  cat("Observations:", nrow(data), "\n")
  
  # ===========================================================================
  # FIXED ORDINAL THRESHOLDS
  # ===========================================================================
  
  precomputed_breaks <- NULL
  
  if (cfg$use_ordinal && !cfg$sliding_window_breaks) {
    section("Computing fixed ordinal thresholds")
    
    warning(
      "Fixed ordinal thresholds are computed from the full dataset. ",
      "They therefore use information from held-out years. ",
      "Use sliding_window_breaks: true for training-only thresholds.",
      call. = FALSE
    )
    
    precomputed_breaks <- ordinal_thresholds(data, cfg)
    
    saveRDS(
      precomputed_breaks,
      file.path(run_folder, "ordinal_breaks.rds")
    )
  }
  
  # ===========================================================================
  # FRAMEWORK RUNNER
  # ===========================================================================
  
  run_models <- function(input_data, framework, breaks = NULL) {
    forecast_function <- switch(
      framework,
      mvgam = make_mvgam_forecasts,
      fable = make_fable_forecasts,
      stop("Unknown framework: ", framework)
    )
    
    model_names <- cfg$models[[framework]]
    
    fit_sliding_window(
      data = input_data,
      make_forecast = forecast_function,
      train_years = cfg$train_years,
      test_years = cfg$test_years,
      cv_windows = cfg$cv_windows,
      parallel = FALSE,
      models_to_run = model_names,
      use_ordinal = cfg$use_ordinal,
      precomputed_breaks = breaks,
      config = cfg
    )
  }
  
  frameworks <- c(
    if (cfg$run_mvgam) "mvgam",
    if (cfg$run_fable) "fable"
  )
  
  # ===========================================================================
  # FIT AND PRESERVE RESULTS
  # ===========================================================================
  
  banner("MODEL FITTING AND FORECASTING")
  
  results <- list()
  run_by_region <- isTRUE(cfg$spatial$run_by_region) &&
    cfg$spatial$level != "system"
  
  if (run_by_region) {
    regions <- sort(unique(data$region))
    region_results <- list()
    
    for (region_name in regions) {
      section(paste("Region:", region_name))
      
      region_data <- data |>
        dplyr::filter(region == region_name)
      
      region_breaks <- precomputed_breaks
      
      if (!is.null(region_breaks) &&
          "region" %in% names(region_breaks)) {
        region_breaks <- region_breaks |>
          dplyr::filter(region == region_name)
      }
      
      region_result <- list()
      
      for (framework in frameworks) {
        region_result[[framework]] <- tryCatch(
          run_models(region_data, framework, region_breaks),
          error = function(e) {
            warning(
              framework, " failed for region ", region_name, ": ",
              conditionMessage(e), call. = FALSE
            )
            
            list(
              forecasts = tibble::tibble(),
              metrics = tibble::tibble(),
              cv_info = NULL,
              window_objects = list(),
              error = conditionMessage(e)
            )
          }
        )
      }
      
      region_results[[region_name]] <- region_result
      
      # Checkpoint completed regions; preserves raw forecast objects.
      saveRDS(
        list(by_region = region_results),
        file.path(run_folder, "regional_checkpoint.rds")
      )
    }
    
    results$by_region <- region_results
    
    for (framework in frameworks) {
      extract_table <- function(region_name, table_name) {
        x <- region_results[[region_name]][[framework]][[table_name]]
        
        if (is.null(x) || !nrow(x)) {
          return(tibble::tibble())
        }
        
        x |>
          dplyr::mutate(region = region_name)
      }
      
      # Flatten window records without combining forecast objects themselves.
      # Each record retains its own model objects and regional metadata.
      combined_windows <- list()
      regional_cv_info <- list()
      
      for (region_name in names(region_results)) {
        regional_result <- region_results[[region_name]][[framework]]
        
        regional_cv_info[region_name] <- list(
          regional_result$cv_info
        )
        
        windows <- regional_result$window_objects %||% list()
        
        for (window_name in names(windows)) {
          record <- windows[[window_name]]
          record$region <- region_name
          
          key <- paste(region_name, window_name, sep = "::")
          combined_windows[[key]] <- record
        }
      }
      
      results[[framework]] <- list(
        forecasts = dplyr::bind_rows(lapply(
          names(region_results), extract_table,
          table_name = "forecasts"
        )),
        metrics = dplyr::bind_rows(lapply(
          names(region_results), extract_table,
          table_name = "metrics"
        )),
        cv_info = list(
          train_years = cfg$train_years,
          test_years = cfg$test_years,
          by_region = regional_cv_info
        ),
        window_objects = combined_windows
      )
    }
  } else {
    section("Fitting all selected series together")
    
    # A joint fit is not automatically a hierarchical model.
    # Hierarchical structure depends on each model's formula.
    for (framework in frameworks) {
      results[[framework]] <- run_models(
        data, framework, precomputed_breaks
      )
    }
  }
  
  # ===========================================================================
  # SAVE BEFORE PLOTTING
  # ===========================================================================
  
  section("Saving results")
  
  results_path <- file.path(run_folder, "forecast_results.rds")
  
  saveRDS(results, results_path)
  assign("results", results, envir = .GlobalEnv)
  
  cat("Saved:", results_path, "\n")
  cat("Raw forecast objects are included in window_objects.\n")
  
  saveRDS(
    list(
      started = start_time,
      saved = Sys.time(),
      session = utils::sessionInfo()
    ),
    file.path(run_folder, "run_metadata.rds")
  )
  
  # ===========================================================================
  # NATIVE MVGAM WINDOW PLOTTING
  # ===========================================================================
  
  save_native_mvgam_plots <- function(window_objects, output_dir) {
    if (!length(window_objects)) {
      message("No raw mvgam window objects available.")
      return(invisible(NULL))
    }
    
    # Resolve the native helper without assuming it is exported.
    ns <- asNamespace("mvgam")
    
    native_plot <- if (exists(
      "plot_mvgam_forecast", envir = ns, inherits = FALSE
    )) {
      get("plot_mvgam_forecast", envir = ns, inherits = FALSE)
    } else {
      NULL
    }
    
    draw_native <- function(fc, series_index) {
      if (is.function(native_plot) &&
          "series" %in% names(formals(native_plot))) {
        native_plot(fc, series = series_index)
      } else {
        # Dispatches to the installed forecast object's plotting method.
        graphics::plot(fc, series = series_index)
      }
    }
    
    save_one <- function(fc, series_index, filename) {
      grDevices::png(
        filename,
        width = 1600,
        height = 900,
        res = 150
      )
      
      device_id <- grDevices::dev.cur()
      
      on.exit({
        devices <- grDevices::dev.list()
        
        if (!is.null(devices) && device_id %in% devices) {
          grDevices::dev.off(which = device_id)
        }
      }, add = TRUE)
      
      draw_native(fc, series_index)
      invisible(filename)
    }
    
    manifest <- list()
    
    for (window_key in names(window_objects)) {
      record <- window_objects[[window_key]]
      
      region_label <- record$region %||% "joint"
      
      if (!length(record$models)) next
      
      train_df <- record$train_data
      series_keys <- c(
        "species",
        intersect("region", names(train_df))
      )
      
      series_table <- train_df |>
        dplyr::distinct(across(all_of(series_keys)))
      
      n_series <- nrow(series_table)
      
      for (model_name in names(record$models)) {
        fc <- record$models[[model_name]]
        if (is.null(fc)) next
        
        destination <- file.path(
          output_dir,
          safe_name(region_label),
          paste0(
            "window_", record$window,
            "_test_", record$test_start, "-", record$test_end
          ),
          safe_name(model_name)
        )
        
        dir.create(
          destination, recursive = TRUE, showWarnings = FALSE
        )
        
        for (series_index in seq_len(n_series)) {
          filename <- file.path(
            destination,
            sprintf("series_%03d.png", series_index)
          )
          
          error_message <- NULL
          
          success <- tryCatch({
            save_one(fc, series_index, filename)
            TRUE
          }, error = function(e) {
            error_message <<- conditionMessage(e)
            
            warning(
              "Native plot failed: ", window_key, " / ",
              model_name, " / series ", series_index, ": ",
              error_message,
              call. = FALSE
            )
            
            FALSE
          })
          
          # Remove incomplete images rather than leaving misleading files.
          if (!success && file.exists(filename)) {
            unlink(filename)
          }
          
          manifest[[length(manifest) + 1L]] <- tibble::tibble(
            window_key = window_key,
            region = as.character(region_label),
            model = model_name,
            series_index = series_index,
            test_start = record$test_start,
            test_end = record$test_end,
            filename = filename,
            success = success,
            error = error_message %||% NA_character_
          )
        }
      }
    }
    
    if (length(manifest)) {
      utils::write.csv(
        dplyr::bind_rows(manifest),
        file.path(output_dir, "plot_manifest.csv"),
        row.names = FALSE
      )
    }
    
    invisible(NULL)
  }
  
  if (cfg$run_mvgam && cfg$plots$native_mvgam) {
    section("Native mvgam plots for every window")
    
    native_dir <- file.path(run_folder, "forecasts_native")
    dir.create(native_dir, recursive = TRUE, showWarnings = FALSE)
    
    save_native_mvgam_plots(
      results$mvgam$window_objects,
      native_dir
    )
  }
  
  # ===========================================================================
  # EXISTING COMPARISON / SKILL PLOTS
  # Native forecasts above do not depend on plotting.R.
  # ===========================================================================
  
  if (cfg$plots$enabled && file.exists("plotting.R")) {
    section("Comparison and skill plots")
    
    tryCatch({
      source_required("plotting.R")
      
      if (!exists(
        "generate_plots", mode = "function", envir = .GlobalEnv
      )) {
        stop("plotting.R does not define generate_plots().")
      }
      
      generate_plots(
        results = results,
        config = cfg,
        data = data,
        results_dir = run_folder
      )
    }, error = function(e) {
      warning(
        "Comparison plotting failed: ", conditionMessage(e),
        ". Saved model results are unaffected.",
        call. = FALSE
      )
    })
  }
  
  # ===========================================================================
  # FINAL SUMMARY AND WRAPPER OUTPUTS
  # ===========================================================================
  
  banner("ANALYSIS COMPLETE")
  
  for (framework in frameworks) {
    x <- results[[framework]]
    
    cat("\nFramework:", framework, "\n")
    cat("Forecast rows:", nrow(x$forecasts), "\n")
    cat("Metric rows:", nrow(x$metrics), "\n")
    cat("Stored window records:", length(x$window_objects), "\n")
    
    # The aggregated regional cv_info has no single n_windows value.
    if (!run_by_region) {
      print_cv_summary(x)
    }
  }
  
  elapsed <- as.numeric(difftime(
    Sys.time(), start_time, units = "mins"
  ))
  
  cat("\nRuntime:", round(elapsed, 1), "minutes\n")
  cat("Results:", run_folder, "\n")
  
  # Required by the current run_all_scales.R sourcing pattern.
  assign("CONFIG", cfg, envir = .GlobalEnv)
  assign("run_folder", run_folder, envir = .GlobalEnv)
  assign("results", results, envir = .GlobalEnv)
  assign("data", data, envir = .GlobalEnv)
  
  invisible(list(
    results = results,
    config = cfg,
    data = data,
    run_folder = run_folder
  ))
}

run_forecasting_pipeline()