# =============================================================================
# TEST_HARNESS.R - Isolated testing of pipeline components
# =============================================================================
library(dplyr)
library(mvgam)
library(glue)
library(future)

conflicted::conflicts_prefer(dplyr::filter,   .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select,   .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::AR,       .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::VAR,      .quiet = TRUE)
conflicted::conflicts_prefer(mvgam::RW,       .quiet = TRUE)
conflicted::conflicts_prefer(base::as.matrix, .quiet = TRUE)
conflicted::conflicts_prefer(base::get,       .quiet = TRUE)

source("data_functions.R")
source("evaluation.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

TEST_CONFIG <- list(
  spatial = list(
    level            = "subregion",
    include_species  = "all",
    include_unknowns = FALSE,
    forecast_totals  = TRUE,
    run_by_region    = FALSE,
    exclude_regions  = c("bigcypress", "coastal")
  ),
  
  # Water data: set remote_url here or leave NULL to use local cache only.
  water = list(
    remote_url = NULL
  ),
  
  chains  = 2L,
  burnin  = 200L,
  samples = 200L,
  
  train_years           = 5L,
  test_years            = 1L,
  cv_windows            = 1L,
  
  use_ordinal           = TRUE,
  ordinal_years         = "All",
  # Three cut-points -> four ordinal categories (below-low / low / mid / high).
  ordinal_breaks        = c(0.33, 0.67, 0.90),
  sliding_window_breaks = FALSE,
  
  family    = "nb",
  data_type = "observed",
  
  parallel = list(enabled = FALSE, workers = 1L),
  cache    = list(data = TRUE, models = FALSE)
)

CONFIG <- TEST_CONFIG
assign("CONFIG", CONFIG, envir = .GlobalEnv)

# =============================================================================
# INTERNAL HELPERS
# =============================================================================

.sep  <- function() cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
.sep2 <- function() cat(paste(rep("-", 50), collapse = ""), "\n", sep = "")

# Create a timestamped output directory and wire it into CONFIG globally.
.make_run_dir <- function(tag) {
  path <- file.path(
    "results",
    paste0("test_", tag, "_", format(Sys.time(), "%Y%m%d-%H%M%S"))
  )
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  CONFIG$output_dir <<- path
  assign("CONFIG",     CONFIG, envir = .GlobalEnv)
  assign("run_folder", path,   envir = .GlobalEnv)
  cat("Output directory:", path, "\n\n")
  invisible(path)
}

# Source model files for a given framework.
# baseline is always loaded for mvgam so skill scores can be computed.
.source_models <- function(framework, model_names) {
  if (framework == "mvgam") {
    files_to_load <- unique(c("baseline", model_names))
    for (m in files_to_load) {
      f <- file.path("models", paste0("mvgam_", m, ".R"))
      if (!file.exists(f)) {
        if (m == "baseline") stop("Required file not found: ", f)
        warning("Model file not found, skipping: ", f, call. = FALSE)
        next
      }
      source(f)
      cat(glue("✓ Loaded {f}\n"))
    }
  } else if (framework == "fable") {
    f <- file.path("models", "fable_models.R")
    if (!file.exists(f)) stop("Required file not found: ", f)
    source(f)
    cat(glue("✓ Loaded {f}\n"))
  } else {
    stop("Unknown framework: ", framework)
  }
  cat("\n")
}

# =============================================================================
# RESULT VALIDATION
# =============================================================================

validate_test_results <- function(results, model_names) {
  issues <- character()
  
  if (!is.data.frame(results$forecasts) || !nrow(results$forecasts)) {
    issues <- c(issues, "No forecast rows returned")
  }
  
  if (!is.data.frame(results$metrics) || !nrow(results$metrics)) {
    issues <- c(issues, "No metric rows returned")
  } else {
    found   <- unique(results$metrics$model)
    missing <- setdiff(model_names, found)
    if (length(missing)) {
      issues <- c(issues, paste(
        "No metrics for:", paste(missing, collapse = ", ")
      ))
    }
    
    if ("crps" %in% names(results$metrics)) {
      n_exploding <- sum(
        is.finite(results$metrics$crps) & results$metrics$crps > 1e6,
        na.rm = TRUE
      )
      if (n_exploding > 0L) {
        issues <- c(issues, sprintf("%d row(s) with CRPS > 1e6", n_exploding))
      }
      
      n_na <- sum(is.na(results$metrics$crps))
      if (n_na > 0L) {
        issues <- c(issues, sprintf("%d row(s) with NA CRPS", n_na))
      }
    }
  }
  
  all_errors <- unlist(lapply(results$window_objects, `[[`, "model_errors"))
  all_errors <- all_errors[nzchar(all_errors)]
  if (length(all_errors)) {
    issues <- c(issues, paste(
      "Recorded model errors:", paste(all_errors, collapse = "; ")
    ))
  }
  
  if (length(issues)) {
    cat("\n⚠  Validation issues:\n")
    for (iss in issues) cat("  -", iss, "\n")
  } else {
    cat("\n✓ All validation checks passed\n")
  }
  
  invisible(issues)
}

# =============================================================================
# UNIT TESTS
# =============================================================================

#' Check the water covariate join produces no unexpected NAs.
test_water_join <- function() {
  .sep()
  cat("TEST: Water data join\n")
  .sep()
  
  data <- get_wading_bird_data(config = CONFIG, cache = FALSE)
  df   <- tibble::as_tibble(data)
  
  water_cols <- intersect(
    c("breed_season_depth", "recession", "dry_days"),
    names(df)
  )
  
  if (!length(water_cols)) {
    cat("⚠  No recognised water covariate columns found.\n")
    cat("   Available columns:", paste(names(df), collapse = ", "), "\n")
    return(invisible(NULL))
  }
  
  all_ok <- TRUE
  for (col in water_cols) {
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0L) {
      all_ok <- FALSE
      cat(sprintf("  ⚠  %s: %d NA value(s)\n", col, n_na))
      df |>
        dplyr::filter(is.na(.data[[col]])) |>
        dplyr::distinct(year, dplyr::across(dplyr::any_of("region"))) |>
        print()
    } else {
      cat(sprintf("  ✓  %s: no NAs\n", col))
    }
  }
  
  if (all_ok) cat("\n✓ Water join clean\n")
  invisible(df)
}

#' Verify prepare_mvgam_data round-trips on the most recent window.
test_data_preparation <- function() {
  .sep()
  cat("TEST: prepare_mvgam_data round-trip\n")
  .sep()
  
  data  <- tibble::as_tibble(get_wading_bird_data(config = CONFIG, cache = TRUE))
  years <- sort(unique(data$year))
  n     <- length(years)
  
  if (n < 2L) stop("Need at least 2 years of data for this test.")
  
  train <- dplyr::filter(data, year <= years[n - 1L])
  test  <- dplyr::filter(data, year == years[n])
  
  prepared <- tryCatch(
    prepare_mvgam_data(train, test),
    error = function(e) {
      cat("  ✗ prepare_mvgam_data failed:", conditionMessage(e), "\n")
      NULL
    }
  )
  
  if (is.null(prepared)) return(invisible(NULL))
  
  cat("Series    :", paste(levels(prepared$train$series), collapse = ", "), "\n")
  cat("Train rows:", nrow(prepared$train),
      "| time range:", paste(range(prepared$train$time), collapse = "-"), "\n")
  cat("Test rows :", nrow(prepared$test),
      "| time range:", paste(range(prepared$test$time),  collapse = "-"), "\n")
  
  covar_cols <- intersect(
    c("breed_season_depth", "recession", "dry_days"),
    names(prepared$train)
  )
  
  for (col in covar_cols) {
    n_tr <- sum(is.na(prepared$train[[col]]))
    n_te <- sum(is.na(prepared$test[[col]]))
    icon <- if (n_tr == 0L && n_te == 0L) "✓" else "⚠"
    cat(sprintf("  %s %s — train NA: %d, test NA: %d\n", icon, col, n_tr, n_te))
  }
  
  invisible(prepared)
}

#' Load a saved forecast .rds and score it in isolation.
#' Useful for diagnosing CRPS failures on completed windows without re-fitting.
#'
#' @param path       Path to a saved forecast.rds
#' @param model_name Label to use in output
test_scoring <- function(path, model_name = "test") {
  .sep()
  cat(glue("TEST: Scoring — {basename(path)}\n"))
  .sep()
  
  if (!file.exists(path)) stop("File not found: ", path)
  
  fc     <- readRDS(path)
  scores <- extract_crps_mvgam(fc, model_name)
  
  cat("Score rows :", nrow(scores), "\n")
  cat("Non-finite :", sum(!is.finite(scores$score)), "\n")
  
  if (nrow(scores)) {
    rng <- range(scores$score, na.rm = TRUE)
    cat(sprintf("Score range: %.4f – %.4f\n", rng[1], rng[2]))
  }
  
  .sep2()
  print(scores)
  invisible(scores)
}

#' Test data loading at all three spatial levels.
test_data_loading <- function() {
  .sep()
  cat("TEST: Data loading across spatial levels\n")
  .sep()
  
  level_configs <- list(
    system    = list(level = "system",    include_species = "top6"),
    subregion = list(level = "subregion", include_species = "top6"),
    colony    = list(level = "colony",    include_species = c("gbhe", "greg"))
  )
  
  for (name in names(level_configs)) {
    .sep2()
    cat(glue("Level: {name}\n"))
    
    cfg         <- CONFIG
    cfg$spatial <- modifyList(CONFIG$spatial, level_configs[[name]])
    
    result <- tryCatch(
      get_wading_bird_data(config = cfg, cache = FALSE),
      error = function(e) {
        cat(glue("  ✗ Failed: {conditionMessage(e)}\n"))
        NULL
      }
    )
    
    if (!is.null(result)) {
      df <- tibble::as_tibble(result)
      cat(glue("  Rows   : {nrow(df)}\n"))
      cat(glue("  Species: {paste(unique(df$species), collapse = ', ')}\n"))
      if ("region" %in% names(df)) {
        cat(glue("  Regions: {paste(unique(df$region), collapse = ', ')}\n"))
      }
      cat(glue("  Years  : {min(df$year)}-{max(df$year)}\n"))
    }
  }
}

# =============================================================================
# MAIN TEST FUNCTIONS
# =============================================================================

#' Run one or more models through the full sliding-window CV pipeline.
#'
#' @param model_names  Character vector of model names e.g. c("baseline", "ar")
#' @param framework    "mvgam" or "fable"
#' @param use_full_data If FALSE, subsets to the most recent 10 years
test_single_model <- function(model_names,
                              framework     = "mvgam",
                              use_full_data = FALSE) {
  .sep()
  cat(glue(
    "TEST: {framework} | models: {paste(model_names, collapse = ', ')}\n"
  ))
  .sep()
  
  .make_run_dir(paste0(framework, "_", paste(model_names, collapse = "_")))
  .source_models(framework, model_names)
  
  cat("Loading data...\n")
  data <- tibble::as_tibble(get_wading_bird_data(config = CONFIG, cache = TRUE))
  
  if (!use_full_data) {
    min_year <- max(data$year) - 10L + 1L
    data     <- dplyr::filter(data, year >= min_year)
    cat(glue("Subsetting to {min_year}-{max(data$year)}\n"))
  }
  
  cat(glue(
    "Data: {nrow(data)} rows | ",
    "{dplyr::n_distinct(data$species)} species | ",
    "years {min(data$year)}-{max(data$year)}\n\n"
  ))
  
  make_fn <- switch(
    framework,
    mvgam = make_mvgam_forecasts,
    fable = make_fable_forecasts,
    stop("Unknown framework: ", framework)
  )
  
  t0 <- proc.time()
  
  results <- fit_sliding_window(
    data          = data,
    make_forecast = make_fn,
    train_years   = CONFIG$train_years,
    test_years    = CONFIG$test_years,
    cv_windows    = CONFIG$cv_windows,
    parallel      = FALSE,
    framework     = framework,
    config        = CONFIG,
    models_to_run = model_names,
    use_ordinal   = CONFIG$use_ordinal
  )
  
  elapsed <- (proc.time() - t0)[["elapsed"]]
  
  .sep()
  cat(sprintf("Elapsed  : %.1f seconds\n\n", elapsed))
  cat("Forecasts:", nrow(results$forecasts), "rows\n")
  cat("Metrics  :", nrow(results$metrics),   "rows\n\n")
  
  if (nrow(results$metrics)) print(results$metrics)
  
  validate_test_results(results, model_names)
  
  invisible(results)
}

#' Fit models on a manually specified train/test split (no sliding window).
#' Raw data is passed directly; prepare_mvgam_data handles all internal
#' time/series construction.
#'
#' @param model_names  Character vector of model names
#' @param train_start  First year of training data
#' @param test_start   Test year (single year only)
#' @param framework    "mvgam" only for now
test_single_window <- function(model_names,
                               train_start = 2010L,
                               test_start  = 2015L,
                               framework   = "mvgam") {
  .sep()
  cat(glue(
    "TEST: Single window | ",
    "train {train_start}-{test_start - 1} → test {test_start}\n"
  ))
  .sep()
  
  .make_run_dir(paste0("window_", test_start))
  .source_models(framework, model_names)
  
  data <- tibble::as_tibble(get_wading_bird_data(config = CONFIG, cache = TRUE))
  
  train <- dplyr::filter(data, year >= train_start, year < test_start)
  test  <- dplyr::filter(data, year == test_start)
  
  if (!nrow(train)) stop("Empty training set for train_start = ", train_start)
  if (!nrow(test))  stop("Empty test set for test_start = ", test_start)
  
  cat(glue("Train: {nrow(train)} rows ({min(train$year)}-{max(train$year)})\n"))
  cat(glue("Test : {nrow(test)}  rows (year {test_start})\n\n"))
  
  # Wire a mock window directory so make_mvgam_forecasts does not error.
  mock_dir                    <- file.path(CONFIG$output_dir, "window")
  CONFIG$.window_dir          <- mock_dir
  CONFIG$.window_relative_dir <- "window"
  dir.create(mock_dir, recursive = TRUE, showWarnings = FALSE)
  assign("CONFIG", CONFIG, envir = .GlobalEnv)
  
  t0 <- proc.time()
  
  result <- make_mvgam_forecasts(
    train_data    = train,
    test_data     = test,
    models_to_run = model_names,
    use_ordinal   = FALSE,
    config        = CONFIG
  )
  
  elapsed <- (proc.time() - t0)[["elapsed"]]
  
  .sep()
  cat(sprintf("Elapsed: %.1f seconds\n\n", elapsed))
  
  if (is.data.frame(result$metrics) && nrow(result$metrics)) {
    print(result$metrics)
  }
  
  validate_test_results(result, model_names)
  
  invisible(result)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================
# test_data_loading()
# test_water_join()
# test_data_preparation()
# test_single_model("baseline")
# test_single_model(c("baseline", "ar"))
# test_single_window(c("baseline", "ar"), train_start = 2010, test_start = 2015)
# test_scoring("results/test_.../windows/.../mvgam/baseline/forecast.rds")