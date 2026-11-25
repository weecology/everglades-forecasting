# =============================================================================
# data_functions.R
# =============================================================================
library(digest)
library(edenR)
library(dplyr)
library(readr)
library(tidyr)
library(tsibble)
library(wader)
library(zoo)

`%||%` <- function(x, y) if (is.null(x)) y else x

translate_level_for_wader <- function(level) {
  if (level == "system") return("all")
  return(level)
}

# =============================================================================
# WATER DATA
# =============================================================================

get_data_water <- function(
    eden_path  = "WaterData",
    remote_url = NULL,        # NULL = skip remote check, use cache
    update     = FALSE
) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Install httr2 first: install.packages('httr2')")
  }
  
  dir.create(eden_path, recursive = TRUE, showWarnings = FALSE)
  
  water_file    <- file.path(eden_path, "eden_covariates.csv")
  metadata_file <- file.path(eden_path, "eden_remote_metadata.rds")
  
  # ------------------------------------------------------------------
  # Remote metadata check (skipped when remote_url is NULL)
  # ------------------------------------------------------------------
  remote <- if (!is.null(remote_url)) {
    tryCatch({
      response <- httr2::request(remote_url) |>
        httr2::req_method("HEAD") |>
        httr2::req_timeout(30) |>
        httr2::req_perform()
      
      last_modified <- httr2::resp_header(response, "last-modified")
      
      if (is.null(last_modified) || !nzchar(last_modified)) {
        stop("Remote server did not provide a Last-Modified header.")
      }
      
      list(url = remote_url, last_modified = last_modified)
    }, error = function(e) {
      if (!file.exists(water_file)) {
        stop(
          "Remote check failed and no local cache exists: ",
          conditionMessage(e)
        )
      }
      warning(
        "Remote check failed: ", conditionMessage(e),
        ". Using existing cache.",
        call. = FALSE
      )
      NULL
    })
  } else {
    NULL
  }
  
  saved <- if (file.exists(metadata_file)) {
    tryCatch(readRDS(metadata_file), error = function(e) NULL)
  } else {
    NULL
  }
  
  remote_changed <- !is.null(remote) && (
    is.null(saved) ||
      !identical(remote$url,           saved$url) ||
      !identical(remote$last_modified, saved$last_modified)
  )
  
  needs_update <- isTRUE(update) || !file.exists(water_file) || remote_changed
  
  if (needs_update) {
    cat("Refreshing EDEN water data...\n")
    
    edenR::update_water(eden_path)
    
    water_long <- edenR::get_eden_covariates(
      eden_path = eden_path, level = "subregions"
    ) |>
      dplyr::bind_rows(
        edenR::get_eden_covariates(eden_path = eden_path, level = "all"),
        edenR::get_eden_covariates(eden_path = eden_path, level = "wcas")
      )
    
    if (inherits(water_long, "sf")) {
      water_long <- sf::st_drop_geometry(water_long)
    }
    
    water <- water_long |>
      tibble::as_tibble() |>
      dplyr::select(year, region = Name, variable, value) |>
      tidyr::pivot_wider(names_from = variable, values_from = value) |>
      dplyr::mutate(year = as.integer(year)) |>
      dplyr::arrange(year, region)
    
    readr::write_csv(water, water_file)
    
    if (!is.null(remote)) {
      saveRDS(remote, metadata_file)
    } else if (file.exists(metadata_file)) {
      unlink(metadata_file)
    }
    
    cat("Updated cache:", water_file, "\n")
  } else {
    water <- readr::read_csv(water_file, show_col_types = FALSE)
    cat("Using cached water data:", water_file, "\n")
  }
  
  water
}

# =============================================================================
# MAIN DATA LOADING
# =============================================================================

get_wading_bird_data <- function(config, path = ".", cache = TRUE) {
  
  level        <- config$spatial$level
  fill_missing <- config$spatial$fill_missing         %||% TRUE
  fill_value   <- config$spatial$fill_value           %||% 0
  min_years    <- config$spatial$min_years_required   %||% 10
  
  if (!level %in% c("system", "subregion", "colony")) {
    stop(glue::glue(
      "Spatial level '{level}' not recognised. ",
      "Use 'system', 'subregion', or 'colony'."
    ))
  }
  
  # -------------------------------------------------------------------------
  # CACHE
  # -------------------------------------------------------------------------
  if (cache) {
    cache_key <- digest::digest(list(
      level            = level,
      forecast_totals  = config$spatial$forecast_totals  %||% FALSE,
      include_species  = config$spatial$include_species  %||% "top6",
      fill_missing     = fill_missing,
      fill_value       = fill_value,
      min_years        = min_years,
      include_regions  = config$spatial$include_regions,
      exclude_regions  = config$spatial$exclude_regions,
      include_colonies = config$spatial$include_colonies,
      exclude_colonies = config$spatial$exclude_colonies
    ))
    
    cache_file <- file.path(
      "cache",
      paste0("data_", level, "_", substr(cache_key, 1L, 8L), ".rds")
    )
    
    if (file.exists(cache_file)) {
      age_h <- as.numeric(difftime(Sys.time(),
                                   file.info(cache_file)$mtime,
                                   units = "hours"))
      cat(glue::glue(
        "✓ Loading cached data: {basename(cache_file)} ",
        "({round(age_h, 1)} hours old)\n\n"
      ))
      return(readRDS(cache_file))
    }
  }
  
  # -------------------------------------------------------------------------
  # SPECIES FILTER
  # -------------------------------------------------------------------------
  species_pref     <- config$spatial$include_species %||% "top6"
  include_unknowns <- config$spatial$include_unknowns %||% FALSE
  
  wader_level <- translate_level_for_wader(level)
  counts      <- tibble::tibble(wader::max_counts(level = wader_level, path = path))
  
  generic_codes <- c("unkn", "lada", "lawh", "smda", "smhe", "smwh")
  
  if (identical(species_pref, "top6") ||
      (length(species_pref) == 1L && species_pref == "top6")) {
    counts <- dplyr::filter(
      counts, species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")
    )
  } else if (identical(species_pref, "all") ||
             (length(species_pref) == 1L && species_pref == "all")) {
    if (!include_unknowns) {
      counts <- dplyr::filter(counts, !species %in% generic_codes)
    }
  } else {
    counts <- dplyr::filter(counts, species %in% species_pref)
  }
  
  # -------------------------------------------------------------------------
  # WATER DATA
  # Pass remote_url from config if present; otherwise use local cache only.
  # -------------------------------------------------------------------------
  water <- get_data_water(
    remote_url = config$water$remote_url %||% NULL
  )
  
  # -------------------------------------------------------------------------
  # SYSTEM
  # -------------------------------------------------------------------------
  if (level == "system") {
    
    if (fill_missing) {
      counts <- counts |>
        tidyr::complete(
          year = tidyr::full_seq(year, 1L),
          species,
          fill = list(count = fill_value)
        ) |>
        dplyr::ungroup()
    }
    
    water_all <- dplyr::filter(water, region == "all")
    
    cat("\n=== Water Data Summary ===\n")
    cat("  Years:", min(water_all$year), "-", max(water_all$year), "\n")
    cat("  N rows:", nrow(water_all), "\n\n")
    
    combined <- counts |>
      dplyr::filter(year >= 1993L) |>
      dplyr::left_join(
        dplyr::select(water_all, -region),
        by = "year"
      ) |>
      tsibble::as_tsibble(key = species, index = year)
    
    # -------------------------------------------------------------------------
    # SUBREGION
    # -------------------------------------------------------------------------
  } else if (level == "subregion") {
    
    # Exclude first, then include (exclude wins if both set).
    if (length(config$spatial$exclude_regions) > 0L) {
      counts <- dplyr::filter(
        counts, !subregion %in% config$spatial$exclude_regions
      )
    }
    if (length(config$spatial$include_regions) > 0L) {
      counts <- dplyr::filter(
        counts, subregion %in% config$spatial$include_regions
      )
    }
    
    counts <- counts |>
      dplyr::group_by(subregion) |>
      dplyr::filter(dplyr::n_distinct(year) >= min_years) |>
      dplyr::ungroup()
    
    if (fill_missing) {
      counts <- counts |>
        tidyr::complete(
          year = tidyr::full_seq(year, 1L),
          subregion,
          species,
          fill = list(count = fill_value)
        ) |>
        dplyr::ungroup()
    }
    
    water_sub <- dplyr::filter(water, region %in% unique(counts$subregion))
    
    cat("\n=== Water data by subregion ===\n")
    water_sub |>
      dplyr::group_by(region) |>
      dplyr::summarise(
        min_year = min(year),
        max_year = max(year),
        n_years  = dplyr::n_distinct(year),
        .groups  = "drop"
      ) |>
      print()
    cat("\n")
    
    combined <- counts |>
      dplyr::filter(year >= 1993L) |>
      dplyr::left_join(water_sub, by = dplyr::join_by(year, subregion == region)) |>
      dplyr::mutate(region = subregion) |>
      dplyr::select(-subregion) |>
      tsibble::as_tsibble(key = c(species, region), index = year)
    
    # -------------------------------------------------------------------------
    # COLONY
    # -------------------------------------------------------------------------
  } else {   # level == "colony"
    
    if (length(config$spatial$include_colonies) > 0L) {
      counts <- dplyr::filter(counts, colony %in% config$spatial$include_colonies)
      cat("Filtering to", length(config$spatial$include_colonies),
          "specified colonies\n")
    }
    if (length(config$spatial$exclude_colonies) > 0L) {
      counts <- dplyr::filter(counts, !colony %in% config$spatial$exclude_colonies)
    }
    if (length(config$spatial$exclude_regions) > 0L) {
      counts <- dplyr::filter(
        counts, !subregion %in% config$spatial$exclude_regions
      )
    }
    if (length(config$spatial$include_regions) > 0L) {
      counts <- dplyr::filter(
        counts, subregion %in% config$spatial$include_regions
      )
    }
    
    counts <- counts |>
      dplyr::group_by(colony) |>
      dplyr::filter(dplyr::n_distinct(year) >= min_years) |>
      dplyr::ungroup()
    
    cat("Colonies after year filter (>=", min_years, "years):",
        dplyr::n_distinct(counts$colony), "\n")
    
    if (fill_missing) {
      counts <- counts |>
        dplyr::ungroup() |>
        tidyr::complete(
          year = tidyr::full_seq(year, 1L),
          tidyr::nesting(colony, subregion),
          species,
          fill = list(count = fill_value)
        )
    }
    
    water_sub <- dplyr::filter(water, region %in% unique(counts$subregion))
    
    cat("\n=== Water data for colony level (via subregion) ===\n")
    print(table(unique(counts$subregion)))
    cat("\n")
    
    combined <- counts |>
      dplyr::filter(year >= 1993L) |>
      dplyr::left_join(water_sub, by = dplyr::join_by(year, subregion == region)) |>
      dplyr::mutate(region = colony) |>
      dplyr::select(-subregion, -colony) |>
      tsibble::as_tsibble(key = c(species, region), index = year)
  }
  
  # -------------------------------------------------------------------------
  # DATA QUALITY CHECK
  # -------------------------------------------------------------------------
  cat("\n=== Data Quality Check ===\n")
  
  missing_summary <- combined |>
    tibble::as_tibble() |>
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(is.na(.)))) |>
    dplyr::select(where(~ . > 0L))
  
  if (ncol(missing_summary) > 0L) {
    cat("⚠  Columns with missing values:\n")
    print(missing_summary)
  } else {
    cat("✓ No missing values in numeric columns\n")
  }
  
  # -------------------------------------------------------------------------
  # AGGREGATE TO TOTALS
  # -------------------------------------------------------------------------
  if (isTRUE(config$spatial$forecast_totals)) {
    cat("\n=== Aggregating to TOTAL Counts ===\n")
    
    combined_df <- tibble::as_tibble(combined)
    group_cols  <- if ("region" %in% names(combined_df)) c("year", "region") else "year"
    water_cols  <- setdiff(names(combined_df), c(group_cols, "species", "count"))
    
    combined <- combined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        count = sum(count, na.rm = TRUE),
        dplyr::across(dplyr::all_of(water_cols), dplyr::first),
        .groups = "drop"
      ) |>
      dplyr::mutate(species = "Total")
    
    combined <- if ("region" %in% names(combined)) {
      tsibble::as_tsibble(combined, key = c(species, region), index = year)
    } else {
      tsibble::as_tsibble(combined, key = species, index = year)
    }
    
    cat("Aggregated to", dplyr::n_distinct(combined$species), "series\n")
  }
  
  # -------------------------------------------------------------------------
  # METADATA AND SUMMARY
  # -------------------------------------------------------------------------
  attr(combined, "spatial_level") <- level
  attr(combined, "spatial_units") <- if (level == "system") {
    "system-wide"
  } else {
    unique(combined$region)
  }
  
  cat(glue::glue(
    "\n✓ Loaded '{level}' level data: {nrow(combined)} observations\n"
  ))
  
  if (level != "system") {
    cat(glue::glue(
      "  Spatial units: {paste(unique(combined$region), collapse = ', ')}\n"
    ))
    combined |>
      tibble::as_tibble() |>
      dplyr::group_by(region) |>
      dplyr::summarise(
        min_year = min(year),
        max_year = max(year),
        n_years  = dplyr::n_distinct(year),
        .groups  = "drop"
      ) |>
      print(n = Inf)
  }
  
  # -------------------------------------------------------------------------
  # SAVE CACHE
  # -------------------------------------------------------------------------
  if (cache) {
    dir.create("cache", showWarnings = FALSE, recursive = TRUE)
    saveRDS(combined, cache_file)
    cat(glue::glue("\n💾 Cached to: {basename(cache_file)}\n"))
  }
  
  cat("\n")
  combined
}