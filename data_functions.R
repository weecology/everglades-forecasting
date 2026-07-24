# data_functions.R
library(digest)
library(edenR)
library(dplyr)
library(readr)
library(tidyr)
library(tsibble)
library(wader)
library(zoo)

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x
# Helper to translate config level to wader level
translate_level_for_wader <- function(level) {
  if (level == "system") return("all")
  return(level)
}


# =============================================================================
# WATER DATA
# =============================================================================
get_data_water <- function(eden_path = "WaterData", update = FALSE) {
  if (update) {
    cat("📥 Downloading fresh EDEN water data...\n")
    update_water(eden_path)
    water <- get_eden_covariates(eden_path = eden_path, level = "subregions") |>
      bind_rows(get_eden_covariates(eden_path = eden_path, level = "all")) |>
      bind_rows(get_eden_covariates(eden_path = eden_path, level = "wcas")) |>
      dplyr::select(year, region = Name, variable, value) |>
      as.data.frame() |>
      dplyr::select(-geometry) |>
      pivot_wider(names_from = "variable", values_from = "value") |>
      mutate(year = as.integer(year)) |>
      arrange(year, region)
    
    # Save updated covariates
    write_csv(water, file.path(eden_path, "eden_covariates.csv"))
    cat(glue::glue(
      "✓ Water data updated and saved to {eden_path}/eden_covariates.csv\n",
      "  Years: {min(water$year)}-{max(water$year)}\n",
      "  Regions: {paste(unique(water$region), collapse = ', ')}\n"
    ))
    
  } else {
    water_file <- file.path(eden_path, "eden_covariates.csv")
    
    if (!file.exists(water_file)) {
      stop(glue::glue(
        "Water data file not found: {water_file}\n",
        "Run get_data_water(update = TRUE) to download fresh data"
      ))
    }
    
    water <- read_csv(water_file, show_col_types = FALSE)
    
    cat(glue::glue(
      "✓ Water data loaded from: {water_file}\n",
      "  Years: {min(water$year)}-{max(water$year)}\n",
      "  Regions: {paste(unique(water$region), collapse = ', ')}\n",
      "  Last modified: {format(file.info(water_file)$mtime, '%Y-%m-%d')}\n"
    ))
  }
  
  return(water)
}

# =============================================================================
# MAIN DATA LOADING (with caching support)
# =============================================================================
get_wading_bird_data <- function(config, path = ".", cache = TRUE) {
  
  level        <- config$spatial$level
  fill_missing <- config$spatial$fill_missing %||% TRUE
  fill_value   <- config$spatial$fill_value   %||% 0
  min_years    <- config$spatial$min_years_required %||% 10
  
  # =========================================================================
  # CACHING
  # =========================================================================
  
  if (cache) {
    # Generate cache key based on config
    cache_key <- digest::digest(list(
      level = level,
      forecast_totals = config$spatial$forecast_totals %||% FALSE,
      include_species = config$spatial$include_species %||% "top6",
      fill_missing = fill_missing,
      fill_value = fill_value,
      min_years = min_years,
      include_regions = config$spatial$include_regions,
      exclude_regions = config$spatial$exclude_regions,
      include_colonies = config$spatial$include_colonies,
      exclude_colonies = config$spatial$exclude_colonies
    ))
    
    cache_file <- file.path("cache", paste0("data_", level, "_", substr(cache_key, 1, 8), ".rds"))
    
    # Load from cache if available
    if (file.exists(cache_file)) {
      cache_time <- file.info(cache_file)$mtime
      cache_age_hours <- as.numeric(difftime(Sys.time(), cache_time, units = "hours"))
      
      cat(glue::glue("✓ Loading cached data from: {basename(cache_file)}\n"))
      cat(glue::glue("  (cached {round(cache_age_hours, 1)} hours ago)\n\n"))
      
      return(readRDS(cache_file))
    }
  }
  
  # =========================================================================
  # LOAD DATA (if not cached)
  # =========================================================================
  
  # Determine species preferences from config
  species_pref <- config$spatial$include_species %||% "top6"
  include_unknowns <- config$spatial$include_unknowns %||% FALSE
  
  # Load raw counts
  # Translate "system" -> "all" for wader compatibility
  wader_level <- translate_level_for_wader(level)
  counts <- tibble(max_counts(level = wader_level, path = path))
  
  # Filter species based on config
  if (length(species_pref) == 1 && species_pref == "top6") {
    counts <- counts |>
      filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib"))
    
  } else if (length(species_pref) == 1 && species_pref == "all") {
    
    # 'all' but NO unknowns, filter the generic codes out
    if (!include_unknowns) {
      generic_codes <- c("unkn", "lada", "lawh", "smda", "smhe", "smwh")
      counts <- counts |>
        filter(!species %in% generic_codes)
    }
    # If include_unknowns is TRUE -> keep everything!
    
  } else {
    # Assume custom list in the yaml (e.g., ["wost", "anhi", "unkn"])
    counts <- counts |>
      filter(species %in% species_pref)
  }
  
  water <- get_data_water()
  
  # ==========================================================================
  # SYSTEM (system-wide)
  # ==========================================================================
  if (level == "system") {
    
    if (fill_missing) {
      counts <- counts |>
        complete(year = full_seq(year, 1), species,
                 fill = list(count = fill_value)) |>
        ungroup()
    }
    
    water_all <- filter(water, region == "all")
    
    cat("\n=== Water Data Summary ===\n")
    cat("  Years:", min(water_all$year), "-", max(water_all$year), "\n")
    cat("  N rows:", nrow(water_all), "\n\n")
    
    combined <- counts |>
      filter(year >= 1993) |>
      left_join(water_all, by = "year") |>
      as_tsibble(key = species, index = year)
    
    # ==========================================================================
    # SUBREGION
    # ==========================================================================
  } else if (level == "subregion") {
    
    # Apply include/exclude filters
    if (!is.null(config$spatial$include_regions) &&
        length(config$spatial$include_regions) > 0) {
      counts <- counts |>
        filter(subregion %in% config$spatial$include_regions)
    }
    if (!is.null(config$spatial$exclude_regions) &&
        length(config$spatial$exclude_regions) > 0) {
      counts <- counts |>
        filter(!subregion %in% config$spatial$exclude_regions)
    }
    
    # Filter by minimum years per subregion
    counts <- counts |>
      group_by(subregion) |>
      filter(n_distinct(year) >= min_years) |>
      ungroup()
    
    # Fill missing years
    if (fill_missing) {
      counts <- counts |>
        complete(year = full_seq(year, 1), subregion, species,
                 fill = list(count = fill_value)) |>
        ungroup()
    }
    
    # Join water data via subregion
    water_sub <- filter(water, region %in% unique(counts$subregion))
    
    cat("\n=== Water data by subregion ===\n")
    water_sub |>
      group_by(region) |>
      summarise(
        min_year = min(year),
        max_year = max(year),
        n_years  = n_distinct(year),
        .groups  = "drop"
      ) |>
      print()
    cat("\n")
    
    combined <- counts |>
      filter(year >= 1993) |>
      left_join(water_sub, by = join_by(year, subregion == region)) |>
      mutate(region = subregion) |>
      dplyr::select(-subregion) |>
      as_tsibble(key = c(species, region), index = year)
    
    # ==========================================================================
    # COLONY
    # ==========================================================================
  } else if (level == "colony") {
    
    # Apply colony include/exclude filters
    if (!is.null(config$spatial$include_colonies) &&
        length(config$spatial$include_colonies) > 0) {
      counts <- counts |>
        filter(colony %in% config$spatial$include_colonies)
      cat("Filtering to", length(config$spatial$include_colonies),
          "specified colonies\n")
    }
    if (!is.null(config$spatial$exclude_colonies) &&
        length(config$spatial$exclude_colonies) > 0) {
      counts <- counts |>
        filter(!colony %in% config$spatial$exclude_colonies)
    }
    
    # Apply subregion filter if specified
    if (!is.null(config$spatial$include_regions) &&
        length(config$spatial$include_regions) > 0) {
      counts <- counts |>
        filter(subregion %in% config$spatial$include_regions)
    }
    
    # Filter by minimum years per colony
    counts <- counts |>
      group_by(colony) |>
      filter(n_distinct(year) >= min_years) |>
      ungroup()
    
    cat("Colonies after year filter (>=", min_years, "years):",
        length(unique(counts$colony)), "\n")
    
    # Fill missing years per colony (FOR MVGAM)
    if (fill_missing) {
      counts <- counts |>
        ungroup() |>
        complete(
          year = full_seq(year, 1),
          tidyr::nesting(colony, subregion),
          species,
          fill = list(count = fill_value)
        )
    }
    
    # Join water data via subregion (colony inherits from subregion)
    water_sub <- filter(water, region %in% unique(counts$subregion))
    
    cat("\n=== Water data for colony level (via subregion) ===\n")
    print(table(unique(counts$subregion)))
    cat("\n")
    
    combined <- counts |>
      filter(year >= 1993) |>
      left_join(water_sub, by = join_by(year, subregion == region)) |>
      mutate(region = colony) |>   # colony becomes the spatial key
      dplyr::select(-subregion, -colony) |>
      as_tsibble(key = c(species, region), index = year)
    
  } else {
    stop(glue::glue(
      "Spatial level '{level}' not recognized. ",
      "Use 'system', 'subregion', or 'colony'."
    ))
  }
  
  # ==========================================================================
  # FINAL DATA QUALITY CHECK
  # ==========================================================================
  cat("\n=== Data Quality Check ===\n")
  
  missing_summary <- combined |>
    as_tibble() |>
    summarise(across(where(is.numeric), ~sum(is.na(.)))) |>
    dplyr::select(where(~. > 0))
  
  if (ncol(missing_summary) > 0) {
    cat("⚠️  Columns with missing values:\n")
    print(missing_summary)
  } else {
    cat("✅ No missing values in numeric columns\n")
  }
  
  # ==========================================================================
  # AGGREGATE TO TOTALS (If configured)
  # ==========================================================================
  if (!is.null(config$spatial$forecast_totals) && isTRUE(config$spatial$forecast_totals)) {
    cat("\n=== Aggregating to TOTAL Counts ===\n")
    cat("Collapsing all species into a single 'Total' category...\n")
    
    # Drop tsibble class temporarily to do dplyr operations safely
    combined_df <- as_tibble(combined)
    
    # Determine grouping columns dynamically
    group_cols <- "year"
    if ("region" %in% names(combined_df)) group_cols <- c(group_cols, "region")
    
    # Identify water covariate columns (everything else)
    water_cols <- setdiff(names(combined_df), c(group_cols, "species", "count"))
    
    # Aggregate
    combined <- combined_df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        count = sum(count, na.rm = TRUE),
        # Water covariates are identical for a given year/region, so take the first
        dplyr::across(dplyr::all_of(water_cols), ~ dplyr::first(.x)),
        .groups = "drop"
      ) |>
      dplyr::mutate(species = "Total")
    
    # Re-create the tsibble
    if ("region" %in% names(combined)) {
      combined <- as_tsibble(combined, key = c(species, region), index = year)
    } else {
      combined <- as_tsibble(combined, key = species, index = year)
    }
  }
  
  # Attach metadata
  attr(combined, "spatial_level") <- level
  attr(combined, "spatial_units") <- if (level == "system") {
    "system-wide"
  } else {
    unique(combined$region)
  }
  
  # Summary output
  cat(glue::glue(
    "\n✅ Loaded data at '{level}' level: {nrow(combined)} observations\n"
  ))
  
  if (level != "system") {
    cat(glue::glue(
      "   Spatial units: {paste(unique(combined$region), collapse = ', ')}\n"
    ))
    combined |>
      as_tibble() |>
      group_by(region) |>
      summarise(
        min_year = min(year),
        max_year = max(year),
        n_years  = n_distinct(year),
        .groups  = "drop"
      ) |>
      print(n = Inf)
  }
  
  # =========================================================================
  # SAVE TO CACHE
  # =========================================================================
  
  if (cache) {
    dir.create("cache", showWarnings = FALSE, recursive = TRUE)
    saveRDS(combined, cache_file)
    cat(glue::glue("\n💾 Data cached to: {basename(cache_file)}\n"))
  }
  
  cat("\n")
  return(combined)
}