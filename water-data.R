library(dplyr)
library(edenR)
library(ggplot2)
library(glue)
library(readr)
library(sf)
library(wader)

eden_path <- "WaterData"
#download_eden_depths(eden_path)

eden_data_files <- list.files(file.path(eden_path), pattern = "_depth.nc", full.names = TRUE)
boundaries <- load_boundaries(level = "subregions")                                                             ### this is not working... 
examp_eden_file <- stars::read_stars(file.path(eden_data_files[1]))
boundaries_utm <- sf::st_transform(boundaries, sf::st_crs(examp_eden_file))

terra::rast(file.path(eden_data_files[1]))


# T:\lab-white-ernest\EDENBackup

# Modified from EvergladesWadingBird DataCleaningScripts/eden_covariates.R
# year <- 2022
regions <- c("inlandenp", "3as")
# region_boundary <- filter(boundaries_utm, Name %in% regions)
# current_year <- file.path(paste(year, "_.*_depth.nc", sep = ""))
# prev_year <- file.path(paste(as.numeric(year) - 1, "_.*_depth.nc", sep = ""))
# nc_files <- c(
#   list.files(eden_path, current_year, full.names = TRUE),
#   list.files(eden_path, prev_year, full.names = TRUE)
# )

for (region in regions) {
  all_years_data <- list()
  region_boundary <- filter(boundaries_utm, Name == region)
  for (year in 1996:2024) {
    print(glue::glue("Processing {year}, {region}"))
    current_year <- file.path(paste(year, "_.*_depth.nc", sep = ""))
    prev_year <- file.path(paste(as.numeric(year) - 1, "_.*_depth.nc", sep = ""))
    nc_files <- c(
      list.files(eden_path, current_year, full.names = TRUE),
      list.files(eden_path, prev_year, full.names = TRUE)
    )

    year_data <- stars::read_stars(nc_files, along = "time") |>
      setNames("depth") |>
      dplyr::mutate(depth = dplyr::case_when(
        depth < units::set_units(0, cm) ~ units::set_units(0, cm),
        depth >= units::set_units(0, cm) ~ depth,
        is.na(depth) ~ units::set_units(NA, cm)
      ))

    region_year_data <- st_crop(year_data, region_boundary)

    average_depths <- region_year_data |>
      as_tibble() |>
      dplyr::group_by(time) |>
      dplyr::summarise(avg_depth = mean(depth, na.rm = TRUE)) |>
      dplyr::mutate(avg_depth = as.numeric(avg_depth)) |>
      # next line should be doing nothing now
      dplyr::filter(time >= as.Date(paste(year - 1, "-06-01", sep = "")) & time <= as.Date(paste(year, "-05-31", sep = ""))) |>
      dplyr::mutate(year = year)

    all_years_data[[as.character(year)]] <- average_depths
  }
  all_years_data <- dplyr::bind_rows(all_years_data)
  write_csv(all_years_data, glue::glue("{region}_depth_data.csv"))                                      ### regions not workig 
}


