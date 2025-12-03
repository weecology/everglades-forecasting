
# everglades forecasting  -------------------------------------------------

library(dplyr)
library(fable)
library(feasts)
library(ggh4x)
library(ggplot2)
library(mvgam)
library(tidyr)
library(tsibble)
library(urca)
library(wader)
library(edenR)



download_observations(".")



# exploring the data ------------------------------------------------------


max_counts <- tibble(max_counts())   #change here to subregion 
max_counts <- max_counts |>
  filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
  group_by(colony) |>
  filter(n_distinct(year) > 10) |>
  complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
  ungroup()

ggplot(max_counts, aes(x = year, y = count, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~colony, scales = "free")




region_counts <- tibble(max_counts(level = "subregion"))
region_counts <- region_counts |>
  filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
  group_by(region) |>
  filter(n_distinct(year) > 10) |>
  complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
  ungroup()

ggplot(region_counts, aes(x = year, y = count, color = species)) +
  geom_point() +
  geom_line() +
  facet_grid2(vars(species), vars(region), scales = "free", independent = "y")


# water/enviromental data --------------------------------------------------------------

#needs fixing, works fornow

water <- read.csv("eden_covariates.csv")

# eden_path <- "WaterData"                                       
# water <- get_eden_covariates(eden_path = eden_path,
#                              years = available_years(eden_path)[1:length(available_years(eden_path))-1]) |>
#     bind_rows(get_eden_covariates(path = eden_path, 
#                                   years = available_years(eden_path)[1:length(available_years(eden_path))-1], 
#                                   level="all")) |>
#     bind_rows(get_eden_covariates(path = eden_path, 
#                                   years = available_years(eden_path)[1:length(available_years(eden_path))-1], 
#                                   level="wcas")) |>
#     select(year, region=Name, variable, value) |>
#     as.data.frame() |>
#     select(-geometry) |>
#     pivot_wider(names_from="variable", values_from="value") |>
#     mutate(year = as.integer(year)) |>
#     arrange("year", "region")

#  2025_xx_deoth.nc not working 
# work around is years = available_years(eden_path)[1:length(available_years(eden_path))-1]





# forecasting -------------------------------------------------------------

region_water <- filter(water, region %in% c(unique(region_counts$region))) |>
  filter(year < 2024) # 2024 data for birds not yet available
count_env_data <- region_counts |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(region_water, by = c("year", "region")) |>
  as_tsibble(key = c(species, region), index = year)

# there are na species, assumption is year/region combos that did not have any birds 
count_env_data |> filter(is.na(species))



models <- model(count_env_data,
                arima = ARIMA(count),
                arima_exog = ARIMA(count ~ breed_season_depth + dry_days + pre_recession + post_recession),
                tslm = TSLM(count ~ breed_season_depth + dry_days + pre_recession + post_recession + trend()))
glance(models)
models_aug <- augment(models)
autoplot(models_aug, count) +
  autolayer(models_aug, .fitted, linetype = 2) +
  facet_grid(vars(species), vars(region), scales = "free")+
  theme(legend.position = "none")

ggplot(mapping = aes(x = year, y = count)) +
  geom_line(data = count_env_data) +
  geom_line(data = models_aug, mapping = aes(y = .fitted, color = `.model`)) +
  facet_grid2(vars(species), vars(region), scales = "free_y", independent = "y")



# max counts 
everglades_counts <- tibble(max_counts(level = "all"))
everglades_counts <- everglades_counts |>
  filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
  complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
  ungroup()

ggplot(everglades_counts, aes(x = year, y = count, color = species)) +
  geom_point() +
  geom_line()


everglades_counts <- as_tsibble(everglades_counts, key = species, index = year)
models <- model(everglades_counts, ARIMA(count))
glance(models)
models_aug <- augment(models)
autoplot(models_aug, count) +
  autolayer(models_aug, .fitted, color = 'black', linetype = 2) +
  facet_wrap(~species, scales = "free")


# include covariates -------------------------------------------------------


everglades_water <- filter(water, region == "all") |>
  filter(year < 2024) # 2024 data for birds not yet available
count_env_data <- everglades_counts |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(everglades_water, by = "year") |>
  as_tsibble(key = species, index = year)




tslm_model <- model(count_env_data, TSLM(count ~ breed_season_depth + breed_season_depth^2 + pre_recession + post_recession + recession + trend()))
glance(tslm_model)
tslm_model_aug = augment(tslm_model)

autoplot(tslm_model_aug, count) +
  autolayer(tslm_model_aug, .fitted, color = 'black', linetype = 2) +
  facet_wrap(~species, scales = "free") +
  theme(legend.position="none")


arima_exog_model <- model(count_env_data, ARIMA(count ~ breed_season_depth + breed_season_depth^2 + pre_recession + post_recession + recession))
glance(arima_exog_model)
arima_exog_model_aug = augment(arima_exog_model)

autoplot(arima_exog_model_aug, count) +
  autolayer(arima_exog_model_aug, .fitted, color = 'black', linetype = 2) +
  facet_wrap(~species, scales = "free") +
  theme(legend.position="none")




