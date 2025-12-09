library(dplyr)
library(fable)
library(feasts)
library(ggh4x)
library(ggplot2)
library(glue)
library(mvgam)
library(lubridate)
library(patchwork)
library(readr)
library(tidyr)
library(tsibble)
library(urca)
library(wader)

water <- load_datafile("eden_covariates.csv") |> 
  filter(year < 2024) # No 2024 bird data yet

depth_data <- read_csv("3as_depth_data.csv") |>
  rename(threeA_depth = avg_depth) |>
  inner_join(read_csv("inlandenp_depth_data.csv"), by = join_by("time", "year")) |>
  mutate(
    month = month(time),
    day = day(time),
    one_month_recession_rate = (lag(avg_depth, 30) - avg_depth) / 30,
    two_month_recession_rate = (lag(avg_depth, 60) - avg_depth) / 60,
    two_month_3as_recession_rate = (lag(threeA_depth, 60) - threeA_depth) / 60,
  )

depth_data_oct1 <- depth_data |>
  group_by(year) |>
  filter(month >= 10 | month <= 5)

recession_rates <- depth_data |>
  group_by(year) |>
  reframe(
    start_depth = avg_depth[month == 11 & day == 1],
    end_depth = avg_depth[month == 1 & day == 31],
    early_recession_rate = (start_depth - end_depth) / 90,
    max_wet_season_depth = max(avg_depth[month %in% c(6, 7, 8, 9, 10, 11)])
  )

init_data <- load_datafile("Indicators/stork_initiation.csv", download_if_missing = TRUE) |>
  filter(year >= 1991) |> # No water data before 1991
  mutate(
    time = as.POSIXct(ymd(paste0(year - 1, "-11-01")) + days(days_past_nov_1)),
    mintime = time - days(14),
    maxtime = time + days(14),
  ) |>
  full_join(water, by = "year") |>
  full_join(recession_rates, by = "year") |>
  drop_na() |>
  as_tibble()

initiation_date <- ggplot(init_data, aes(x = year, y = days_past_nov_1)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Initiation (days past Nov 1)"
  )

pre_recession <- ggplot(init_data, aes(x = year, y = pre_recession)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Pre-recession"
  )

early_recession <- ggplot(init_data, aes(x = year, y = early_recession_rate)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Early recession rate"
  )

initial_depth <- ggplot(init_data, aes(x = year, y = init_depth)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Initial Depth"
  )

initiation_date + initial_depth + early_recession + plot_layout(ncol = 1)

ggplot(init_data, aes(x = early_recession_rate, y = days_past_nov_1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Pre-recession",
    y = "Initiation (days past Nov 1)"
  )

summary(lm(days_past_nov_1 ~ pre_recession + init_depth, data = init_data))
summary(lm(days_past_nov_1 ~ early_recession_rate + init_depth, data = init_data))
summary(lm(days_past_nov_1 ~ early_recession_rate + max_wet_season_depth, data = init_data))

ggplot(depth_data_oct1, aes(x = time, group = year)) +
  geom_line(mapping = aes(y = avg_depth)) +
  geom_line(mapping = aes(y = threeA_depth / 2), color = "orange") +
  geom_rect(
    mapping = aes(
      xmin = mintime,
      xmax = maxtime,
      ymin = 0,
      ymax = 50
    ),
    data = init_data,
    alpha = 0.2
  ) +
  facet_wrap(~year, scales = "free")

ggplot(depth_data_oct1, aes(x = time, group = year)) +
  geom_line(mapping = aes(y = two_month_recession_rate)) +
  geom_line(mapping = aes(y = one_month_recession_rate), color = "gray") +
  geom_rect(
    mapping = aes(
      xmin = mintime,
      xmax = maxtime,
      ymin = -.75,
      ymax = .75
    ),
    data = init_data,
    alpha = 0.2
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(~year, scales = "free_x")
