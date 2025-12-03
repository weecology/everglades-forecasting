
# MVGAM wading birds  ------------------------------------------------------


library(dplyr)
library(mvgam)
library(tibble)
library(tidyr)
library(wader)



everglades_counts <- tibble(max_counts(level = "all"))
everglades_counts <- everglades_counts |>
  filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
  complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
  ungroup()

water <- load_datafile("eden_covariates.csv")
everglades_water <- filter(water, region == "all") |>
  filter(year < 2024) # 2024 data for birds not yet available

count_env_data <- everglades_counts |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(everglades_water, by = "year") |>
  drop_na(species) |> 
  mutate(time = year - min(year) + 1, 
         series = factor(species)) # series = species and region! 

data_train <- filter(count_env_data, year < 2018)
data_test <- filter(count_env_data, year >= 2018 & year < 2024 )

plot_mvgam_series(data = data_train, y = "count", series = "all")



# by species by region. fix this 


# everglades_counts <- tibble(max_counts(level = 'region'))
# everglades_counts <- everglades_counts |>
#   filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
#   complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
#   ungroup()
# 
# everglades_water <- load_datafile("eden_covariates.csv") |>
#   filter(year < 2024) # 2024 data for birds not yet available
# 
# count_env_data <- everglades_counts |>
#   filter(year >= 1991) |> # No water data prior to 1991
#   full_join(everglades_water, by = c("year",'region')) |>
#   mutate(region_species = paste(region, species, sep = '-'), #if NA then == NA
#     time = year - min(year) + 1, 
#          series = factor(species)) |> 
#   mutate(series = factor(region_species))
# 
# data_train <- filter(count_env_data, year < 2018)
# data_test <- filter(count_env_data, year >= 2018 & year < 2024 )


# by species by region. fix this 


plot_mvgam_series(data = data_train, y = "count", series = "all")




# priors for later --------------------------------------------------------


priors <- get_mvgam_priors(
  formula = count ~ 1,
  trend_formula = ~ s(breed_season_depth, trend, bs = "re"),
  trend_model = "VAR1", ####
  family = nb(),
  data = data_train
)

# priors <- prior(beta(10, 10), 
#                 class = sigma, 
#                 lb = 0.2, 
#                 ub = 1)
priors <- c(priors, prior(normal(0, 0.001), class = Intercept))




# mvgam -------------------------------------------------------------------


plot_mvgam_series(data = data_train, y = "count")


baseline_model <- mvgam(
  count ~ recession, 
  trend_model = AR(p = 2), #autoregressive, p =1 one timestep back first order 
  family = nb(), 
  data = data_train, 
  newdata = data_test,
  burnin = 2000)



gam_ar1 = mvgam(
  formula = count ~ 1,
  trend_formula = ~ s(recession, trend, bs = "re") +
    s(pre_recession , trend, bs = "re") +
    #s(post_recession , trend, bs = "re") +
    s(reversals , trend, bs = "re") +
    s(dry_days , trend, bs = "re")+
    species,
 # trend_model = AR(),     #trend model, no autocorrelation ar1 
  family = nb(),
  data = data_train,
  burnin = 2000, 
  newdata = data_test,
  chains = 4
)

# nick clark et al 2025 portal paper. 
# https://github.com/nicholasjclark/portal_VAR/blob/main/2.%20models.R


# skill baseline from portal code. 
# model of just the mean 

gam_ar1 <- baseline_model

summary(gam_ar1, include_betas = FALSE)
mcmc_plot(gam_ar1, 
          type = 'trace', 
          variable = c( 'recession',
                        'ar1[1]', 
                        'sigma[1]')
)
plot(gam_ar1, type = 'residuals')
mcmc_plot(gam_ar1,
          regex = TRUE, type = 'hist')

forecast <- forecast(gam_ar1, newdata = data_test)
scores <- mvgam::score(forecast, interval_width = 0.5)
in_interval <- scores$wost$in_interval
length(in_interval[in_interval == 1]) / length(in_interval)

plot(forecast)


#https://stats.stackexchange.com/questions/657495/uncertain-serial-autocorrelation-in-gam-count-model-residuals
#https://www.r-bloggers.com/2024/09/state-space-vector-autoregressions-in-mvgam/


plot(gam_ar1, type = "forecast", series = 1)
plot(gam_ar1, type = "forecast", series = 2)
plot(gam_ar1, type = "forecast", series = 3)
plot(gam_ar1, type = "forecast", series = 4)
plot(gam_ar1, type = "forecast", series = 5)
plot(gam_ar1, type = "forecast", series = 6)








#calculate skill relative to this: 
# baseline_model <- mvgam(
#   formula = y ~ series,
#   data = data_train,
#   newdata = data_test,
#   family = nb()
# )

#extra
# priors = ar_priors,
# burnin = 5000,
# samples = 10000
# )
