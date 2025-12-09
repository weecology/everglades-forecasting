
# MVGAM wading birds  ------------------------------------------------------


library(dplyr)
library(mvgam)
library(stringr)
library(tibble)
library(tidyr)
library(wader)



everglades_counts_all <- tibble(max_counts(level = "all"))
everglades_counts_all <- everglades_counts_all |>
  filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
  complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
  ungroup()

everglades_region <- tibble(max_counts(level = "subregion"))
everglades_counts_region <- everglades_region |>
  filter(species %in% c("gbhe", "greg", "rosp", "sneg", "wost", "whib")) |>
  mutate(bird_region = paste(species, region,sep = '-')) |>             # series = species and region! 
  complete(year = full_seq(year, 1), bird_region, fill = list(count = 0)) |>
  ungroup() |> 
  mutate(region = if_else(is.na(region), 
                          word(bird_region, 2, sep = fixed('-')), 
                          region), 
         species = if_else(is.na(species),
                           word(bird_region, 1, sep = fixed('-')), 
                           species)) |> 
  filter(region != '3an' & region != '2a')



# removing 2a and 3an as these don't have WOST - will add them later. 


everglades_counts_region <- everglades_counts_region |> 
  filter(region != '3an' & region != '2a')



# add water data ----------------------------------------------------------


water <- load_datafile("eden_covariates.csv")
everglades_water_all <- filter(water, region == "all") 

everglades_water_region <- water 

count_env_data_all <- everglades_counts_all |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(everglades_water_all, by = "year") |>
  drop_na(species) |>
  mutate(time = year - min(year) + 1, 
         series = factor(species) ) 

count_env_data_region <- everglades_counts_region |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(everglades_water_region, 
            by = c("year",'region'), 
            relationship = "many-to-many") |>
  mutate(time = year - min(year) + 1, 
         series = factor(bird_region)) |> 
  drop_na(series)


table(count_env_data_region$year, 
      count_env_data_region$series) 

table(count_env_data_region$year, 
      count_env_data_region$species) 

# test for data structure  ------------------------------------------------




get_mvgam_priors(count ~ 1,
                 data = count_env_data_all,
                 family = gaussian()
)

get_mvgam_priors(count ~ 1,
                 data = count_env_data_region,
                 family = gaussian()
)





# mvgams ------------------------------------------------------------------




data_train_all <- filter(count_env_data_all, year < 2020)
data_test_all <- filter(count_env_data_all, year >= 2020  )

data_train_region <- filter(count_env_data_region, year < 2020)
data_test_region <- filter(count_env_data_region, year >= 2020 )



# _all = the entire everglades
# _region = regions in the everglades enp, wcas, 




plot_mvgam_series(data = data_train_all, y = "count", series = "all")
plot_mvgam_series(data = data_train_region, y = "count", series = "all", log_scale = TRUE)




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


baseline_model_all <- mvgam(
  count ~ 1, 
  #trend_model = AR(p = 2), #autoregressive, p =1 one timestep back first order 
  family = nb(), 
  data = data_train_all, 
  newdata = data_test_all,
  burnin = 2000)



baseline_model_region <- mvgam(
  count ~ 1, 
  #trend_model = AR(p = 2), #autoregressive, p =1 one timestep back first order 
  family = nb(), 
  data = data_train_region, 
  newdata = data_test_region,
  burnin = 2000)


# nick clark et al 2025 portal paper. 
# https://github.com/nicholasjclark/portal_VAR/blob/main/2.%20models.R


# skill baseline from portal code. 
# model of just the mean 


#all 
summary(baseline_model_all, include_betas = FALSE)
mcmc_plot(baseline_model_all, 
          type = 'trace')

plot(baseline_model_all, type = 'residuals')
mcmc_plot(baseline_model_all,
          regex = TRUE, type = 'hist')

forecast_all <- forecast(baseline_model_all, newdata = data_test)
scores_all <- mvgam::score(forecast_all, interval_width = 0.5)
in_interval_all <- scores_all$wost$in_interval
length(in_interval_all[in_interval_all == 1]) / length(in_interval_all)

plot(forecast_all)

#region
summary(baseline_model_region, include_betas = FALSE)
mcmc_plot(baseline_model_region, 
          type = 'trace')

plot(baseline_model_region, type = 'residuals')
mcmc_plot(baseline_model_region,
          regex = TRUE, type = 'hist')

forecast_region <- forecast(baseline_model_region, newdata = data_test)
scores_region <- mvgam::score(forecast_region, interval_width = 0.5)
in_interval_region <- scores_region$`wost-2b`$in_interval
length(in_interval_region[in_interval_region == 1]) / length(in_interval_region)

plot(forecast)







#https://stats.stackexchange.com/questions/657495/uncertain-serial-autocorrelation-in-gam-count-model-residuals
#https://www.r-bloggers.com/2024/09/state-space-vector-autoregressions-in-mvgam/




# 
# mod2 <- mvgam(
#   formula = count ~ -1,
#   trend_formula = ~ s(recession , trend, bs = 're') +
#     # Hierarchical distributed lags of minimum temperature
#     te(recession, year, k = c(3, 4), bs = c('tp', 'cr')) +
#     te(recession, year, by = as.factor(region), k = c(3, 4), bs = c('tp', 'cr')),
#   data = data_train,
#   newdata = data_test,
#   family = nb(),
#   #trend_model = AR(),
#   samples = 1600,
#   #algorithm = 'sampling',
#   burnin = 2000
# )

gam_all = mvgam(
  formula = count ~ 1,
  trend_formula = ~ s(recession, trend, bs = "re") +
    s(pre_recession , trend, bs = "re") +
    #s(post_recession , trend, bs = "re") +   
    te(reversals , year, bs = c("re","cc")) +
    te(dry_days , year, bs = c("re","cc"))+
    species,
  # trend_model = AR(),      trend_model = RW()?
  family = nb(),
  data = data_train_all,
  burnin = 2000, 
  newdata = data_test_all,
  chains = 4
)




gam_region = mvgam(
  formula = count ~ 1,
  trend_formula = ~ s(recession, trend, bs = "re") +
    s(pre_recession , trend, bs = "re") +
    #s(post_recession , trend, bs = "re") +
    te(reversals , year, bs = c("re","cc")) +
    te(dry_days , year, bs = c("re","cc"))+
    species,
  # trend_model = AR(),     trend_model = AR(p = 1),
  family = nb(),
  data = data_train_region,
  burnin = 2000, 
  newdata = data_test_region,
  chains = 4
) 




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











m1 <- mvgam(
  y ~ 1,
  trend_formula = ~ time +
    s(season, bs = 'cc', k = 9),
  trend_model = AR(p = 1),
  noncentred = TRUE,
  data = simdat$data_train,
  newdata = simdat$data_test,
  chains = 2,
  silent = 2
)
m2 <- mvgam(
  y ~ time,
  trend_model = RW(),
  noncentred = TRUE,
  data = simdat$data_train,
  newdata = simdat$data_test,
  chains = 2,
  silent = 2
)


# Calculate forecast distributions for each model
fc1 <- forecast(m1)
fc2 <- forecast(m2)


# Generate the ensemble forecast
ensemble_fc <- ensemble(fc1, fc2)
# Plot forecasts
plot(fc1)
plot(fc2)
plot(ensemble_fc)
# Score forecasts
score(fc1)
score(fc2)
score(ensemble_fc)
