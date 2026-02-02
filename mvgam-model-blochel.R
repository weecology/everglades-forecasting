
# need to add priors, tell model what the system is doing. 
# water draw down does things with the available prey concentrations
# birds need x amount of food to start nesting, laying
# there is a mosaic of water bodies that dry up at different rates
# largest scale is cardinal directions, smallest is GIS locations 

#water levels affect prey fish and bird populations differently 
#in different time periods. 

# fish(t-5) water is HIGH,  
#     large fish effect prey 
# fish(t-4) water HIGH-MEDIUM generating topographic hiding locations in landscape
#     effect of large fish goes down, more prey fish 
#fish(t-3) water MEDIUM, more hiding paces for prey. 
#     small effect from large fish, highest amount of pry fish
#fish(t-2) water LOW-MEDIUM 
#     prey start becoming available for birds, predator switch on prey fish 
#fish(t-1) water LOW, prey becomes constantly available for bird predation   
#     high effect from birds on prey fish 
#fish(t-0) water LOW-LOW, water mass can't contain fish, less fish for birds
#     negative effects on bird population 

#birds(t-5) water is HIGH, 
#     to high water levels for most species to forage, available around shore
#birds(t-4) water HIGH-MEDIUM
#     more areas become available, bathymetry can allow for spots
#birds(t-3) water MEDIUM
#     more areas become available, bathymetry can allow for spots
#birds(t-2) water LOW-MEDIUM 
#     high availability in the landscape for prey availability 
#birds(t-1) water LOW
#     highest availability in the landscape for prey availability, high pressure on prey 
#birds(t-0) water LOW-LOW
#     not enough water to sustain prey population, high negative effect on birds 


# MVGAM wading birds  ------------------------------------------------------


library(dplyr)
library(mvgam)
library(parallel)
library(stringr)
library(tibble)
library(tidymodels)
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



ggplot(data = count_env_data_region,
       aes(x = breed_season_depth, 
           y = init_depth, 
           fill = recession ,
           color = recession ,
           size = count)) +
  geom_point(alpha = 0.4) +
  scale_size(range = c(1, 24), name="Population") + 
  #coord_fixed() +
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  # geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('depth - start vs nesting')+
  facet_wrap(~species)



hist(count_env_data_region$breed_season_depth)

hist(count_env_data_region$init_depth)
hist(count_env_data_region$recession)



hist((count_env_data_region$breed_season_depth *
       count_env_data_region$init_depth) /
       count_env_data_region$recession)

hist((count_env_data_region$breed_season_depth *
        count_env_data_region$recession) /
       count_env_data_region$init_depth)


ggplot(data = count_env_data_region ,
       aes(x = (breed_season_depth *
                  recession) /
             init_depth, 
           y = log(count), 
           colour = species)) +
  geom_point(alpha = 0.4) +
  #geom_abline (slope=1, linetype = "dashed", color="Red")+
  # geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('recession - count vs species')+
  facet_wrap(~species)


ggplot(data = count_env_data_region ,
       aes(y = (breed_season_depth *
                  recession) /
             (init_depth+pre_recession) , 
           x = log(count), 
           colour = species)) +
  geom_boxplot(alpha = 0.4) +
  #geom_abline (slope=1, linetype = "dashed", color="Red")+
  # geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('recession - count vs species')+
  facet_wrap(~species)

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




data_train_all <- filter(count_env_data_all, year < 2022 )
data_test_all <- filter(count_env_data_all, year >= 2022 )

data_train_region <- filter(count_env_data_region, year < 2022 )
data_test_region <- filter(count_env_data_region, year >= 2022 )



# _all = the entire everglades
# _region = regions in the everglades enp, wcas, 




plot_mvgam_series(data = data_train_all, y = "count", 
                  series = "all")
plot_mvgam_series(data = data_train_all, y = "count", 
                  series = "all", 
                  log_scale = TRUE)
plot_mvgam_series(data = data_train_region, y = "count", 
                  series = "all")
plot_mvgam_series(data = data_train_region, y = "count", 
                  series = "all", 
                  log_scale = TRUE)




# priors for later --------------------------------------------------------

# 
# priors <- get_mvgam_priors(
#   formula = count ~ 1,
#   trend_formula = ~ s(breed_season_depth, trend, bs = "re"),
#   trend_model = "VAR1", ####
#   family = nb(),
#   data = data_train_all
# )
# 
# # priors <- prior(beta(10, 10),
# #                 class = sigma,
# #                 lb = 0.2,
# #                 ub = 1)
# priors <- c(priors, prior(normal(0, 0.001), class = Intercept))
# 
# 


# mvgam -------------------------------------------------------------------


plot_mvgam_series(data = data_train_all, y = "count")
plot_mvgam_series(data = data_train_region, y = "count")

#all 
baseline_model_all <- mvgam(
  count ~ 1, 
  #trend_model = AR(), #autoregressive, p =1 one timestep back first order 
  family = nb(), 
  data = data_train_all, 
  newdata = data_test_all,
  burnin = 2000)

summary(baseline_model_all, include_betas = FALSE)
mcmc_plot(baseline_model_all, 
          type = 'trace')

plot(baseline_model_all, type = 'residuals')
mcmc_plot(baseline_model_all,
          regex = TRUE, type = 'hist')

forecast_baseline <- forecast(baseline_model_all, newdata = data_test_all)
scores_baseline <- mvgam::score(forecast_baseline, interval_width = 0.5)
in_interval_baseline <- scores_baseline$wost$in_interval
length(in_interval_baseline[in_interval_baseline == 1]) / length(in_interval_baseline)


plot(forecast_baseline, type = "forecast", series = 1)
plot(forecast_baseline, type = "forecast", series = 2)
plot(forecast_baseline, type = "forecast", series = 3)
plot(forecast_baseline, type = "forecast", series = 4)
plot(forecast_baseline, type = "forecast", series = 5)
plot(forecast_baseline, type = "forecast", series = 6)

# 
# #all 
# AR_model_all <- mvgam(
#   count ~ init_depth, 
#   trend_model = AR(), #autoregressive, p =1 one timestep back first order 
#   family = nb(), 
#   data = data_train_all, 
#   newdata = data_test_all,
#   burnin = 2000)
# 
# summary(AR_model_all, include_betas = FALSE)
# mcmc_plot(AR_model_all, 
#           type = 'trace')
# 
# plot(AR_model_all, type = 'residuals')
# mcmc_plot(AR_model_all,
#           regex = TRUE, type = 'hist')
# 
# forecast_AR <- forecast(AR_model_all, newdata = data_test_all)
# scores_AR <- mvgam::score(forecast_AR, interval_width = 0.5)
# in_interval_AR <- scores_AR$wost$in_interval
# length(in_interval_AR[in_interval_AR == 1]) / length(in_interval_AR)
# 
# 
# plot(forecast_AR, type = "forecast", series = 1)
# plot(forecast_AR, type = "forecast", series = 2)
# plot(forecast_AR, type = "forecast", series = 3)
# plot(forecast_AR, type = "forecast", series = 4)
# plot(forecast_AR, type = "forecast", series = 5)
# plot(forecast_AR, type = "forecast", series = 6)
# 
# 
# 



##


























#region
baseline_model_region <- mvgam(
  count ~ 1 + series, 
  #trend_model = AR(p = 2), #autoregressive, p =1 one timestep back first order 
  family = nb(), 
  data = data_train_region, 
  newdata = data_test_region,
  burnin = 2000)


# nick clark et al 2025 portal paper. 
# https://github.com/nicholasjclark/portal_VAR/blob/main/2.%20models.R


# skill baseline from portal code. 
# model of just the mean 

summary(baseline_model_region, include_betas = FALSE)
mcmc_plot(baseline_model_region, 
          type = 'trace')

plot(baseline_model_region, type = 'residuals')
mcmc_plot(baseline_model_region,
          regex = TRUE, type = 'hist')

forecast_region <- forecast(baseline_model_region, newdata = data_test_region)
scores_region <- mvgam::score(forecast_region, interval_width = 0.5)
in_interval_region <- scores_region$`wost-2b`$in_interval
length(in_interval_region[in_interval_region == 1]) / length(in_interval_region)

plot(forecast_region)





#https://stats.stackexchange.com/questions/657495/uncertain-serial-autocorrelation-in-gam-count-model-residuals
#https://www.r-bloggers.com/2024/09/state-space-vector-autoregressions-in-mvgam/


priors <- get_mvgam_priors(
  formula = count ~ 1,
  trend_formula = ~ s(breed_season_depth, trend, bs = "re"),
  trend_model = "VAR1",
  family = nb(),
  data = data_train_region
)

priors <- prior(beta(10, 10), class = sigma, lb = 0.2, ub = 1)
priors <- c(priors, prior(normal(0, 0.001), class = Intercept))

gam_ar1 = mvgam(
  formula = count ~ 1,
  trend_formula = ~ s(breed_season_depth, trend, bs = "re") +
    s(dry_days, trend, bs = "re") +
    s(recession, trend, bs = "re"),
  trend_model = "VAR1",
  family = nb(),
  data = data_train_region,
  newdata = data_test_region,
  chains = 2
)

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

plot_mvgam_series(data = data_train_all, y = 'count', series = 'all')
plot_mvgam_series(data = data_train_all, y = 'count', series = 1)
plot_mvgam_series(data = data_train_all, y = 'count', series = 2)
plot_mvgam_series(data = data_train_all, y = 'count', series = 3)
plot_mvgam_series(data = data_train_all, y = 'count', series = 4)
plot_mvgam_series(data = data_train_all, y = 'count', series = 5)
plot_mvgam_series(data = data_train_all, y = 'count', series = 6)

plot_mvgam_series(data = data_train_region, y = 'count', series = 'all')
plot_mvgam_series(data = data_train_region, y = 'count', series = 1)
plot_mvgam_series(data = data_train_region, y = 'count', series = 2)
plot_mvgam_series(data = data_train_region, y = 'count', series = 3)
plot_mvgam_series(data = data_train_region, y = 'count', series = 4)
plot_mvgam_series(data = data_train_region, y = 'count', series = 5)
plot_mvgam_series(data = data_train_region, y = 'count', series = 6)
plot_mvgam_series(data = data_train_region, y = 'count', series = 7)
plot_mvgam_series(data = data_train_region, y = 'count', series = 8)
plot_mvgam_series(data = data_train_region, y = 'count', series = 9)
plot_mvgam_series(data = data_train_region, y = 'count', series = 10)
plot_mvgam_series(data = data_train_region, y = 'count', series = 11)
plot_mvgam_series(data = data_train_region, y = 'count', series = 12)
plot_mvgam_series(data = data_train_region, y = 'count', series = 13)
plot_mvgam_series(data = data_train_region, y = 'count', series = 14)
plot_mvgam_series(data = data_train_region, y = 'count', series = 15)
plot_mvgam_series(data = data_train_region, y = 'count', series = 16)
plot_mvgam_series(data = data_train_region, y = 'count', series = 17)
plot_mvgam_series(data = data_train_region, y = 'count', series = 18)
plot_mvgam_series(data = data_train_region, y = 'count', series = 19)
plot_mvgam_series(data = data_train_region, y = 'count', series = 20)
plot_mvgam_series(data = data_train_region, y = 'count', series = 21)
plot_mvgam_series(data = data_train_region, y = 'count', series = 22)
plot_mvgam_series(data = data_train_region, y = 'count', series = 23)
plot_mvgam_series(data = data_train_region, y = 'count', series = 24)
plot_mvgam_series(data = data_train_region, y = 'count', series = 25)
plot_mvgam_series(data = data_train_region, y = 'count', series = 26)
plot_mvgam_series(data = data_train_region, y = 'count', series = 27)
plot_mvgam_series(data = data_train_region, y = 'count', series = 28)
plot_mvgam_series(data = data_train_region, y = 'count', series = 29)
plot_mvgam_series(data = data_train_region, y = 'count', series = 30)
plot_mvgam_series(data = data_train_region, y = 'count', series = 31)
plot_mvgam_series(data = data_train_region, y = 'count', series = 32)
plot_mvgam_series(data = data_train_region, y = 'count', series = 33)
plot_mvgam_series(data = data_train_region, y = 'count', series = 34)
plot_mvgam_series(data = data_train_region, y = 'count', series = 35)
plot_mvgam_series(data = data_train_region, y = 'count', series = 36)





# everglades --------------------------------------------------------------

data_train_all <- data_train_all |> 
  mutate(count = as.numeric(count))
  
data_test_all <- data_test_all |> 
  mutate(count = as.numeric(count))

gam_all1 = mvgam(
  count ~ init_depth + breed_season_depth + recession + 
    pre_recession + post_recession + dry_days + reversals + series,
  trend_model = AR(p = 1),
  family = nb(),
  data = data_train_all,
  burnin = 2000, 
  newdata = data_test_all,
  chains = 4
) 

#score(forecast(gam_all1), score = 'drps')

#STATESPACE MODEL -> TREND FORMULA 
#JOINT MODELS FOR var() 

summary(gam_all1)
mcmc_plot(gam_all1, 
          type = 'trace')

plot(gam_all1, type = 'residuals')

plot(gam_all1, type = "forecast", series = 1)     
plot(gam_all1, type = "forecast", series = 2)  
plot(gam_all1, type = "forecast", series = 3)     
plot(gam_all1, type = "forecast", series = 4)  
plot(gam_all1, type = "forecast", series = 5)     
plot(gam_all1, type = "forecast", series = 6)  



gam_all2 = mvgam(
  count ~ s(init_depth) + s(breed_season_depth) + s(recession) + 
    s(pre_recession) + s(post_recession) + s(dry_days) + s(reversals) +
    s(series, recession, bs = 'sz') + series,
  #s(x)+s(f1,x,bs="sz")+s(f2,x,bs="sz")+s(f1,f2,x,bs="sz",id=1)
  family = nb(),
  data = data_train_all,
  burnin = 2000, 
  newdata = data_test_all,
  chains = 4
) 

summary(gam_all2)
mcmc_plot(gam_all2, 
          type = 'trace')

plot(gam_all2, type = 'residuals')

plot(gam_all2, type = "forecast", series = 1)     
plot(gam_all2, type = "forecast", series = 2)  
plot(gam_all2, type = "forecast", series = 3)     
plot(gam_all2, type = "forecast", series = 4)  
plot(gam_all2, type = "forecast", series = 5)     
plot(gam_all2, type = "forecast", series = 6)  





gam_all3 = mvgam(
  count ~ s(init_depth, series, bs = 'sz') + s(breed_season_depth, series, bs = 'sz') + 
    s(recession, series, bs = 'sz') + s(pre_recession, series, bs = 'sz') + 
    s(post_recession, series, bs = 'sz') + s(dry_days, series, bs = 'sz') + 
    s(reversals, series, bs = 'sz') +
    s(recession, series, bs = 'sz') + series -1,
  family = nb(),
  data = data_train_all,
  trend_model = RW(),         #random walking 
  burnin = 2000, 
  newdata = data_test_all,
  chains = 4
) 

summary(gam_all3)
mcmc_plot(gam_all3, 
          type = 'trace')

plot(gam_all3, type = 'residuals')

plot(gam_all3, type = "forecast", series = 1)     
plot(gam_all3, type = "forecast", series = 2)  
plot(gam_all3, type = "forecast", series = 3)     
plot(gam_all3, type = "forecast", series = 4)  
plot(gam_all3, type = "forecast", series = 5)     
plot(gam_all3, type = "forecast", series = 6)  





gam_all4 = mvgam(
  count ~ s(init_depth, series, bs = 'sz') + s(breed_season_depth, series, bs = 'sz') + 
    s(recession, series, bs = 'sz') + s(pre_recession, series, bs = 'sz') + 
    s(post_recession, series, bs = 'sz') + s(dry_days, series, bs = 'sz') + 
    s(reversals, series, bs = 'sz') +
    s(recession, series, bs = 'sz') + series -1,
  family = nb(),
  data = data_train_all,
  trend_model = GP(),         #Gaussian Process 
  burnin = 2000, 
  newdata = data_test_all,
  chains = 4
) 

summary(gam_all4)
mcmc_plot(gam_all4, 
          type = 'trace')

plot(gam_all4, type = 'residuals')

plot(gam_all4, type = "forecast", series = 1)     
plot(gam_all4, type = "forecast", series = 2)  
plot(gam_all4, type = "forecast", series = 3)     
plot(gam_all4, type = "forecast", series = 4)  
plot(gam_all4, type = "forecast", series = 5)     
plot(gam_all4, type = "forecast", series = 6)  

# subregion ---------------------------------------------------------------



gam_region1 = mvgam(
  count ~ init_depth + breed_season_depth + recession + 
    pre_recession + post_recession + dry_days + reversals+ series,
  family = nb(),
  data = data_train_region,
  burnin = 2000, 
  newdata = data_test_region,
  chains = 4
) 

summary(gam_region1)
mcmc_plot(gam_region1, 
          type = 'trace')

plot(gam_region1, type = 'residuals')

plot(gam_region1, type = "forecast")   

plot(gam_region1, type = "forecast", series = 1)     
plot(gam_region1, type = "forecast", series = 2)  
plot(gam_region1, type = "forecast", series = 3)     
plot(gam_region1, type = "forecast", series = 4)  
plot(gam_region1, type = "forecast", series = 5)     
plot(gam_region1, type = "forecast", series = 6)  
plot(gam_region1, type = "forecast", series = 7)     
plot(gam_region1, type = "forecast", series = 8)  
plot(gam_region1, type = "forecast", series = 9)     
plot(gam_region1, type = "forecast", series = 10)  
plot(gam_region1, type = "forecast", series = 11)     
plot(gam_region1, type = "forecast", series = 12) 
plot(gam_region1, type = "forecast", series = 13)  
plot(gam_region1, type = "forecast", series = 14)     
plot(gam_region1, type = "forecast", series = 15) 
plot(gam_region1, type = "forecast", series = 16)  
plot(gam_region1, type = "forecast", series = 17)     
plot(gam_region1, type = "forecast", series = 18) 
plot(gam_region1, type = "forecast", series = 19)  
plot(gam_region1, type = "forecast", series = 20)     
plot(gam_region1, type = "forecast", series = 21) 
plot(gam_region1, type = "forecast", series = 22)     
plot(gam_region1, type = "forecast", series = 23) 
plot(gam_region1, type = "forecast", series = 24)     
plot(gam_region1, type = "forecast", series = 25) 
plot(gam_region1, type = "forecast", series = 26)     
plot(gam_region1, type = "forecast", series = 27) 
plot(gam_region1, type = "forecast", series = 28) 
plot(gam_region1, type = "forecast", series = 29)  
plot(gam_region1, type = "forecast", series = 30)     
plot(gam_region1, type = "forecast", series = 31) 
plot(gam_region1, type = "forecast", series = 32)  
plot(gam_region1, type = "forecast", series = 33)     
plot(gam_region1, type = "forecast", series = 34) 
plot(gam_region1, type = "forecast", series = 35)     
plot(gam_region1, type = "forecast", series = 36) 




#https://www.mjandrews.org/notes/rparallel/

gam_region2 = mvgam(
  count ~ s(recession) + 
    series,
  family = nb(),
  data = data_train_region,
  burnin = 2000, 
  newdata = data_test_region,
  chains = 4,
  parallel = TRUE, 
  threads = 1, 
  backend = 'cmdstanr'
) 

summary(gam_region2)
mcmc_plot(gam_region2, 
          type = 'trace')

plot(gam_region2, type = 'residuals')

plot(gam_region2, type = "forecast") 
code(gam_region2) #see stan code 



# 
# mod3 <- mvgam(
#   count ~ dynamic(recession, k = 25, scale = FALSE),
#   family = nb(),
#   data = data_train_all,
#   burnin = 2000, 
#   newdata = data_test_all,
#   chains = 4,
#   parallel = TRUE, 
#   threads = 1, 
#   backend = 'cmdstanr'
# ) 
# 
# summary(mod3)
# mcmc_plot(mod3, 
#           type = 'trace')
# 
# plot(mod3, type = 'residuals')
# 
# plot(mod3, type = "forecast") 
# code(mod3) #see stan code 
# 
# 
# plot(mod3, type = "forecast", series = 1)     
# plot(mod3, type = "forecast", series = 2)  
# plot(mod3, type = "forecast", series = 3)     
# plot(mod3, type = "forecast", series = 4)  
# plot(mod3, type = "forecast", series = 5)     
# plot(mod3, type = "forecast", series = 6)  
# plot(mod3, type = "forecast", series = 7)     
# plot(gam_region1, type = "forecast", series = 8)  
# plot(gam_region1, type = "forecast", series = 9)    


#  Piecewise trends - doesnt work 
# gam_region_piece = mvgam(
#   count ~ init_depth + recession + dry_days + reversals - 1, #remove intercept
#   family = nb(),
#   trend_model = PW(),
#   data = data_train_region,
#   burnin = 2000, 
#   newdata = data_test_region,
#   chains = 4
# ) 
# 
# summary(gam_region_piece)
# mcmc_plot(gam_region_piece, 
#           type = 'trace')
# 
# 
# plot(gam_region_piece, type = "trend", series = 5)
# 
# plot(gam_region_piece, type = "forecast", series = 5)









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


#next step: 


m1 <- mvgam(
  y ~ 1,
  trend_formula = ~ time +
    s(year, bs = 'cc', k = 9),
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





















# from Ethan 2024 ---------------------------------------------------------


library(dplyr)
library(mvgam)
library(tibble)
library(tidyr)
library(wader)


## Everglades wide

### Data

everglades_counts <- everglades_counts_all
everglades_water <-everglades_water_all
count_env_data <- everglades_counts |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(everglades_water, by = "year") |>
  mutate(time = year - min(year) + 1, series = factor(species))

data_train <- filter(count_env_data, year < 2018)
data_test <- filter(count_env_data, year >= 2018)


### Model


plot_mvgam_series(data = data_train, y = "count", series = "all")


priors <- get_mvgam_priors(
  formula = count ~ 1,
  trend_formula = ~ s(pre_recession, trend, bs = "re"),
  trend_model = "VAR1",
  family = nb(),
  data = data_train
)

#priors <- prior(beta(10, 10), class = sigma, lb = 0.2, ub = 1)
priors_test <- c(priors, prior(normal(0, 0.001), class = Intercept))

gam_ar1 = mvgam(
  formula = count ~ 1,
  trend_formula = ~ te(init_depth, recession) +
    s(dry_days, trend, bs = "re") +
    s(recession, trend, bs = "re"),
  trend_model = "VAR1",
  family = nb(),
  data = data_train,
  newdata = data_test,
  priors = priors,
  chains = 2
)



plot(gam_ar1, type = "forecast", series = 1)

