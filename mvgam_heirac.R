
# mvgam_forecasting -------------------------------------------------------


library(mvgam)          
library(tidyverse)      
library(ggplot2)         
library(marginaleffects) 
library(gratia)         
library(parallel)
library(stringr)
library(tibble)
library(tidyr)
library(wader)



everglades_counts_all <- tibble(max_counts(level = "all"))
everglades_counts_all <- everglades_counts_all |>
  filter(species %in% c("gbhe", "greg", "sneg", "whib", "wost", 'rosp')) |>  #, 'rosp' remove ROSP ? ROSP FROM SOUTH, others from North??
  complete(year = full_seq(year, 1), species, fill = list(count = 0)) |>
  ungroup()

everglades_region <- tibble(max_counts(level = "subregion"))
everglades_counts_region <- everglades_region |>
  filter(species %in% c("gbhe", "greg", "sneg", "whib", "wost", 'rosp')) |> #wait with 
  group_by(species) |> 
  complete(year = full_seq(year, 1), region, fill = list(count = 0)) |>
  ungroup() |>
  arrange(year, region)
  # mutate(region = if_else(is.na(region), 
  #                         word(bird_region, 2, sep = fixed('-')), 
  #                         region), 
  #        species = if_else(is.na(species),
  #                          word(bird_region, 1, sep = fixed('-')), 
  #                          species)) #|> 
 # filter(region != '3an' & region != '2a')



# removing 2a and 3an as these don't have WOST - will add them later. 



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
  filter(region != 'inlandenp' &                                                 #removing missing locations
           region !=   'wcas' &
           region != 'all' &
           region != 'coastalenp' ) |>
  mutate(time = year - min(year) + 1, 
         series = as.factor(paste0(region, '_', species)) ) 





table(count_env_data_region$year, 
      count_env_data_region$series) 

table(count_env_data_region$year, 
      count_env_data_region$species) 

# test data structure  ------------------------------------------------



  
  
  
get_mvgam_priors(count ~ 1,
                 data = count_env_data_all,
                 family = gaussian()
)

# get_mvgam_priors(count ~ 1,
#                  data = count_env_data_region,
#                  family = gaussian()
# )



# split data into training and testing sets -------------------------------

#count_env_data_all <- count_env_data_region

data_train_all <- filter(count_env_data_all, year < 2022 )
data_test_all <- filter(count_env_data_all, year >= 2022 )

data_train_region <- filter(count_env_data_region, year < 2022 )
data_test_region <- filter(count_env_data_region, year >= 2022 )


plot_mvgam_series(data = data_train_all,
                  y = 'count',
                  series = 'all') +
  labs(y = 'Count',
       x = 'Time', 
       title = 'all')

plot_mvgam_series(data = data_train_region,
                  y = 'count',
                  series = 'all') +
  labs(y = 'Count',
       x = 'Time', 
       title = 'region')

#features of series all
unique(data_train_all$series)
plot_mvgam_series(data = data_train_all,
                  newdata = data_test_all,
                  y = 'count',
                  series = 1)

plot_mvgam_series(data = data_train_all,
                  newdata = data_test_all,
                  y = 'count',
                  series = 2)

plot_mvgam_series(data = data_train_all,
                  newdata = data_test_all,
                  y = 'count',
                  series = 3)

plot_mvgam_series(data = data_train_all,
                  newdata = data_test_all,
                  y = 'count',
                  series = 4)

plot_mvgam_series(data = data_train_all,
                  newdata = data_test_all,
                  y = 'count',
                  series = 5)

plot_mvgam_series(data = data_train_all,
                  newdata = data_test_all,
                  y = 'count',
                  series = 6)





# fit hierarchical  --------------------------------------------------------

ggplot(data_train_all,
       aes(x = count)) +
  geom_histogram(col = 'white',
                 fill = 'darkgreen') +
  labs(y = 'Frequency',
       x = 'count')
summary(data_train_all$count)

#gamma distribution (probably?)

?mgcv::gam.models
?mgcv::factor.smooth.interaction
?mvgam::mvgam_formulae
?mvgam::get_mvgam_priors
?brms::prior


# birds share a latent state
# each bird also responds differently to the food availability 

# priors ------------------------------------------------------------------

def_priors <- get_mvgam_priors(
  # Observation formula containing region-level intercepts
  formula = count ~ series,
  
  # Process model that contains the hierarchical temporal smooths
  trend_formula = ~
    0 +
    
    # Shared smooth of year for all series
    s(year, 
      k = 30, 
      bs = 'cr') +
    
    # Deviation smooths for each series
    s(year, 
      trend, 
      k = 15, 
      bs = 'sz', 
      xt = list(bs = 'cr')),
  data = data_train_all,
  family = nb()
)
View(def_priors)

sigma_prior <- prior(beta(10, 10), class = sigma, lb = 0.2, ub = 1)

intercept_prior <- prior(normal(0, 0.001), class = Intercept)

# ndvi_random_slopes_prior <- prior(
#   inv_gamma(2.3693353, 0.7311319),                              #should not be gamma if family = nb()
#   class = sigma_raw_trend)

ar_sp_intercept_prior <- prior(std_normal(), class = b)

gam_var_priors <- c(ar_sp_intercept_prior, intercept_prior)

# Fit the model
mod1 <- mvgam(
  # Observation formula containing region-level intercepts
  formula = count ~ series,
  
  # Process model that contains the hierarchical temporal smooths
  trend_formula = ~
    0 + #think adding 0+ makes it hierarchical. not sure

    # Shared smooth of x for all series
    s(init_depth, 
      bs = 'cr') +
    
    # Shared smooth of x for all series
    s(dry_days, 
      bs = 'cr') +
    
    # Deviation smooths for each series
    s(dry_days,
      trend,
      bs = 'sz',
      xt = list(bs = 'cr'))+
    
    # Shared smooth of x for all series
    s(breed_season_depth, 
      bs = 'cr') +
   # 
   # # Deviation smooths for each series
    s(breed_season_depth,
      trend,
      bs = 'sz',
      xt = list(bs = 'cr'))+

  
    # Shared smooth of x for all series
    s(recession, 
      bs = 'cr') +

     # Deviation smooths for each series
     s(recession,                                       #removing this makes R_hat larger
       trend,
       bs = 'sz',
       xt = list(bs = 'cr'))
    ,
  
  trend_model = CAR(), 

  
  
  # Updated prior distributions for the series-level 
  # intercepts using brms::prior()
  priors = sigma_prior,
 
  # Training and testing data in mvgam's long format
  data = data_train_all,
  newdata = data_test_all,
  
  # nb observation model
  family = nb(),
  
  # Each series shares the same nb shape parameter
  # If TRUE and the family has additional 
  # family-specific observation parameters (e.g., 
  # variance components, dispersion parameters), 
  # these will be shared across all outcome variables. 
  # Useful when multiple outcomes share properties. 
  share_obs_params = TRUE,
  
  # Non-centring the latent states tends to improve
  # performance in State Space models
  noncentred = TRUE,
  backend = 'cmdstanr'
)




# subgr	
# A subgrouping factor variable specifying which element 
# in data represents the different time series. Defaults 
# to series, but note that models that use the hierarchical 
# correlations, where the subgr time series are measured in 
# each level of gr, should not include a series element in 
# data. Rather, this element will be created internally 
# based on the supplied variables for gr and subgr.
# 
# For example, if you are modelling temporal counts for a 
# group of species (labelled as species in data) across 
# three different geographical regions (labelled as region), 
# and you would like the residuals to be correlated within 
# regions, then you should specify gr = region and 
# subgr = species. Internally, mvgam() will create the 
# series element for the data using:
#   series = interaction(group, subgroup, drop = TRUE)


#zero inflated NB - needed? - probably if we look at the regions 





# Look at the structure of the object
?mvgam::`mvgam-class`
methods(class = "mvgam")

# Look at the Stan code to better understand the model
stancode(mod1)

# Generate a methods skeleton with references
how_to_cite(mod1)


# Diagnostics
summary(mod1)
summary(mod1,
        include_betas = FALSE,
        smooth_test = FALSE)
mcmc_plot(mod1,
          type = 'rhat_hist')
mcmc_plot(mod1,
          variable = 'obs_params',
          type = 'trace')
mcmc_plot(mod1,
          variable = 'sigma',
          regex = TRUE,
          type = 'trace')


mcmc_plot(mod1, variable = 'delta_trend', regex = TRUE) +
  scale_y_discrete(labels = mod1$trend_model$autoregressive_coef) +
  labs(
    y = 'Potential changepoint',
    x = 'Rate change'
  )


# Draw the individual component smooths
gratia::draw(mod1, trend_effects = TRUE)

# Inspect estimated effects on the outcome scale ...
conditional_effects(mod1)

# ... and on the link scale
conditional_effects(mod1,
                    type = 'link')

marginaleffects::avg_predictions(mod1, 
                                 variable = 'series')


# look at model fit
pp_check(mod1,
         type = "ecdf_overlay_grouped",
         group = "series",
         ndraws = 50)
pp_check(mod1,
         type = "dens_overlay_grouped",
         group = "series",
         ndraws = 50)


# Conditional posterior predictive checks
hcs <- hindcast(mod1)
class(hcs)
?mvgam::`mvgam_forecast-class`
methods(class = "mvgam_forecast")


# Inspect forecasts, which were already computed by the
# model for the test data
fcs <- forecast(mod1)
class(fcs)
layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(hcs, series = 1)
plot(hcs, series = 2)
plot(hcs, series = 3)
plot(hcs, series = 4)

layout(1)

# Another way to look at forecasts for this example
plot_predictions(mod1, 
                 newdata = count_env_data_all,
                 by = c('time', 'series', 'series'),
                 points = 0.5) +
  geom_vline(xintercept = max(data_train_all$time),
             linetype = 'dashed') +
  geom_point(aes(x = time, y = count), 
             data = data_test_all)

plot(mod1, type = "forecast", series = 1)
plot(mod1, type = "forecast", series = 2)
plot(mod1, type = "forecast", series = 3)
plot(mod1, type = "forecast", series = 4)
plot(mod1, type = "forecast", series = 5)
plot(mod1, type = "forecast", series = 6)


# Dynamic trend extrapolations
fc <- forecast(
  mod1,
  type = 'trend' #“link”, “response”, “trend”, “expected”, “detection”, “latent_N”
)
plot(fc, series = 1)
plot(fc, series = 2)
plot(fc, series = 3)
plot(fc, series = 4)
plot(fc, series = 5)
plot(fc, series = 6)

