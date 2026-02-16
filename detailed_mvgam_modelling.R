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





# add water data ----------------------------------------------------------


water <- load_datafile("eden_covariates.csv")
everglades_water_all <- filter(water, region == "all") 

count_env_data_all <- everglades_counts_all |>
  filter(year >= 1991) |> # No water data prior to 1991
  full_join(everglades_water_all, by = "year") |>
  drop_na(species) |>
  mutate(time = year - min(year) + 1, 
         series = factor(species) ) 




get_mvgam_priors(count ~ 1,
                 data = count_env_data_all,
                 family = gaussian()
)




# split data into training and testing sets -------------------------------

#count_env_data_all <- count_env_data_region

data_train <- filter(count_env_data_all, year < 2022 )
data_test <- filter(count_env_data_all, year >= 2022 )

plot_mvgam_series(data = data_train,
                  y = 'count',
                  series = 'all') +
  labs(y = 'Count',
       x = 'Time', 
       title = 'all')

#features of series all
unique(data_train$series)
plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'count',
                  series = 1)

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'count',
                  series = 2)

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'count',
                  series = 3)

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'count',
                  series = 4)

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'count',
                  series = 5)

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'count',
                  series = 6)

# fit hierarchical  --------------------------------------------------------

ggplot(data_train,
       aes(x = count)) +
  geom_histogram(col = 'white',
                 fill = 'darkgreen') +
  labs(y = 'Frequency',
       x = 'count')
summary(data_train$count)

#gamma distribution (probably?)

?mgcv::gam.models
?mgcv::factor.smooth.interaction
?mvgam::mvgam_formulae
?mvgam::get_mvgam_priors
?brms::prior



# priors ------------------------------------------------------------------

def_priors <- get_mvgam_priors(
  # Observation formula containing species intercepts
  formula = count ~ series,
  
  # Process model that contains the hierarchical temporal smooths
  trend_formula = ~
    0 +
    
    # Shared smooth of year for all series
    s(year, 
      # there are 13 years in the training data
      k = 13, 
      bs = 'cr') +
    
    # Deviation smooths for each species
    s(year, 
      trend, 
      # there are 13 years in the training data
      k = 13, 
      bs = 'sz', 
      xt = list(bs = 'cr')),
  data = data_train,
  family = nb()
)

def_priors

sigma_prior <- prior(beta(10, 10), class = sigma, lb = 0.2, ub = 1)
intercept_prior <- prior(normal(0, 0.001), class = Intercept)
ar_sp_intercept_prior <- prior(std_normal(), class = b)

gam_var_priors <- c(sigma_prior, ar_sp_intercept_prior, intercept_prior)

data_train |> 
  filter(count != 0) |> 
  #filter(species != 'rosp')  |> 
  as.data.frame()  |> 
  ggplot(aes( recession, count)) +
  geom_point() +
  facet_wrap(~species, 
             scales = 'free')

# model -------------------------------------------------------------------

# Fit the model
mod1 <- mvgam(
  # Observation formula containing region-level intercepts
  formula = count ~ series,
  
  # Process model that contains the hierarchical temporal smooths
  trend_formula = ~
    #0 + #think adding 0+ makes it hierarchical. not sure
    
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
  
  trend_model = VAR(), 
  
  trend_map =
    # trend_map forces species to track same /different latent signals
    data.frame(
      series = unique(data_train$series),
      trend = c(1, 2, 3, 4, 5, 6)
    ),
  
  
  # Updated prior distributions for the series-level 
  # intercepts using brms::prior()
  priors = def_priors,
  
  # Training and testing data in mvgam's long format
  data = data_train,
  newdata = data_test,
  
  # nb observation model
  family = nb(),
  
  
  control = list(max_treedepth = 12,   #10 
                 adapt_delta = 0.9),   #0.8
  
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
  backend = 'cmdstanr',
  
  
  
  samples = 1500
)




# model evaulations -------------------------------------------------------




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


plot(mod1, type = "trend", series = 1)
plot(mod1, type = "trend", series = 2)
plot(mod1, type = "trend", series = 3)
plot(mod1, type = "trend", series = 4)
plot(mod1, type = "trend", series = 5)
plot(mod1, type = "trend", series = 6)


# Dynamic trend extrapolations
fc <- forecast(
  mod1,
  type = 'trend') #“link”, “response”, “trend”, “expected”, “detection”, “latent_N”

# ) +
#   ggplot2::geom_point(data = 
#                        data.frame(time = 1:32, 
#                                   y = true_signal), 
#                      mapping = ggplot2::aes(x = time, 
#                                             y = y))
plot(fc, series = 1)
plot(fc, series = 2)
plot(fc, series = 3)
plot(fc, series = 4)
plot(fc, series = 5)
plot(fc, series = 6)



augment(mod1, robust = TRUE, probs = c(0.25, 0.75))
