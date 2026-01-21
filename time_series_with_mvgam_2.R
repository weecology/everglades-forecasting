# Introduction to time series modelling in R with mvgam
# Nicholas J Clark
# February 2025


#### Load libraries ####
library(mvgam)           # Fit, interrogate and forecast DGAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(ggplot2)         # Flexible plotting
library(marginaleffects) # Compute conditional and marginal effects
library(gratia)          # Clean plotting of GAM objects


# Set a ggplot theme
# theme_set(
#   theme_classic(base_size = 12, base_family = 'serif') +
#     theme(
#       axis.line.x.bottom = element_line(colour = "black",
#                                         linewidth = 1),
#       axis.line.y.left = element_line(colour = "black",
#                                       linewidth = 1)
#     )
# )
# 

#### Download and inspect data ####
# Load the annual American kestrel, Falco sparverius, abundance 
# time series taken in British Columbia, Canada. These data have 
# been collected annually, corrected for changes in observer 
# coverage and detectability, and logged. They can be found in 
# the MARSS package
load(url('https://github.com/atsa-es/MARSS/raw/master/data/kestrel.rda'))
head(kestrel)

# Arrange the data into a long-format data.frame
regions <- c("BC",
             "Alb",
             "Sask")
model_data <- do.call(rbind,
                      lapply(
                        seq_along(regions),
                        function(x){
                          data.frame(year = kestrel[, 1],
                                     # Reverse logging so that we deal 
                                     # directly with detection-adjusted 
                                     # counts
                                     adj_count = exp(kestrel[, 1 + x]),
                                     region = regions[x])}
                      )
) %>%
  # Add series and time indicators for mvgam modelling
  dplyr::mutate(yearfac = as.factor(year),
                region = as.factor(region),
                series = as.factor(region),
                time = year)

# Inspect modelling data structure
head(model_data)
dplyr::glimpse(model_data)
levels(model_data$series)


#### Split data into training and testing sets ####
data_train <- model_data %>%
  dplyr::filter(year <= 2001)
data_test <- model_data %>%
  dplyr::filter(year > 2001)

# Plot all three time series together
plot_mvgam_series(data = data_train,
                  y = 'adj_count',
                  series = 'all') +
  labs(y = 'Adjusted count',
       x = 'Time') +
  theme_bw(base_size = 12, base_family = 'serif')

# Now plot features for just one series at a time
plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'adj_count',
                  series = 1) #& # use & to add themes to 
  # patchwork::wrap_plots()
  # objects
  # theme_classic(base_size = 12, base_family = 'serif')

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'adj_count',
                  series = 2) #& 
  # theme_classic(base_size = 12, base_family = 'serif')

plot_mvgam_series(data = data_train,
                  newdata = data_test,
                  y = 'adj_count',
                  series = 3) #& 
  # theme_classic(base_size = 12, base_family = 'serif')


#### Fit hierarchical GAMs using mvgam ####
# Look at the distribution of the outcome variable
ggplot(data_train,
       aes(x = adj_count)) +
  geom_histogram(col = 'white',
                 fill = 'darkred') +
  labs(y = 'Frequency',
       x = 'Adjusted count')
summary(data_train$adj_count)

# Heavy-ish tail to the right; perhaps a Gamma distribution
# Inspect default priors for a hierarchical GAM that allows
# a nonlinear temporal trend for each series to vary
# around a common 'shared' trend
?mgcv::gam.models
?mgcv::factor.smooth.interaction
?mvgam::mvgam_formulae
?mvgam::get_mvgam_priors
def_priors <- get_mvgam_priors(
  # Observation formula containing region-level intercepts
  formula = adj_count ~ series,
  
  # Process model that contains the hierarchical temporal smooths
  trend_formula = ~
    0 +
    
    # Shared smooth of year for all series
    s(year, 
      k = 15, 
      bs = 'cr') +
    
    # Deviation smooths for each series
    s(year, 
      trend, 
      k = 10, 
      bs = 'sz', 
      xt = list(bs = 'cr')),
  data = data_train,
  family = Gamma()
)
View(def_priors)

# prior() from brms can be used within mvgam()
# to change default prior distributions
?brms::prior

# Fit the model
mod1 <- mvgam(
  # Observation formula containing region-level intercepts
  formula = adj_count ~ series,
  
  # Process model that contains the hierarchical temporal smooths
  trend_formula = ~
    0 +
    
    # Shared smooth of year for all series
    s(year, 
      k = 15, 
      bs = 'cr') +
    
    # Deviation smooths for each series
    s(year, 
      trend, 
      k = 10, 
      bs = 'sz', 
      xt = list(bs = 'cr')),
  
  # Updated prior distributions for the series-level 
  # intercepts using brms::prior()
  priors = prior(std_normal(),
                 class = b),
  
  # Training and testing data in mvgam's long format
  data = data_train,
  newdata = data_test,
  
  # Gamma observation model
  family = Gamma(),
  
  # Each series shares the same Gamma shape parameter
  share_obs_params = TRUE,
  
  # Non-centring the latent states tends to improve
  # performance in State Space models
  noncentred = TRUE,
  backend = 'cmdstanr'
)

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

# Draw the individual component smooths
gratia::draw(mod1, trend_effects = TRUE)

# Inspect estimated effects on the outcome scale ...
conditional_effects(mod1)

# ... and on the link scale
conditional_effects(mod1,
                    type = 'link')

# Many other types of predictions and contrasts can be 
# made with marginaleffects
marginaleffects::avg_predictions(mod1, 
                                 variable = 'series')

# Unconditional posterior predictive checks to 
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

layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(hcs, series = 1)
plot(hcs, series = 2)
plot(hcs, series = 3)
layout(1)

# Residual checks
plot(mod1, type = 'residuals', series = 1)
plot(mod1, type = 'residuals', series = 2)
plot(mod1, type = 'residuals', series = 3)

# Inspect forecasts, which were already computed by the
# model for the test data
fcs <- forecast(mod1)
class(fcs)
layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(fcs, series = 1)
plot(fcs, series = 2)
plot(fcs, series = 3)
layout(1)

# Another way to look at forecasts for this example
plot_predictions(mod1, 
                 newdata = model_data,
                 by = c('year', 'series', 'series'),
                 points = 0.5) +
  geom_vline(xintercept = max(data_train$time),
             linetype = 'dashed')


#### Expand to a State-Space model with more appropriate nonlinear 
# temporal effects; here we use Gaussian Processes for the 
# shared and deviation effects, which tend to extrapolate
# much better than splines do ####
?brms::gp
?mvgam::AR
def_priors <- get_mvgam_priors(
  formula = adj_count ~ 
    # Observation formula, still only containing region-level intercepts
    series,
  
  # Process model formula, containing hierarchical GPs of time
  trend_formula = ~ 
    0 +
    gp(year, 
       k = 32) + 
    gp(year, 
       by = trend,
       k = 20),
  
  # Correlated AR(1) for short-term temporal 
  # autocorrelation
  trend_model = AR(cor = TRUE),
  data = model_data,
  family = Gamma()
)
View(def_priors)

# Fit the model
mod2 <- mvgam(
  # Observation formula, still only containing region-level intercepts
  formula = adj_count ~ series,
  
  # Process model formula, containing hierarchical GPs of time
  trend_formula = ~ 
    0 +
    gp(year, 
       k = 32, 
       cov = 'exponential') + 
    gp(year, 
       by = trend,
       k = 20, 
       cov = 'exponential'),
  
  # Additional autoregressive dynamics (using a correlated AR(1))
  trend_model = AR(cor = TRUE),
  
  # Updated prior distributions using brms::prior()
  priors = c(prior(beta(3, 10),
                   class = sigma,
                   lb = 0, 
                   ub = 1),
             prior(std_normal(),
                   class = `alpha_gp_trend(year)`),
             prior(std_normal(),
                   class = `alpha_gp_trend(year):trendtrend1`),
             prior(std_normal(),
                   class = `alpha_gp_trend(year):trendtrend2`),
             prior(std_normal(),
                   class = `alpha_gp_trend(year):trendtrend3`),
             prior(normal(0.25, 0.50),
                   class = ar1),
             prior(std_normal(),
                   class = b)),
  
  # Training and testing data in mvgam's long format
  data = data_train,
  newdata = data_test,
  
  # Gamma observation model
  family = Gamma(),
  share_obs_params = TRUE,
  
  # Stan MCMC control for slower but more precise sampling
  control = list(adapt_delta = 0.95)
)

# Inspect the Stan code
stancode(mod2)
how_to_cite(mod2)

# Diagnostics
summary(mod2,
        include_betas = FALSE,
        smooth_test = FALSE)
mcmc_plot(mod2,
          type = 'rhat_hist')
mcmc_plot(mod2,
          variable = c('sigma',
                       'ar1',
                       'shape'),
          regex = TRUE,
          type = 'trace')

# Unconditional posterior check
pp_check(mod2,
         type = "dens_overlay_grouped",
         group = "series",
         ndraws = 50)

# Inferences and unconditional predictions
gratia::draw(mod2, trend_effects = TRUE)
plot_predictions(mod2,
                 condition = c('year', 'series'),
                 type = 'link',
                 conf_level = 0.5)
plot_predictions(mod2,
                 condition = c('year', 'series', 'series'),
                 points = 0.5)
marginaleffects::avg_predictions(mod2,
                                 variable = 'series')

# Inspect the AR1 variance-covariance parameters
Sigma_pars <- matrix(NA,
                     nrow = 3,
                     ncol = 3)
for(i in 1:3){
  for(j in 1:3){
    Sigma_pars[i, j] <- paste0('Sigma[', i, ',', j, ']')
  }
}
mcmc_plot(mod2,
          variable = as.vector(t(Sigma_pars)),
          type = 'hist') +
  geom_vline(xintercept = 0,
             col = 'white',
             linewidth = 2) +
  geom_vline(xintercept = 0,
             linewidth = 1)

# Comparing models using in-sample fit metrics such as PSIS-LOO
# can be a bit deceptive (the highly flexible AR1 process makes it 
# challenging to estimate these metrics)
loo_compare(mod1,
            mod2)

# Look at forecasts from each model and compare
fcs1 <- forecast(mod1)
fcs2 <- forecast(mod2)

layout(matrix(1:2,
              nrow = 2,
              byrow = TRUE))

# plot.new()
# for(x in 1:3){
#   message(paste0('Series ', x, ' splines'))
#   plot(fcs1,
#        series = x)
#   title('Splines of year')
#   
#   message(paste0('Series ', x, ' GPs'))
#   plot(fcs2,
#        series = x)
#   title('GPs of year with AR1 dynamics')
#   message()
# }

#### Other useful inferences from hierarchical nonlinear models ####
# Set a sequence of times over which to generate predictions;
# the higher the value of length.out, the finer the grid and the
# smoother the resulting predictions (at a higher computational
# cost)
year_seq <- seq(min(data_train$year), 
                max(data_train$year), 
                length.out = 100)

# Rate of change over time, averaged over all regions
patchwork::wrap_plots(
  # First a plot of the average temporal trend
  plot_predictions(
    model = mod2, 
    
    # Predict over the sequence of times in 'year_seq'
    newdata = datagrid(year = year_seq,
                       series = unique),
    
    # Use by = 'year' to ensure predictions are averaged
    # over the sequence of times in year_seq
    by = 'year',
    
    # Compute predictions on the link scale
    type = 'link'
  ) +
    labs(y = 'Linear predictor',
         x = ''),
  
  # Now plot the first derivative (slope) of the temporal
  # trend at each time in year_seq
  plot_slopes(
    model = mod2,
    
    # Predict over the sequence of times in 'year_seq'
    newdata = datagrid(year = year_seq,
                       series = unique),
    
    # Compute the slope of the year variable
    variables = 'year',
    
    # Plot with year on the x-axis
    by = 'year',
    
    # Compute predictions on the link scale
    type = 'link'
  ) +
    labs(y = 'Slope of linear predictor',
         x = 'Year') +
    geom_hline(yintercept = 0, linetype = 'dashed'),
  nrow = 2
) + 
  
  # Add an informative title
  patchwork::plot_annotation(
    title = 'Average conditional and marginal effects'
  ) 

# Differences among each series' temporal trend
plot_comparisons(
  model = mod2, 
  
  # Predict over the sequence of times in 'year_seq'
  newdata = datagrid(year = year_seq,
                     series = unique),
  
  # At each time in 'year_seq', calculate contrasts between
  # each pair of time series
  variables = list(series = 'pairwise'),
  
  # Plot the difference trends using year as the x-axis
  by = 'year',
  
  # Compute predictions on the link scale
  type = 'link'
) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(y = "Estimated difference",
       title = "Differences between series' temporal trends")

# When did each series reach peak growth? In other words,
# when was the slope of each series' trend at its highest 
# value?
max_growth = function(hi, lo, x) {
  dydx <- (hi - lo) / 1e-6
  dydx_max <- max(dydx)
  x[dydx == dydx_max][1]
}

plot_comparisons(
  model = mod2,
  
  # Predict over the sequence of times in 'year_seq'
  newdata = datagrid(year = year_seq,
                     series = unique),
  
  # Compare pairs of predicted slopes using the 
  # max_growth() function to identify when the slope was
  # the highest
  comparison = max_growth,
  
  # Compute a forward contrast for a gap of 1e-6
  # years
  variables = list(year = 1e-6),
  
  # Compute the max_growth() of temporal slopes for each
  # series
  by = "series",
  
  # Compute predictions on the link scale
  type = 'link',
  
  # Return credible intervals of 25th and 75th quantiles
  conf_level = 0.5
)

plot_predictions(mod2,
                 condition = c('year', 'series'),
                 type = 'link',
                 conf_level = 0.5)

# Differences in peak growth rate (i.e. velocity) for each
# pairwise comparison
comparisons(model = mod2,
            
            # Predict over the sequence of times in 'year_seq'
            newdata = datagrid(year = year_seq,
                               series = unique),


            # Compute the maximum growth rate for each series'
            # temporal trend
            comparison = \(hi, lo) max((hi - lo) / 1e-6),
            
            # Compute a forward contrast for a gap of 1e-6
            # years
            variables = list(year = 1e-6),
            
            # Compare maximum predicted slope for each
            # series using a pairwise hypothesis test
            by = "series",
            hypothesis = "pairwise",
            
            # Compute predictions on the link scale
            type = 'link')

# To learn more about hierarchical GAMs, have a look at the following
# key references

# Clark, Nicholas J., and Konstans Wells. "Dynamic generalised 
# additive models (DGAMs) for forecasting discrete ecological 
# time series." Methods in Ecology and Evolution 14.3 (2023): 771-784.

# Clark, Nicholas J., et al. "Multi-species dependencies improve 
# forecasts of population dynamics in a long-term monitoring study." 
# ecoevorxiv (2023). DOI: https://doi.org/10.32942/X2TS34

# Pedersen, Eric J., et al. "Hierarchical generalized additive models 
# in ecology: an introduction with mgcv." PeerJ 7 (2019): e6876.