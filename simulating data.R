
# simulating data ---------------------------------------------------------

library('deSolve')
library('ggplot2')
library('sicegar')
library('stats')



# Water Depths  -----------------------------------------------------------


mean_depth = 40
amplitude = 40
maxday = 150
sd = 0
years = 2 

water_year <- data.frame(day = integer(), water_depth = numeric())
days = 1:(365*years)




x <- rnorm(5, mean=3, sd=2)

for (i in seq_along(days)) {
  water_year[i, ] <- c(days[i],
                       rnorm(
                         days[i], 
                         mean = mean_depth + (amplitude  *
                            cos(2 * pi * ((days[i] - maxday) / 365))),
                         sd = sd))
}




water_year |> 
  ggplot(aes(x = day, y = water_depth)) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = c(20,0), 
             color = 'red') + 
  geom_hline(yintercept = 10, 
             color = 'blue') +
  geom_hline(yintercept = 5, 
             color = 'grey') +
  theme_bw() +
  geom_text(x = (365*years), y=75, size = 4, 
             label = paste(
               'Mean annual depth = ', mean_depth,'\n', 
               'Amplitude of cos = ', amplitude,'\n', 
               'Max waterlevel day = ', maxday,'\n', 
               'Generated numbers sd = ', sd
             ))+ 
  geom_text(x = (365*years),  y=11, size = 5, color = 'blue',
            label = paste(
              'Dry Days: ', 
              water_year |> 
                filter(water_depth <= 10) |> 
                nrow()
            )) +
  coord_cartesian(xlim = c(0, (365*years)), # This focuses the x-axis on the range of interest
                  clip = 'off') +   # This keeps the labels from disappearing
  theme(text = element_text(size = 20), 
        plot.margin = unit(c(1,7,1,1), "lines")) 






# fish catch  -------------------------------------------------------------

# include this in large fish line:
# water_year |> 
#   filter(water_depth <= 10) |> 
#   nrow()

depth <- water_year$water_depth

background_bird_take <- 5
background_fish_take <- 1


#simulate intensity data and add noise
noise_parameter <- 0
intensity_noise <- stats::runif(n = length(depth), min = 0, max = 1) * noise_parameter

intensity_b <- sigmoidalFitFormula_h0(depth, 
                                    maximum = 100, 
                                    slopeParam = -0.3, 
                                    midPoint = 30, 
                                    h0 = background_bird_take)

intensity_bird <- intensity_b + intensity_noise


intensity_f <- sigmoidalFitFormula_h0(depth, 
                                      maximum = 95, 
                                      slopeParam = 0.5, 
                                      midPoint = 20, 
                                      h0 = background_fish_take)

intensity_fish <- intensity_f + intensity_noise

dataInput <- data.frame(intensity_bird = intensity_bird, 
                        intensity_fish = intensity_fish, 
                        depth_cm = depth)
  

ggplot(dataInput) +
  geom_vline(xintercept = 10, 
             size = 1.5, 
             color = 'grey') +
  geom_line(aes(x = depth_cm, y = intensity_bird), 
            color = 'blue', linewidth = 2) +
  geom_line(aes(x = depth_cm, y = intensity_fish), 
            color = 'red', linewidth = 2) +
  expand_limits(x = 0, y = 0) + 
  theme_bw() +
  theme(text = element_text(size = 20)) +
  ylab('Foraging Pressure \n on Prey Base Fish (%)') +
  xlab('Depth (cm)') +
  geom_text(x=70, y=98, size = 5, color = 'red',
            label = 'Piscivorous Fish') +
  geom_text(x=70, y=3, size = 5, color = 'blue',
            label = 'Wading Birds')






# Dry Days ----------------------------------------------------------------





# Lotka Volterra - model --------------------------------------------------
#from: https://www.rpubs.com/Jeet1994/Prey-predator-model



LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)

out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

# matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
# legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)


PrPred <- function(a,b,g,d){
  library(deSolve)
  
  Pars <- c(a, b, g, d)
  State <- c(x = 10, y = 10)
  
  
  LotVmod <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dx = x*(a - b*y)
      dy = -y*(g - d*x)
      return(list(c(dx, dy)))
    })
  }
  
  Time <- seq(0, 100, by = 1)
  out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
  
  matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
  legend("topright", c("Prey", "Predator"), lty = c(1,2), col = c(1,2), box.lwd = 0)
  
}

# α - The growth rate of Prey,
# dependent on high or low water levels - birds and large fish 

# β - Rate at which Predators destroy Prey
#high during low low water

# γ - The death rate of predators,
#they disappear and forage elsewhere 

# δ - The rate at which predators increase by consuming prey.


PrPred(5, 1, 5, 3)



