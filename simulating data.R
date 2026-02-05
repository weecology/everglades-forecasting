
# simulating data ---------------------------------------------------------

library('deSolve')
library('ggplot2')




# Water Depths  -----------------------------------------------------------


mean_depth = 40
amplitude = 40
maxday = 150
sd = 0

water_year <- data.frame(day = integer(), water_depth = numeric())
days = 1:365




x <- rnorm(5, mean=3, sd=2)

for (i in seq_along(days)) {
  water_year[i, ] <- c(days[i],
                       rnorm(
                         days[i], 
                         mean = mean_depth + amplitude *
                           cos(2 * pi * ((days[i] - maxday) / 365)),
                         sd = sd))
}




water_year |> 
  ggplot(aes(x = day, y = water_depth)) +
  geom_line(linewidth = 2) +
  geom_hline(yintercept = c(20,0), 
             color = 'red') + 
  geom_hline(yintercept = 10, 
             color = 'blue') +
  theme_bw() +
  geom_text(x=330, y=55, size = 5, 
             label = paste(
               'Mean annual depth = ', mean_depth,'\n', 
               'Amplitude of cos = ', amplitude,'\n', 
               'Max waterlevel day = ', maxday,'\n', 
               'Generated numbers sd = ', sd
             ))+ 
  geom_text(x=350, y=11, size = 5, color = 'blue',
            label = paste(
              'Dry Days: ', 
              water_year |> 
                filter(water_depth <= 10) |> 
                nrow()
            ))+ 
  theme(text = element_text(size = 20)) 




# Lotka Volterra - model --------------------------------------------------




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

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)



