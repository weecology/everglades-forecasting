
# simulating data ---------------------------------------------------------

library('ggplot2')



# water depths 



mean_depth = 30
amplitude = 30
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





