
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

water_year <- as.data.frame(water_year)


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
                dplyr::filter(water_depth <= 10) |> 
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
             linewidth = 1.5, 
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

#alpha = 2, beta = .5, gamma = .2, delta = .6)

PrPred(3, 1, 3, 3)
PrPred(3, 0.5, 2, 2)
PrPred(2, 0.5, 2, 2)
PrPred(1, 0.5, 2, 2)

# Lorenz Attractor Equations --------------------------------------------------------


parameters <- c(s = 10, r = 28, b = 8/3)
state <- c(X = 0, Y = 1, Z = 1)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- s * (Y - X)
    dY <- X * (r - Z) - Y
    dZ <- X * Y - b * Z
    list(c(dX, dY, dZ))
  })
}


times <- seq(0, 50, by = 0.01)

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "Y"], out[, "Z"], pch = ".", type = "l")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)




# Turing patterns  --------------------------------------------------------

# https://github.com/ijmbarr/turing-patterns/blob/master/turing-patterns.ipynb
# file:///C:/Users/alexanderblochel/OneDrive%20-%20University%20of%20Florida/Desktop/books/b978-0-12-397014-5.00001-8.pdf





# fourier transformations  ------------------------------------------------


# https://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html



xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)


wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
par(mfrow=c(1,1))
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)


wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)

#Some concepts:
  
#The fundamental period, T, is the period of all the samples taken, 
#the time between the first sample and the last
#The sampling rate, sr, is the number of samples taken over a time 
#period (aka acquisition frequency). For simplicity we will make the 
#time interval between samples equal. This time interval is called the 
#sample interval, si, which is the fundamental period time divided by 
#the number of samples N. So, si=TN
#The fundamental frequency, f0, which is 1T. The fundamental frequency 
#is the frequency of the repeating pattern or how long the wavelength 
#is. In the previous waves, the fundamental frequency was 12π. The 
#frequencies of the wave components must be integer multiples of the 
#fundamental frequency. f0 is called the first harmonic, the second 
#harmonic is 2∗f0, the third is 3∗f0, etc.


repeat.xs     <- seq(-2*pi,0,pi/100)
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)

#wave.3 is the weighted sum of wave.1 and wave.2. This equation is 
#the Fourier Series for wave.3


plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}

# An eg
plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 





acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c(.5,.25)    # strength of signal components

f <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts)  


#phase shifts

component.delay <- c(pi/2,0)       # delay of signal components (radians)
plot.fourier(f,f.0,ts)


#DC components

dc.component <- -2
plot.fourier(f,f.0,ts)



#Fourier Transform
#Here are two egs of use, a stationary and an increasing trajectory:

library(stats)
fft(c(1,1,1,1)) / 4  # to normalize

fft(1:4) / 4  


#Cycle’s properties
# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

convert.fft(fft(1:4))





# returns the x.n time series for a given time sequence (ts) and
# a vector with the amount of frequencies k in the signal (X.k)
get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  
  x.n * acq.freq 
}



plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}






X.k <- fft(c(4,0,0,0))                   # get amount of each frequency k

time     <- 4                            # measuring time interval (seconds)
acq.freq <- 100                          # data acquisition frequency (Hz)
ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 

x.n <- get.trajectory(X.k,ts,acq.freq)   # create time wave

plot(ts,x.n,type="l",ylim=c(-2,4),lwd=2)
abline(v=0:time,h=-2:4,lty=3); abline(h=0)

plot.harmonic(X.k,1,ts,acq.freq,"red")
plot.harmonic(X.k,2,ts,acq.freq,"green")
plot.harmonic(X.k,3,ts,acq.freq,"blue")


#EXAMPLES 


acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,.5,.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)




w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
head(trajectory,n=30)




X.k <- fft(trajectory)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,20))




x.n <- get.trajectory(X.k,ts,acq.freq) / acq.freq  # TODO: why the scaling?
plot(ts,x.n, type="l"); abline(h=0,lty=3)
points(ts,trajectory,col="red",type="l") # compare with original




plot.show <- function(trajectory, time=1, harmonics=-1, plot.freq=FALSE) {
  
  acq.freq <- length(trajectory)/time      # data acquisition frequency (Hz)
  ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
  
  X.k <- fft(trajectory)
  x.n <- get.trajectory(X.k,ts, acq.freq=acq.freq) / acq.freq
  
  if (plot.freq)
    plot.frequency.spectrum(X.k)
  
  max.y <- ceiling(1.5*max(Mod(x.n)))
  
  if (harmonics[1]==-1) {
    min.y <- floor(min(Mod(x.n)))-1
  } else {
    min.y <- ceiling(-1.5*max(Mod(x.n)))
  }
  
  plot(ts,x.n, type="l",ylim=c(min.y,max.y))
  abline(h=min.y:max.y,v=0:time,lty=3)
  points(ts,trajectory,pch=19,col="red")  # the data points we know
  
  if (harmonics[1]>-1) {
    for(i in 0:length(harmonics)) {
      plot.harmonic(X.k, harmonics[i], ts, acq.freq, color=i+1)
    }
  }
}



trajectory <- 4:1
plot.show(trajectory, time=2)


trajectory <- c(rep(1,5),rep(2,6),rep(3,7))
plot.show(trajectory, time=2, harmonics=0:3, plot.freq=TRUE)


trajectory <- c(1:5,2:6,3:7)
plot.show(trajectory, time=1, harmonics=c(1,2))



set.seed(101)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)
trajectory <- 3*rnorm(101) + 3*sin(3*w*ts)
plot(trajectory, type="l")




X.k <- fft(trajectory)
plot.frequency.spectrum(X.k,xlimits=c(0,acq.freq/2))



library(GeneCycle)

f.data <- GeneCycle::periodogram(trajectory)
harmonics <- 1:(acq.freq/2)

plot(f.data$freq[harmonics]*length(trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")



trajectory1 <- trajectory + 25*ts # let's create a linear trend 
plot(trajectory1, type="l")



f.data <- GeneCycle::periodogram(trajectory1)
harmonics <- 1:20
plot(f.data$freq[harmonics]*length(trajectory1), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")



trend <- lm(trajectory1 ~ts)
detrended.trajectory <- trend$residuals
plot(detrended.trajectory, type="l")



f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:20
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")




library(zoo) # use: index() converts Date to its index 

everglades_counts <- tibble(wader::max_counts(level = "all")) |> 
  group_by(year) |> 
  summarise(count = sum(count)) |> 
  ungroup()




everglades_counts <- everglades_counts[order(nrow(everglades_counts):1),]  # revert data frame
plot(everglades_counts, type="l")

trend <- lm(count ~ index(year), data = everglades_counts)
abline(trend, col="red")

detrended.trajectory <- trend$residuals
plot(detrended.trajectory, type="l", main="detrended time series")



f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:20 
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")


# migration  --------------------------------------------------------------


#Cagnacci et al. 2014, Berg et al. 2019 Partial migration 

#migrants vs residents 
#summer vs winter range 
# sympatric winter range
# switching among tactics 


#dynamic forage model, water dynamics and pred prey interactions 
# wetness? 






