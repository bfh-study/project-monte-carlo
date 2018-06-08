

# TODO: markdown integration für Prüfung




# #############################################################
# ##                       MAIN-FILE                         ##
# #############################################################
#
#
# # load util functions
source("util.R")
source("calc.R")

# example of how you can create a line chart
# data2017 <- subset(allData, as.Date(timestamp) > as.Date('2016-12-31') & as.Date(timestamp) < as.Date('2018-01-01'))
# # convert column 'timestamp'
# data2017$timestamp <- as.Date(data2017$timestamp)
# # get the range for the y axis
# yrange <- range(data2017$close)
# plot(data2017$timestamp, data2017$close, type='l', ylim = yrange)
# lines(trucks, type="o", pch=22, lty=2, col="red")

#allData <- loadData('GOOGL')
#allData <- loadData('KO')
allData <- loadData('AAPL')

# reverse order
allData <- allData[nrow(allData):1,]

# required data intervall
dataIntervall <- subset(allData, as.Date(timestamp) > as.Date('2015-12-31') & as.Date(timestamp) < as.Date('2018-01-01'))

# Data assignment
data <-dataIntervall
#data <-allData

# --------------------------------------
# Configuration: 
# --------------------------------------
# Number of simulation days and siumulations
sim_count <- 100
sim_days <- 250
sd_max = 2
sd_min = -2


# Effective stock prices for the whole period
prices    <- data$open
# number of days in the defined interval, also includes the sumulation dayss
days      <- length(prices)
# The price on the simulation start day
price     <- prices[[days-sim_days]]

# Calculation of the data from the begin of the period until to the start of the simulation
data_until_sim  <- dataIntervall[sim_days:nrow(dataIntervall)-sim_days,,drop=F]$open
return          <- calcReturns(data_until_sim)
mu              <- mean(return)
sigma           <- sd(return)


# #############################################################
# ##                       Simulations                       ##
# #############################################################

#set.seed(377)

# Execute Simulations

continuous <- list()
continuous[[1]]  <- continuousStochastic(price, mu, sigma, sd_max, sim_days)
continuous[[2]]  <- continuousStochastic(price, mu, sigma, 0, sim_days)
continuous[[3]]  <- continuousStochastic(price, mu, sigma, sd_min, sim_days)

discrete <- list()
for (i in 1:sim_count){
  discrete[[i]] <-discreteStochastic(sim_days, price, mu, sigma)
}

# Plot simulations
plotSimulations(prices, continuous, discrete, days, sim_days)


blackscholes <- function(S, X, r, t, sigma) {
  values <- c()
  
  d1 <- (log(S/X) + (r*t + (sigma^2 * t)/2)) / (sigma * sqrt(t))
  d2 <- (log(S/X) + (r*t - (sigma^2 * t)/2)) / (sigma * sqrt(t))
  
  values[1] <- S * pnorm(d1) - X*exp(-r*t) * pnorm(d2)
  values[2] <- (-S * pnorm(-d1) + X*exp(-r*t) * pnorm(-d2))
  
  return (values)
}
strike_price <- continuousStochastic(price, mu, sigma, 0, sim_days)[[sim_days]]
li <- c()
for (i in 1:sim_days) {
  li <- c(li, blackscholes(price,strike_price,0.005,i,sigma)[[1]])
}

# get the range for the x and y axis
xrange <- range(seq(1,sim_days))
yrange <- range(li)
plot(xrange, yrange, type="n", xlab="Tage",ylab="Preis" ) 
lines(li, type = "l", col = 1, lwd=2) # Color 1 = black


print(results)
print(blackscholes(110,100,.05,1,.2))




