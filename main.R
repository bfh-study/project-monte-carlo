

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


discreteStochastic <- function(days, price, mu, sigma) {
  prices <- c(price)
  for (i in 2:(days)) {
    epsilon <- runif(n=1, min=-1, max=1)
    a <- rnorm(1)
    price <- price*(1 + mu + sigma * a)
    prices <- c(prices, price)
  }
  return(prices)
}

# --------------------------------------
# Configuration: 
# --------------------------------------
# Number of simulation days and siumulations
sim_count <- 1000
sim_days <- 250
sim_epsilon = 2.5 #  1=70%

# Effective stock prices for the whole period
prices    <- data$open
# number of days in the defined interval, also includes the sumulation dayss
days      <- length(prices)
# The price on the simulation start day
price     <- prices[[days-sim_days]]

# Calculation of the data from the begin of the period until to the start of the simulation
data_until_sim  <- dataIntervall[sim_days:nrow(dataIntervall)-sim_days,,drop=F]$open
v          <- calcReturns(data_until_sim)
mu              <- mean(return)
sigma           <- sd(return)



# #############################################################
# ##                       Simulations                       ##
# #############################################################

set.seed(3)

# Execute Simulations

continuous <- list()
continuous[[1]]  <- continuousStochastic(price, mu, sigma, sim_epsilon, sim_days)
continuous[[2]]  <- continuousStochastic(price, mu, sigma, 0, sim_days)
continuous[[3]]  <- continuousStochastic(price, mu, sigma, -sim_epsilon, sim_days)

discrete <- list()
for (i in 1:sim_count){
  discrete[[i]] <-discreteStochastic(sim_days, price, mu, sigma)
}

# # # Plot simulations
plotSimulations(prices, continuous, discrete, days, sim_days, outrange)

# --------------------------------------
# Log-Normalverteilung: 
# --------------------------------------
S <- matrix(unlist(discrete), byrow=TRUE, nrow=length(discrete))

lnMean = price*exp(mu*sim_days)
lnSD = price*exp(mu*sim_days)*sqrt(exp((sigma^2)*sim_days)-1)

meanOfLog = log(price) + (mu-(sigma^2)/2)*sim_days
sdOfLog = sigma*sqrt(sim_days)
#priceGrid = seq(0,mu+10*lnSD,length=1000)

priceGrid = seq(0,mu+10*lnSD,length=1000)
theoreticalDens = dlnorm(priceGrid,meanOfLog, sdOfLog)


empiricalDens = density(S[,sim_days])#S[sim_days,])

yrange = seq(0, 0.015, length=1000)
plot(priceGrid,yrange ,type='n',xlab='Preise',ylab='Dichte')

#plot(xrange,yrange,type='n',xlab='Preise',ylab='Dichte')
lines(priceGrid, theoreticalDens,col='black')
lines(empiricalDens,col='blue')



# --------------------------------------
# Log-Normalverteilung Histogram: 
# --------------------------------------
h <- hist(S[,sim_days], breaks = 10, , xlab="Preis",ylab="Dichte", col="green", main="Histogram der Log-Normalverteilten Preise")

xfit <- seq(min(S[,sim_days]), max(S[,sim_days]))
yfit <- dlnorm(xfit, meanlog=meanOfLog, sdlog=sdOfLog) #dlnorm(xfit, mean=mean(S[,sim_days]), sd=sd(S[,sim_days]))
A <- data.frame(yfit)
yfit <- yfit*diff(h$mids[1:2])*length(S[,sim_days])

lines(xfit, yfit,col='black')




# --------------------------------------
# Black Sholes Model: 
# --------------------------------------
bs_lines <- blackscholes_sim(price, mu, sigma, sim_days)

# get the range for the x and y axis
xrange <- range(seq(1,sim_days))
yrange <- range(c(li1,li2))
plot(xrange, yrange, type="n", xlab="Tage",ylab="Preis" )
lines(bs_lines[[1]], type = "l", col = "black", lwd=2) # Color 1 = black
lines(bs_lines[[2]], type = "l", col = "red", lwd=2) # Color 1 = black

print(blackscholes(110,100,.05,1,.2))



