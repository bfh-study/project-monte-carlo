

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
allData <- loadData('KO')
#allData <- loadData('AAPL')

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
sim_count <- 1000
sim_days <- 250
sim_sd = 1.3 #  1=70%


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

#set.seed(133)

# Execute Simulations

continuous <- list()
continuous[[1]]  <- continuousStochastic(price, mu, sigma, sim_sd, sim_days)
continuous[[2]]  <- continuousStochastic(price, mu, sigma, 0, sim_days)
continuous[[3]]  <- continuousStochastic(price, mu, sigma, -sim_sd, sim_days)

discrete <- list()
for (i in 1:sim_count){
  discrete[[i]] <-discreteStochastic(sim_days, price, mu, sigma)
}

# Plot simulations
plotSimulations(prices, continuous, discrete, days, sim_days, outrange)


lnMean = price*exp(mu*days)
lnSD = price*exp(mu*days)*sqrt(exp((sigma^2)*days)-1)

meanOfLog = log(price) + (mu-(sigma^2)/2)*sim_days
sdOfLog = sigma*sqrt(sim_days)
priceGrid = seq(0,mu+10*lnSD,length=1000)
theoreticalDens = dlnorm(priceGrid,meanOfLog, sdOfLog)

#xrange <- range(priceGrid)
#yrange <- range(theoreticalDens)
S = matrix(0,nrow=sim_days-1,ncol=sim_count-1)

S <- matrix(unlist(discrete), byrow=TRUE, nrow=length(discrete) )
empiricalDens = density(S)#S[sim_days,])
plot(priceGrid,theoreticalDens,type='l',xlab='Prices',ylab='Density')
lines(empiricalDens,col='blue')











