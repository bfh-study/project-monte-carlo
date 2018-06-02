
# #############################################################
# ##                       MAIN-FILE                         ##
# #############################################################
#
#
# # load util functions
source("util.R")
source("calc.R")

#allData <- loadData('GOOGL')
allData <- loadData('KO')
#allData <- loadData('AAPL')

# reverse order
allData <- allData[nrow(allData):1,]

# example of how you can create a line chart
# data2017 <- subset(allData, as.Date(timestamp) > as.Date('2016-12-31') & as.Date(timestamp) < as.Date('2018-01-01'))
# # convert column 'timestamp'
# data2017$timestamp <- as.Date(data2017$timestamp)
# # get the range for the y axis
# yrange <- range(data2017$close)
# plot(data2017$timestamp, data2017$close, type='l', ylim = yrange)
# lines(trucks, type="o", pch=22, lty=2, col="red")

dataIntervall <- subset(allData, as.Date(timestamp) > as.Date('2017-09-30') & as.Date(timestamp) < as.Date('2018-01-01'))

# Data assignment
data <-dataIntervall
#data <-allData

# Number of simulation days
sim_days  <- 25
sim_count <- 3

# Data until simulation date. 
# It is used for calculating: return, sample mean and sample standard deviation 
calcData  <- dataIntervall[sim_days:nrow(dataIntervall)-sim_days,,drop=F]
rendite   <- calcRentide(calcData$close)
mean      <- mean(rendite)
sd        <- sd(rendite)

# number of days in the defined interval, also includes the sumulation days
days      <- length(data$X)

# The price on the simulation start day
price     <- data$close[[days-sim_days]]

# Execute Simulations
sim_list <- list()
for (i in seq(0:sim_count)){
  sim_list[[i]] <-simulation(sim_days, price, mean, sd)
}

# Plot simulations
plotSimulations(calcPrices, sim_list, days, sim_days)
