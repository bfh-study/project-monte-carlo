#############################################################
##                       MAIN-FILE                         ##
#############################################################


# load util functions
source("util.R")

# loadData('KO')
allData <- loadData('SCMWY')

# example of how you can create a line chart
# data2017 <- subset(allData, as.Date(timestamp) > as.Date('2016-12-31') & as.Date(timestamp) < as.Date('2018-01-01'))
# # convert column 'timestamp'
# data2017$timestamp <- as.Date(data2017$timestamp)
# # get the range for the y axis
# yrange <- range(data2017$close)
# plot(data2017$timestamp, data2017$close, type='l', ylim = yrange)
# lines(trucks, type="o", pch=22, lty=2, col="red")

data2017 <- subset(allData, as.Date(timestamp) > as.Date('2016-12-31') & as.Date(timestamp) < as.Date('2018-01-01'))
days <- length(data2017$rendite)

allData$open <- as.numeric(allData$open)
allData$close <- as.numeric(allData$close)
allData$rendite <- allData$close - allData$open
mean <- mean(allData$rendite)
sd <- sd(allData$rendite)

#' Stock price calculation
#' 
#' Calculates stock price after n periods using standard stock price model
#' @param stock_price original stock price
#' @param n number of periods
#' @param stock_mu expected percentual stock drift over n periods
#' @param stock_sigma expecter percentual stock volatility
#' @return stock price after n periods
f_stock_return <- function(stock_price, n, stock_mu, stock_sigma){
  delta_t <- 1/n # one period
  for (i in seq(n)){
    epsilon <- runif(n=1, min=0, max=1) # random generated number
    # calculate stock price (using quantile function of normal distribution)
    stock_price <- stock_price * (1 + qnorm(epsilon, 
                                            stock_mu * delta_t, 
                                            stock_sigma* sqrt(delta_t)))
  }
  return(stock_price)
}

gugus <- f_stock_return(data2017$close[[days]], days, mean, sd)


# f_stock_return <- function(stock_price, n, mean, sd){
#   mu <- n * mean
#   sigma <- sqrt(n) * sd
#   delta_t <- 365 / n
#   for (i in seq(n)){
#     epsilon <- runif(n=1, min=0, max=1) # random generated number
#     # calculate stock price (using quantile function of normal distribution)
#     stock_price <- stock_price * (1 + qnorm(epsilon, stock_mu * delta_t, stock_sigma* sqrt(delta_t)))
#   }
#   return(stock_price)
# }

