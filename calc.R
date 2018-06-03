
calcReturns <- function(data) {
  return <- c()
  for (x in 2:length(data)-1) {
    a <- (data[x] - data[x-1])/data[x-1]
    return <- c(return, a)
  }
  return(return)
}

discreteStochastic <- function(days, price, mu, sigma) {
  prices <- c(price)
  for (i in 2:(days)) {
    epsilon <- runif(n=1, min=-1, max=1)
    price = price*(1 + mu + sigma * epsilon)
    prices <- c(prices, price)
    
    # uncomment the block to calculate new sigma
    # return <- calcReturns(c(data_until_sim, prices))
    # if (length(return) > 1) {
    #   #mu <- median(return)
    #   sigma <- sd(return)
    # }
  }
  return(prices)
}

continuousStochastic <- function(price, mu, sigma, epsilon, days, period=365){
  prices <- c()
  for (t in 0:(days-1)){
    
    # uncomment the block to calculate the factor period/days
    # eg. 365/250*t
    #t <- t * (period / days)
    p <- price * exp((mu-(sigma^2)/2)*t+sigma*epsilon*sqrt(t))
    prices <- c(prices,  p)
  }
  return(prices)
}
