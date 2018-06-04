
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
    epsilon <- runif(n=1, min=-1.5, max=1.5)
    price = price*(1 + mu + sigma * epsilon)
    prices <- c(prices, price)
  }
  return(prices)
}

continuousStochastic <- function(price, mu, sigma, epsilon, days, period=365){
  prices <- c()
  for (t in 0:(days-1)) {
    p <- price * exp((mu-(sigma^2)/2)*t+sigma*epsilon*sqrt(t))
    prices <- c(prices,  p)
  }
  return(prices)
}
