calcRentide <- function(data, pre) {
  rendite <- c()
  for (x in 2:length(data)-1) {
    a <- (data[x] - data[x-1])/data[x-1]
    rendite <- c(rendite, a)
  }
  return(rendite)
}

f_stock_return <- function(stock_price, stock_mu, stock_sigma){
  #set.seed(42) # for reproducibility
  delta_t <- 1/1 # one period
  for (i in seq(1)){
    epsilon <- runif(n=1, min=0, max=1) # random generated number
    # calculate stock price (using quantile function of normal distribution)
    stock_price <- stock_price * (1 + qnorm(epsilon,
                                            stock_mu * delta_t,
                                            stock_sigma* sqrt(delta_t)))
  }
  return(stock_price)
}

f_stock_return <- function(price, stock_mu, stock_sigma){
  #set.seed(10000000) # for reproducibility
  epsilon <- runif(n=1, min=-1, max=1) # random generated number
  # calculate stock price (using quantile function of normal distribution)
  stock_price <- price * (1 + stock_mu + stock_sigma * epsilon)
  return(stock_price)
}

simulation <- function(simulations, stock_price, stock_mu, stock_sigma) {
  stock_prices <- c()
  rendite <- c()
  stock_prices <- c(stock_price)
  for (i in seq(simulations-1)){
    stock_prices <- c(stock_prices, f_stock_return(stock_price,stock_mu,stock_sigma))
    rendite <- calcRentide(stock_prices)
    if (length(rendite) > 1) {
      #stock_mu <- mean(rendite)
      stock_sigma <- sd(rendite)
    }
  }
  return(stock_prices)
}

