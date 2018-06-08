
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

outrange <- function(discrete) {
  out <- 0
  for (dx in discrete) {
    c1 <- continuous[[1]]
    c2 <- continuous[[3]]
    for (z in seq(length(c1))) {
      if (dx[z] > c1[z] || dx[z] < c2[z]) {
        out = out + 1
        break
      }
    }
  }
  x <- 100/sim_count * out
  return(x)
}

# Black-Scholes Option Value
# S = Spot price
# X = Strike price
# r = Riskfree Interest Rate
# t = Time to Maturity
# sigma = Volatility
blackscholes <- function(S, X, r, t, sigma) {
  values <- c()
  
  d1 <- (log(S/X) + (r*t + (sigma^2 * t)/2)) / (sigma * sqrt(t))
  d2 <- (log(S/X) + (r*t - (sigma^2 * t)/2)) / (sigma * sqrt(t))
  
  values[1] <- S * pnorm(d1) - X*exp(-r*t) * pnorm(d2)
  values[2] <- (-S * pnorm(-d1) + X*exp(-r*t) * pnorm(-d2))
  
  return (values)
}
