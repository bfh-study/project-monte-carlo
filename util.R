
concatKeyValuePair <- function(key, value) {
  return(paste(key, value, sep='='))
}

concatUrlParams <- function(param1, param2) {
  return(paste(param1, param2, sep='&'))
}

loadData <- function(symbol, path = './data') {
  filePath <- paste(path, '/', symbol, '.csv', sep='')
  if (file.exists(filePath)) {
    loadData <- read.csv(filePath)
  } else {
    url <- 'https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&datatype=csv&outputsize=full'
    url <- concatUrlParams(concatUrlParams(url, concatKeyValuePair('symbol', symbol)), concatKeyValuePair('apikey', get('API_KEY')))
    loadData <- read.csv(url(url))
    write.csv(loadData, filePath) 
  }
  
  return(loadData)
}


plotSimulations <- function(data, continuous, discrete, days, sim_days, outrange) {
  # Create Line Chart
  d <- data.frame(X=c(seq(days-sim_days,days-1)))
  
  # get the range for the x and y axis
  xrange <- range(seq(1, days))
  yrange <- range(c(range(data)-80, data, range(data)+80))
  plot(xrange, yrange, type="n", xlab="Tage",ylab="Preis" ) 
  
  # continuous lines
  color = 5
  for (prices in discrete) {
    lines(y=prices, x=d$X, type = "l", col = "green")
    color = color + 1
  }
  
  # discrete lines
  for (prices in continuous) {
    lines(y=prices, x=d$X, type = "l", col = "blue",  lwd=2)
  }
  
  # effective line
  lines(data, type = "l", col = 1, lwd=2) # Color 1 = black
  
  out <- outrange(discrete)
  b <- format(mu, digits=3)
  c <- format(sigma, digits=3)
  
  mtext(paste("sim=",sim_count, ",",  out, "% lines out,", "epsilon(blue)=", sim_epsilon, ",mu", b,",sigma", c , sep=" "))
  
}



