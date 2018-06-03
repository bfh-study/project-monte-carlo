
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

plotSimulations <- function(data, simulations, days, sim_days) {
  # Create Line Chart
  d <- data.frame(X=c(seq(days-sim_days,days-1)))
  
  # get the range for the x and y axis
  xrange <- range(seq(1,days))
  yrange <- range(data)
  plot(xrange, yrange, type="n", xlab="Tage",ylab="Preis" ) 
  lines(data, type = "l", col = 1) # Color 1 = black
  
  color = 2
  for (prices in simulations) {
    lines(y=prices, x=d$X, type = "l", col = color)
    color = color + 1
  }
}


