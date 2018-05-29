
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
