#############################################################
##                       MAIN-FILE                         ##
#############################################################


# load util functions
source("util.R")

# loadData('KO')
allData <- loadData('SCMWY')
data2017 <- subset(allData, as.Date(timestamp) > as.Date('2016-12-31') & as.Date(timestamp) < as.Date('2018-01-01'))
