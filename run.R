source("dataprepare.R")
source("predict.R")

#initialization variables
country="esp" # eng/esp/ger/fra/ita
years = c(2014:2017) # (1994-2017)

theData <- data.load()
DataPrepared <- data.prepare(theData)
DataSeasonly <- data.seasonly(DataPrepared)
DataOverall <- data.accum(DataSeasonly)

#clubs existence
clubsVector <-(unique(as.character( DataSeasonly$Team)))

simParams <- makePrediction(DataPrepared,"Chelsea","Tottenham")

Simulate(simParams)
