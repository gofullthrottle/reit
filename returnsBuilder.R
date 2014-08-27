library(plyr)
library(quantmod)

#"constants" to be maniupulated and applied within the code
# change date here
setwd("C:/Users/Kevin/Documents/Projects/R/Shiny/REIT/CorrelationApp_v0")
fileName = "monthlyReturns.csv"
endDate <- Sys.Date()
startDate <- "1999-01-01"

tickers <- read.csv("data/tickers.csv", stringsAsFactors = FALSE)
reits <- tickers$reits

dfReturns <- data.frame()

for(j in 1:length(reits))
{
  print(paste0("-Current REIT: ", reits[j]))
    
  # downloads prices
  dailyPrices <- getSymbols(reits[j], from = startDate, to = endDate, warnings = FALSE, auto.assign=FALSE)
  
  # use this variable to strip only the Adjusted Closes from Yahoo Finance
  # not relevant if using a different source, or if Yahoo changes their format
  k = 6
  dailyPrices <- dailyPrices[,k]
  
  # for each stock, calculate monthly return and append it to the matrix.    
  monthlyReturns <- monthlyReturn(dailyPrices)
  
  if(nrow(dfReturns)==0)
    dfReturns <- data.frame("date" = index(monthlyReturns), "V2" = monthlyReturns)
  
  else
  {
    df2 <- data.frame("date" = index(monthlyReturns), "V2" = monthlyReturns)
    dfReturns <- merge(dfReturns, df2, by = "date", all = TRUE)
  }
  colnames(dfReturns)[j+1] <- reits[j]
}

write.csv(dfReturns, file="data/monthlyReturns.csv", row.names = FALSE)

