library(zoo)
library(reshape2)

rawMonthlyReturns <- read.csv("data/monthlyReturns.csv", stringsAsFactors = FALSE)
tickers <- read.csv("data/tickers.csv", stringsAsFactors = FALSE)

zooMonthly <- zoo(rawMonthlyReturns[,2:length(rawMonthlyReturns)])
dates <- as.Date(rawMonthlyReturns$date)
index(zooMonthly) <- dates

shinyServer(function(input, output){
  
  listREITs <- reactive({
    tickers$reits[which(tickers$sector == input$sector)]  
  })
  
  output$ui <- renderUI({
    listREITs <- listREITs()
        
    if(is.null(input$sector))
      return()
    
    switch(input$sector,
           "Apts" = checkboxGroupInput("selectedREITs", 
                                       label = "Choose the REITs you want to display",
                                       choices = listREITs,
                                       selected = listREITs[1:length(listREITs)]),
           "Hotels" = checkboxGroupInput("selectedREITs", 
                                       label = "Choose the REITs you want to display",
                                       choices = listREITs,
                                       selected = listREITs[1:length(listREITs)]),
           "Other" = checkboxGroupInput("selectedREITs", 
                                       label = "Choose the REITS you want to display",
                                       choices = listREITs,
                                       selected = listREITs[1:length(listREITs)])
    )
  })
  
  dateRange <- reactive({
    startDate1 <- as.Date(input$dateRange0[1], "%Y-%m-%d")
    endDate1 <- as.Date(input$dateRange0[2], "%Y-%m-%d")
    
    startDate2 <- max(which(as.Date(rawMonthlyReturns$date, "%Y-%m-%d")<startDate1))
    endDate2 <- min(which(as.Date(rawMonthlyReturns$date, "%Y-%m-%d")>endDate1))
    
    dateRange <- seq(startDate2,endDate2)
    
  })
  
  seriesInput <- reactive({
  
    selectedREITs <- which(tickers$reits %in% input$selectedREITs)
    dateRange <- dateRange()    
    
    selectedSeries0 <- zooMonthly[dateRange,c(1,selectedREITs)]
    
#     selectedSeries1 <- zoo(selectedSeries0)
#     dates <- as.Date(rawMonthlyReturns$date[dateRange])
#     index(selectedSeries1) <- dates
    
    return(selectedSeries0)
    
  })
  
    
  output$timeSeries <- renderPlot({
    
    selectedSeries1 <- seriesInput()
    
    selectedSeries1[ which(is.na(selectedSeries1)) ]<- 0
    selectedSeries2 <- cumprod(selectedSeries1+1)
    
    selectedREITs <- input$selectedREITs
    selectedREITs <- append("VNQ", selectedREITs)
    tsRainbow <- rainbow(length(selectedREITs))
    
    plot(selectedSeries2, xlab = "Time", ylab = "Cumulative Return", screens = 1, col = tsRainbow)
    legend(x = "topleft", legend = selectedREITs, lty = 1,col = tsRainbow)
      
  })
  
  output$correlationTable <- renderTable({
    
    selectedSeries1 <- seriesInput()
    selectedSeries2 <- selectedSeries1[,-1]
    
    m <- cor(selectedSeries2, y = NULL, use = "pairwise.complete.obs")
    
    return(m)

    
  })
  
  output$TEST <- renderText({
    
    "This is a compact web-app to screen REIT correlations. Prices are up-to-date as of 8/26/2014"
  })
  
  output$Table1 <- renderDataTable({
    selectedSeries1 <- seriesInput()
    selectedSeries2 <- selectedSeries1[,-1]
    
    m0 <- cor(selectedSeries2, y = NULL, use = "pairwise.complete.obs")
    
    m0[lower.tri(m0, diag=TRUE)] <- NA
    m1 <- melt(m0,varnames = c("V1", "V2"), na.rm = TRUE, value.name = "Cor")
    
    m2 <- m1[order(-m1$Cor),]
    
    return(m2)
    
  }, options = list(bPaginate = FALSE))
  

    
})

