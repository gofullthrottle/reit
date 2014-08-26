rawMonthlyReturns <- read.csv("data/monthlyReturns.csv")
tickers <- names(rawMonthlyReturns)[2:length(rawMonthlyReturns)]
minDate = as.Date("2000-01-01")
maxDate = as.Date("2014-06-01")

shinyUI(fluidPage(
  
  h1("REIT Correlation Dashboard"),
  
  textOutput("TEST"),
  
  

             plotOutput("timeSeries"),             
             hr(),
             
             sidebarLayout(
               sidebarPanel(
                 h3("REIT selection."),
                 
                                  
                 radioButtons("sector", label = "Select sector.",
                              choices = c("Apts", "Hotels", "Other"),
                              selected = "Apts"),
                 
                 br(),
                 
                 dateRangeInput("dateRange0",
                                label = 'Input date. (yyyy-mm-dd)',
                                start = "2009-01-01", end = "2014-06-01",
                                min = minDate, max = maxDate
                 ),
                 
                 br(),
                 
                 uiOutput("ui")
                 
               ),
               
               mainPanel(
                 h3("Correlation Table"),
                 dataTableOutput("Table1"),
                 br(),
                 h3("Raw Correlation Matrix"),
                 tableOutput("correlationTable")
               )
             )

)
)