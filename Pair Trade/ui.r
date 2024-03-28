# ui.R

library(shiny)

fluidPage(
  titlePanel("Forex Currency Pair Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("baseCurrency", "Select Base Currency",
                  choices = c("EUR", "USD", "GBP", "JPY", "AUD"),
                  selected = "EUR"),
      selectInput("quoteCurrency", "Select Quote Currency",
                  choices = c("USD", "EUR", "GBP", "JPY", "AUD"),
                  selected = "USD"),
      dateRangeInput("dateRange", label = "Select Date Range",
                     start = "2020-01-01", end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      numericInput("smaDays", "Number of days for Simple Moving Average:", value = 10),
      numericInput("emaDays", "Number of days for Exponential Moving Average:", value = 10),
      actionButton("runAnalysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Price Data",
                 plotOutput("pricePlot")),
        tabPanel("Price Data with EMA",
                 plotOutput("priceEMAPlot")),
        tabPanel("Simple Moving Average",
                 plotOutput("smaPlot")),
        tabPanel("Exponential Moving Average",
                 plotOutput("emaPlot")),
        tabPanel("Data",
                 verbatimTextOutput("forexData")
        )
      )
    )
  )
)
