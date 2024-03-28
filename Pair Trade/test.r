library(shiny)

# Load the UI
ui <- shinyUI(fluidPage(
  titlePanel("Forex Trading Strategy Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("forexPair", "Select Forex Pair",
                  choices = c("EURUSD", "GBPUSD", "USDJPY"),
                  selected = "EURUSD"),
      dateRangeInput("dateRange", label = "Select Date Range",
                     start = "2022-01-01", end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      numericInput("investment", "Investment", value = 1000000),
      actionButton("runAnalysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard",
                 plotOutput("fvaluePlot"),
                 plotOutput("returnsummary")),
        tabPanel("Summary",
                 h4("Invested Value:"),
                 verbatimTextOutput("investedValue"),
                 h4("Current Value:"),
                 verbatimTextOutput("currentValue"),
                 h4("Profit/Loss:"),
                 verbatimTextOutput("profitorloss"),
                 h4("Cash In Wallet:"),
                 verbatimTextOutput("cash_left"),
                 h4("Shares on Hold:"),
                 verbatimTextOutput("shares_left"),
                 h4("Total Share Value present in Portfolio on Hold:"),
                 verbatimTextOutput("totalsharevalue")
        )
      )
    )
  )
))


