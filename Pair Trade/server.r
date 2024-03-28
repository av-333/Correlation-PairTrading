# server.R

library(shiny)
library(quantmod)

function(input, output, session) {
  
  # Function to fetch Yahoo Finance data for the selected currency pair
  getForexData <- function(base_currency, quote_currency, start_date, end_date) {
    # Construct the Yahoo Finance ticker symbol for the currency pair
    ticker <- paste0(base_currency, quote_currency, "=X")
    
    # Fetch historical data using getSymbols function
    forex_data <- tryCatch(
      expr = {
        getSymbols(ticker,
                   src = "yahoo",
                   from = start_date,
                   to = end_date,
                   auto.assign = FALSE)  # We set auto.assign to FALSE to prevent automatic assignment
      },
      error = function(e) {
        return(NULL)  # Return NULL if there's an error fetching data
      }
    )
    
    # Return the fetched data
    return(forex_data)
  }
  
  # Observe the action button click event to fetch forex data
  observeEvent(input$runAnalysis, {
    # Get the selected base currency and quote currency from the UI
    base_currency <- input$baseCurrency
    quote_currency <- input$quoteCurrency
    
    # Set the start and end date for data retrieval
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    # Fetch forex data for the selected currency pair
    forex_data <- getForexData(base_currency, quote_currency, start_date, end_date)
    
    # Check if forex_data is NULL (indicating an error in data retrieval)
    if (is.null(forex_data)) {
      # Display an error message if data retrieval fails
      output$forexData <- renderPrint({
        "Error fetching data. Please try again."
      })
      return()  # Exit the function early if data retrieval fails
    }
    
    # Display the fetched forex data
    output$forexData <- renderPrint({
      forex_data
    })
    
    # Plot price data for the selected currency pair
    output$pricePlot <- renderPlot({
      plot(Cl(forex_data), type = "l", main = paste("Price Data for", paste(base_currency, quote_currency)))
    })
    
    # Plot Exponential Moving Average (EMA) with price data
    output$priceEMAPlot <- renderPlot({
      price <- Cl(forex_data)
      ema_days <- input$emaDays
      ema <- EMA(price, n = ema_days)
      plot(price, type = "l", main = paste("Price Data with Exponential Moving Average (", ema_days, "-day)"))
      lines(ema, col = "red")
      legend("topright", legend = c("Price", "EMA"), col = c("black", "red"), lty = 1)
    })
    
    # Plot Simple Moving Average (SMA)
    output$smaPlot <- renderPlot({
      if (!is.null(forex_data)) {
        sma_days <- input$smaDays
        ma <- SMA(Cl(forex_data), n = sma_days)
        plot(ma, type = "l", col = "blue", lwd = 2, main = paste("Simple Moving Average (", sma_days, "-day)"))
      }
    })
    
    # Plot Exponential Moving Average (EMA)
    output$emaPlot <- renderPlot({
      if (!is.null(forex_data)) {
        ema_days <- input$emaDays
        ema <- EMA(Cl(forex_data), n = ema_days)
        plot(ema, type = "l", col = "red", lwd = 2, main = paste("Exponential Moving Average (", ema_days, "-day)"))
      }
    })
    
  })
}
