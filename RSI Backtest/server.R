# server.R
library(shiny)
library(quantmod)
library(PerformanceAnalytics)

function(input, output, session) {
  
  observeEvent(input$runAnalysis, {
    # Get data based on selected stock and date range
    getSymbols(input$stockSymbol, from = input$dateRange[1], to = input$dateRange[2])
    price <- na.locf(Ad(get(input$stockSymbol)))

    qty <- 100
    day <- 14
    
    
    
    # Initialize vectors for trading signals, shares, cash, fund value, and returns
    signal <- c()
    shares <- c()
    cash <- c()
    fvalue <- c()
    return <- c()
    signal[1:(day + 1)] <- 0
    shares[1:(day + 1)] <- 0
    cash[1:(day + 1)] <- input$investment
    fvalue[1:(day + 1)] <- input$investment
    return[1:(day + 1)] <- 0
    
    rsi <- RSI(price, day)  # calculates RSI with past 14 days 
    plot(rsi, type = 'l')   # make sure that there are oversold and overbought situations
    plot(price, type = 'l')
    
    # Generate trading signals using if-else statements
    for (i in (day + 1):length(price)) {
      if (rsi[i] < input$rsiLower) {
        signal[i] <- 1  # Buy signal
      } else if (rsi[i] > input$rsiUpper) {
        signal[i] <- -1  # Sell signal
      } else {
        signal[i] <- 0  # No action
      }
    }
    
    # Convert signals to time series with correct date index
    trade <- reclass(signal, price)
    
    # Assuming `sell_all_option` is a user-defined variable indicating whether to sell all shares (TRUE) or a specific quantity (FALSE)
    sell_all_option <- input$sellall  # Set this based on user input
    
    # Track cash, shares, fund values as and when a trade happens using if-else
    for (i in (day + 1):length(price)) {
      if (trade[i] > 0) {
        # Buy signal
        if (qty * trade[i] * price[i] <= cash[i - 1] & cash[i - 1] > 0) {
          # Check if there is enough cash to buy the required shares
          shares[i] <- shares[i - 1] + qty * trade[i]
          cash[i] <- cash[i - 1] - qty * trade[i] * price[i]
        } else {
          # If not enough cash, maintain previous values
          shares[i] <- shares[i - 1]
          cash[i] <- cash[i - 1]
        }
      } else if (trade[i] < 0) {
        # Sell signal
        if (sell_all_option) {
          # Sell all available shares
          shares[i] <- 0
          cash[i] <- cash[i - 1] + shares[i - 1] * price[i]
        } else {
          # Sell a fixed quantity (qty)
          if (qty <= shares[i - 1]) {
            shares[i] <- shares[i - 1] - qty
            cash[i] <- cash[i - 1] + qty * price[i]
          } else {
            # If not enough shares, maintain previous values
            shares[i] <- shares[i - 1]
            cash[i] <- cash[i - 1]
          }
        }
      } else {
        # No action, maintain previous values
        shares[i] <- shares[i - 1]
        cash[i] <- cash[i - 1]
      }
    }
    
    
    # Calculate fund values and returns
    for (i in (day + 1):length(price)) {
      fvalue[i] <- shares[i] * price[i] + cash[i]
      return[i] <- fvalue[i] / fvalue[i - 1] - 1
    }
    
    
    # Convert fvalue and return to time series with correct date index
    fvalue <- reclass(fvalue, price)
    return <- reclass(return, price)

    
    # Plot fund value and calculate performance metrics
    charts.PerformanceSummary(return)
    chart_Series(fvalue, main = "fund value")
    table.Stats(return)
    
    
    # Merge data for analysis
    d <- merge(rsi, price, trade, signal, cash, fvalue,return)
    
    # Calculate total return
    initial_investment <- input$investment  # Initial cash amount
    final_portfolio_value <- tail(fvalue, 1)  # Final value of the portfolio
    shares_left<- tail(shares, 1)
    cash_left <- tail(cash,1)
    total_return_percentage <- (final_portfolio_value - initial_investment) / initial_investment
    total_return <- (final_portfolio_value-initial_investment)
    
    cash_flow <- fvalue-100000
    
    
    
    # Calculate total return
    initial_investment <- input$investment
    final_portfolio_value <- tail(fvalue, 1)
    total_return <- (final_portfolio_value - initial_investment)
    
    # Plot fvalue
    output$fvaluePlot <- renderPlot({
      chart_Series(fvalue, main = "Fund Value")
    })
    output$returnsummary <- renderPlot({
      charts.PerformanceSummary(return)
    })
    
    # Display invested value and current value
    output$investedValue <- renderText({
      paste("Invested Value: $", format(initial_investment, big.mark = ","), sep = "")
    })
    
    output$currentValue <- renderText({
      paste("Total Portfolio Value: $", format(final_portfolio_value, big.mark = ","), sep = "")
    })
    
    output$profitorloss <- renderText({
      paste("(Realised + UnRealised)Profit/Loss: $", format(total_return, big.mark = ","), sep = "")
    })
    output$cash_left <- renderText({
      paste("Cash in wallet : $", format(cash_left, big.mark = ","), sep = "")
    })
    output$shares_left <- renderText({
      paste("No of Shares on Hold: $", format(shares_left, big.mark = ","), sep = "")
    })
    output$totalsharevalue <- renderText({
      paste("Total Share Value present in Portfolio on Hold: $", format(shares_left*tail(price,1), big.mark = ","), sep = "")
    })
  })
}
