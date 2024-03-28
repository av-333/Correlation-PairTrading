
library(quantmod)
library(PerformanceAnalytics)

getSymbols("RELIANCE.NS", from = "2023-03-01")
price <- na.locf(Ad(get("RELIANCE.NS")))

qty <- 100
day <- 14




signal <- c()
shares <- c()
cash <- c()
fvalue <- c()
return <- c()
signal[1:(day + 1)] <- 0
shares[1:(day + 1)] <- 0
cash[1:(day + 1)] <- 1000000
fvalue[1:(day + 1)] <- 1000000
return[1:(day + 1)] <- 0

rsi <- RSI(price, day)  
plot(rsi, type = 'l') 
plot(price, type = 'l')

# Generate trading signals using if-else statements
for (i in (day + 1):length(price)) {
  if (rsi[i] < 30) {
    signal[i] <- 1  # Buy signal
  } else if (rsi[i] > 70) {
    signal[i] <- -1  # Sell signal
  } else {
    signal[i] <- 0  # No action
  }
}

# Convert signals to time series with correct date index
trade <- reclass(signal, price)

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
  } 
  else if (trade[i] < 0) {
    # Sell signal
    if (shares[i - 1] > 0) {
      # Check if there are enough shares to sell
      shares[i] <- 0  # Sell all available shares
      cash[i] <- cash[i - 1] + shares[i - 1] * price[i]  # Sell all available shares
    } else {
      # If not enough shares, maintain previous values
      shares[i] <- shares[i - 1]
      cash[i] <- cash[i - 1]
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
initial_investment <- 100000  # Initial cash amount
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
