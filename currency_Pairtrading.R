library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(gridExtra)
library(scales)
library(dplyr)


# Fetching stock data
stocks <- c("AUDUSD=X", "GBPUSD=X", "USDCHF=X", "NZDUSD=X", "USDJPY=X", "EURUSD=X", "USDCAD=X", "EURGBP=X", "EURCHF=X", "NZDCHF=X")
getSymbols(stocks, from = "2020-01-01", src = "yahoo")

# Creating a list to store the adjusted stock prices
stock_list <- lapply(stocks, function(stock) {
  na.locf(Cl(get(stock)))
})

stocks <- gsub("=X", "", stocks)

names(stock_list) <- stocks

stock_list

calculation_fun <- function(stock1, stock2) {
  ret_stock1 <- Delt(stock1)
  ret_stock2 <- Delt(stock2)
  data <- merge(ret_stock1, ret_stock2)
  
  correlation <- function(x) {
    result <- cor(x[, 1], x[, 2])
    return(result)
  }
  
  corr <- rollapply(data, 100, correlation, by.column = FALSE)
  
  rolling_mean <- rollapply(corr, 14, mean)
  rolling_sd <- rollapply(corr, 14, sd)
  
  upper_band <- rolling_mean + 1 * rolling_sd
  lower_band <- rolling_mean - 1 * rolling_sd
  
  spread_return <- ret_stock1 - ret_stock2 * (stock1 / stock2)
  
  rsi_threshold <- 30
  rsi_stock1 <- RSI(stock1, n = 14)
  rsi_stock2 <- RSI(stock2, n = 14)
  
  rsi_signal <- NA
  rsi_signal[(rsi_stock1 > rsi_threshold & lag(rsi_stock1) <= rsi_threshold)] <- 1
  rsi_signal[(rsi_stock1 < rsi_threshold & lag(rsi_stock1) >= rsi_threshold)] <- -1
  rsi_signal <- lag(rsi_signal, 1)
  
  signals <- integer(length(spread_return))
  
  prev_signal <- lag(signals, 1, default = 0)
  
  #signals <- rep(NA, length(spread_return))
  signals[corr > upper_band | (corr > lower_band & rsi_signal == 1)] <- -1
  signals[corr < lower_band | (corr < upper_band & rsi_signal == -1)] <- 1
  
  signals[prev_signal == signals] <- 0
  
  signals <- lag(signals, 1)
  
  trade_returns <- numeric(length(spread_return))
  trade_returns[which(!is.na(signals))] <- spread_return[which(!is.na(signals))] * signals[!is.na(signals)]
  
  combined_data <- merge(data, corr, rolling_mean, spread_return, signals, trade_returns, rsi_stock1, rsi_stock2, rsi_signal)
  combined_data_df <- data.frame(Date = index(combined_data), coredata(combined_data))
  
  return(combined_data_df)
}

add_arrows <- function(data, direction) {
  data %>%
    mutate(
      arrow = case_when(
        lead(direction) == 1 & lag(direction) == -1 ~ "Sell",
        lead(direction) == -1 & lag(direction) == 1 ~ "Buy",
        TRUE ~ NA_character_
      )
    ) -> data
  
  return(data)
}


# Shiny App
shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      selectInput("stock1", label = "Currency Pair pink",
                  choices = c("GBP/USD" = "GBPUSD",
                              "EUR/USD" = "EURUSD",
                              "USD/CHF" = "USDCHF",
                              "NZD/USD" = "NZDUSD",
                              "USD/JPY" = "USDJPY"),
                  selected = ""),
      selectInput("stock2", label = "Currency Pair Yellow",
                  choices = c("AUD/USD" = "AUDUSD",
                              "EUR/CHF" = "EURCHF",
                              "EUR/GBP" = "EURGBP",
                              "USD/CAD" = "USDCAD",
                              "NZD/CHF" = "NZDCHF"),
                  selected = ""),
      actionButton("submit", "Submit")
    ),
    dashboardBody(
      fluidRow(
        box(
          width = 12,
          textOutput("selected_stocks"),
          uiOutput("plots")
        )
      )
    )
  ),
  server = function(input, output, session) {
    observeEvent(input$submit, {
      req(input$stock1, input$stock2)
      stock1 <- stock_list[[input$stock1]]
      stock2 <- stock_list[[input$stock2]]
      
      # Set the start and end date for data retrieval
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      
      plot_data <- reactive({
        calculation_fun(stock1, stock2)
      })
      
      plot_data_annotated <- reactive({
        plot_data() %>%
          add_arrows(signals)
      })
      
      positions <- reactive({
        plot_data_annotated() %>%
          group_by(Date) %>%
          summarise(
            position = median(c(min(stock1, stock2), max(stock1, stock2))),
            direction = unique(arrow)
          )
      })
      
      output$plots <- renderUI({
        tagList(
          plotOutput("priceCharts"),
          tabsetPanel(
            tabPanel("correlation Plot", plotOutput("correlationPlot")),
            tabPanel("Stock Returns Scatter Plot", plotOutput("scatterPlot")),
            tabPanel("Spread Returns", plotOutput("spreadPlot")),
            tabPanel("Signals", plotOutput("signalPlot")),
            tabPanel("Profit Loss", plotOutput("tradeReturnPlot")),
            tabPanel("RSIs", plotOutput("rsisPlots"))
          )
        )
      })
      
      output$priceCharts <- renderPlot({
        price_range <- diff(range(cbind(stock1, stock2)[endpoints(cbind(stock1, stock2)),]))
        ggplot(plot_data(), aes(x = Date)) +
          geom_line(aes(y = stock1, color = input$stock1), size = 1) +
          geom_line(aes(y = stock2, color = input$stock2), size = 1) +
          geom_vline(data = positions()[!is.na(positions()$direction), ], aes(xintercept = Date, color = direction), alpha = 0.5) +
          geom_label(data = positions()[!is.na(positions()$direction), ], aes(x = Date, y = position, label = direction), nudge_y = price_range*0.02) +
          scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
          labs(title = "Price Charts of Two Stocks", x = "", y = "") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10), legend.position = "bottom", )
      })
      
      ret_stock1 <- Delt(stock1)
      ret_stock2 <- Delt(stock2)
      
      output$correlationPlot <- renderPlot({
        plot_data_subset <- subset(plot_data(), select = c(Date, corr))
        ggplot(plot_data_subset, aes(x = Date)) +
          geom_line(aes(y = corr, color = "Correlation"), size = 1) +
          scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
          labs(title = "Correlation Over Time", x = "", y = "") +
          geom_hline(yintercept = 0.5, color = "darkgray", linewidth = 0.5) +
          geom_hline(yintercept = 0.8, color = "forestgreen", linewidth = 0.5) +
          geom_hline(yintercept = -0.8, color = "steelblue", linewidth = 0.5) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10), legend.position = "bottom")
      })
      
      output$scatterPlot <- renderPlot({
        ggplot(plot_data(), aes(x = ret_stock1, y = ret_stock2)) + geom_point() +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          ggtitle("Scatter Plot of Stock Returns")
      })
      
      data <- merge(ret_stock1, ret_stock2)
      
      correlation <- function(x) {
        result <- cor(x[, 1], x[, 2])
        return(result)
      }
      
      corr <- rollapply(data, 100, correlation, by.column = FALSE)
      
      rolling_mean <- rollapply(corr, 14, mean)
      rolling_sd <- rollapply(corr, 14, sd)
      
      upper_band <- rolling_mean + 1 * rolling_sd
      lower_band <- rolling_mean - 1 * rolling_sd
      
      spread_return <- ret_stock1 - ret_stock2 * (stock1 / stock2)
      
      output$spreadPlot <- renderPlot({
        ggplot(plot_data(), aes(x = Date)) +
          geom_line(aes(y = spread_return, color = "Spread Return"), size = 1) +
          scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
          labs(title = "Spread Returns", x = "", y = "") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10), legend.position = "bottom")
      })
      
      signals <- rep(NA, length(spread_return))
      signals[corr > upper_band] <- -1
      signals[corr < lower_band] <- 1
      
      signals <- lag(signals, 1)
      
      output$signalPlot <- renderPlot({
        ggplot(plot_data(), aes(x = Date)) +
          geom_line(aes(y = signals, color = "Signal"), size = 1) +
          scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
          labs(title = "Signals", x = "", y = "") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10), legend.position = "bottom")
      })
      
      trade_returns <- numeric(length(spread_return))
      trade_returns[which(!is.na(signals))] <- spread_return[which(!is.na(signals))] * signals[!is.na(signals)]
      
      output$tradeReturnPlot <- renderPlot({
        ggplot(plot_data(), aes(x = Date)) +
          geom_line(aes(y = trade_returns, color = "Trade Return"), size = 1) +
          scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
          labs(title = "Trade Returns", x = "", y = "") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10), legend.position = "bottom")
      })
      
      rsi_stock1 <- RSI(stock1, n = 14)
      rsi_stock2 <- RSI(stock2, n = 14)
      
      output$rsisPlots <- renderPlot({
        p1 <- ggplot(plot_data(), aes(x = index(rsi_stock1), y = rsi_stock1)) +
          geom_line() +
          geom_hline(yintercept = 30, color = "darkgreen", linetype = "dotted") +
          geom_hline(yintercept = 70, color = "darkred", linetype = "dotted") +
          ggtitle(input$stock1)
        
        p2 <- ggplot(plot_data(), aes(x = index(rsi_stock2), y = rsi_stock2)) +
          geom_line() +
          geom_hline(yintercept = 30, color = "darkgreen", linetype = "dotted") +
          geom_hline(yintercept = 70, color = "darkred", linetype = "dotted") +
          ggtitle(input$stock2)
        
        grid.arrange(p1, p2, ncol = 1)
      })
    })
  }
)