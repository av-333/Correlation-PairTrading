# app.R (or any other filename)
library(shiny)

# Run the Shiny app
shinyApp(ui = source("ui.r")$value, server = source("server.r")$value)

