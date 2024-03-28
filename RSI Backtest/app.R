# app.R (or any other filename)
library(shiny)

# Run the Shiny app
shinyApp(ui = source("ui.R")$value, server = source("server.R")$value)
