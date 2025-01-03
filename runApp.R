library(shiny)

# The code of ui.R and server.R to be used for running the application
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)