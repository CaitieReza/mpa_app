library(shiny)
library(tidyverse)
library(palmerpenguins)


# User Interface
ui <- fluidPage(
  titlePanel("Visualizing Wild-Caught Stocks"),
  sidebarLayout(
    sidebarPanel("here are my widgets"),
    mainPanel("this is where our graphs will go")
  )
)

# Server function
server <- function(input, output) {}

# Combine user interface + server
shinyApp(ui = ui, server = server)


