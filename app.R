library(shiny)
library(tidyverse)
library(palmerpenguins)


# User Interface
ui <- fluidPage(
  titlePanel("Visualizing Wild-Caught Stocks"),
  sidebarLayout(
    sidebarPanel("our widgets",
                 radioButtons(inputId = "penguin_species", 
                              label = "Biomass Estimates by Species", 
                              choices = c("Tuna", "Salmon", "Fish 3", "Fish 4", "Fish 5")
                 )
    ),
    mainPanel("here's where our graph will go")
  )
)



# Server function
server <- function(input, output) {}

# Combine user interface + server
shinyApp(ui = ui, server = server)


