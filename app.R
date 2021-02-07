library(shiny)
library(tidyverse)
library(palmerpenguins)


# User Interface
ui <- fluidPage(
  titlePanel("Visualizing Wild-Caught Stocks"),
  sidebarLayout(
    sidebarPanel("our widgets",
                 radioButtons(inputId = "penguin_species", 
                              label = h3("Biomass Estimates by Species"), 
                              choices = c("Tuna" = 1, "Salmon" = 2, "Fish 3" = 3, "Fish 4" = 4, "Fish 5" = 5)
                 ),
                 sliderInput("slider1", 
                             label = h3("Biomass by Year"), 
                             min = 1950, 
                             max = 2020, value = 10),
                 selectInput("select", label = h3("MPA's by Region"), 
                             choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                             selected = 1)
                 
                 
    ),
    mainPanel("here's where our graph will go")
  )
)



# Server function
server <- function(input, output) {}

# Combine user interface + server
shinyApp(ui = ui, server = server)


