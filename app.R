library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)


#?shinythemes on console shows you all the different themes (if you want to pick a different one)



# User Interface
<<<<<<< HEAD
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                navbarPage("StockWatch",
                           tabPanel("Home"),
                           
                           tabPanel("Ecology",
=======
ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage("OUR COOL TITLE",
                           tabPanel("Summary/Information Tab"),
                           
                           tabPanel("Tab 1",
>>>>>>> main
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   radioButtons(inputId = "penguin_species", 
                                                                label = h3("Biomass Estimates by Species"), 
                                                                choices = c("Tuna" = 1, "Salmon" = 2, "Fish 3" = 3, "Fish 4" = 4, "Fish 5" = 5)
<<<<<<< HEAD
                                                                
                                                   )
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
                           ),
                           tabPanel("Biomass",
=======
                                                     
                                                   )
                                                   ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
                                    ),
                           tabPanel("Tab 2",
>>>>>>> main
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   sliderInput("slider1", 
                                                               label = h3("Biomass by Year"), 
                                                               min = 1950, 
                                                               max = 2020, value = 10
<<<<<<< HEAD
                                                               
=======
                                                     
>>>>>>> main
                                                   )
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
<<<<<<< HEAD
                           ),
                           tabPanel("Fishing Pressure",
=======
                                    ),
                           tabPanel("Tab 3",
>>>>>>> main
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   selectInput("select", label = h3("MPA's by Region"), 
                                                               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                               selected = 1
                                                   )
                                                   
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
<<<<<<< HEAD
                           ),
                           tabPanel("Exploring Trends",
=======
                                    ),
                           tabPanel("Tab 4",
>>>>>>> main
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   selectInput("select", label = h3("Ecological Factors"), 
                                                               choices = list("Mobility" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                               selected = 1)
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
<<<<<<< HEAD
                           )
                           
                           
                           
                )
                
=======
                                    )
                           
                           
                           
                           )
  
>>>>>>> main
)
  
  





# Server function
server <- function(input, output) {}

# Combine user interface + server
shinyApp(ui = ui, server = server)


