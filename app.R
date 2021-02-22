library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)
library(here)


#?shinythemes on console shows you all the different themes (if you want to pick a different one)



# User Interface

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                navbarPage(title=div(img(src="https://creazilla-store.fra1.digitaloceanspaces.com/silhouettes/67427/tuna-silhouette-4bb7ee-md.png", height = "05%", width = "05%"), "StockWatch"),
                           tabPanel("Home",
                                  mainPanel(h2("Background"),
                                            p("Monitoring fish stocks is a crucial aspect of fisheries management.  
                                            Stock assessments are extremely valuable for assessing biodiversity, setting catch limits, and implementing a
                                              multitude of other management strategies necessary to conserve marine species."),
                                            img(src = "https://images.squarespace-cdn.com/content/v1/511cdc7fe4b00307a2628ac6/1598456180510-8HP33TBNY8Z8B4FMFE64/ke17ZwdGBToddI8pDm48kEhk4PMdjneZU7fdR_q5soxZw-zPPgdn4jUwVcJE1ZvWQUxwkmyExglNqGp0IvTJZamWLI2zvYWH8K3-s_4yszcp2ryTI0HqTOaaUohrI8PIMdG2tpRl3f2mZAJsRkSvSCVIhKT8STsEVs-xYFgM6b8KMshLAGzx4R3EDFOm1kBS/1280_wmrMzTIk3962.jpg"),
                                            h2("About the App"),
                                            p("StockWatch is a tool for visualizing biomass, fishing pressure, and other components of stock assessments that are useful for tracking the health of target species. "),
                                            h3("Management Implications"),
                                            p("Still want to fill this part in"),
                                            h3("Focus Species"),
                                            h4("Our tool focuses on 10 commercially viable species in the North Atlantic:"),
                                            h5("Northern Atlantic Albacore tuna"),
                                            
                                            h5("South Atlantic Albacore tuna"),
                                            h5("Bigeye tuna"),
                                            h5("Blue Marlin"),
                                            h5("Eastern Atlantic Sailfish"),
                                            h5("Western Atlantic Sailfish"),
                                            h5("Eastern Atlantic Skipjack"),
                                            h5("Western Atlantic Skipjack"),
                                            h5("North Atlantic Swordfish"),
                                            h5("South Atlantic Swordfish"),
                                            h5("White marlin"),
                                            h5("Yellowfin tuna"),
                                            img(src = "https://www.freeworldmaps.net/ocean/atlantic/atlantic-ocean-blank-map.jpg", height = "80%", width = "80%", align = "right")
                                            
                                            
                                            
                                            
                                            
                                            
                            
                                            )  
                           
                                    ),
                           
                           tabPanel("Ecology",
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   radioButtons(inputId = "penguin_species", 
                                                                label = h3("Biomass Estimates by Species"), 
                                                                choices = c("Tuna" = 1, "Salmon" = 2, "Fish 3" = 3, "Fish 4" = 4, "Fish 5" = 5)
                                                                
                                                   )
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
                           ),
                           tabPanel("Biomass",
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   sliderInput("slider1", 
                                                               label = h3("Biomass by Year"), 
                                                               min = 1950, 
                                                               max = 2020, value = 10
                                                               
                                                   )
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
                           ),
                           tabPanel("Fishing Pressure",
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   selectInput("select", label = h3("MPA's by Region"), 
                                                               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                               selected = 1
                                                   )
                                                   
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
                           ),
                           tabPanel("Trends",
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   selectInput("select", label = h3("Ecological Factors"), 
                                                               choices = list("Mobility" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                               selected = 1)
                                      ),
                                      mainPanel("here's where our AWESOME graph will go")
                                    )
                           )
                           
                           
                           
                           
                           
                )
                
)





# Server function
server <- function(input, output) {}

# Combine user interface + server
shinyApp(ui = ui, server = server)


