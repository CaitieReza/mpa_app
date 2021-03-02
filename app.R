library(shiny)
library(tidyverse)
library(palmerpenguins)
library(shinythemes)
library(here)
library(plotly)
library(shinydashboard)


load("DBdata[asmt][v4.491].RData")


#Tidy format: tb.data --- Total biomass data
#No data: ATBTUNAEATL, ATBTUNAWATL (both ADDED as all NAs!)
tuna_biomass <- tb.data %>% 
  select(ALBANATL, ALBASATL, BIGEYEATL, BMARLINATL, SAILEATL, SAILWATL, SKJEATL, SKJWATL, SWORDNATL, SWORDSATL, WMARLINATL, YFINATL)%>% 
  rownames_to_column() %>% 
  add_column(ATBTUNAEATL = NA) %>% 
  add_column(ATBTUNAWATL = NA) %>% 
  pivot_longer(!rowname, names_to = "species", values_to = "biomass") %>% 
  rename(year = rowname) %>% 
  mutate(year = as.numeric(as.character(year)))  %>% 
  filter(year >= 1950) %>% 
  mutate(species = case_when(species == "ALBANATL" ~ "Albacore tuna Northern Atlantic", 
                             species == "ALBASATL" ~ "Albacore tuna South Atlantic",
                             species == "BIGEYEATL" ~ "Bigeye tuna Atlantic Ocean",
                             species == "BMARLINATL" ~ "Blue marlin Atlantic Ocean",
                             species == "SAILEATL" ~ "Sailfish Eastern Atlantic",
                             species == "SAILWATL" ~ "Sailfish Western Atlantic",
                             species == "SKJEATL" ~ "Skipjack tuna Eastern Atlantic",
                             species == "SKJWATL" ~ "Skipjack tuna Western Atlantic",
                             species == "SWORDNATL" ~ "Swordfish Northern Atlantic",
                             species == "SWORDSATL" ~ "Swordfish South Atlantic",
                             species == "WMARLINATL" ~ "White marlin Atlantic Ocean",
                             species == "YFINATL" ~ "Yellowfin tuna Atlantic Ocean",
                             species == "ATBTUNAEATL" ~ "Atlantic bluefin tuna Eastern Atlantic",
                             species == "ATBTUNAWATL" ~ "Atlantic bluefin tuna Western Atlantic"))


#Tidy format: tc.data --- Total catch data
#Data for all 14 species
tuna_catch <- tc.data %>% 
  select(ALBANATL, ALBASATL, BIGEYEATL, BMARLINATL, SAILEATL, SAILWATL, SKJEATL, SKJWATL, SWORDNATL, SWORDSATL, WMARLINATL, YFINATL, ATBTUNAEATL, ATBTUNAWATL)%>% 
  rownames_to_column() %>% 
  pivot_longer(!rowname, names_to = "species", values_to = "catch") %>% 
  rename(year = rowname) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1950) %>% #NO data before 1950
  mutate(species = case_when(species == "ALBANATL" ~ "Albacore tuna Northern Atlantic", 
                             species == "ALBASATL" ~ "Albacore tuna South Atlantic",
                             species == "BIGEYEATL" ~ "Bigeye tuna Atlantic Ocean",
                             species == "BMARLINATL" ~ "Blue marlin Atlantic Ocean",
                             species == "SAILEATL" ~ "Sailfish Eastern Atlantic",
                             species == "SAILWATL" ~ "Sailfish Western Atlantic",
                             species == "SKJEATL" ~ "Skipjack tuna Eastern Atlantic",
                             species == "SKJWATL" ~ "Skipjack tuna Western Atlantic",
                             species == "SWORDNATL" ~ "Swordfish Northern Atlantic",
                             species == "SWORDSATL" ~ "Swordfish South Atlantic",
                             species == "WMARLINATL" ~ "White marlin Atlantic Ocean",
                             species == "YFINATL" ~ "Yellowfin tuna Atlantic Ocean",
                             species == "ATBTUNAEATL" ~ "Atlantic bluefin tuna Eastern Atlantic",
                             species == "ATBTUNAWATL" ~ "Atlantic bluefin tuna Western Atlantic")) 


#Tidy format: er.data --- Exploitation rate data (usually an annual fraction harvested)
#No data: ATBTUNAEATL, ATBTUNAWATL  (both ADDED as all NAs!)
#NOTE: harvest rate (U); may be either exploitation rate or fishing mortality
tuna_er <- er.data %>% 
  select(ALBANATL, ALBASATL, BIGEYEATL, BMARLINATL, SAILEATL, SAILWATL, SKJEATL, SKJWATL, SWORDNATL, SWORDSATL, WMARLINATL, YFINATL) %>% 
  rownames_to_column() %>% 
  add_column(ATBTUNAEATL = NA) %>% 
  add_column(ATBTUNAWATL = NA) %>% 
  pivot_longer(!rowname, names_to = "species", values_to = "explotation_rate") %>% 
  rename(year = rowname) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1950) %>% 
  mutate(species = case_when(species == "ALBANATL" ~ "Albacore tuna Northern Atlantic", 
                             species == "ALBASATL" ~ "Albacore tuna South Atlantic",
                             species == "BIGEYEATL" ~ "Bigeye tuna Atlantic Ocean",
                             species == "BMARLINATL" ~ "Blue marlin Atlantic Ocean",
                             species == "SAILEATL" ~ "Sailfish Eastern Atlantic",
                             species == "SAILWATL" ~ "Sailfish Western Atlantic",
                             species == "SKJEATL" ~ "Skipjack tuna Eastern Atlantic",
                             species == "SKJWATL" ~ "Skipjack tuna Western Atlantic",
                             species == "SWORDNATL" ~ "Swordfish Northern Atlantic",
                             species == "SWORDSATL" ~ "Swordfish South Atlantic",
                             species == "WMARLINATL" ~ "White marlin Atlantic Ocean",
                             species == "YFINATL" ~ "Yellowfin tuna Atlantic Ocean",
                             species == "ATBTUNAEATL" ~ "Atlantic bluefin tuna Eastern Atlantic",
                             species == "ATBTUNAWATL" ~ "Atlantic bluefin tuna Western Atlantic")) 

#Tidy format: divbpref.data --- B/Bmsy pref data (B/Bmsy if available, otherwise B/Bmgt)
#Data for all 14 species
tuna_b_bmsy <- divbpref.data %>% 
  select(ALBANATL, ALBASATL, BIGEYEATL, BMARLINATL, SAILEATL, SAILWATL, SKJEATL, SKJWATL, SWORDNATL, SWORDSATL, WMARLINATL, YFINATL, ATBTUNAEATL, ATBTUNAWATL) %>% 
  rownames_to_column() %>% 
  pivot_longer(!rowname, names_to = "species", values_to = "b_bmsy") %>% 
  rename(year = rowname) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1950) %>% 
  mutate(species = case_when(species == "ALBANATL" ~ "Albacore tuna Northern Atlantic", 
                             species == "ALBASATL" ~ "Albacore tuna South Atlantic",
                             species == "BIGEYEATL" ~ "Bigeye tuna Atlantic Ocean",
                             species == "BMARLINATL" ~ "Blue marlin Atlantic Ocean",
                             species == "SAILEATL" ~ "Sailfish Eastern Atlantic",
                             species == "SAILWATL" ~ "Sailfish Western Atlantic",
                             species == "SKJEATL" ~ "Skipjack tuna Eastern Atlantic",
                             species == "SKJWATL" ~ "Skipjack tuna Western Atlantic",
                             species == "SWORDNATL" ~ "Swordfish Northern Atlantic",
                             species == "SWORDSATL" ~ "Swordfish South Atlantic",
                             species == "WMARLINATL" ~ "White marlin Atlantic Ocean",
                             species == "YFINATL" ~ "Yellowfin tuna Atlantic Ocean",
                             species == "ATBTUNAEATL" ~ "Atlantic bluefin tuna Eastern Atlantic",
                             species == "ATBTUNAWATL" ~ "Atlantic bluefin tuna Western Atlantic"))

#Tidy format: divupref.data --- U/Umsy pref data (U/Umsy if available, otherwise U/Umgt)
#Data for all 14 species
tuna_u_umsy <- divupref.data %>% 
  select(ALBANATL, ALBASATL, BIGEYEATL, BMARLINATL, SAILEATL, SAILWATL, SKJEATL, SKJWATL, SWORDNATL, SWORDSATL, WMARLINATL, YFINATL, ATBTUNAEATL, ATBTUNAWATL) %>% 
  rownames_to_column() %>% 
  pivot_longer(!rowname, names_to = "species", values_to = "u_umsy") %>% 
  rename(year = rowname) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1950) %>% 
  mutate(species = case_when(species == "ALBANATL" ~ "Albacore tuna Northern Atlantic", 
                             species == "ALBASATL" ~ "Albacore tuna South Atlantic",
                             species == "BIGEYEATL" ~ "Bigeye tuna Atlantic Ocean",
                             species == "BMARLINATL" ~ "Blue marlin Atlantic Ocean",
                             species == "SAILEATL" ~ "Sailfish Eastern Atlantic",
                             species == "SAILWATL" ~ "Sailfish Western Atlantic",
                             species == "SKJEATL" ~ "Skipjack tuna Eastern Atlantic",
                             species == "SKJWATL" ~ "Skipjack tuna Western Atlantic",
                             species == "SWORDNATL" ~ "Swordfish Northern Atlantic",
                             species == "SWORDSATL" ~ "Swordfish South Atlantic",
                             species == "WMARLINATL" ~ "White marlin Atlantic Ocean",
                             species == "YFINATL" ~ "Yellowfin tuna Atlantic Ocean",
                             species == "ATBTUNAEATL" ~ "Atlantic bluefin tuna Eastern Atlantic",
                             species == "ATBTUNAWATL" ~ "Atlantic bluefin tuna Western Atlantic"))

#Tidy format: cdivmeanc.data --- Catch/(mean catch) data
#Data for all 14 species
tuna_catch_mean <- cdivmeanc.data  %>% 
  select(ALBANATL, ALBASATL, BIGEYEATL, BMARLINATL, SAILEATL, SAILWATL, SKJEATL, SKJWATL, SWORDNATL, SWORDSATL, WMARLINATL, YFINATL, ATBTUNAEATL, ATBTUNAWATL) %>% 
  rownames_to_column() %>% 
  pivot_longer(!rowname, names_to = "species", values_to = "c_c_mean") %>% 
  rename(year = rowname) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1950) %>% 
  mutate(species = case_when(species == "ALBANATL" ~ "Albacore tuna Northern Atlantic", 
                             species == "ALBASATL" ~ "Albacore tuna South Atlantic",
                             species == "BIGEYEATL" ~ "Bigeye tuna Atlantic Ocean",
                             species == "BMARLINATL" ~ "Blue marlin Atlantic Ocean",
                             species == "SAILEATL" ~ "Sailfish Eastern Atlantic",
                             species == "SAILWATL" ~ "Sailfish Western Atlantic",
                             species == "SKJEATL" ~ "Skipjack tuna Eastern Atlantic",
                             species == "SKJWATL" ~ "Skipjack tuna Western Atlantic",
                             species == "SWORDNATL" ~ "Swordfish Northern Atlantic",
                             species == "SWORDSATL" ~ "Swordfish South Atlantic",
                             species == "WMARLINATL" ~ "White marlin Atlantic Ocean",
                             species == "YFINATL" ~ "Yellowfin tuna Atlantic Ocean",
                             species == "ATBTUNAEATL" ~ "Atlantic bluefin tuna Eastern Atlantic",
                             species == "ATBTUNAWATL" ~ "Atlantic bluefin tuna Western Atlantic"))


#Merge all time series tidy dataframes:
tuna_merged <- tuna_catch %>% 
  right_join(tuna_biomass, by=c("year","species")) %>% 
  right_join(tuna_er, by=c("year","species")) %>% 
  right_join(tuna_b_bmsy, by=c("year","species")) %>% 
  right_join(tuna_u_umsy, by=c("year","species")) %>% 
  right_join(tuna_catch_mean, by=c("year","species"))

#NOTES: add text describing each time series



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

                           tabPanel("Time Series",titlePanel("Interactive Atlantic Ocean stocks time series (1950-2017)"),
                                    sidebarLayout(
                                      sidebarPanel(selectInput(inputId = "select", label = h4("Select time series"),
                                                               choices = list("Total Biomass" = "biomass", 
                                                                              "Total Catch" = "catch",
                                                                              "Explotation Rate" = "explotation_rate",
                                                                              "B/Bmsy" = "b_bmsy",
                                                                              "U/Umsy" = "u_umsy",
                                                                              "Catch/catch mean" = "c_c_mean" ),
                                                               selected = 1), width = 3
                                                   
                                                   
                                                   
                                      ),
                                      mainPanel("Time series",
                                                plotlyOutput(outputId = "tuna_plot"),
                                                hr(),
                                                fluidRow(column(12, tags$strong(verbatimTextOutput("value")) ))
                                      )
                                    )
                           ),
                           
                           tabPanel("Stock Status",titlePanel("Atlantic Ocean stocks relative B, U, and catch (1950-2017)"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "species",
                                                     label = h4("Select Atlantic Ocean stock species"),
                                                     choices = c("Albacore tuna Northern Atlantic",
                                                                 "Albacore tuna South Atlantic",
                                                                 "Bigeye tuna Atlantic Ocean",
                                                                 "Blue marlin Atlantic Ocean",
                                                                 "Sailfish Eastern Atlantic",
                                                                 "Sailfish Western Atlantic",
                                                                 "Skipjack tuna Eastern Atlantic",
                                                                 "Skipjack tuna Western Atlantic",
                                                                 "Swordfish Northern Atlantic",
                                                                 "Swordfish South Atlantic",
                                                                 "White marlin Atlantic Ocean",
                                                                 "Yellowfin tuna Atlantic Ocean",
                                                                 "Atlantic bluefin tuna Eastern Atlantic",
                                                                 "Atlantic bluefin tuna Western Atlantic"),
                                                     selected = "Albacore tuna Northern Atlantic"
                                        )
                                        
                                        
                                        
                                      ),
                                      mainPanel("Relative B, U, and catch by species",
                                                plotOutput(outputId = "b_u_catch_plot"),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("date1", 
                                                                                                                                label = h6("Starting date:"),
                                                                                                                                choices = 1950:2017,
                                                                                                                                selected = 1950)),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput("date2", 
                                                                                                                                 label = h6("Ending date:"),
                                                                                                                                 choices = 1950:2017,
                                                                                                                                 selected = 2017))
                                                
                                                
                                      )
                                    )
                           ),

                           tabPanel("Trends",
                                    sidebarLayout(
                                      sidebarPanel("our widget/s!",
                                                   selectInput("select", label = h3("Ecological Factors"), 
                                                               choices = list("Mobility" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                               selected = 1)
                                      ),
                                      mainPanel("here's where our AWESOME graph will go",
                                                img(src = "https://cpb-us-e1.wpmucdn.com/sites.uw.edu/dist/f/2132/files/2019/07/B-U-C_msy_trends-for-indiv-region_Atlantic-Ocean-tunas_RAM4-46-asmt_2019_07_16-1.jpg"))
                                    )
                           )
                           
                           
                           
                           
                           
                )
                
)





# Server function
server <- function(input, output) {
  tuna_react <- reactive({ 
    tuna_merged %>% 
      rename(unit = input$select)
  })
  
  y_labs <- reactive({
    case_when(input$select == "explotation_rate" ~ "explotation rate",
              input$select == "catch" ~ "total catch (MT)",
              input$select == "biomass" ~ "total biomass (MT)",
              input$select == "b_bmsy" ~ "B/Bmsy",
              input$select == "u_umsy" ~ "U/Umsy",
              input$select == "c_c_mean" ~ "Catch/catch mean")
  }) 
  
  
  output$value <- renderPrint({ 
    case_when(input$select == "explotation_rate" ~ "write brief explanation of explotation rate",
              input$select == "catch" ~ "write brief explanation of total catch",
              input$select == "biomass" ~ "write brief explanation of total biomass",
              input$select == "b_bmsy" ~ "write brief explanation of B/Bmsy",
              input$select == "u_umsy" ~ "write brief explanation of U/Umsy",
              input$select == "c_c_mean" ~ "write brief explanation of Catch/catch mean")
    
  })
  
  output$tuna_plot <- renderPlotly({
    ggplotly(ggplot(data = tuna_react(), aes(x = year, y = unit)) +
               geom_line(aes(color = species)) +
               theme_minimal() +
               labs(x = "year",
                    y = y_labs()) +
               theme(legend.title = element_blank())
    )
  })
  
  
  
  
  

  tuna_relative_react <- reactive({ 
    tuna_merged %>% 
      filter(species == input$species) %>% 
      filter(year >= input$date1, year<=input$date2)
  })
  
  
  
  output$b_u_catch_plot <- renderPlot({
    ggplot(data = tuna_relative_react(), aes(x = year,)) +
      geom_line(aes(y = b_bmsy, color = "B/Bmsy"), alpha = 0.7) +
      geom_line(aes(y = u_umsy, color = "U/Umsy"), alpha = 0.7) +
      geom_line(aes(y = c_c_mean, color = "Catch/catch mean"), alpha = 0.7)+
      geom_hline(yintercept= 1, linetype = "dashed") +
      scale_color_manual(values = c( "B/Bmsy" = "red3", "U/Umsy" = "palegreen4", "Catch/catch mean" = "dodgerblue1")) +
      labs(color = "Relative B, U, and catch")+
      labs(y = "")+
      theme_minimal()
    
  })
  
  
}

# Combine user interface + server
shinyApp(ui = ui, server = server)


