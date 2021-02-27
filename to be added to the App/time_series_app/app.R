
library(shiny)
library(tidyverse)
library(here)
library(plotly)
library(shinydashboard)


load(here("DBdata[asmt][v4.491].RData"))


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

#NOTES: can add text describing each time series


# Define UI 
ui <- fluidPage(
    titlePanel("Interactive Atlantic Ocean stocks time series (1950-2017)"),
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
)



# Define server 
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
    
}
    
    
    
# Run the application 
shinyApp(ui = ui, server = server)
