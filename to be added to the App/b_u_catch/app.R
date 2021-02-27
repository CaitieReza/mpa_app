library(shiny)
library(tidyverse)
library(here)
library(plotly)

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


# Define UI 
ui <- fluidPage(
    titlePanel("Atlantic Ocean stocks relative B, U, and catch (1950-2017)"),
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
)



# Define server 
server <- function(input, output) {
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



# Run the application 
shinyApp(ui = ui, server = server)