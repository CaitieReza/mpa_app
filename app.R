library(shiny)
library(tidyverse)
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

#TAB 5 Plots:
##B/Bmsy plot:

tuna_region_bmsy1 <- tuna_merged %>% 
  group_by(year) 


p1 <- ggplot(tuna_region_bmsy1)+ 
  geom_boxplot(aes(x= factor(year), y = b_bmsy), 
               fill = "blue", 
               width = 0.2, 
               outlier.color = NA,
               outlier.shape = NA)+
  stat_summary(aes(x= factor(year), y = b_bmsy, group = 1),
               fun = mean, 
               geom = "line",
               alpha = 0.7,
               size = 0.5,
               color = "red")+
  geom_hline(yintercept = 1, color = "orange2")+
  scale_x_discrete("Year", breaks = c("1950","1960", "1970", "1980", "1990", "2000", "2010", "2018") )+
  scale_y_continuous(breaks=c(0, 1, 2, 3, 4), limits = c(-0.1, 4.5))+
  labs(y = "Relative biomass (B/Bmsy)")+
  theme_minimal()


p1 <- plotly_build(p1)%>% 
  layout(annotations = 
           list(x = 0.2, y = -0.1, text = "Red line shows mean B/Bmsy.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=10))
  )

p1$x$data <- lapply(p1$x$data, FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})



#Umsy plot:

tuna_region_2 <- tuna_merged %>% 
  filter(year>=1970) %>% 
  group_by(year) 


p2 <- ggplot(tuna_region_2)+ 
  geom_boxplot(aes(x= factor(year), y = u_umsy), fill = "blue", 
               width = 0.2, 
               outlier.color = NA)+
  stat_summary(aes(x= factor(year), y = u_umsy, group = 1),
               fun = mean, 
               geom = "line",
               alpha = 0.7,
               size = 0.5,
               color = "red")+
  geom_hline(yintercept = 1, color = "orange2")+
  scale_x_discrete("Year", breaks = c("1970", "1980", "1990", "2000", "2010", "2018") )+
  scale_y_continuous(breaks=c(0, 1, 2, 3, 4), limits = c(-0.1, 4.5))+
  labs(y = "Relative fishing pressure (U/Umsy)")+
  theme_minimal()

p2 <- plotly_build(p2)%>% 
  layout(annotations = 
           list(x = 0.2, y = -0.1, text = "Red line shows mean U/Umsy.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=10))
  )

p2$x$data <- lapply(p2$x$data, FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})


#Catch/catch mean plots:
p3 <- ggplot()+ 
  geom_boxplot(data = tuna_region_2, aes(x= factor(year), y = c_c_mean), fill = "blue", 
               width = 0.2, 
               outlier.color = NA)+
  stat_summary(data = tuna_region_2, aes(x= factor(year), y = c_c_mean, group = 1),
               fun = mean, 
               geom = "line",
               alpha = 0.7,
               size = 0.5,
               color = "red")+
  geom_hline(yintercept = 1, color = "orange2")+
  scale_x_discrete("Year", breaks = c("1970", "1980", "1990", "2000", "2010", "2018") )+
  scale_y_continuous(breaks=c(0, 1, 2, 3, 4), limits = c(-0.1, 4.5))+
  labs(y = "Catch/mean catch")+
  theme_minimal()


p3 <- plotly_build(p3)%>% 
  layout(annotations = 
           list(x = 0.26, y = -0.1, text = "Red line shows mean catch/mean catch.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=10))
  )

p3$x$data <- lapply(p3$x$data, FUN = function(x){
  x$marker = list(opacity = 0)
  return(x)
})



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
                                            h5("• Northern Atlantic Albacore tuna"),
                                            
                                            h5("• South Atlantic Albacore tuna"),
                                            h5("• Bigeye tuna"),
                                            h5("• Blue Marlin"),
                                            h5("• Eastern Atlantic Sailfish"),
                                            h5("• Western Atlantic Sailfish"),
                                            h5("• Eastern Atlantic Skipjack"),
                                            h5("• Western Atlantic Skipjack"),
                                            h5("• North Atlantic Swordfish"),
                                            h5("• South Atlantic Swordfish"),
                                            h5("• White marlin"),
                                            h5("• Yellowfin tuna"),
                                            img(src = "https://www.freeworldmaps.net/ocean/atlantic/atlantic-ocean-blank-map.jpg", height = "80%", width = "80%", align = "right")
                                            
                                            
                                            
                                            
                                            
                                            
                            
                                            )  
                           
                                    ),
                           
                           tabPanel("Species Profiles",
                                    titlePanel("Basic Species Information"),
                                    sidebarLayout(
                                      sidebarPanel("Click a species to learn more:",
                                                   radioButtons(inputId = "table1", 
                                                                label = h3("Species"), 
                                                                choiceNames = list("Northern Atlantic Albacore tuna",
                                                                  "South Atlantic Albacore tuna",
                                                                  "East Atlantic Bluefin Tuna",
                                                                  "West Atlantic Bluefin Tuna",
                                                                "Bigeye tuna",
                                                                "Blue Marlin",
                                                                "Eastern Atlantic Sailfish",
                                                                "Western Atlantic Sailfish",
                                                                "Eastern Atlantic Skipjack",
                                                                "Western Atlantic Skipjack",
                                                                "North Atlantic Swordfish",
                                                                "South Atlantic Swordfish",
                                                                "White marlin",
                                                                "Yellowfin tuna"),
                                                                choiceValues = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"),
                                                                
                                                                  
                                                                
                                                   )
                                      ),
                                      mainPanel(
                                                uiOutput("table")
                                                )
                                    )
                           ),

                           tabPanel("Time Series",
                                    titlePanel("Atlantic Ocean stocks time series"),
                                    sidebarLayout(
                                      sidebarPanel(selectInput(inputId = "select", label = h4("Select time series"),
                                                               choices = list("Total Biomass" = "biomass", 
                                                                              "Total Catch" = "catch",
                                                                              "Explotation Rate" = "explotation_rate",
                                                                              "Relative biomass (B/Bmsy)" = "b_bmsy",
                                                                              "Relative fishing pressure (U/Umsy)" = "u_umsy",
                                                                              "Catch/mean catch" = "c_c_mean" ),
                                                               selected = 1), width = 3
                                                   
                                                   
                                                   
                                      ),
                                      mainPanel("Time series",
                                                plotlyOutput(outputId = "tuna_plot"),
                                                hr(),
                                                textOutput("value")
                                      )
                                    )
                           ),
                           
                           tabPanel("Stock Status",titlePanel("Individual Atlantic Ocean stocks B/Bmsy, U/Umsy, and catch/mean catch"),
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
                                      mainPanel("Relative biomass, fishing pressure, and catch/mean catch by species",
                                                plotlyOutput(outputId = "b_u_catch_plot"),
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

                           tabPanel("Trends", titlePanel("Atlantic Ocean stocks trends: B/Bmsy, U/Umsy, and catch/mean catch"),
                                    sidebarLayout(
                                      sidebarPanel(style = "position:fixed;width:inherit;",
                                                   radioButtons(inputId ="campare", 
                                                                label = h4("Select"),
                                                                choices = c("Atlantic Ocean stocks" = "Atlantic_stock", 
                                                                            "Compare species" = "Compare_species"),
                                                                selected = "Atlantic_stock"), 
                                                   
                                                   conditionalPanel(condition = "input.campare == 'Compare_species'",
                                                                    radioButtons(inputId = "comp_sp",
                                                                                 label = h5("Select species"),
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
                                                                    )),
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   width = 3
                                                   
                                                   
                                                   
                                      ),
                                      mainPanel(h4("Relative biomass trends", align = "center"),
                                                plotlyOutput(outputId = "atl_oc_plot"),
                                                br(),
                                                br(),
                                                h4("Relative fishing pressure trends", align = "center"),
                                                plotlyOutput(outputId = "atl_oc_plot2"),
                                                br(),
                                                br(),
                                                h4("Catch/mean catch trends", align = "center"),
                                                plotlyOutput(outputId = "atl_oc_plot3"),
                                                br(),
                                                br(),
                                                br()
                                                
                                      )
                                    )
                           )
                           
                           
                           
                           
                           
                )
                
)





# Server function
server <- function(input, output) {
  

  
  
##TAB TIME SERIES:  
  tuna_react <- reactive({ 
    tuna_merged %>% 
      rename(unit = input$select)
  })
  
  y_labs <- reactive({
    case_when(input$select == "explotation_rate" ~ "explotation rate",
              input$select == "catch" ~ "Total catch (Mt)",
              input$select == "biomass" ~ "Total biomass (Mt)",
              input$select == "b_bmsy" ~ "Relative biomass (B/Bmsy)",
              input$select == "u_umsy" ~ "Relative fishing pressure (U/Umsy)",
              input$select == "c_c_mean" ~ "Catch/mean catch")
  }) 
  
  
  output$value <- renderText({ 
    case_when(input$select == "explotation_rate" ~ "Exploitation rate is the proportion of biomass that is removed from a fish stock or population.",
              input$select == "catch" ~ "Total catch is simply the total quantity of fish or fishery product removed from a single population or species.",
              input$select == "biomass" ~ "Total Biomass is the total volume or “stock” of a population. Here, it is measured as a weight, in metric tons (MT). Though it is perhaps one of the most useful parameters in estimating the health of a specific fishery, it should be noted that total biomass does not indicate a population’s age distribution. (citation)",
              input$select == "b_bmsy" ~ "Relative biomass (B/Bmsy) is the ratio of observed biomass (total population volume) to the  at Maximum Sustainable Yield (MSY).  MSY is the highest catch that can be removed from a continuously fished population, given average environmental conditions.",
              input$select == "u_umsy" ~ "Relative fishing pressure (U/Umsy) is the ratio of the fishing mortality rate (U) of a target species to that same species’ mortality rate, adjusted to achieve maximum sustainable yield (Umsy).",
              input$select == "c_c_mean" ~ "Catch / catch mean is the ratio of annual catch to the average catch overall, and is another helpful parameter for assessing fishing pressure.")
    
  })
  

  output$table <- renderPrint({
    if(input$table1=="1"){
      img(src='north_albacore.png', height = '800px') }
    
    else
      if(input$table1 == "2"){
        img(src='south_alb.png', height = '800px') }
        
    else
      if(input$table1 == "3"){
        img(src='e_bluefin.png', height = '800px') }
    else
      if(input$table1 == "4"){
        img(src='w_bluefin.png', height = '800px') }
    else
      if(input$table1 == "5"){
        img(src='bigeye.png', height = '800px') }
    else
      if(input$table1 == "6"){
        img(src='blue_marlin.png', height = '800px') }
    else
      if(input$table1 == "7"){
        img(src='east_sail.png', height = '800px') }
    else
      if(input$table1 == "8"){
        img(src='west_sail.png', height = '800px') }
    else
      if(input$table1 == "9"){
        img(src='north_albacore.png', height = '800px') }
    else
      if(input$table1 == "10"){
        img(src='https://dl.boxcloud.com/api/2.0/internal_files/785244974875/versions/839032428475/representations/png_paged_2048x2048/content/1.png?access_token=1!72y7WF907o4tP6thNLJiTff_cI6pcJrS3FNjKAMCyCZBkGSNwuR5Q-i9sp6fSay2xVL6FAf0zGTE7qExfM4VZqpN4HiJdB4a9lgJLFILtTh4F4HBsGUo-EcGLApO-DTPuep9jkFysdVFzvhw4BnSWuOTZ3Qqj8t1MuDpLiEaJgY5SXLdGQYwZtoWTOmMRRfU_sB3HW1ZVI-mlQ0PF09URdKK-sMd5zI9mqqqttDeYEGLZUfwL2Uaa7D40jPveLeA_88RadRlq5AH-EwjFkoMaW1yMW71vKw_JRMcWWtybPZj-dh6prfK-LbC3YNqnVd3iK4IXY_4R6r3VqpQ2zMXcRA2WB2xkDqgL9erHvx_xhGUPfhG8W7f-8XOCoIZSbpjOWALdEL8UoCiQ9kt2HVXsvj9HKw-HDr0zi201a39pQV7jt4z1tUvB7eQMadG4fftFKjjG9WmyIoYpFsglaNg_ANZ1K6y7YK76dZT7-fQHKmTX0hyNGLUUHFEkYHdx_mDP1TVdOzGKnkvMZCN_TVVIjHkB31mO0zwKVKs3wGec-17M4ofPuHK0fb16KbWPL1QTXSA4IAlX608LTOFa74BVIlsrUT6fibst-6JAuqiIoJkkDxjAd9XLD47JvUzGe2FiHAuGXNyeQrRdOf0GjtvqvX18L8qsPODlalObSHuCGTWAf38&box_client_name=box-content-preview&box_client_version=2.65.0', height = '800px') }
    else
      if(input$table1 == "11"){
        img(src='https://dl.boxcloud.com/api/2.0/internal_files/785234611545/versions/839020016745/representations/png_paged_2048x2048/content/1.png?access_token=1!6A7K_P2KcPQOmoXNQfnzclbvxPAw6GmbzLSiqUUGPFqIGIqpX3A_qrZV1R9pyR3MQXmcp_o0UWnWy470GnxfGkD-0gpiPo8Norc8FtRFAqkQrrqzUoiwiIed3gn07e95xifasmqMoaEJWpvXsA-Fg1j4opVEx_DUkMQJJuykq-ew_CJfEXFgO2zQnlp2vAnChDsh4PaeX1QKRWvoEyoMK3A_shEpZDI8Vjb2MnjsXfuC-9nIvNgZDDTefDNHb_CeGXzbIXn81SDQLznctnuK-mQSbI6V5ilc7q2zzEQqFEHK8r-sudaNlfM6G2Cisf0tpuK7dBM484e2lfvBb9bP0IDUk04IpRUvspjX_Tw2NtIQSRgD6Ou9qkQcV9nvU3drZ-MlrB-UKwYGfepj-W3pO_4gPRBXLgqUkVyb43BfWiXsADfCGnMsq_uWvPUkzicF8bibFOzVNRhkXz7mwWPh5UMVT8pcevkNARLXkTmR2e6uDm7JtS-xvtXUP82VOqUEAG9-vIquAXZ6C21s6HF28Rwe4ZleTf7mOFELB2vYt4vNULj_VWi-otbHRHFrCcoIRmwVMgNGE0EorUMDocQWV84AF9A_GtekqG_TkEQNLAIWAhw9RyCGx49UX5I5Jkn858gIWfU-MabyfaS7oqtvyKMYpP9j0IEGGo2SZZ2alxS4mLz0&box_client_name=box-content-preview&box_client_version=2.65.0', height = '800px') }
    else
      if(input$table1 == "12"){
        img(src='https://dl.boxcloud.com/api/2.0/internal_files/785249806996/versions/839037537796/representations/png_paged_2048x2048/content/1.png?access_token=1!01Z3Ht9ey9iScjNtpPmIZqVNoZL89POnnzVnNSxZIJ8ju7lW4AQQaIpN7D8ydoAR5trwYd8G1Av1bVqazRpyk-lefHm1GJap_LC6FTxmSP4JV9Pd59wmlTPZK6rn5LxVCA0PgRt3_viW0Tc6yGRETnOS1IkN2zCLLKV9kzun5um3VQJIznGHEQ2VshM_BoHugz9Ik35vZPvT4ax4tgNWvAt9xpC1Ck2lMasitPpELbvPSn9d2TRZnQEM1gRYE90Y5HVEnruFwXk-A9mws2we-vlC4KL7BPl_1zvnCpRrsq883v09kQ_GDrEaq9lMmm44ii7aulvbxL9m6Nfxs7YC5Ghmbo3bIJ22wK_WxRu7ScxQduQ6pqwwU0Qu9uBimd-U44DvlF_hYAiH5foTRulPJZsnSdtTyOMeU09DBjjbbluQatvWwdH-3bchzFh6tWeZWlvwTG438TcRtkCk68ljCjkoSOYY3WmnnC21h-ECJcmDSTxpOO9Z_e8JTkc2uCYSq5PIbtERJMyu-lPLITTGcYANXnVK7hY36Q7iJz11kceK2SLinEDiBNccrQ7khzXXrz_Bnx8okMOvqxxt6DTSNdvsk9ts661OSiE6zIO2-3-b2w8NERF6KsLLZJ-1yHVbdxtDbksQTG7JbA5nVzMdy3pAUK8-BHHA0XH-8hy7GWUQAEu6&box_client_name=box-content-preview&box_client_version=2.65.0', height = '800px') }
    else
      if(input$table1 == "13"){
        img(src='https://dl.boxcloud.com/api/2.0/internal_files/785250448457/versions/839038311257/representations/png_paged_2048x2048/content/1.png?access_token=1!T9lPYiInFbcF1j0Gj1trlE3lPHZf6q9nON0e6q0u3XC7P8QxA7ui58z5fODlRSOm1rcRxDQyMySMJrRtkroVIa216B9xbbxVYw72bCtd0Vuj7wkFquFTf9UGnKwNH55oIjoTWZ2zwH9QZGCvuVrS-0FWU-4FF3m0-XbCYwakbks3AVSXHcLzZ5NLxDbr6YJVz7s36kl3uGd7LuXKpbT-zdppDIA3B_iCxpu8appZLfDcQxiOPtU0FO5vf8Vh95aSo-wloEZTNcOahwiWwedUn4qucUyNGL9QkyYm5QruNC2LoNQDNsl31GadiDyhZC946GV3U353esP7__Up6Mugxg2XTmRRBLcUAlgkMVoYrV7rvQinL5vm0TrIw7Yjs9pkLF3_jwcScLQxwawZ1ZvfCE-zoRIddJw0TyrtejYCKXQ_obXDQEXABHQwKmOVdB2qLa0pnQm1zgx6MLEEuXcDp2OcSjkriCroa6XbWL-a8OaruaibcFVAKTgzdIVg33d-Nz3lq9bpaJkiPR1tnMqVH5uadmlGNHJeOc_fXYgevsw3QLkc7GaunabpdixehRuXeeH6JjwvTb2hqhA_aRuoJ9eF8Yzob0r6v8-ekfn3pHJ4FDAn4VmtDUDg9miNuFUFfAeXGm0cFL-OlnpCCeeSCfQ3X1ByrhDWnO9tYFhvmcpvMKxW&box_client_name=box-content-preview&box_client_version=2.65.0', height = '800px') }
    else
      if(input$table1 == "14"){
        img(src='https://dl.boxcloud.com/api/2.0/internal_files/785250465226/versions/839038382026/representations/png_paged_2048x2048/content/1.png?access_token=1!UNr1VMg_tQufjz3Vd38OegGniaAiOz_jkcdtW0f9vnNDidyyE4067igzjm6rp0DSwXSxQog0BG9_2RKWlx4puVepQqzyi4UUzNGilPUmLabGwR0lk_z13ppsDeRbPlrOndh98DNqFGn5kDuDzZdrNWqmv_aGSF05DA1hwtkpjiUlcr9TYCnq47guRtktxiYS7BYegO20mq8tZITDgjPs3FEq69MesHqwGbij_0VQ-tifv7xc9nm7-jdrWGA1eRUR2FmApYAM_pXGW1t5WF5TdFl6WQjUzenXZHeCf99zZAhoRGo_cgbntFOVzfLb_m9a_Gqej-FD-wy_9G_aWxuwD0Xpv-vTeWfPkz3Tn7d9fFhPZRId5IWRE2DMS9AT1nxmtSodVXqsdHUMs40pbF7a-uE9Bj4IlDxNvZlMSYYRjlekb_VT3MdbF9zXgHlMA9wNtkUVsMxM4MJEs5JbPjUG9ThGozpjkmDGCJ37hDx03u4-hffQ8ISF_RxVCvDAYV5bZDIhwEB8pbwHM0M5OkYdvz7naCI9ld72_A2de1Y92VzPVy2mlgrAmSjwojtuER94K6NP0n0j2Vwq9s0m492gDizaMXNP6ND7JljkTQBGjp5BF210BJg_HNiVSCDKyBsi1zuW_1XB7rpuWalYncpsi1mXGaJxp0JwN0SahTYoeOOt_6kn&box_client_name=box-content-preview&box_client_version=2.65.0', height = '800px') }
    
    
     
  })

  
  

  output$tuna_plot <- renderPlotly({
    ggplotly(ggplot(data = tuna_react(), aes(x = year, y = unit)) +
               geom_line(aes(color = species)) +
               theme_minimal() +
               labs(x = "Year",
                    y = y_labs()) +
               theme(legend.title = element_blank())
    )
  })
  
  
  
##TAB STOCK STATUS:
  
  
  tuna_relative_react <- reactive({ 
    tuna_merged %>% 
      filter(species == input$species) %>% 
      filter(year >= input$date1, year<=input$date2)
  })
  
  
  
  output$b_u_catch_plot <- renderPlotly({
    ggplot(data = tuna_relative_react(), aes(x = year,)) +
      geom_line(aes(y = b_bmsy, color = "B/Bmsy"), alpha = 0.7) +
      geom_line(aes(y = u_umsy, color = "U/Umsy"), alpha = 0.7) +
      geom_line(aes(y = c_c_mean, color = "Catch/mean catch"), alpha = 0.7)+
      geom_hline(yintercept = 1, color = "orange2") +
      scale_color_manual(values = c( "B/Bmsy" = "red3", "U/Umsy" = "palegreen4", "Catch/mean catch" = "dodgerblue1")) +
      labs(color = "")+
      labs(y = "")+
      theme_minimal()
    
  })
  
#TAB TENDS:
#Tab 5 output 1 (B/Bmsy):
  
  output$atl_oc_plot <- renderPlotly({
    if (input$campare ==  "Atlantic_stock")  {p1} 
    else {
      
      tuna_region_react <- reactive({ 
        tuna_merged %>% 
          filter(species == input$comp_sp) %>% 
          group_by(year)
      })
      
      
      
      p11 <- ggplot()+ 
        geom_boxplot(data = tuna_region_bmsy1, aes(x= factor(year), y = b_bmsy), 
                     fill = "grey",
                     color = "grey",
                     width = 0.2, 
                     outlier.color = NA,
                     outlier.shape = NA)+
        stat_summary(data = tuna_region_bmsy1, aes(x= factor(year), y = b_bmsy, group = 1),
                     fun = mean, 
                     geom = "line",
                     alpha = 0.7,
                     size = 0.5,
                     color = "grey")+
        stat_summary(data = tuna_region_react(), aes(x= factor(year), y = b_bmsy, group = 1),
                     fun = mean, 
                     geom = "line",
                     alpha = 0.7,
                     size = 0.5,
                     color = "red")+
        geom_hline(yintercept = 1, color = "orange2")+
        scale_x_discrete("Year", breaks = c("1950","1960", "1970", "1980", "1990", "2000", "2010", "2018") )+
        scale_y_continuous(breaks=c(0, 1, 2, 3, 4), limits = c(-0.1, 4.5))+
        labs(y = "Relative biomass (B/Bmsy)")+
        theme_minimal()
      p11 <- plotly_build(p11)%>% 
        layout(annotations = 
                 list(x = 0.2, y = -0.1, text = "Red line shows mean B/Bmsy.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10))
        )
      
      p11$x$data <- lapply(p11$x$data, FUN = function(x){
        x$marker = list(opacity = 0)
        return(x)
      })   
      p11
      
      
    } 
    
  })
  
  
  
  
  
  
  output$atl_oc_plot2 <- renderPlotly({
    if (input$campare ==  "Atlantic_stock")  {p2} 
    else {
      
      tuna_region_react2 <- reactive({ 
        tuna_merged %>% 
          filter(year>=1970) %>%
          filter(species == input$comp_sp) %>% 
          group_by(year)
      })
      
      
      
      p22 <- ggplot()+ 
        geom_boxplot(data = tuna_region_2, aes(x= factor(year), y = u_umsy), 
                     fill = "grey",
                     color = "grey",
                     width = 0.2, 
                     outlier.color = NA,
                     outlier.shape = NA)+
        stat_summary(data = tuna_region_2, aes(x= factor(year), y = u_umsy, group = 1),
                     fun = mean, 
                     geom = "line",
                     alpha = 0.7,
                     size = 0.5,
                     color = "grey")+
        stat_summary(data = tuna_region_react2(), aes(x= factor(year), y = u_umsy, group = 1),
                     fun = mean, 
                     geom = "line",
                     alpha = 0.7,
                     size = 0.5,
                     color = "red")+
        geom_hline(yintercept = 1, color = "orange2")+
        scale_x_discrete("Year", breaks = c("1970", "1980", "1990", "2000", "2010", "2018") )+
        scale_y_continuous(breaks=c(0, 1, 2, 3, 4), limits = c(-0.1, 4.5))+
        labs(y = "Relative fishing pressure (U/Umsy)")+
        theme_minimal()
      
      p22 <- plotly_build(p22)%>% 
        layout(annotations = 
                 list(x = 0.2, y = -0.1, text = "Red line shows mean U/Umsy.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10))
        )
      
      p22$x$data <- lapply(p22$x$data, FUN = function(x){
        x$marker = list(opacity = 0)
        return(x)
      }) 
      
      p22
      
      
    } 
    
  })
  
  
  
  
  
  
  output$atl_oc_plot3 <- renderPlotly({
    if (input$campare ==  "Atlantic_stock")  {p3} 
    else {
      tuna_region_react3 <- reactive({ 
        tuna_merged %>% 
          filter(year>=1970) %>%
          filter(species == input$comp_sp) %>% 
          group_by(year)
      })
      
      
      p33 <- ggplot()+ 
        geom_boxplot(data = tuna_region_2, aes(x= factor(year), y = c_c_mean), 
                     fill = "grey",
                     color = "grey",
                     width = 0.2, 
                     outlier.color = NA,
                     outlier.shape = NA)+
        stat_summary(data = tuna_region_2, aes(x= factor(year), y = c_c_mean, group = 1),
                     fun = mean, 
                     geom = "line",
                     alpha = 0.7,
                     size = 0.5,
                     color = "grey")+
        stat_summary(data = tuna_region_react3(), aes(x= factor(year), y = c_c_mean, group = 1),
                     fun = mean, 
                     geom = "line",
                     alpha = 0.7,
                     size = 0.5,
                     color = "red")+
        geom_hline(yintercept = 1, color = "orange2")+
        scale_x_discrete("Year", breaks = c("1970", "1980", "1990", "2000", "2010", "2018") )+
        scale_y_continuous(breaks=c(0, 1, 2, 3, 4), limits = c(-0.1, 4.5))+
        labs(y = "Catch/mean catch")+
        theme_minimal()
      
      p33 <- plotly_build(p33)%>% 
        layout(annotations = 
                 list(x = 0.26, y = -0.1, text = "Red line shows mean catch/mean catch.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10))
        )
      
      
      p33$x$data <- lapply(p33$x$data, FUN = function(x){
        x$marker = list(opacity = 0)
        return(x)
      }) 
      
      p33
      
      
    } 
    
  })
  
  
  
}

# Combine user interface + server
shinyApp(ui = ui, server = server)


