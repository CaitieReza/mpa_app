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
                           
                           tabPanel("Species Profiles",
                                    titlePanel("Basic Species Information"),
                                    sidebarLayout(
                                      sidebarPanel("Click a species to learn more:",
                                                   radioButtons(inputId = "table1", 
                                                                label = h3("Species"), 
                                                                choices = c(
                                                                  "Northern Atlantic Albacore tuna",
                                                                  "South Atlantic Albacore tuna" = 2,
                                                                "Bigeye tuna" = 3,
                                                                "Blue Marlin" = 4,
                                                                "Eastern Atlantic Sailfish" = 5,
                                                                "Western Atlantic Sailfish" = 6,
                                                                "Eastern Atlantic Skipjack" = 7,
                                                                "Western Atlantic Skipjack" = 8,
                                                                "North Atlantic Swordfish" = 9,
                                                                "South Atlantic Swordfish" = 10,
                                                                "White marlin" = 11,
                                                                "Yellowfin tuna" = 12)
                                                                  
                                                                
                                                   )
                                      ),
                                      mainPanel("Possibly some explainers here",
                                                imageOutput("table")
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
  
  
  
##TAB SPECIES PROFILES:  
  
  output$table <- renderImage({
    if(input$table1=="Northern Atlantic Albacore tuna")
      img(src='south_albacore.png', height = '300px')
    else
      img(src='biomass.png', height = '300px')
  })  
  
  
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


