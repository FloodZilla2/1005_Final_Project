#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#test comment from name change

# load libraries 

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(janitor)
library(leaflet)
library(DT)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(markdown)


#load in data, first being all data and second being coefficient data 
plot_data <- read_csv("FIRST_REGRESSIONS_CLEAN.csv") %>% clean_names() %>% 
  mutate(trade_value_us = as.numeric(trade_value_us)) %>% 
  rename(`Value of Exports (USD)` = trade_value_us)

#format rad in and format the table with the coordinates for the map

table_data <- read_csv("final_project_stats_table.csv") %>% clean_names()

#create additional variable for drop down list, paste partner and reporter for new column and reformat the trade value column

plot1 <- plot_data %>% mutate(pair = paste(reporter, partner, sep = " - "), fta_dummy = as.factor(fta_dummy)) %>% 
   mutate(`Value of Exports (USD)` = as.double(`Value of Exports (USD)`))

#create choices for drop down menue

choices <- plot1 %>% select(pair) %>% distinct %>% pull(pair)


#table 1 create additional variable to match up with previous regressions, select necessary variables, change names to look better in table, mutate variables to be of proper type

table1 <- plot_data %>% mutate(pair = paste(reporter, partner, sep = " - "), fta_dummy = as.factor(fta_dummy))  %>%
  select(pair, year, year_in_force, gdp_country_1, gdp_country_2, `Value of Exports (USD)`) %>%
  mutate(`Value of Exports (USD)` = as.double(`Value of Exports (USD)`)) %>% 
  rename(Year = year, `Year In Force` = year_in_force, `Exporter GDP (USD)` = gdp_country_1, `Importer GDP (USD)` = gdp_country_2) %>% 
  mutate(`Exporter GDP (USD)` = dollar(`Exporter GDP (USD)`), `Importer GDP (USD)` = dollar(`Importer GDP (USD)`), `Value of Exports (USD)` = dollar(`Value of Exports (USD)`))
         


#map 1 data - select the co-ordinate columns from the table_table tibble

map1 <- table_data %>% mutate(pair = paste(reporter, partner, sep = " - ")) %>% 
  select(pair, reporter_longitude, reporter_latitude, partner_latitude, partner_longitude)

# Define UI for application that plots features of movies 

ui <- fluidPage(theme = shinytheme("sandstone"),

                

#create Navbar layout 

navbarPage("",
           tabPanel("About This App",
                    
                    #Title for explainatory page of app
                    
                    titlePanel("Investigating Bilateral Free Trade Agreements"),
                    
                    br(),
                    
                    p("This interactive app can be used to to explore 187 bilateral free trade agreements, through 3 different lenses. After reading through the instructions below, 
                      click the Explore the Data tab to continue."), 
                    
                    br(),
                    
                    h3("How to Use the Graph"),
                    
                    br(),
                    
                    #Explaination for how to use the first tab
                    
                    p("The graph, found in the first tab of the exploration page, shows how bilateral exports from the first country listed to the second
                      have changed over the last 30 years. The vertical black line shows when the FTA went into the effect, 
                      the red points represent data points before the FTA was in effect and the blue points represent after.
                      You can add regression lines by clicking the lines of best fit button in the side panel to see how the trends have changed since the FTA went into effect,
                      or change the time period by ajusting the slider."),
                    
                    br(),
                    
                    h3("How to Use the Map"), 
                    
                    br(),
                    
                    #Explain the second tab
                    
                    p("You can find a map pin-pointing the two countries in the agreement under the second tab. 
                      Zoom in and out using the buttons in the top left corner, and use the mouse to move around the area being viewed."),
                    
                  
                    br(),
                    
                    h3("How to Use the Data Table"), 
                    
                    br(),
                    
                    #Explain the third tab
                    
                    p("To view the raw data being used to create the line plot, as well as other variables including yearly
                      GPA data, look under the third tab. Often in trade analysis, GDP variables are included in the given trade model
                      as trade is highly correlated with GDP size. Thus panel GDP data is included for if you would like to download the data
                  (by clicking the Download button in the side panel) and do your own analysis on the causal impact of the FTA on bilateral exports.")
           ),
                
           tabPanel("Explore the Data",
  
  br(),
  br(),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout( 
    
    # Inputs
    sidebarPanel(class = "text-left",
      h4("Choose FTA and Filter Data", class = "text-center"),
      
      br(),
      
      #create input selection box to choose an FTA to look at
      
      selectInput(inputId = "pair", 
                  label = "Choose FTA to Investigate",
                  choices = choices, 
                  selected = "Canada - Chile"),
      br(),
      
      # Create a slider box to choose a range of years for the graph plot
      
      sliderInput("years", "year range",
                  1989, 2017, value = c(1989, 2017), step = 1, timeFormat = "%F"),
      
      br(),
      
      #create checkbox for user input of adding a trendline
      
      checkboxInput("best_fit", label = "Add Lines of Best Fit", value = FALSE), 
      
       br(),
      
      #create the download button 
      downloadButton("downloadData", "Download This FTA's Trade Data")
 
      
    ),
    
    
    # Outputs
    
    mainPanel(
      
      #create tab layout for the differente exploratory elements of the app
      
      tabsetPanel(type = "tabs",
                  tabPanel("FTA Visualization", br(), plotOutput("scatterplot")),
                  tabPanel("Map of the 2 Countries", br(), leafletOutput("mymap")),
                  tabPanel("The Data", br(), DT::dataTableOutput("table1"))
                  ),
      
      br(),
      br()
      
    )
  )
)
)
)

# Define server function required to create the different plots, maps and tables
server <- function(input, output) {
  
  #filter data based on user selection using a reactive sequence
  
  pair_subset <- reactive({
    req(input$pair, input$years[1], input$years[2])
    filter(plot1, pair %in% input$pair, year >= input$years[1], year <= input$years[2]) 
    
  })
  
  # Create scatterplot object the plotOutput function is expecting
  
  output$scatterplot <- renderPlot({
    
    #Create visualization using ggplot, adding the vertical line for the FTA, labelling axis and scaling axis, and change the themes
    
    plot1 <- ggplot(data = pair_subset(), aes_string(x = "year", y = "`Value of Exports (USD)`", color = "fta_dummy")) +
      geom_point(size = 3, alpha = 0.8)  +
      geom_vline(xintercept = pair_subset()$year_in_force) +
      labs(x = "Year", y = "Value of Exports (USD)", color = "FTA Dummy (0 before, 1 after)") +
      ggtitle(paste("Exports vs Time,", input$pair, sep = " ")) +
      scale_y_continuous(labels = comma) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      theme_light()
    
    #print this plot
    
    print(plot1)
    
    #create condition to add lines of best fit
    
    if (input$best_fit == TRUE) {
      
      # create a straight line of best fit
      
      bf_line <- plot1 + geom_smooth(method = lm, se = FALSE)
      
      bf_line
    }
  })
  
  #create and display table data from plot 1 as well as GDP data and other variables 
  output$table1 <- DT::renderDataTable({
   
    
     #filters so only data from selected pair is shown
    
      df <- table1 %>% filter(pair %in% input$pair) 
      print(df)
      return(df)
    
    
  })
  
  #create output for download button
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$pair, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pair_subset(), file, row.names = FALSE)
    }
  )
  
  # Create and call map obect for second tab
  
  output$mymap <- renderLeaflet({
    
    #reformat data so coordinates can be recognised by leaflet function. 2 gathers then a reactive filter seems to do the trick
  
    req(input$pair)
    points <- map1 %>% gather(key = "type", value = "lat", reporter_latitude,partner_latitude) %>% 
      gather(key = "type2", value = "long", reporter_longitude:partner_longitude) %>% 
      filter(pair %in% input$pair) %>% 
      slice(-2:-3) %>% 
      select(lat, long)
    
    # create the map by adding tiles, markers etc 
    
   myMap1 <-  leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points) 
   
   # Call the map 
   
   myMap1 
   
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)