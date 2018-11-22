#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(janitor)


#load in data, first being all data and second being coefficient data 
plot_data <- read_csv("FIRST_REGRESSIONS_CLEAN.csv") %>% clean_names()
table_data <- read_csv("final_project_stats_table.csv") %>% clean_names()

#create choices for drop down menue
choices <- plot1 %>% select(pair) %>% distinct %>% pull(pair)

#create additional variable for drop down list 

plot1 <- plot_data %>% mutate(pair = paste(reporter, partner, sep = " - "), fta_dummy = as.factor(fta_dummy))

#table 1 create additional variable to match up with previous regressions

table1 <- table_data %>% mutate(pair = paste(reporter, partner, sep = " - ")) %>% 
  select(reporter, partner, year_in_force, pair) %>% 
  rename(Reporter = reporter, Partner = partner, `Year In Force` = year_in_force)

# Define UI for application that plots features of movies 
ui <- fluidPage(
  
  # Application title
  titlePanel("Graph of Exports vs Time "),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      h4("Filters"),
      
      sliderInput("years", "year range",
                  1989, 2017, value = c(1989, 2017), step = 1, timeFormat = "%F"),
      
      
      selectInput(inputId = "pair", 
                  label = "Choose FTA to Investigate",
                  choices = choices, 
                  selected = "Canada - Chile"),
      
      
      #create checkbox for user input of states
      checkboxInput("best_fit", label = "Add Lines of Best Fit", value = FALSE),
      
      
      #create checkbox for whether of not to return table with regression data 
      checkboxInput("fe", label = " Add Summary Statistics Below Graph", value = FALSE)
      
    ),
    
    
    # Outputs
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("FTA Visualization", plotOutput("scatterplot")),
                  tabPanel("Map of FTA", verbatimTextOutput("map in this panel"))
                  ),
      
      # plotOutput(outputId = "scatterplot", height = 600, width = 800),
      # p("Choose from the different parties to see how polling margin of errors vary with voter turnout."),
       p("You can also select which state elections you would like to view."),
       tableOutput("contents")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  #filter data based on user selection
  pair_subset <- reactive({
    req(input$pair, input$years[1], input$years[2])
    filter(plot1, pair %in% input$pair, year >= input$years[1], year <= input$years[2]) 
    
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    
    #Create visualization using ggplot 
    plot1 <- ggplot(data = pair_subset(), aes_string(x = "year", y = "trade_value_us", color = "fta_dummy")) +
      geom_point(size = 3, alpha = 0.8)  +
      geom_vline(xintercept = pair_subset()$year_in_force) +
      labs(x = "Year", y = "Value of Exports (USD)") +
      ggtitle("Margin of Error vs Voter Turnout by District")
    
    print(plot1)
    
    if (input$best_fit == TRUE) {
      # creates a straight line of best fit with no wide range around it.
      bf_line <- plot1 + geom_smooth(method = lm, se = FALSE)
      
      bf_line
    }
  })
  
  #create table with coefficients from regressions
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
   
    req(input$fe)
      # creates a straight line of best fit with no wide range around it.
      df <- table1 %>% filter(pair %in% input$pair) 
      print(df)
      return(df)
    
    
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)