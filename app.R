# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(dplyr)

# Load data ----
counties <- read.csv('HS.csv',stringsAsFactors = FALSE)
counties <- select(counties,'state.county','Value')
# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c('Heart & Stroke'),
                  selected = 'Heart & Stroke'),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min =0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Heart & Stroke" =counties$Value )
    
    color <- switch(input$var, 
                    "Heart & Stroke" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Heart & Stroke" = "HS data")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
}

# Run app ----
shinyApp(ui, server)