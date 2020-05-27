# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(ggplot2)

# Load data ----
counties <- read.csv('HS.csv',stringsAsFactors = FALSE)
counties <- select(counties,'name','Value')
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
                  min =min(counties$Value,na.rm=TRUE), max =max(counties$Value,na.rm=TRUE), 
                  value = c(0, 100))
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("map")),
                  tabPanel("Density Plot", plotOutput("density")),
                  tabPanel("Table", tableOutput("table"))
        )
      
      )
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Heart & Stroke"=counties$Value )
    
    color <- switch(input$var, 
                    "Heart & Stroke" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Heart & Stroke" = "HS data")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
  
  # density plot q
 
  
  output$density <- renderPlot({ 
    
    ggplot(counties,aes(Value,fill=TRUE))+
      geom_vline(xintercept = mean(counties$Value,na.rm = TRUE),linetype='dashed',color='blue',size=2) +
      geom_density(size=1,alpha=0.1)+
      scale_color_brewer(palette="Paired") + theme_classic()
    },height = 400,width = 600)
  
}

# Run app ----
shinyApp(ui, server)