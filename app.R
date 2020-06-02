# Load packages ----
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# Load data ----
counties <- read.csv('comb.csv',stringsAsFactors = FALSE)



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
                  tabPanel("Map",leafletOutput("map")),
                  tabPanel("Density Plot", plotOutput("density")),
                  tabPanel("Table", tableOutput("table"))
        )
      
      )
  )
)

# Server logic ----
server <- function(input, output) {
  
  
  output$map <- renderLeaflet({
    HS.df <- comb.df %>% group_by(state,county) %>% 
      summarise(Long = median(longitude),Lat= median(latitude),
                HS_county = median(Value,na.rm=TRUE))  
    
    leaflet(HS.df) %>% addTiles() %>%
      addCircles(lng = ~Long, lat = ~Lat, weight = 2,
                 radius = ~HS_county^1.5, 
                 popup = ~paste(county,',',state,':',HS_county))
    
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