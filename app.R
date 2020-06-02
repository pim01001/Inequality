# Load packages ----
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# Load data ----
comb.df <- read.csv('comb.csv',stringsAsFactors = FALSE)

comb.df <- comb.df %>% group_by(state,county) %>% 
  summarise(Long = median(longitude),Lat= median(latitude),
            HS_county = median(Value,na.rm=TRUE),
            Poverty = median(PCTPOVALL_2018))


# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c('Heart & Stroke','Income'),
                  selected = 'Heart & Stroke'),
      
      sliderInput('range', 
                  label = "Range of interest:",
                  min =min(comb.df$HS_county,na.rm=TRUE), max=max(comb.df$HS_county,na.rm=TRUE), 
                  value = c(min, max))
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
server <- function(input, output, session) {
  observe({
      x <-input$var
      tt <- switch (input$var,
                    "Heart & Stroke" = updateSliderInput(session,'range',label = "Range of interest:",
                                                         min=min(comb.df$HS_county,na.rm=TRUE),
                                                         max=max(comb.df$HS_county,na.rm=TRUE)),value = c(min, max),
                    
                    'Income'=updateSliderInput(session,'range',label = "Range of interest:",
                                               min=min(comb.df$Poverty,na.rm=TRUE),
                                               max=max(comb.df$Poverty,na.rm=TRUE),value = c(min, max))
      )
    
    
    # print(x)
    # if (x == 'Income'){
    #   updateSliderInput(session,'range',min=min(comb.df$Poverty),max(comb.df$Poverty))
    # } else {
    #   updateSliderInput(session,'range',min=min(comb.df$HS_county),max(comb.df$HS_county))
    # }
  })
  
  output$map <- renderLeaflet({
    
    data.df <- switch(input$var, 
                   "Heart & Stroke"=leaflet(comb.df) %>% addTiles() %>%
                     addCircles(lng = ~Long, lat = ~Lat, weight = 2,
                                radius = ~HS_county^1.5, 
                                popup = ~paste(county,',',state,':',HS_county)),
                   'Income'= leaflet(comb.df) %>% addTiles() %>%
                     addCircles(lng = ~Long, lat = ~Lat, weight = 2,
                                radius = ~Poverty^3, color = 'red',
                                popup = ~paste(county,',',state,':',Poverty)))
   
  
    
    # leaflet(comb.df) %>% addTiles() %>%
    #   addCircles(lng = ~Long, lat = ~Lat, weight = 2,
    #              radius = data.df^3, 
    #              popup = ~paste(county,',',state,':',HS_county))
    # 
  })
  
  # density plot q
 
  
  output$density <- renderPlot({ 
    
    ggplot(counties,aes(Value,fill=TRUE))+
      geom_vline(xintercept = mean(counties$Value,na.rm = TRUE),linetype='dashed',color='blue',size=2) +
      geom_density(size=1,alpha=0.1)+
      scale_color_brewer(palette="Paired") + theme_classic()
    },height = 400,width = 600)
  
} # end of server

# Run app ----
shinyApp(ui, server)