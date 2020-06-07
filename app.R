# Load packages ----
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# Load data ----
# Load data ----
comb.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/comb.csv',stringsAsFactors = FALSE)

comb.df <- comb.df %>% group_by(state,county) %>% 
  summarise(Long = median(longitude),Lat= median(latitude),
            HS_county = median(Value,na.rm=TRUE),
            Poverty = median(PCTPOVALL_2018))
# add a columnfor re
names(state.division)<-state.abb
comb.df$region <- state.division[comb.df$state]

# User interface ----
ui <- fluidPage(
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Map",
                            
                         sidebarLayout(
                           sidebarPanel(
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
                             fluidPage(leafletOutput("map",height = 700,width = 900))
                           )
                          )
                         
                         ),
                tabPanel("Density Plot",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("Xaxis", 
                                         label = "X axis of Plot",
                                         choices = c('Heart & Stroke','Income'),
                                         selected = 'Heart & Stroke'),
                             selectInput("Yaxis", 
                                         label = "Y axis of Plot",
                                         choices = c('Heart & Stroke','Income'),
                                         selected = 'Income')
                            
                           ),
                           mainPanel(
                             fluidPage(plotOutput("density"))
                           )
                           
                         )
                  

                )
    
  )
  
  
  )
)

# Server logic ----
server <- function(input, output, session) {
  observe({
    # updates the slider based on variable toggled like  HS & income
    
    tt <- switch (input$var,
                  "Heart & Stroke" = updateSliderInput(session,'range',label = "Range of Heart & Stoke Rate:",
                                                       min=min(comb.df$HS_county,na.rm=TRUE),
                                                       max=max(comb.df$HS_county,na.rm=TRUE)),value = c(min, max),
                  
                  'Income'=updateSliderInput(session,'range',label = "Range of % Poverty:",
                                             min=min(comb.df$Poverty,na.rm=TRUE),
                                             max=max(comb.df$Poverty,na.rm=TRUE),value = c(min, max))
    )
    
  })
  
  output$map <- renderLeaflet({
    
    
    data.df <- switch(input$var, 
                      "Heart & Stroke"=comb.df %>% dplyr::filter(.,HS_county >= input$range[1] & HS_county <= input$range[2]) %>%
                        leaflet()  %>% addTiles() %>% setView(-96.98,38.615, zoom = 4.2)%>%
                        addCircles(lng = ~Long, lat = ~Lat, weight = 2,
                                   radius = ~HS_county^1.5, 
                                   popup = ~paste(county,',',state,':',HS_county)),
                      'Income'= comb.df %>% dplyr::filter(.,Poverty >= input$range[1] & Poverty <= input$range[2]) %>% 
                        leaflet() %>% addTiles() %>% setView(-96.98,38.615, zoom = 4.2)%>%
                        addCircles(lng = ~Long, lat = ~Lat, weight = 2,
                                   radius = ~Poverty^3, color = 'red',
                                   popup = ~paste(county,',',state,':',Poverty)))
    
    
    
  })
  output$density <- renderPlot({ 
    
    switch(input$Xaxis,
           'Heart & Stroke'= xx <- 'HS_county',
           'Income'=xx <-'Poverty')
           
    switch(input$Yaxis,
           'Heart & Stroke'= yy <- 'HS_county',
           'Income'=yy <-'Poverty')    

    
    ggplot(comb.df,aes_string(x=xx,y=yy,color='region',shape='region'))+ 
      geom_point()+stat_ellipse(size=1.5)+
      theme_classic()
  })
}

# Run app ----
shinyApp(ui, server)