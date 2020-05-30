library(leaflet)
library(dplyr)

pop.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/Shiny_test/geo.csv')

# m <- leaflet() %>% addTiles() %>% addMarkers(lng=174.768,lat=-36.852,
#     popup='The birthplace of R')
# print(m)

pop2.df <- pop.df %>% group_by(state,county) %>% 
  summarise(Long = median(longitude),Lat= median(latitude),
            pop_county = sum(estimated_population,na.rm=TRUE))

m<-leaflet(pop2.df) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(pop_county)*15 , 
             popup = ~paste(county,':',pop_county))

print(m)

# m<-leaflet(pop.df) %>% addTiles() %>%
#   addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
#              radius = ~sqrt(estimated_population) * 30, popup = ~county)
# 
# print(m)