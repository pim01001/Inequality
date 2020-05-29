library(dplyr)
library(ggplot2)
library(maps)

#gives sates name colum name of 2 letter abb
names(state.name)<-state.abb

# function helps take state (AL) and county and combines the data
state_count <-function(state,county){
  if(nchar(state)==2){
    state = state.name[state]
    tt<- tolower(paste0(state,',',county))
  }else{
    tt <- tolower(paste0(tate,',',county))
  }
   return(tt)
}



HS.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/nteractive Atlas of Heart Disease and Stroke Tables.csv',
                stringsAsFactors = FALSE)

HS.df$name <-state_count(HS.df$State,HS.df$County)

HS.df<-select(HS.df,"name",'Value')

# tt is being using to order that data in certain way so map function works
tt.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/Shiny_test/test1/data/data.csv',stringsAsFactors = FALSE)

ll.df<-left_join(tt.df,HS.df,by='name')

ll.df<-select(ll.df,name,Value)

#write.csv(ll.df,'/home/pim01001/Documents/Bootcamp/R/shiny_proj/HS.csv')

#----------------------------------------------------

#reads only the header
income.header <-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/PovertyEstimates.csv',
                        skip=4,nrow=1,header = FALSE,stringsAsFactors = FALSE)

income.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/PovertyEstimates.csv',
                    skip=5,header = FALSE,stringsAsFactors = FALSE)
colnames(income.df)<-income.header

#getting rid of County after each county name
income.df$Area_name<-gsub(' County','',income.df$Area_name)

income.df$name<- state_count(income.df$Stabr,income.df$Area_name)
income.df <-left_join(ll.df,income.df,by='name')
income.df <- select(income.df,-Value)
#----------------------------------------
geo.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/Geocodes_USA_with_Counties.csv',
                   stringsAsFactors = FALSE)

# get rid of empty space colums
geo.df<-geo.df[geo.df$county %in% c('')==FALSE,]
# filter our PR 
geo.df <- geo.df %>% filter(., state!='PR')
geo.df<-select(geo.df,'state','county','primary_city',
               'longitude','latitude','estimated_population')
geo.df$name <- state_count(geo.df$state,geo.df$county)

#-----------------------------------


# #------test of map function-------------------------------
# #percent_map <- function(var, color, legend.title, min = 0, max = 100) {
#   var <- ll.df$Value
#   color <- 'darkviolet'
#   legend.title <-"HS data"
#   # generate vector of fill colors for map
#   shades <- colorRampPalette(c("white", color))(100)
#   
#   # constrain gradient to percents that occur between min and max
#   min=500
#   max=800
#   var <- pmax(var, min)
#   var <- pmin(var, max)
#   # 
#   
#   percents <- table(cut(var, 100, 
#                              include.lowest = TRUE, ordered = TRUE))
#   fills <- shades[percents]
#   #min <- min(percents,na.rm = TRUE)
#   #max <- max(percents,na.rm = TRUE)
#   
# 
#   par(mar=c(1,1,1,1))
#   # plot choropleth map
#   map("county", fill = TRUE, col = fills, 
#       resolution = 0, lty = 0, projection = "polyconic", 
#       myborder = 0, mar = c(0,0,0,0))
#   
#   # overlay state borders
#   map("state", col = "white", fill = FALSE, add = TRUE,
#       lty = 1, lwd = 1, projection = "polyconic", 
#       myborder = 0, mar = c(0,0,0,0))
#   
#   # add a legend
#   inc <- (max - min) / 4
#   legend.text <- c(paste0(min, " % or less"),
#                    paste0(min + inc, " %"),
#                    paste0(min + 2 * inc, " %"),
#                    paste0(min + 3 * inc, " %"),
#                    paste0(max, " % or more"))
#   
#   legend("bottomleft", 
#          legend = legend.text, 
#          fill = shades[c(1, 25, 50, 75, 100)], 
#          title = legend.title)
# 
# 
# #percent_map(ll.df$Value, 'darkviolet', "HS data", min = 0, max = 100)
# 
# 
# 
