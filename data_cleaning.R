library(dplyr)
library(ggplot2)
library(maps)

HS.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/nteractive Atlas of Heart Disease and Stroke Tables.csv',
                stringsAsFactors = FALSE)

names(state.name)<-state.abb

HS.df <- mutate(HS.df,'state_full'=state.name[HS.df$State])

HS.df <- mutate(HS.df,
                'name' = paste0(HS.df$state_full,',',HS.df$County))
HS.df<-select(HS.df,"name",'Value')

HS.df$name <- tolower(HS.df$name)
# tt is being using to order that data in certain way so map function works
tt.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/Shiny_test/test1/data/data.csv',stringsAsFactors = FALSE)

ll.df<-left_join(tt.df,HS.df,by='name')

ll.df<-select(ll.df,name,Value)

write.csv(ll.df,'/home/pim01001/Documents/Bootcamp/R/shiny_proj/HS.csv')

#----------------------------------------------------

#reads only the header
income.header <-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/PovertyEstimates.csv',
                        skip=4,nrow=1,header = FALSE,stringsAsFactors = FALSE)

income.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/PovertyEstimates.csv',
                    skip=5,header = FALSE,stringsAsFactors = FALSE)
colnames(income.df)<-income.header

#convert income AL into Alabama
income.df<-mutate(income.df,'state.full'=state.name[income.df$Stabr])
#getting rid of County after each county name
income.df$Area_name<-gsub(' County','',income.df$Area_name)
income.df <- mutate(income.df,
                'name' = tolower(paste0(income.df$state.full,',',income.df$Area_name)))
income.df <-left_join(ll.df,income.df,by='name')
income.df <- select(income.df,-Value)
#------test of map function-------------------------------
#percent_map <- function(var, color, legend.title, min = 0, max = 100) {
  var <- ll.df$Value
  color <- 'darkviolet'
  legend.title <-"HS data"
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  min=500
  max=800
  var <- pmax(var, min)
  var <- pmin(var, max)
  # 
  
  percents <- table(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  #min <- min(percents,na.rm = TRUE)
  #max <- max(percents,na.rm = TRUE)
  

  par(mar=c(1,1,1,1))
  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " % or more"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)


#percent_map(ll.df$Value, 'darkviolet', "HS data", min = 0, max = 100)



