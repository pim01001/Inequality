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

tt.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/Shiny_test/test1/data/data.csv',stringsAsFactors = FALSE)

ll.df<-left_join(tt.df,HS.df,by='name')

