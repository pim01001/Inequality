library(dplyr)
library(ggplot2)
library(maps)

HS.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/Shiny_test/inequality/nteractive Atlas of Heart Disease and Stroke Tables.csv',
                stringsAsFactors = FALSE)

names(state.name)<-state.abb

HS.df <- mutate(HS.df,'state_full'=state.name[HS.df$State])

HS.df <- mutate(HS.df,
                'state.county' = paste0(HS.df$state_full,',',HS.df$County))
