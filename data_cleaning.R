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

#-----------------------------------------------------------
geo.df <- read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/Geocodes_USA_with_Counties.csv',
                   stringsAsFactors = FALSE)

# get rid of empty space colums
geo.df<-geo.df[geo.df$county %in% c('')==FALSE,]
# filter our PR 
geo.df <- geo.df %>% filter(., state!='PR')
# filters out the work Parish for merging with HS.df
geo.df$county<-gsub(' Parish','',geo.df$county)


geo.df<-select(geo.df,'state','county','primary_city',
               'longitude','latitude','estimated_population')
geo.df$name <- state_count(geo.df$state,geo.df$county)


#-----------------------------------

HS.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/nteractive Atlas of Heart Disease and Stroke Tables.csv',
                stringsAsFactors = FALSE)

HS.df<-HS.df %>% rename(.,county=County,state=State)

HS.df<-left_join(geo.df,HS.df,by=c('state','county'))

#HS.df$name <-state_count(HS.df$State,HS.df$County)

write.csv(HS.df,'/home/pim01001/Documents/Bootcamp/R/shiny_proj/HS.csv')

#----------------------------------------------------

#reads only the header
income.header <-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/PovertyEstimates.csv',
                        skip=4,nrow=1,header = FALSE,stringsAsFactors = FALSE)

income.df<-read.csv('/home/pim01001/Documents/Bootcamp/R/shiny_proj/PovertyEstimates.csv',
                    skip=5,header = FALSE,stringsAsFactors = FALSE)
colnames(income.df)<-income.header

#getting rid of County after each county name
income.df$Area_name<-gsub(' County','',income.df$Area_name)

#income.df$name<- state_count(income.df$Stabr,income.df$Area_name)

income.df<-income.df %>% rename(.,county=Area_name,state=Stabr)
final.df <-left_join(HS.df,income.df,by=c('state','county'))

write.csv(final.df,'/home/pim01001/Documents/Bootcamp/R/shiny_proj/comb.csv')

#----------------------------------------

