#############
#### TFGM occupancy data generation
####################




## Loading library
rm(list = ls())

library(tidyverse)
library(lubridate)
library(utilities)

### reading data 

df = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM cleaned data.csv")


df$start_time_stamp <- floor_date(df$start_time_stamp, "30 mins") 

## now this can be matched with carbon intensity data to allocate a carbon cost to each charge event. 
df_sept  <- df %>%  filter(start_time_stamp >= as.Date('2018-09-01') & start_time_stamp <= as.Date('2018-09-10'))

df_sept <-select(df_sept, c('Total kWh','start_time_stamp'))

df_sept<-df_sept%>% group_by(start_time_stamp)%>% summarise(kwh_total=sum(`Total kWh`))

df_energy <- read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/carbon intensity data.csv")

df_energy <- df_energy %>%  filter(`Datetime (UTC)` >= as.Date('2018-09-01') & `Datetime (UTC)` <= as.Date('2018-09-10'))

df_energy<-rename(df_energy,time_stamp = `Datetime (UTC)`)
df_sept<-rename(df_sept,time_stamp =start_time_stamp)

df_list<-list(df_energy, df_sept)

carbondata<-df_list%>%reduce(full_join,by='time_stamp')

carbondata$carbon_cost <-carbondata$`North West England`*carbondata$kwh_total

carbondata$carbon_cost <- carbondata$carbon_cost %>% replace_na(0)
carbondata$kwh_total <- carbondata$kwh_total %>% replace_na(0)

ggplot(data=carbondata, aes(x=time_stamp)) + geom_line(aes(y=carbon_cost)) + geom_line(aes(y=kwh_total))


### plot carbon cost with total kwh demand
p <- ggplot(carbondata, aes(x = time_stamp))
  p <- p + geom_line(aes(y = carbon_cost, colour = "Carbon cost"))


  p <- p + geom_line(aes(y = kwh_total/0.0085, colour = "Total kWh"))

  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*0.0085, name = "Total kWh"))

  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = "Carbon cost",
              x = "Date and time",
              colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.9))
p

cor(carbondata$carbon_cost,carbondata$kwh_total,method=c("pearson"))







