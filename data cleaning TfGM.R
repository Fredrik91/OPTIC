# ################################################################# #
#### TFGM Charge Event data 
# ################################################################# #


#### Data cleaning of charge event data through several steps:

## Step 1 load library
## Step 2 importing data
## Step 3 Generating variables
## Step 4 Dropping missing data and obsolete variables
## Step 5 Drop outliers and erroneous values
## Step 6 Generate summary statistics on the site level
## Step 7 Exporting data
## Step 8 Summary statistics and visualisations

## Step 1 load library ####
rm(list = ls())

library(tidyverse)

## Step 2 importing data #####

df = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Use.csv")
#assets = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Asset Inventory.csv")
#assets_reduced = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Assets_reduced.csv")
 
## Step 3. Generate variables ####

# 1. generate timestamp from string variables. 
# 2. Generate charge duration in minutes. 
# 3. Generate power output variable 
# 4. Generate minimum plugin time
# 5. Generate idle plugin time
# 6. Generate end timestamp for charging time

df$start_time_stamp <- as.POSIXct(paste(df$`Start Date`, df$`Start Time`), format="%d/%m/%Y %H:%M:%S")
df$end_time_stamp <- as.POSIXct(paste(df$`End Date`, df$`End Time`), format="%d/%m/%Y %H:%M:%S")
df <- mutate(df,charge_duration = end_time_stamp - start_time_stamp)
df$charge_duration <- as.numeric(df$charge_duration, units = "hours")
df$charge_duration <- df$charge_duration*60

df$power_output = 0
df$power_output[str_detect(df$Model,'3.6kW')] <-3.6
df$power_output[str_detect(df$Model,'7kW')] <-7
df$power_output[str_detect(df$Model,'Rapid')] <-50


df <- mutate(df,minimum_plugin_time =df$`Total kWh`/ df$power_output)
df$minimum_plugin_time<- df$minimum_plugin_time*60
df <-mutate(df,idle_plugin_time=df$charge_duration - df$minimum_plugin_time)


df$end_time_stamp_charge <- df$start_time_stamp + dminutes(df$minimum_plugin_time)
df$end_time_stamp_charge <- round_date(df$end_time_stamp_charge,unit="1 minute")


## Step 4. Dropping missing data and obsolete variables####
#### Drop missing variables. Missings are present in timestamp, total kwh and vehicle. Missings in vehicle are not relevant.  
# missings in timestamps. Complete cases function used after infilling missings in vehicle field will drop all missings in timestamp and totalkwh
df$Vehicle <- df$Vehicle %>% replace_na('Unknown vehicle')

df <- df[complete.cases(df), ]


#assets <- select(assets,-c('Site ID','District','CP1 Asset Number','CP2 Asset Number','Commissioning Date','Commissioning Certificate Date','Additional Notes','Charger Model'))
#assets <- select(assets,-c('Charger type', 'Bay Marking Completed','Total Days to Completion','MPAN Number'))
#assets <- select(assets,-c('CP3 Asset Number','Signage Completed'))

#assets_reduced<-select(assets_reduced,-c('Serial','Make','Charge Point ID'))
#assets_reduced <-unique(assets_reduced)

## Step 5. Drop outliers and erroneous values####

### drop observations that have a charge duration of less than 1 minute and more than 1440 minutes
### drop observations with illogical kWh consumption values (based on plotting histograms)
### drop variables where minimum duration exceeds charge duration

df <- subset(df, charge_duration <=1440 & charge_duration >5)
df <- subset(df, `Total kWh` <=100 & `Total kWh` >0.5)
df <- subset(df, charge_duration - minimum_plugin_time >0)

## Step 6. Generate summary statistics on the site level####

df0<-df %>% count(Site, sort = TRUE)
df1<-df%>% group_by(Site)%>% summarise(chargeduration_mean=mean(charge_duration)) 
df2<-df%>% group_by(Site)%>% summarise(chargeduration_median=median(charge_duration))
df3<-df%>% group_by(Site)%>% summarise(kwh_mean=mean(`Total kWh`))
df4<-df%>% group_by(Site)%>% summarise(kwh_median=median(`Total kWh`))
df5<-df%>% group_by(Site)%>% summarise(kwh_total=sum(`Total kWh`))
df6 <-group_by(df, Site) %>% summarise(first_event = min(start_time_stamp))
df7 <-group_by(df, Site) %>% summarise(final_event = max(start_time_stamp))
df8 <-group_by(df, Site) %>% summarise(median_min_plugin_time=median(minimum_plugin_time))
df9 <-group_by(df, Site) %>% summarise(median_idle_time=median(idle_plugin_time))



df_list<-list(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9)

#df_list <- lapply(paste0("df",0:7))
site<-df_list%>%reduce(full_join,by='Site')

rm(df_list)
rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9)

## Step 7. Export data ####
write_csv(df,"C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM cleaned data.csv")
write_csv(site,"C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Site data.csv")

## Step 8. Summary statistics and visualisations ####

df$start_time_stamp_new <- as.Date(df$start_time_stamp)
date<-aggregate(df$`Total kWh`, by=list(df$start_time_stamp_new), sum)

duration <-aggregate(df$charge_duration, by=list(df$start_time_stamp_new),median)
plugin_time <-aggregate(df$minimum_plugin_time,by=list(df$start_time_stamp_new),median)

day_kwh <- aggregate(df$`Total kWh`, by=list(df$start_time_stamp_new),median)

ggplot(date, aes(x=Group.1,y=x)) + geom_line() 

ggplot(duration, aes(x=Group.1,y=x)) + geom_line() 

ggplot(plugin_time,aes(x=Group.1,y=x)) + geom_line()

ggplot(day_kwh, aes(x=Group.1,y=x)) + geom_line() 


#site level statistics
ggplot(site,aes(x=chargeduration_median)) + geom_histogram(binwidth=5)  +xlim(0,500)
ggplot(site,aes(x=median_min_plugin_time)) + geom_histogram(binwidth=5)  +xlim(0,200)


ggplot(site,aes(x=kwh_median))+ geom_histogram(binwidth=1)  +xlim(0,50)
ggplot(site,aes(x=n))+ geom_histogram(binwidth=40)  +xlim(0,4000)



#charge event level statistics
ggplot(df, aes(x=charge_duration)) + geom_histogram(binwidth=1) +xlim(0,500)
ggplot(df, aes(x=minimum_plugin_time)) + geom_histogram(binwidth=1) +xlim(0,400)
ggplot(df, aes(x=idle_plugin_time)) + geom_histogram(binwidth=1) +xlim(0,400)
ggplot(df, aes(x=`Total kWh`)) + geom_histogram(binwidth=0.1)





                                     