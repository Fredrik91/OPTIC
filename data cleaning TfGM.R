# ################################################################# #
#### TFGM Charge Event data 
# ################################################################# #


#### Data cleaning of charge event data through several steps:

## Step 1 load library
## Step 2 importing data
## Step 3 Generating variables
## Step 4 Dropping missing data, obsolete variables and non public charging stations
## Step 5 Drop outliers and erroneous values
## Step 6 Generate summary statistics on the site level
## Step 7 Generate summary statistics on the userid level
## Step 8 Exporting data
## Step 9 Summary statistics and visualisations

## Step 1 load library ####
rm(list = ls())

library(tidyverse)
library(lubridate)
## Step 2 importing data #####
# please note, the current dataset used does not contain userid's. 
#Update. TFGM 2018 2019 data is available which includes a userid. Please note, the vehicle field is only available for 2018 as this could not 
# be made reliable for 2019. 

#df = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Use.csv")
df = read_csv("C:/Users/cvfm9/OneDrive - Loughborough University/OPTIC/GMEV/OPTIC/TFGM 2018 2019 userid.csv")


#assets = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Asset Inventory.csv")
#assets_reduced = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM Assets_reduced.csv")

### FOr usage into QGIS, read in file with keys for lat long. This can subsequently be matched with the QGIS summary statistic files
df_location = read_csv("C:/Users/cvfm9/OneDrive - Loughborough University/OPTIC/GMEV/OPTIC/TFGM Assets_reduced.csv")
df_location$site= df_location$Site
df_location <-select(df_location, c('site','Lat','Lon'))
df_location <-unique(df_location)


## Step 3. Generate variables ####

# 1. generate timestamp from string variables. 
# 2. Generate charge duration in minutes. 
# 3. Generate power output variable 
# 4. Generate minimum plugin time
# 5. Generate idle plugin time
# 6. Generate end timestamp for charging time

df$start_time_stamp <- as.POSIXct(paste(df$startdate, df$starttime), format="%d/%m/%Y %H:%M:%S")
df$end_time_stamp <- as.POSIXct(paste(df$enddate, df$endtime), format="%d/%m/%Y %H:%M:%S")
df <- mutate(df,charge_duration = end_time_stamp - start_time_stamp)
df$charge_duration <- as.numeric(df$charge_duration, units = "hours")
df$charge_duration <- df$charge_duration*60

df$power_output = 0
df$power_output[str_detect(df$model,'3.6kW')] <-3.6
df$power_output[str_detect(df$model,'7kW')] <-7
df$power_output[str_detect(df$model,'Rapid')] <-50

# The variable minimum plugin time is based on the assumption that each charger provides power at its maximum power rating
# Power rating (see df$power_output) is based on the model specifications. 
# This assumption very likely overstates the true minimum charging time. An option is to use 80% or 90% of the assumed minimum time.
# this may then be more realistic
df <- mutate(df,minimum_plugin_time =df$totalkwh/ df$power_output)
df$minimum_plugin_time<- df$minimum_plugin_time*60
df <-mutate(df,idle_plugin_time=df$charge_duration - df$minimum_plugin_time)

#somehow the two functions below do not work
#df$end_time_stamp_charge <- df$start_time_stamp + minutes(df$minimum_plugin_time)
#df$end_time_stamp_charge <- round_date(df$end_time_stamp_charge,unit="1 minute")


## Step 4. Dropping missing data and obsolete variables####
#### Drop missing variables. Missings are present in timestamp, total kwh and vehicle. Missings in vehicle are not relevant.  
# missings in timestamps. Complete cases function used after infilling missings in vehicle field will drop all missings in timestamp and totalkwh
# Update: Users can record the vehicle type when they apply for an account, however the field 'vehicle' is not mandatory. 
# Hence 20% in this field is missing. 
df$vehicle <- df$vehicle %>% replace_na('Unknown vehicle')

df <- df[complete.cases(df), ]

#filter out non public sites

df <- filter(df, site != "** Chargemaster Test **")
df <- filter(df, site != "***TEST SITE*** Charge Your Car HQ")
df <- filter(df, site != "Ashton Bus Station (TfGM access only)")
df <- filter(df, site != "Bolton Interchange (TfGM access only)")
df <- filter(df, site != "Hyde Bus Station (TfGM access only)")
df <- filter(df, site != "Station Approach (TfGM access only)")
df <- filter(df, site != "TfGM HQ (TfGM access only)")
df <- filter(df, site != "Stockport Bus Station (TfGM access only)")
df <- filter(df, site != "Middleton Bus Station (TfGM access only)")
df <- filter(df, site != "Oldham West Bus Station (TfGM access only)")
df <- filter(df, site != "Shudehill Interchange Bus Station (TfGM access only)")

df <- filter(df, site != "Wigan Council Depot")
df <- filter(df, site != "Rochdale Bus Station")



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
df <- subset(df, totalkwh <=100 & totalkwh >0.5)
df <- subset(df, charge_duration - minimum_plugin_time >0)


## Step 6. Generate summary statistics on the site level####
### Sites can have multiple chargepoints (e.g., P+R such as Trafford Park), thus multiple charge point id's may be available per site. 
### In case 2019 data is added, be aware that less sites are available in 2019 due to rationalisation of the network
### Some sites aren't actually public (e.g. TFGM HQ, Manchester Met Uni)


df0 <-df%>% count(site, sort = TRUE)
df1 <-group_by(df, site) %>% summarise(chargeduration_mean=mean(charge_duration)) 
df2 <-group_by(df, site) %>% summarise(chargeduration_median=median(charge_duration))
df3 <-group_by(df, site) %>% summarise(kwh_mean=mean(totalkwh))
df4 <-group_by(df, site) %>% summarise(kwh_median=median(totalkwh))
df5 <-group_by(df, site) %>% summarise(kwh_total=sum(totalkwh))
df6 <-group_by(df, site) %>% summarise(first_event = min(start_time_stamp))
df7 <-group_by(df, site) %>% summarise(final_event = max(start_time_stamp))
df8 <-group_by(df, site) %>% summarise(median_min_plugin_time=median(minimum_plugin_time))
df9 <-group_by(df, site) %>% summarise(median_idle_time=median(idle_plugin_time))
df10<-group_by(df, site) %>% summarise(unique_users = n_distinct(userid))


df_list<-list(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)

#df_list <- lapply(paste0("df",0:7))
site<-df_list%>%reduce(full_join,by='site')
QGIS_data <-merge(site,df_location,by='site')
#avoid duplicates in site
QGIS_data <- QGIS_data%>% distinct(site, .keep_all= TRUE)


rm(df_list)
rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)


#### Step 7 Generate summary statistics on the user-id level ####

df0<-df%>% count(userid, sort = TRUE)
df1<-df%>% group_by(userid)%>% summarise(chargeduration_mean=mean(charge_duration)) 
df2<-df%>% group_by(userid)%>% summarise(chargeduration_median=median(charge_duration))
df3<-df%>% group_by(userid)%>% summarise(kwh_mean=mean(totalkwh))
df4<-df%>% group_by(userid)%>% summarise(kwh_median=median(totalkwh))
df5<-df%>% group_by(userid)%>% summarise(kwh_total=sum(totalkwh))
df6 <-group_by(df,userid) %>% summarise(first_event = min(start_time_stamp))
df7 <-group_by(df,userid) %>% summarise(final_event = max(start_time_stamp))
df8 <-group_by(df,userid) %>% summarise(median_min_plugin_time=median(minimum_plugin_time))
df9 <-group_by(df,userid) %>% summarise(median_idle_time=median(idle_plugin_time))
df10<-group_by(df,userid) %>% summarise(unique_sites = n_distinct(site))

df_list<-list(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)
userdata<-df_list%>%reduce(full_join,by='userid')

rm(df_list)
rm(df0,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)

## Step 8. Export data ####
write_csv(df,"C:/Users/cvfm9/OneDrive - Loughborough University/OPTIC/GMEV/TFGM cleaned data.csv")
# For use in QGIS
# QGIS file contains contextual information from the UK census, as well as shapefiles such as road network and so on.
# Land use characteristics could be added there to enrich the data. 
write_csv(QGIS_data,"C:/Users/cvfm9/OneDrive - Loughborough University/OPTIC/QGIS/Shapefiles/TFGM Site data.csv")

## Step 9. Summary statistics and visualisations ####

df$start_time_stamp_new <- as.Date(df$start_time_stamp)
date<-aggregate(df$totalkwh, by=list(df$start_time_stamp_new), sum)

duration <-aggregate(df$charge_duration, by=list(df$start_time_stamp_new),median)
plugin_time <-aggregate(df$minimum_plugin_time,by=list(df$start_time_stamp_new),median)

day_kwh <- aggregate(df$totalkwh, by=list(df$start_time_stamp_new),median)
day_kwh_total <- aggregate(df$totalkwh, by=list(df$start_time_stamp_new),sum)

ggplot(date, aes(x=Group.1,y=x)) + geom_line() + labs(x="",y="", title = "Daily charging events")

ggplot(duration, aes(x=Group.1,y=x)) + geom_line() 

ggplot(plugin_time,aes(x=Group.1,y=x)) + geom_line()

ggplot(day_kwh, aes(x=Group.1,y=x)) + geom_line() 

ggplot(day_kwh_total, aes(x=Group.1,y=x)) + geom_line() + labs(x="",y="kWh", title = "Daily energy consumption (kWh)")

#site level statistics
ggplot(site,aes(x=chargeduration_median)) + geom_histogram(binwidth=5)  +xlim(0,500)
ggplot(site,aes(x=median_min_plugin_time)) + geom_histogram(binwidth=5)  +xlim(0,200)


ggplot(site,aes(x=kwh_median))+ geom_histogram(binwidth=1)  +xlim(0,50)
ggplot(site,aes(x=n))+ geom_histogram(binwidth=40)  +xlim(0,4000)

ggplot(site,aes(x=unique_users)) + geom_histogram(binwidth=1) +xlim(0,1000)

#charge event level statistics
ggplot(df, aes(x=charge_duration)) + geom_histogram(binwidth=1) +xlim(0,500)
ggplot(df, aes(x=minimum_plugin_time)) + geom_histogram(binwidth=1) +xlim(0,400)
ggplot(df, aes(x=idle_plugin_time)) + geom_histogram(binwidth=1) +xlim(0,400)
ggplot(df, aes(x=totalkwh)) + geom_histogram(binwidth=0.1)

ggplot(df, aes(x=`power_output`)) + geom_histogram(binwidth=1)

### user id level statistics
ggplot(userdata, aes(x=n))+geom_histogram(binwidth=1) +xlim(0,100)
ggplot(userdata,aes(x=unique_sites)) +geom_histogram(binwidth=1) +xlim(0,30)

ggplot(userdata,aes(x=chargeduration_median)) + geom_histogram(binwidth=5)  +xlim(0,500)
ggplot(userdata,aes(x=median_min_plugin_time)) + geom_histogram(binwidth=5)  +xlim(0,200)
ggplot(userdata,aes(x=kwh_median))+ geom_histogram(binwidth=1)  +xlim(0,50)





                                     