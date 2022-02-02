
##
#### TFGM occupancy data generation

### The aim is to generate occupancy profiles for each connector on the TFGM network, and subsequently compile this.
### Compiling this yields occupancy profiles across the network (or selected subsets of the network). Statistics can also be derived
### At the end, the whole timeseries can be aggregated to weekdays and weekends to get a general sense of charging profiles. 


#### step 1. Loading library ####
rm(list = ls())

library(tidyverse)
library(lubridate)
library(splitstackshape)

### step 2. Generate time series for 2018 ####

start <- as.POSIXct("2018-01-01")
interval <- 60
#730 days for two years
end <- start + as.difftime(730, units="days")

timestamps<- as.data.frame(seq(from=start, by=interval*1, to=end))

timestamps <- rename(timestamps, time_stamp = `seq(from = start, by = interval * 1, to = end)`)



### step 3. Load in cleaned TFGM dataset, with events in 2018 and 2019 ####
#please note, this dataset is generated through the coding file 'data cleaning TfGM.R'.  
df = read_csv("C:/Users/cvfm9/OneDrive - Loughborough University/OPTIC/GMEV/TFGM cleaned data.csv")


df <- tibble::rowid_to_column(df, "charge_id")
### step 4. Generate ID by site, cpid, connector.In total 306 ID's are generated. #### 
### for each connector, charge profiles should be generated and then linked back together to form time series plots on occupancy, or to enable occupancy calculations

df<- df %>% group_by(site,cpid,connector)%>% dplyr::mutate(ID=cur_group_id())

#the idea is to program the code below such that the connector id is used to generate a file with all the timestamps and an indicator value
# indicating whether the connector is occupied for that timestamp or not. 
#subsequently, all these files can be joined, and then the occupancy indicators can be added up, to profile total occupancy on the network for any
#given timestamp. After this, occupancy profiles for each weekday and weekend day can be derived. 

df_1 <- subset(df, ID==111)
df_1 <- select(df_1,c("totalkwh","start_time_stamp","end_time_stamp","charge_id"))
#expand by 2 and then generate timestamp for start and end. This can subsequently be merged with timestamp dataframe
df_1 <- expandRows(df_1, count = 2, count.is.col=FALSE)
df_1$row_odd <-seq_len(nrow(df_1)) %% 2
df_1$time_stamp <-df_1$start_time_stamp
df_1$time_stamp <-if_else(df_1$row_odd == 0,df_1$start_time_stamp,df_1$end_time_stamp) 


#using fill for row_odd essentially creates a dummy variable for when the connector is occupied or not.
#After this, timestamps without charging can be dropped. 

# problem here is the use of 'ymd_hms' format for the time stamp. It is better to have ymd_hm format, however unclear how to achieve that
## 60 instead of 1 after 'max(time_stamp) does the trick

df_1 <-df_1 %>%
  mutate(time_stamp = ymd_hms(time_stamp)) %>%
  complete(time_stamp = seq(floor_date(min(time_stamp),"minute"), max(time_stamp),60)) %>%
  fill(row_odd,  .direction = "up")

df_1<-subset(df_1, row_odd==1)

df_1 <-ungroup(df_1)
df_1<-select(df_1,c("row_odd","time_stamp"))

# merge with time stamps 2018
df_1 <- full_join(df_1, timestamps, by="time_stamp")
df_1$row_odd <- df_1$row_odd %>% replace_na(0)

### This then plots the occupancy for one site. 
ggplot(data=df_1, aes(x=time_stamp,y=row_odd)) + geom_line()


## Now this needs to be automated for all sites, in such a way that total occupancy on the network can be derived. 
## In STATA, codes for this are fully automated and available. 

## perhaps after this, append by timestamp to obtain total occupancy on the network. 