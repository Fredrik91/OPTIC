
##
#### TFGM occupancy data generation

## Loading library
rm(list = ls())

library(tidyverse)
library(lubridate)
library(utilities)
library(splitstackshape)
library(neatRanges)

### step 1. Generate time series for 2018

start <- as.POSIXct("2018-01-01")
interval <- 60

end <- start + as.difftime(365, units="days")

timestamps<- as.data.frame(seq(from=start, by=interval*1, to=end))

timestamps <- rename(timestamps, time_stamp = `seq(from = start, by = interval * 1, to = end)`)

### step 2. Load in TFGM dataset
df = read_csv("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/TFGM cleaned data.csv")

df <- tibble::rowid_to_column(df, "charge_id")
### step 3. Generate ID by site, cpid, connector.In total 306 ID's are generated. 

df<- df %>% group_by(Site,`CP ID`,Connector)%>% dplyr::mutate(ID=cur_group_id())

df_1 <- subset(df, ID==1)
df_1 <- select(df_1,c("Total kWh","start_time_stamp","end_time_stamp","charge_id"))
#expand by 2 and then generate timestamp for start and end. This can subsequently be merged with timestamp dataframe
df_1 <- expandRows(df_1, count = 2, count.is.col=FALSE)
df_1$row_odd <-seq_len(nrow(df_1)) %% 2
df_1$time_stamp <-df_1$start_time_stamp
df_1$time_stamp <-if_else(df_1$row_odd == 0,df_1$start_time_stamp,df_1$end_time_stamp) 
#using fill for row_odd essentially creates a dummy variable for when the connector is occupied or not.
#After this, timestamps without charging can be dropped. 
df_1 <-df_1 %>%
  mutate(time_stamp = ymd_hms(time_stamp)) %>%
  complete(time_stamp = seq(floor_date(min(time_stamp), "minute"), max(time_stamp), 1)) %>%
  fill(row_odd,  .direction = "up")
df_1$time_stamp <- round_date(df_1$time_stamp,unit="1 minute")
df_1<-subset(df_1, row_odd==1)
df_1 <- unique(df_1)
df_1 <-ungroup(df_1)
df_1<-select(df_1,c("row_odd","time_stamp"))

# merge with time stamps 2018
df_1 <- full_join(df_1, timestamps, by="time_stamp")
df_1$row_odd <- df_1$row_odd %>% replace_na(0)


ggplot(data=df_1, aes(x=time_stamp,y=row_odd)) + geom_line()




## perhaps after this, append by timestamp to obtain total occupancy on the network. 