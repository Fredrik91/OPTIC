# ################################################################# #
#### Carbon Intensity Data 
# ################################################################# #
rm(list = ls())

library(tidyverse)

energy_18 <- c(9:12) %>% paste0("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/Carbon intensity data/Carbon_Intensity_Data_2018_", .,".csv") %>% map(read_csv)
energy_18 <-do.call(rbind.data.frame, energy_18)
energy_19 <- c(1:2) %>% paste0("C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/Carbon intensity data/Carbon_Intensity_Data_2019_", .,".csv") %>% map(read_csv)
energy_19 <-do.call(rbind.data.frame, energy_19)

energy<- rbind(energy_18,energy_19)
energy <-select(energy, c('Datetime (UTC)','North West England'))

## convert datetime variable into date format using lubridate package

str(energy)

#energy$`Datetime (UTC)`<- as_date(energy$`Datetime (UTC)`)

energy<-unique(energy)

energy<- energy[order(as.Date(energy$`Datetime (UTC)`, format="%d/%m/%Y")),]

energy$`Datetime (UTC)` <- as.POSIXct(energy$`Datetime (UTC)`, format="%H:%M:%S")
ggplot(data=energy, aes(x=`Datetime (UTC)`,y=`North West England`)) + geom_line()


## write to csv
write_csv(energy,"C:/Users/cvfm9/Loughborough University/Craig Morton - VPACH/Phase 2/Analysis/TripAttraction/Data/Greater Manchester EV data/carbon intensity data.csv")




