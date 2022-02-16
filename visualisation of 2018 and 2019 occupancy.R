rm(list = ls())

library(tidyverse)
library(lubridate)


df = read_csv("C:/Users/cvfm9/OneDrive - Loughborough University/OPTIC/2018_2019_timestamps_GMEV.csv")

ggplot(data=df, aes(x=stata_timestamp,y=occupied)) + geom_line()

