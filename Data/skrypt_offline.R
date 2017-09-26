setwd("../offline")

# load important libraries
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")

# read all files
files <- list.files(pattern = "part")
parts <- lapply(files, read_csv2, col_names = FALSE)

# enrich
one_day <- do.call(rbind, parts)
colnames(one_day) <- c( "versionID", #String
   "line",#String,
   "brigade", #String
   "time",#String,
   "lon",#Double,
   "lat",#Double,
   "rawLon",#Double,
   "rawLat",#Double,
   "status",#String,
   "delay",#String,
   "delayAtStop",#String,
   "plannedLeaveTime",#String,
   "nearestStop",#String,
   "nearestStopDistance",#Double,
   "nearestStopLon",#Double,
   "nearestStopLat",#Double,
   "previousStop",#String,
   "previousStopLon",#Double,
   "previousStopLat",#Double,
   "previousStopDistance",#Double,
   "previousStopArrivalTime",#String,
   "previousStopLeaveTime",#String,
   "nextStop",#String,
   "nextStopLon",#Double,
   "nextStopLat",#Double,
   "nextStopDistance",#Double,
   "nextStopTimetableVisitTime",#String,
   "courseIdentifier",#String,
   "courseDirection",#String,
   "timetableIdentifier",#String,
   "timetableStatus",#String,
   "receivedTime",#String,
   "processingFinishedTime",#String
   "onWayToDepot" ,#String
   "overlapsWithNextBrigade",#String
   "atStop",#String
   "overlapsWithNextBrigadeStopLineBrigade",#String
   "speed")
   
# clean up the data

one_day$time16 <- substr(one_day$time, 1, 15)
one_day$delay <- as.numeric(as.character(one_day$delay))

# show quartiles of delays
quant <- one_day[,-39]  %>%
  filter(time16 > "2017-09-01 06:1",
         time16 < "2017-09-01 23:5") %>%
  mutate(delay=delay/60) %>%
  group_by(time16) %>%
  summarise(q05 = quantile(delay, 0.5, na.rm=TRUE),
            q25 = quantile(delay, 0.25, na.rm=TRUE),
            q50 = quantile(delay, 0.50, na.rm=TRUE),
            q75 = quantile(delay, 0.75, na.rm=TRUE),
            q95 = quantile(delay, 0.95, na.rm=TRUE)) %>%
  mutate(time = ymd_hm(paste0(time16,"0")))

# plot something
ggplot(quant, aes(time, q50)) +
  geom_linerange(aes(ymin=q25, ymax=q75), color="blue")+
  geom_point(color="red") 

