# WCZYTANIE BUSOW
library("readr")
library("dplyr")


setwd("offline")
# jak chcesz wczytac wszystkie dni, to zostaw po prostu path="VAVEL")
bus_files <- list.files(path="VAVEL",
           recursive=T,
           pattern="part"
           ,full.names=T)
bus_parts <- lapply(bus_files, read_csv2, col_names = FALSE)

# enrich
bus_all <- do.call(rbind, bus_parts)
rm(bus_parts)

colnames(bus_all) <- c( "versionID", #String
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
bus_all$time16 <- substr(bus_all$time, 1, 15)
bus_all$delay_num = as.numeric(bus_all$delay)
# any(is.na(bus_all$delay_num))

# READ ORANGE DATA
orange <- read.csv("measurements.csv", stringsAsFactors = FALSE)
orange$time16 = substr(orange$datetime, 1, 15)
orange_4 <- orange %>% filter(datetime > "2017-09-04 00:00:00", datetime < "2017-09-05 23:59:59")
orange_grp = orange_4 %>% group_by(zoneid, time16) %>% summarise(mean_intensity=mean(intensity))

# pogrupowanie po strefie (orange), godzinie
# todo add zone
bus_all$my_lon <- bus_all$lon / 1000000
bus_all$my_lat <- bus_all$lat / 1000000
zones_dict <- orange %>% 
  select(zoneid, geom4326) %>% 
  unique()

multipol <- orange$geom4326[1]

polygon <- apply(zones_dict, 1, function(x) {
  multipol <- x[2]
  tmp <- gsub(multipol, pattern = "[^0-9 \\.,]", replacement = "")
  tmp2 <- strsplit(tmp, split=",")
  tmp3 <- lapply(tmp2, strsplit, split = " ")
  tmp4 <- lapply(tmp3, function(x) {
    tt <- sapply(x, function(y) {
      as.numeric(y)
    })
    tt <- cbind(tt, tt[,1])
    tt
  })
  return(tmp4)
}
)

#load("polygon_unique_zones.rda")

#polygon <- polygon_unique_zones 

#point_in_polygon <- function(point_lon, point_lat, pol_x, pol_y) {
#  point.in.polygon(point_lon, point_lat, pol_x, pol_y)
#}

library(rgdal)

find_zone <- function(point_lon, point_lat) {
  in_zone <- sapply(polygon, function(x) {
    point_in_polygon(point_lon, point_lat, x[[1]][1, ], x[[1]][2, ])
  }) 
  if (sum(in_zone) == 0) {
    return(-1)
  } 
  return(which.max(in_zone))
}

#wanted_buses <- bus_all$line %in% c("102", "111")
wanted_buses <- bus_all$line %in% c("162");
bus_all_short <- bus_all[wanted_buses, ]

bus_all_short$zoneid <- apply(bus_all_short, 1, function(x) find_zone(x["my_lon"], x["my_lat"]))

bus_all_short <- bus_all_short[bus_all_short$delay > 0, ]
delay_zone <- bus_all_short %>% group_by(time16, zoneid) %>% summarise(mean_deley=mean(delay_num))
                                   
# inner data
joined = delay_zone %>% inner_join(bus_all_short, by = c("zoneid" = "zoneid", "time16" = "time16"))
joined <- joined[joined$zoneid > 0,]
joined$hour <- hour(joined$time16)
joined$date <- date(joined$time16)

orange$hour <- hour(orange$time16)
orange$date <- date(orange$time16)

bus_and_orange <- joined %>% left_join(orange, by = c("zoneid" = "zoneid", "date" = "date", "hour" = "hour"))

# ==========================

# TODO:

(cor(bus_and_orange$intensity, bus_and_orange$mean_deley))

#ggplot(daneBemowo, aes(ymd_hms(datetime), intensity*10000, fill=factor(zoneid))) +
#  geom_col() + facet_grid(zoneid~., scales = "free_y") +
#  theme_light()
#pacggplot2(joined)


#joined$cor = abs(cor(joined$mean_intensity, joined$mean_deley))

library(lubridate)
bus_and_orange$hour = hour(bus_and_orange$time)
bus_and_orange$day = weekdays(bus_and_orange$time)
bus_and_orange %>% 
  group_by(zoneid, hour, day) %>% 
    summarise(mean_delay = mean(delay))
group <- paste0(bus_and_orange$zoneid, bus_and_orange$hour, bus_and_orange$day)
bus_and_orange$group <- group
aggregate(delay ~group, data = bus_and_orange, mean)

ggplot(aes(x = time, y = delay, group = zoneid)) + geom_line()

stadion <- bus_and_orange[bus_and_orange$zoneid == "338", ]
stadion %>% ggplot(aes(x = intensity, y = mean_deley)) + geom_point()
