library(readr)
library(tidyverse)
measurements <- read_csv("measurements.csv")
load("../offline/one_day.rda")
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

one_day$my_lon <- one_day$lon / 1000000
one_day$my_lat <- one_day$lat / 1000000
zones_dict <- measurements %>% 
    select(zoneid, geom4326) %>% 
    unique()

multipol <- measurements$geom4326[1]

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

point_in_polygon <- function(point_lon, point_lat, pol_x, pol_y) {
    point.in.polygon(point_lon, point_lat, pol_x, pol_y)
}

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

one_day$zoneid <- apply(one_day, 1, function(x) find_zone(x["my_lon"], x["my_lat"]))
