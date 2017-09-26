# Intro ---------------------------------------------------------------
# Exploratory analysis of data provided by https://mkuran.pl/feed/.

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(rebus)

# Stops --------------------------------------------------------
stops <- read_csv("data/ztm-latest/stops.txt") 
summary(stops)

# Stop timetables ----------------------------------------------------
stop_times <- read_csv("data/ztm-latest/stop_times.txt")

# Routes ------------------------------------------------------------------
routes <- read_csv("data/ztm-latest/routes.txt")
summary(routes)

# Trips ------------------------------------------------------------------
trips <- read_csv("data/ztm-latest/trips.txt")

# Joins -------------------------------------------------------------------
ztm_full <- left_join(stop_times, stops)
ztm_full2 <- left_join(ztm_full, trips)

ztm_full3 <- ztm_full2 %>% 
        mutate(line = str_extract(trip_id, pattern = START %R% one_or_more(DIGIT))) %>% 
        filter(service_id == 'DP',
               line %in% c(3, 11, 13))
        
longest_routes <- ztm_full3 %>% count(line, trip_id, trip_headsign) %>% group_by(line, trip_headsign) %>% filter(n == max(n)) %>% ungroup()

ztm_full4 <- ztm_full3 %>% 
        filter(trip_id %in% longest_routes$trip_id) %>% 
        distinct(line, stop_code, stop_name, stop_sequence, trip_headsign, stop_lat, stop_lon) %>% 
        nest(-line, -trip_headsign)

ztm_full4$data[[4]] <- ztm_full4$data[[4]][-40,]

stops <- ztm_full4

save(stops, file = './data/stops.RData')

