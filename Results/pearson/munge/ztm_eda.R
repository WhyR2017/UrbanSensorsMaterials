# Intro ---------------------------------------------------------------
# Exploratory analysis of data provided by https://mkuran.pl/feed/.

library(tidyverse)
library(readr)
library(lubridate)

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