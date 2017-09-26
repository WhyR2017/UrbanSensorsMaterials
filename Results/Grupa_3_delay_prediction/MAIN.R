
# this script deals only with one day - you could use same processing and combine dataframes 
#before fitting the model to get results as presented
setwd('D:/R_projects/Why_R/Hackaton/Data/Trams')
library(tidyverse)
library(lubridate)
trams_09_18 <- read.csv2('2017_09_18.csv', stringsAsFactors = F, header = F)
trams_09_19 <- read.csv2('2017_09_19.csv', stringsAsFactors = F, header = F) 
trams_09_21 <- read.csv2('2017_09_21.csv', stringsAsFactors = F, header = F) 

colnames(trams_09_18) <- c( "versionID", #String
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
                            "speed",'blank')
trams_09_18$time
trams_09_18_p <- 
  trams_09_18  %>% mutate(delay = as.numeric(delay),
                          delay_hours = delay/60,
                          time_hour = substr(time,12,13),
                          time_minute = substr(time, 15,16) %>% as.numeric,
                          time_second = substr(time, 18,20),
                          line = as.character(line),
                          time_16 = substr(time, 1, 15),
                          time_PB = ymd_hm(paste0(time_16,"0")),
                          time_numeric = time_hour %>% as.numeric,
                          speed = as.numeric(speed),
                          nextStopDistance = as.numeric(nextStopDistance),
                          nextStopTimetableVisitTime_15 = substr(time, 1, 16),
                          nextStopTimetableVisitTime_15_PB = ymd_hm(paste0(nextStopTimetableVisitTime_15))
  ) %>% filter(line %in% c('10','17')) %>% arrange(line, brigade, time) %>%
  filter(time_numeric > 6)

drop_not_needed_rows <- trams_09_18_p %>% group_by(line, brigade, courseIdentifier, delayAtStop) %>% 
  mutate(count = 1:n(), count_max = max(count)) %>% filter(count == count_max) %>%
  #filter(nextStop != lag(nextStop)) %>%
   group_by(line, brigade, courseIdentifier) %>%
  mutate(change_in_delay = delay - lag(delay),
         change_in_delay3 = delay - lag(delay,3),
         lagged_delay = lag(delay,1),
         lagged_delay2 = lag(delay,2),
         previous_change_in_delay = lag(change_in_delay),
         previous_change_in_delay3 = lag(change_in_delay3,3)) %>% data.frame %>%
  group_by(courseIdentifier, courseDirection) %>%
  mutate(total_distance = sum(nextStopDistance),
         left_distance = cumsum(nextStopDistance),
         A = ifelse(is.na(lag(left_distance)),0,lag(left_distance)),
         left_distance_c = total_distance - A)
str(drop_not_needed_rows)
drop_not_needed_rows %>% select(nextStopDistance, total_distance, left_distance,A,left_distance_c,courseDirection,courseIdentifier, line) %>% View

drop_not_needed_rows %>% data.frame %>% filter(line == 10 ) %>% select(total_distance) %>% as.matrix %>% hist




#ggplot(trams_09_21_p %>% filter(line %in% c('10','17')), aes(x = time_hour, y = delay_hours)) + geom_point() + facet_wrap(~line)

#ggplot(trams_09_21_p %>% filter(line %in% c('10')), aes(delay_hours)) + geom_density() + facet_wrap(~time_hour)


drop_not_needed_rows %>% filter(line %in% c('10'), total_distance > 16000) %>% ggplot(aes(x = time_PB, y = delay, group = NULL)) + geom_line() +
  facet_wrap(~brigade)

drop_not_needed_rows %>% filter(line %in% c('10'), total_distance > 16000, courseIdentifier == '10_1_2_0731') %>% 
  ggplot(aes(x = left_distance_c, y = delay, group = NULL)) + geom_line() +
  facet_wrap(~brigade)


lista <- drop_not_needed_rows %>%  filter(line %in% c('10')) %>% data.frame %>% select(courseIdentifier) %>% unique %>% as.matrix

drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[3:11]) %>% 
  ggplot(aes(x = nextStopTimetableVisitTime_15_PB, y = change_in_delay, colour = courseIdentifier)) + geom_line() + geom_point()+
  facet_wrap(~courseIdentifier, scale = 'free_x')


przystanki <- drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[3]) %>%
  data.frame %>% select(delayAtStop) %>% as.matrix



drop_not_needed_rows$delayAtStop <- factor(drop_not_needed_rows$delayAtStop, levels = przystanki)

drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[3]) %>% 
  ggplot(aes(x = delayAtStop, y = change_in_delay, colour = courseIdentifier)) + geom_line() + geom_point()+
  facet_wrap(~courseIdentifier, scale = 'free_x') +theme(axis.text.x = element_text(angle = 90, hjust = 1))


peak <- data.frame(time_numeric = c(0:23),peak = c(rep(0,7), rep(0,3), rep(0,4), rep(1,4), rep(0,6)))
peak$peak <- as.numeric(peak$peak)

joined <- drop_not_needed_rows %>% left_join(peak, by = 'time_numeric') %>%
  mutate(hour_dropped = ifelse(peak == 1, time_hour, '0'))
model <- lm(change_in_delay ~ hour_dropped+ peak*delayAtStop + previous_change_in_delay +lagged_delay,data = joined %>% filter(line == 10)) 
model %>% summary


