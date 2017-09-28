library(tidyverse)
load("./102_111_01_09.rda")
load("./zones_01_09_102_111.rda")
load("../offline/polygon_unique_zones.rda")
orange <- read.csv("../offline/measurements.csv", stringsAsFactors = FALSE)

orange$time16 = substr(orange$datetime, 1, 15)
orange_4 <- orange %>% filter(datetime > "2017-09-04 00:00:00", datetime < "2017-09-05 23:59:59")
orange_grp = orange_4 %>% group_by(zoneid, time16) %>% summarise(mean_intensity=mean(intensity))

bus_all <- one_day_short
bus_all$zoneid <- zones
bus_all$time16 <- substr(bus_all$time, 1, 15)
library(lubridate)
bus_all$time16 <- as_datetime(bus_all$time16) + days(3)
bus_all$delay_num = as.numeric(bus_all$delay)
bus_all <- bus_all %>% 
    filter(zoneid > 0)
bus_all <- bus_all %>% mutate(pos_delay = ifelse(delay_num > 0, delay_num, 0))
delay_zone <- bus_all %>% group_by(time16, zoneid) %>% summarise(mean_delay=mean(pos_delay))
orange_grp$zoneid <- as.double(orange_grp$zoneid)
orange_grp$time16 <- as_datetime(orange_grp$time16)

joined = delay_zone %>% left_join(orange_grp, by = c("zoneid" = "zoneid", "time16" = "time16"))
joined <- joined %>% filter(!is.na(mean_intensity))


# ----------
bus_and_orange <- joined %>% left_join(orange, by = c("zoneid" = "zoneid", "time16" = "time16"))

joined %>% 
    ggplot(aes(x = mean_delay, y = mean_intensity)) +
    geom_point()

cor(joined$mean_delay, joined$mean_intensity)

joined %>% 
    group_by(zoneid, time16) %>% 
    summarize(mean_del = mean(mean_delay)) %>% 
    ggplot(aes(x = time16)) +
    geom_line(aes(y = mean_del, group = zoneid))

joined %>% 
    group_by(zoneid, time16) %>% 
    summarize(mean_intensity = mean(mean_intensity)) %>% 
    ggplot(aes(x = time16)) +
    geom_line(aes(y = mean_intensity, group = zoneid))

orange_grp %>% ggplot(aes(x = time16, y = mean_intensity, group = zoneid)) +
    geom_line()
    

orange %>% ggplot(aes(x = time16, y = intensity, group = zoneid)) +
    geom_line()

stadion_narodowy <- orange %>% filter(zoneid == 338)
stadion_narodowy %>% ggplot(aes(x = time16, y = intensity, group = 1)) +
    geom_line()
stadion_narodowy_bez_meczu <- stadion_narodowy %>% 
    filter(time16 > as_datetime("2017-09-05 00:0")) 

stadion_narodowy_mecz <- stadion_narodowy %>% 
    filter(date(as_datetime(time16)) == "2017-09-04")
stadion_narodowy_bez_meczu <- stadion_narodowy_bez_meczu %>% 
    mutate(weekday = weekdays(as_datetime(time16))) %>% 
    filter(!(weekday %in% c("sobota", "niedziela")))

stadion_narodowy_bez_meczu_profil <- stadion_narodowy_bez_meczu %>% 
    mutate(hour = hour(time16)) %>% 
    group_by(hour) %>% 
    summarise(mean_int = mean(intensity), sd = sd(intensity))
stadion_narodowy_bez_meczu_profil %>% ggplot(aes(x = hour, y = mean_int, group = 1)) +
    geom_ribbon(aes(ymin = mean_int - 2 * sd, ymax = mean_int + 2 * sd), alpha  = 0.3, fill = "red") + 
    geom_line(color = "red") + 
    geom_line(data = stadion_narodowy_mecz, aes(x = hour(time16), y = intensity), color = "blue") +
    theme_bw()
