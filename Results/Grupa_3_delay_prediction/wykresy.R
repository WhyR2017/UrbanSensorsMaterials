
setwd('D:/R_projects/Why_R/Hackaton')
# opoznienia
png(file = 'linia_10_delay.png', width = 1920*2, height = 1080*2,res = 165*2)
drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[3:11]) %>% 
  ggplot(aes(x = nextStopTimetableVisitTime_15_PB, y = delay, colour = courseIdentifier)) + geom_line() + geom_point()+
  facet_wrap(~courseIdentifier, scale = 'free_x')
dev.off()

png(file = 'linia_10_delay_part_2.png', width = 1920*2, height = 1080*2,res = 165*2)
drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[40:51]) %>% 
  ggplot(aes(x = nextStopTimetableVisitTime_15_PB, y = delay, colour = courseIdentifier)) + geom_line() + geom_point()+
  facet_wrap(~courseIdentifier, scale = 'free_x')
dev.off()

# zmiana w opoznieniu
png(file = 'linia_10_change_in_delay_part.png', width = 1920*2, height = 1080*2,res = 165*2)
drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[3:11]) %>% 
  ggplot(aes(x = nextStopTimetableVisitTime_15_PB, y = change_in_delay, colour = courseIdentifier)) + geom_line() + geom_point()+
  facet_wrap(~courseIdentifier, scale = 'free_x') + ylim(-100,300)
dev.off()

drop_not_needed_rows %>% filter(line %in% c('10'), total_distance > 16000, courseIdentifier == '10_1_2_0731') %>% 
  ggplot(aes(x = left_distance_c, y = delay, group = NULL)) + geom_line() +
  facet_wrap(~brigade)



# opoznienie / przystanki
drop_not_needed_rows %>% filter(line %in% c('10'), courseIdentifier %in% lista[3]) %>% 
  ggplot(aes(x = delayAtStop, y = change_in_delay, colour = courseIdentifier)) + geom_line() + geom_point()+
  facet_wrap(~courseIdentifier, scale = 'free_x') +theme(axis.text.x = element_text(angle = 90, hjust = 1))
