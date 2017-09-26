library(dplyr)
library(stringr)
library(rebus)
# library(rvest)
# library(jsonlite)
# library(httr)
# library(ggplot2)
# 
# linie <- 3
# token2 <- "1191ddd185cb84aaa1ae9b2f0b2145b57ef3e438"
# 
# res <- GET(url = paste0("https://vavel.mini.pw.edu.pl/api/vehicles/v1/full/?line=", linie),
#            add_headers(Authorization = paste("Token", token2)))
# 
# x <- jsonlite::fromJSON(as.character(res))

# returns stop code for selected stop
get_stop_code <- function(line_no, direction, stp_nme){
        stops %>% filter(line == line_no,
                         trip_headsign == direction) %>% 
                .$data %>% 
                .[[1]] %>% 
                filter(stop_name == stp_nme) %>% 
                .$stop_code %/% 100
}


# returns nearest train
get_nearest_tram <- function(line_no, start, direction, data) {
        
        ##print(line_no)
        #print(start)
        #print(direction)
        # print(data)
        if (start != "Wybierz" & direction != "Wybierz") {
        
        start_code <- get_stop_code(line_no, direction, start)
        
        # check what tram heads to selected stop
        nearest_tram <- data %>% 
                mutate(nextStopCode = str_extract(nextStop, pattern = START %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT)) %>% 
                filter(line == line_no, 
                       nextStopCode == start_code,
                       courseDirection == direction) %>% 
                group_by(line) %>% 
                filter(nextStopDistance == min(nextStopDistance)) %>% 
                ungroup() %>% 
                select(brigade, lon, lat, timetableStatus)
        
        if(nrow(nearest_tram) == 0){
                prev_stop <- get_previous_stop(direction, line_no, get_stop_sequence_no(direction, line_no, start_code))
                if (!identical(prev_stop, character(0))) {
                        nearest_tram <- get_nearest_tram(line_no, prev_stop, direction, data)
                }
        }
        #print(nearest_tram)
        nearest_tram
        }
}


# returns stop number in the sequence
get_stop_sequence_no <- function(direction, line_no, stp_cde){
        # returns the number of the stop in the sequence
        stops %>% filter(line == line_no,
                         trip_headsign == direction) %>% 
                .$data %>% 
                .[[1]] %>% 
                mutate(stop_code_short = stop_code %/% 100) %>% 
                filter(stop_code_short == stp_cde) %>% 
                .$stop_sequence %>% 
                .[1]
}

get_previous_stop <- function(direction, line_no, stp_seq){
        # returns the name for the previous stop
        stops %>% filter(line == line_no,
                         trip_headsign == direction) %>% 
                .$data %>% 
                .[[1]] %>% 
                filter(stop_sequence == stp_seq - 1) %>% 
                .$stop_name
                
}

# get_nearest_tram('3', 'Wybierz','Wybierz', dataTram)
# 
# get_nearest_tram('3', 'Gocławek 05','Annopol', dataTram) %>% mutate(rodzaj = "tram")
# line_no <- 3
# direction <- 'Annopol'
# start <- 'Gocławek 05'
# 
# start_code <- get_stop_code(line_no, direction, start)

