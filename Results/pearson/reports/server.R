library(shiny)
library(httr)
library(rvest)
library(jsonlite)
library(dplyr)
library(purrr)

load('../data/stops.RData')
load('../data/all_days_avgs.RData')

# stops <- stops %>% 
#         mutate(trip_headsign = case_when(.$trip_headsign == 'Pl. Narutowicza' ~ 'pl.Narutowicza',
#                                          .$trip_headsign == 'Kawęczyńska - Bazylika' ~ 'Kawęczyńska-Bazylika',
#                                          .$trip_headsign == 'Cm. Wolski' ~ 'Cm.Wolski',
#                                          TRUE ~ .$trip_headsign))

#stops <- filter(stops, !grepl('zajezdni', trip_headsign)) %>% unnest(data)
stopsUnnested <- filter(stops, !grepl('zajezdni', trip_headsign)) %>% unnest(data)
liniePrzystanki <- stopsUnnested %>% mutate(stop_name = stri_sub(stop_name, 1, -4)) %>% count(line,stop_name,stop_lat,stop_lon) %>% filter(n > 1)

source("../munge/get_nearest_tram.R")

find_trip_time = function(line, headsign, from, to, weekday, hour) {
        trip = stopsUnnested %>%
        dplyr::filter(line == line, trip_headsign == headsign) %>% 
        mutate(short_code = as.integer(substring(stop_code, 1, 4)))
        p#rint(trip)
        total = 0
        open = FALSE
        last = NA
        dat = all_days_avgs %>%
                filter(
                        line == line,
                        courseDirection == headsign,
                        time_weekday == weekday,
                        time_hour == hour
                )
        for(stop in trip$short_code){
                if(stop == from){
                        open = TRUE
                        #print(paste0('start: ',stop))
                }
                if(open & !is.na(last)){
                        d = dat %>%
                                filter(
                                        previousStopNumber == last,
                                        nextStopNumber == stop
                                )
                        #print(paste0('ADD:', coalesce(as.numeric(mean(d$mean_stop_interval)),0)))
                        total = total + coalesce(as.numeric(mean(d$mean_stop_interval)),0)
                }
                if(open){
                        last = stop
                }
                if(stop == to){
                        #print(paste0('STOP: ',stop))
                        break;
                }
        }
        return(round(total / 60))
}


shinyServer(function(input, output, session) {
        
        # Reactive returning the data about the start stop
        dataStart <- reactive({
                req(input$poczatek, input$direction, input$linia)
                filter(stopsUnnested, line == input$linia & stop_name == input$poczatek & trip_headsign == input$direction) %>% mutate(
                        lat = stop_lat,
                        lon = stop_lon,
                        rodzaj = "poczatek",
                        color = "#0065a5"
                ) %>% select(lat,lon,rodzaj)
                
        })
        
        # Reactive returning the data about the choosen stop
        dataEnd <- reactive ({
                req(input$koniec, input$direction, input$linia)
                filter(stopsUnnested, line == input$linia & stop_name == input$koniec & trip_headsign == input$direction) %>% mutate(
                        lat = stop_lat,
                        lon = stop_lon,
                        rodzaj = "koniec",
                        color = "#5000a5"
                ) %>% select(lat,lon,rodzaj)      
        })
        
        # Calculate time to ride
        czasJazdy <- reactive({
                req(input$poczatek, input$koniec, input$linia, input$direction)
                dataStops <- filter(stopsUnnested, stop_name %in% c(input$poczatek, input$koniec) & trip_headsign == input$direction) %>% mutate(
                        stop_code = str_sub(stop_code, 1, 4)
                ) %>% arrange(stop_sequence)
                #print(dataStops)
                
                time = Sys.time()
                
                weekday <- weekdays(time)
                hour <- as.integer(substr(time, 12,13))

                # print(weekday)
                # print(hour)
               
                # print(dataStops)
                # print(input$linia)
                # print(input$direction)
                # print(dataStops$stop_code[1])
                # print(dataStops$stop_code[2])
                if(input$direction != "Wybierz" & !is.na(dataStops$stop_code[1]) & !is.na(dataStops$stop_code[2])) {
                        x <- find_trip_time(input$linia, input$direction, as.numeric(dataStops$stop_code[1]), as.numeric(dataStops$stop_code[2]), weekday, hour)
                        #print(x)
                        x
                }
        })
        
        # Plik do zapisywania pozycji
        logfilename <- paste0('logfile', floor(runif(1, 1e+05, 1e+06 - 1)), '.csv')
        
        # Observer 
        logwriter <- observe({
                # Invalidate this observer every second (1000 milliseconds)
                invalidateLater(10000, session)
                
                # Get the data about the start point selected
                # dataStart <- dataStart()
                
                dataTram <- GET(url = paste0("https://vavel.mini.pw.edu.pl/api/vehicles/v1/full/?line=", input$linia),
                                add_headers(Authorization = paste("Token", token2))) %>% as.character() %>% fromJSON()
                
                # dataTram <- dataTram %>% mutate(
                #         euclidean = sqrt((lat - dataStart$lat[1])^2 + (lon - dataStart$lon[1])^2)
                # ) %>% arrange(euclidean) %>% head(1) %>% mutate(
                #         rodzaj = "tram",
                #         color = "#ce3700"
                # ) %>% select(lat,lon,rodzaj,color)
                
                                
                # Add an entry to the log file
                write.csv(dataTram, file = logfilename)
        })
        
        # Reactive returning data about the nearest Tram
        dataTram <- reactivePoll(1000, session,
                # This function returns the time that the logfile was last
                # modified
                checkFunc = function() {
                 if (file.exists(logfilename))
                         #file.info(logfilename)$mtime[1]
                         read.csv(logfilename) %>% select(lon)
                 else
                         ""
                },
                
                # This function returns the content of the logfile
                valueFunc = function() {
                        if (file.exists(logfilename)) {
                                read.csv(logfilename)
                        }
                }
        )
        
        # Calculate time left for the train to arrive
        # czasDoOdjazdu <- reactive({
        #         req(input$poczatek, input$koniec)
        #         dataStart <- dataStart()
        #         dataTram <- dataTram()
        #         round(sqrt((dataStart$lat - dataTram$lat)^2 + (dataStart$lon - dataTram$lon)^2)*400)
        # })
        
        # When the client ends the session, suspend the observer and
        # remove the log file.
        session$onSessionEnded(function() {
                logwriter$suspend()
                unlink(logfilename)
        })
        
        # Return the text
        output$outputText <- renderUI({
                czasJazdy <- czasJazdy()
                #czasDoOdjazdu <- czasDoOdjazdu()
                box(width = NULL, status = "warning",
                    HTML(paste0("Twój tramwaj przyjedzie niebawem. Możesz śledzić jego położenie na mapie. Średni czas podróży na wybranej trasie o tej porze to:<b> ", czasJazdy, " minut</b>.")
                    )
                )
                
        })
        
        # Return list of directions for selected tram
        output$kierunek <- renderUI({
                przystanki <- filter(stopsUnnested, line == input$linia)
                selectInput("direction", "Wybierz kierunek:",
                            c("Wybierz", unique(przystanki$trip_headsign))
                )
        })
        
        # Return the stops for selected tram
        output$start <- renderUI({
                req(input$linia, input$direction)
                przystanki <- filter(stopsUnnested, line == input$linia, trip_headsign == input$direction)
                selectInput("poczatek", "Wybierz przystanek początkowy:",
                            c("Wybierz", przystanki$stop_name)
                )
        })
        
        # Return the stops for selected tram
        output$end <- renderUI({
                req(input$linia, input$direction)
                przystanki <- filter(stopsUnnested, line == input$linia, trip_headsign == input$direction)
                selectInput("koniec", "Wybierz przystanek końcowy:",
                            c("Wybierz", przystanki$stop_name)
                )
        })
        
        # Return the map
        output$mapa <- renderLeaflet({
                req(input$linia, input$direction)
                dataStart <- dataStart()
                dataEnd <- dataEnd()
                #print(dataTram())
                dataTram <- get_nearest_tram(input$linia, input$poczatek, input$direction, dataTram())
                #print(input$linia)
               # print(input$poczatek)
                #print(input$direction)
                #print(dataTram)
                if (!is.null(dataTram)) {
                        if (!nrow(dataTram) == 0) {
                        
                                dataTram <- dataTram %>% select(lat,lon) %>% mutate(rodzaj = "tram")
                                
                                #print(dataTram)
                                data = rbind.data.frame(dataStart, dataEnd, dataTram)
                                
                                conditionIcon <- icons(
                                        iconUrl = ~ifelse(data$rodzaj == "poczatek",
                                                          "www/start_icon.png", ifelse(data$rodzaj == "koniec", "www/end_icon.png", "www/tram_icon.png")),
                                        iconWidth = 40, iconHeight = 40,
                                        iconAnchorX = 20, iconAnchorY = 20
                                        #shadowUrl = "www/img/marker-shadow.png",
                                        #shadowAnchorX = 12, shadowAnchorY = 41,
                                )
                                
                                map <- leaflet(data) %>%
                                        #addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
                                        addTiles() %>%
                                        addMarkers(~lon,
                                                   ~lat,
                                                   icon = conditionIcon
                                        )
                
                                map
                        }
                }
        })
})