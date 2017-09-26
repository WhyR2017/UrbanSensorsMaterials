## app.R ##
library(shinydashboard)
library(shiny)
library("rvest")
library("jsonlite")
library("httr")
library("tidyverse")
library(leaflet)
library(htmlwidgets)
library(stringr)
library(lubridate)


linie2 <- c("1","2","3","4","7","9","10","11","13","14","15","17","18","20","22","23","24","25","26","27","31","33","35","71")
token2 <- "7a42593ed52a83ef1174496c58192c35053cffa8"
dzis <- ymd(str_sub(Sys.time(), 1, 10))

ui <- dashboardPage(
  dashboardHeader(title = "Lines dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      leafletOutput("mymap"),
      p(),
      selectInput(inputId = "line", label = strong("Wybierz linię:"),
                  choices = linie2,
                  selected = "1"),
      p("Wybierz interesującą Cię linię. W celu odświeżenia danych dla danej linii naciśnij F5."))
      
      
      
    )
  )


server <- function(input, output, session) {
  
  points <- reactive({
    res <- GET(url = paste0("https://vavel.mini.pw.edu.pl/api/vehicles/v1/full/?line=", input$line),
               add_headers(Authorization = paste("Token", token2)))
    jsonlite::fromJSON(as.character(res)) %>%
      mutate(time = ymd(str_sub(time, 1, 10)),
             opoznienie = ifelse(delay>0, 1, 0)) %>%
      filter(status == "MOVING",
             time == dzis,
             timetableStatus != "MISSING")
  })
  
  output$mymap <- renderLeaflet({
    
    icons <- awesomeIcons(
      icon = 'ion-android-subway',
      iconColor = 'black',
      library = 'ion',
      markerColor = ifelse(points()$opoznienie == 1, "red", "green")
    )
    
    leaflet(points()) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addAwesomeMarkers(lng = ~lon, lat = ~lat,
                        label = ~paste(nearestStop, ", opoznienie =",delay, "sekund", sep = " "),
                        icon = icons)
    
    
  })
}

shinyApp(ui, server)
