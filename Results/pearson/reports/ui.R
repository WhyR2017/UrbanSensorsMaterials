library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
        title = "CzyZdąże.pl"
)

body <- dashboardBody(
        fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("mapa", height = 500)
                       )
                ),
                column(width = 3,
                       box(width = NULL, status = "warning",
                           selectInput("linia", "Wybierz linię:",
                                       list(3,11,13)
                           ),
                           uiOutput("kierunek"),
                           uiOutput("start"),
                           uiOutput("end")
                       ),
                       conditionalPanel(
                               condition = "input.poczatek != 'Wybierz' & input.koniec != 'Wybierz'",
                               uiOutput("outputText")
                       )
                )
        )
)

dashboardPage(
        header,
        dashboardSidebar(disable = TRUE),
        body
)