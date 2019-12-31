#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(maps)

mapStates = map('state', fill = TRUE, plot = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "style.css")
    ),

    # Application title
    titlePanel("Weather App"),
    leafletOutput('mymap'),

    tags$div(
        class = 'light-blue',
        tags$h3(class = 'center-title', "Current Weather"),
        tags$h4(class = 'center-title', 'Logan, UT')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mymap <- renderLeaflet({
        leaflet(data = mapStates) %>%
            addTiles(options = tileOptions(minZoom = 4,
                                           maxZoom = 7)) %>%
            addPolygons(stroke = FALSE) %>%
            setMaxBounds(lat1 = 20,
                         lat2 = 50,
                         lng1 = -130,
                         lng2 = -60) %>%
            fitBounds(lat1 = 20,
                      lat2 = 50,
                      lng1 = -130,
                      lng2 = -60)
    })

    observeEvent(input$mymap_click, {
        lat <- paste0('Latitude: ', input$mymap_click$lat)
        lng <- paste0('Longitude: ', input$mymap_click$lng)
        cat(paste0(lat, '\n', lng, '\n\n'))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
