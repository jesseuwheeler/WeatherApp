#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app is designed to get the latest weather report for any location
# that you click on the map.

library(shiny)
library(leaflet)  # This package gives us the interactive map
library(maps) # This package gives us the US States
library(jsonlite)  # This package allows us to us JSON data
library(httr)  # This package sends a request to get weather data
library(future)  # this package allows us to create and handle promises

options(stringsAsFactors = FALSE)

get_Icon_URL <- function(icon) {
  # Returns the URL with the icon of the current weather
  #
  # Args:
  #   icon: A string of the icon code for the current weather. This
  #         is returned with the current weather report.
  #
  # Returns:
  #   String URL were the icon of the current weather can be found

  paste0('http://openweathermap.org/img/wn/', icon, '@2x.png')
}

# Website to send API GET request
WEATHER_URL <- 'http://api.openweathermap.org/data/2.5/weather'

# Below I read in my unique API key. You will need to get your own for this app to run locally
APPID <- sourcetools::read_lines('www/appid.txt')

# Get the map of the United States
mapStates = map('state', fill = TRUE, plot = FALSE)

print_weather_report <- function(result) {
  # Returns raw HTML code containing an image tag and formatted text
  # of the current weather report.
  #
  # Args:
  #   result: list object containing the current weather report
  #
  # Returns:
  #   Raw HTML text of the current weather report

  # Get the weather icon (image of current weather)
  weather_icon <- get_Icon_URL(result$weather$icon)

  # Create HTML weather report
  HTML(paste0(
    '<div><img src=', weather_icon, ' height=90 width=90 class="center-image"></div>',
    'Weather: ', result$weather$main, "<br/>", # <br/> is HTML code for new line
    'Min Temperature: ', result$main$temp_min, "&#176;F<br/>", #&#176; is HTML for degree symbol
    'Max Temperature: ', result$main$temp_max, "&#176;F<br/>",
    'Current Temperature: ', result$main$temp, "&#176;F<br/>",
    'Feels Like: ', result$main$feels_like, "&#176;F<br/>",
    'Humidity: ', result$main$humidity, '%<br/>',
    'Pressure: ', result$main$pressure, ' hPa'
  ))
}

# Define UI for application that draws the map
ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "style.css")
  ),

  # Application title
  titlePanel("Weather App"),

  # The single line of code below displays the interactive map
  leafletOutput('mymap'),

  # This is the HTML code that displays the Weather report
  tags$div(
    id = 'history-div',
    class = 'light-blue',
    tags$h3(class = 'center-title', "Weather Report"),
    tags$h4(class = 'center-title', textOutput('location')),
    tags$h4(class = 'center-title', htmlOutput('weather'))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Create the interactive map
  output$mymap <- renderLeaflet({
    leaflet(data = mapStates) %>%
      addTiles(options = tileOptions(minZoom = 4,
                                     maxZoom = 10)) %>%
      addPolygons(stroke = FALSE) %>%
      # Setting the bounds to be the United States
      setMaxBounds(lat1 = 20,
                   lat2 = 50,
                   lng1 = -130,
                   lng2 = -60) %>%
      # Setting the starting location of the map
      fitBounds(lat1 = 20,
                lat2 = 50,
                lng1 = -130,
                lng2 = -60)
  })

  # This function defines what happens when we click the map
  observeEvent(input$mymap_click, {

    # Get and save the Latitude and Longitude of click location
    lat <- round(input$mymap_click$lat, 2)
    lng <- round(input$mymap_click$lng, 2)

    text_lat <- paste0('Latitude: ', lat)
    text_lng <- paste0('Longitude: ', lng)

    # Create text output of the latitude and longitude
    output$location <- renderText({
      paste0(text_lat, '\n', text_lng)
    })

    # Use the Latitude and longitude to modify weather URL
    current_weather_url <- paste0(WEATHER_URL,
                                  '?', 'lat=', lat,
                                  '&', 'lon=', lng,
                                  '&', 'units=imperial',
                                  '&', 'appid=', APPID)

    # Send GET request to weather API using updated URL
    output$weather <- renderUI({
      future({
        GET(url = current_weather_url)$content %>%
          # Convert raw bites to characters
          rawToChar() %>%
          # Convert JSON (Java Script Object Notation) data to a list
          fromJSON() %>%
          # input list into our weather report function
          print_weather_report()
      })
    })
  })

}

# Run the application
shinyApp(ui = ui, server = server)
