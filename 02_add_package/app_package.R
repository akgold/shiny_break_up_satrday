library(shiny)
library(shinydashboard)
library(httr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(bikeRpkg)

# Create dashboard page UI
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Capitol Bikeshare Bikes"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      box(
                        title = "Station Map",
                        leafletOutput(
                          "map"
                        ),
                        shiny::actionButton(
                          "refresh", 
                          label = "Refresh", 
                          icon("refresh")
                        ),
                        verbatimTextOutput(
                          "last_updated"
                        ),
                        width = 12
                      )
                    )
)

server <- function(input, output, session) {
  
  #----- Data Gathering-----
  # Get stations (name, lat, long)
  stations <- bikeRpkg::get_feed_dat("station_information")
  
  # Get current status (bikes/docks per station)
  status <- reactive({
    input$refresh
    
    bikeRpkg::get_feed_dat("station_status")
  })
  
  #----- Create Plot Data -----
  plot_dat <- reactive({
    req(status)
    bikeRpkg::make_plot_dat(stations$dat, status()$dat)
  })
  
  #----- Format Outputs -----
  output$last_updated <- renderText({
    req(status)
    glue::glue("Last Updated: {status()$last_updated}")
  })
  
  output$map <- renderLeaflet({
    req(plot_dat)
    
    plot_dat() %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = median(plot_dat()$lon), lat = median(plot_dat()$lat), zoom = 14) %>%
      addAwesomeMarkers(
        lng = ~lon,
        lat = ~lat,
        icon = awesomeIcons(
          "bicycle",
          library = "fa",
          iconColor = "white",
          markerColor = "red"
        ),
        popup = ~paste0(plot_val)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)