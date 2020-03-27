library(shiny)
library(shinydashboard)
library(httr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(bikeR6pkg)

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
                        refreshPane("data"),
                        width = 12
                      )
                    )
)

server <- function(input, output, session) {
  bike_client <- BikeClient$new()
  
  #----- Data Gathering + Plot Data Formatting -----
  plot_dat <- callModule(data_gather, "data", bike_client)
  
  #----- Format Outputs -----
  output$map <- renderLeaflet({
    req(plot_dat)
    
    plot_dat() %>%
      leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      setView(
        lng = median(plot_dat()$lon), 
        lat = median(plot_dat()$lat), 
        zoom = 14) %>%
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