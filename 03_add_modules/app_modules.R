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
                        refreshButton("data"),
                        verbatimTextOutput(
                          "last_updated"
                        ),
                        width = 12
                      )
                    )
)

server <- function(input, output, session) {
  
  #----- Data Gathering + Plot Data Formatting -----
  dat <- callModule(data_gather, "data")
  
  #----- Format Outputs -----
  output$last_updated <- renderText({
    req(dat$status)
    glue::glue("Last Updated: {dat$status()$last_updated}")
  })
  
  output$map <- renderLeaflet({
    req(dat$plot_dat)
    
    plot_dat <- dat$plot_dat
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