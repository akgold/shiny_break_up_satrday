library(shiny)


refreshButton <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("refresh"), "Refresh", icon = icon('refresh'))
  )
}

#' Data Module for Bike App
data_gather <- function(input, output, session) {
  status <- reactive({
    input$refresh
    bikeRpkg::get_feed_dat("station_status")
  })
  
  list(
    stations = bikeRpkg::get_feed_dat("station_information"), 
    status = status, 
    plot_dat =  reactive({
      req(status)
      bikeRpkg::make_plot_dat(stations$dat, status()$dat)
    })
  )
}
