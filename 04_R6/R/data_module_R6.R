library(shiny)


refreshPane <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(
      ns("refresh"), 
      "Refresh", 
      icon = icon('refresh')), 
    verbatimTextOutput(
      ns("last_updated")
    )
  )
}

#' Data Module for Bike App
data_gather <- function(input, output, session, 
                        api_client) {
  
  dat <- reactive({
    input$refresh
    
    api_client$update_status()
    list(
      last_updated = api_client$last_updated, 
      status = api_client$status
    )
  })
  
  output$last_updated <- renderText(dat()$last_updated)
  
  reactive({
    req(dat)
    bikeRpkg::make_plot_dat(api_client$stations, dat()$status)
  })
}
