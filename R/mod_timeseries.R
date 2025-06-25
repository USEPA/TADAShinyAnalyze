#' timeseries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timeseries_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' timeseries Server Functions
#'
#' @noRd 
mod_timeseries_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_timeseries_ui("timeseries_1")
    
## To be copied in the server
# mod_timeseries_server("timeseries_1")
