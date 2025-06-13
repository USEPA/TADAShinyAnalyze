#' custom_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_custom_analysis_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("3. Custom Analysis"),
    
    # start fluidrow
    # fill in code here
 
  )
}
    
#' custom_analysis Server Functions
#'
#' @noRd 
mod_custom_analysis_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_custom_analysis_ui("custom_analysis_1")
    
## To be copied in the server
# mod_custom_analysis_server("custom_analysis_1")
