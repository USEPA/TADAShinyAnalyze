#' batch_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_batch_analysis_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("2. Batch Analysis"),
    
    # start fluidrow
    # fill in code here
 
  )
}
    
#' batch_analysis Server Functions
#'
#' @noRd 
mod_batch_analysis_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_batch_analysis_ui("batch_analysis_1")
    
## To be copied in the server
# mod_batch_analysis_server("batch_analysis_1")
