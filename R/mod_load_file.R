#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_file_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("1. Load File"),
    
    # start fluidrow
    # fill in code here
 
  )
}
    
#' load_file Server Functions
#'
#' @noRd 
mod_load_file_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_load_file_ui("load_file_1")
    
## To be copied in the server
# mod_load_file_server("load_file_1")
