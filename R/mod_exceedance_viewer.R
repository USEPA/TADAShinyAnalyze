#' exceedance_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exceedance_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' exceedance_viewer Server Functions
#'
#' @noRd 
mod_exceedance_viewer_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_exceedance_viewer_ui("exceedance_viewer_1")
    
## To be copied in the server
# mod_exceedance_viewer_server("exceedance_viewer_1")
