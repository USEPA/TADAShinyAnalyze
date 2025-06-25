#' boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boxplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' boxplot Server Functions
#'
#' @noRd 
mod_boxplot_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_boxplot_ui("boxplot_1")
    
## To be copied in the server
# mod_boxplot_server("boxplot_1")
