#' criteria_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_criteria_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # header
    htmltools::h2("2. Criteria Table")
    
    # Two vertical panels
    # 1. The first three options from TADA_DefineCriteriaMethodology 
    # a. templates available in the TADA Community Hub
    # b. Used ATTAINS in the past 
    # c. Any state/tribe that has submitted to ATTAINS in the past 
    
    # 2. The last two options from TADA_DefineCriteriaMethodology
    # a. blank templateArgument inputs to fill out
    # b. Users editted and uploaded a template
    
    # Section to have a drop-down menu to select the methods
 
  )
}
    
#' criteria_table Server Functions
#'
#' @noRd 
mod_criteria_table_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_criteria_table_ui("criteria_table_1")
    
## To be copied in the server
# mod_criteria_table_server("criteria_table_1")
