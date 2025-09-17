#' analysis_data_viewer_custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_data_viewer_custom_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             htmltools::br("Summary of the selected data"),
             shiny::verbatimTextOutput(ns("Avail_Data_Custom"), 
                                       placeholder = TRUE)
             )
    )
  )
}
    
#' analysis_data_viewer_custom Server Functions
#'
#' @noRd 
mod_analysis_data_viewer_custom_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shiny::observe({
      output$Avail_Data_Custom <- shiny::renderText(
        # if file was selected
        if (is.null(tadat$available_param_num_custom)) {
          "Users need to provide inputs to select the data or the tool could not find matched parameters based on the selected state/tribe criteria table. \nPlease refine the state/tribe criteria selection"
        } else if (tadat$available_param_num_custom == 0){
          "The tool could not find matched parameters based on the selected state/tribe criteria table. \nPlease refine the state/tribe criteria selection"
        } else {
          paste0(
            "The selected dataset has ", tadat$available_param_num_custom, " parameters that matched the selected state/tribe criteria table."
          )
        }
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_analysis_data_viewer_custom_ui("analysis_data_viewer_custom_1")
    
## To be copied in the server
# mod_analysis_data_viewer_custom_server("analysis_data_viewer_custom_1")
