#' analysis_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(inputId = ns("loc_select"),
                            label = "Batch Analyzed by: ",
                            choices = c("Monitoring Location ID" = "MLId",
                                        "Assessment Unit (Individual)" = "AU_ind",
                                        "Assessment Unit (Group)" = "AU_group"))
      ),
      column(
        width = 6,
        shiny::selectInput(inputId = ns("state_tribe"),
                           label = "Select state/tribe",
                           choices = NULL),
        shinyWidgets::virtualSelectInput(
          inputId = ns("uses_select"),
          label = "Select:",
          choices = NULL,
          showValueAsTags = TRUE,
          search = TRUE,
          multiple = TRUE
        )
      )
    )
  )
}
    
#' analysis_selector Server Functions
#'
#' @noRd 
mod_analysis_selector_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_analysis_selector_ui("analysis_selector_1")
    
## To be copied in the server
# mod_analysis_selector_server("analysis_selector_1")
