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
    # Components
    fluidRow(
      column(
        width = 12,
        mod_analysis_selector_ui("Batch_Select")
      )
    ),
    
    # Horizontal divider
    hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          div(style = "display: flex; align-items: center; gap: 10px;",
              htmltools::h4("Run Batch Analysis:", style = "margin: 0;"),
              shiny::actionButton(inputId = ns("Run_Batch"),
                                  label = "Run")
          )
        )
      )
    ),
    
    # Horizontal divider
    hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    # Select the ML/AU iD
    fluidRow(
      column(
        width = 12,
        column(
          width = 6,
          selectizeInput(inputId = ns("loc_filter"),
                         label = "Filter ML/AU ID to view the results",
                         choices = NULL)
        ),
        column(
          width = 6,
          selectizeInput(inputId = ns("parameter_filter"),
                         label = "Filter parameter to view the results",
                         choices = NULL)
        )
      )

    ),
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
