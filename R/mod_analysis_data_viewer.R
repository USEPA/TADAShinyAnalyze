#' analysis_data_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_data_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             htmltools::h4("Summary of the selected data"),
             shiny::verbatimTextOutput(ns("Avail_Data"), 
                                       placeholder = TRUE)
      ),
      column(width = 6,
               htmltools::h4("Matched Parameters"),
               DT::DTOutput(ns("Matched_Data"))
      ),
    )#,
    # fluidRow(
    #   # column(width = 6,
    #   #        htmltools::h4("Matched Parameters"),
    #   #        DT::DTOutput(ns("Matched_Data"))
    #   # ),
    #   # column(width = 6,
    #   #        htmltools::h4("Partial Matched Parameters"),
    #   #        DT::DTOutput(ns("Not_Matched_Data"))
    #   # )
    # )
 
  )
}
    
#' analysis_data_viewer Server Functions
#'
#' @noRd 
mod_analysis_data_viewer_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shiny::observe({
      output$Avail_Data <- shiny::renderText(
        # if file was selected
        if (is.null(tadat$available_param_num)) {
          "Users need to provide inputs to select the data or the tool could not find matched parameters based on current selections. \nPlease refine the selection"
        } else if (tadat$available_param_num == 0){
          "The tool could not find matched parameters based on current selections. \nPlease refine the selection"
        } else {
          paste0(
            "The selected dataset has ", tadat$available_param_num, " parameters that matched the selections."
          )
        }
      )
      
      output$Matched_Data <- DT::renderDT({
        shiny::validate(need(!is.null(tadat$dat_match), "No matched data."))
        
        # render table
        DT::datatable(tadat$dat_match,
                      filter = "top",
                      class = "compact",
                      options = list(scrollX = TRUE,
                                     scrollY = "400px",
                                     scrollCollapse = TRUE,
                                     paging = TRUE,
                                     pageLength = 5,
                                     lengthMenu = c(5, 10, 25, 50, 100),
                                     autoWidth = TRUE))
      })
      
      # output$Not_Matched_Data <- DT::renderDT({
      #   shiny::validate(need(!is.null(tadat$dat_match), "No matched data."))
      #   
      #   # render table
      #   DT::datatable(tadat$dat_not_match,
      #                 filter = "top",
      #                 class = "compact",
      #                 options = list(scrollX = TRUE,
      #                                scrollY = "400px",
      #                                scrollCollapse = TRUE,
      #                                paging = TRUE,
      #                                pageLength = 5,
      #                                lengthMenu = c(5, 10, 25, 50, 100),
      #                                autoWidth = TRUE))
      # })
      
    })
 
  })
}
    
## To be copied in the UI
# mod_analysis_data_viewer_ui("analysis_data_viewer_1")
    
## To be copied in the server
# mod_analysis_data_viewer_server("analysis_data_viewer_1")
