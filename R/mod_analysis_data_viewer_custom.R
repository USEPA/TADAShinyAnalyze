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
      column(width = 6,
             htmltools::h4("Summary of the selected data"),
             shiny::verbatimTextOutput(ns("Avail_Data_Custom"), 
                                       placeholder = TRUE)
      ),
      column(width = 12,
             htmltools::h4("Matched Parameters"),
             DT::DTOutput(ns("Matched_Data_Custom"))
      ),
    )
  )
}

#' analysis_data_viewer_custom Server Functions
#'
#' @noRd
mod_analysis_data_viewer_custom_server <- function(id, tadat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      output$Avail_Data_Custom <- shiny::renderText(
        # if file was selected
        if (is.null(tadat$available_param_num_custom)) {
          "Users need to provide inputs to select the data or the tool could not find matched parameters based on current selections. \nPlease refine the selection"
        } else if (tadat$available_param_num_custom == 0) {
          "The tool could not find matched parameters based on current selections. \nPlease refine the selection"
        } else {
          paste0(
            "The selected dataset has ",
            tadat$available_param_num_custom,
            " parameters that matched the selections."
          )
        }
      )
    })
 
    # See mod_custom_analysis.R for what tadat$dat_match_custom looks like.
    output$Matched_Data_Custom <- DT::renderDataTable({
      shiny::validate(need(!is.null(tadat$custom_raw), "No matched data."))
      
      custom_raw <- tadat$custom_raw |>
        dplyr::select(
          ATTAINS.ParameterName, TADA.CharacteristicName, TADA.ResultSampleFractionText,
          TADA.MethodSpeciationName, TADA.ResultMeasure.MeasureUnitCode
        ) |>
        dplyr::distinct()
      
      # render table
      DT::datatable(custom_raw,
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   scrollY = "400px",
                                   scrollCollapse = TRUE,
                                   paging = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = FALSE, 
                                   fillContainer = TRUE ))
    })
    
  })
}

## To be copied in the UI
# mod_analysis_data_viewer_custom_ui("analysis_data_viewer_custom_1")

## To be copied in the server
# mod_analysis_data_viewer_custom_server("analysis_data_viewer_custom_1")
