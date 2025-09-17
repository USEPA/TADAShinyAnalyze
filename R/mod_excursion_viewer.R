#' excursion_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_excursion_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::p("The table shows the summary of excursion (individual sample in comparison to the standard) and exceedance (evaluated based on the duration and frequency information.)"),
    DT::DTOutput((outputId = ns("excurse_table")))
  )
}
    
#' excursion_viewer Server Functions
#'
#' @noRd 
mod_excursion_viewer_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ### Show the data as a data table
    shiny::observeEvent(tadat$excurse_summary_f, {

      # Handle NULL or empty data - clear the table
      if (is.null(tadat$excurse_summary_f) || nrow(tadat$excurse_summary_f) == 0) {
        output$excurse_table <- DT::renderDT(NULL)
        return()
      }
      
      # # Simplify the table when tadat$loc_select == AU_group
      # if (tadat$loc_select %in% "AU"){
      #   dat <- tadat$excurse_summary_f |>
      #     dplyr::select(-TADA.MonitoringLocationIdentifier,
      #                   -TADA.MonitoringLocationName,
      #                   -TADA.LongitudeMeasure,
      #                   -TADA.LatitudeMeasure) |>
      #     dplyr::distinct()
      # } else {
      #   dat <- tadat$excurse_summary_f
      # }
      dat <- tadat$excurse_summary_f

      dat <- dat |> 
        dplyr::mutate(Excursion_Percentage = Excursion_Percentage/100) |>
        dplyr::mutate(Duration_Percentage = Duration_Percentage/100)

      output$excurse_table <- DT::renderDT(
        DT::datatable(
          dat,
          filter = "top",
          class = "compact",
          options = list(scrollX = TRUE,
                         scrollY = "400px",
                         scrollCollapse = TRUE,
                         paging = TRUE,
                         pageLength = 5,
                         lengthMenu = c(5, 10, 25, 50, 100),
                         autoWidth = TRUE)) |>
          DT::formatRound(
            columns = c("Minimum", "Median", "Maximum")
          ) |>
          DT::formatPercentage(
            columns = c("Excursion_Percentage", "Duration_Percentage")
          ) 
        )
    }, ignoreNULL = FALSE)
    
  })
}
    
## To be copied in the UI
# mod_excursion_viewer_ui("excursion_viewer_1")
    
## To be copied in the server
# mod_excursion_viewer_server("excursion_viewer_1")
