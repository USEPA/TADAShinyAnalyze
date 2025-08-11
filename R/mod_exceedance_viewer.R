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
    DT::DTOutput((outputId = ns("exceed_table")))
  )
}
    
#' exceedance_viewer Server Functions
#'
#' @noRd 
mod_exceedance_viewer_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ### Show the data as a data table
    shiny::observeEvent(tadat$exceed_summary_f, {

      # Handle NULL or empty data - clear the table
      if (is.null(tadat$exceed_summary_f) || nrow(tadat$exceed_summary_f) == 0) {
        output$exceed_table <- DT::renderDT(NULL)
        return()
      }
      
      # Simplify the table when tadat$loc_select == AU_group
      if (tadat$loc_select %in% "AU_group"){
        dat <- tadat$exceed_summary_f |>
          dplyr::select(-TADA.MonitoringLocationIdentifier,
                        -TADA.MonitoringLocationName,
                        -TADA.LongitudeMeasure,
                        -TADA.LatitudeMeasure) |>
          dplyr::distinct()
      } else {
        dat <- tadat$exceed_summary_f
      }

      dat <- dat |> dplyr::mutate(Exceedance_Percentage = Exceedance_Percentage/100)

      output$exceed_table <- DT::renderDT(
        DT::datatable(
          dat,
          filter = "top",
          class = "compact",
          options = list(scrollX = TRUE,
                         scrollY = TRUE,
                         pageLength = 10,
                         lengthMenu = c(10, 25, 50, 100),
                         autoWidth = TRUE)) |>
          DT::formatRound(
            columns = c("Minimum", "Median", "Maximum")
          ) |>
          DT::formatPercentage(
            columns = c("Exceedance_Percentage")
          ) 
        )
    }, ignoreNULL = FALSE)
    
  })
}
    
## To be copied in the UI
# mod_exceedance_viewer_ui("exceedance_viewer_1")
    
## To be copied in the server
# mod_exceedance_viewer_server("exceedance_viewer_1")
