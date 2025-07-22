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

      output$exceed_table <- DT::renderDT(

        DT::datatable(
          tadat$exceed_summary_f |>
            dplyr::mutate(Exceedance_Percentage = Exceedance_Percentage/100),
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
          DT::formatRound(
            columns = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"),
          digits = 3) |>
          DT::formatPercentage(
            columns = c("Exceedance_Percentage")
          )
        )
    })
    
  })
}
    
## To be copied in the UI
# mod_exceedance_viewer_ui("exceedance_viewer_1")
    
## To be copied in the server
# mod_exceedance_viewer_server("exceedance_viewer_1")
