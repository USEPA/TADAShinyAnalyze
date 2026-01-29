#' analysis_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_plots_ui_TADA <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12, 
               plotly::plotlyOutput(ns("TADA_timeseries_view"))
      )
      )
    )
}

#' analysis_plots Server Functions
#'
#' @noRd 
mod_analysis_plots_server_TADA <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Set the initial state
    rv <- shiny::reactiveValues(
      unique_flag = FALSE,
      season_flag = FALSE,
      p_boxplot   = NULL,
      p_timeseries= NULL
    )

    # Create plot from TADA
    output$TADA_timeseries_view <- plotly::renderPlotly({
      # Create your plotly plot (using plot_ly or ggplotly)
      p <- EPATADA::TADA_Scatterplot(.data = as.data.frame(tadat$df_mlid_input))
      
      # Return the plotly object
      p
    })
  })
}