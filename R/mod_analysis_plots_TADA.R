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
    rv <- shiny::reactive({
      req(!is.null(tadat$df_mlid_input))
      
      df_mlid_input <- tadat$df_mlid_input
      return(df_mlid_input)
    })
    
    # check if tadat$df_mlid_input has been loaded
    # shiny::observe({
    #   shiny::validate(need(!is.null(tadat$df_mlid_input), "No file selected."))
    #   
    #   rv$df_mlid_input <- tadat$df_mlid_input
    # })
    
    # Create plot from TADA
    output$TADA_timeseries_view <- plotly::renderPlotly({
      req(tadat$df_mlid_input)
      
      # data to plot
      df_mlid_input <- rv()
      
      # Create your plotly plot (using plot_ly or ggplotly)
      p <- EPATADA::TADA_Scatterplot(EPATADA::Data_MT_MissoulaCounty)[1]
      
      # Return the plotly object
      return(p)
    })
  })
}