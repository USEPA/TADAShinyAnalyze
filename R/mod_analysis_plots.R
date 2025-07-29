#' analysis_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_plots_ui <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    column(
      width = 6,
      # Parameter selectize input (single selection)
      shiny::selectizeInput(
        inputId = ns("parameter_box_select"),
        label = "Select a parameter",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Select a parameter",
          create = FALSE
          )
        )
      ),
    column(
      width = 6,
      # Uses selectize input (multiple selection)
      shiny::selectizeInput(
        inputId = ns("uses_box_select"),
        label = "Select the uses",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Select the uses",
          create = FALSE
          )
        )
      )
    ),
  fluidRow(
    column(
      width = 6,
      # Uses selectize input (multiple selection)
      shiny::selectizeInput(
        inputId = ns("loc_box_select"),
        label = "Select the location",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select location",
          create = FALSE
          )
        )
      )
    ),
  fluidRow(
    column(width = 6, plotOutput(ns("boxplot_view"))),
    column(width = 6, plotOutput(ns("timeseries_view")))
  )
  )
}
    
#' analysis_plots Server Functions
#'
#' @noRd 
mod_analysis_plots_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # Update selectize inputs when data changes
    shiny::observe({
      shiny::req(tadat$exceed_dat)
      shiny::req(nrow(tadat$exceed_dat) > 0)
      
      # Get unique values for Parameter dropdown
      param_choices <- sort(unique(tadat$exceed_dat$TADA.CharacteristicName))
      
      # Update Parameter selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "parameter_box_select",
        choices = param_choices,
        selected = if(length(param_choices) > 0) param_choices[1] else NULL,
        server = TRUE
      )
    })
    
    # Reactive to filter data based on selections
    filtered_data1 <- shiny::reactive({
      shiny::req(tadat$exceed_dat)
      shiny::req(input$parameter_box_select)
      
      # Filter by selected parameter
      dat2 <- tadat$exceed_dat |>
        dplyr::filter(TADA.CharacteristicName %in% input$parameter_box_select)
      
      return(dat2)
    })
    
    shiny::observe({
      shiny::req(filtered_data1())
      
      # Get unique values for Uses dropdown
      uses_choices <- sort(unique(filtered_data1()$ATTAINS.UseName))
      
      # Update Uses selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "uses_box_select",
        choices = uses_choices,
        selected = NULL,
        server = TRUE
      )
      
    })
    
    
    # Reactive to filter data based on selections
    filtered_data2 <- shiny::reactive({
      shiny::req(filtered_data1())
      shiny::req(input$uses_box_select)
      
      # Filter by selected uses
      dat2 <- filtered_data1() |>
        dplyr::filter(ATTAINS.UseName %in% input$uses_box_select)
      
      return(dat2)
    })
    
    # Update the available location selection
    shiny::observe({
      shiny::req(filtered_data2())
      shiny::req(tadat$loc_select)
      if (tadat$loc_select %in% c("MLid", "AU_ind")){
        loc_choices <- sort(unique(filtered_data2()$TADA.MonitoringLocationIdentifier))
      } else {
        loc_choices <- sort(unique(filtered_data2()$JoinToAU.AssessmentUnitIdentifier))
      }
      
      # Update location selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "loc_box_select",
        choices = loc_choices,
        selected = if(length(loc_choices) > 0) loc_choices[1] else NULL,
        server = TRUE
      )
      
    })
    
    # Reactive to filter data based on location selections
    filtered_data3 <- shiny::reactive({
      shiny::req(filtered_data2())
      shiny::req(input$loc_box_select)
      
      # Filter by selected location
      if (tadat$loc_select %in% c("MLid", "AU_ind")){
        dat2 <- filtered_data2() |>
          dplyr::filter(TADA.MonitoringLocationIdentifier %in% input$loc_box_select)
      } else {
        dat2 <- filtered_data2() |>
          dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% input$loc_box_select)
      }
      #add filtered_data4 - change 0 values to 0,000001
      dat2 <- dat2 |>
        dplyr::mutate(TADA.ResultMeasureValue = ifelse(TADA.ResultMeasureValue == 0,
                                                       0.000001, TADA.ResultMeasureValue))
      
      return(dat2)
    })
    

    # Create boxplot
    output$boxplot_view <- shiny::renderPlot({
      shiny::req(filtered_data2())
      shiny::req(filtered_data3())
      
      # Check if there's data to plot
      if(nrow(filtered_data2()) == 0 | nrow(filtered_data3()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", 
             cex = 1.2, col = "gray50")
        return(NULL)
      }
      

      p <- ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = filtered_data3(),
                              ggplot2::aes(x = ATTAINS.UseName,
                                           y = TADA.ResultMeasureValue),
                              color = 'gray30',
                              outlier.shape = NA) 
      
      if (tadat$loc_select %in% c("MLid", "AU_ind")){
        tadat$p_boxplot <- p + ggplot2::geom_jitter(data = filtered_data3(), ggplot2::aes(x = ATTAINS.UseName,
                                                                            y = TADA.ResultMeasureValue
                                                                            , fill = TADA.MonitoringLocationIdentifier),
                                      color = 'black',
                                      shape = 21,
                                      size = 3.5,
                                      width = 0.2,
                                      alpha = 0.8) +
          ggplot2::xlab('Uses') +
          ggplot2::ylab(paste0(unique(filtered_data2()$TADA.CharacteristicName), 
                               ' (', filtered_data2()$TADA.ResultMeasure.MeasureUnitCode, ')')) +
          ggplot2::scale_y_log10() +
          ggplot2::scale_x_discrete(name = "") +
          ggplot2::theme_bw() +
          viridis::scale_fill_viridis(discrete = T,
                                      option = "mako") +
          ggplot2::labs(fill = 'Monitoring Location ID') +
          ggplot2::theme(legend.position = "right"
                         , text = ggplot2::element_text(size = 24)
                         , axis.text = ggplot2::element_text(size = 22)
                         , legend.background = ggplot2::element_rect(colour = 'gray60', fill = 'white', linetype='dashed'))
      } else {
        tadat$p_boxplot <- p + ggplot2::geom_jitter(data = filtered_data3(), ggplot2::aes(x = ATTAINS.UseName,
                                                                            y = TADA.ResultMeasureValue
                                                                            , fill = JoinToAU.AssessmentUnitIdentifier),
                                      color = 'black',
                                      shape = 21,
                                      size = 3.5,
                                      width = 0.2,
                                      alpha = 0.8) +
          ggplot2::xlab('Uses') +
          ggplot2::ylab(paste0(filtered_data2()$TADA.CharacteristicName,
                               ' (', filtered_data2()$TADA.ResultMeasure.MeasureUnitCode, ')')) +
          ggplot2::scale_y_log10() +
          ggplot2::scale_x_discrete(name = "") +
          ggplot2::theme_bw() +
          viridis::scale_fill_viridis(discrete = T,
                                      option = "mako") +
          ggplot2::labs(fill = 'Assessment Unit ID') +
          ggplot2::theme(legend.position = "right"
                         , text = ggplot2::element_text(size = 24)
                         , axis.text = ggplot2::element_text(size = 22)
                         , legend.background = ggplot2::element_rect(colour = 'gray60', fill = 'white', linetype='dashed'))
      }
      return(tadat$p_boxplot)
    })
      # Create timeseries
      output$timeseries_view <- renderPlot({
        req(filtered_data2())
        req(filtered_data3())
        
        # Check if there's data to plot
        if(nrow(filtered_data2()) == 0 | nrow(filtered_data3()) == 0) {
          plot.new()
          text(0.5, 0.5, "No data available for selected filters", 
               cex = 1.2, col = "gray50")
          return(NULL)
        }
        
        
        if (tadat$loc_select %in% c("MLid", "AU_ind")){
          tadat$p_timeseries<-ggplot2::ggplot() +
            ggplot2::geom_point(data = filtered_data3(),
                                ggplot2::aes(x = ActivityStartDate,
                                             y = TADA.ResultMeasureValue,
                                             fill = MonitoringLocationIdentifier),
                                color = 'black',
                                shape = 21,
                                size = 3.5,
                                alpha = 0.8) +
            ggplot2::xlab('Time') +
            ggplot2::scale_y_log10() +
            ggplot2::ylab(paste0(filtered_data2()$TADA.CharacteristicName,
                                 ' (', filtered_data2()$TADA.ResultMeasure.MeasureUnitCode, ')')) +
            ggplot2::theme_bw() +
            viridis::scale_fill_viridis(discrete = T,
                                        option = "mako") +
            ggplot2::labs(fill = 'Monitoring Location ID') +
            ggplot2::theme(legend.position = "right"
                           , text = ggplot2::element_text(size = 24)
                           , axis.text = ggplot2::element_text(size = 22)
                           , legend.background = ggplot2::element_rect(colour = 'gray60', fill = 'white', linetype='dashed'))
        } else {
          tadat$p_timeseries<-ggplot2::ggplot() +
            ggplot2::geom_point(data = filtered_data3(),
                                ggplot2::aes(x = ActivityStartDate,
                                             y = TADA.ResultMeasureValue,
                                             fill = JoinToAU.AssessmentUnitIdentifier),
                                color = 'black',
                                shape = 21,
                                size = 3.5,
                                alpha = 0.8) +
            ggplot2::xlab('Time') +
            ggplot2::scale_y_log10() +
            ggplot2::ylab(paste0(filtered_data2()$TADA.CharacteristicName,
                                 ' (', filtered_data2()$TADA.ResultMeasure.MeasureUnitCode, ')')) +
            ggplot2::theme_bw() +
            viridis::scale_fill_viridis(discrete = T,
                                        option = "mako") +
            ggplot2::labs(fill = 'Monitoring Location ID') +
            ggplot2::theme(legend.position = "right"
                           , text = ggplot2::element_text(size = 24)
                           , axis.text = ggplot2::element_text(size = 22)
                           , legend.background = ggplot2::element_rect(colour = 'gray60', fill = 'white', linetype='dashed'))
        }
        return(tadat$p_timeseries)
    })
  })
}
    
## To be copied in the UI
# mod_analysis_plots_ui("analysis_plots_1")
    
## To be copied in the server
# mod_analysis_plots_server("analysis_plots_1")
