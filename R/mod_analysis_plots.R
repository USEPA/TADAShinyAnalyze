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
      
      options(scipen=999)
      
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
    
    
    output$timeseries_view <- shiny::renderPlot({
      shiny::req(filtered_data2())
      shiny::req(filtered_data3())
      
      options(scipen = 999)
      
      if (nrow(filtered_data3()) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data available for selected filters",
                       cex = 1.2, col = "gray50")
        return(NULL)
      }
      
      df <- filtered_data3()
      
      fill_var <- if (tadat$loc_select %in% c("MLid", "AU_ind")) {
        df$MonitoringLocationIdentifier
      } else {
        df$JoinToAU.AssessmentUnitIdentifier
      }
      
      # Base plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = ActivityStartDate, y = TADA.ResultMeasureValue)) +
        ggplot2::geom_point(ggplot2::aes(fill = fill_var),
                            color = "black", shape = 21, size = 3.5, alpha = 0.8) +
        ggplot2::scale_y_log10() +
        ggplot2::xlab("Time") +
        ggplot2::ylab(paste0(filtered_data2()$CharacteristicName[1], " (", filtered_data2()$ResultMeasure.MeasureUnitCode[1], ")")) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = TRUE, option = "mako") +
        ggplot2::labs(fill = if (tadat$loc_select %in% c("MLid", "AU_ind")) {
          "Monitoring Location ID"
        } else {
          "Assessment Unit ID"
        }) +
        ggplot2::theme(
          legend.position = "right",
          text = ggplot2::element_text(size = 24),
          axis.text = ggplot2::element_text(size = 22),
          legend.background = ggplot2::element_rect(colour = "gray60", fill = "white", linetype = "dashed")
        )
      
      # Helper: line type from AcuteChronic
      get_linetype <- function(vals) {
        typ <- unique(tolower(na.omit(vals)))
        if ("acute" %in% typ) return("dotted")
        if ("chronic" %in% typ) return("dashed")
        return("solid")
      }
      
      # Helper: shape from AcuteChronic
      get_shape <- function(vals) {
        typ <- unique(tolower(na.omit(vals)))
        if ("acute" %in% typ) return(3)
        if ("chronic" %in% typ) return(4)
        return(17)
      }
      
      plot_criteria <- function(df_sub, value_col, color) {
        if (!value_col %in% names(df_sub)) return(NULL)
        df_valid <- df_sub[!is.na(df_sub[[value_col]]) & df_sub[[value_col]] > 0, ]
        
        if (nrow(df_valid) < 1) return(NULL)
        
        # Equation-based → always points
        is_equation_based <- unique(tolower(df_valid$EquationBased)) == "yes"
        shape <- get_shape(df_valid$AcuteChronic)
        linetype <- get_linetype(df_valid$AcuteChronic)
        
        if (is_equation_based) {
          # Plot as points
          p <<- p + ggplot2::geom_point(
            data = df_valid,
            ggplot2::aes_string(x = "ActivityStartDate", y = value_col),
            color = color,
            shape = shape,
            size = 3,
            stroke = 1
          )
        } else {
          # Split into groups by unique value to avoid zig-zag
          for (val in unique(df_valid[[value_col]])) {
            df_group <- df_valid[df_valid[[value_col]] == val, ]
            if (nrow(df_group) < 2) next  # skip single-point groups
            
            p <<- p + ggplot2::geom_line(
              data = df_group,
              ggplot2::aes_string(x = "ActivityStartDate", y = value_col),
              color = color,
              linetype = get_linetype(df_group$AcuteChronic),
              linewidth = 1
            )
          }
        }
      }
      
      
      # Plot upper (red) and lower (blue) criteria
      plot_criteria(df, "MagnitudeValueUpper", "red")
      plot_criteria(df, "MagnitudeValueLower", "blue")
      
      # WQS Criteria Legend Setup 
      legend_lines <- data.frame(
        ActivityStartDate = rep(min(df$ActivityStartDate, na.rm = TRUE), 6),
        Value = rep(min(df$TADA.ResultMeasureValue[df$TADA.ResultMeasureValue > 0], na.rm = TRUE), 6),
        LimitType = rep(c("Upper", "Lower"), each = 3),
        SourceType = rep(c("Acute", "Chronic", "Other"), 2)
      )
      
      # Add dummy lines for legend
      p <- p + ggplot2::geom_line(
        data = legend_lines,
        ggplot2::aes(x = ActivityStartDate, y = Value, color = LimitType, linetype = SourceType),
        size = 1,
        show.legend = TRUE,
        inherit.aes = FALSE
      )
      
      # Manual legends
      p <- p +
        ggplot2::scale_color_manual(
          name = "Limit Type",
          values = c("Upper" = "red", "Lower" = "blue")
        ) +
        ggplot2::scale_linetype_manual(
          name = "Criteria Source",
          values = c("Acute" = "dotted", "Chronic" = "dashed", "Other" = "solid")
        )
      
      tadat$p_timeseries <- p
      return(p)
    })
    
    
  })
}
    
## To be copied in the UI
# mod_analysis_plots_ui("analysis_plots_1")
    
## To be copied in the server
# mod_analysis_plots_server("analysis_plots_1")
