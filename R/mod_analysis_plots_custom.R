#' analysis_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_plots_custom_ui <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    column(
      width = 6,
      # Parameter selectize input (single selection)
      shiny::selectizeInput(
        inputId = ns("parameter_box_select_custom"),
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
        inputId = ns("uses_box_select_custom"),
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
        inputId = ns("loc_box_select_custom"),
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
    column(width = 6, 
           plotOutput(ns("boxplot_view_custom")),
           fluidRow(
             column(
               width = 4,
               radioButtons(
                 ns("boxplot_format_custom"),
                 "Download format:",
                 choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                 selected = "png",
                 inline = TRUE
               )
             ),
             column(
               width = 4,
               downloadButton(ns("download_boxplot_custom"), "Download boxplot",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )
           )
    ),
    column(width = 6, 
           plotOutput(ns("timeseries_view_custom")),
           fluidRow(
             column(
               width = 4,
               radioButtons(
                 ns("timeseries_format_custom"),
                 "Download format:",
                 choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                 selected = "png",
                 inline = TRUE
               )
             ),
             column(
               width = 4,
               downloadButton(ns("download_timeseries_custom"), "Download time series plot",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             )
           )
    )
  )
  )
}
    
#' analysis_plots Server Functions
#'
#' @noRd 
mod_analysis_plots_custom_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Disable the button
    shinyjs::disable("download_boxplot_custom")
    shinyjs::disable("download_timeseries_custom")
 
    # Update selectize inputs when data changes
    shiny::observe({
      shiny::req(tadat$excurse_dat_custom_filtered)
      shiny::req(nrow(tadat$excurse_dat_custom_filtered) > 0)
      shiny::req(tadat$exceed_summary_custom)
      shiny::req(nrow(tadat$exceed_summary_custom) > 0)
      
      # Get unique values for Parameter dropdown
      param_choices <- sort(unique(tadat$exceed_summary_custom$TADA.CharacteristicName))
      
      # Update Parameter selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "parameter_box_select_custom",
        choices = param_choices,
        selected = if(length(param_choices) > 0) param_choices[1] else NULL,
        server = TRUE
      )
    })
    
    # Observe changes to loc_select_custom to enable/disable uses dropdown
    shiny::observe({
      shiny::req(tadat$loc_select_custom)
      
      if (tadat$loc_select_custom %in% "CG") {
        # Disable the uses dropdown using shinyjs
        shinyjs::disable("loc_box_select_custom")
      } else {
        # Enable the uses dropdown using shinyjs
        shinyjs::enable("loc_box_select_custom")
      }
    })
    
    # Reactive to filter data based on selections
    filtered_data1 <- shiny::reactive({
      shiny::req(tadat$excurse_dat_custom_filtered)
      shiny::req(tadat$exceed_summary_custom)
      shiny::req(input$parameter_box_select_custom)
      
      # Filter by selected parameter
      dat2 <- tadat$excurse_dat_custom_filtered |>
        dplyr::filter(TADA.CharacteristicName %in% input$parameter_box_select_custom)
      
      return(dat2)
    })
    
    shiny::observe({
      shiny::req(filtered_data1())
      
      # Get unique values for Uses dropdown
      uses_choices <- sort(unique(filtered_data1()$ATTAINS.UseName))
      
      # Update Uses selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "uses_box_select_custom",
        choices = uses_choices,
        selected = NULL,
        server = TRUE
      )
      
    })
    
    
    # Reactive to filter data based on selections
    filtered_data2 <- shiny::reactive({
      shiny::req(filtered_data1())
      shiny::req(input$uses_box_select_custom)
      
      # Filter by selected uses
      dat2 <- filtered_data1() |>
        dplyr::filter(ATTAINS.UseName %in% input$uses_box_select_custom)
      
      return(dat2)
    })
    
    # Update the available location selection
    shiny::observe({
      shiny::req(filtered_data2())
      shiny::req(tadat$loc_select_custom)
      
      if (tadat$loc_select_custom %in% "CG") {
        # For Custom Grouping, clear the choices and selection
        loc_choices <- NULL
        selected_loc <- NULL
      } else if (tadat$loc_select_custom %in% "MLid") {
        loc_choices <- sort(unique(filtered_data2()$TADA.MonitoringLocationIdentifier))
        selected_loc <- if(length(loc_choices) > 0) loc_choices[1] else NULL
      } else if (tadat$loc_select_custom %in% "AU") {
        loc_choices <- sort(unique(filtered_data2()$JoinToAU.AssessmentUnitIdentifier))
        selected_loc <- if(length(loc_choices) > 0) loc_choices[1] else NULL
      }
      
      # Update location selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "loc_box_select_custom",
        choices = loc_choices,
        selected = if(length(loc_choices) > 0) loc_choices[1] else NULL,
        server = TRUE
      )
      
    })
    
    # Reactive to filter data based on location selections
    filtered_data3 <- shiny::reactive({
      shiny::req(filtered_data2())
      
      # For Custom Grouping, don't require location selection
      if (tadat$loc_select_custom %in% "CG") {
        dat2 <- filtered_data2()
      } else {
        # For MLid and AU, require location selection
        shiny::req(input$loc_box_select_custom)
        
        if (tadat$loc_select_custom %in% "MLid") {
          dat2 <- filtered_data2() |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% input$loc_box_select_custom)
        } else if (tadat$loc_select_custom %in% "AU") {
          dat2 <- filtered_data2() |>
            dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% input$loc_box_select_custom)
        }
      }
      
      # Change 0 values to 0.000001
      dat2 <- dat2 |>
        dplyr::mutate(TADA.ResultMeasureValue = ifelse(TADA.ResultMeasureValue == 0,
                                                       0.000001, TADA.ResultMeasureValue))
      
      return(dat2)
    })
    

    # Create boxplot
    output$boxplot_view_custom <- shiny::renderPlot({
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
      
      if (tadat$loc_select_custom %in% "MLid"){
        tadat$p_boxplot_custom <- p + ggplot2::geom_jitter(data = filtered_data3(), ggplot2::aes(x = ATTAINS.UseName,
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
                         , text = ggplot2::element_text(size = 14)
                         , axis.text = ggplot2::element_text(size = 12)
                         , legend.background = ggplot2::element_rect(colour = 'gray60', fill = 'white', linetype='dashed'))
      } else if (tadat$loc_select_custom %in% "AU"){
        tadat$p_boxplot_custom <- p + ggplot2::geom_jitter(data = filtered_data3(), ggplot2::aes(x = ATTAINS.UseName,
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
                         , text = ggplot2::element_text(size = 14)
                         , axis.text = ggplot2::element_text(size = 12)
                         , legend.background = ggplot2::element_rect(colour = 'gray60', fill = 'white', linetype='dashed'))
      } else if (tadat$loc_select_custom %in% "CG"){
        # For CG, just show points without fill grouping
        tadat$p_boxplot_custom <- p + 
          ggplot2::geom_jitter(data = filtered_data3(), 
                               ggplot2::aes(x = ATTAINS.UseName,
                                            y = TADA.ResultMeasureValue),
                               color = 'darkblue',
                               shape = 21,
                               fill = 'lightblue',
                               size = 3.5,
                               width = 0.2,
                               alpha = 0.8) +
          ggplot2::xlab('Uses') +
          ggplot2::ylab(paste0(unique(filtered_data2()$TADA.CharacteristicName), 
                               ' (', filtered_data2()$TADA.ResultMeasure.MeasureUnitCode[1], ')')) +
          ggplot2::scale_y_log10() +
          ggplot2::theme_bw() +
          ggplot2::theme(text = ggplot2::element_text(size = 14),
                         axis.text = ggplot2::element_text(size = 12))
      }
      return(tadat$p_boxplot_custom)
    })
    
    
    output$timeseries_view_custom <- shiny::renderPlot({
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
      
      if (tadat$loc_select_custom %in% "MLid") {
        df$fill_var <- df$TADA.MonitoringLocationIdentifier
      } else if (tadat$loc_select_custom %in% "AU"){
        df$fill_var <- df$JoinToAU.AssessmentUnitIdentifier
      } else {
        df$fill_var <- "Custom Group"
      }
      
      # Base plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = ActivityStartDate, y = TADA.ResultMeasureValue)) +
        ggplot2::geom_point(ggplot2::aes(fill = fill_var),
                            color = "black", shape = 21, size = 3.5, alpha = 0.8) +
        ggplot2::scale_y_log10() +
        ggplot2::xlab("Time") +
        ggplot2::ylab(paste0(filtered_data2()$TADA.CharacteristicName[1], " (", filtered_data2()$TADA.ResultMeasure.MeasureUnitCode[1], ")")) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = TRUE, option = "mako") +
        ggplot2::labs(fill = if (tadat$loc_select_custom %in% "MLid") {
          "Monitoring Location ID"
        } else if (tadat$loc_select_custom %in% "AU"){
          "Assessment Unit ID"
        } else {
          "Custom Group"
        }) +
        ggplot2::theme(
          legend.position = "right",
          text = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 12),
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
            ggplot2::aes(x = .data[["ActivityStartDate"]], 
                         y = .data[[value_col]]),
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
              ggplot2::aes(x = .data[["ActivityStartDate"]], 
                           y = .data[[value_col]]),
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
        linewidth = 1,
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
      
      tadat$p_timeseries_custom <- p
      return(p)
    })
    
    ### Download the plots
    
    # Activate the download button if plots are available
    shiny::observe({
      shinyjs::toggleState("download_boxplot_custom", condition = !is.null(tadat$p_boxplot_custom))
      shinyjs::toggleState("download_timeseries_custom", condition = !is.null(tadat$p_timeseries_custom))
    })
    
    output$download_boxplot <- downloadHandler(
      filename = function() {
        paste0("custom_boxplot", ".", input$boxplot_format)
      },
      content = function(file) {
        if (!is.null(tadat$p_boxplot)) {
          ggplot2::ggsave(
            file, 
            plot = tadat$p_boxplot, 
            width = 10, 
            height = 6, 
            dpi = 300,
            units = "in",
            device = input$boxplot_format
          )
        }
      }
    )
    
    output$download_timeseries <- downloadHandler(
      filename = function() {
        paste0("custom_time_series", ".", input$timeseries_format)
      },
      content = function(file) {
        if (!is.null(tadat$p_timeseries)) {
          ggplot2::ggsave(
            file, 
            plot = tadat$p_timeseries, 
            width = 10, 
            height = 6, 
            dpi = 300,
            units = "in",
            device = input$timeseries_format
          )
        }
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_analysis_plots_ui("analysis_plots_1")
    
## To be copied in the server
# mod_analysis_plots_server("analysis_plots_1")
