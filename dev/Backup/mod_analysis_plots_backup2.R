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
      width = 4,
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
      width = 4,
      # Parameter selectize input (single selection)
      shiny::selectizeInput(
        inputId = ns("fraction_box_select"),
        label = "Select the fraction",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Select the fraction",
          create = FALSE
        )
      )
    ),
    column(
      width = 4,
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
    ),
  fluidRow(
    column(
      width = 4,
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
    ),
    column(
      width = 4,
      # Uses selectize input (single selection)
      shiny::selectizeInput(
        inputId = ns("unique_box_select"),
        label = "Select the unique criteria (Optional)",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Select the unique criteria",
          create = FALSE
        )
      )
    ),
    column(
      width = 4,
      # Uses selectize input (single selection)
      shiny::selectizeInput(
        inputId = ns("season_box_select"),
        label = "Select the season (Optional)",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Select the season",
          create = FALSE
        )
      )
    )
  ),
  fluidRow(
    column(width = 6, 
           plotOutput(ns("boxplot_view")),
           fluidRow(
             column(
               width = 4,
               radioButtons(
                 ns("boxplot_format"),
                 "Download format:",
                 choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                 selected = "png",
                 inline = TRUE
               )
             ),
             column(
               width = 4,
               downloadButton(ns("download_boxplot"), "Download boxplot",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )
           )
    ),
    column(width = 6, 
           plotOutput(ns("timeseries_view")),
           fluidRow(
             column(
               width = 4,
               radioButtons(
                 ns("timeseries_format"),
                 "Download format:",
                 choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                 selected = "png",
                 inline = TRUE
               )
             ),
             column(
               width = 4,
               downloadButton(ns("download_timeseries"), "Download time series plot",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             )
           )
    )
  )))
}
    
#' analysis_plots Server Functions
#'
#' @noRd 
mod_analysis_plots_server <- function(id, 
                                      # Excursion dataset
                                      excurse_dat, 
                                      # Excursion summary
                                      excurse_summary,
                                      # Location selection
                                      loc_select, 
                                      # Tab names
                                      tabname){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Set the initial state
    rv <- shiny::reactiveValues(
      unique_flag = FALSE,
      season_flag = FALSE,
      p_boxplot   = NULL,
      p_timeseries= NULL
    )
    
    # Disable the button
    shinyjs::disable("download_boxplot")
    shinyjs::disable("download_timeseries")
    
    # Disable the filters for UniquSpatialCriteria and Season
    shinyjs::disable("unique_box_select")
    shinyjs::disable("season_box_select")
    
    # Update selectize inputs when data changes
    shiny::observe({
      shiny::req(excurse_dat())
      shiny::req(nrow(excurse_dat()) > 0)
      shiny::req(excurse_summary())
      shiny::req(nrow(excurse_summary()) > 0)
      
      # Get unique values for Parameter dropdown
      param_choices <- sort(unique(excurse_summary()$TADA.CharacteristicName))
      
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
      shiny::req(excurse_dat())
      shiny::req(excurse_summary())
      shiny::req(input$parameter_box_select)
      
      # Filter by selected parameter
      dat2 <- excurse_dat() |>
        # Replace NA in TADA.ResultSampleFractionText for now
        tidyr::replace_na(list(TADA.ResultSampleFractionText = "None")) |>
        dplyr::filter(TADA.CharacteristicName %in% input$parameter_box_select)
      
      return(dat2)
    })
    
    shiny::observe({
      shiny::req(filtered_data1())
      
      # Get unique values for Fraction dropdown
      fraction_choices <- sort(unique(filtered_data1()$TADA.ResultSampleFractionText))
      
      # Update Uses selectize
      shiny::updateSelectizeInput(
        session = session,
        inputId = "fraction_box_select",
        choices = fraction_choices,
        selected = NULL,
        server = TRUE
      )
      
    })
    
    # Reactive to filter data based on selections
    filtered_data1_1 <- shiny::reactive({
      shiny::req(filtered_data1())
      
      # Filter by selected fraction
      dat2 <- filtered_data1() |>
        dplyr::filter(TADA.ResultSampleFractionText %in% input$fraction_box_select)
      
      return(dat2)
    })
    
    shiny::observe({
      shiny::req(filtered_data1_1())
      
      # Get unique values for Uses dropdown
      uses_choices <- sort(unique(filtered_data1_1()$ATTAINS.UseName))
      
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
      shiny::req(filtered_data1_1())
      
      # Filter by selected uses
      dat2 <- filtered_data1_1() |>
        dplyr::filter(ATTAINS.UseName %in% input$uses_box_select)
      
      return(dat2)
    })
    
    # Update the available location selection
    shiny::observe({
      shiny::req(filtered_data2())
      shiny::req(loc_select())
      if (loc_select() %in% c("MLid")){
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
      if (loc_select() %in% c("MLid")){
        dat2 <- filtered_data2() |>
          dplyr::filter(TADA.MonitoringLocationIdentifier %in% input$loc_box_select)
      } else {
        dat2 <- filtered_data2() |>
          dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% input$loc_box_select)
      }
      #add filtered_data3 - change 0 values to 0,000001
      dat2 <- dat2 |>
        dplyr::mutate(TADA.ResultMeasureValue = ifelse(TADA.ResultMeasureValue == 0,
                                                       0.000001, TADA.ResultMeasureValue))
      
      return(dat2)
    })
    
    # Optional filters in UniqueSpatialCriteria and Season

    # Check if there are multiple values in UniqueSpatialCriteria in filtered_data3
    shiny::observe({
      shiny::req(filtered_data3)

      # Get unique values for unique dropdown
      unique_choices <- sort(unique(filtered_data3()$UniqueSpatialCriteria))

      rv$unique_flag <- any(!is.na(unique_choices))

      shinyjs::toggleState(id = "unique_box_select",
                           condition = rv$unique_flag)

      if (rv$unique_flag){
        # Update Uses selectize
        shiny::updateSelectizeInput(
          session = session,
          inputId = "unique_box_select",
          choices = unique_choices,
          selected = NULL,
          server = TRUE
        )
      } else {
        # Update Uses selectize
        shiny::updateSelectizeInput(
          session = session,
          inputId = ns("unique_box_select"),
          label = "Select the unique criteria (Optional)",
          choices = NULL,
          selected = NULL,
          options = list(
            placeholder = "Select the unique criteria",
            create = FALSE
          )
        )
      }
    })

    
    # Check if there are multiple values in Season in filtered_data3
    shiny::observe({
      shiny::req(filtered_data3())

      # Get unique values for unique dropdown
      season_choices <- sort(unique(filtered_data3()$Season))

      rv$season_flag <- any(!is.na(season_choices))

      shinyjs::toggleState(id = "season_box_select",
                           condition = rv$season_flag)

      if (rv$season_flag){
        # Update Uses selectize
        shiny::updateSelectizeInput(
          session = session,
          inputId = "season_box_select",
          choices = season_choices,
          selected = NULL,
          server = TRUE
        )
      } else {
        # Update Uses selectize
        shiny::updateSelectizeInput(
          session = session,
          inputId = ns("season_box_select"),
          label = "Select the season (Optional)",
          choices = NULL,
          selected = NULL,
          options = list(
            placeholder = "Select the season",
            create = FALSE
          )
        )
      }
    })
    
    # Filter filtered_data3 based on UniqueSpatialCriteria and Season if needed
    filtered_data4 <- shiny::reactive({
      shiny::req(filtered_data3())
      
      if (isTRUE(rv$unique_flag)){
        dat2 <- filtered_data3() |>
          dplyr::filter(UniqueSpatialCriteria %in% input$unique_box_select)
      } else {
        dat2 <- filtered_data3()
      }
      
      if (isTRUE(rv$season_flag)){
        dat3 <- dat2 |>
          dplyr::filter(Season %in% input$season_box_select)
      } else {
        dat3 <- dat2
      }

      return(dat3)
    })

    # Create boxplot
    output$boxplot_view <- shiny::renderPlot({
      shiny::req(filtered_data4())

      options(scipen=999)
      
      # Check if there's data to plot
      if(nrow(filtered_data4()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", 
             cex = 1.2, col = "gray50")
        return(NULL)
      }
      

      p <- ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = filtered_data4(),
                              ggplot2::aes(x = ATTAINS.UseName,
                                           y = TADA.ResultMeasureValue),
                              color = 'gray30',
                              outlier.shape = NA) 
      
      if (loc_select() %in% c("MLid")){
        rv$p_boxplot <- p + ggplot2::geom_jitter(data = filtered_data4(), ggplot2::aes(x = ATTAINS.UseName,
                                                                            y = TADA.ResultMeasureValue
                                                                            , fill = TADA.MonitoringLocationIdentifier),
                                      color = 'black',
                                      shape = 21,
                                      size = 3.5,
                                      width = 0.2,
                                      alpha = 0.8) +
          ggplot2::xlab('Uses') +
          ggplot2::ylab(paste0(unique(filtered_data4()$TADA.CharacteristicName), 
                               ' (', filtered_data4()$TADA.ResultMeasure.MeasureUnitCode, ')')) +
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
      } else {
        rv$p_boxplot <- p + ggplot2::geom_jitter(data = filtered_data4(), ggplot2::aes(x = ATTAINS.UseName,
                                                                            y = TADA.ResultMeasureValue
                                                                            , fill = JoinToAU.AssessmentUnitIdentifier),
                                      color = 'black',
                                      shape = 21,
                                      size = 3.5,
                                      width = 0.2,
                                      alpha = 0.8) +
          ggplot2::xlab('Uses') +
          ggplot2::ylab(paste0(filtered_data4()$TADA.CharacteristicName,
                               ' (', filtered_data4()$TADA.ResultMeasure.MeasureUnitCode, ')')) +
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
      }
      return(rv$p_boxplot)
    })
    
    
    output$timeseries_view <- shiny::renderPlot({
      shiny::req(filtered_data4())

      options(scipen = 999)
      
      if (nrow(filtered_data4()) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data available for selected filters",
                       cex = 1.2, col = "gray50")
        return(NULL)
      }
      
      df <- filtered_data4()
      
      fill_var <- if (loc_select() %in% c("MLid")) {
        df$TADA.MonitoringLocationIdentifier
      } else {
        df$JoinToAU.AssessmentUnitIdentifier
      }
      
      # Base plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = ActivityStartDate, y = TADA.ResultMeasureValue)) +
        ggplot2::geom_point(ggplot2::aes(fill = fill_var),
                            color = "black", shape = 21, size = 3.5, alpha = 0.8) +
        ggplot2::scale_y_log10() +
        ggplot2::xlab("Time") +
        ggplot2::ylab(paste0(filtered_data4()$TADA.CharacteristicName[1], " (", filtered_data4()$TADA.ResultMeasure.MeasureUnitCode[1], ")")) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = TRUE, option = "mako") +
        ggplot2::labs(fill = if (loc_select() %in% c("MLid")) {
          "Monitoring Location ID"
        } else {
          "Assessment Unit ID"
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
      
      rv$p_timeseries <- p
      
      return(p)
    })
    
    ### Download the plots
    
    # Activate the download button if plots are available
    shiny::observe({
      shinyjs::toggleState("download_boxplot", condition = !is.null(rv$p_boxplot))
      shinyjs::toggleState("download_timeseries", condition = !is.null(rv$p_timeseries))
    })
    
    output$download_boxplot <- downloadHandler(
      filename = function() {
        paste0(tabname, "_boxplot", ".", input$boxplot_format)
      },
      content = function(file) {
        if (!is.null(rv$p_boxplot)) {
          ggplot2::ggsave(
            file, 
            plot = rv$p_boxplot, 
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
        paste0(tabname, "_time_series", ".", input$timeseries_format)
      },
      content = function(file) {
        if (!is.null(rv$p_timeseries)) {
          ggplot2::ggsave(
            file, 
            plot = rv$p_timeseries, 
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
