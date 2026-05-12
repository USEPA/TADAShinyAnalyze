# modules/mod_tada_plots.R
# ------------------------
# Shiny module for TADA plots with in-module selection:
# - Single parameter (TADA.ComparableDataIdentifier)
# - Up to 4 monitoring locations
# Boxplots display MonitoringLocationName on the x-axis (fallback to ID).
# Dependencies: EPATADA, plotly, dplyr, shiny, htmltools

mod_tada_plots_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::h4("TADA Plots: Monitoring Location or Assessment Unit"),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectizeInput(
          inputId = ns("param_id"),
          label   = "Select a parameter (single):",
          choices = NULL,
          multiple = FALSE,
          options  = list(placeholder = "Choose a parameter")
        ),
        shiny::selectizeInput(
          inputId = ns("ml_ids"),
          label   = "Select monitoring locations (up to 4):",
          choices = NULL,
          multiple = TRUE,
          options  = list(maxItems = 4, placeholder = "Choose up to 4 locations")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        plotly::plotlyOutput(ns("TADA_boxplots_view_filtered"))
      ),
      shiny::column(
        width = 6,
        plotly::plotlyOutput(ns("TADA_timeseries_view_filtered"))
      )
    )
  )
}

# Arguments:
# - data_react: reactive expression or data.frame that returns the input WQP dataframe
mod_tada_plots_server <- function(id, data_react) {
  if (missing(data_react)) {
    stop("mod_tada_plots_server: 'data_react' is required.")
  }
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Normalize data source to a reactive getter
    force(data_react)
    get_data <- if (is.function(data_react)) {
      function() data_react()
    } else {
      shiny::reactive(data_react)
    }
    
    # Populate selectors whenever new data arrives
    shiny::observeEvent(get_data(), {
      df <- get_data()
      if (is.null(df) || !nrow(df)) {
        return(invisible(NULL))
      }
      
      # Parameter choices (single)
      params <- sort(unique(df$TADA.ComparableDataIdentifier))
      shiny::updateSelectizeInput(
        session = session,
        inputId = "param_id",
        choices = params,
        selected = if (!is.null(input$param_id) &&
                       input$param_id %in% params) {
          input$param_id
        } else {
          params[1]
        },
        server = TRUE
      )
      
      # Monitoring location choices: label = "Name (ID)" or just ID if name missing
      ml_tbl <- df |>
        dplyr::distinct(
          .data$MonitoringLocationIdentifier,
          .data$MonitoringLocationName,
          .keep_all = FALSE
        ) |>
        dplyr::arrange(
          .data$MonitoringLocationName,
          .data$MonitoringLocationIdentifier
        )
      
      ids <- ml_tbl$MonitoringLocationIdentifier
      labels <- if ("MonitoringLocationName" %in% names(ml_tbl)) {
        nm <- ml_tbl$MonitoringLocationName
        nm <- ifelse(is.na(nm) | nm == "", ids, nm)
        paste0(nm, " (", ids, ")")
      } else {
        ids
      }
      
      choices <- stats::setNames(ids, labels)
      
      current_sel <- if (is.null(input$ml_ids)) character(0) else input$ml_ids
      current_sel <- intersect(current_sel, ids)
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "ml_ids",
        choices = choices,
        selected = current_sel,
        server = TRUE
      )
    })
    
    # Boxplots filtered by MonitoringLocationIdentifier and TADA.ComparableDataIdentifier
    output$TADA_boxplots_view_filtered <- plotly::renderPlotly({
      # Validate that data exists
      shiny::validate(
        shiny::need(!is.null(df), "No file selected.")
      )
      
      df <- get_data()
      ml_sel <- input$ml_ids
      if (length(ml_sel) > 4) {
        ml_sel <- ml_sel[seq_len(4)]
      }
      
      # Filter by user selections
      df_filtered <- df |>
        dplyr::filter(
          .data$TADA.ComparableDataIdentifier == input$param_id,
          .data$MonitoringLocationIdentifier %in% ml_sel
        )
      
      shiny::validate(
        shiny::need(nrow(df_filtered) > 0, "Your selection(s) returned an empty data.frame.")
      )
      
      # Build a subplot of boxplots, one per monitoring location
      ml_ids <- unique(df_filtered$MonitoringLocationIdentifier)
      n_panels <- length(ml_ids)
      
      plot_list <- vector("list", n_panels)
      
      for (i in seq_len(n_panels)) {
        df_i <- df_filtered |>
          dplyr::filter(.data$MonitoringLocationIdentifier %in% ml_ids[i])
        
        plt <- suppressWarnings(
          EPATADA::TADA_Boxplot(
            df_i,
            id_cols = "MonitoringLocationIdentifier"
          )
        )
        
        plt <- plt |>
          plotly::add_trace(
            boxmean = TRUE,
            showlegend = FALSE
          ) |>
          plotly::layout(
            xaxis = list(title = unique(df_i$MonitoringLocationIdentifier)),
            title = paste(
              "Boxplots by MonitoringLocationIdentifier",
              unique(df_i$TADA.ComparableDataIdentifier)[1]
            ),
            showlegend = FALSE
          )
        
        plot_list[[i]] <- plt
      }
      
      plotly::subplot(
        plot_list,
        shareY = TRUE,
        margin = 0.05,
        titleX = TRUE
      )
    })
    
    # Time series / scatterplot filtered by selected parameter and locations
    output$TADA_timeseries_view_filtered <- plotly::renderPlotly({
      shiny::validate(
        shiny::need(!is.null(get_data()), "No file selected."),
        shiny::need(!is.null(input$param_id), "Please select a parameter."),
        shiny::need(length(input$ml_ids) >= 1,
                    "Please select at least one monitoring location (up to 4).")
      )
      
      df <- get_data()
      ml_sel <- input$ml_ids
      if (length(ml_sel) > 4) {
        ml_sel <- ml_sel[seq_len(4)]
      }
      
      df_filtered <- df |>
        dplyr::filter(
          .data$TADA.ComparableDataIdentifier == input$param_id,
          .data$MonitoringLocationIdentifier %in% ml_sel
        )
      
      shiny::validate(
        shiny::need(nrow(df_filtered) > 0,
                    "Your selection(s) returned an empty data.frame.")
      )
      
      if (length(ml_sel) == 1) {
        p <- EPATADA::TADA_Scatterplot(df_filtered)
      } else {
        p <- EPATADA::TADA_GroupedScatterplot(df_filtered)
      }

      # # Handle different return types from TADA_GroupedScatterplot
      # if (inherits(p, "plotly")) {
      #   p
      # } else if (inherits(p, "gg") || inherits(p, "ggplot")) {
      #   plotly::ggplotly(p)
      # } else if (is.list(p)) {
      #   # Attempt to convert list of plots to plotly and combine
      #   pl_list <- lapply(p, to_plotly)
      #   pl_list <- Filter(Negate(is.null), pl_list)
      #   shiny::validate(
      #     shiny::need(length(pl_list) > 0,
      #                 "No plottable object returned by TADA_GroupedScatterplot().")
      #   )
      #   if (length(pl_list) == 1) {
      #     pl_list[[1]]
      #   } else {
      #     plotly::subplot(pl_list, nrows = length(pl_list), shareX = TRUE, titleY = TRUE)
      #   }
      # } else {
      #   stop("TADA_GroupedScatterplot returned an unsupported object type.")
      # }
    })
  })
}

