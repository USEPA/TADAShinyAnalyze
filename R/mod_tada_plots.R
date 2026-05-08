# modules/mod_tada_plots.R
# ------------------------
# Shiny module for TADA plots (filtered by user selections).
# Dependencies: EPATADA, plotly, dplyr, shiny, htmltools

mod_tada_plots_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::h4("TADA Plots: Monitoring Location or Assessment Unit"),
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
# - df_mlid_input: reactive() that returns the input WQP dataframe (tadat$df_mlid_input)
# - user_choice: reactive() that returns selected TADA.ComparableDataIdentifier (vector)
# - user_choice_ML: reactive() that returns selected MonitoringLocationIdentifier (vector)
mod_tada_plots_server <- function(id,
                                  df_mlid_input,
                                  user_choice,
                                  user_choice_ML) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Boxplots filtered by MonitoringLocationIdentifier and TADA.ComparableDataIdentifier
    output$TADA_boxplots_view_filtered <- plotly::renderPlotly({
      # Validate that data exists
      shiny::validate(
        shiny::need(!is.null(df_mlid_input()), "No file selected.")
      )
      
      # Filter by user selections
      df_filtered <- df_mlid_input() |>
        dplyr::filter(
          .data$TADA.ComparableDataIdentifier %in% user_choice(),
          .data$MonitoringLocationIdentifier %in% user_choice_ML()
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
        margin = 0.05
      )
    })
    
    # Time series / scatterplot filtered by MonitoringLocationIdentifier and ComparableDataIdentifier
    output$TADA_timeseries_view_filtered <- plotly::renderPlotly({
      # Validate that data exists
      shiny::validate(
        shiny::need(!is.null(df_mlid_input()), "No file selected.")
      )
      
      # Filter by user selections
      df_filtered <- df_mlid_input() |>
        dplyr::filter(
          .data$TADA.ComparableDataIdentifier %in% user_choice(),
          .data$MonitoringLocationIdentifier %in% user_choice_ML()
        )
      
      shiny::validate(
        shiny::need(nrow(df_filtered) > 0, "Your selection(s) returned an empty data.frame.")
      )
      
      # Return the grouped scatterplot
      EPATADA::TADA_GroupedScatterplot(df_filtered)
    })
  })
}
