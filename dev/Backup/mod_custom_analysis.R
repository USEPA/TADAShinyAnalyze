#' custom_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_custom_analysis_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("3. Custom Analysis"),
    
    # Components
    fluidRow(
      column(
        width = 12,
        mod_analysis_selector_custom_ui(ns("Custom_Select"))
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          mod_map_table_selector_custom_ui(ns("Custom_map_table_selector"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          shiny::checkboxInput(inputId = ns("custom_group"),
                               label = "Group the ML/AU ID for analysis"),
          shiny::selectizeInput(inputId = ns("parameter_filter_custom"),
                                label = "Filter parameter to view the results",
                                choices = NULL,
                                multiple = TRUE),
          shiny::actionButton(ns("Run_Custom"), "Run Custom Analysis", shiny::icon("computer"),
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        mod_exceedance_viewer_custom_ui(ns("Summary_View_Custom"))
      )
    ),
    # Add to your UI
    fluidRow(
      column(12,
             tabsetPanel(
               tabPanel("Overall Status", 
                        leaflet::leafletOutput(ns("overall_map"))
               ),
               tabPanel("By Use", 
                        shiny::selectInput(ns("selected_use"), "Select Use:", 
                                    choices = NULL),
                        leaflet::leafletOutput(ns("use_map"))
               ),
               tabPanel("By Parameter", 
                        shiny::selectInput(ns("selected_param"), "Select Parameter:", 
                                           choices = NULL),
                        shiny::selectInput(ns("selected_use_param"), "Select Use:", 
                                           choices = NULL),
                        leaflet::leafletOutput(ns("param_map"))
               )
             )
      )
    )
  )
}
    
#' custom_analysis Server Functions
#'
#' @noRd 
mod_custom_analysis_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    mod_analysis_selector_custom_server("Custom_Select", tadat)
    
    ### If the input data are ready, conduct the analysis
    shiny::observe({
      shiny::req(tadat$df_mlid_input, tadat$df_mltoau_input_f, tadat$df_autouse_input,
                 tadat$loc_select_custom, 
                 tadat$state_tribe_custom, 
                 tadat$uses_select_re_custom)
      
      ### Get the input data and convert ActivityStartDateTime to dateTime
      dat <- tadat$df_mlid_input
      dat <- dat |>
        dplyr::mutate(ActivityStartDateTime = lubridate::ymd_hms(ActivityStartDateTime)) |>
        dplyr::mutate(ActivityStartDate = lubridate::ymd(ActivityStartDate)) |>
        dplyr::mutate(DateTime = ActivityStartDateTime)
      
      ### Step 1: Join pH, Temperature, and Hardness data
      dat2 <- dat |> 
        pH_fun() |>
        Temperature_fun() |>
        hardness_fun()
      
      ### Step 2: Join the criteria table
      criteria_table_f1 <- criteria_table |>
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$state_tribe_custom) |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re_custom)
      
      # Filter the AU_Use based on available_uses_s
      AU_Use <- tadat$df_autouse_input
      AU_MLID <- tadat$df_mltoau_input_f |>
        dplyr::mutate(TADA.MonitoringLocationIdentifier = 
                        stringr::str_to_upper(MonitoringLocationIdentifier))
      
      AU_Use_f1 <- AU_Use |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re_custom)
      
      # Filter the AU_MLID based on AU_Use_f1
      AU_MLID_f1 <- AU_MLID |>
        dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% 
                        AU_Use_f1$JoinToAU.AssessmentUnitIdentifier)
      
      # Filter the input data based on AU_MLID_f1
      dat3 <- dat2 |>
        dplyr::filter(TADA.MonitoringLocationIdentifier %in% 
                        AU_MLID_f1$TADA.MonitoringLocationIdentifier)
      
      # Join the criteria_table_f1 and AU_MLID_f1 to dat2
      dat4 <- dat3 |>
        dplyr::filter(TADA.CharacteristicName %in% 
                        unique(criteria_table_f1$TADA.CharacteristicName)) |>
        dplyr::left_join(AU_MLID_f1) |>
        dplyr::left_join(AU_Use_f1, 
                         by = "JoinToAU.AssessmentUnitIdentifier",
                         relationship = "many-to-many") |>
        criteria_join(criteria_table_f1) |>
        dplyr::filter(!is.na(EquationBased))
      
      # Save the data
      tadat$custom_raw <- dat4 
      
      # Create a table for the map-table selector
      site_AU_table <- dat4 |>
        dplyr::distinct(TADA.MonitoringLocationIdentifier,
                        TADA.MonitoringLocationName,
                        TADA.MonitoringLocationTypeName,
                        TADA.LongitudeMeasure,
                        TADA.LatitudeMeasure,
                        JoinToAU.AssessmentUnitIdentifier)
      
      tadat$site_AU_table_custom <- site_AU_table
      
    })
    
    ### Based on tadat$site_AU_table_custom activate the map-table selector
    mod_map_table_selector_custom_server("Custom_map_table_selector", tadat)
    
    # Filter tadat$custom_raw based on selected monitoring locations
    shiny::observeEvent(tadat$selected_monitoring_locations_custom, {
      req(tadat$custom_raw)
      
      # Get selected monitoring locations
      selected_sites <- tadat$selected_monitoring_locations_custom
      
      # If no sites selected, use all sites
      if (is.null(selected_sites) || length(selected_sites) == 0) {
        tadat$custom_raw2 <- tadat$custom_raw
      } else {
        # Filter the data based on selected sites
        tadat$custom_raw2 <- tadat$custom_raw |>
          dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_sites)
      }
      
      # Update parameter filter choices based on filtered data
      if (!is.null(tadat$custom_raw2) && nrow(tadat$custom_raw2) > 0) {
        available_params <- unique(tadat$custom_raw2$TADA.CharacteristicName)
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter_custom",
          choices = sort(available_params),
          selected = NULL
        )
      }
    }, ignoreNULL = FALSE)

    # Update tadat$custom_raw3 based on parameter_filter_custom
    shiny::observeEvent(input$parameter_filter_custom, {
      req(tadat$custom_raw2)
      
      # Check if any parameters are selected
      if (is.null(input$parameter_filter_custom) || length(input$parameter_filter_custom) == 0) {
        # If no parameters selected, use all data
        tadat$custom_raw3 <- tadat$custom_raw2
      } else {
        # Filter based on selected parameters
        tadat$custom_raw3 <- tadat$custom_raw2 |>
          dplyr::filter(TADA.CharacteristicName %in% input$parameter_filter_custom)
      }
      
    }, ignoreNULL = FALSE)

    ### Run the analysis if tadat$custom_raw3 is ready
    shiny::observeEvent(input$Run_Custom, {
      req(tadat$custom_raw3)

      dat4 <- tadat$custom_raw3

      ### Step 3: Separate the dataset based on if criteria exist
      # dat_na <- dat4 |> dplyr::filter(is.na(EquationBased))
      dat_yes <- dat4 |> dplyr::filter(EquationBased %in% "Yes")
      dat_no <- dat4 |> dplyr::filter(EquationBased %in% "No")

      ### Step 4: Compare the dataset that the condition is not based on equation
      dat_no2 <- dat_no |> exceedance_fun()

      # Combine the results from each cases
      # TODO Need to make sure all the cases have the same column headers
      dat5 <- dplyr::bind_rows(dat_no2)

      ### Step 6: Summarize the data
      dat6 <- dat5 |>
        exceedance_summary(type = tadat$loc_select_custom, group = input$custom_group) 
      
      dat6a <- dat6 |> purrr::pluck("data")
      
      dat6b <- dat6 |> purrr::pluck("coords")

      # Save the data to tadat
      tadat$exceed_summary_custom <- dat6a
      tadat$exceed_summary_coords_custom <- dat6b
    })
    mod_exceedance_viewer_custom_server("Summary_View_Custom", tadat)
    
    # Render the summary maps
    # Render the summary maps
    output$overall_map <- leaflet::renderLeaflet({
      req(tadat$exceed_summary_custom)
      req(!input$custom_group)
      
      create_overall_map(
        data = tadat$exceed_summary_custom,
        coords_data = tadat$exceed_summary_coords_custom,
        group_by = tadat$loc_select_custom
      )
    })
    
    output$use_map <- leaflet::renderLeaflet({
      req(tadat$exceed_summary_custom)
      req(!input$custom_group)
      
      create_use_map(
        data = tadat$exceed_summary_custom,
        coords_data = tadat$exceed_summary_coords_custom,
        selected_use = input$selected_use,
        group_by = tadat$loc_select_custom
      )
    })
    
    output$param_map <- leaflet::renderLeaflet({
      req(tadat$exceed_summary_custom)
      req(input$selected_param)
      req(input$selected_use_param)
      req(!input$custom_group)
      
      # Filter data to check if there are any results
      filtered_data <- tadat$exceed_summary_custom |>
        dplyr::filter(TADA.CharacteristicName == input$selected_param,
                      ATTAINS.UseName == input$selected_use_param)
      
      # Only create map if filtered data has rows
      if (nrow(filtered_data) > 0) {
        create_parameter_map(
          data = tadat$exceed_summary_custom,
          coords_data = tadat$exceed_summary_coords_custom,
          selected_param = input$selected_param,
          selected_use = input$selected_use_param,
          group_by = tadat$loc_select_custom
        )
      } else {
        # Return empty leaflet map with message
        leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::addControl(
            html = "<div style='padding: 20px; background: white; border-radius: 5px;'>
                <h4>No data available</h4>
                <p>No results found for the selected parameter and use combination.</p>
                </div>",
            position = "topright"
          )
      }
    })
    
    # Update dropdown choices
    observe({
      req(tadat$exceed_summary_custom)
      req(!input$custom_group)
      
      uses <- sort(unique(tadat$exceed_summary_custom$ATTAINS.UseName))  
      params <- sort(unique(tadat$exceed_summary_custom$TADA.CharacteristicName))
      uses_with_all <- uses 
      
      updateSelectInput(session, "selected_use", choices = uses, selected = uses[1])
      updateSelectInput(session, "selected_param", choices = params, selected = params[1])
      updateSelectInput(session, "selected_use_param", choices = uses, selected = uses[1])
    })

  })
  
  
}
    
## To be copied in the UI
# mod_custom_analysis_ui("custom_analysis_1")
    
## To be copied in the server
# mod_custom_analysis_server("custom_analysis_1")
