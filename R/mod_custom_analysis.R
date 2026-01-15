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
    htmltools::h2("4. Custom Analysis"),
    
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
        mod_map_table_selector_custom_ui(ns("Custom_map_table_selector"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        shiny::selectizeInput(inputId = ns("parameter_filter_custom"),
                              label = "Filter parameter to view the results",
                              choices = NULL,
                              multiple = TRUE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        htmltools::h4("After finalizing the selections, click the 'Run Custom Analysis' button."),
        shinyjs::disabled(
          shiny::actionButton(ns("Run_Custom"), "Run Custom Analysis", shiny::icon("computer"),
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
        )
      )
    ),
    
    htmltools::br(),

    fluidRow(
      column(
        width = 12,
        htmltools::h4("Download the custom analysis results by clicking the 'Download Custom Results' button."),
        shinyjs::disabled(shiny::downloadButton(
          outputId = ns("download_results_custom"),
          label = "Download Custom Results (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4") # download button
        )
      )
    ),
    
    # Horizontal divider
    htmltools::hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    fluidRow(
      column(
        width = 12,
        htmltools::h3("Summary Table"),
        mod_excursion_viewer_ui(ns("Summary_View_Custom"))
      )
    ),
    # Add to your UI
    fluidRow(
      column(12,
             htmltools::h3("Summary Maps"),
             htmltools::p("Use the maps to view the exceedance results with different levels."),
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
    ),
    fluidRow(
      column(
        width = 12,
        htmltools::h3("Plots"),
        htmltools::p("Use filters to view the results"),
        mod_analysis_plots_ui(ns("Analysis_Plots_Custom"))
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
    
    # Run the Custom_Select
    mod_analysis_selector_custom_server("Custom_Select", tadat)
    
    # Reset dependent values when state/tribe changes
    shiny::observeEvent(c(tadat$criteria_state_tribe, tadat$uses_select_re_custom, tadat$criteria_template), {
      tadat$custom_raw <- NULL
      tadat$custom_raw2 <- NULL
      tadat$custom_raw3 <- NULL
      tadat$site_AU_table_custom <- NULL
      tadat$available_param_num_custom <- NULL
      tadat$exceed_summary_custom <- NULL
      tadat$exceed_summary_coords_custom <- NULL
      tadat$excurse_dat_custom_filtered <- NULL
      tadat$duration_table_custom <- NULL
    }, priority = 100)
    
    # # Run Custom_Data_Viewer
    # mod_analysis_data_viewer_custom_server("Custom_Data_Viewer", tadat)
    
    ### If the input data are ready, conduct the analysis
    shiny::observe({
      shiny::req(tadat$df_mlid_input, tadat$use_type_custom,
                 tadat$loc_select_custom, 
                 tadat$criteria_state_tribe, 
                 tadat$criteria_template,
                 tadat$uses_select_re_custom)
      
      # Check if uses are selected, if not, don't proceed
      if (is.null(tadat$uses_select_re_custom) || length(tadat$uses_select_re_custom) == 0) {
        tadat$custom_raw <- NULL
        tadat$site_AU_table_custom <- NULL
        tadat$available_param_num_custom <- NULL
        return()
      }
      
      ### Get the input data and convert ActivityStartDateTime to dateTime
      dat <- tadat$df_mlid_input
      dat <- dat |>
        dplyr::mutate(ActivityStartDateTime = 
                        suppressWarnings(
                        lubridate::parse_date_time(ActivityStartDateTime, 
                                                   orders = c("ymd HMS", "ymd HM", 
                                                              "ymd", "mdy")))
                        ) |>
        dplyr::mutate(ActivityStartDate = lubridate::ymd(ActivityStartDate)) |>
        dplyr::mutate(DateTime = ActivityStartDateTime)
      
      ### Step 1: Join pH, Temperature, and Hardness data
      dat2 <- dat |> 
        pH_fun() |>
        Temperature_fun() |>
        hardness_fun()
      
      ### Step 2: Join the criteria table
      
      if (tadat$use_type_custom %in% "Option 1"){
        req(tadat$df_mltoau_input, tadat$df_autouse_input)
        
        criteria_table_f1 <- tadat$criteria_template |>
          dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe) |>
          dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re_custom)
        
        # Filter the AU_Use based on available_uses_s
        AU_Use <- tadat$df_autouse_input
        AU_MLID <- tadat$df_mltoau_input 
        
        AU_Use_f1 <- AU_Use |>
          dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re_custom)
        
        # Filter the AU_MLID based on AU_Use_f1
        AU_MLID_f1 <- AU_MLID |>
          dplyr::filter(ATTAINS.AssessmentUnitIdentifier %in% 
                          AU_Use_f1$ATTAINS.AssessmentUnitIdentifier)
        
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
                           by = c("ATTAINS.AssessmentUnitIdentifier", 
                                  "ATTAINS.WaterType",
                                  "ATTAINS.OrganizationIdentifier"),
                           relationship = "many-to-many") |>
          criteria_join(criteria_table_f1, 
                        match_type = tadat$join_select_custom,
                        use_type = tadat$use_type_custom) |>
          # Remove NA in TADA.ResultMeasureValue and DateTime
          tidyr::drop_na(TADA.ResultMeasureValue) |>
          tidyr::drop_na(DateTime)
        
      } else {
        
        criteria_table_f1 <- tadat$criteria_template |>
          dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe) |>
          dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re_custom)
        
        # Join the criteria_table_f1 and AU_MLID_f1 to dat2
        dat4 <- dat2 |>
          criteria_join(criteria_table_f1, 
                        match_type = tadat$join_select_custom,
                        use_type = tadat$use_type_custom) |>
          # Remove NA in TADA.ResultMeasureValue and DateTime
          tidyr::drop_na(TADA.ResultMeasureValue) |>
          tidyr::drop_na(DateTime)
        
      }
      
      # Construct the selected columns
      selected_cols <- c(
        "TADA.MonitoringLocationIdentifier",
        "TADA.MonitoringLocationName",
        "TADA.LongitudeMeasure",
        "TADA.LatitudeMeasure",
        "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "AcuteChronic",
        "UniqueSpatialCriteria",
        "Season",
        "EquationBased",
        "EquationType", 
        "TADA.CharacteristicName",
        "TADA.ResultSampleFractionText",
        "TADA.MethodSpeciationName",
        "TADA.ResultMeasure.MeasureUnitCode",
        "TADA.ResultMeasureValue",
        "ActivityStartDate",
        "DateTime",
        "pH",
        "Temperature",
        "Hardness",
        "MagnitudeValueLower",
        "MagnitudeValueUpper",
        "DurationValue",
        "DurationUnit",
        "DurationMethod",
        "FreqValue",
        "FreqMethod",
        # Equation coefficient columns
        "Equation",
        "hardness_param_1",
        "hardness_param_2",
        "hardness_param_3",
        "hardness_param_4",
        "hardness_param_5",
        "hardness_param_6",
        "pH_param_1",
        "pH_param_2",
        "pH_param_3",
        "pH_param_4"
      )
      
      if (tadat$use_type_custom %in% "Option 1"){
        selected_cols <- c(selected_cols[1:4], 
                           "ATTAINS.AssessmentUnitIdentifier",
                           selected_cols[5:40])
      } else {
        selected_cols <- selected_cols
      }
      
      # Select columns
      dat4_1 <- dat4 |> dplyr::select(dplyr::all_of(selected_cols))
      
      ### Step 3: Separate the dataset based on if criteria exist
      dat_na <- dat4_1 |> dplyr::filter(is.na(EquationBased))
      dat_yes <- dat4_1 |> 
        dplyr::filter(EquationBased %in% "Yes") |>
        # Reove Additional Information in the EquationType for now
        dplyr::filter(!EquationType %in% "Additional Information")
      
      dat_no <- dat4_1 |> dplyr::filter(EquationBased %in% "No")
      
      dat_match <- dplyr::bind_rows(dat_yes, dat_no)
      dat_match2 <- dat_match |>
        dplyr::distinct(TADA.CharacteristicName, TADA.ResultSampleFractionText,
                        TADA.ResultMeasure.MeasureUnitCode)
      
      dat_viewer_count_num <- nrow(dat_match2)
      
      # Create a table for the map-table selector
      if (tadat$use_type_custom %in% "Option 1"){
        # Create a table for the map-table selector
        site_AU_table <- dat_match |>
          dplyr::distinct(TADA.MonitoringLocationIdentifier,
                          TADA.MonitoringLocationName,
                          TADA.LongitudeMeasure,
                          TADA.LatitudeMeasure,
                          ATTAINS.AssessmentUnitIdentifier)
      } else {
        site_AU_table <- dat_match |>
          dplyr::distinct(TADA.MonitoringLocationIdentifier,
                          TADA.MonitoringLocationName,
                          TADA.LongitudeMeasure,
                          TADA.LatitudeMeasure)
      }
      
      # Save the data
      tadat$available_param_num_custom <- dat_viewer_count_num

      tadat$custom_raw <- dat_match
      
      # tadat$dat_yes_custom <- dat_yes
      # tadat$dat_no_custom <- dat_no
      
      tadat$site_AU_table_custom <- site_AU_table
      
    })
    
    ### Based on tadat$site_AU_table_custom activate the map-table selector
    mod_map_table_selector_custom_server("Custom_map_table_selector", tadat)
    
    # Filter tadat$custom_raw based on selected monitoring locations
    shiny::observeEvent(tadat$selected_monitoring_locations_custom, {
      req(tadat$custom_raw)
      req(tadat$selected_monitoring_locations_custom)
      req(length(tadat$selected_monitoring_locations_custom) > 0)

      # Filter the data based on selected sites
      tadat$custom_raw2 <- tadat$custom_raw |>
        dplyr::filter(TADA.MonitoringLocationIdentifier %in% tadat$selected_monitoring_locations_custom)
      
      params <- sort(unique(tadat$custom_raw2$TADA.CharacteristicName))
      
      # Update parameter filter choices based on filtered data
      if (length(params) > 0) {
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter_custom",
          choices = params,
          selected = params
        )
      } else {
        # Clear the parameter filter if no data
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter_custom",
          choices = character(0),
          selected = character(0)
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
    shiny::observe({
      req(tadat$custom_raw3)
      shinyjs::toggleState(id = "Run_Custom", 
                           condition = nrow(tadat$custom_raw3) > 0)
    })
    
    shiny::observeEvent(input$Run_Custom, {
      req(tadat$custom_raw3)

      # a modal that pops up showing it's working on uploading the dataset from the users file
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Running the analysis ...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      dat4_2 <- tadat$custom_raw3
      
      dat_yes <- dat4_2 |> 
        dplyr::filter(EquationBased %in% "Yes") |>
        # Reove Additional Information in the EquationType for now
        dplyr::filter(!EquationType %in% "Additional Information")
      dat_no <- dat4_2 |> dplyr::filter(EquationBased %in% "No")
      
      drop_cols <- c("Equation", 
                     "hardness_param_1", "hardness_param_2", 
                     "hardness_param_3", "hardness_param_4",
                     "hardness_param_5", "hardness_param_6",
                     "pH_param_1", "pH_param_2", "pH_param_3", "pH_param_4")
      
      ### Step 4: Compare the dataset that the condition is not based on equation
      dat_no2 <- dat_no |> 
        excursion_fun() |>
        # Drop columns
        dplyr::select(-dplyr::all_of(drop_cols))
      
      ## Hardness
      dat_hardness <- dat_yes |>
        dplyr::filter(EquationType %in% "Hardness") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(Hardness), ~!is.na(.)))
      
      if (nrow(dat_hardness) > 0){
        dat_hardness2 <- dat_hardness |>
          dplyr::mutate(MagnitudeValueUpper = purrr::pmap_dbl(
            list("hardness" = Hardness,
                 "CF_A" = hardness_param_1, 
                 "CF_B" = hardness_param_2, 
                 "CF_C" = hardness_param_3,
                 "E_A" = hardness_param_4, 
                 "E_B" = hardness_param_5),
            .f = hardness_eq
          )) |>
          excursion_fun() |>
          dplyr::select(all_of(names(dat_no2)))
      } else {
        dat_hardness2 <- dat_hardness
      }
      
      # pH
      dat_pH <- dat_yes |>
        dplyr::filter(EquationType %in% "pH") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(pH), ~!is.na(.)))
      
      if (nrow(dat_pH) > 0){
        dat_pH2 <- dat_pH |>
          dplyr::mutate(
            MagnitudeValueUpper = purrr::map2_dbl(
              Equation, pH,
              ~ eval(parse(text = .x), envir = list(pH = .y))
            )
          ) |>
          excursion_fun() |>
          dplyr::select(all_of(names(dat_no2)))
      } else {
        dat_pH2 <- dat_pH
      }
      
      # pH and Hardness
      dat_pH_hardness <- dat_yes |>
        dplyr::filter(EquationType %in% "pH and Hardness") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(pH, Hardness), ~!is.na(.)))
      
      # Check if data are available
      if (nrow(dat_pH_hardness) > 0){
        dat_pH_hardness2 <- dat_pH_hardness |>
          dplyr::mutate(MagnitudeValueUpper = purrr::pmap_dbl(
            list("hardness" = Hardness,
                 "CF_A" = hardness_param_1, 
                 "CF_B" = hardness_param_2, 
                 "CF_C" = hardness_param_3,
                 "E_A" = hardness_param_4, 
                 "E_B" = hardness_param_5),
            .f = hardness_eq
          )) |>
          dplyr::mutate(MagnitudeValueUpper = if_else(
            pH < 7,
            pmin(hardness_param_6, MagnitudeValueUpper),
            MagnitudeValueUpper
          )) |>
          excursion_fun() |>
          dplyr::select(all_of(names(dat_no2)))
      } else {
        dat_pH_hardness2 <- dat_pH_hardness
      }
      
      # pH and Temperature
      dat_pH_temperature <- dat_yes |>
        dplyr::filter(EquationType %in% "pH and Temperature") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(pH, Temperature), ~!is.na(.)))
      
      # Check if data are available
      if (nrow(dat_pH_temperature) > 0){
        dat_pH_temperature2 <- dat_pH_temperature |>
          dplyr::mutate(
            MagnitudeValueUpper = purrr::pmap_dbl(
              list(Equation = Equation, pH = pH, Temperature = Temperature),
              ~ eval(parse(text = .x), envir = list(pH = .y, Temperature = .z))
            )
          ) |>
          excursion_fun() |>
          dplyr::select(all_of(names(dat_no2)))
      } else {
        dat_pH_temperature2 <- dat_pH_temperature
      }
      
      # Combine the results from each cases
      dat5 <- dplyr::bind_rows(
        dat_no2,
        dat_hardness2,
        dat_pH2,
        dat_pH_hardness2,
        dat_pH_temperature2
      )
      
      tadat$excurse_dat_custom_filtered <- dat5
      
      # Check if dat5 has zero rows and exit
      if (nrow(dat5) == 0) {
        # Remove the spinner
        shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
        
        shiny::showNotification(
          "No data available after processing. Please check your input criteria.",
          type = "warning",
          duration = 5
        )
        
        # Exit the observeEvent
        return()
      }
      
      ### Step 6: Summarize the data
      dat6 <- dat5 |>
        excursion_summary(type = tadat$loc_select_custom) 
      
      dat6a <- dat6 |> purrr::pluck("data")
      
      dat6b <- dat6 |> purrr::pluck("coords")
      
      ### Step 7. Aggregate the data based on time
      dat7 <- dat5 |> time_aggregate(type = tadat$loc_select_custom)
      
      ### Step 8. Conduct Duration Analysis
      dat8 <- dat7 |> duration_cal(type = tadat$loc_select_custom, complete_windows = FALSE)
      
      # Update the magnitude
      dat8_no <- dat8 |> dplyr::filter(EquationBased %in% "No")
      dat8_yes <- dat8 |> dplyr::filter(EquationBased %in% "Yes")
      dat8_yes2 <- dat8_yes |> 
        magnitude_update(match_type = tadat$join_select_custom,
                         hardness_equation = tadat$hardness_equation,
                         pH_equation = tadat$pH_equation,
                         pH_Hardness_equation = tadat$pH_hardness_equation,
                         pH_Temperature__equation = tadat$pH_Temperature_equation) |>
        dplyr::select(dplyr::all_of(names(dat8_no)))
      
      dat8_3 <- dplyr::bind_rows(dat8_no, dat8_yes2)
      
      tadat$duration_table_custom <- dat8_3
      
      ### Step 9. Conduct frequency summary
      dat9 <- dat8_3 |> frequency_summary(type = tadat$loc_select_custom)
      
      ### Step 10. Join the data
      dat9_1 <- dat9 |>
        dplyr::rename(Duration_Excursions = Number_of_Excursions,
                      Duration_Percentage = Excursion_Percentage) |>
        dplyr::select(-Percentile, -EquationBased, -EquationType,
                      -Start_Date, -End_Date, -Sample_Count)
      
      dat10 <- dat6a |> dplyr::left_join(dat9_1)
      
      ### Step 11. Prepare the output
      dat11 <- dat10 |> simplify_duration_frequency()
      
      tadat$exceed_summary_custom <- dat11
      tadat$exceed_summary_coords_custom <- dat6b
      
      ### Step 11. Download the batch analysis results
      output$download_results_custom <- shiny::downloadHandler(
        
        # define zipfile name
        filename = function() {
          paste0("Custom_Results_", tadat$default_outfile, ".zip")
        },
        
        # define contents of zipfile
        content = function(file) {
          # define file paths
          temp_dir <- tempdir()
          custom_result_path <- file.path(temp_dir, "TADAShinyAnalyze_custom_analysis_result.csv")
          custom_summary_path <- file.path(temp_dir, "TADAShinyAnalyze_custom_analysis_summary.csv")
          progress_file_custom_path <- file.path(temp_dir, "TADAShinyAnalyze_custom_prog.rda")
          
          # Load the DOCX file
          custom_docx_source <- app_sys("extdata/ReadMe_Custom.docx")
          custom_docx_path <- file.path(temp_dir, "ReadMe_Custom.docx")
          file.copy(custom_docx_source, custom_docx_path)
          
          # function to save tadat values
          write_tadat_file <- function(tadat, filename) {
            
            # define file variables to be saved
            default_outfile <- tadat$default_outfile
            job_id <- tadat$job_id
            df_custom_result <- tadat$duration_table_custom
            df_custom_summary <- tadat$exceed_summary_custom
            temp_dir <- tadat$temp_dir
            
            # save file
            save(default_outfile,
                 job_id,
                 df_custom_result,
                 df_custom_summary,
                 temp_dir,
                 file = filename)
          }
          
          # write tadat RData file with session info
          write_tadat_file(tadat, progress_file_custom_path)
          
          # write data frames to csv
          readr::write_csv(x = as.data.frame(tadat$duration_table_custom), 
                           file = custom_result_path, na = "")
          readr::write_csv(x = as.data.frame(tadat$exceed_summary_custom), 
                           file = custom_summary_path, na = "")
          
          
          # zip them
          utils::zip(zipfile = file,
                     files = c(custom_result_path, custom_summary_path, 
                               progress_file_custom_path, custom_docx_path),
                     flags = "-j")
        },
        contentType = "application/zip"
      ) # END ~ downloadHandler
      
      # enable download button
      shinyjs::enable("download_results_custom")
      
      # Ensure spinner is removed regardless of success or error
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
    })
    
    # Render the summary maps
    # Render the summary maps
    output$overall_map <- leaflet::renderLeaflet({
      req(tadat$exceed_summary_custom)

      create_overall_map(
        data = tadat$exceed_summary_custom,
        coords_data = tadat$exceed_summary_coords_custom,
        type = tadat$loc_select_custom
      )
    })
    
    output$use_map <- leaflet::renderLeaflet({
      req(tadat$exceed_summary_custom)
      
      create_use_map(
        data = tadat$exceed_summary_custom,
        coords_data = tadat$exceed_summary_coords_custom,
        selected_use = input$selected_use,
        type = tadat$loc_select_custom
      )
    })
    
    output$param_map <- leaflet::renderLeaflet({
      req(tadat$exceed_summary_custom)
      req(input$selected_param)
      req(input$selected_use_param)
      
      # Filter data to check if there are any results
      filtered_data <- tadat$exceed_summary_custom |>
        dplyr::filter(TADA.CharacteristicName %in% input$selected_param,
                      ATTAINS.UseName %in% input$selected_use_param)
      
      # Only create map if filtered data has rows
      if (nrow(filtered_data) > 0) {
        create_parameter_map(
          data = tadat$exceed_summary_custom,
          coords_data = tadat$exceed_summary_coords_custom,
          selected_param = input$selected_param,
          selected_use = input$selected_use_param,
          type = tadat$loc_select_custom
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
      
      params <- sort(unique(tadat$exceed_summary_custom$TADA.CharacteristicName))
      uses <- sort(unique(tadat$exceed_summary_custom$ATTAINS.UseName))  
      
      updateSelectInput(session, "selected_use", choices = uses, selected = uses[1])
      updateSelectInput(session, "selected_param", choices = params, selected = params[1])
    })
    
    observe({
      req(tadat$exceed_summary_custom)
      req(input$selected_param)
      
      # Get the uses after selecting the param
      filtered_data <- tadat$exceed_summary_custom |>
        dplyr::filter(TADA.CharacteristicName %in% input$selected_param)
      
      use_param <- sort(unique(filtered_data$ATTAINS.UseName))  
      
      updateSelectInput(session, "selected_use_param", choices = use_param, selected = use_param[1])
    })
    
    mod_excursion_viewer_server("Summary_View_Custom", 
                                summary_dat = reactive(tadat$exceed_summary_custom))
    mod_analysis_plots_server("Analysis_Plots_Custom",
                              excurse_dat = reactive(tadat$excurse_dat_custom_filtered),
                              excurse_summary = reactive(tadat$exceed_summary_custom),
                              loc_select = reactive(tadat$loc_select_custom),
                              tabname = "custom")

  })
  
  
}
    
## To be copied in the UI
# mod_custom_analysis_ui("custom_analysis_1")
    
## To be copied in the server
# mod_custom_analysis_server("custom_analysis_1")
