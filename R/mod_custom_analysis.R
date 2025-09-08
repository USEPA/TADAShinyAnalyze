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
        # shiny::checkboxInput(inputId = ns("custom_group"),
        #                      label = "Group the ML/AU ID for analysis"),
        shiny::selectizeInput(inputId = ns("parameter_filter_custom"),
                              label = "Filter parameter to view the results",
                              choices = NULL,
                              multiple = TRUE)
      )
    ),
    fluidRow(
      column(
        width = 12,
        htmltools::p("After finalizing the selections, click the 'Run Custom Analysis' button."),
        shiny::actionButton(ns("Run_Custom"), "Run Custom Analysis", shiny::icon("computer"),
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      )
    ),

    fluidRow(
      column(
        width = 12,
        htmltools::p("Download the custom analysis results by clicking the 'Download Custom Results' button."),
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
        htmltools::p("The table shows the summary of excursion (individual sample in comparison to the standard) and exceedance (evaluated based on the duration and frequency information)."),
        mod_exceedance_viewer_custom_ui(ns("Summary_View_Custom"))
      )
    ),
    # Add to your UI
    fluidRow(
      htmltools::h3("Summary Maps"),
      htmltools::p("Use the maps to view the exceedance results with different levels."),
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
    ),
    fluidRow(
      column(
        width = 12,
        htmltools::h3("Plots"),
        htmltools::p("Use filters to view the results"),
        mod_analysis_plots_custom_ui(ns("Analysis_Plots_Custom"))
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
        dplyr::mutate(ActivityStartDateTime = 
                        lubridate::parse_date_time(ActivityStartDateTime, 
                                                   orders = c("ymd HMS", "ymd HM"))) |>
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
        criteria_join(criteria_table_f1, match_type = tadat$join_select_custom) |>
        # Remove Equation based calculation for now
        dplyr::filter(!is.na(EquationBased)) |>       
        # Remove NA in TADA.ResultMeasureValue and DateTime
        tidyr::drop_na(TADA.ResultMeasureValue) |>
        tidyr::drop_na(DateTime)
      
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

      # a modal that pops up showing it's working on uploading the dataset from the users file
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Runniing the analysis ...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      dat4 <- tadat$custom_raw3
      
      # Select columns
      dat4_1 <- dat4 |>
        dplyr::select(
          TADA.MonitoringLocationIdentifier,
          TADA.MonitoringLocationName,
          TADA.LongitudeMeasure,
          TADA.LatitudeMeasure,
          JoinToAU.AssessmentUnitIdentifier,
          ATTAINS.OrganizationIdentifier,
          ATTAINS.ParameterName,
          ATTAINS.UseName,
          AcuteChronic,
          EquationBased,
          Notes2, # kept for reference, but no longer used for routing
          TADA.CharacteristicName,
          TADA.ResultSampleFractionText,
          TADA.MethodSpeciationName,
          TADA.ResultMeasure.MeasureUnitCode,
          TADA.ResultMeasureValue,
          ActivityStartDate,
          DateTime,
          pH,
          Temperature,
          Hardness,
          MagnitudeValueLower,
          MagnitudeValueUpper,
          DurationValue,
          DurationUnit,
          DurationAggregation,
          FrequencyCriteriaValue,
          FrequencyCriteriaMethod
        ) 
      
      ### Step 3: Separate the dataset based on if criteria exist
      dat_na <- dat4_1 |> dplyr::filter(is.na(EquationBased))
      dat_yes <- dat4_1 |> dplyr::filter(EquationBased %in% "Yes")
      dat_no <- dat4_1 |> dplyr::filter(EquationBased %in% "No")
      
      ### Step 4: Compare the dataset that the condition is not based on equation
      dat_no2 <- dat_no |> excursion_fun()
      
      ## Hardness
      dat_hardness <- dat_yes |>
        dplyr::filter(Notes2 %in% "Hardness") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(Hardness), ~!is.na(.)))
      
      if (nrow(dat_hardness) > 0){
        dat_hardness2 <- dat_hardness |>
          dplyr::left_join(hardness_equation) |>
          dplyr::mutate(MagnitudeValueUpper = purrr::pmap_dbl(
            list("hardness" = Hardness,
                 "CF_A" = CF_A, "CF_B" = CF_B, "CF_C" = CF_C,
                 "E_A" = E_A, "E_B" = E_B),
            .f = hardness_eq
          )) |>
          excursion_fun() |>
          dplyr::select(all_of(names(dat_no2)))
      } else {
        dat_hardness2 <- dat_hardness
      }
      
      # pH
      dat_pH <- dat_yes |>
        dplyr::filter(Notes2 %in% "pH") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(pH), ~!is.na(.)))
      
      if (nrow(dat_pH) > 0){
        dat_pH2 <- dat_pH |>
          dplyr::left_join(pH_equation) |>
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
        dplyr::filter(Notes2 %in% "pH and Hardness") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(pH, Hardness), ~!is.na(.)))
      
      # Check if data are available
      if (nrow(dat_pH_hardness) > 0){
        dat_pH_hardness2 <- dat_pH_hardness |>
          dplyr::left_join(pH_Hardness_equation) |>
          dplyr::mutate(MagnitudeValueUpper = purrr::pmap_dbl(
            list("hardness" = Hardness,
                 "CF_A" = CF_A, "CF_B" = CF_B, "CF_C" = CF_C,
                 "E_A" = E_A, "E_B" = E_B),
            .f = hardness_eq
          )) |>
          dplyr::mutate(MagnitudeValueUpper = if_else(
            pH < 7,
            pmin(87, MagnitudeValueUpper),
            MagnitudeValueUpper
          )) |>
          excursion_fun() |>
          dplyr::select(all_of(names(dat_no2)))
      } else {
        dat_pH_hardness2 <- dat_pH_hardness
      }
      
      # pH and Temperature
      dat_pH_temperature <- dat_yes |>
        dplyr::filter(Notes2 %in% "pH and Temperature") |>
        # Check the completeness of the input data
        dplyr::filter(dplyr::if_all(c(pH, Temperature), ~!is.na(.)))
      
      # Check if data are available
      if (nrow(dat_pH_temperature) > 0){
        dat_pH_temperature2 <- dat_pH_temperature |>
          dplyr::left_join(pH_Temperature_equation) |>
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
      
      ### Step 6: Summarize the data
      dat6 <- dat5 |>
        excursion_summary(type = tadat$loc_select_custom) 
      
      dat6a <- dat6 |> purrr::pluck("data")
      
      dat6b <- dat6 |> purrr::pluck("coords")
      
      ### Step 7. Aggregate the data based on time
      dat7 <- dat5 |> time_aggregate(type = tadat$loc_select_custom)
      
      ### Step 8. Conduct Duration Analysis
      dat8 <- dat7 |> duration_cal(type = tadat$loc_select, complete_windows = FALSE)
      
      # Update the magnitude
      dat8_no <- dat8 |> dplyr::filter(EquationBased %in% "No")
      dat8_yes <- dat8 |> dplyr::filter(EquationBased %in% "Yes")
      dat8_yes2 <- dat8_yes |> 
        magnitude_update() |>
        dplyr::select(dplyr::all_of(names(dat8_no)))
      
      dat8_3 <- dplyr::bind_rows(dat8_no, dat8_yes2)
      
      tadat$duration_table <- dat8_3
      
      ### Step 9. Conduct frequency summary
      dat9 <- dat8 |> frequency_summary(type = tadat$loc_select_custom)
      
      ### Step 10. Join the data
      dat9_1 <- dat9 |>
        dplyr::rename(Duration_Excursions = Number_of_Excursions,
                      Duration_Percentage = Excursion_Percentage) |>
        dplyr::select(-Percentile, -EquationBased, -Notes2,
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
          # Make sure the filename has .zip extension
          if (!is.null(tadat$default_custom_outfile)) {
            paste0(tadat$default_custom_outfile, ".zip")
          } else {
            paste0("Custom_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
          }
        },
        
        # define contents of zipfile
        content = function(file) {
          # define file paths
          temp_dir <- tempdir()
          custom_result_path <- file.path(temp_dir, "TADAShinyAnalyze_custom_analysis_result.csv")
          custom_summary_path <- file.path(temp_dir, "TADAShinyAnalyze_custom_analysis_summary.csv")
          progress_file_custom_path <- file.path(temp_dir, "TADAShinyAnalyze_custom_prog.rda")
          
          # function to save tadat values
          write_tadat_file <- function(tadat, filename) {
            
            # define file variables to be saved
            default_outfile <- tadat$default_outfile
            job_id <- tadat$job_id
            df_custom_result <- tadat$duration_table
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
          readr::write_csv(x = as.data.frame(tadat$duration_table), file = custom_result_path)
          readr::write_csv(x = as.data.frame(tadat$exceed_summary_custom), file = custom_summary_path)
          
          
          # zip them
          utils::zip(zipfile = file,
                     files = c(custom_result_path, custom_summary_path, progress_file_custom_path),
                     flags = "-j")
        },
        contentType = "application/zip"
      ) # END ~ downloadHandler
      
      # enable download button
      shinyjs::enable("download_results_custom")
      
      # Ensure spinner is removed regardless of success or error
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
    })
    
    mod_exceedance_viewer_custom_server("Summary_View_Custom", tadat)
    mod_analysis_plots_custom_server("Analysis_Plots_Custom", tadat)
    
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
        dplyr::filter(TADA.CharacteristicName == input$selected_param,
                      ATTAINS.UseName == input$selected_use_param)
      
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
