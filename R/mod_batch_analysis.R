#' batch_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# Load the input data
mod_batch_analysis_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("2. Batch Analysis"),
    
    # Components
    fluidRow(
      column(
        width = 12,
        mod_analysis_selector_ui(ns("Batch_Select"))
      )
    ),
    
    
    fluidRow(
      column(
        width = 12,
        mod_analysis_data_viewer_ui(ns("Batch_Data_Viewer"))
      )
    ),
    
    htmltools::br(),
    htmltools::br(),
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          htmltools::h4("After finalizing the selections, click the 'Run Batch Analysis' button."),
          shinyjs::disabled(
            shiny::actionButton(ns("Run_Batch"), "Run Batch Analysis", shiny::icon("computer"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
          )
        )
      )
    ),
    
    htmltools::br(),
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          htmltools::h4("Download the batch analysis results by clicking the 'Download Batch Results' button."),
          shinyjs::disabled(shiny::downloadButton(
            outputId = ns("download_results"),
            label = "Download Batch Results (.zip)",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
      )
    ),
    
    # Horizontal divider
    htmltools::hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    # Map-table selector
    fluidRow(
      column(
        width = 12,
        mod_map_table_selector_ui(ns("Batch_map_table_selector"))
      )
    ),
    
    # Horizontal divider
    htmltools::hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    # Select the ML/AU iD
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          shiny::selectizeInput(inputId = ns("parameter_filter"),
                                label = "Filter parameter to view the results",
                                choices = NULL,
                                multiple = TRUE)
        )
      )
      
    ),
    fluidRow(
      column(
        width = 12,
        htmltools::h3("Summary Table"),
        mod_excursion_viewer_ui(ns("Summary_View"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        htmltools::h3("Plots"),
        htmltools::p("Use filters to view the results"),
        mod_analysis_plots_ui(ns("Analysis_Plots"))
      )
    ),
    
    # # Horizontal divider
    # htmltools::hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    # 
    # fluidRow(
    #   column(
    #     width = 12,
    #     mod_exceedance_viewer_ui(ns("Summary_Exceed_View"))
    #   )
    # )
  )
}

#' batch_analysis Server Functions
#'
#' @noRd 
mod_batch_analysis_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Run the Batch_Select
    mod_analysis_selector_server("Batch_Select", tadat)
    
    # Clear all dependent data immediately when state/tribe or uses change
    shiny::observeEvent(c(tadat$state_tribe, tadat$uses_select_re), {
      # Clear all analysis results
      tadat$site_AU_table <- NULL
      tadat$available_param_num <- NULL
      tadat$exceed_summary <- NULL
      tadat$exceed_summary_f <- NULL
      tadat$duration_table <- NULL
      tadat$excurse_dat <- NULL
      tadat$excurse_dat_filtered <- NULL
      tadat$excurse_summary <- NULL
      tadat$excursion_summary2 <- NULL
      
      # Clear intermediate data
      tadat$dat_yes <- NULL
      tadat$dat_no <- NULL
      
      # # Reset the filtered crosswalk
      # if (!is.null(tadat$df_mltoau_input)) {
      #   tadat$df_mltoau_input_f <- tadat$df_mltoau_input |>
      #     dplyr::filter(Needs_Review == "No")
      # } else {
      #   tadat$df_mltoau_input_f <- NULL
      # }
    }, priority = 100)
    
    # Run Barch_Data_Viewer
    mod_analysis_data_viewer_server("Batch_Data_Viewer", tadat)
    
    # ### Remove records need to be reviewed in tadat$df_mltoau_input
    # shiny::observe({
    #   # Only proceed if we need the crosswalk (Option 1)
    #   if (!is.null(tadat$use_type_batch) && tadat$use_type_batch == "Option 1") {
    #     if (!is.null(tadat$df_mltoau_input)) {
    #       tadat$df_mltoau_input_f <- tadat$df_mltoau_input |>
    #         dplyr::filter(Needs_Review == "No")
    #     } else {
    #       tadat$df_mltoau_input_f <- NULL
    #     }
    #   } else {
    #     # Clear if not using Option 1
    #     tadat$df_mltoau_input_f <- NULL
    #   }
    # })
    
    # shiny::observeEvent(tadat$df_mltoau_input, {
    #   tadat$df_mltoau_input <- tadat$df_mltoau_input |>
    #     dplyr::filter(Needs_Review == "No")
    # })
    
    shiny::observe({
      shiny::req(tadat$df_mlid_input, tadat$use_type_batch,
                 tadat$loc_select, tadat$state_tribe, tadat$uses_select_re, tadat$join_select)
      
      # Check if uses are selected, if not, don't proceed
      if (is.null(tadat$uses_select_re) || length(tadat$uses_select_re) == 0) {
        tadat$dat_yes <- NULL
        tadat$dat_no <- NULL
        tadat$site_AU_table <- NULL
        tadat$available_param_num <- NULL
        return()
      }
      
      isolate({
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
          dplyr::mutate(DateTime = ActivityStartDateTime) |>
          # Remove NA in TADA.ResultMeasureValue and DateTime
          tidyr::drop_na(TADA.ResultMeasureValue) |>
          tidyr::drop_na(DateTime)
        
        ### Step 1: Join pH, Temperature, and Hardness data
        dat2 <- dat |> 
          pH_fun() |>
          Temperature_fun() |>
          hardness_fun()
        
        ### Step 2: Join the criteria table
        
        if (tadat$use_type_batch %in% "Option 1"){
          req(tadat$df_mltoau_input, tadat$df_autouse_input)
          
          criteria_table_f1 <- criteria_table |>
            dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$state_tribe) |>
            dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re)
          
          # Filter the AU_Use based on available_uses_s
          AU_Use <- tadat$df_autouse_input
          AU_MLID <- tadat$df_mltoau_input |>
            dplyr::mutate(TADA.MonitoringLocationIdentifier = 
                            stringr::str_to_upper(MonitoringLocationIdentifier))
          
          AU_Use_f1 <- AU_Use |>
            dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re)
          
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
            dplyr::left_join(AU_MLID_f1) |>
            dplyr::left_join(AU_Use_f1, 
                             by = "JoinToAU.AssessmentUnitIdentifier",
                             relationship = "many-to-many") |>
            criteria_join(criteria_table_f1, 
                          match_type = tadat$join_select,
                          use_type = tadat$use_type_batch) |>
            # Remove NA in TADA.ResultMeasureValue and DateTime
            tidyr::drop_na(TADA.ResultMeasureValue) |>
            tidyr::drop_na(DateTime)
          
        } else {
          
          criteria_table_f1 <- criteria_table |>
            dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$state_tribe) |>
            dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re)
          
          # Join the criteria_table_f1 and AU_MLID_f1 to dat2
          dat4 <- dat2 |>
            criteria_join(criteria_table_f1, 
                          match_type = tadat$join_select,
                          use_type = tadat$use_type_batch) |>
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
        
        if (tadat$use_type_batch %in% "Option 1"){
          selected_cols <- c(selected_cols[1:4], 
                             "JoinToAU.AssessmentUnitIdentifier",
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
        
        # Save the data
        tadat$dat_yes <- dat_yes
        tadat$dat_no <- dat_no
        
        # Count available parameter
        dat_match <- dplyr::bind_rows(dat_yes, dat_no)
        dat_match2 <- dat_match |>
          dplyr::distinct(TADA.CharacteristicName, TADA.ResultSampleFractionText,
                          TADA.ResultMeasure.MeasureUnitCode)
        
        # Get the sample size
        dat_viewer_count_num <- nrow(dat_match2)
        
        # # Get the parameter that is not in dat_match, but with the same parameter names
        # if (tadat$join_select %in% "Option 1"){
        #   dat_not_match <- dat_na |>
        #     dplyr::semi_join(dat_match, by = c("TADA.CharacteristicName",
        #                                               "TADA.ResultSampleFractionText")) |>
        #     dplyr::anti_join(dat_match, by = "TADA.ResultMeasure.MeasureUnitCode") |>
        #   dplyr::distinct(TADA.CharacteristicName, TADA.ResultSampleFractionText,
        #                   TADA.ResultMeasure.MeasureUnitCode)
        # } else {
        #   dat_not_match <- dat_na |>
        #     dplyr::semi_join(dat_match , by = c("TADA.CharacteristicName")) |>
        #     dplyr::anti_join(dat_match, by = "TADA.ResultMeasure.MeasureUnitCode") |>
        #     dplyr::distinct(TADA.CharacteristicName, TADA.ResultSampleFractionText,
        #                     TADA.ResultMeasure.MeasureUnitCode)
        # }
        
        # Save the data
        tadat$available_param_num <- dat_viewer_count_num
        # tadat$dat_match  <- dat_match
        # tadat$dat_not_match <- dat_not_match
      })
    })
    
    ### Run the analysis if tadat$custom_raw3 is ready
    shiny::observe({
      req(tadat$available_param_num)
      shinyjs::toggleState(id = "Run_Batch",
                           condition = tadat$available_param_num > 0)
    })
    
    ### If the input data are ready, conduct the analysis
    shiny::observeEvent(input$Run_Batch, {
      shiny::req(tadat$dat_yes, tadat$dat_no)
      
      # a modal that pops up showing it's working on uploading the dataset from the users file
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Running the analysis ...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      drop_cols <- c("Equation", 
                     "hardness_param_1", "hardness_param_2", 
                     "hardness_param_3", "hardness_param_4",
                     "hardness_param_5", "hardness_param_6",
                     "pH_param_1", "pH_param_2", "pH_param_3", "pH_param_4")
      
      dat_yes <- tadat$dat_yes
      dat_no <- tadat$dat_no
      
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
      
      tadat$excurse_dat <- dat5
      tadat$excurse_dat_filtered <- tadat$excurse_dat
      
      if (tadat$use_type_batch %in% "Option 1"){
        # Create a table for the map-table selector
        site_AU_table <- dat5 |>
          dplyr::distinct(TADA.MonitoringLocationIdentifier,
                          TADA.MonitoringLocationName,
                          TADA.LongitudeMeasure,
                          TADA.LatitudeMeasure,
                          JoinToAU.AssessmentUnitIdentifier)
      } else {
        site_AU_table <- dat5 |>
          dplyr::distinct(TADA.MonitoringLocationIdentifier,
                          TADA.MonitoringLocationName,
                          TADA.LongitudeMeasure,
                          TADA.LatitudeMeasure)
      }
      
      tadat$site_AU_table <- site_AU_table
      
      ### Step 6: Summarize the data
      dat6 <- dat5 |> 
        excursion_summary(type = tadat$loc_select) |>
        purrr::pluck("data")
      
      ### Step 7. Aggregate the data based on time
      dat7 <- dat5 |> time_aggregate(type = tadat$loc_select)
      
      ### Step 8. Conduct Duration Analysis
      dat8 <- dat7 |> duration_cal(type = tadat$loc_select, complete_windows = FALSE)
      
      # Update the magnitude
      dat8_no <- dat8 |> dplyr::filter(EquationBased %in% "No")
      dat8_yes <- dat8 |> dplyr::filter(EquationBased %in% "Yes")
      dat8_yes2 <- dat8_yes |> 
        magnitude_update(match_type = tadat$join_select) |>
        dplyr::select(dplyr::all_of(names(dat8_no)))
      
      dat8_3 <- dplyr::bind_rows(dat8_no, dat8_yes2)
      
      tadat$duration_table <- dat8_3
      
      ### Step 9. Conduct frequency summary
      dat9 <- dat8_3 |> frequency_summary(type = tadat$loc_select)
      
      tadat$exceed_summary <- dat9
      
      ### Step 10. Join the data
      dat9_1 <- dat9 |>
        dplyr::rename(Duration_Excursions = Number_of_Excursions,
                      Duration_Percentage = Excursion_Percentage) |>
        dplyr::select(-Percentile, -EquationBased, -EquationType,
                      -Start_Date, -End_Date, -Sample_Count)
      
      dat10 <- dat6 |> dplyr::left_join(dat9_1)
      
      ### Step 11. Prepare the output
      dat11 <- dat10 |> simplify_duration_frequency()
      
      # Save the data to tadat
      tadat$excurse_summary <- dat11
      
      ### Step 10. Download the batch analysis results
      output$download_results <- shiny::downloadHandler(
        
        # define zipfile name
        filename = function() {
          paste0("Batch_Results_", tadat$default_outfile, ".zip")
        },
        
        # define contents of zipfile
        content = function(file) {
          
          # define file paths
          temp_dir <- tempdir()
          batch_result_path <- file.path(temp_dir, "TADAShinyAnalyze_batch_analysis_result.csv")
          batch_summary_path <- file.path(temp_dir, "TADAShinyAnalyze_batch_analysis_summary.csv")
          progress_file_path <- file.path(temp_dir, "TADAShinyAnalyze_prog.rda")
          
          # Load the DOCX file
          batch_docx_source <- app_sys("extdata/ReadMe_Batch.docx")
          batch_docx_path <- file.path(temp_dir, "ReadMe_Batch.docx")
          file.copy(batch_docx_source, batch_docx_path)
          
          # function to save tadat values
          write_tadat_file <- function(tadat, filename) {
            
            # define file variables to be saved
            default_outfile <- tadat$default_outfile
            job_id <- tadat$job_id
            df_batch_result <- tadat$duration_table
            df_batch_summary <- tadat$excurse_summary
            temp_dir <- tadat$temp_dir
            
            # save file
            save(default_outfile,
                 job_id,
                 df_batch_result,
                 df_batch_summary,
                 temp_dir,
                 file = filename)
          }
          
          # write tadat RData file with session info
          write_tadat_file(tadat, progress_file_path)
          
          # write data frames to csv
          readr::write_csv(x = as.data.frame(tadat$duration_table), file = batch_result_path)
          readr::write_csv(x = as.data.frame(tadat$excurse_summary), file = batch_summary_path)
          
          
          # zip them
          utils::zip(zipfile = file,
                     files = c(batch_result_path, batch_summary_path, 
                               progress_file_path, batch_docx_path),
                     flags = "-j")
        }
      ) # END ~ downloadHandler
      
      # enable download button
      shinyjs::enable("download_results")
      
      # Ensure spinner is removed regardless of success or error
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
    })
    
    # Activate the map-table selector
    mod_map_table_selector_server("Batch_map_table_selector", tadat)
    
    ### Subset tadat$excurse_summary if selected_monitoring_locations is ready
    shiny::observeEvent(c(tadat$selected_monitoring_locations, tadat$excurse_summary), {
      # Check if we have the excurse_summary data
      req(tadat$excurse_summary)
      
      # Get selected locations - if NULL or empty, use all locations
      selected_locs <- tadat$selected_monitoring_locations
      
      if (is.null(selected_locs) || length(selected_locs) == 0) {
        # No selection - set to NULL to show empty state
        tadat$excursion_summary2 <- NULL
      } else {
        # Filter based on location type
        if (tadat$loc_select %in% "MLid") {
          excursion_summary2 <- tadat$excurse_summary |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs)
        } else {
          # For AU_group, need to filter by AU instead
          # First get the AUs for selected monitoring locations
          selected_aus <- tadat$site_AU_table |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs) |>
            dplyr::pull(JoinToAU.AssessmentUnitIdentifier) |>
            unique()
          
          excursion_summary2 <- tadat$excurse_summary |>
            dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% selected_aus)
        }
        
        # Save excursion_summary2 to tadat
        tadat$excursion_summary2 <- excursion_summary2
      }
      
    }, ignoreNULL = FALSE)
    
    # Update parameter filter when excursion_summary2 changes
    shiny::observeEvent(tadat$excursion_summary2, {
      # Handle NULL excursion_summary2 (no sites selected)
      if (is.null(tadat$excursion_summary2)) {
        # Clear the parameter filter
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter",
          choices = character(0),
          selected = character(0)
        )
        return()  # Exit early
      }
      
      # Get the parameter names
      params <- sort(unique(tadat$excursion_summary2$TADA.CharacteristicName))
      
      # Only update if we have parameters to show
      if (length(params) > 0) {
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter",
          choices = params,
          selected = params
        )
      } else {
        # Clear the parameter filter if no data
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter",
          choices = character(0),
          selected = character(0)
        )
      }
    }, ignoreNULL = FALSE)
    
    # Filter the tadat$excurse_summary2 by parameter
    shiny::observeEvent(c(tadat$excursion_summary2, input$parameter_filter), {
      req(tadat$loc_select)
      
      # Handle NULL excursion_summary2 (no sites selected)
      if (is.null(tadat$excursion_summary2)) {
        tadat$excurse_summary_f <- NULL
        return()
      }
      
      # Handle NULL or empty parameter filter
      if (is.null(input$parameter_filter) || length(input$parameter_filter) == 0) {
        # If no parameters selected, show empty data
        tadat$excurse_summary_f <- NULL
      } else {
        excurse_summary3 <- tadat$excursion_summary2 |>
          dplyr::filter(TADA.CharacteristicName %in% input$parameter_filter)
        
        # Save the data to tadat
        tadat$excurse_summary_f <- excurse_summary3
      }
      
      if (!is.null(tadat$excurse_summary_f) && nrow(tadat$excurse_summary_f) > 0) {
        # Get the filtered parameters and locations
        filtered_params <- unique(tadat$excurse_summary_f$TADA.CharacteristicName)
        
        if (tadat$loc_select %in% c("MLid")) {
          filtered_locs <- unique(tadat$excurse_summary_f$TADA.MonitoringLocationIdentifier)
          tadat$excurse_dat_filtered <- tadat$excurse_dat |>
            dplyr::filter(TADA.CharacteristicName %in% filtered_params,
                          TADA.MonitoringLocationIdentifier %in% filtered_locs)
        } else {
          filtered_aus <- unique(tadat$excurse_summary_f$JoinToAU.AssessmentUnitIdentifier)
          tadat$excurse_dat_filtered <- tadat$excurse_dat |>
            dplyr::filter(TADA.CharacteristicName %in% filtered_params,
                          JoinToAU.AssessmentUnitIdentifier %in% filtered_aus)
        }
      } else {
        tadat$excurse_dat_filtered <- NULL
      }
      
    }, ignoreNULL = FALSE)
    
    # Filter the tadat$exceed_summary by parameter
    shiny::observeEvent(c(input$parameter_filter, tadat$exceed_summary), {
      req(tadat$loc_select, tadat$selected_monitoring_locations)
      
      # Handle NULL exceedance_summary2 (no sites selected)
      if (is.null(tadat$selected_monitoring_locations)) {
        tadat$exceed_summary_f <- NULL
        return()
      }
      
      # Get selected locations - if NULL or empty, use all locations
      selected_locs <- tadat$selected_monitoring_locations
      
      # Filter based on location type for the exceedance results
      if (tadat$loc_select %in% "MLid") {
        exceedance_summary2 <- tadat$exceed_summary |>
          dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs)
      } else {
        selected_aus <- tadat$site_AU_table |>
          dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs) |>
          dplyr::pull(JoinToAU.AssessmentUnitIdentifier) |>
          unique()
        
        exceedance_summary2 <- tadat$exceed_summary |>
          dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% selected_aus)
      }
      
      # Handle NULL or empty parameter filter
      if (is.null(input$parameter_filter) || length(input$parameter_filter) == 0) {
        # If no parameters selected, show empty data
        tadat$exceed_summary_f <- NULL
      } else {
        exceedance_summary3 <- exceedance_summary2 |>
          dplyr::filter(TADA.CharacteristicName %in% input$parameter_filter)
        
        # Save the data to tadat
        tadat$exceed_summary_f <- exceedance_summary3
      }
      
    }, ignoreNULL = FALSE)
    
    mod_excursion_viewer_server("Summary_View", 
                                summary_dat = reactive(tadat$excurse_summary_f))
    
    mod_analysis_plots_server("Analysis_Plots",
                              excurse_dat = reactive(tadat$excurse_dat_filtered),
                              excurse_summary = reactive(tadat$excurse_summary_f),
                              loc_select = reactive(tadat$loc_select),
                              tabname = "batch")
    
    
  })
}

## To be copied in the UI
# mod_batch_analysis_ui("batch_analysis_1")

## To be copied in the server
# mod_batch_analysis_server("batch_analysis_1")