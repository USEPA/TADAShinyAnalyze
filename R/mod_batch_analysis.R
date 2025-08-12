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
data_path1 <- app_sys("extdata/Criteria_Table_Input.RData")
load(data_path1)

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
        column(
          width = 12,
          shiny::actionButton(ns("Run_Batch"), "Run Batch Analysis", shiny::icon("computer"),
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
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
          shinyjs::disabled(shiny::downloadButton(
            outputId = ns("download_results"),
            label = "Download Batch Results (.zip)",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4") # download button
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
        mod_exceedance_viewer_ui(ns("Summary_View"))
      )
    ),
    fluidRow(
      column(
        width = 6,
        mod_map_viewer_ui(ns("Summary_Map"))
      ),
      column(
        width = 6,
        htmltools::p("Placehoder for summary descrition.")
      )
    ),
    fluidRow(
      column(
        width = 12,
        mod_analysis_plots_ui(ns("Analysis_Plots"))
      )
    )
  )
}
    
#' batch_analysis Server Functions
#'
#' @noRd 
mod_batch_analysis_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mod_analysis_selector_server("Batch_Select", tadat)
    
    ### Remove records need to be reviewed in tadat$df_mltoau_input
    shiny::observeEvent(tadat$df_mltoau_input, {
      tadat$df_mltoau_input_f <- tadat$df_mltoau_input |>
        dplyr::filter(Needs_Review == "No")
    })
    
    ### If the input data are ready, conduct the analysis
    shiny::observeEvent(input$Run_Batch, {
      shiny::req(tadat$df_mlid_input, tadat$df_mltoau_input_f, tadat$df_autouse_input,
                 tadat$loc_select, tadat$state_tribe, tadat$uses_select)
      
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
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% tadat$state_tribe) |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select)
      
      # Filter the AU_Use based on available_uses_s
      AU_Use <- tadat$df_autouse_input
      AU_MLID <- tadat$df_mltoau_input_f |>
        dplyr::mutate(TADA.MonitoringLocationIdentifier = 
                        stringr::str_to_upper(MonitoringLocationIdentifier))
      
      AU_Use_f1 <- AU_Use |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select)
      
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
        criteria_join(criteria_table_f1) 
      
      ### Step 3: Separate the dataset based on if criteria exist
      dat_na <- dat4 |> dplyr::filter(is.na(EquationBased))
      dat_yes <- dat4 |> dplyr::filter(EquationBased %in% "Yes")
      dat_no <- dat4 |> dplyr::filter(EquationBased %in% "No")
      
      ### Step 4: Compare the dataset that the condition is not based on equation
      dat_no2 <- dat_no |> exceedance_fun()
      
      # Combine the results from each cases
      # TODO Need to make sure all the cases have the same column headers
      dat5 <- dplyr::bind_rows(dat_no2)
      
      tadat$exceed_dat <- dat5
      
      # Create a table for the map-table selector
      site_AU_table <- dat5 |>
        dplyr::distinct(TADA.MonitoringLocationIdentifier,
                        TADA.MonitoringLocationName,
                        TADA.MonitoringLocationTypeName,
                        TADA.LongitudeMeasure,
                        TADA.LatitudeMeasure,
                        JoinToAU.AssessmentUnitIdentifier)
      
      tadat$site_AU_table <- site_AU_table
      
      # A label to activate the third tab
      if (nrow(tadat$exceed_dat) > 0){
        tadat$exceed_dat_label <- TRUE
      } else {
        tadat$exceed_dat_label <- FALSE
      }
      
      ### Step 6: Summarize the data
      dat6 <- dat5 |> 
        exceedance_summary(type = tadat$loc_select, group = FALSE)
      
      # Save the data to tadat
      tadat$exceed_summary <- dat6
      
      ### Step 7. Download the batch analysis results
      output$download_results <- shiny::downloadHandler(
        
        # define zipfile name
        filename = function() {
          paste0(tadat$default_outfile, ".zip")
          # paste0("Batch_Results_",
          #        format(Sys.time(), "%Y%m%d_%H%M%S"),
          #        ".zip")
        },
        
        # define contents of zipfile
        content = function(file) {
          
          # define file paths
          temp_dir <- tempdir()
          ml_input_file_path <- file.path(temp_dir, paste0("TADAShinyAnalyze_copy_ml_input_file.csv"))
          mltoaus_file_path <- file.path(temp_dir, paste0("TADAShinyAnalyze_copy_mltoau_input_file.csv"))
          mltoaus_file_f_path <- file.path(temp_dir, paste0("TADAShinyAnalyze_copy_mltoau_input_file_filtered.csv"))
          autouse_file_path <- file.path(temp_dir, paste0("TADAShinyAnalyze_copy_autouse_input_file.csv"))
          batch_result_path <- file.path(temp_dir, paste0("TADAShinyAnalyze_batch_analysis_result.csv"))
          progress_file_path <- file.path(temp_dir, paste0("TADAShinyAnalyze_prog.rda"))
          zipfile <- file.path(temp_dir, paste0(tadat$default_outfile, ".zip"))
          
          # function to save tadat values
          write_tadat_file <- function(tadat, filename) {
            
            # define file variables to be saved
            default_outfile <- tadat$default_outfile
            job_id <- tadat$job_id
            df_ml_input <- tadat$df_mlid_input
            df_mltoau_input <- tadat$df_mltoau_input
            df_mltoau_input_f <- tadat$df_mltoau_input_f
            df_autouse_input <- tadat$df_autouse_input
            df_batch_result <- tadat$exceed_summary
            temp_dir <- tadat$temp_dir
            
            # save file
            save(default_outfile,
                 job_id,
                 df_ml_input,
                 df_mltoau_input,
                 df_mltoau_input_f,
                 df_autouse_input,
                 df_batch_result,
                 temp_dir,
                 file = filename)
          }
          
          # write tadat RData file with session info
          write_tadat_file(tadat, progress_file_path)
          
          # write data frames to csv
          readr::write_csv(x = as.data.frame(tadat$df_ml_input), file = ml_input_file_path)
          readr::write_csv(x = as.data.frame(tadat$df_mltoau_input), file = mltoaus_file_path)
          readr::write_csv(x = as.data.frame(tadat$df_mltoau_input_f), file = mltoaus_file_f_path)
          readr::write_csv(x = as.data.frame(tadat$df_autouse_input), file = autouse_file_path)
          readr::write_csv(x = as.data.frame(tadat$exceed_summary), file = batch_result_path)
          
          
          # zip them
          utils::zip(zipfile = zipfile,
                     files = c(ml_input_file_path, mltoaus_file_path, mltoaus_file_f_path, autouse_file_path, batch_result_path, progress_file_path),
                     flags = "-j")
          
          # Copy zip to final destination
          file.copy(zipfile, file)
        }
      ) # END ~ downloadHandler
      
      # enable download button
      shinyjs::enable("download_results")
      
    })
    
    # Activate the map-table selector
    mod_map_table_selector_server("Batch_map_table_selector", tadat)
    
    ### Subset tadat$exceed_summary if selected_monitoring_locations is ready
    shiny::observeEvent(c(tadat$selected_monitoring_locations, tadat$exceed_summary), {
      # Check if we have the exceed_summary data
      req(tadat$exceed_summary)
      
      # Get selected locations - if NULL or empty, use all locations
      selected_locs <- tadat$selected_monitoring_locations
      
      if (is.null(selected_locs) || length(selected_locs) == 0) {
        # No selection - set to NULL to show empty state
        tadat$exceedance_summary2 <- NULL
      } else {
        # Filter based on location type
        if (tadat$loc_select %in% c("MLid", "AU_ind")) {
          exceedance_summary2 <- tadat$exceed_summary |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs)
        } else {
          # For AU_group, need to filter by AU instead
          # First get the AUs for selected monitoring locations
          selected_aus <- tadat$site_AU_table |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs) |>
            dplyr::pull(JoinToAU.AssessmentUnitIdentifier) |>
            unique()
          
          exceedance_summary2 <- tadat$exceed_summary |>
            dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% selected_aus)
        }
        
        # Save exceedance_summary2 to tadat
        tadat$exceedance_summary2 <- exceedance_summary2
      }
      
      print("Test: exceedance_summary2")
      print(tadat$exceedance_summary2)
      
    }, ignoreNULL = FALSE)  # Changed to FALSE to handle empty selections
    
    # Update parameter filter when exceedance_summary2 changes
    shiny::observeEvent(tadat$exceedance_summary2, {
      # Handle NULL exceedance_summary2 (no sites selected)
      if (is.null(tadat$exceedance_summary2)) {
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
      params <- sort(unique(tadat$exceedance_summary2$TADA.CharacteristicName))
      
      # Only update if we have parameters to show
      if (length(params) > 0) {
        shiny::updateSelectizeInput(
          session = session,
          inputId = "parameter_filter",
          choices = params,
          selected = params  # Select all by default
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
    
    # Filter the tadat$exceed_summary2 by parameter
    shiny::observeEvent(c(tadat$exceedance_summary2, input$parameter_filter), {
      req(tadat$loc_select)
      
      # Handle NULL exceedance_summary2 (no sites selected)
      if (is.null(tadat$exceedance_summary2)) {
        tadat$exceed_summary_f <- NULL
        return()
      }
      
      # Handle NULL or empty parameter filter
      if (is.null(input$parameter_filter) || length(input$parameter_filter) == 0) {
        # If no parameters selected, show empty data
        tadat$exceed_summary_f <- NULL
      } else {
        exceed_summary3 <- tadat$exceedance_summary2 |>
          dplyr::filter(TADA.CharacteristicName %in% input$parameter_filter)
        
        # Save the data to tadat
        tadat$exceed_summary_f <- exceed_summary3
      }
      
      print("Test: exceedance_summary3")
      print(tadat$exceed_summary_f)
      
    }, ignoreNULL = FALSE)
    
    mod_exceedance_viewer_server("Summary_View", tadat)
    
    mod_map_viewer_server("Summary_Map", tadat)
    
    mod_analysis_plots_server("Analysis_Plots", tadat)
    
    # enable the third tab to be selected once input data is processed
    shiny::observeEvent(tadat$exceed_dat_label, {
      if (tadat$exceed_dat_label){
        shinyjs::enable(selector = '.nav li a[data-value="Custom"]') # also custom!
      } else {
        shinyjs::disable(selector = '.nav li a[data-value="Custom"]') # also custom!
      }})
    
  })
}
    
## To be copied in the UI
# mod_batch_analysis_ui("batch_analysis_1")
    
## To be copied in the server
# mod_batch_analysis_server("batch_analysis_1")
