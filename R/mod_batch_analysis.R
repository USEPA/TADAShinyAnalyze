#' batch_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_batch_analysis_ui <- function(id) {
  # set module session id
  ns <- NS(id)

  # start taglist
  tagList(
    # header
    htmltools::h2("3. Batch Analysis"),

    # Components
    fluidRow(column(width = 12, mod_analysis_selector_ui(ns("Batch_Select")))),

    fluidRow(column(
      width = 12,
      mod_analysis_data_viewer_ui(ns("Batch_Data_Viewer"))
    )),

    htmltools::br(),
    htmltools::br(),

    fluidRow(column(
      width = 12,
      column(
        width = 12,
        htmltools::h4(
          "After finalizing the selections, click the 'Run Batch Analysis' button."
        ),
        shinyjs::disabled(shiny::actionButton(
          ns("Run_Batch"),
          "Run Batch Analysis",
          shiny::icon("computer"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      )
    )),

    htmltools::br(),

    fluidRow(column(
      width = 12,
      column(
        width = 12,
        htmltools::h4(
          "Download the batch analysis results by clicking the 'Download Batch Results' button."
        ),
        shinyjs::disabled(shiny::downloadButton(
          outputId = ns("download_results"),
          label = "Download Batch Results (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      )
    )),

    # Horizontal divider
    htmltools::hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),

    # Map-table selector
    fluidRow(column(
      width = 12,
      mod_map_table_selector_ui(ns("Batch_map_table_selector"))
    )),

    # Horizontal divider
    htmltools::hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),

    # Filter selector
    fluidRow(column(
      width = 12,
      shiny::selectizeInput(
        inputId = ns("parameter_filter"),
        label = "Filter ATTAINS parameter to view the results",
        choices = NULL,
        multiple = TRUE
      )
    )),

    fluidRow(column(
      width = 12,
      htmltools::h3("Summary Table"),
      mod_excursion_viewer_ui(ns("Summary_View"))
    )),

    fluidRow(column(
      width = 12,
      htmltools::h3("Plots"),
      htmltools::p("Use filters to view the results"),
      mod_analysis_plots_ui(ns("Analysis_Plots"))
    ))
  )
}

#' batch_analysis Server Functions
#'
#' @noRd
mod_batch_analysis_server <- function(id, tadat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Run the Batch_Select
    mod_analysis_selector_server("Batch_Select", tadat)

    # Clear all dependent data immediately when state/tribe or uses change
    shiny::observeEvent(
      c(
        tadat$uses_select_re,
        tadat$criteria_state_tribe,
        tadat$criteria_template
      ),
      {
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
      },
      priority = 100
    )

    # Run Batch_Data_Viewer
    mod_analysis_data_viewer_server("Batch_Data_Viewer", tadat)

    shiny::observe({
      shiny::req(
        tadat$df_mlid_input,
        tadat$use_type_batch,
        tadat$criteria_template,
        tadat$loc_select,
        tadat$uses_select_re,
        tadat$join_select
      )

      # Check if uses are selected, if not, don't proceed
      if (is.null(tadat$uses_select_re) || length(tadat$uses_select_re) == 0) {
        tadat$dat_yes <- NULL
        tadat$dat_no <- NULL
        tadat$site_AU_table <- NULL
        tadat$available_param_num <- NULL
        return()
      }

      isolate({
        # Get the input data and convert ActivityStartDateTime to dateTime
        dat <- tadat$df_mlid_input |>
          dplyr::mutate(
            ActivityStartDateTime = suppressWarnings(lubridate::parse_date_time(
              ActivityStartDateTime,
              orders = c("ymd HMS", "ymd HM", "ymd", "mdy")
            ))
          ) |>
          dplyr::mutate(
            ActivityStartDate = lubridate::ymd(ActivityStartDate)
          ) |>
          dplyr::mutate(DateTime = ActivityStartDateTime) |>
          tidyr::drop_na(TADA.ResultMeasureValue) |>
          tidyr::drop_na(DateTime)

        # Step 1: Join pH, Temperature, and Hardness data
        dat2 <- dat |> pH_fun() |> Temperature_fun() |> hardness_fun()

        # Step 2: Join the criteria table
        if (tadat$use_type_batch %in% "Option 1") {
          req(tadat$df_mltoau_input, tadat$df_autouse_input)

          criteria_table_f1 <- tadat$criteria_template |>
            dplyr::filter(
              ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe,
              ATTAINS.UseName %in% tadat$uses_select_re
            )

          AU_Use <- tadat$df_autouse_input
          AU_MLID <- tadat$df_mltoau_input

          AU_Use_f1 <- AU_Use |>
            dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_re)

          AU_MLID_f1 <- AU_MLID |>
            dplyr::filter(
              ATTAINS.AssessmentUnitIdentifier %in%
                AU_Use_f1$ATTAINS.AssessmentUnitIdentifier
            )

          dat3 <- dat2 |>
            dplyr::filter(
              TADA.MonitoringLocationIdentifier %in%
                AU_MLID_f1$TADA.MonitoringLocationIdentifier
            )

          dat4 <- dat3 |>
            dplyr::left_join(AU_MLID_f1) |>
            dplyr::left_join(
              AU_Use_f1,
              by = c(
                "ATTAINS.AssessmentUnitIdentifier",
                "ATTAINS.WaterType",
                "ATTAINS.OrganizationIdentifier"
              ),
              relationship = "many-to-many"
            ) |>
            criteria_join(
              criteria_table_f1,
              match_type = tadat$join_select,
              use_type = tadat$use_type_batch
            ) |>
            tidyr::drop_na(TADA.ResultMeasureValue) |>
            tidyr::drop_na(DateTime)
        } else {
          criteria_table_f1 <- tadat$criteria_template |>
            dplyr::filter(
              ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe,
              ATTAINS.UseName %in% tadat$uses_select_re
            )

          dat4 <- dat2 |>
            criteria_join(
              criteria_table_f1,
              match_type = tadat$join_select,
              use_type = tadat$use_type_batch
            ) |>
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
          "EquationFormula",
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

        if (tadat$use_type_batch %in% "Option 1") {
          selected_cols2 <- c(
            selected_cols[1:4],
            "ATTAINS.AssessmentUnitIdentifier",
            selected_cols[5:40]
          )
        } else {
          selected_cols2 <- selected_cols
        }

        # Select columns
        dat4_1 <- dat4 |> dplyr::select(dplyr::all_of(selected_cols2))

        # Step 3: Separate the dataset based on if criteria exist
        dat_na <- dat4_1 |> dplyr::filter(is.na(EquationBased))
        dat_yes <- dat4_1 |>
          dplyr::filter(EquationBased %in% "Yes") |>
          dplyr::filter(!EquationType %in% "Additional Information")

        dat_no <- dat4_1 |> dplyr::filter(EquationBased %in% "No")

        # Save the data
        tadat$dat_yes <- dat_yes
        tadat$dat_no <- dat_no

        # Count available parameter
        dat_match <- dplyr::bind_rows(dat_yes, dat_no)
        dat_match2 <- dat_match |>
          dplyr::distinct(
            ATTAINS.ParameterName,
            TADA.CharacteristicName,
            TADA.ResultSampleFractionText,
            TADA.MethodSpeciationName,
            TADA.ResultMeasure.MeasureUnitCode
          )

        # Get the sample size
        dat_viewer_count_num <- nrow(dat_match2)

        # Save the data
        tadat$available_param_num <- dat_viewer_count_num
        tadat$dat_match <- dat_match2
      })
    })

    # Enable Run button when ready
    shiny::observe({
      req(tadat$available_param_num)
      shinyjs::toggleState(
        id = "Run_Batch",
        condition = tadat$available_param_num > 0
      )
    })

    # If the input data are ready, conduct the analysis
    shiny::observeEvent(input$Run_Batch, {
      shiny::req(tadat$dat_yes, tadat$dat_no)

      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Running the analysis ...",
        session = shiny::getDefaultReactiveDomain()
      )

      drop_cols <- c(
        "EquationFormula",
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

      dat_yes <- tadat$dat_yes
      dat_no <- tadat$dat_no

      # Step 4: Compare the dataset that the condition is not based on equation
      dat_no2 <- dat_no |>
        excursion_fun() |>
        dplyr::select(-dplyr::all_of(drop_cols))

      # Hardness
      dat_hardness <- dat_yes |>
        dplyr::filter(EquationType %in% "Hardness") |>
        dplyr::filter(dplyr::if_all(c(Hardness), ~ !is.na(.)))

      if (nrow(dat_hardness) > 0) {
        dat_hardness2 <- dat_hardness |>
          dplyr::mutate(
            MagnitudeValueUpper = purrr::pmap_dbl(
              list(
                "hardness" = Hardness,
                "CF_A" = hardness_param_1,
                "CF_B" = hardness_param_2,
                "CF_C" = hardness_param_3,
                "E_A" = hardness_param_4,
                "E_B" = hardness_param_5
              ),
              .f = hardness_eq
            )
          ) |>
          excursion_fun() |>
          dplyr::select(dplyr::all_of(names(dat_no2)))
      } else {
        dat_hardness2 <- dat_hardness
      }

      # pH
      dat_pH <- dat_yes |>
        dplyr::filter(EquationType %in% "pH") |>
        dplyr::filter(dplyr::if_all(c(pH), ~ !is.na(.)))

      if (nrow(dat_pH) > 0) {
        dat_pH2 <- dat_pH |>
          dplyr::mutate(
            MagnitudeValueUpper = purrr::map2_dbl(
              Equation,
              pH,
              ~ eval(parse(text = .x), envir = list(pH = .y))
            )
          ) |>
          excursion_fun() |>
          dplyr::select(dplyr::all_of(names(dat_no2)))
      } else {
        dat_pH2 <- dat_pH
      }

      # pH and Hardness
      dat_pH_hardness <- dat_yes |>
        dplyr::filter(EquationType %in% "pH and Hardness") |>
        dplyr::filter(dplyr::if_all(c(pH, Hardness), ~ !is.na(.)))

      if (nrow(dat_pH_hardness) > 0) {
        dat_pH_hardness2 <- dat_pH_hardness |>
          dplyr::mutate(
            MagnitudeValueUpper = purrr::pmap_dbl(
              list(
                "hardness" = Hardness,
                "CF_A" = hardness_param_1,
                "CF_B" = hardness_param_2,
                "CF_C" = hardness_param_3,
                "E_A" = hardness_param_4,
                "E_B" = hardness_param_5
              ),
              .f = hardness_eq
            )
          ) |>
          dplyr::mutate(
            MagnitudeValueUpper = dplyr::if_else(
              pH < 7,
              pmin(hardness_param_6, MagnitudeValueUpper),
              MagnitudeValueUpper
            )
          ) |>
          excursion_fun() |>
          dplyr::select(dplyr::all_of(names(dat_no2)))
      } else {
        dat_pH_hardness2 <- dat_pH_hardness
      }

      # pH and Temperature
      dat_pH_temperature <- dat_yes |>
        dplyr::filter(EquationType %in% "pH and Temperature") |>
        dplyr::filter(dplyr::if_all(c(pH, Temperature), ~ !is.na(.)))

      if (nrow(dat_pH_temperature) > 0) {
        dat_pH_temperature2 <- dat_pH_temperature |>
          dplyr::mutate(
            MagnitudeValueUpper = purrr::pmap_dbl(
              list(Equation = Equation, pH = pH, Temperature = Temperature),
              ~ eval(parse(text = .x), envir = list(pH = .y, Temperature = .z))
            )
          ) |>
          excursion_fun() |>
          dplyr::select(dplyr::all_of(names(dat_no2)))
      } else {
        dat_pH_temperature2 <- dat_pH_temperature
      }

      # Combine the results from each case
      dat5 <- dplyr::bind_rows(
        dat_no2,
        dat_hardness2,
        dat_pH2,
        dat_pH_hardness2,
        dat_pH_temperature2
      )

      if (nrow(dat5) == 0) {
        shinybusy::remove_modal_spinner(
          session = shiny::getDefaultReactiveDomain()
        )
        shiny::showNotification(
          "No data available after processing. Please check your input criteria.",
          type = "warning",
          duration = 5
        )
        return()
      }

      tadat$excurse_dat <- dat5 |>
        dplyr::mutate(
          ParameterForFilter = dplyr::coalesce(
            ATTAINS.ParameterName,
            TADA.CharacteristicName
          )
        )
      tadat$excurse_dat_filtered <- tadat$excurse_dat

      if (tadat$use_type_batch %in% "Option 1") {
        site_AU_table <- dat5 |>
          dplyr::distinct(
            TADA.MonitoringLocationIdentifier,
            TADA.MonitoringLocationName,
            TADA.LongitudeMeasure,
            TADA.LatitudeMeasure,
            ATTAINS.AssessmentUnitIdentifier
          )
      } else {
        site_AU_table <- dat5 |>
          dplyr::distinct(
            TADA.MonitoringLocationIdentifier,
            TADA.MonitoringLocationName,
            TADA.LongitudeMeasure,
            TADA.LatitudeMeasure
          )
      }
      tadat$site_AU_table <- site_AU_table

      # Step 6: Summarize the data
      dat6 <- dat5 |>
        excursion_summary(type = tadat$loc_select) |>
        purrr::pluck("data")

      # Step 7: Aggregate the data based on time
      dat7 <- dat5 |> time_aggregate(type = tadat$loc_select)

      # Step 8: Conduct Duration Analysis
      dat8 <- dat7 |>
        duration_cal(type = tadat$loc_select, complete_windows = FALSE)

      dat8_no <- dat8 |> dplyr::filter(EquationBased %in% "No")
      dat8_yes <- dat8 |> dplyr::filter(EquationBased %in% "Yes")
      dat8_yes2 <- dat8_yes |>
        magnitude_update(
          match_type = tadat$join_select,
          hardness_equation = tadat$hardness_equation,
          pH_equation = tadat$pH_equation,
          pH_Hardness_equation = tadat$pH_hardness_equation,
          pH_Temperature_equation = tadat$pH_Temperature_equation
        ) |>
        dplyr::select(dplyr::all_of(names(dat8_no)))

      dat8_3 <- dplyr::bind_rows(dat8_no, dat8_yes2)
      tadat$duration_table <- dat8_3

      # Step 9. Conduct frequency summary
      dat9 <- dat8_3 |> frequency_summary(type = tadat$loc_select)

      tadat$exceed_summary <- dat9 |>
        dplyr::mutate(
          ParameterForFilter = dplyr::coalesce(
            ATTAINS.ParameterName,
            TADA.CharacteristicName
          )
        )

      # Step 10. Join the data
      dat9_1 <- dat9 |>
        dplyr::rename(
          Duration_Excursions = Number_of_Excursions,
          Duration_Percentage = Excursion_Percentage
        ) |>
        dplyr::select(
          -Percentile,
          -EquationBased,
          -EquationType,
          -Start_Date,
          -End_Date,
          -Sample_Count
        )

      dat10 <- dat6 |> dplyr::left_join(dat9_1)

      # Step 11. Prepare the output
      dat11 <- dat10 |> simplify_duration_frequency()

      # Save the data to tadat
      tadat$excurse_summary <- dat11 |>
        dplyr::mutate(
          ParameterForFilter = dplyr::coalesce(
            ATTAINS.ParameterName,
            TADA.CharacteristicName
          )
        )

      # Step 12. Download the batch analysis results
      output$download_results <- shiny::downloadHandler(
        filename = function() {
          paste0("Batch_Results_", tadat$default_outfile, ".zip")
        },
        content = function(file) {
          temp_dir <- tempdir()
          batch_result_path <- file.path(
            temp_dir,
            "TADAShinyAnalyze_batch_analysis_result.csv"
          )
          batch_summary_path <- file.path(
            temp_dir,
            "TADAShinyAnalyze_batch_analysis_summary.csv"
          )
          progress_file_path <- file.path(temp_dir, "TADAShinyAnalyze_prog.rda")

          # Load the DOCX file
          batch_docx_source <- app_sys("extdata/ReadMe_Batch.docx")
          batch_docx_path <- file.path(temp_dir, "ReadMe_Batch.docx")
          file.copy(batch_docx_source, batch_docx_path)

          write_tadat_file <- function(tadat, filename) {
            default_outfile <- tadat$default_outfile
            job_id <- tadat$job_id
            df_batch_result <- tadat$duration_table
            df_batch_summary <- tadat$excurse_summary
            temp_dir <- tadat$temp_dir

            save(
              default_outfile,
              job_id,
              df_batch_result,
              df_batch_summary,
              temp_dir,
              file = filename
            )
          }

          write_tadat_file(tadat, progress_file_path)

          readr::write_csv(
            x = as.data.frame(tadat$duration_table),
            file = batch_result_path,
            na = ""
          )
          readr::write_csv(
            x = as.data.frame(tadat$excurse_summary),
            file = batch_summary_path,
            na = ""
          )

          utils::zip(
            zipfile = file,
            files = c(
              batch_result_path,
              batch_summary_path,
              progress_file_path,
              batch_docx_path
            ),
            flags = "-j"
          )
        }
      ) # END ~ downloadHandler

      # enable download button
      shinyjs::enable("download_results")

      # Ensure spinner is removed regardless of success or error
      shinybusy::remove_modal_spinner(
        session = shiny::getDefaultReactiveDomain()
      )
    })

    # Activate the map-table selector
    mod_map_table_selector_server("Batch_map_table_selector", tadat)

    # Subset tadat$excurse_summary if selected_monitoring_locations is ready
    shiny::observeEvent(
      c(tadat$selected_monitoring_locations, tadat$excurse_summary),
      {
        req(tadat$excurse_summary)

        selected_locs <- tadat$selected_monitoring_locations

        if (is.null(selected_locs) || length(selected_locs) == 0) {
          tadat$excursion_summary2 <- NULL
          return()
        }

        if (tadat$loc_select %in% "MLid") {
          excursion_summary2 <- tadat$excurse_summary |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs)
        } else {
          selected_aus <- tadat$site_AU_table |>
            dplyr::filter(
              TADA.MonitoringLocationIdentifier %in% selected_locs
            ) |>
            dplyr::pull(ATTAINS.AssessmentUnitIdentifier) |>
            unique()

          excursion_summary2 <- tadat$excurse_summary |>
            dplyr::filter(ATTAINS.AssessmentUnitIdentifier %in% selected_aus)
        }

        tadat$excursion_summary2 <- excursion_summary2 |>
          dplyr::mutate(
            ParameterForFilter = dplyr::coalesce(
              ATTAINS.ParameterName,
              TADA.CharacteristicName
            )
          )
      },
      ignoreNULL = FALSE
    )

    # Update parameter filter when excursion_summary2 changes
    shiny::observeEvent(
      tadat$excursion_summary2,
      {
        if (is.null(tadat$excursion_summary2)) {
          shiny::updateSelectizeInput(
            session = session,
            inputId = "parameter_filter",
            choices = character(0),
            selected = character(0)
          )
          return()
        }

        params <- sort(unique(tadat$excursion_summary2$ParameterForFilter))

        if (length(params) > 0) {
          shiny::updateSelectizeInput(
            session = session,
            inputId = "parameter_filter",
            choices = params,
            selected = params
          )
        } else {
          shiny::updateSelectizeInput(
            session = session,
            inputId = "parameter_filter",
            choices = character(0),
            selected = character(0)
          )
        }
      },
      ignoreNULL = FALSE
    )

    # Filter the tadat$excurse_summary2 by parameter
    shiny::observeEvent(
      c(tadat$excursion_summary2, input$parameter_filter),
      {
        req(tadat$loc_select)

        if (is.null(tadat$excursion_summary2)) {
          tadat$excurse_summary_f <- NULL
          return()
        }

        # Handle NULL or empty parameter filter
        if (
          is.null(input$parameter_filter) || length(input$parameter_filter) == 0
        ) {
          tadat$excurse_summary_f <- NULL
          tadat$excurse_dat_filtered <- NULL
          return()
        }

        excurse_summary3 <- tadat$excursion_summary2 |>
          dplyr::filter(ParameterForFilter %in% input$parameter_filter)

        tadat$excurse_summary_f <- excurse_summary3

        if (
          !is.null(tadat$excurse_summary_f) && nrow(tadat$excurse_summary_f) > 0
        ) {
          filtered_params <- unique(tadat$excurse_summary_f$ParameterForFilter)

          if (tadat$loc_select %in% c("MLid")) {
            filtered_locs <- unique(
              tadat$excurse_summary_f$TADA.MonitoringLocationIdentifier
            )
            tadat$excurse_dat_filtered <- tadat$excurse_dat |>
              dplyr::filter(
                ParameterForFilter %in% filtered_params,
                TADA.MonitoringLocationIdentifier %in% filtered_locs
              )
          } else {
            filtered_aus <- unique(
              tadat$excurse_summary_f$ATTAINS.AssessmentUnitIdentifier
            )
            tadat$excurse_dat_filtered <- tadat$excurse_dat |>
              dplyr::filter(
                ParameterForFilter %in% filtered_params,
                ATTAINS.AssessmentUnitIdentifier %in% filtered_aus
              )
          }
        } else {
          tadat$excurse_dat_filtered <- NULL
        }
      },
      ignoreNULL = FALSE
    )

    # Filter the tadat$exceed_summary by parameter
    shiny::observeEvent(
      c(input$parameter_filter, tadat$exceed_summary),
      {
        req(tadat$loc_select, tadat$selected_monitoring_locations)

        if (is.null(tadat$selected_monitoring_locations)) {
          tadat$exceed_summary_f <- NULL
          return()
        }

        selected_locs <- tadat$selected_monitoring_locations

        if (is.null(selected_locs) || length(selected_locs) == 0) {
          tadat$exceedance_summary2 <- NULL
          return()
        }

        if (tadat$loc_select %in% "MLid") {
          exceedance_summary2 <- tadat$exceed_summary |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% selected_locs)
        } else {
          selected_aus <- tadat$site_AU_table |>
            dplyr::filter(
              TADA.MonitoringLocationIdentifier %in% selected_locs
            ) |>
            dplyr::pull(ATTAINS.AssessmentUnitIdentifier) |>
            unique()

          exceedance_summary2 <- tadat$exceed_summary |>
            dplyr::filter(ATTAINS.AssessmentUnitIdentifier %in% selected_aus)
        }

        if (
          is.null(input$parameter_filter) || length(input$parameter_filter) == 0
        ) {
          tadat$exceed_summary_f <- NULL
        } else {
          exceedance_summary3 <- exceedance_summary2 |>
            dplyr::filter(ParameterForFilter %in% input$parameter_filter)

          tadat$exceed_summary_f <- exceedance_summary3
        }
      },
      ignoreNULL = FALSE
    )

    mod_excursion_viewer_server(
      "Summary_View",
      summary_dat = reactive(tadat$excurse_summary_f)
    )

    mod_analysis_plots_server(
      "Analysis_Plots",
      excurse_dat = reactive(tadat$excurse_dat_filtered),
      excurse_summary = reactive(tadat$excurse_summary_f),
      loc_select = reactive(tadat$loc_select),
      tabname = "batch"
    )
  })
}

## To be copied in the UI
# mod_batch_analysis_ui("batch_analysis_1")

## To be copied in the server
# mod_batch_analysis_server("batch_analysis_1")
