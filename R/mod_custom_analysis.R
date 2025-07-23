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
    
    # Horizontal divider
    hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 6,
          shiny::selectizeInput(inputId = ns("loc_filter_custom"),
                         label = "Filter ML/AU ID to view the results",
                         choices = NULL,
                         multiple = TRUE),
          shiny::checkboxInput(inputId = ns("custom_group"),
                               label = "Group the ML/AU ID for analysis"),
          shiny::selectizeInput(inputId = ns("parameter_filter_custom"),
                         label = "Filter parameter to view the results",
                         choices = NULL,
                         multiple = TRUE)
        ),
        column(
          width = 6,
          div(style = "display: flex; align-items: center; gap: 10px;",
              htmltools::h4("Run Custom Analysis:", style = "margin: 0;"),
              shiny::actionButton(inputId = ns("Run_Custom"),
                                  label = "Run")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        mod_exceedance_viewer_custom_ui(ns("Summary_View_Custom"))
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
                 tadat$uses_select_custom)
      
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
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_custom)
      
      # Filter the AU_Use based on available_uses_s
      AU_Use <- tadat$df_autouse_input
      AU_MLID <- tadat$df_mltoau_input_f |>
        dplyr::mutate(TADA.MonitoringLocationIdentifier = 
                        stringr::str_to_upper(MonitoringLocationIdentifier))
      
      AU_Use_f1 <- AU_Use |>
        dplyr::filter(ATTAINS.UseName %in% tadat$uses_select_custom)
      
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
        criteria_join(criteria_table_f1)
      
      # Save the data
      tadat$custom_raw <- dat4
      
    })
    
    ### Based on tadat$custom_raw, update loc_filter_custom and parameter_filter_custom
    shiny::observeEvent(tadat$custom_raw, {
      if (tadat$loc_select_custom %in% c("MLid")){
        loc <- sort(unique(tadat$custom_raw$TADA.MonitoringLocationIdentifier))
      } else {
        loc <- sort(unique(tadat$custom_raw$JoinToAU.AssessmentUnitIdentifier))
      }
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "loc_filter_custom",
        selected = NULL,
        choices = loc
      )
    })
    
    # Update tadat$custom_raw based on loc_filter_custom
    shiny::observeEvent(input$loc_filter_custom, {
      req(tadat$custom_raw)
      
      if (tadat$loc_select_custom %in% c("MLid")){
        dat <- tadat$custom_raw |>
          dplyr::filter(TADA.MonitoringLocationIdentifier %in% input$loc_filter_custom)
      } else {
        dat <- tadat$custom_raw |>
          dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% input$loc_filter_custom)
      }
      
      # Save the data
      tadat$custom_raw2 <- dat
      
    })
    
    ### Based on tadat$custom_raw2, update parameter_filter_custom
    shiny::observeEvent(tadat$custom_raw2, {
      
      params <- sort(unique(tadat$custom_raw2$TADA.CharacteristicName))
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "parameter_filter_custom",
        selected = NULL,
        choices = params
      )
    })
    
    # Update tadat$custom_raw based on parameter_filter_custom
    shiny::observeEvent(input$parameter_filter_custom, {
      req(tadat$custom_raw2)
      
      dat <- tadat$custom_raw2 |>
        dplyr::filter(TADA.CharacteristicName %in% input$parameter_filter_custom)
      
      # Save the data
      tadat$custom_raw3 <- dat
      
    })
    
    ### Run the analysis if tadat$custom_raw3 is ready
    shiny::observeEvent(input$Run_Custom, {
      req(tadat$custom_raw3)
      
      dat4 <- tadat$custom_raw3
      
      ### Step 3: Separate the dataset based on if criteria exist
      dat_na <- dat4 |> dplyr::filter(is.na(EquationBased))
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
      
      # Save the data to tadat
      tadat$exceed_summary_custom <- dat6
    })
    mod_exceedance_viewer_custom_server("Summary_View_Custom", tadat)
    
  })
}
    
## To be copied in the UI
# mod_custom_analysis_ui("custom_analysis_1")
    
## To be copied in the server
# mod_custom_analysis_server("custom_analysis_1")
