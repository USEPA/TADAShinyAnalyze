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
    
    # Horizontal divider
    hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    fluidRow(
      column(
        width = 12,
        column(
          width = 12,
          div(style = "display: flex; align-items: center; gap: 10px;",
              htmltools::h4("Run Batch Analysis:", style = "margin: 0;"),
              shiny::actionButton(inputId = ns("Run_Batch"),
                                  label = "Run")
          )
        )
      )
    ),
    
    # Horizontal divider
    hr(style = "border-top: 2px solid #ddd; margin: 30px 0;"),
    
    # Select the ML/AU iD
    fluidRow(
      column(
        width = 12,
        column(
          width = 6,
          shiny::selectizeInput(inputId = ns("loc_filter"),
                         label = "Filter ML/AU ID to view the results",
                         choices = NULL,
                         multiple = TRUE)
        ),
        column(
          width = 6,
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
        mod_boxplot_ui(ns("Boxplot_View"))
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
    
    ### Remove records need to be reviewed in adat$df_mltoau_input
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
      
    })
    
    ### Update the loc_filter and parameter_filter if tadat$exceed_summary is ready
    shiny::observeEvent(tadat$exceed_summary, {
      req(tadat$loc_select)
      if (tadat$loc_select %in% c("MLid", "AU_ind")){
        loc <- sort(unique(tadat$exceed_summary$TADA.MonitoringLocationIdentifier))
      } else {
        loc <- sort(unique(tadat$exceed_summary$JoinToAU.AssessmentUnitIdentifier))
      }
      
      ### TODO Find a way to not display all selected items in the UI
      shiny::updateSelectizeInput(
        session = session,
        inputId = "loc_filter",
        selected = loc,
        choices = loc
      )
      
      params <- sort(unique(tadat$exceed_summary$TADA.CharacteristicName))
      
      ### TODO Find a way to not display all selected items in the UI
      shiny::updateSelectizeInput(
        session = session,
        inputId = "parameter_filter",
        selected = params,
        choices = params
      )
    }, ignoreNULL = TRUE)
    
    # Filter the tadat$exceed_summary
    shiny::observeEvent(
      c(tadat$exceed_summary, input$loc_filter, input$parameter_filter),{
        req(tadat$loc_select)
        if (tadat$loc_select %in% c("MLid", "AU_ind")){
          exceed_summary2 <- tadat$exceed_summary |>
            dplyr::filter(TADA.MonitoringLocationIdentifier %in% 
                            input$loc_filter)
        } else {
          exceed_summary2 <- tadat$exceed_summary |>
            dplyr::filter(JoinToAU.AssessmentUnitIdentifier %in% 
                            input$loc_filter)
        }
        
        exceed_summary3 <- exceed_summary2 |>
          dplyr::filter(TADA.CharacteristicName %in% 
                          input$parameter_filter)
        
        # Save the data to tadat
        tadat$exceed_summary_f <- exceed_summary3
      
    })
    mod_exceedance_viewer_server("Summary_View", tadat)
    
    mod_map_viewer_server("Summary_Map", tadat)
    
    mod_boxplot_server("Boxplot_View", tadat)
    
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
