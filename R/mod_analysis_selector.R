#' analysis_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

# Load files
data_path1 <- app_sys("extdata/Criteria_Table_Input.RData")
load(data_path1)

mod_analysis_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(inputId = ns("loc_select"),
                            label = "Batch Analyzed by: ",
                            choices = c("Monitoring Location ID" = "MLid",
                                        "Assessment Unit (Individual)" = "AU_ind",
                                        "Assessment Unit (Group)" = "AU_group"))
      ),
      column(
        width = 6,
        shiny::selectizeInput(inputId = ns("state_tribe"),
                              label = "Select state/tribe",
                              choices = NULL),
        shinyWidgets::virtualSelectInput(
          inputId = ns("uses_select"),
          label = "Select the uses:",
          choices = NULL,
          showValueAsTags = TRUE,
          search = TRUE,
          multiple = TRUE
        )
      )
    )
  )
}
    
#' analysis_selector Server Functions
#'
#' @noRd 
mod_analysis_selector_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update the Select state/tribe menu
    shiny::observeEvent(tadat$df_mltoau_input, {
      shiny::updateSelectizeInput(
        session = session,
        inputId = "state_tribe",
        options = list(placeholder = "Select the state/tribe", maxItems = 1),
        selected = character(0),
        choices = sort(unique(criteria_table$ATTAINS.OrganizationIdentifier))
      )
    }, ignoreNULL = TRUE)
    
    # Update the available uses
    shiny::observeEvent(c(input$state_tribe, tadat$df_autouse_input), {
      req(input$state_tribe)
      req(tadat$df_autouse_input)
      
      criteria_table_f1 <- criteria_table |>
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% input$state_tribe)
      
      # Get the list of available uses from criteria_table_f1
      criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)
      
      AU_Use_uses <- unique(tadat$df_autouse_input$ATTAINS.UseName)
      
      # Find the intersection
      available_uses <- base::intersect(criteria_uses, AU_Use_uses)
      
      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "uses_select",
        choices = sort(available_uses)
      )
    }, ignoreNULL = TRUE)
    
    ### Save the selected loc_select, state_tribe and uses to tadat
    shiny::observeEvent(input$loc_select, {
      tadat$loc_select <- input$loc_select
    })
    
    shiny::observeEvent(input$state_tribe, {
      tadat$state_tribe <- input$state_tribe
    })
    
    shiny::observeEvent(input$uses_select, {
      tadat$uses_select <- input$uses_select
    })
    
    
  })
}
    
## To be copied in the UI
# mod_analysis_selector_ui("analysis_selector_1")
    
## To be copied in the server
# mod_analysis_selector_server("analysis_selector_1")
