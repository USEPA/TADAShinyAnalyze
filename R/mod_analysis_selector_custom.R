#' analysis_selector_custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_selector_custom_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        htmltools::p("Determine the spatial unit, state/tribe of the criteria, and the uses included in the analysis."),
      )
    ),
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(inputId = ns("loc_select_custom"),
                            label = "Batch Analyzed by the spatial unit: ",
                            choices = c("Monitoring Location ID" = "MLid",
                                        "Assessment Unit" = "AU"))
      ),
      column(
        width = 3,
        shiny::selectizeInput(inputId = ns("state_tribe_custom"),
                              label = "Select state/tribe of the criteria",
                              choices = NULL),
        shiny::checkboxInput(inputId = ns("uses_all_custom"),
                             label = "Select all uses",
                             value = TRUE)
      ),
      column(
        width = 3,
        shinyWidgets::virtualSelectInput(
          inputId = ns("uses_select_custom"),
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
mod_analysis_selector_custom_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update the Select state/tribe menu
    shiny::observeEvent(tadat$df_mltoau_input, {
      shiny::updateSelectizeInput(
        session = session,
        inputId = "state_tribe_custom",
        options = list(placeholder = "Select the state/tribe", maxItems = 1),
        selected = character(0),
        choices = sort(unique(criteria_table$ATTAINS.OrganizationIdentifier))
      )
    }, ignoreNULL = TRUE)
    
    # Update the available uses
    shiny::observeEvent(c(input$state_tribe_custom, tadat$df_autouse_input), {
      req(input$state_tribe_custom)
      req(tadat$df_autouse_input)
      
      criteria_table_f1 <- criteria_table |>
        dplyr::filter(ATTAINS.OrganizationIdentifier %in% input$state_tribe_custom)
      
      # Get the list of available uses from criteria_table_f1
      criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)
      
      AU_Use_uses <- unique(tadat$df_autouse_input$ATTAINS.UseName)
      
      # Find the intersection
      available_uses <- base::intersect(criteria_uses, AU_Use_uses)
      
      # Save available_uses to tadat
      tadat$available_uses_custom <- available_uses
      
    }, ignoreNULL = TRUE)
    
    # Initialize uses_select_re
    shiny::observe({
      req(tadat$available_uses_custom)
      if (is.null(tadat$uses_select_re_custom)) {
        tadat$uses_select_re_custom <- if(isolate(input$uses_all_custom)) {
          tadat$available_uses_custom
        } else {
          character(0)
        }
      }
    })
    
    # Handle checkbox changes
    shiny::observeEvent(input$uses_all_custom, {
      req(tadat$available_uses_custom)
      
      if (input$uses_all_custom) {
        shinyjs::disable("uses_select_custom")
        tadat$uses_select_re_custom <- tadat$available_uses_custom
        # Update the select to show all selected (visual consistency)
        shinyWidgets::updateVirtualSelect(
          session = session,
          inputId = "uses_select_custom",
          choices = sort(tadat$available_uses_custom),
          selected = tadat$available_uses_custom
        )
      } else {
        shinyjs::enable("uses_select_custom")
        shinyWidgets::updateVirtualSelect(
          session = session,
          inputId = "uses_select_custom",
          choices = sort(tadat$available_uses_custom),
          selected = tadat$uses_select_re_custom  # Maintain current selection
        )
        # Don't update tadat$uses_select_re here
      }
    }, ignoreInit = FALSE)
    
    # Handle uses_select changes separately
    shiny::observeEvent(input$uses_select_custom, {
      # Only update when checkbox is unchecked AND uses_select is not disabled
      if (!input$uses_all_custom && !is.null(input$uses_select_custom)) {
        tadat$uses_select_re_custom <- input$uses_select_custom
      }
    }, ignoreNULL = FALSE)  # Important: Allow empty selections
    
    ### Save the selected loc_select, state_tribe and uses to tadat
    shiny::observe({
      tadat$loc_select_custom <- input$loc_select_custom
      tadat$state_tribe_custom <- input$state_tribe_custom
    })
  })
}

## To be copied in the UI
# mod_analysis_selector_custom_ui("analysis_selector_custom_1")
    
## To be copied in the server
# mod_analysis_selector_custom_server("analysis_selector_custom_1")
