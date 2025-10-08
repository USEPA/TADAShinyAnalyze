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
        htmltools::p("If only Water Quality Data File is available 
                     or if users select the EPA 304(a) option in the 'Select state/tribe of the criteria',
                     The AU option would not be available for the 'Custom Analyzed by the spatial unit'. In this case, 
                     the tool will not match the ATTAINS.UseName from the criteria table to the assessment units.")
      )
    ),
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(inputId = ns("loc_select_custom"),
                            label = "Custom analyzed by the spatial unit: ",
                            choices = c("Monitoring Location ID" = "MLid",
                                        "Assessment Unit" = "AU",
                                        "Custom Gouping (Use the following map-table selector to select sites to group)" = "CG"))
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
    ),
    fluidRow(
      column(width = 6,
             shiny::radioButtons(inputId = ns("join_select_custom"),
                                 label = "Join the criteria table with fraction information",
                                 choices = c("Yes" = "Option 1", 
                                             "No" = "Option 2")))
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
        choices = org_options
      )
    }, ignoreNULL = TRUE)
    
    # An observe block to determine the use_type
    shiny::observeEvent(list(tadat$df_mlid_input, tadat$files_loaded_mlid), {
      if (isTRUE(tadat$files_loaded_mlid)) {
        shiny::updateSelectizeInput(
          session = session,
          inputId = "state_tribe_custom",
          options = list(placeholder = "Select the state/tribe", maxItems = 1),
          selected = character(0),
          choices = org_options
        )
      }
    }, ignoreNULL = TRUE)
    
    # An observe block to determine the use_type
    shiny::observe({
      # Check if all three files are loaded
      if (isTRUE(tadat$files_loaded_mlid) && 
          isTRUE(tadat$files_loaded_mltoau) && 
          isTRUE(tadat$files_loaded_autouse)) {
        # All files are loaded - check if user selected default criteria
        if (isTRUE(input$state_tribe_custom %in% "D")) {
          use_type <- "Option 2"  # Default criteria selected
        } else {
          use_type <- "Option 1"  # Use crosswalk files
        }
      } else if (isTRUE(tadat$files_loaded_mlid)) {
        # Only main file is loaded, crosswalk files missing or incomplete
        use_type <- "Option 2"
      } else {
        # No files loaded yet
        use_type <- "Option 2"
      }
      
      tadat$use_type_custom <- use_type
      
      print(paste("use_type_custom:", tadat$use_type_custom))
      print(paste("files loaded - mlid:", tadat$files_loaded_mlid, 
                  "mltoau:", tadat$files_loaded_mltoau, 
                  "autouse:", tadat$files_loaded_autouse))
    })
    
    # Update the loc_select_custom choices based on use_type
    shiny::observeEvent(tadat$use_type_custom, {
      if (tadat$use_type_custom %in% "Option 2") {
        # Remove AU option when Option 2
        choices <- c("Monitoring Location ID" = "MLid",
                     "Custom Grouping (Use the following map-table selector to select sites to group)" = "CG")
        
        # Check if current selection is AU and change it to MLid
        current_selection <- isolate(input$loc_select_custom)
        if (!is.null(current_selection) && current_selection %in% "AU") {
          selected <- "MLid"
        } else if (!is.null(current_selection) && current_selection %in% c("MLid", "CG")) {
          selected <- current_selection
        } else {
          selected <- "MLid"
        }
      } else {
        # Include AU option when Option 1
        choices <- c("Monitoring Location ID" = "MLid",
                     "Assessment Unit" = "AU",
                     "Custom Grouping (Use the following map-table selector to select sites to group)" = "CG")
        
        # Keep current selection if valid
        current_selection <- isolate(input$loc_select_custom)
        if (!is.null(current_selection) && current_selection %in% c("MLid", "AU", "CG")) {
          selected <- current_selection
        } else {
          selected <- "MLid"
        }
      }
      
      shiny::updateRadioButtons(
        session = session,
        inputId = "loc_select_custom",
        choices = choices,
        selected = selected
      )
    })
    
    # Update the available uses when state/tribe changes
    shiny::observeEvent(c(input$state_tribe_custom, tadat$use_type_custom), {
      req(input$state_tribe_custom)
      req(tadat$use_type_custom)
      
      if (tadat$use_type_custom %in% "Option 1"){
        req(tadat$df_autouse_input)
        
        criteria_table_f1 <- criteria_table |>
          dplyr::filter(ATTAINS.OrganizationIdentifier %in% input$state_tribe_custom)
        
        # Get the list of available uses from criteria_table_f1
        criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)
        
        AU_Use_uses <- unique(tadat$df_autouse_input$ATTAINS.UseName)
        
        # Find the intersection
        available_uses <- base::intersect(criteria_uses, AU_Use_uses)
        
      } else {
        
        criteria_table_f1 <- criteria_table |>
          dplyr::filter(ATTAINS.OrganizationIdentifier %in% input$state_tribe_custom)
        
        # Get the list of available uses from criteria_table_f1
        criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)
        
        # Find the intersection
        available_uses <- criteria_uses
        
      }
      
      # Save available_uses to tadat
      tadat$available_uses_custom <- available_uses
      
      # Reset uses selection when state/tribe changes
      if (input$uses_all_custom) {
        tadat$uses_select_re_custom <- available_uses
      } else {
        # Keep only uses that are still available
        current_uses <- isolate(tadat$uses_select_re_custom)
        if (!is.null(current_uses) && length(current_uses) > 0) {
          valid_uses <- intersect(current_uses, available_uses)
          tadat$uses_select_re_custom <- if(length(valid_uses) > 0) valid_uses else available_uses[1]
        } else {
          tadat$uses_select_re_custom <- if(length(available_uses) > 0) available_uses[1] else character(0)
        }
      }
      
      # Update the UI
      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "uses_select_custom",
        choices = sort(available_uses),
        selected = tadat$uses_select_re_custom
      )
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
        # When unchecking, maintain current selection if valid, otherwise select first
        if (!is.null(tadat$uses_select_re_custom) && length(tadat$uses_select_re_custom) > 0) {
          selected_uses <- tadat$uses_select_re_custom
        } else if (length(tadat$available_uses_custom) > 0) {
          selected_uses <- tadat$available_uses_custom[1]
        } else {
          selected_uses <- character(0)
        }
        
        shinyWidgets::updateVirtualSelect(
          session = session,
          inputId = "uses_select_custom",
          choices = sort(tadat$available_uses_custom),
          selected = selected_uses
        )
        tadat$uses_select_re_custom <- selected_uses
      }
    }, ignoreInit = TRUE)
    
    # Handle uses_select changes separately
    shiny::observeEvent(input$uses_select_custom, {
      # Only update when checkbox is unchecked
      if (!input$uses_all_custom) {
        if (!is.null(input$uses_select_custom) && length(input$uses_select_custom) > 0) {
          tadat$uses_select_re_custom <- input$uses_select_custom
        }
      }
    }, ignoreNULL = FALSE)
    
    ### Save the selected loc_select, state_tribe and uses to tadat
    shiny::observe({
      tadat$loc_select_custom <- input$loc_select_custom
      tadat$state_tribe_custom <- input$state_tribe_custom
      tadat$join_select_custom <- input$join_select_custom
    })
  })
}

## To be copied in the UI
# mod_analysis_selector_custom_ui("analysis_selector_custom_1")

## To be copied in the server
# mod_analysis_selector_custom_server("analysis_selector_custom_1")