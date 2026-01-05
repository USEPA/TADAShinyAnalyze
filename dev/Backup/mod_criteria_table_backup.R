#' criteria_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs useShinyjs show hide

mod_criteria_table_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    
    # header
    htmltools::h2("2. Criteria Table"),
    
    # Instructions
    htmltools::p("Use this tab to generate the criteria table based on the following options:"),
    htmltools::tags$ul(
      htmltools::tags$li(
        htmltools::strong("Option A:"), 
        " Generate the table using one of the templates available in the TADA Community Hub, including EPA Region 8 states and tribes, EPA 304(a) criteria & methods, and other TADA users."
      ),
      htmltools::tags$li(
        htmltools::strong("Option B:"), 
        " Generate the table based on information from a specific state/tribe that has submitted to ATTAINS."
      ),
      htmltools::tags$li(
        htmltools::strong("Option C:"), 
        " Generate the table based on information from any state/tribe that has submitted to ATTAINS."
      ),
      htmltools::tags$li(
        htmltools::strong("Option D:"),
        " Generate a blank template."
      )
    ),
    htmltools::p('Once the selection is completed, click the "Generate Template" button to generate an Excel file with the criteria table template.'),
    
    htmltools::hr(),
    
    # Options to generate the draft criteria and methods template
    shiny::fluidRow(
      # instructions column
      shiny::column(
        width = 6,
        shiny::radioButtons(
          inputId = ns("criteria_method"),
          label = "Select the method to generate a draft criteria and methods template.",
          choices = c(
            "Option A: TADA Community Hub Templates" = "A",
            "Option B: State/Tribe from ATTAINS" = "B",
            "Option C: Any State/Tribe from ATTAINS" = "C",
            "Option D: Blank Template" = "D"
          ),
          width = "100%"
        ),
        shiny::actionButton(ns("Generate_Template"), "Generate Template", shiny::icon("computer"),
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ),
      shiny::column(
        width = 6,
        shinyjs::disabled(
          shiny::selectInput(
            inputId = ns("state_tribe_select"),
            label = "Select the State/Tribe",
            choices = ATTAINS_orgs_vec
          )
        )
      )
    ),
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::verbatimTextOutput(outputId = ns("template_status")),
        shinyjs::disabled(shiny::downloadButton(
          outputId = ns("download_template"),
          label = "Download Template (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      )
    ),
    htmltools::br()
  )
}

#' criteria_table Server Functions
#'
#' @noRd 
mod_criteria_table_server <- function(id, tadat) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Enable or disable the state_tribe_select
    shiny::observeEvent(input$criteria_method, {
      
      # If input$criteria_method is B, use criteria_file_list instead
      if (input$criteria_method %in% "B"){
        shiny::updateSelectInput(
          session = session,
          inputId = "state_tribe_select",
          choices = criteria_file_list$display_name
        )
      } else {
        shiny::updateSelectInput(
          session = session,
          inputId = "state_tribe_select",
          choices = ATTAINS_orgs_vec
        )
      }

      # Activate state_tribe_select if input$criteria_method is A or B
      shinyjs::toggleState(id = "state_tribe_select",
                           condition = input$criteria_method %in% c("A", "B"))
      
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # Run the TADA_DefineCriteriaMethodology_Shiny functon to get the criteria table
    shiny::observeEvent(input$Generate_Template, {
      req(input$criteria_method, input$state_tribe_select)
      
      # Option A
      if (input$criteria_method %in% "A"){
        
        # Get the criteria table from the TADACommunityHub
        temp_table <- loadCriteria(input$state_tribe, ref = criteria_file_list)
        
        # Get the org ID
        org_ID <- unique(temp_table$ATTAINS.OrganizationIdentifier)
        
        criteria_template <- TADA_DefineCriteriaMethodology_Shiny(
          .data = tadat$files_loaded_mlid,
          org_id = org_ID,
          auto_assign = FALSE,
          criteriaMethods = temp_table,
          AUMLRef = NULL,
          AU_UsesRef = NULL,
          return_workbook = TRUE
        )
        
        # Option B
      } else if (input$criteria_method %in% "B"){
        
        criteria_template <- TADA_DefineCriteriaMethodology_Shiny(
          .data = tadat$files_loaded_mlid,
          org_id = input$state_tribe_select,
          auto_assign = TRUE,
          criteriaMethods = NULL,
          AUMLRef = NULL,
          AU_UsesRef = NULL,
          return_workbook = TRUE
        )
        
        # Option C
      } else if (input$criteria_method %in% "C"){
        
        criteria_template <- TADA_DefineCriteriaMethodology_Shiny(
          .data = tadat$files_loaded_mlid,
          org_id = NULL,
          auto_assign = TRUE,
          criteriaMethods = NULL,
          AUMLRef = NULL,
          AU_UsesRef = NULL,
          return_workbook = TRUE
        )
        
        # Add "All" flag to the ATTAINS.OrganizationIdentifier columns
        temp_c <- criteria_template$data |>
          dplyr::mutate(ATTAINS.OrganizationIdentifier = "All") 
        
        criteria_template$data <- temp_c
        
        # Option D
      } else {
        
        criteria_template <- TADA_DefineCriteriaMethodology_Shiny(
          return_workbook = TRUE
        )
        
      }}
      
      # Text output to show the status of the template
      output$template_status <- shiny::renderText({
        "Ready to generate template. Select options above and click 'Generate Template'."
      })
      
    )
    
    # Download the Excel workbook with criteria_template
    
    # Upload the edited criteria_template
    
    # Create a summary to view the data
    
    # View the table
    
    # Activate tabs 3 and 4
    
  }) 
}

## To be copied in the UI
# mod_criteria_table_ui("criteria_table_1")

## To be copied in the server
# mod_criteria_table_server("criteria_table_1")