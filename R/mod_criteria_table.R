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

# Get the organization ID
ATTAINS_orgs <- rExpertQuery::EQ_DomainValues("org_id")
ATTAINS_orgs_vec <- ATTAINS_orgs$code
names(ATTAINS_orgs_vec) <- ATTAINS_orgs$name

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
    htmltools::p('Once the selection is completed, click the "Generate Template" button to generate an Excel file with the criteria table template. When the template is ready, click "Download Template" to save.'),
    
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
    
    # Reactive value to store warnings
    warning_msg <- shiny::reactiveVal(NULL)
    
    # Reactive value to store the criteria template
    criteria_template_rv <- shiny::reactiveVal(NULL)
    
    # Enable or disable the state_tribe_select
    shiny::observeEvent(input$criteria_method, {
      
      # If input$criteria_method is A, use criteria_file_list instead
      if (input$criteria_method %in% "A"){
        shiny::updateSelectInput(
          session = session,
          inputId = "state_tribe_select",
          choices = tadat$criteria_file_list$display_name
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
    
    # Run the TADA_DefineCriteriaMethodology_Shiny function to get the criteria table
    shiny::observeEvent(input$Generate_Template, {
      req(input$criteria_method)
      
      # Temporarily reset warn option to allow warning capture
      options(warn = 1)
      
      # Reset warning message
      warning_msg(NULL)
      
      # Show loading spinner
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Generating criteria template...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      criteria_template <- NULL
      
      tryCatch({
        # Option A: TADA Community Hub Templates
        if (input$criteria_method %in% "A") {
          req(input$state_tribe_select)
          
          # Get the criteria table from the TADACommunityHub
          temp_table <- loadCriteria(input$state_tribe_select, ref = tadat$criteria_file_list)
          
          # Get the org ID
          org_ID <- unique(temp_table$ATTAINS.OrganizationIdentifier)
          
          criteria_template <- tryCatch(
            {
              TADA_DefineCriteriaMethodology_Shiny(
                .data = tadat$df_mlid_input,
                org_id = org_ID,
                auto_assign = FALSE,
                criteriaMethods = temp_table,
                AUMLRef = tadat$df_mltoau_input,
                AU_UsesRef = tadat$df_autouse_input,
                return_workbook = TRUE
              )
            },
            warning = function(w) {
              warning_msg(paste(warning_msg(), conditionMessage(w), sep = "\n"))
              invokeRestart("muffleWarning")
            },
            error = function(e) {
              warning_msg(paste("Error:", conditionMessage(e)))
              NULL
            }
          )
          
        # Option B: State/Tribe from ATTAINS
        } else if (input$criteria_method %in% "B") {
          req(input$state_tribe_select)
          req(tadat$df_mlid_input)
          
          criteria_template <- tryCatch(
            {
              TADA_DefineCriteriaMethodology_Shiny(
                .data = tadat$df_mlid_input,
                org_id = input$state_tribe_select,
                auto_assign = TRUE,
                criteriaMethods = NULL,
                AUMLRef = tadat$df_mltoau_input,
                AU_UsesRef = tadat$df_autouse_input,
                return_workbook = TRUE
              )
            },
            warning = function(w) {
              warning_msg(paste(warning_msg(), conditionMessage(w), sep = "\n"))
              invokeRestart("muffleWarning")
            },
            error = function(e) {
              warning_msg(paste("Error:", conditionMessage(e)))
              NULL
            }
          )
          
        # Option C: Any State/Tribe from ATTAINS
        } else if (input$criteria_method %in% "C") {
          
          criteria_template <- tryCatch(
            {
              TADA_DefineCriteriaMethodology_Shiny(
                .data = tadat$df_mlid_input,
                org_id = NULL,
                auto_assign = TRUE,
                criteriaMethods = NULL,
                AUMLRef = tadat$df_mltoau_input,
                AU_UsesRef = tadat$df_autouse_input,
                return_workbook = TRUE
              )
            },
            warning = function(w) {
              warning_msg(paste(warning_msg(), conditionMessage(w), sep = "\n"))
              invokeRestart("muffleWarning")
            },
            error = function(e) {
              warning_msg(paste("Error:", conditionMessage(e)))
              NULL
            }
          )
          
          # Add "All" flag to the ATTAINS.OrganizationIdentifier columns
          if (!is.null(criteria_template) && !is.null(criteria_template$data)) {
            temp_c <- criteria_template$data |>
              dplyr::mutate(ATTAINS.OrganizationIdentifier = "All") 
            
            criteria_template$data <- temp_c
          }
          
        # Option D: Blank Template
        } else {
          
          criteria_template <- tryCatch(
            {
              TADA_DefineCriteriaMethodology_Shiny(
                return_workbook = TRUE
              )
            },
            warning = function(w) {
              warning_msg(paste(warning_msg(), conditionMessage(w), sep = "\n"))
              invokeRestart("muffleWarning")
            },
            error = function(e) {
              warning_msg(paste("Error:", conditionMessage(e)))
              NULL
            }
          )
          
        }
        
        # Store the result
        criteria_template_rv(criteria_template)
        
      }, error = function(e) {
        warning_msg(paste("Error generating template:", conditionMessage(e)))
        criteria_template_rv(NULL)
      })
      
      # Set warn back to 2
      options(warn = 2)
      
      # Remove spinner
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
      # Enable download button if template was generated successfully
      if (!is.null(criteria_template_rv())) {
        shinyjs::enable("download_template")
      } else {
        shinyjs::disable("download_template")
      }
    })
    
    # Text output to show the status of the template
    output$template_status <- shiny::renderText({
      if (is.null(criteria_template_rv())) {
        if (!is.null(warning_msg()) && nchar(trimws(warning_msg())) > 0) {
          paste("Template generation issue:\n", warning_msg())
        } else {
          "Ready to generate template. Select options above and click 'Generate Template'."
        }
      } else {
        status_text <- "Template generated successfully! Click 'Download Template' to save."
        if (!is.null(warning_msg()) && nchar(trimws(warning_msg())) > 0) {
          paste(status_text, "\n\nWarnings:\n", warning_msg())
        } else {
          status_text
        }
      }
    })
    
    # Download the Excel workbook with criteria_template
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        paste0("Criteria_Template_", format(Sys.time(), "%Y%m%d%H%M%S"), ".zip")
      },
      content = function(file) {
        req(criteria_template_rv())
        
        # Create temp directory
        temp_dir <- tempdir()
        
        # Define file paths
        excel_path <- file.path(temp_dir, "Criteria_Methods_Template.xlsx")
        csv_path <- file.path(temp_dir, "Criteria_Methods_Template.csv")
        
        # Save workbook
        if (!is.null(criteria_template_rv()$workbook)) {
          openxlsx::saveWorkbook(criteria_template_rv()$workbook, excel_path, overwrite = TRUE)
        }
        
        # Save data as CSV
        if (!is.null(criteria_template_rv()$data)) {
          readr::write_csv(criteria_template_rv()$data, csv_path, na = "")
        }
        
        # Create zip file
        files_to_zip <- c()
        if (file.exists(excel_path)) files_to_zip <- c(files_to_zip, excel_path)
        if (file.exists(csv_path)) files_to_zip <- c(files_to_zip, csv_path)
        
        utils::zip(zipfile = file, files = files_to_zip, flags = "-j")
      },
      contentType = "application/zip"
    )
    
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
