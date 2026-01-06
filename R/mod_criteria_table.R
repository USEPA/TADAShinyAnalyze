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
        " Generate the table based on information from any state/tribe that has submitted to ATTAINS. The tool will automatically fill in the 'ATTAINS.OrganizationIdentifier' column as 'All.' Users can review and update this column after downloading the template."
      ),
      htmltools::tags$li(
        htmltools::strong("Option D:"),
        " Generate a blank template."
      ),
      htmltools::tags$li(
        htmltools::strong("Option E:"),
        " Upload a template users have filled out and reviewed using the file uploader. The tool will check if there is any missing information."
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
            "Option D: Blank Template" = "D",
            "Option E: A Template from Users" = "E"
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
        ),
        shinyjs::hidden(
          shiny::fileInput(
            inputId = ns("upload_template"),
            label = "Choose file to load (for Option E):",
            width = "90%",
            placeholder = "No file selected.",
            multiple = FALSE,
            accept = c(".xlsx")
          )
        )
      )
    ),
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::verbatimTextOutput(outputId = ns("template_status")),
        shinyjs::hidden(shiny::downloadButton(
          outputId = ns("download_template"),
          label = "Download Template (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      )
    ),
    htmltools::br(),
    
    htmltools::hr(),
    
    htmltools::p("After reviewing the template, and updating the template if needed, upload the final template in the file uploader."),
    
    shiny::fluidRow(
      shiny::column(
        width = 12,
        
        shiny::fileInput(
          inputId = ns("review_template"),
          label = "Choose file to load:",
          width = "90%",
          placeholder = "No file selected.",
          multiple = FALSE,
          accept = c(".xlsx")
        ),
        
        htmltools::p("Summary of the uploaded template."),
        
        shiny::verbatimTextOutput(outputId = ns("template_summary")),
        
        htmltools::p("Review the template."),
        
        DT::DTOutput(outputId = ns("final_template"))

      )
    )
    
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
    
    # A function to handle the warning message
    run_with_warnings <- function(expr) {
      tryCatch(
        withCallingHandlers(
          expr,
          warning = function(w) {
            warning_msg(paste(warning_msg(), conditionMessage(w), sep = "\n"))
            invokeRestart("muffleWarning")  # Works here because we're inside withCallingHandlers
          }
        ),
        error = function(e) {
          warning_msg(paste("Error:", conditionMessage(e)))
          NULL
        }
      )
    }
    
    # UI control
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
      
      # Activate the upload_template if input$criteria_method is E
      shinyjs::toggle(id = "upload_template",
                           condition = input$criteria_method %in% c("E"))
      
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    ### Upload the template for Option E
    
    # Reactive to read the uploaded template file
    uploaded_temp_table <- shiny::eventReactive(input$upload_template, {
      
      # Validate file is selected
      shiny::validate(need(!is.null(input$upload_template), "No file selected."))
      
      # Define file path
      file_path <- input$upload_template$datapath
      file_ext <- tools::file_ext(file_path)
      
      # Log to console
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Criteria Template Import, file name: ", input$upload_template$name, "\n",
          "Criteria Template Import, file path: ", file_path, "\n"
        )
      )
      
      # User notification
      shiny::showNotification(
        paste0("Loading criteria template: ", input$upload_template$name),
        type = "message",
        duration = 5
      )
      
      # Read the Excel file
      df_template <- NULL
      
      if (file_ext %in% c("xlsx")) {
        df_template <- tryCatch({
          # Try reading the DefineCriteriaMethodology sheet first
          readxl::read_excel(file_path, 
                             sheet = "DefineCriteriaMethodology",
                             na = c("NA", ""),
                             trim_ws = TRUE, 
                             col_names = TRUE,
                             guess_max = 100000)
        }, error = function(e) {
          # If sheet name fails, try reading first sheet
          tryCatch({
            readxl::read_excel(file_path, 
                               sheet = 1,
                               na = c("NA", ""),
                               trim_ws = TRUE, 
                               col_names = TRUE,
                               guess_max = 100000)
          }, error = function(e2) {
            shiny::showNotification(
              paste("Error reading file:", e2$message),
              type = "error",
              duration = 10
            )
            return(NULL)
          })
        })
      } else {
        shiny::showNotification("Please upload an Excel file (.xlsx)", type = "error")
        return(NULL)
      }
      
      # Define required columns for criteria template
      required_cols <- c(
        "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName", 
        "ATTAINS.UseName",
        "TADA.CharacteristicName",
        "TADA.ComparableDataIdentifier"
      )
      
      # Check for missing required columns
      missing_cols <- setdiff(required_cols, names(df_template))
      
      if (length(missing_cols) > 0) {
        shiny::showNotification(
          paste0("Warning: Missing columns in template: ", 
                 paste(missing_cols, collapse = ", ")),
          type = "warning",
          duration = 10
        )
      }
      
      return(df_template)
    })
    
    ### Determine the options to generate the criteria table
    
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
          
          criteria_template <- run_with_warnings({
              TADA_DefineCriteriaMethodology_Shiny(
                .data = tadat$df_mlid_input,
                org_id = org_ID,
                auto_assign = FALSE,
                criteriaMethods = temp_table,
                AUMLRef = tadat$df_mltoau_input,
                AU_UsesRef = tadat$df_autouse_input,
                return_workbook = TRUE
              )
            })
          
        # Option B: State/Tribe from ATTAINS
        } else if (input$criteria_method %in% "B") {
          req(input$state_tribe_select)
          req(tadat$df_mlid_input)
          
          criteria_template <- run_with_warnings({
              TADA_DefineCriteriaMethodology_Shiny(
                .data = tadat$df_mlid_input,
                org_id = input$state_tribe_select,
                auto_assign = TRUE,
                criteriaMethods = NULL,
                AUMLRef = tadat$df_mltoau_input,
                AU_UsesRef = tadat$df_autouse_input,
                return_workbook = TRUE
              )
            })
          
        # Option C: Any State/Tribe from ATTAINS
        } else if (input$criteria_method %in% "C") {
          
          criteria_template <- run_with_warnings({
              TADA_DefineCriteriaMethodology_Shiny(
                .data = tadat$df_mlid_input,
                org_id = NULL,
                auto_assign = TRUE,
                criteriaMethods = NULL,
                AUMLRef = tadat$df_mltoau_input,
                AU_UsesRef = tadat$df_autouse_input,
                return_workbook = TRUE
              )
            })
          
          # Add "All" flag to the ATTAINS.OrganizationIdentifier columns
          if (!is.null(criteria_template) && !is.null(criteria_template$data)) {
            temp_c <- criteria_template$data |>
              dplyr::mutate(ATTAINS.OrganizationIdentifier = "All")

            criteria_template$data <- temp_c
          }
          
        # Option D: Blank Template
        } else if (input$criteria_method %in% "D"){
          
          criteria_template <- run_with_warnings({
              TADA_DefineCriteriaMethodology_Shiny(
                return_workbook = TRUE
              )
            })
          
        } else {
          
          req(uploaded_temp_table)
          
          temp_table <- uploaded_temp_table()
          
          # Get the org ID
          org_ID <- unique(temp_table$ATTAINS.OrganizationIdentifier)
          
          criteria_template <- run_with_warnings({
            TADA_DefineCriteriaMethodology_Shiny(
              .data = tadat$df_mlid_input,
              org_id = org_ID,
              auto_assign = FALSE,
              criteriaMethods = temp_table,
              AUMLRef = tadat$df_mltoau_input,
              AU_UsesRef = tadat$df_autouse_input,
              return_workbook = TRUE
            )
          })
          
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
        
        # Save workbook
        if (!is.null(criteria_template_rv()$workbook)) {
          openxlsx::saveWorkbook(criteria_template_rv()$workbook, excel_path, overwrite = TRUE)
        }
        
        # Create zip file
        files_to_zip <- c()
        if (file.exists(excel_path)) files_to_zip <- c(files_to_zip, excel_path)

        utils::zip(zipfile = file, files = files_to_zip, flags = "-j")
      },
      contentType = "application/zip"
    )
    
    ### Upload the reviewed criteria template
    
    # Reactive to read the uploaded template file
    review_template_input <- shiny::eventReactive(input$review_template, {
      
      # Validate file is selected
      shiny::validate(need(!is.null(input$review_template), "No file selected."))
      
      # Define file path
      file_path <- input$review_template$datapath
      file_ext <- tools::file_ext(file_path)
      
      # Log to console
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Criteria Template Import, file name: ", input$review_template$name, "\n",
          "Criteria Template Import, file path: ", file_path, "\n"
        )
      )
      
      # User notification
      shiny::showNotification(
        paste0("Loading criteria template: ", input$review_template$name),
        type = "message",
        duration = 5
      )
      
      # Read the Excel file
      df_template <- NULL
      
      if (file_ext %in% c("xlsx")) {
        df_template <- tryCatch({
          # Try reading the DefineCriteriaMethodology sheet first
          readxl::read_excel(file_path, 
                             sheet = "DefineCriteriaMethodology",
                             na = c("NA", ""),
                             trim_ws = TRUE, 
                             col_names = TRUE,
                             guess_max = 100000)
        }, error = function(e) {
          # If sheet name fails, try reading first sheet
          tryCatch({
            readxl::read_excel(file_path, 
                               sheet = 1,
                               na = c("NA", ""),
                               trim_ws = TRUE, 
                               col_names = TRUE,
                               guess_max = 100000)
          }, error = function(e2) {
            shiny::showNotification(
              paste("Error reading file:", e2$message),
              type = "error",
              duration = 10
            )
            return(NULL)
          })
        })
      } else {
        shiny::showNotification("Please upload an Excel file (.xlsx)", type = "error")
        return(NULL)
      }
      
      # Define required columns for criteria template
      required_cols <- c(
        "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName", 
        "ATTAINS.UseName",
        "TADA.CharacteristicName",
        "TADA.ComparableDataIdentifier"
      )
      
      # Check for missing required columns
      missing_cols <- setdiff(required_cols, names(df_template))
      
      if (length(missing_cols) > 0) {
        shiny::showNotification(
          paste0("Warning: Missing columns in template: ", 
                 paste(missing_cols, collapse = ", ")),
          type = "warning",
          duration = 10
        )
      }
      
      # Also save to tadat for use in other modules
      tadat$criteria_template <- df_template
      
      return(df_template)
    })
    
    ### Render template summary
    output$template_summary <- shiny::renderText({
      # Check if file was uploaded
      if (is.null(input$review_template)) {
        return("No file selected.")
      }
      
      # Call the reactive expression to get the data
      df_template <- review_template_input()
      
      if (is.null(df_template)) {
        return("Error loading file. Please check the file format.")
      }
      
      # Build summary text
      paste0(
        "Loaded dataset has ", nrow(df_template), " rows.\n",
        "There are ", length(unique(df_template$ATTAINS.OrganizationIdentifier)), " unique organization(s).\n",
        "There are ", length(unique(df_template$TADA.CharacteristicName)), " unique TADA characteristic name(s).\n",
        "There are ", length(unique(df_template$ATTAINS.UseName)), " unique use type(s).\n",
        "There are ", length(unique(df_template$TADA.ComparableDataIdentifier)), " unique TADA.ComparableDataIdentifier(s)."
      )
    })
    
    ### Generate the template summary table
    output$final_template <- DT::renderDT({
      # Validate file is uploaded
      shiny::validate(need(!is.null(input$review_template), "No file selected."))
      
      # Get the data from the reactive
      df_template <- review_template_input()
      
      shiny::validate(need(!is.null(df_template), "Error loading file."))
      
      # Render table
      DT::datatable(df_template,
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   scrollY = "400px",
                                   scrollCollapse = TRUE,
                                   paging = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE))
    })
    
    # Activate the batch and custom tabs if the final criteria table is ready
    shiny::observe({
      
      # Validate file is uploaded
      shiny::validate(need(!is.null(input$review_template), "No file selected."))

      req(review_template_input())

      df_template <- review_template_input()

      # Define required columns for criteria template
      required_cols <- c(
        "ATTAINS.OrganizationIdentifier",
        "ATTAINS.ParameterName",
        "ATTAINS.UseName",
        "TADA.CharacteristicName",
        "TADA.ComparableDataIdentifier"
      )

      # Check for missing required columns
      missing_cols <- setdiff(required_cols, names(df_template))

      if (length(missing_cols) == 0){
        shinyjs::enable(selector = '.nav li a[data-value="Batch"]')
        shinyjs::enable(selector = '.nav li a[data-value="Custom"]')
      } else {
        shinyjs::disable(selector = '.nav li a[data-value="Batch"]')
        shinyjs::disable(selector = '.nav li a[data-value="Custom"]')
      }})
    
    
  }) 
}

## To be copied in the UI
# mod_criteria_table_ui("criteria_table_1")

## To be copied in the server
# mod_criteria_table_server("criteria_table_1")
