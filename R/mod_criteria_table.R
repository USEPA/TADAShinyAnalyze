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
    htmltools::p(
      "After you create the Criteria and Methodology template, open the Excel file. Check the Data Dictionary and Allowable Values tabs, and use the drop-down options where shown so the analysis works correctly.",
      style = "font-weight: bold; color: red;"
    ),
    htmltools::p(
      "Use this tab to generate the criteria table based on the following options:"
    ),
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
    htmltools::p(
      'Once the selection is completed, click the "Generate and Download Template" button to generate an Excel file with the criteria table template.'
    ),

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
            "Option E: User Supplied Template" = "E"
          ),
          width = "100%"
        ),
        htmltools::br(),
        htmltools::h4(
          "Print all unique TADA.ComparableDataIdentifier in the criteria table"
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("criteria_displayUniqueId"),
          label = "",
          status = "info"
        ),
        shinyjs::disabled(shiny::actionButton(
          ns("Generate_Template"),
          "Generate and Download Template",
          shiny::icon("computer"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ))
      ),
      shiny::column(
        width = 6,
        shinyjs::hidden(shiny::selectInput(
          inputId = ns("state_tribe_select"),
          label = "Select the State/Tribe",
          choices = character(0)
        )),
        shinyjs::hidden(shiny::fileInput(
          inputId = ns("upload_template"),
          label = "Choose file to load (for Option E):",
          width = "90%",
          placeholder = "No file selected.",
          multiple = FALSE,
          accept = c(".xlsx")
        )),
        shinyjs::hidden(shiny::selectInput(
          inputId = ns("state_tribe_select_OP_E"),
          label = "Select the State/Tribe (from the uploaded criteria table file)",
          choices = character(0)
        ))
      )
    ),
    htmltools::br(),
    shiny::fluidRow(shiny::column(
      width = 12,
      shiny::verbatimTextOutput(outputId = ns("template_status"))
      # shinyjs::disabled(shiny::downloadButton(
      #   outputId = ns("download_template"),
      #   label = "Download Template (.xlsx)",
      #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      #)
    )),
    htmltools::br(),

    htmltools::hr(),

    htmltools::p(
      "After reviewing the template, and updating the template if needed, upload the final template in the file uploader."
    ),

    shiny::fluidRow(shiny::column(
      width = 12,

      shinyjs::hidden(shiny::fileInput(
        inputId = ns("review_template"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = c(".xlsx")
      )),

      htmltools::p("Summary of the uploaded template."),

      shiny::verbatimTextOutput(outputId = ns("template_summary")),

      htmltools::p("Review the template."),

      DT::DTOutput(outputId = ns("final_template"))
    ))
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

    # UI control
    shiny::observeEvent(
      c(input$criteria_method, input$state_tribe_select_OP_E),
      {
        # If input$criteria_method is A, use criteria_file_list instead
        if (input$criteria_method %in% "A") {
          shiny::updateSelectInput(
            session = session,
            inputId = "state_tribe_select",
            choices = tadat$criteria_file_list$display_name
          )
        } else {
          shiny::updateSelectInput(
            session = session,
            inputId = "state_tribe_select",
            choices = tadat$ATTAINS_orgs_vec
          )
        }

        # Activate state_tribe_select if input$criteria_method is A or B
        shinyjs::toggle(
          id = "state_tribe_select",
          condition = input$criteria_method %in% c("A", "B")
        )

        # Activate the upload_template if input$criteria_method is E
        shinyjs::toggle(
          id = "upload_template",
          condition = input$criteria_method %in% "E"
        )

        # Activate the state_tribe_select_OP_E if input$criteria_method is E
        shinyjs::toggle(
          id = "state_tribe_select_OP_E",
          condition = input$criteria_method %in% "E"
        )

        # Activate the Generate Template button if input$criteria_method %in% "E"
        # and input$state_tribe_select_OP_E" is not ""
        shinyjs::toggleState(
          id = "Generate_Template",
          condition = input$criteria_method %in%
            c("A", "B", "C", "D") |
            (input$criteria_method %in%
              "E" &
              (input$state_tribe_select_OP_E != ""))
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )

    ### Upload the template for Option E

    # Reactive to read the uploaded template file
    uploaded_temp_table <- shiny::eventReactive(input$upload_template, {
      # Validate file is selected
      shiny::validate(need(
        !is.null(input$upload_template),
        "No file selected."
      ))

      # Define file path
      file_path <- input$upload_template$datapath
      file_ext <- tools::file_ext(file_path)

      # Log to console
      message(paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "\n",
        "Criteria Template Import, file name: ",
        input$upload_template$name,
        "\n",
        "Criteria Template Import, file path: ",
        file_path,
        "\n"
      ))

      # User notification
      shiny::showNotification(
        paste0("Loading criteria template: ", input$upload_template$name),
        type = "message",
        duration = 5
      )

      # Read the Excel file
      df_template <- NULL

      if (file_ext %in% c("xlsx")) {
        df_template <- tryCatch(
          {
            # Try reading the DefineCriteriaMethodology sheet first
            readxl::read_excel(
              file_path,
              sheet = "DefineCriteriaMethodology",
              na = c("NA", ""),
              trim_ws = TRUE,
              col_names = TRUE,
              guess_max = 100000
            )
          },
          error = function(e) {
            # If sheet name fails, try reading first sheet
            tryCatch(
              {
                readxl::read_excel(
                  file_path,
                  sheet = 1,
                  na = c("NA", ""),
                  trim_ws = TRUE,
                  col_names = TRUE,
                  guess_max = 100000
                )
              },
              error = function(e2) {
                shiny::showNotification(
                  paste("Error reading file:", e2$message),
                  type = "error",
                  duration = 10
                )
                return(NULL)
              }
            )
          }
        )
      } else {
        shiny::showNotification(
          "Please upload an Excel file (.xlsx)",
          type = "error"
        )
        return(NULL)
      }

      # Define required columns for criteria template
      selected_cols <- c(
        names(suppressMessages(EPATADA::TADA_DefineCriteriaMethodology())),
        "EquationType",
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

      # Check for missing required columns
      missing_cols <- setdiff(selected_cols, names(df_template))

      if (length(missing_cols) > 0) {
        shiny::showNotification(
          paste0(
            "Warning: Missing columns in Option E: User Supplied Template: \n",
            paste(missing_cols, collapse = ", ")
          ),
          type = "warning",
          duration = 10
        )
      }

      return(df_template)
    })

    # Update the input$state_tribe_select_OP_E if users uploaded criteria table for Option E
    shiny::observeEvent(uploaded_temp_table(), {
      req(uploaded_temp_table())

      temp_table <- uploaded_temp_table()

      # Get the org ID
      org_ID <- unique(temp_table$ATTAINS.OrganizationIdentifier)

      shiny::updateSelectInput(
        session = session,
        inputId = "state_tribe_select_OP_E",
        choices = org_ID
      )
    })

    ### Determine the options to generate the criteria table

    # Run the TADA_DefineCriteriaMethodology_Shiny function to get the criteria table
    criteria_template_rv <- shiny::reactiveVal(NULL) # store last capture if you need it

    # Return the capture (messages + stdout) when Generate is clicked
    criteria_cap <- eventReactive(
      input$Generate_Template,
      {
        req(input$criteria_method)

        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Generating criteria template...",
          session = shiny::getDefaultReactiveDomain()
        )
        on.exit(
          shinybusy::remove_modal_spinner(
            session = shiny::getDefaultReactiveDomain()
          ),
          add = TRUE
        )

        warning_msg(NULL)

        wrap_error <- function(e) {
          list(
            result = structure(conditionMessage(e), class = "try-error"),
            lines = character(0)
          )
        }

        cap <- tryCatch(
          {
            if (input$criteria_method %in% "A") {
              req(input$state_tribe_select)
              temp_table <- EPATADA::TADA_GetCriteriaFile(
                display_name = input$state_tribe_select
              )
              org_ID <- unique(temp_table$ATTAINS.OrganizationIdentifier)
              capture_all_output({
                EPATADA::TADA_DefineCriteriaMethodology(
                  .data = tadat$df_mlid_input,
                  org_id = org_ID,
                  auto_assign = FALSE,
                  criteriaMethods = temp_table,
                  displayUniqueId = input$criteria_displayUniqueId,
                  AUMLRef = tadat$df_mltoau_input,
                  AU_UsesRef = tadat$df_autouse_input,
                  excel = TRUE,
                  overwrite = FALSE
                )
              })
            } else if (input$criteria_method %in% "B") {
              req(input$state_tribe_select, tadat$df_mlid_input)
              capture_all_output({
                EPATADA::TADA_DefineCriteriaMethodology(
                  .data = tadat$df_mlid_input,
                  org_id = input$state_tribe_select,
                  auto_assign = TRUE,
                  criteriaMethods = NULL,
                  displayUniqueId = input$criteria_displayUniqueId,
                  AUMLRef = tadat$df_mltoau_input,
                  AU_UsesRef = tadat$df_autouse_input,
                  excel = TRUE,
                  overwrite = FALSE
                )
              })
            } else if (input$criteria_method %in% "C") {
              capture_all_output({
                EPATADA::TADA_DefineCriteriaMethodology(
                  .data = tadat$df_mlid_input,
                  org_id = NULL,
                  auto_assign = TRUE,
                  criteriaMethods = NULL,
                  displayUniqueId = input$criteria_displayUniqueId,
                  AUMLRef = tadat$df_mltoau_input,
                  AU_UsesRef = tadat$df_autouse_input,
                  excel = TRUE,
                  overwrite = FALSE
                )
              })
            } else if (input$criteria_method %in% "D") {
              capture_all_output({
                EPATADA::TADA_DefineCriteriaMethodology(
                  excel = TRUE,
                  overwrite = FALSE
                )
              })
            } else {
              req(input$state_tribe_select_OP_E != "", uploaded_temp_table())
              temp_table <- uploaded_temp_table()
              capture_all_output({
                EPATADA::TADA_DefineCriteriaMethodology(
                  .data = tadat$df_mlid_input,
                  org_id = input$state_tribe_select_OP_E,
                  auto_assign = FALSE,
                  criteriaMethods = temp_table,
                  displayUniqueId = input$criteria_displayUniqueId,
                  AUMLRef = tadat$df_mltoau_input,
                  AU_UsesRef = tadat$df_autouse_input,
                  excel = TRUE,
                  overwrite = FALSE
                )
              })
            }
          },
          error = wrap_error
        )

        criteria_template_rv(cap) # store for other UI toggles
        cap
      },
      ignoreInit = TRUE
    )

    observe({
      shinyjs::toggle(
        id = "review_template",
        condition = !is.null(criteria_template_rv())
      )
      #shinyjs::toggleState(id = "download_template", condition = !is.null(criteria_template_rv()))
    })

    output$template_status <- shiny::renderText({
      cap <- criteria_cap()
      if (is.null(cap)) {
        if (!is.null(warning_msg()) && nzchar(trimws(warning_msg()))) {
          paste("Template generation issue:\n", warning_msg())
        } else {
          "Ready to generate template. Select options above and click 'Generate and Download Template'."
        }
      } else if (inherits(cap$result, "try-error")) {
        paste("Error:\n", as.character(cap$result))
      } else if (length(cap$lines)) {
        paste(cap$lines, collapse = "\n")
      } else {
        "No messages or output."
      }
    })

    ### Upload the reviewed criteria template

    # Reactive to read the uploaded template file
    review_template_input <- shiny::eventReactive(input$review_template, {
      # Validate file is selected
      shiny::validate(need(
        !is.null(input$review_template),
        "No file selected."
      ))

      # Define file path
      file_path <- input$review_template$datapath
      file_ext <- tools::file_ext(file_path)

      # Log to console
      message(paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "\n",
        "Criteria Template Import, file name: ",
        input$review_template$name,
        "\n",
        "Criteria Template Import, file path: ",
        file_path,
        "\n"
      ))

      # User notification
      shiny::showNotification(
        paste0("Loading criteria template: ", input$review_template$name),
        type = "message",
        duration = 5
      )

      # Read the Excel file
      df_template <- NULL

      if (file_ext %in% c("xlsx")) {
        df_template <- tryCatch(
          {
            # Try reading the DefineCriteriaMethodology sheet first
            readxl::read_excel(
              file_path,
              sheet = "DefineCriteriaMethodology",
              na = c("NA", ""),
              trim_ws = TRUE,
              col_names = TRUE,
              guess_max = 100000
            )
          },
          error = function(e) {
            # If sheet name fails, try reading first sheet
            tryCatch(
              {
                readxl::read_excel(
                  file_path,
                  sheet = 1,
                  na = c("NA", ""),
                  trim_ws = TRUE,
                  col_names = TRUE,
                  guess_max = 100000
                )
              },
              error = function(e2) {
                shiny::showNotification(
                  paste("Error reading file:", e2$message),
                  type = "error",
                  duration = 10
                )
                return(NULL)
              }
            )
          }
        )
      } else {
        shiny::showNotification(
          "Please upload an Excel file (.xlsx)",
          type = "error"
        )
        return(NULL)
      }

      # Define required columns for criteria template
      selected_cols <- c(
        names(suppressMessages(EPATADA::TADA_DefineCriteriaMethodology())),
        "EquationType",
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

      # Check for missing required columns
      missing_cols <- setdiff(selected_cols, names(df_template))

      if (length(missing_cols) > 0) {
        shiny::showNotification(
          paste0(
            "Warning: Missing columns in template: ",
            paste(missing_cols, collapse = ", ")
          ),
          type = "warning",
          duration = 10
        )

        shiny::showNotification(
          paste0("Appending missing columns with NA values."),
          type = "warning",
          duration = 10
        )
      }

      # Check for missing rows
      df_template2 <- df_template |>
        dplyr::filter(dplyr::if_any(6:dplyr::last_col(), ~ !is.na(.)))

      # Adds missing cols
      df_template2[missing_cols] <- NA

      # convert criteria table col types to match
      df_template2 <- EPATADA::TADA_CorrectColType(df_template2)

      # after checking missing rows, assume remaining rows are EquationBased = No if left blank (common occurrence from beta testing.)
      df_template2 <- df_template2 |>
        dplyr::mutate(
          EquationBased = dplyr::if_else(
            is.na(EquationBased),
            "No",
            EquationBased
          )
        )

      # A reactive value to determine if notification will be displayed if the final criteria table is empty
      notification_id <- reactiveVal(NULL)

      # checks if df_template2 is empty
      if (nrow(df_template2) == 0) {
        empty.df <- shiny::showNotification(
          paste0(
            "Warning: No available data in your final TADA-compatible criteria table. Cannot proceed with no metric to analyze. \n",
            "Please review and re-upload your final TADA-compatible criteria table to ensure all columns have been filled out appropriately."
          ),
          type = "warning",
          duration = NULL
        )
        # store the empty.df as the reactive value
        notification_id(empty.df)
      }

      # If the user initially submitted an empty final criteria table, check to see if their re-uploads is correct. If so, close the error notification.
      shiny::observe({
        # Validate file is uploaded
        shiny::validate(need(
          !is.null(input$review_template),
          "No file selected."
        ))

        req(review_template_input())

        df_template <- review_template_input()

        if (nrow(df_template) > 0) {
          shiny::removeNotification(notification_id())
          notification_id(NULL)
        }
      })

      # Also save to tadat for use in other modules
      tadat$criteria_template <- df_template2
      # Get the organization ID from the criteria table
      tadat$criteria_state_tribe <- unique(
        df_template2$ATTAINS.OrganizationIdentifier
      )[1]

      # Get the equation tables for each Equation Type for the magnitude update step
      tadat$hardness_equation <- df_template2 |>
        dplyr::filter(EquationType %in% "Hardness")

      tadat$pH_equation <- df_template2 |> dplyr::filter(EquationType %in% "pH")

      tadat$pH_hardness_equation <- df_template2 |>
        dplyr::filter(EquationType %in% "pH and Hardness")

      tadat$pH_Temperature_equation <- df_template2 |>
        dplyr::filter(EquationType %in% "pH and Temperature")

      return(df_template2)
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

      # Check for missing rows
      df_template2 <- df_template |>
        dplyr::filter(dplyr::if_any(6:dplyr::last_col(), ~ !is.na(.)))

      # Count how many rows contain missing criteria/methods values
      row_NA <- nrow(df_template) - nrow(df_template2)
      equationBased_NA <- sum(is.na(df_template2$EquationBased))

      # convert criteria table col types to match
      df_template2 <- EPATADA::TADA_CorrectColType(df_template2)

      # after checking missing rows, assume remaining rows are EquationBased = No if left blank (common occurrence from beta testing.)
      df_template2 <- df_template2 |>
        dplyr::mutate(
          EquationBased = dplyr::if_else(
            is.na(EquationBased),
            "No",
            EquationBased
          )
        )

      # check for mismatching WQP Char, Fraction, Speciation and Units with the Criteria table
      df_template2_ID <- EPATADA::TADA_CreateComparableID(dplyr::rename(
        df_template2,
        TADA.ResultMeasure.MeasureUnitCode = MagnitudeUnit
      ))
      non_matches <- dplyr::anti_join(
        df_template2_ID,
        tadat$df_mlid_input,
        "TADA.ComparableDataIdentifier"
      ) |>
        dplyr::select(
          TADA.ComparableDataIdentifier,
          TADA.ResultSampleFractionText,
          TADA.MethodSpeciationName,
          TADA.ResultMeasure.MeasureUnitCode
        ) |>
        dplyr::distinct()

      # check for accepted/rejected values in columns using TADACommunityHub functions
      # wrap the validator call in tryCatch so a validation error can’t take down the output
      status <- tryCatch(
        TADACommunityHub::runAllValidations(df_template2),
        error = function(e) {
          list(overall_status = paste("Validation error:", e$message))
        }
      )

      # EquationBased must be populated as "Yes" or "No". If left as NA, print a message that this occurred.
      eq_text <- paste0("")
      if (equationBased_NA > 0) {
        eq_text <- paste0(
          "Warning: EquationBased must be populated - Your uploaded criteria table contains ",
          equationBased_NA,
          " rows for analysis with EquationBased values populated as 'NA'. \n",
          "   These NAs will be filled in as 'No'. \n"
        )
      }

      # Build summary text
      text <- paste0(
        "Your template contains ",
        nrow(df_template2),
        " rows of criteria information for anlaysis. \n",
        " and is missing criteria information for: \n",
        nrow(non_matches),
        " rows. "
      )

      if (nrow(non_matches) > 0) {
        extra_text <- paste(
          c(
            "Warning: Mismatching fraction, speciation, and/or units were found for these TADA.ComparableDataIdentifiers:",
            unique(non_matches$TADA.ComparableDataIdentifier),
            "\n"
          ),
          collapse = "\n - "
        )
        text <- paste(text, eq_text, extra_text, sep = "\n")
      }

      # Prints final message
      paste(paste0(text, "\n", status$overall_status, sep = "\n"))
    })

    ### Generate the template summary table (adds missing required columns to the output)
    output$final_template <- DT::renderDT({
      # Validate file is uploaded
      shiny::validate(need(
        !is.null(input$review_template),
        "No file selected."
      ))

      # Get the data from the reactive
      df_template <- review_template_input()

      shiny::validate(need(!is.null(df_template), "Error loading file."))

      # Render table
      DT::datatable(
        df_template,
        filter = "top",
        class = "compact",
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          scrollCollapse = TRUE,
          paging = TRUE,
          pageLength = 5,
          lengthMenu = c(5, 10, 25, 50, 100),
          autoWidth = TRUE
        )
      )
    })

    # Save the options to the next tab
    shiny::observe({
      tadat$criteria_method <- input$criteria_method
      tadat$state_tribe_select <- input$state_tribe_select
    })

    # Activate the batch and custom tabs if the final criteria table is ready
    shiny::observe({
      # Validate file is uploaded
      shiny::validate(need(
        !is.null(input$review_template),
        "No file selected."
      ))

      req(review_template_input())

      df_template <- review_template_input()

      # Define required columns for criteria template
      selected_cols <- c(
        names(suppressMessages(EPATADA::TADA_DefineCriteriaMethodology())),
        "EquationType",
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

      # Check for missing required columns
      missing_cols <- setdiff(selected_cols, names(df_template))

      # handle missing cols
      df_template[missing_cols] <- NA

      # Check for missing rows
      df_template2 <- df_template |>
        dplyr::filter(dplyr::if_any(6:dplyr::last_col(), ~ !is.na(.)))

      if (length(missing_cols) == 0 & nrow(df_template2) > 0) {
        shinyjs::enable(selector = '.nav li a[data-value="Batch"]')
        shinyjs::enable(selector = '.nav li a[data-value="Custom"]')
      } else {
        shinyjs::disable(selector = '.nav li a[data-value="Batch"]')
        shinyjs::disable(selector = '.nav li a[data-value="Custom"]')
      }
    })
  })
}

## To be copied in the UI
# mod_criteria_table_ui("criteria_table_1")

## To be copied in the server
# mod_criteria_table_server("criteria_table_1")
