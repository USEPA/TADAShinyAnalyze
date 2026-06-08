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
mod_analysis_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(
      width = 12,
      shiny::verbatimTextOutput(ns("current_criteria_info"))
    )),
    fluidRow(column(
      width = 12,
      htmltools::p(
        "Determine the spatial unit and the uses included in the analysis."
      ),
      htmltools::p(
        "If only Water Quality Data File is available 
                     or if users select the EPA option in the 'Criteria Table' tab,
                     The AU option would not be available for the 'Batch Analyzed by the spatial unit'. In this case, 
                     the tool will not match the ATTAINS.UseName from the criteria table to the assessment units."
      )
    )),
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(
          inputId = ns("loc_select"),
          label = "Batch analyzed by the spatial unit: ",
          choices = c(
            "Monitoring Location ID" = "MLid",
            "Assessment Unit" = "AU"
          )
        )
      ),
      column(
        width = 6,
        shiny::checkboxInput(
          inputId = ns("uses_all"),
          label = "Select all uses",
          value = TRUE
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("uses_select"),
          label = "Select the uses:",
          choices = NULL,
          showValueAsTags = TRUE,
          search = TRUE,
          multiple = TRUE
        )
      )
    ),
    fluidRow(column(
      width = 12,
      htmltools::p(htmltools::strong(
        "Join by TADA.CharacteristicName or TADA.ComparableDataIdentifier (Characteristic, Fraction and Speciation)."
      ))
    )),
    fluidRow(column(
      width = 6,
      shiny::radioButtons(
        inputId = ns("join_select"),
        label = tagList(
          "Choose option for joining the criteria table to your WQP dataframe",
          # info icon that opens the popup
          actionLink(
            ns("join_help"),
            label = NULL,
            icon = icon("circle-info"),
            title = "More details"
          )
        ),
        choices = c(
          "TADA.ComparableDataIdentifier" = "Option 1",
          "TADA.CharacteristicName only" = "Option 2"
        )
      ),
      helpText(
        "Please review the matches and make sure everything you expect is listed.",
        "If something is missing, check that the entries in your Criteria & Methodologies template ",
        "use the exact same wording and formatting as in your WQP data file for: ",
        "characteristic name, fraction (for example, total vs. dissolved), and speciation (for example, nitrate reported as N vs. as NO3)"
      )
    ))
  )
}

#' analysis_selector Server Functions
#'
#' @noRd
mod_analysis_selector_server <- function(id, tadat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Disable uses_select on initialization since uses_all defaults to TRUE
    shinyjs::disable("uses_select")

    # Show the state/tribe selection
    output$current_criteria_info <- shiny::renderText({
      paste0(
        "Current criteria: ",
        tadat$criteria_state_tribe,
        " (Method: ",
        tadat$criteria_method,
        ")"
      )
    })

    # An observe block to determine the use_type
    shiny::observe({
      req(tadat$criteria_template)
      # Check if all three files are loaded
      if (
        isTRUE(tadat$files_loaded_mlid) &&
          isTRUE(tadat$files_loaded_mltoau) &&
          isTRUE(tadat$files_loaded_autouse)
      ) {
        use_type <- "Option 1" # Use crosswalk files
      } else if (isTRUE(tadat$files_loaded_mlid)) {
        # Only the main water quality data file is loaded
        use_type <- "Option 2"
      } else {
        # No files loaded yet
        use_type <- "Option 2"
      }

      tadat$use_type_batch <- use_type
    })

    # Update the loc_select choices based on use_type
    shiny::observeEvent(tadat$use_type_batch, {
      if (tadat$use_type_batch %in% "Option 2") {
        # Remove AU option when Option 2
        choices <- c("Monitoring Location ID" = "MLid")

        # Check if current selection is AU and change it to MLid
        current_selection <- isolate(input$loc_select)
        if (!is.null(current_selection) && current_selection %in% "AU") {
          selected <- "MLid"
        } else if (
          !is.null(current_selection) && current_selection %in% "MLid"
        ) {
          selected <- current_selection
        } else {
          selected <- "MLid"
        }
      } else {
        # Include AU option when Option 1
        choices <- c(
          "Monitoring Location ID" = "MLid",
          "Assessment Unit" = "AU"
        )

        # Keep current selection if valid
        current_selection <- isolate(input$loc_select)
        if (
          !is.null(current_selection) && current_selection %in% c("MLid", "AU")
        ) {
          selected <- current_selection
        } else {
          selected <- "MLid"
        }
      }

      shiny::updateRadioButtons(
        session = session,
        inputId = "loc_select",
        choices = choices,
        selected = selected
      )
    })

    # Update the available uses
    shiny::observeEvent(
      c(
        tadat$use_type_batch,
        tadat$criteria_state_tribe,
        tadat$criteria_template
      ),
      {
        req(tadat$use_type_batch)
        req(tadat$criteria_state_tribe)
        req(tadat$criteria_template)

        if (tadat$use_type_batch == "Option 1") {
          req(tadat$df_autouse_input)

          criteria_table_f1 <- tadat$criteria_template |>
            dplyr::filter(
              ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe
            )

          # Get the list of available uses from criteria_table_f1
          criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)

          AU_Use_uses <- unique(tadat$df_autouse_input$ATTAINS.UseName)

          # Find the intersection
          available_uses <- base::intersect(criteria_uses, AU_Use_uses)
        } else {
          criteria_table_f1 <- tadat$criteria_template |>
            dplyr::filter(
              ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe
            )

          # Get the list of available uses from criteria_table_f1
          criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)

          # Find the intersection
          available_uses <- criteria_uses
        }

        # Save available_uses to tadat
        tadat$available_uses <- available_uses

        # Reset uses selection when state/tribe changes
        if (input$uses_all) {
          tadat$uses_select_re <- available_uses
        } else {
          # Keep only uses that are still available
          current_uses <- isolate(tadat$uses_select_re)
          if (!is.null(current_uses) && length(current_uses) > 0) {
            valid_uses <- intersect(current_uses, available_uses)
            tadat$uses_select_re <- if (length(valid_uses) > 0) {
              valid_uses
            } else {
              available_uses[1]
            }
          } else {
            tadat$uses_select_re <- if (length(available_uses) > 0) {
              available_uses[1]
            } else {
              character(0)
            }
          }
        }

        # Update the UI
        shinyWidgets::updateVirtualSelect(
          session = session,
          inputId = "uses_select",
          choices = sort(available_uses),
          selected = tadat$uses_select_re
        )
      }
    )

    # Initialize uses_select_re
    shiny::observe({
      req(tadat$available_uses)
      if (is.null(tadat$uses_select_re)) {
        tadat$uses_select_re <- if (isolate(input$uses_all)) {
          tadat$available_uses
        } else {
          character(0)
        }
      }
    })

    # Handle checkbox changes
    shiny::observeEvent(
      input$uses_all,
      {
        req(tadat$available_uses)

        if (input$uses_all) {
          shinyjs::disable("uses_select")
          tadat$uses_select_re <- tadat$available_uses
          # Update the select to show all selected (visual consistency)
          shinyWidgets::updateVirtualSelect(
            session = session,
            inputId = "uses_select",
            choices = sort(tadat$available_uses),
            selected = tadat$available_uses
          )
        } else {
          shinyjs::enable("uses_select")
          shinyWidgets::updateVirtualSelect(
            session = session,
            inputId = "uses_select",
            choices = sort(tadat$available_uses),
            selected = tadat$uses_select_re
          )
        }
      },
      ignoreInit = FALSE
    )

    # Handle uses_select changes separately
    shiny::observeEvent(
      input$uses_select,
      {
        # Only update when checkbox is unchecked AND uses_select is not disabled
        if (!input$uses_all && !is.null(input$uses_select)) {
          tadat$uses_select_re <- input$uses_select
        }
      },
      ignoreNULL = FALSE
    ) # Important: Allow empty selections

    #################################### pop up display helper
    observeEvent(input$join_help, {
      showModal(modalDialog(
        title = "Join options explained",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          tags$h5("Option 1 - ComparableDataIdentifier"),
          tags$p(
            "Joins using TADA.CharacteristicName, TADA.ResultSampleFractionText, and TADA.MethodSpeciationName."
          ),
          tags$ul(
            tags$li(
              "Use when fraction and speciation are present and consistent between your criteria table and WQP data frame."
            ),
            tags$li("Stricter matching (fewer false/ambiguous joins).")
          ),
          tags$hr(),
          tags$h5("Option 2 - CharacteristicName only"),
          tags$p("Joins only on TADA.CharacteristicName."),
          tags$ul(
            tags$li(
              "Use when fraction/speciation are missing or inconsistent between your criteria table and WQP data frame."
            ),
            tags$li(
              "More permissive; TADAShinyAnalyze will not consider fraction or speciation in analysis."
            )
          )
        )
      ))
    })
    ####################################

    ### Save the selected loc_select and uses to tadat
    shiny::observe({
      tadat$loc_select <- input$loc_select
      tadat$join_select <- input$join_select
    })
  })
}

## To be copied in the UI
# mod_analysis_selector_ui("analysis_selector_1")

## To be copied in the server
# mod_analysis_selector_server("analysis_selector_1")
