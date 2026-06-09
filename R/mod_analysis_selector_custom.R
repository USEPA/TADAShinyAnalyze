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
    fluidRow(column(
      width = 12,
      shiny::verbatimTextOutput(ns("current_criteria_info_custom"))
    )),
    fluidRow(column(
      width = 12,
      htmltools::p(
        "Determine the spatial unit and the uses included in the analysis."
      ),
      htmltools::p(
        "If only Water Quality Data File is available or if users select the EPA option in the 'Criteria Table' tab, ",
        "the AU option would not be available for the 'Batch Analyzed by the spatial unit'. In this case, ",
        "the tool will not match the ATTAINS.UseName from the criteria table to the assessment units."
      )
    )),
    fluidRow(
      column(
        width = 6,
        shiny::radioButtons(
          inputId = ns("loc_select_custom"),
          label = "Custom analyzed by the spatial unit: ",
          choices = c(
            "Monitoring Location ID" = "MLid",
            "Assessment Unit" = "AU",
            "Custom Grouping (Use the following map-table selector to select sites to group)" = "CG"
          )
        )
      ),
      column(
        width = 6,
        shiny::checkboxInput(
          inputId = ns("uses_all_custom"),
          label = "Select all uses",
          value = TRUE
        ),
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
    fluidRow(column(
      width = 12,
      htmltools::p(htmltools::strong(
        "Join by TADA.CharacteristicName only, or choose Automatic (recommended) to join using TADA.ComparableDataIdentifier when available, else by TADA.CharacteristicName with TADA.ResultSampleFractionText and/or TADA.MethodSpeciationName, else by TADA.CharacteristicName alone."
      ))
    )),
    fluidRow(column(
      width = 12,
      shiny::radioButtons(
        inputId = ns("join_select_custom"),
        label = tagList(
          "Choose option for joining the criteria table to your WQP dataframe",
          shiny::actionLink(
            ns("join_help"),
            label = NULL,
            icon = shiny::icon("circle-info"),
            title = "More details"
          )
        ),
        choices = c(
          "Automatic (Recommended)" = "Option 1",
          "TADA.CharacteristicName only" = "Option 2"
        )
      ),
      shiny::helpText(
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
mod_analysis_selector_custom_server <- function(id, tadat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Disable uses_select on initialization since uses_all defaults to TRUE
    shinyjs::disable("uses_select_custom")

    # Show the state/tribe selection
    output$current_criteria_info_custom <- shiny::renderText({
      paste0(
        "Current criteria: ",
        tadat$criteria_state_tribe,
        " (Method: ",
        tadat$criteria_method,
        ")"
      )
    })

    # Determine the use_type
    shiny::observe({
      req(tadat$criteria_template)
      if (
        isTRUE(tadat$files_loaded_mlid) &&
          isTRUE(tadat$files_loaded_mltoau) &&
          isTRUE(tadat$files_loaded_autouse)
      ) {
        use_type <- "Option 1"
      } else if (isTRUE(tadat$files_loaded_mlid)) {
        use_type <- "Option 2"
      } else {
        use_type <- "Option 2"
      }
      tadat$use_type_custom <- use_type
    })

    # Update the loc_select_custom choices based on use_type
    shiny::observeEvent(tadat$use_type_custom, {
      if (tadat$use_type_custom %in% "Option 2") {
        choices <- c(
          "Monitoring Location ID" = "MLid",
          "Custom Grouping (Use the following map-table selector to select sites to group)" = "CG"
        )

        current_selection <- isolate(input$loc_select_custom)
        if (!is.null(current_selection) && current_selection %in% "AU") {
          selected <- "MLid"
        } else if (
          !is.null(current_selection) && current_selection %in% c("MLid", "CG")
        ) {
          selected <- current_selection
        } else {
          selected <- "MLid"
        }
      } else {
        choices <- c(
          "Monitoring Location ID" = "MLid",
          "Assessment Unit" = "AU",
          "Custom Grouping (Use the following map-table selector to select sites to group)" = "CG"
        )

        current_selection <- isolate(input$loc_select_custom)
        if (
          !is.null(current_selection) &&
            current_selection %in% c("MLid", "AU", "CG")
        ) {
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
    shiny::observeEvent(c(tadat$criteria_state_tribe, tadat$use_type_custom), {
      req(
        tadat$criteria_state_tribe,
        tadat$use_type_custom,
        tadat$criteria_template
      )

      if (tadat$use_type_custom %in% "Option 1") {
        req(tadat$df_autouse_input)

        criteria_table_f1 <- tadat$criteria_template |>
          dplyr::filter(
            ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe
          )

        criteria_uses <- unique(criteria_table_f1$ATTAINS.UseName)
        AU_Use_uses <- unique(tadat$df_autouse_input$ATTAINS.UseName)
        available_uses <- base::intersect(criteria_uses, AU_Use_uses)
      } else {
        criteria_table_f1 <- tadat$criteria_template |>
          dplyr::filter(
            ATTAINS.OrganizationIdentifier %in% tadat$criteria_state_tribe
          )
        available_uses <- unique(criteria_table_f1$ATTAINS.UseName)
      }

      tadat$available_uses_custom <- available_uses

      if (input$uses_all_custom) {
        tadat$uses_select_re_custom <- available_uses
      } else {
        current_uses <- isolate(tadat$uses_select_re_custom)
        if (!is.null(current_uses) && length(current_uses) > 0) {
          valid_uses <- intersect(current_uses, available_uses)
          tadat$uses_select_re_custom <- if (length(valid_uses) > 0) {
            valid_uses
          } else {
            available_uses[1]
          }
        } else {
          tadat$uses_select_re_custom <- if (length(available_uses) > 0) {
            available_uses[1]
          } else {
            character(0)
          }
        }
      }

      shinyWidgets::updateVirtualSelect(
        session = session,
        inputId = "uses_select_custom",
        choices = sort(available_uses),
        selected = tadat$uses_select_re_custom
      )
    })

    # Handle checkbox changes
    shiny::observeEvent(
      input$uses_all_custom,
      {
        req(tadat$available_uses_custom)

        if (input$uses_all_custom) {
          shinyjs::disable("uses_select_custom")
          tadat$uses_select_re_custom <- tadat$available_uses_custom
          shinyWidgets::updateVirtualSelect(
            session = session,
            inputId = "uses_select_custom",
            choices = sort(tadat$available_uses_custom),
            selected = tadat$available_uses_custom
          )
        } else {
          shinyjs::enable("uses_select_custom")
          if (
            !is.null(tadat$uses_select_re_custom) &&
              length(tadat$uses_select_re_custom) > 0
          ) {
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
      },
      ignoreInit = TRUE
    )

    # Handle uses_select changes separately
    shiny::observeEvent(
      input$uses_select_custom,
      {
        req(isFALSE(input$uses_all_custom))
        tadat$uses_select_re_custom <- if (
          shiny::isTruthy(input$uses_select_custom)
        ) {
          input$uses_select_custom
        } else {
          NULL
        }
      },
      ignoreInit = TRUE
    )

    # Pop-up helper
    observeEvent(input$join_help, {
      showModal(modalDialog(
        title = "Join options explained",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          tags$h5("Option 1 - Automatic (Recommended)"),
          tags$p("Join by using the most specific available match for each row, in priority order:"),
          tags$p("1) Exact Identifier (TADA.ComparableDataIdentifier = Characteristic + Fraction + Speciation + Unit)"),
          tags$p("2) Characteristic + Fraction + Speciation"),
          tags$p("3a) Characteristic + Fraction"),
          tags$p("3b) Characteristic + Speciation"),
          tags$p("4) Characteristic only"),
          tags$ul(
            tags$li("Use when TADA.ComparableDataIdentifier has been filled or by fraction and/or speciation that are present and consistent between your criteria table and WQP data frame."),
            tags$li("Stricter matching (fewer false/ambiguous joins).")
          ),
          tags$hr(),
          tags$h5("Option 2 - CharacteristicName only"),
          tags$p("Joins only on TADA.CharacteristicName."),
          tags$ul(
            tags$li("Use when fraction/speciation are missing or inconsistent between your criteria table and WQP data frame."),
            tags$li("More permissive; TADAShinyAnalyze will not consider fraction or speciation in analysis.")
          )
        )
      ))
    })

    # Save selected loc_select and join_select to tadat
    shiny::observe({
      tadat$loc_select_custom <- input$loc_select_custom
      tadat$join_select_custom <- input$join_select_custom
    })
  })
}

## To be copied in the UI
# mod_analysis_selector_custom_ui("analysis_selector_custom_1")

## To be copied in the server
# mod_analysis_selector_custom_server("analysis_selector_custom_1")
