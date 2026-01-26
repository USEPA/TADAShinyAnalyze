#' TADA_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_TADA_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::fluidRow(
    style = "padding-left:20px",
    shiny::fluidRow(column(
      2, shiny::actionButton(ns("disclaimer"), "DISCLAIMER")
    )),
    htmltools::br(),
    htmltools::br()
  ))
}
    
#' TADA_summary Server Functions
#'
#' @noRd 
mod_TADA_summary_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shiny::observeEvent(input$disclaimer, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Disclaimer",
          shiny::tags$ul(
            shiny::tags$li("This module is still under development, and new feature additions are being added."),
            shiny::tags$li(
              "Currently, the app references the following columns in the criteria methodologies table:",
              shiny::tags$ul(
                shiny::tags$li("ATTAINS.OrganizationIdentifier"),
                shiny::tags$li("ATTAINS.UseName"),
                shiny::tags$li("TADA.CharacteristicName"),
                shiny::tags$li("TADA.ResultSampleFractionText"),
                shiny::tags$li("UniqueSpatialCriteria"),
                shiny::tags$li("AcuteChronic"),
                shiny::tags$li("Season"),
                shiny::tags$li("Magnitude Columns: MagnitudeValueLower, MagnitudeValueUpper, and MagnitudeUnit"),
                shiny::tags$li("Duration Columns: DurationValue, DurationUnit, DurationMethod"),
                shiny::tags$li("Frequency Columns: FreqValue and FreqMethod"),
                shiny::tags$li("EquationBased"),
                shiny::tags$li("EquationType"),
                shiny::tags$li("Equation"),
                shiny::tags$li("hardness_param_1 to hardness_param_6"),
                shiny::tags$li("pH_param_1 to pH_param_4")
              )
            ),
            shiny::tags$li(
              "Currently, the app supports analysis of the following methods, with expansions coming soon:",
              shiny::tags$ul(
                shiny::tags$li("DurationMethod:"),
                shiny::tags$ul(
                  shiny::tags$li("arithmetic mean"),
                  shiny::tags$li("rolling arithmetic mean"),
                  shiny::tags$li("arithmetic min"),
                  shiny::tags$li("arithmetic max"),
                  shiny::tags$li("geometric mean"),
                  shiny::tags$li("arithmetic extremes")
                ),
                shiny::tags$li("FreqMethod:"),
                shiny::tags$ul(
                  shiny::tags$li("NumberNotMeeting"),
                  shiny::tags$li("n-samples in 3 years"),
                  shiny::tags$li("Percent of samples not meeting"),
                  shiny::tags$li("Percentile")
                ),
              )
            )
          )
        )
      )
    })
  })
}
    
## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")
    
## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")
