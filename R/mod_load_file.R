#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_file_ui <- function(id) {
  # set module session id
  ns <- shiny::NS(id)
  
  # start taglist
  shiny::tagList(
    # header
    # htmltools::h2("Load Files"),
    
    # load ml id file
    shiny::fluidRow(
      # instructions column
      shiny::column(
        width = 4,
        htmltools::h2("1. Load Monitoring Location File"),
        htmltools::strong("Purpose"),
        htmltools::p("text text text text"),
        htmltools::strong("Instructions"),
        htmltools::p("text text text text"),
        htmltools::h3("Select file parameters"),
        shiny::radioButtons(
          inputId = ns("mlid_separator"),
          label = "1a. Choose file separator:",
          choices = c(Comma = ",", Tab = "\t"),
          selected = ","
        ),
        shiny::fileInput(
          inputId = ns("mlid_input_file"),
          label = "1b. Choose file to load:",
          width = "90%",
          placeholder = "No file selected.",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv", ".tsv", ".txt"
          )
        )
      ),
      
      # data table column
      shiny::column(
        width = 8,
        htmltools::h3("Monitoring Location File Summary"),
        htmltools::p("Summary of loaded file (blank until file is loaded)."),
        shiny::verbatimTextOutput(outputId = ns("mlid_input_summary"), placeholder = TRUE),
        htmltools::h3("Data Preview"),
        htmltools::p("Interactive table of input dataset (blank until file 
        is loaded). Scroll, search, or sort the table below to explore."),
        # htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_mlid_input_dt"))
      )
    ),
    
    htmltools::hr(),
    
    # load ml to au crosswalk file
    shiny::fluidRow(
      # instructions column
      shiny::column(
        width = 4,
        htmltools::h2("2. Load ML ID to AU ID Crosswalk File"),
        htmltools::strong("Purpose"),
        htmltools::p("text text text text"),
        htmltools::strong("Instructions"),
        htmltools::p("text text text text"),
        htmltools::h3("Select file parameters"),
        shiny::radioButtons(
          inputId = ns("mltoau_separator"),
          label = "2a. Choose file separator:",
          choices = c(Comma = ",", Tab = "\t"),
          selected = ","
        ),
        shiny::fileInput(
          inputId = ns("mltoau_input_file"),
          label = "2b. Choose file to load:",
          width = "90%",
          placeholder = "No file selected.",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv", ".tsv", ".txt"
          )
        )
      ),
      
      # data table column
      shiny::column(
        width = 8,
        htmltools::h3("ML ID to AU ID Crosswalk File Summary"),
        htmltools::p("Summary of loaded file (blank until file is loaded)."),
        shiny::verbatimTextOutput(outputId = ns("mltoau_input_summary"), placeholder = TRUE),
        htmltools::h3("Data Preview"),
        htmltools::p("Interactive table of input dataset (blank until file 
        is loaded). Scroll, search, or sort the table below to explore."),
        # htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_mltoau_input_dt"))
      )
    ),
    
    htmltools::hr(),
    
    # load au to use crosswalk file
    shiny::fluidRow(
      # instructions column
      shiny::column(
        width = 4,
        htmltools::h2("3. Load AU ID to Uses Crosswalk File"),
        htmltools::strong("Purpose"),
        htmltools::p("text text text text"),
        htmltools::strong("Instructions"),
        htmltools::p("text text text text"),
        htmltools::h3("Select file parameters"),
        shiny::radioButtons(
          inputId = ns("autouse_separator"),
          label = "3a. Choose file separator:",
          choices = c(Comma = ",", Tab = "\t"),
          selected = ","
        ),
        shiny::fileInput(
          inputId = ns("autouse_input_file"),
          label = "3b. Choose file to load:",
          width = "90%",
          placeholder = "No file selected.",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv", ".tsv", ".txt"
          )
        )
      ),
      
      # data table column
      shiny::column(
        width = 8,
        htmltools::h3("AU ID to Uses Crosswalk File Summary"),
        htmltools::p("Summary of loaded file (blank until file is loaded)."),
        shiny::verbatimTextOutput(outputId = ns("autouse_input_summary"), placeholder = TRUE),
        htmltools::h3("Data Preview"),
        htmltools::p("Interactive table of input dataset (blank until file 
        is loaded). Scroll, search, or sort the table below to explore."),
        # htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_autouse_input_dt"))
      )
    ),
  )
}
    
#' load_file Server Functions
#'
#' @noRd 
mod_load_file_server <- function(id, tadat){
  shiny::moduleServer(id, function(input, output, session){
    # get module session id
    ns <- session$ns
    
    # Create reactive values to track file upload status
    files_loaded <- reactiveValues(
      mlid = FALSE,
      mltoau = FALSE,
      autouse = FALSE
    )
    
    #### 1. ml file loaded event ####
    df_mlid_input <- shiny::eventReactive(input$mlid_input_file, {
      
      # validate file is selected
      shiny::validate(need(!is.null(input$mlid_input_file), "No file selected."))
      
      # log to command line
      message(
        paste0(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S \n")),
          paste0("Monitoring Location Import, separator: '", input$mlid_separator, "'\n"),
          paste0("Monitoring Location Import, file name: ", input$mlid_input_file$name, "\n"),
          paste0("Monitoring Location Import, file path: ", input$mlid_input_file$datapath, "\n")
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "Monitoring Location Import, separator: '", input$mlid_separator, "'\n",
          "Monitoring Location Import, file name: ", input$mlid_input_file$name, "\n"
        ),
        type = "message",
        duration = 5
      )
      
      # read user imported file
      df_mlid_input <- utils::read.delim(input$mlid_input_file$datapath,
                                         header = TRUE,
                                         sep = input$mlid_separator,
                                         stringsAsFactors = FALSE,
                                         na.strings = c("", "NA"))
      
      # define required columns
      # TODO need to check this is correct
      mlid_required_cols <- c("MonitoringLocationIdentifier",
                              "MonitoringLocationTypeName",
                              "TADA.MonitoringLocationIdentifier")
      
      # get missing columns
      mlid_missing_cols <- setdiff(mlid_required_cols, names(df_mlid_input))
      
      # To check if the files have the correct columns
      if (length(mlid_missing_cols) > 0){
        files_loaded$mlid <- FALSE
      } else {
        files_loaded$mlid <- TRUE
      }
      
      if (length(mlid_missing_cols) > 0) {
        shiny::validate(
          need(
            FALSE,
            paste0("Error: Missing required columns in loaded dataset.\n",
                   "Required columns missing from loaded dataset:\n",
                   paste0("* ", mlid_missing_cols, collapse = "\n"))
          )
        )
      } 
  
      # save to tadat
      tadat$df_mlid_input <- df_mlid_input
      
      # return
      df_mlid_input
    }) # end of df_mlid_input
    
    # render data in a table
    output$df_mlid_input_dt <- DT::renderDT({
      
      # validate data is there
      shiny::validate(need(!is.null(input$mlid_input_file), "No file selected."))
      
      # render table
      DT::datatable(df_mlid_input(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE))
    }) # end renderDT
    
    # render summary
    output$mlid_input_summary <- shiny::renderText({
      # if file was selected
      if (is.null(df_mlid_input)) {
        
        # print
        "No file selected or file invalid."
      }
      
      # 
      else {
        # define data to summarize
        df_mlid_summary <- df_mlid_input()
        
        # print
        paste0(
          "Loaded dataset has ", nrow(df_mlid_summary), " rows and ", ncol(df_mlid_summary), " columns.\n",
          "There are ", length(unique(df_mlid_summary$MonitoringLocationIdentifier)), " unique monitoring locations."
        )
      }
    }) # end renderText
    
    #### 2. ml to au crosswalk file loaded event ####
    df_mltoau_input <- shiny::eventReactive(input$mltoau_input_file, {
      
      # validate file is selected
      shiny::validate(need(!is.null(input$mltoau_input_file), "No file selected."))
      
      # log to command line
      message(
        paste0(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S \n")),
          paste0("ML to AU Crosswalk Import, separator: '", input$mltoau_separator, "'\n"),
          paste0("ML to AU Crosswalk Import, file name: ", input$mltoau_input_file$name, "\n"),
          paste0("ML to AU Crosswalk Import, file path: ", input$mltoau_input_file$datapath, "\n")
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "ML to AU Crosswalk Import, separator: '", input$mltoau_separator, "'\n",
          "ML to AU Crosswalk Import, file name: ", input$mltoau_input_file$name, "\n"
        ),
        type = "message",
        duration = 5
      )
      
      # read user imported file
      df_mltoau_input <- utils::read.delim(input$mltoau_input_file$datapath,
                                           header = TRUE,
                                           sep = input$mltoau_separator,
                                           stringsAsFactors = FALSE,
                                           na.strings = c("", "NA"))
      
      # define required columns
      # TODO need to check this is correct
      mltoau_required_cols <- c("MonitoringLocationIdentifier",
                                "TADA.MonitoringLocationIdentifier",
                                "JoinToAU.AssessmentUnitIdentifier")
      
      # get missing columns
      mltoau_missing_cols <- setdiff(mltoau_required_cols, names(df_mltoau_input))
      
      # To check if the files have the correct columns
      if (length(mltoau_missing_cols) > 0){
        files_loaded$mltoau <- FALSE
      } else if (length(mltoau_missing_cols) <= 0){
        files_loaded$mltoau <- TRUE
      }
      
      if (length(mltoau_missing_cols) > 0) {
        shiny::validate(
          need(
            FALSE,
            paste0("Error: Missing required columns in loaded dataset.\n",
                   "Required columns missing from loaded dataset:\n",
                   paste0("* ", mltoau_missing_cols, collapse = "\n"))
          )
        )
      } 
      
      # save to tadat
      tadat$df_mltoau_input <- df_mltoau_input
      
      # return
      df_mltoau_input
    }) # end of df_mltoau_input
    
    # render data in a table
    output$df_mltoau_input_dt <- DT::renderDT({
      # validate data is there
      # shiny::req(df_mltoau_input())
      shiny::validate(need(!is.null(input$mltoau_input_file), "No file selected."))
      
      # render table
      DT::datatable(df_mltoau_input(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE))
    }) # end renderDT
    
    # render summary
    output$mltoau_input_summary <- shiny::renderText({
      # if file was selected
      if (is.null(df_mltoau_input)) {
        
        # print
        "No file selected or file invalid."
      }
      
      # 
      else {
        # define data to summarize
        df_mltoau_summary <- df_mltoau_input()
        
        # print
        paste0(
          "Loaded dataset has ", nrow(df_mltoau_summary), " rows and ", ncol(df_mltoau_summary), " columns.\n",
          "There are ", length(unique(df_mltoau_summary$MonitoringLocationIdentifier)), " unique monitoring locations.",
          "There are ", length(unique(df_mltoau_summary$JoinAUApp.AssessmentUnitIdentifier)), " unique assessment units."
        )
      }
    }) # end renderText
    
    #### 3. au to use crosswalk file loaded event ####
    df_autouse_input <- shiny::eventReactive(input$autouse_input_file, {
      
      # validate file is selected
      shiny::validate(need(!is.null(input$autouse_input_file), "No file selected."))
      
      # log to command line
      message(
        paste0(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S \n")),
          paste0("AU to Use Crosswalk Import, separator: '", input$autouse_separator, "'\n"),
          paste0("AU to Use Crosswalk Import, file name: ", input$autouse_input_file$name, "\n"),
          paste0("AU to Use Crosswalk Import, file path: ", input$autouse_input_file$datapath, "\n")
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "AU to Use Crosswalk Import, separator: '", input$autouse_separator, "'\n",
          "AU to Use Crosswalk Import, file name: ", input$autouse_input_file$name, "\n"
        ),
        type = "message",
        duration = 5
      )
      
      # read user imported file
      df_autouse_input <- utils::read.delim(input$autouse_input_file$datapath,
                                            header = TRUE,
                                            sep = input$autouse_separator,
                                            stringsAsFactors = FALSE,
                                            na.strings = c("", "NA"))
      
      # define required columns
      # TODO need to check this is correct
      autouse_required_cols <- c("JoinToAU.AssessmentUnitIdentifier",
                                 "ATTAINS.UseName")
      
      # get missing columns
      autouse_missing_cols <- setdiff(autouse_required_cols, names(df_autouse_input))
      
      # To check if the files have the correct columns
      if (length(autouse_missing_cols) > 0){
        files_loaded$autouse <-FALSE
      } else {
        files_loaded$autouse <- TRUE
      }
      
      if (length(autouse_missing_cols) > 0) {
        shiny::validate(
          need(
            FALSE,
            paste0("Error: Missing required columns in loaded dataset.\n",
                   "Required columns missing from loaded dataset:\n",
                   paste0("* ", autouse_missing_cols, collapse = "\n"))
          )
        )
      } 
      
      # save to tadat
      tadat$df_autouse_input <- df_autouse_input
      
      # return
      df_autouse_input
    }) # end of df_autouse_input
    
    # render data in a table
    output$df_autouse_input_dt <- DT::renderDT({
      # validate data is there
      # shiny::req(df_autouse_input())
      shiny::validate(need(!is.null(input$autouse_input_file), "No file selected."))
      
      # render table
      DT::datatable(df_autouse_input(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE))
    }) # end renderDT
    
    # render summary
    output$autouse_input_summary <- shiny::renderText({
      # if file was selected
      if (is.null(df_autouse_input)) {
        
        # print
        "No file selected or file invalid."
      }
      
      # 
      else {
        # define data to summarize
        df_autouse_summary <- df_autouse_input()
        
        # print
        paste0(
          "Loaded dataset has ", nrow(df_autouse_summary), " rows and ", ncol(df_autouse_summary), " columns.\n",
          "There are ", length(unique(df_autouse_summary$JoinAUApp.AssessmentUnitIdentifier)), " unique assessment units.",
          "There are ", length(unique(df_autouse_summary$ATTAINS.UseName)), " unique use types."
        )
      }
    }) # end renderText
    
    # enable second tab to be selected once input data is processed
    shiny::observe({
      if (files_loaded$mlid & files_loaded$mltoau & files_loaded$autouse){
      shinyjs::enable(selector = '.nav li a[data-value="Batch"]') # also custom!
      } else {
      shinyjs::disable(selector = '.nav li a[data-value="Batch"]') # also custom!
      }})

  }) # end of moduleServer
} # end of server function
    
## To be copied in the UI
# mod_load_file_ui("load_file_1")
    
## To be copied in the server
# mod_load_file_server("load_file_1")
