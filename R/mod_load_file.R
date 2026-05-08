#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' 
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
        htmltools::h3("Purpose"),
        htmltools::p("This app is used to analyze water quality data based on the criteria table (Step 2) through 
        either a batch analysis (Step 3) or custom analysis (Step 4)."),
        
        htmltools::p("This tab is used to upload three data files: 
                     The water quality data (Step 1a), monitoring location to assessment unit (AU) crosswalk table (Step 1b),
                     and an AU to Designated Uses crosswalk table (Step 1c).
                     If the tables in Steps 1b or 1c are not available, 
                     user will not be able to match the ATTAINS AU identifier or ATTAINS use name, respectively, for use in their batch and custom tab analysis."),
        
        htmltools::h3("Instructions"),
        htmltools::p("First, upload water 
        quality data (Step 1a). The water quality file for this app is a 
        long-format data file with MLs and water quality data. You can download 
        a file with MLs and associated chemistry data from the Water Quality 
        Portal or use the", htmltools::a("TADAShinyApp."
                                         , href = "https://rconnect-public.epa.gov/TADAShiny/"
                                         , target = "_blank")),
        htmltools::p("Second, upload a ML to AU crosswalk table (Step 1b, optional)."),
        htmltools::p("Third, upload an AU to Designated Uses crosswalk table (Step 1c, optionl).
        The files for Steps 1b and 1c can be supplied from an organization 
        or generated using the", htmltools::a("TADAShinyJoinToAU App."
            , href = "https://tetratech-wtr-wne.shinyapps.io/TADAShinyJoinToAU/"
            , target = "_blank")),
        htmltools::p("Users need to at least upload the water 
        quality data to proceed to the Batch and Custom Analysis tab. If the tables in the Steps 1b or 1c are not available, 
                     the tool will not match the ATTAINS.UseName from the criteria table to the assessment units."),
        htmltools::h2("1a. Load Water Quality Data File"),
        htmltools::h3("Select file parameters"),
        shiny::radioButtons(
          inputId = ns("mlid_separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Excel = "excel" , Tab = "\t"),
          selected = ","
        ),
        shiny::uiOutput(ns("mlid_file_input_ui"))
      ),
      
      # data table column
      shiny::column(
        width = 8,
        htmltools::h3("Water Quality Data File Summary"),
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
        htmltools::h2("1b. Load ML ID to AU ID Crosswalk File (Optional)"),
        htmltools::h3("Select file parameters"),
        shiny::radioButtons(
          inputId = ns("mltoau_separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Excel = "excel" , Tab = "\t"),
          selected = ","
        ),
        shiny::uiOutput(ns("mltoau_file_input_ui"))
        # shiny::fileInput(
        #   inputId = ns("mltoau_input_file"),
        #   label = "Choose file to load:",
        #   width = "90%",
        #   placeholder = "No file selected.",
        #   multiple = FALSE,
        #   accept = c(
        #     "text/csv",
        #     "text/comma-separated-values",
        #     "text/tab-separated-values",
        #     "text/plain",
        #     ".csv", ".tsv", ".txt", ".xlsx"
        #   )
        # )
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
        htmltools::h2("1c. Load AU ID to Uses Crosswalk File (Optional)"),
        htmltools::h3("Select file parameters"),
        shiny::radioButtons(
          inputId = ns("autouse_separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Excel = "excel" , Tab = "\t"),
          selected = ","
        ),
        shiny::uiOutput(ns("autouse_file_input_ui"))
        # shiny::fileInput(
        #   inputId = ns("autouse_input_file"),
        #   label = "Choose file to load:",
        #   width = "90%",
        #   placeholder = "No file selected.",
        #   multiple = FALSE,
        #   accept = c(
        #     "text/csv",
        #     "text/comma-separated-values",
        #     "text/tab-separated-values",
        #     "text/plain",
        #     ".csv", ".tsv", ".txt", ".xlsx"
        #   )
        # )
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
    
    fluidRow(
      column(
        width = 12,
        mod_tada_plots_ui(ns("TADA_Plots"))
      )
    )
    
  )
}
    
#' load_file Server Functions
#'
#' @noRd 
mod_load_file_server <- function(id, tadat){
  shiny::moduleServer(id, function(input, output, session){
    # get module session id
    ns <- session$ns
    
    # Render dynamic file input for mlid
    output$mlid_file_input_ui <- shiny::renderUI({
      accept_types <- switch(
        input$mlid_separator,
        "," = c("text/csv", "text/comma-separated-values", ".csv"),
        "excel" = c(".xlsx", ".xls"),
        "\t" = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"),
        c(".csv", ".tsv", ".txt", ".xlsx", ".xls") # default
      )
      
      shiny::fileInput(
        inputId = ns("mlid_input_file"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = accept_types
      )
    })
    
    # Render dynamic file input for mltoau
    output$mltoau_file_input_ui <- shiny::renderUI({
      accept_types <- switch(
        input$mltoau_separator,
        "," = c("text/csv", "text/comma-separated-values", ".csv"),
        "excel" = c(".xlsx", ".xls"),
        "\t" = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"),
        c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
      )
      
      shiny::fileInput(
        inputId = ns("mltoau_input_file"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = accept_types
      )
    })
    
    # Render dynamic file input for autouse
    output$autouse_file_input_ui <- shiny::renderUI({
      accept_types <- switch(
        input$autouse_separator,
        "," = c("text/csv", "text/comma-separated-values", ".csv"),
        "excel" = c(".xlsx", ".xls"),
        "\t" = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"),
        c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
      )
      
      shiny::fileInput(
        inputId = ns("autouse_input_file"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = accept_types
      )
    })
    
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
      
      
      # define file path and extension
      file_path_mlid_input <- input$mlid_input_file$datapath
      file_ext_mlid_input <- tools::file_ext(file_path_mlid_input)
      
      # log to command line
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Monitoring Location Import, separator: '", input$mlid_separator, "'\n",
          "Monitoring Location Import, file name: ", input$mlid_input_file$name, "\n",
          "Monitoring Location Import, file path: ", file_path_mlid_input, "\n",
          "Monitoring Location Import, file extension: ", file_ext_mlid_input, "\n"
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
      
      # read user imported file based on extension
      if (file_ext_mlid_input %in% c("csv", "tsv", "txt")) {
        df_mlid_input <- utils::read.delim(file_path_mlid_input, header = TRUE
                                           , sep = input$mlid_separator
                                           , stringsAsFactors = FALSE
                                           , na.strings = c("", "NA"))
      } else if (file_ext_mlid_input %in% c("xlsx", "xls")) {
        df_mlid_input <- readxl::read_excel(file_path_mlid_input, na = c("NA","")
                                            , trim_ws = TRUE, col_names = TRUE
                                            , guess_max = 100000)
      } else {
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      } # END ~ if/else
      
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
                                   scrollY = "400px",
                                   scrollCollapse = TRUE,
                                   paging = TRUE,
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
          "There are ", length(unique(df_mlid_summary$TADA.MonitoringLocationIdentifier)), " unique monitoring locations."
        )
      }
    }) # end renderText
    

    
    #### 2. ml to au crosswalk file loaded event ####
    df_mltoau_input <- shiny::eventReactive(input$mltoau_input_file, {
      
      # validate file is selected
      shiny::validate(need(!is.null(input$mltoau_input_file), "No file selected."))
      
      # define file path and extension
      file_path_mltoau <- input$mltoau_input_file$datapath
      file_ext_mltoau <- tools::file_ext(file_path_mltoau)
      
      # log to command line
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Monitoring Location Import, separator: '", input$mltoau_separator, "'\n",
          "Monitoring Location Import, file name: ", input$mltoau_input_file$name, "\n",
          "Monitoring Location Import, file path: ", file_path_mltoau, "\n",
          "Monitoring Location Import, file extension: ", file_ext_mltoau, "\n"
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
      
      # read user imported file based on extension
      if (file_ext_mltoau %in% c("csv", "tsv", "txt")) {
        df_mltoau_input <- utils::read.delim(file_path_mltoau, header = TRUE
                                           , sep = input$mltoau_separator
                                           , stringsAsFactors = FALSE
                                           , na.strings = c("", "NA"))
      } else if (file_ext_mltoau %in% c("xlsx", "xls")) {
        df_mltoau_input <- readxl::read_excel(file_path_mltoau, na = c("NA","")
                                            , trim_ws = TRUE, col_names = TRUE
                                            , guess_max = 100000)
      } else {
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      } # END ~ if/else
      
      # define required columns
      # TODO need to check this is correct
      mltoau_required_cols <- c("TADA.MonitoringLocationIdentifier",
                                "ATTAINS.AssessmentUnitIdentifier")
      
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
                                   scrollY = "400px",
                                   scrollCollapse = TRUE,
                                   paging = TRUE,
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
          "There are ", length(unique(df_mltoau_summary$TADA.MonitoringLocationIdentifier)), " unique monitoring locations.",
          "There are ", length(unique(df_mltoau_summary$ATTAINS.AssessmentUnitIdentifier)), " unique assessment units."
        )
      }
    }) # end renderText
    
    #### 3. au to use crosswalk file loaded event ####
    df_autouse_input <- shiny::eventReactive(input$autouse_input_file, {
      
      # validate file is selected
      shiny::validate(need(!is.null(input$autouse_input_file), "No file selected."))
      
      # define file path and extension
      file_path_autouse <- input$autouse_input_file$datapath
      file_ext_autouse <- tools::file_ext(file_path_autouse)
      
      # log to command line
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Monitoring Location Import, separator: '", input$autouse_separator, "'\n",
          "Monitoring Location Import, file name: ", input$autouse_input_file$name, "\n",
          "Monitoring Location Import, file path: ", file_path_autouse, "\n",
          "Monitoring Location Import, file extension: ", file_ext_autouse, "\n"
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
      
      # read user imported file based on extension
      if (file_ext_autouse %in% c("csv", "tsv", "txt")) {
        df_autouse_input <- utils::read.delim(file_path_autouse, header = TRUE
                                             , sep = input$autouse_separator
                                             , stringsAsFactors = FALSE
                                             , na.strings = c("", "NA"))
      } else if (file_ext_autouse %in% c("xlsx", "xls")) {
        df_autouse_input <- readxl::read_excel(file_path_autouse, na = c("NA","")
                                              , trim_ws = TRUE, col_names = TRUE
                                              , guess_max = 100000)
      } else {
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      } # END ~ if/else
      
      # define required columns
      # TODO need to check this is correct
      autouse_required_cols <- c("ATTAINS.AssessmentUnitIdentifier",
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
                                   scrollY = "400px",
                                   scrollCollapse = TRUE,
                                   paging = TRUE,
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
          "There are ", length(unique(df_autouse_summary$ATTAINS.AssessmentUnitIdentifier)), " unique assessment units.",
          "There are ", length(unique(df_autouse_summary$ATTAINS.UseName)), " unique use types."
        )
      }
    }) # end renderText
    
    # enable criteria table to be selected once input data is processed
    shiny::observe({
      if (files_loaded$mlid){
        shinyjs::enable(selector = '.nav li a[data-value="Criteria"]')
      } else {
        shinyjs::disable(selector = '.nav li a[data-value="Criteria"]')
      }})
    
    # # enable batch tab to be selected once input data is processed
    # shiny::observe({
    #   if (files_loaded$mlid){
    #   shinyjs::enable(selector = '.nav li a[data-value="Batch"]')
    #   } else {
    #   shinyjs::disable(selector = '.nav li a[data-value="Batch"]')
    #   }})
    # 
    # # enable the custom tab to be selected once input data is processed
    # shiny::observe({
    #   if (files_loaded$mlid){
    #     shinyjs::enable(selector = '.nav li a[data-value="Custom"]')
    #   } else {
    #     shinyjs::disable(selector = '.nav li a[data-value="Custom"]')
    #   }})
    
    # Save files_loaded to tadat
    shiny::observe({
      tadat$files_loaded_mlid <- files_loaded$mlid
      tadat$files_loaded_mltoau <- files_loaded$mltoau
      tadat$files_loaded_autouse <- files_loaded$autouse
    })

    ###############################
    mod_tada_plots_server(
      id = "TADA_Plots",
      df_mlid_input = reactive({
        tadat$df_mlid_input
      }),
      user_choice = reactive({
        req(tadat$df_mlid_input)
        # Pick all ComparableDataIdentifier by default (or narrow as needed)
        unique(na.omit("PH_NONE_NONE_NONE"))
      }),
      user_choice_ML = reactive({
        req(tadat$df_mlid_input)
        rows_sel <- input$df_mlid_input_dt_rows_selected
        if (!is.null(rows_sel) && length(rows_sel) > 0) {
          # Use MLs from selected rows in the preview table
          unique(tadat$df_mlid_input$MonitoringLocationIdentifier[rows_sel])
        } else {
          # Fallback: include all MLs so plots don’t filter to empty
          unique(na.omit(tadat$df_mlid_input$MonitoringLocationIdentifier))
        }
      })
    )
    
  }) # end of moduleServer
} # end of server function
    
## To be copied in the UI
# mod_load_file_ui("load_file_1")
    
## To be copied in the server
# mod_load_file_server("load_file_1")
