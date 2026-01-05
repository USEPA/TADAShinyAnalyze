#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 

# set options
options(shiny.maxRequestSize = get_golem_config("MB_LIMIT")*1024^2)
options(warn = 2)

# Get the organization ID
ATTAINS_orgs <- rExpertQuery::EQ_DomainValues("org_id")
ATTAINS_orgs_vec <- ATTAINS_orgs$code
names(ATTAINS_orgs_vec) <- ATTAINS_orgs$name

# server
app_server <- function(input, output, session) {
  # Your application server logic
  
  # Fetch criteria file list ONCE at app startup
  criteria_file_list <- tryCatch({
    
    getCriteriaFiles(branch = "main")
  }, error = function(e) {
    warning("Failed to fetch criteria file list from GitHub: ", e$message)
    NULL
  })
  
  # create list object to hold reactive values passed between modules
  tadat <- shiny::reactiveValues()
  
  # Add explicit initialization
  tadat$criteria_file_list <- criteria_file_list
  
  tadat$df_mltoau_input <- NULL
  tadat$df_autouse_input <- NULL
  tadat$df_mlid_input <- NULL
  
  # modules
  mod_load_file_server("load_file_1", tadat)
  mod_criteria_table_server("criteria_table_1", tadat)
  mod_batch_analysis_server("batch_analysis_1", tadat)
  mod_custom_analysis_server("custom_analysis_1", tadat)
  
  # disable other tabs upon start
  shinyjs::disable(selector = '.nav li a[data-value="Criteria"]')
  shinyjs::disable(selector = '.nav li a[data-value="Batch"]')
  shinyjs::disable(selector = '.nav li a[data-value="Custom"]')
  
  # save session info to tadat
  job_id <- paste0("ts", format(Sys.time(), "%Y%m%d%H%M%S"))
  tadat$default_outfile <- paste0("tada_analyze_output_", job_id)
  tadat$job_id <- job_id
}
