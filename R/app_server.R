#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  # create list object to hold reactive values passed between modules
  tadat <- shiny::reactiveValues()
  
  # modules
  mod_load_file_server("load_file_1", tadat)
  mod_batch_analysis_server("batch_analysis_1", tadat)
  mod_custom_analysis_server("custom_analysis_1", tadat)
  
  # disable other tabs upon start
  # commeting this out for now so yu-chen can develop
  # shinyjs::disable(selector = '.nav li a[data-value="Batch"]')
  # shinyjs::disable(selector = '.nav li a[data-value="Custom"]')
  
  # save session info to tadat
  job_id <- paste0("ts", format(Sys.time(), "%Y%m%d%H%M%S"))
  tadat$default_outfile <- paste0("tada_analyze_output_", job_id)
  tadat$job_id <- job_id
}
