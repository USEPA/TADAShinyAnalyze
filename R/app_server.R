#' Application server-side
#'
#' Initializes reactive state, fetches external resources with guarded error
#' handling, wires Shiny modules, and manages initial UI state.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny
#' @noRd

# Do NOT set global options here. Upload size and timeouts are configured in run_app().

app_server <- function(input, output, session) {
  # Fetch ATTAINS organization IDs
  ATTAINS_orgs_vec <- tryCatch(
    {
      ATTAINS_orgs <- suppressWarnings(suppressMessages(rExpertQuery::EQ_DomainValues(
        "org_id"
      )))
      ATTAINS_orgs <- dplyr::arrange(ATTAINS_orgs, name)
      v <- ATTAINS_orgs$code
      names(v) <- ATTAINS_orgs$name
      v
    },
    error = function(e) {
      warning("Failed to fetch ATTAINS org IDs: ", e$message)
      NULL
    }
  )

  # Fetch criteria file list ONCE at app startup
  criteria_file_list <- tryCatch(
    {
      EPATADA::TADA_ListCriteriaFiles()
    },
    error = function(e) {
      warning("Failed to fetch criteria file list from GitHub: ", e$message)
      NULL
    }
  )

  # Guard arrange: only sort when criteria_file_list is a data.frame with 'display_name'
  if (
    !is.null(criteria_file_list) &&
      is.data.frame(criteria_file_list) &&
      "display_name" %in% names(criteria_file_list)
  ) {
    criteria_file_list <- dplyr::arrange(criteria_file_list, display_name)
  }

  # create list object to hold reactive values passed between modules
  tadat <- shiny::reactiveValues()

  # Add explicit initialization
  tadat$criteria_file_list <- criteria_file_list
  tadat$ATTAINS_orgs_vec <- ATTAINS_orgs_vec

  tadat$df_mltoau_input <- NULL
  tadat$df_autouse_input <- NULL
  tadat$df_mlid_input <- NULL

  # modules
  mod_load_file_server("load_file_1", tadat)
  mod_criteria_table_server("criteria_table_1", tadat)
  mod_batch_analysis_server("batch_analysis_1", tadat)
  mod_custom_analysis_server("custom_analysis_1", tadat)
  mod_TADA_summary_server("TADA_summary_1")

  # disable other tabs upon start
  shinyjs::disable(selector = '.nav li a[data-value="Criteria"]')
  shinyjs::disable(selector = '.nav li a[data-value="Batch"]')
  shinyjs::disable(selector = '.nav li a[data-value="Custom"]')

  # save session info to tadat
  job_id <- paste0("ts", format(Sys.time(), "%Y%m%d%H%M%S"))
  tadat$default_outfile <- paste0("tada_analyze_output_", job_id)
  tadat$job_id <- job_id
}
