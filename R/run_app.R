#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  
  # Run app-start initialization
  internal_onStart <- function() {
    
    # set options (moved from top-level to avoid running during package load)
    options(shiny.maxRequestSize = get_golem_config("MB_LIMIT") * 1024^2)
    options(warn = 2)
    
    # Get the organization ID once per R process (cache in golem options)
    ATTAINS_orgs_vec <- tryCatch({
      ATTAINS_orgs <- suppressWarnings(suppressMessages(
        rExpertQuery::EQ_DomainValues("org_id")
      ))
      ATTAINS_orgs <- dplyr::arrange(ATTAINS_orgs, name)
      v <- ATTAINS_orgs$code
      names(v) <- ATTAINS_orgs$name
      v
    }, error = function(e) {
      warning("Failed to fetch ATTAINS org IDs: ", e$message)
      NULL
    })
    
    golem::set_golem_options(list(ATTAINS_orgs_vec = ATTAINS_orgs_vec))
    
    if (is.function(onStart)) onStart()
  }
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
