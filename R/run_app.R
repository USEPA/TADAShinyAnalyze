#' Run the Shiny Application
#'
#' @description Launch the Shiny application.
#'
#' @inheritParams shiny::shinyApp
#' @param ... Named options forwarded to `golem_opts` via [golem::with_golem_options()],
#'   retrievable with [golem::get_golem_options()].
#'
#' @return A shiny.appobj returned by [shiny::shinyApp()].
#' @export
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  # Apply a 500 MB upload limit (from config, with 500 as fallback)
  # Use internal get_golem_config from app_config.R
  limit_mb_raw <- get_golem_config("MB_LIMIT", default = 500)
  limit_mb <- suppressWarnings(as.numeric(limit_mb_raw))[1]
  
  # Guard against NA/Inf and non-positive values
  if (!is.na(limit_mb) && is.finite(limit_mb) && limit_mb > 0) {
    base::options(shiny.maxRequestSize = limit_mb * 1024^2)
  }
  
  golem::with_golem_options(
    app = shiny::shinyApp(
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
