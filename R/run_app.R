#' Run the Shiny Application
#'
#' @description Launch the Shiny application.
#'
#' @inheritParams shiny::shinyApp onStart options enableBookmarking uiPattern
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
  limit_mb <- as.numeric(get_golem_config("MB_LIMIT", default = 500))
  if (!is.na(limit_mb)) {
    options(shiny.maxRequestSize = limit_mb * 1024^2)
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
