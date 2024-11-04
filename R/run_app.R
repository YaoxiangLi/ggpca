#' Run the Shiny Application
#'
#' This function launches the Shiny application with the specified user interface and server function.
#' The function does not return a value but starts the Shiny app, allowing users to interact with it.
#'
#' @param ... Arguments to pass to `golem_opts`. See `?golem::get_golem_options` for more details.
#'
#' @inheritParams shiny::shinyApp
#'
#' @return No return value, called for side effects.
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
