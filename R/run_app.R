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
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  required <- c("golem", "config")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "The optional app requires: ",
      paste(missing, collapse = ", "),
      ". Install them before calling `run_app()`.",
      call. = FALSE
    )
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
