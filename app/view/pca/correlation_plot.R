box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/pca/correlation_plot[
    create_correlation_ggplot,
    render_correlation_girafe,
  ],
  app/view/error_display,
)

#' Render correlation plot output
#'
#' Wires up the ggiraph output for the correlation plot.
#' Checks for errors in the result and displays them via
#' error_alert_structured. Called by the parent pca module
#' using dependency injection.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param correlation_result Reactive returning the correlation
#'   computation result (from compute_correlation_data)
#' @export
render_output <- function(input, output, session,
                          correlation_result) {
  ns <- session$ns

  last_plot <- shiny$reactiveVal(NULL)

  output$correlation_plot <- ggiraph$renderGirafe({
    result <- correlation_result()
    if (is.null(result)) return(NULL)
    if (!result$success) return(NULL)
    last_plot(create_correlation_ggplot(result$result))
    render_correlation_girafe(result$result)
  })

  list(plot = last_plot)
}
