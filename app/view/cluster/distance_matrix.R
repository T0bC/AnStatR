box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/cluster/distance_matrix[
    create_distance_ggplot,
    render_distance_girafe,
  ],
)

#' Render distance matrix plot output
#'
#' Wires up the ggiraph output for the distance matrix plot.
#' Checks for errors in the result and displays them via
#' error_alert_structured. Called by the parent cluster module
#' using dependency injection.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param distance_result Reactive returning the distance matrix
#'   computation result (from compute_distance_matrix)
#' @export
render_output <- function(input, output, session,
                          distance_result) {
  ns <- session$ns
  
  last_plot <- shiny$reactiveVal(NULL)
  
  output$distance_matrix_plot <- ggiraph$renderGirafe({
    result <- distance_result()
    if (is.null(result)) return(NULL)
    if (!result$success) return(NULL)
    last_plot(create_distance_ggplot(result$result))
    render_distance_girafe(result$result)
  })
  
  list(plot = last_plot)
}
