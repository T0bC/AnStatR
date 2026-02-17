box::use(
  plotly,
  shiny,
)

box::use(
  app/logic/pca/biplot3d[create_biplot3d],
)

#' Render 3D biplot output
#'
#' Wires up the plotly output for the 3D PCA biplot.
#' Reads sidebar inputs via a unified debounced reactive
#' to prevent race conditions when the user rapidly
#' changes inputs. Called by the parent pca module using
#' dependency injection.
#'
#' Exposes a `biplot3d_error` reactiveVal so the parent
#' module can display errors in the accordion panel via
#' error_display$error_alert_structured().
#'
#' @param input Shiny input object from parent module.
#'   Reads: input$dimX, input$dimY, input$dimZ,
#'   input$GroupBiplot
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param pca_result Reactive returning the PCA computation
#'   result wrapper (with $success and $result)
#' @return List with $error — a reactive returning the
#'   last error (or NULL)
#' @export
render_output <- function(input, output, session,
                          pca_result) {
  ns <- session$ns

  last_error <- shiny$reactiveVal(NULL)

  # Unified debounced params: bundle sidebar inputs
  # into one reactive with a single debounce.
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(
      params$dim_x,
      params$dim_y,
      params$dim_z,
      paste(params$group_cols, collapse = ","),
      sep = "|"
    )
  }

  shiny$observe({
    new_params <- list(
      dim_x = input$dimX,
      dim_y = input$dimY,
      dim_z = input$dimZ,
      group_cols = input$GroupBiplot
    )

    current <- cached_params()
    new_fp <- make_fingerprint(new_params)
    old_fp <- if (!is.null(current)) {
      make_fingerprint(current)
    } else {
      ""
    }
    if (new_fp != old_fp) {
      cached_params(new_params)
    }
  }) |> shiny$debounce(400)

  biplot3d_params <- shiny$reactive({
    cached_params()
  })

  output$biplot3d <- plotly$renderPlotly({
    last_error(NULL)

    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    params <- biplot3d_params()
    if (is.null(params)) return(NULL)

    # Extract params with defaults
    dim_x <- params$dim_x
    dim_y <- params$dim_y
    dim_z <- params$dim_z
    if (is.null(dim_x)) dim_x <- "Dim.1"
    if (is.null(dim_y)) dim_y <- "Dim.2"
    if (is.null(dim_z)) dim_z <- "Dim.3"

    # Need at least 3 distinct dims
    available <- colnames(pca_res$result$var$coord)
    if (length(available) < 3) return(NULL)

    group_cols <- params$group_cols
    if (is.null(group_cols) ||
        length(group_cols) == 0) {
      group_cols <- NULL
    }

    # Build the plotly via logic function
    plot_res <- create_biplot3d(
      pca_result = pca_res$result,
      dim_x = dim_x,
      dim_y = dim_y,
      dim_z = dim_z,
      group_cols = group_cols
    )

    if (!plot_res$success) {
      last_error(plot_res$error)
      return(NULL)
    }

    plot_res$result
  })

  list(error = last_error)
}
