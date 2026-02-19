box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/pca/biplot[create_biplot],
)

#' Render biplot output
#'
#' Wires up the ggiraph output for the PCA biplot.
#' Reads sidebar inputs via a unified debounced reactive
#' to prevent race conditions when the user rapidly
#' changes inputs (e.g. GroupBiplot select/deselect).
#' Called by the parent pca module using dependency injection.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param pca_result Reactive returning the PCA computation
#'   result wrapper (with $success and $result)
#' @export
render_output <- function(input, output, session,
                          pca_result) {
  ns <- session$ns

  last_plot <- shiny$reactiveVal(NULL)

  # Unified debounced params: bundle all sidebar inputs
  # into one reactive with a single debounce to avoid
  # oscillation when inputs change in quick succession.
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(
      params$layer,
      params$dim_x,
      params$dim_y,
      paste(params$group_cols, collapse = ","),
      params$show_hull,
      params$point_alpha,
      params$point_size,
      params$show_title,
      sep = "|"
    )
  }

  shiny$observe({
    new_params <- list(
      layer = input$biplotLayer,
      dim_x = input$dimX,
      dim_y = input$dimY,
      group_cols = input$GroupBiplot,
      show_hull = input$showConvexHull,
      point_alpha = input$pointAlpha,
      point_size = input$pointSize,
      show_title = TRUE
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

  biplot_params <- shiny$reactive({ cached_params() })

  output$biplot <- ggiraph$renderGirafe({
    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    params <- biplot_params()
    if (is.null(params)) return(NULL)

    # Extract params with defaults
    layer <- params$layer
    if (is.null(layer)) layer <- "combined"

    dim_x <- params$dim_x
    dim_y <- params$dim_y
    if (is.null(dim_x)) dim_x <- "Dim.1"
    if (is.null(dim_y)) dim_y <- "Dim.2"

    group_cols <- params$group_cols
    if (is.null(group_cols) || length(group_cols) == 0) {
      group_cols <- NULL
    }

    show_hull <- isTRUE(params$show_hull)
    point_alpha <- params$point_alpha %||% "Contribution"
    point_size <- params$point_size %||% "Contribution"
    show_title <- isTRUE(params$show_title)

    # Build the ggplot via logic function
    plot_res <- create_biplot(
      pca_result = pca_res$result,
      dim_x = dim_x,
      dim_y = dim_y,
      layer = layer,
      group_cols = group_cols,
      show_convex_hull = show_hull,
      point_alpha = point_alpha,
      point_size = point_size,
      show_title = show_title
    )

    if (!plot_res$success) return(NULL)

    last_plot(plot_res$result)

    ggiraph$girafe(
      ggobj = plot_res$result,
      width_svg = 10,
      height_svg = 7,
      options = list(
        ggiraph$opts_sizing(rescale = TRUE, width = 1),
        ggiraph$opts_hover(
          css = paste0(
            "fill-opacity:0.8;",
            "stroke:black;stroke-width:2px;"
          )
        ),
        ggiraph$opts_tooltip(
          css = paste0(
            "background-color:white;padding:8px;",
            "border-radius:4px;",
            "border:1px solid #ccc;",
            "font-family:sans-serif;"
          ),
          use_fill = FALSE
        ),
        ggiraph$opts_selection(type = "none")
      )
    )
  })

  list(plot = last_plot)
}
