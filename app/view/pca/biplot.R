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
#' Reads sidebar inputs to decide which layer to render.
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

  output$biplot <- ggiraph$renderGirafe({
    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    # Read sidebar inputs
    layer <- input$biplotLayer
    if (is.null(layer)) layer <- "combined"

    dim_x <- input$dimX
    dim_y <- input$dimY
    if (is.null(dim_x)) dim_x <- "Dim.1"
    if (is.null(dim_y)) dim_y <- "Dim.2"

    group_cols <- input$GroupBiplot
    group_col <- if (!is.null(group_cols) &&
                     length(group_cols) == 1) {
      group_cols
    } else {
      NULL
    }

    show_hull <- isTRUE(input$showConvexHull)
    point_alpha <- input$pointAlpha %||% "Contribution"
    point_size <- input$pointSize %||% "Contribution"
    show_title <- isTRUE(input$title)

    # Build the ggplot via logic function
    plot_res <- create_biplot(
      pca_result = pca_res$result,
      dim_x = dim_x,
      dim_y = dim_y,
      layer = layer,
      group_col = group_col,
      show_convex_hull = show_hull,
      point_alpha = point_alpha,
      point_size = point_size,
      show_title = show_title
    )

    if (!plot_res$success) return(NULL)

    ggiraph$girafe(
      ggobj = plot_res$result,
      width_svg = 7,
      height_svg = 6,
      options = list(
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
}
