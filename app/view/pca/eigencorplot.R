box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/pca/eigencorplot[
    compute_eigencor_data,
    create_eigencor_plot,
  ],
)

#' Render eigencorrelation plot output
#'
#' Wires up the ggiraph output for the eigencorrelation
#' heatmap (PC dimensions vs metadata). Uses dependency
#' injection from the parent pca module.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param pca_result Reactive returning the PCA computation
#'   result wrapper (with $success and $result)
#' @param display_ncp Reactive returning the number of dimensions
#' @export
render_output <- function(input, output, session,
                          pca_result, display_ncp = NULL) {
  ns <- session$ns

  output$eigencorplot <- ggiraph$renderGirafe({
    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    # Check metadata availability
    meta <- pca_res$result$ind$meta
    if (is.null(meta)) return(NULL)
    if ("Row" %in% names(meta) && ncol(meta) == 1) return(NULL)

    ncp <- if (!is.null(display_ncp)) {
      display_ncp()
    } else {
      5L
    }
    if (is.null(ncp)) ncp <- 5L

    # Compute correlation data
    eigencor_res <- compute_eigencor_data(
      pca_result = pca_res$result,
      display_ncp = ncp
    )
    if (!eigencor_res$success) return(NULL)

    # Create the plot
    plot_res <- create_eigencor_plot(eigencor_res$result)
    if (!plot_res$success) return(NULL)

    # Adaptive SVG sizing
    n_dims <- nrow(eigencor_res$result$cor_matrix)
    n_meta <- ncol(eigencor_res$result$cor_matrix)
    width_svg <- min(max(n_meta * 1.2 + 3, 6), 14)
    height_svg <- min(max(n_dims * 0.8 + 2, 4), 12)

    ggiraph$girafe(
      ggobj = plot_res$result,
      width_svg = width_svg,
      height_svg = height_svg,
      options = list(
        ggiraph$opts_hover(
          css = paste0(
            "fill-opacity:1;",
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
