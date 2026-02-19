box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/pca/var_contrib_jitter[
    create_var_contrib_jitter_plot
  ],
)

#' Render variable contribution jitter plot output
#'
#' Wires up the ggiraph output for the jitter/strip variable
#' contribution plot. Dimensions on X, contribution % on Y,
#' colored by cos2, with smart ggrepel labels.
#' Called by the parent pca module using dependency injection.
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

  last_plot <- shiny$reactiveVal(NULL)

  # Debounced params for title input
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(params$show_title, sep = "|")
  }

  shiny$observe({
    new_params <- list(
      show_title = input$title
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

  vcj_params <- shiny$reactive({ cached_params() })

  output$var_contrib_jitter <- ggiraph$renderGirafe({
    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    ncp <- if (!is.null(display_ncp)) {
      display_ncp()
    } else {
      5L
    }
    if (is.null(ncp)) ncp <- 5L

    show_title <- isTRUE(vcj_params()$show_title)

    plot_res <- create_var_contrib_jitter_plot(
      pca_result = pca_res$result,
      display_ncp = ncp,
      show_title = show_title
    )

    if (!plot_res$success) return(NULL)

    last_plot(plot_res$result)

    # SVG sizing: width scales with number of facets,
    # height is generous to spread points vertically
    n_vars <- nrow(pca_res$result$var$contrib)
    n_dims_vis <- min(ncp, ncol(pca_res$result$var$contrib))
    width_svg <- min(max(n_dims_vis * 2.5 + 3, 8), 16)
    height_svg <- min(max(n_vars * 0.25 + 3, 6), 10)

    ggiraph$girafe(
      ggobj = plot_res$result,
      width_svg = width_svg,
      height_svg = height_svg,
      options = list(
        ggiraph$opts_sizing(rescale = TRUE, width = 1),
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

  list(plot = last_plot)
}
