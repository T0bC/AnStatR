box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/pca/ind_contrib[create_ind_contrib_plot],
)

#' Render individual contribution jitter plot output
#'
#' Wires up the ggiraph output for the individual
#' contribution plot. Reads sidebar inputs via a unified
#' debounced reactive. Called by the parent pca module
#' using dependency injection.
#'
#' @param input Shiny input object from parent module.
#'   Reads: input$GroupBiplot, input$title
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param pca_result Reactive returning the PCA computation
#'   result wrapper (with $success and $result)
#' @param display_ncp Reactive returning the number of
#'   dimensions to display
#' @export
render_output <- function(input, output, session,
                          pca_result,
                          display_ncp = NULL) {
  ns <- session$ns

  last_plot <- shiny$reactiveVal(NULL)

  # Unified debounced params
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(
      paste(params$group_cols, collapse = ","),
      params$show_title,
      sep = "|"
    )
  }

  shiny$observe({
    new_params <- list(
      group_cols = input$GroupBiplot,
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

  ic_params <- shiny$reactive({ cached_params() })

  output$ind_contrib_plot <- ggiraph$renderGirafe({
    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    ncp <- if (!is.null(display_ncp)) {
      display_ncp()
    } else {
      5L
    }
    if (is.null(ncp)) ncp <- 5L

    params <- ic_params()
    if (is.null(params)) return(NULL)

    group_cols <- params$group_cols
    if (is.null(group_cols) ||
        length(group_cols) == 0) {
      group_cols <- NULL
    }
    show_title <- isTRUE(params$show_title)

    plot_res <- create_ind_contrib_plot(
      pca_result = pca_res$result,
      display_ncp = ncp,
      group_cols = group_cols,
      show_title = show_title
    )

    if (!plot_res$success) return(NULL)

    last_plot(plot_res$result)

    # SVG sizing: width scales with dims,
    # height is fixed for jitter readability
    n_dims_vis <- min(
      ncp, ncol(pca_res$result$ind$contrib)
    )
    width_svg <- min(max(n_dims_vis * 1.5 + 2, 6), 14)
    height_svg <- 6

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

  list(plot = last_plot)
}
