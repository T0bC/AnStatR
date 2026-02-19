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
#' contribution plot, plus a figure caption that explains any
#' filtering applied (cos2 threshold, dropped dimensions).
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
  last_meta <- shiny$reactiveVal(NULL)

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

    plot_res <- create_var_contrib_jitter_plot(
      pca_result = pca_res$result,
      display_ncp = ncp,
      show_title = TRUE
    )

    if (!plot_res$success) return(NULL)

    res <- plot_res$result
    last_plot(res$plot)
    last_meta(res)

    # SVG sizing from actual filtered data
    plot_data <- res$plot$data
    n_facets <- length(unique(plot_data$dim_label))
    n_points <- max(table(plot_data$dim_label))
    width_svg <- min(max(n_facets * 2.5 + 3, 8), 12)
    height_svg <- min(max(n_points * 0.35 + 3, 6), 8)

    ggiraph$girafe(
      ggobj = res$plot,
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

  # Figure caption explaining filtering
  output$var_contrib_jitter_caption <- shiny$renderUI({
    meta <- last_meta()
    if (is.null(meta)) return(NULL)

    build_caption(meta)
  })

  list(plot = last_plot, meta = last_meta)
}


#' Build the figure caption HTML from filtering metadata
build_caption <- function(meta) {
  parts <- list()

  if (meta$filter_applied) {
    parts[[length(parts) + 1]] <- shiny$tags$span(
      sprintf(
        paste0(
          "Variables with cos\u00b2 < %.2f are filtered ",
          "per dimension (%d total variables). "
        ),
        meta$cos2_threshold,
        meta$n_vars_total
      )
    )

    if (length(meta$dropped_dims) > 0) {
      dim_details <- vapply(
        seq_along(meta$dropped_dims),
        function(i) {
          sprintf(
            "%s (max cos\u00b2 = %.3f)",
            meta$dropped_dims[i],
            meta$dropped_max_cos2[i]
          )
        },
        character(1)
      )
      parts[[length(parts) + 1]] <- shiny$tags$span(
        sprintf(
          paste0(
            "Dropped %d dimension%s with no variable ",
            "above threshold: %s. "
          ),
          length(meta$dropped_dims),
          if (length(meta$dropped_dims) != 1) "s" else "",
          paste(dim_details, collapse = ", ")
        )
      )
    }

    parts[[length(parts) + 1]] <- shiny$tags$span(
      sprintf(
        "Showing %d of %d requested dimensions.",
        meta$n_dims_shown,
        meta$n_dims_requested
      )
    )

    parts[[length(parts) + 1]] <- shiny$tags$span(
      paste0(
        " See the PCA Results tables ",
        "for unfiltered data."
      )
    )
  } else {
    parts[[length(parts) + 1]] <- shiny$tags$span(
      sprintf(
        paste0(
          "All %d variables shown across %d dimensions. ",
          "No filtering applied."
        ),
        meta$n_vars_total,
        meta$n_dims_shown
      )
    )
  }

  shiny$tags$figcaption(
    class = "text-muted small mt-2 px-2",
    style = "font-style: italic; line-height: 1.5;",
    parts
  )
}
