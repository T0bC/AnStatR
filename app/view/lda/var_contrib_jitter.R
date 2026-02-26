box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/lda/lda_var_contrib[lda_to_pca_var_structure],
  app/logic/pca/var_contrib_jitter[
    create_var_contrib_jitter_plot
  ],
)

#' Render variable contribution jitter plot for LDA/QDA/MDA
#'
#' Converts the LDA scaling matrix to PCA-like structure,
#' then delegates to the shared jitter plot function.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param lda_result Reactive returning the LDA computation
#'   result (the raw result, not the safe_execute wrapper)
#' @export
render_output <- function(input, output, session,
                          lda_result) {
  ns <- session$ns

  last_plot <- shiny$reactiveVal(NULL)
  last_meta <- shiny$reactiveVal(NULL)
  last_analysis_type <- shiny$reactiveVal(NULL)

  output$var_contrib_jitter <- ggiraph$renderGirafe({
    res <- lda_result()
    if (is.null(res)) return(NULL)

    # Convert LDA scaling to PCA-like structure
    pca_like <- lda_to_pca_var_structure(res)
    if (is.null(pca_like)) return(NULL)

    n_dims <- ncol(pca_like$var$contrib)

    plot_res <- create_var_contrib_jitter_plot(
      pca_result = pca_like,
      display_ncp = n_dims,
      show_title = TRUE
    )

    if (!plot_res$success) return(NULL)

    plot_data <- plot_res$result
    last_plot(plot_data$plot)
    last_meta(plot_data)
    last_analysis_type(res$analysis_type)

    # SVG sizing from actual filtered data
    df <- plot_data$plot$data
    n_facets <- length(unique(df$dim_label))
    n_points <- max(table(df$dim_label))
    width_svg <- min(max(n_facets * 2.5 + 3, 8), 12)
    height_svg <- min(max(n_points * 0.35 + 3, 6), 8)

    ggiraph$girafe(
      ggobj = plot_data$plot,
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

    build_caption(meta, last_analysis_type())
  })

  list(plot = last_plot, meta = last_meta)
}


#' Build the figure caption HTML from filtering metadata
build_caption <- function(meta, analysis_type = NULL) {
  parts <- list()

  if (identical(analysis_type, "qda")) {
    parts[[length(parts) + 1]] <- shiny$tags$span(
      class = "text-info",
      paste0(
        "Note: QDA does not produce linear ",
        "discriminant axes. This plot uses ",
        "coefficients from a companion LDA fit ",
        "to approximate variable contributions. "
      )
    )
  }

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
          if (length(meta$dropped_dims) != 1) {
            "s"
          } else {
            ""
          },
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
        " See the LDA Results tables ",
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
