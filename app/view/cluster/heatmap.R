box::use(
  bsicons,
  plotly,
  shiny,
)

box::use(
  app/logic/cluster/heatmap[create_cluster_heatmap],
  app/logic/error_handling,
  app/view/error_display,
)

#' Render heatmap panel content
#'
#' Returns UI for the cluster heatmap accordion panel.
#' For hierarchical clustering: plotlyOutput with native
#' dendrograms. For other algorithms: plotlyOutput with
#' independently computed dendrograms and an info banner.
#'
#' @param cluster_result Result from run_clustering()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object
#' @export
render_heatmap_content <- function(cluster_result, ns) {
  if (is.null(cluster_result)) {
    return(shiny$tags$div(
      class = "text-muted p-3",
      "No cluster results available."
    ))
  }

  variant <- cluster_result$details$variant

  info_banner <- if (variant != "hclust") {
    render_non_hierarchical_note(variant)
  }

  shiny$tagList(
    info_banner,
    shiny$tags$div(
      class = "mt-2",
      plotly$plotlyOutput(
        ns("heatmap_plot"), height = "600px"
      )
    )
  )
}

#' Server-side rendering for the cluster heatmap
#'
#' Renders the heatmap as an interactive plotly widget.
#' Re-renders reactively when display options change.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param cluster_result_rv reactiveVal with cluster result
#' @param membership_data_rv reactiveVal with membership
#'   data frame (includes metadata columns)
#' @param analysis_data_rv reactiveVal with scaled data
#' @param measure_cols_rv reactiveVal with measure col names
#' @export
render_output <- function(input, output, session,
                          cluster_result_rv,
                          membership_data_rv,
                          analysis_data_rv,
                          measure_cols_rv) {
  output$heatmap_plot <- plotly$renderPlotly({
    res <- cluster_result_rv()
    if (is.null(res)) return(NULL)

    analysis_data <- analysis_data_rv()
    measure_cols <- measure_cols_rv()
    if (is.null(analysis_data) ||
        is.null(measure_cols)) {
      return(NULL)
    }

    show_labels <- isTRUE(input$showLabels)
    seriation <- input$seriation %||% "OLO"

    # Build custom labels from selected column
    custom_labels <- NULL
    label_col <- input$labelColumn
    if (show_labels && !is.null(label_col) &&
        nzchar(label_col)) {
      md <- membership_data_rv()
      if (!is.null(md) && label_col %in% names(md)) {
        custom_labels <- as.character(
          md[[label_col]]
        )
      }
    }

    # Build row side colors from selected columns
    row_side_colors_df <- NULL
    side_cols <- input$rowSideColors
    if (!is.null(side_cols) && length(side_cols) > 0) {
      md <- membership_data_rv()
      if (!is.null(md)) {
        valid_cols <- intersect(side_cols, names(md))
        if (length(valid_cols) > 0) {
          row_side_colors_df <- md[
            , valid_cols, drop = FALSE
          ]
        }
      }
    }

    # Map scale_method to heatmaply scale param
    scale_method <- input$scale_method
    scale_heatmap <- "none"

    hm_result <- create_cluster_heatmap(
      res,
      data = analysis_data,
      measure_cols = measure_cols,
      show_labels = show_labels,
      custom_labels = custom_labels,
      seriation = seriation,
      row_side_colors_df = row_side_colors_df,
      scale_heatmap = scale_heatmap
    )

    if (!hm_result$success) {
      return(NULL)
    }

    hm_result$result
  })
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_non_hierarchical_note <- function(variant) {
  algo_label <- switch(
    variant,
    kmeans = "K-Means",
    pam    = "K-Means (PAM)",
    dbscan = "DBSCAN",
    variant
  )

  shiny$tags$div(
    class = paste(
      "alert alert-info",
      "d-flex align-items-start mb-2"
    ),
    bsicons$bs_icon(
      "info-circle-fill",
      class = "me-2 flex-shrink-0 mt-1"
    ),
    shiny$tags$div(
      shiny$tags$strong(
        paste0(
          "Dendrograms computed independently ",
          "(", algo_label, ")"
        )
      ),
      shiny$tags$p(
        class = "mb-0 mt-1",
        paste0(
          algo_label, " does not produce a merge ",
          "hierarchy. The dendrograms shown here ",
          "are computed independently from the data ",
          "using Ward's method to provide meaningful ",
          "row and column ordering. The heatmap ",
          "still reveals feature patterns across ",
          "the assigned clusters and helps validate ",
          "whether cluster assignments correspond ",
          "to distinct data profiles."
        )
      )
    )
  )
}
