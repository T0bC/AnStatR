box::use(
  bsicons,
  shiny,
)

box::use(
  app/logic/cluster/dendrogram[create_dendrogram_plot],
  app/logic/error_handling,
  app/view/error_display,
)

#' Render dendrogram panel content
#'
#' Returns UI for the dendrogram accordion panel.
#' For hierarchical clustering: plotOutput + download buttons.
#' For other algorithms: info alert explaining dendrograms
#' are only available for hierarchical clustering.
#'
#' @param cluster_result Result from run_clustering()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object
#' @export
render_dendrogram_content <- function(cluster_result, ns) {
  if (is.null(cluster_result)) {
    return(shiny$tags$div(
      class = "text-muted p-3",
      "No cluster results available."
    ))
  }

  variant <- cluster_result$details$variant
  if (variant != "hclust") {
    return(render_non_hierarchical_message(variant))
  }

  shiny$tagList(
    shiny$tags$div(
      class = "mt-2",
      shiny$plotOutput(
        ns("dendrogram_plot"), height = "500px"
      )
    )
  )
}

#' Server-side rendering for the dendrogram plot
#'
#' Renders the dendrogram as a static ggplot. Re-renders
#' reactively when display options change.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param cluster_result_rv reactiveVal with cluster result
#' @param last_plot_rv reactiveVal to store the ggplot for
#'   download handlers
#' @export
render_output <- function(input, output, session,
                          cluster_result_rv,
                          last_plot_rv) {
  output$dendrogram_plot <- shiny$renderPlot({
    res <- cluster_result_rv()
    if (is.null(res)) return(NULL)
    if (res$details$variant != "hclust") return(NULL)

    horiz <- isTRUE(input$horizDendro)
    polar <- isTRUE(input$polarDend)
    show_labels <- isTRUE(input$showLabels)

    plot_result <- create_dendrogram_plot(
      res,
      horiz = horiz,
      polar = polar,
      show_labels = show_labels
    )

    if (!plot_result$success) {
      last_plot_rv(NULL)
      return(NULL)
    }

    last_plot_rv(plot_result$result)
    plot_result$result
  })
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_non_hierarchical_message <- function(variant) {
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
      "d-flex align-items-start"
    ),
    bsicons$bs_icon(
      "info-circle-fill",
      class = "me-2 flex-shrink-0 mt-1"
    ),
    shiny$tags$div(
      shiny$tags$strong(
        "Dendrogram not available"
      ),
      shiny$tags$p(
        class = "mb-0 mt-1",
        paste0(
          "Dendrograms visualize the hierarchical ",
          "merge tree and are only available for ",
          "hierarchical clustering. The current ",
          "analysis uses ", algo_label, ", which ",
          "does not produce a merge hierarchy."
        )
      ),
      shiny$tags$small(
        class = "text-muted d-block mt-1",
        paste(
          "To generate a dendrogram, select",
          "'Hierarchical' as the clustering",
          "algorithm and re-run the analysis."
        )
      )
    )
  )
}
