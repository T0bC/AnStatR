box::use(
  ggiraph,
  shinycssloaders,
  shiny,
)

box::use(
  app/logic/cluster/cluster_biplot[create_cluster_biplot],
)

#' Render cluster biplot panel content
#'
#' Returns UI for the cluster biplot accordion panel.
#'
#' @param cluster_result Result from run_clustering()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object
#' @export
render_biplot_content <- function(cluster_result, ns) {
  if (is.null(cluster_result)) {
    return(shiny$tags$div(
      class = "text-muted p-3",
      "No cluster results available."
    ))
  }

  shiny$tagList(
    shiny$tags$div(
      class = "mt-2",
      shinycssloaders$withSpinner(
        ggiraph$girafeOutput(
          ns("cluster_biplot_plot"),
          height = "600px"
        ),
        type = 6,
        color = "#0d6efd"
      )
    )
  )
}

#' Server-side rendering for the cluster biplot
#'
#' Renders the biplot as an interactive ggiraph widget.
#' Re-renders reactively when display options change.
#' Uses debounced params to prevent double-renders.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param cluster_result_rv reactiveVal with cluster result
#' @param membership_data_rv reactiveVal with membership
#'   data frame (includes metadata columns + Cluster)
#' @param analysis_data_rv reactiveVal with scaled data
#' @param cleaned_data_rv reactiveVal with raw unscaled
#'   cleaned data (used for "raw" reduction method)
#' @param measure_cols_rv reactiveVal with measure col names
#' @export
render_output <- function(input, output, session,
                          cluster_result_rv,
                          membership_data_rv,
                          analysis_data_rv,
                          cleaned_data_rv,
                          measure_cols_rv) {
  last_plot <- shiny$reactiveVal(NULL)
  last_error <- shiny$reactiveVal(NULL)

  # Unified debounced params
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(
      params$dim_x,
      params$dim_y,
      paste(params$group_cols, collapse = ","),
      params$reduction_method,
      params$show_group_shapes,
      params$show_hull,
      params$point_alpha,
      params$point_size,
      sep = "|"
    )
  }

  shiny$observe({
    new_params <- list(
      dim_x = input$clusterBiplotDimX,
      dim_y = input$clusterBiplotDimY,
      group_cols = input$groupBiplot,
      reduction_method = input$reductionMethod,
      show_group_shapes = input$showGroupShapes,
      show_hull = input$showConvexHull,
      point_alpha = input$pointAlpha,
      point_size = input$pointSize
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

  output$cluster_biplot_plot <- ggiraph$renderGirafe({
    res <- cluster_result_rv()
    if (is.null(res)) return(NULL)

    analysis_data <- analysis_data_rv()
    raw_data <- cleaned_data_rv()
    measure_cols <- measure_cols_rv()
    if (is.null(analysis_data) ||
        is.null(measure_cols)) {
      return(NULL)
    }

    params <- biplot_params()
    if (is.null(params)) return(NULL)

    # Extract params with defaults
    dim_x <- params$dim_x
    dim_y <- params$dim_y
    if (is.null(dim_x)) dim_x <- "Dim.1"
    if (is.null(dim_y)) dim_y <- "Dim.2"

    reduction_method <- params$reduction_method
    if (is.null(reduction_method)) {
      reduction_method <- "pca"
    }

    # Guard: validate dims match the reduction method
    # to avoid transient errors during method switching
    if (reduction_method == "pca" &&
        !grepl("^Dim\\.", dim_x)) {
      return(NULL)
    }
    if (reduction_method == "raw" &&
        grepl("^Dim\\.", dim_x)) {
      return(NULL)
    }

    group_cols <- params$group_cols
    if (is.null(group_cols) ||
        length(group_cols) == 0) {
      group_cols <- NULL
    }

    # show_convex_hull is only active when
    # showGroupShapes is enabled
    show_group_shapes <- isTRUE(
      params$show_group_shapes
    )
    show_hull <- show_group_shapes &&
      isTRUE(params$show_hull)
    point_alpha <- params$point_alpha %||% 1
    point_size <- params$point_size %||% 3

    # Choose data source: scaled for PCA, raw for
    # raw data mode
    base_data <- if (reduction_method == "raw" &&
        !is.null(raw_data)) {
      raw_data
    } else {
      analysis_data
    }

    # Resolve "CLUSTER" pseudo-column: inject cluster
    # assignments as a real column in the data
    meta_cols <- character(0)
    plot_data <- base_data

    if (!is.null(group_cols)) {
      md <- membership_data_rv()
      if (!is.null(md)) {
        # Add metadata columns to analysis data for PCA
        for (gc in group_cols) {
          if (gc == "CLUSTER") {
            plot_data$CLUSTER <- as.factor(
              res$clusters
            )
            meta_cols <- c(meta_cols, "CLUSTER")
          } else if (gc %in% names(md) &&
                     !gc %in% names(plot_data)) {
            plot_data[[gc]] <- md[[gc]]
            meta_cols <- c(meta_cols, gc)
          } else if (
            gc %in% names(plot_data)) {
            meta_cols <- c(meta_cols, gc)
          }
        }
      }
    }

    meta_cols <- unique(meta_cols)

    plot_res <- create_cluster_biplot(
      data = plot_data,
      measure_cols = measure_cols,
      clusters = res$clusters,
      meta_cols = meta_cols,
      dim_x = dim_x,
      dim_y = dim_y,
      group_cols = group_cols,
      show_convex_hull = show_hull,
      show_group_shapes = show_group_shapes,
      point_alpha = point_alpha,
      point_size = point_size,
      reduction_method = reduction_method,
      show_title = TRUE
    )

    if (!plot_res$success) {
      last_error(plot_res$error)
      last_plot(NULL)
      return(NULL)
    }

    last_error(NULL)
    last_plot(plot_res$result$plot)

    ggiraph$girafe(
      ggobj = plot_res$result$plot,
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
            "background-color:white;",
            "padding:8px;",
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

  list(
    plot = last_plot,
    error = last_error
  )
}
