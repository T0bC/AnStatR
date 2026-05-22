box::use(
  plotly,
  shinycssloaders,
  shiny,
)

box::use(
  app/logic/cluster/cluster_biplot3d[create_cluster_biplot3d],
)

#' Render 3D cluster biplot panel content
#'
#' Returns UI for the 3D cluster biplot accordion panel.
#'
#' @param cluster_result Result from run_clustering()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object
#' @export
render_biplot3d_content <- function(cluster_result, ns) {
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
        plotly$plotlyOutput(
          ns("cluster_biplot3d_plot"),
          height = "600px"
        ),
        type = 6,
        color = "#0d6efd"
      )
    )
  )
}

#' Server-side rendering for the 3D cluster biplot
#'
#' Renders the 3D biplot as an interactive plotly widget.
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
  last_error <- shiny$reactiveVal(NULL)

  # Unified debounced params
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(
      params$dim_x,
      params$dim_y,
      params$dim_z,
      paste(params$group_cols, collapse = ","),
      params$reduction_method,
      sep = "|"
    )
  }

  debounced_params_raw <- shiny$reactive({
    list(
      dim_x = input$clusterBiplot3dDimX,
      dim_y = input$clusterBiplot3dDimY,
      dim_z = input$clusterBiplot3dDimZ,
      group_cols = input$groupBiplot,
      reduction_method = input$reductionMethod
    )
  }) |> shiny$debounce(400)

  shiny$observe({
    new_params <- debounced_params_raw()
    shiny$req(new_params)
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
  })

  biplot3d_params <- shiny$reactive({ cached_params() })

  output$cluster_biplot3d_plot <- plotly$renderPlotly({
    last_error(NULL)

    res <- cluster_result_rv()
    if (is.null(res)) return(NULL)

    analysis_data <- analysis_data_rv()
    raw_data <- cleaned_data_rv()
    measure_cols <- measure_cols_rv()
    if (is.null(analysis_data) ||
        is.null(measure_cols)) {
      return(NULL)
    }

    params <- biplot3d_params()
    if (is.null(params)) return(NULL)

    # Extract params with defaults
    dim_x <- params$dim_x %||% "Dim.1"
    dim_y <- params$dim_y %||% "Dim.2"
    dim_z <- params$dim_z %||% "Dim.3"

    reduction_method <- params$reduction_method %||% "pca"

    # Guard: validate dims match the reduction method
    data_source <- input$data_source
    is_reduced_source <- !is.null(data_source) &&
      data_source %in% c("pca_scores", "lda_scores")

    if (!is_reduced_source) {
      if (reduction_method == "pca" &&
          !grepl("^Dim\\.", dim_x)) {
        return(NULL)
      }
      if (reduction_method == "raw" &&
          grepl("^Dim\\.", dim_x)) {
        return(NULL)
      }
    }

    # Choose data source
    base_data <- if (reduction_method == "raw" &&
        !is.null(raw_data)) {
      raw_data
    } else {
      analysis_data
    }

    # Resolve grouping columns
    group_cols <- params$group_cols
    meta_cols <- character(0)
    plot_data <- base_data

    if (!is.null(group_cols) && length(group_cols) > 0) {
      md <- membership_data_rv()
      if (!is.null(md)) {
        for (gc in group_cols) {
          if (gc == "CLUSTER") {
            plot_data$CLUSTER <- as.factor(res$clusters)
            meta_cols <- c(meta_cols, "CLUSTER")
          } else if (gc %in% names(md) &&
                     !gc %in% names(plot_data)) {
            plot_data[[gc]] <- md[[gc]]
            meta_cols <- c(meta_cols, gc)
          } else if (gc %in% names(plot_data)) {
            meta_cols <- c(meta_cols, gc)
          }
        }
      }
    }

    meta_cols <- unique(meta_cols)

    # Check we have at least 3 dimensions available
    n_dims <- length(measure_cols)
    if (reduction_method == "pca") {
      # For PCA, we need at least 3 components
      if (n_dims < 3) {
        last_error(list(
          message = "Need at least 3 variables for 3D plot.",
          operation_name = "3D Cluster Plot"
        ))
        return(NULL)
      }
    } else {
      # For raw, check columns exist
      for (d in c(dim_x, dim_y, dim_z)) {
        if (!d %in% names(plot_data)) {
          return(NULL)
        }
      }
    }

    plot_res <- create_cluster_biplot3d(
      data = plot_data,
      measure_cols = measure_cols,
      clusters = res$clusters,
      meta_cols = meta_cols,
      dim_x = dim_x,
      dim_y = dim_y,
      dim_z = dim_z,
      group_cols = group_cols,
      reduction_method = reduction_method
    )

    if (!plot_res$success) {
      last_error(plot_res$error)
      return(NULL)
    }

    last_error(NULL)
    plot_res$result
  })

  list(error = last_error)
}
