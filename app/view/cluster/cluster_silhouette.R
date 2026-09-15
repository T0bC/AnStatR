box::use(
  ggiraph,
  shiny,
  shinycssloaders,
)

box::use(
  app/logic/cluster/silhouette[
    compute_silhouette_data,
    create_silhouette_plot
  ],
)

#' Render cluster silhouette panel content
#'
#' Returns UI for the cluster silhouette accordion
#' panel.
#'
#' @param cluster_result Result from run_clustering()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object
#' @export
render_silhouette_content <- function(cluster_result,
                                      ns) {
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
          ns("cluster_silhouette_plot"),
          height = "500px"
        ),
        type = 6,
        color = "#0d6efd"
      )
    )
  )
}

#' Server-side rendering for the cluster silhouette
#'
#' Renders the silhouette plot as an interactive
#' ggiraph widget. Re-renders reactively when
#' cluster results or display options change.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent
#'   module
#' @param cluster_result_rv reactiveVal with cluster
#'   result
#' @param membership_data_rv reactiveVal with
#'   membership data frame
#' @param analysis_data_rv reactiveVal with scaled data
#' @param measure_cols_rv reactiveVal with measure
#'   column names
#' @export
render_output <- function(input, output, session,
                          cluster_result_rv,
                          membership_data_rv,
                          analysis_data_rv,
                          measure_cols_rv) {
  last_plot <- shiny$reactiveVal(NULL)
  last_error <- shiny$reactiveVal(NULL)

  # Debounced display params
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(
      paste(params$group_cols, collapse = ","),
      params$sil_sort_by,
      params$sil_show_avg_line,
      sep = "|"
    )
  }

  debounced_params_raw <- shiny$reactive({
    list(
      group_cols = input$groupBiplot,
      sil_sort_by = input$silSortBy,
      sil_show_avg_line = input$silShowAvgLine
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

  sil_params <- shiny$reactive({
    cached_params()
  })

  output$cluster_silhouette_plot <-
    ggiraph$renderGirafe({
      res <- cluster_result_rv()
      if (is.null(res)) {
        return(NULL)
      }

      analysis_data <- analysis_data_rv()
      measure_cols <- measure_cols_rv()
      if (is.null(analysis_data) ||
            is.null(measure_cols)) {
        return(NULL)
      }

      # Compute silhouette data
      num_data <- as.matrix(
        analysis_data[, measure_cols, drop = FALSE]
      )
      sil_res <- compute_silhouette_data(
        data = num_data,
        clusters = res$clusters,
        metric = res$metric
      )

      if (!sil_res$success) {
        last_error(sil_res$error)
        last_plot(NULL)
        return(NULL)
      }

      # Extract display params
      params <- sil_params()
      sort_by <- if (
        !is.null(params$sil_sort_by)
      ) {
        params$sil_sort_by
      } else {
        "width"
      }
      show_avg_line <- if (
        !is.null(params$sil_show_avg_line)
      ) {
        isTRUE(params$sil_show_avg_line)
      } else {
        TRUE
      }

      # Resolve group columns (exclude "CLUSTER")
      group_cols <- params$group_cols
      if (is.null(group_cols) ||
            length(group_cols) == 0) {
        group_cols <- NULL
      }

      md <- membership_data_rv()

      plot_res <- create_silhouette_plot(
        sil_data = sil_res$result,
        membership_data = md,
        group_cols = group_cols,
        sort_by = sort_by,
        show_avg_line = show_avg_line
      )

      if (!plot_res$success) {
        last_error(plot_res$error)
        last_plot(NULL)
        return(NULL)
      }

      last_error(NULL)
      last_plot(plot_res$result$plot)

      n_obs <- nrow(sil_res$result$sil_df)
      svg_width <- max(7, min(14, n_obs / 40))

      ggiraph$girafe(
        ggobj = plot_res$result$plot,
        width_svg = svg_width,
        height_svg = 5,
        options = list(
          ggiraph$opts_hover(
            css = paste0(
              "fill-opacity:1;",
              "stroke:black;stroke-width:1.5px;"
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
