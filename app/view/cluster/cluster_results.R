box::use(
  bsicons,
  DT,
  shiny,
)

#' Render cluster analysis results
#'
#' Dispatches to algorithm-specific renderers based on the
#' clustering result. Shows a summary banner, cluster size
#' table, algorithm-specific details, and centers/medoids
#' table where applicable.
#'
#' @param cluster_result Result list from run_clustering()
#'   (the $result field, not the wrapper)
#' @return Shiny tagList with formatted cluster display
#' @export
render_cluster_results <- function(cluster_result, ns,
                                   cluster_summary = NULL) {
  if (is.null(cluster_result)) {
    return(shiny$tags$div(
      class = "text-muted p-3",
      "No cluster results available."
    ))
  }

  shiny$tagList(
    render_summary_banner(cluster_result),
    shiny$tags$div(
      class = "mt-3",
      render_cluster_sizes(cluster_result)
    ),
    shiny$tags$div(
      class = "mt-3",
      render_algorithm_details(cluster_result)
    ),
    render_cluster_profile(cluster_summary),
    render_membership_placeholder(ns),
    render_download_section(ns)
  )
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_summary_banner <- function(res) {
  algo_label <- switch(
    res$details$variant,
    kmeans = "K-Means",
    pam    = "K-Means (PAM)",
    hclust = paste0(
      "Hierarchical (",
      format_method_label(res$details$method),
      ")"
    ),
    dbscan = "DBSCAN",
    res$algorithm
  )

  metric_label <- paste0(
    toupper(substring(res$metric, 1, 1)),
    substring(res$metric, 2)
  )

  n_obs <- length(res$clusters)

  shiny$tags$div(
    class = paste(
      "alert alert-success",
      "d-flex align-items-center"
    ),
    bsicons$bs_icon(
      "check-circle-fill",
      class = "me-2 flex-shrink-0"
    ),
    shiny$tags$div(
      shiny$tags$strong(algo_label),
      shiny$tags$span(
        class = "mx-1", "\u2014"
      ),
      shiny$tags$span(
        class = "badge bg-primary me-1",
        paste(res$n_clusters, "clusters")
      ),
      shiny$tags$span(
        class = "badge bg-secondary me-1",
        paste(metric_label, "distance")
      ),
      shiny$tags$small(
        class = "d-block text-muted mt-1",
        paste(n_obs, "observations clustered")
      )
    )
  )
}

render_cluster_sizes <- function(res) {
  clusters <- res$clusters
  n_total <- length(clusters)

  # Build frequency table (sorted by cluster id)
  tbl <- table(clusters)
  cluster_ids <- as.integer(names(tbl))
  counts <- as.integer(tbl)
  pcts <- round(counts / n_total * 100, 1)

  is_dbscan <- res$details$variant == "dbscan"

  rows <- lapply(seq_along(cluster_ids), function(i) {
    cid <- cluster_ids[i]
    is_noise <- is_dbscan && cid == 0

    label <- if (is_noise) {
      shiny$tags$span(
        class = "text-warning",
        bsicons$bs_icon(
          "exclamation-triangle", class = "me-1"
        ),
        "Noise"
      )
    } else {
      shiny$tagList(
        cluster_badge_tag(cid),
        shiny$tags$span(
          class = "ms-1",
          paste("Cluster", cid)
        )
      )
    }

    row_class <- if (is_noise) {
      "table-warning"
    } else {
      ""
    }

    shiny$tags$tr(
      class = row_class,
      shiny$tags$td(label),
      shiny$tags$td(
        class = "text-end",
        counts[i]
      ),
      shiny$tags$td(
        class = "text-end",
        paste0(pcts[i], "%")
      )
    )
  })

  shiny$tags$div(
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("people", class = "me-1"),
      "Cluster Sizes"
    ),
    shiny$tags$div(
      class = "table-responsive",
      shiny$tags$table(
        class = paste(
          "table table-sm table-striped",
          "table-hover mb-0"
        ),
        shiny$tags$thead(
          shiny$tags$tr(
            shiny$tags$th("Cluster"),
            shiny$tags$th(
              class = "text-end", "Count"
            ),
            shiny$tags$th(
              class = "text-end", "Percentage"
            )
          )
        ),
        shiny$tags$tbody(rows)
      )
    )
  )
}

render_algorithm_details <- function(res) {
  d <- res$details

  # Shared quality metrics (computed for all algorithms)
  shared_items <- render_shared_quality_metrics(d)

  # Algorithm-specific extras
  extra_items <- switch(
    d$variant,
    kmeans = NULL,
    pam    = render_pam_extras(d),
    hclust = render_hclust_extras(d),
    dbscan = render_dbscan_extras(d),
    NULL
  )

  shiny$tags$div(
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("bar-chart-line", class = "me-1"),
      "Cluster Quality"
    ),
    shared_items,
    extra_items
  )
}

render_shared_quality_metrics <- function(d) {
  items <- list()

  # Silhouette
  if (
    !is.null(d$silhouette_avg) &&
    !is.na(d$silhouette_avg)
  ) {
    sil_val <- round(d$silhouette_avg, 4)
    sil_interp <- interpret_silhouette(sil_val)
    items <- c(items, list(
      detail_row(
        "Avg. Silhouette Width",
        shiny$tagList(
          sprintf("%.4f", sil_val),
          shiny$tags$span(
            class = paste(
              "badge ms-1",
              sil_interp$badge_class
            ),
            sil_interp$label
          )
        ),
        paste(
          "How well each object fits its cluster.",
          "Range: -1 to 1. Higher is better."
        )
      )
    ))
  }

  # BSS / TSS
  if (
    !is.null(d$bss_tss) && !is.na(d$bss_tss)
  ) {
    items <- c(items, list(
      detail_row(
        "BSS / TSS",
        paste0(round(d$bss_tss * 100, 1), "%"),
        paste(
          "Between-cluster variance as percentage",
          "of total variance. Higher is better."
        )
      )
    ))
  }

  # Total Within-SS
  if (!is.null(d$tot_withinss)) {
    items <- c(items, list(
      detail_row(
        "Total Within-SS",
        sprintf("%.2f", d$tot_withinss),
        paste(
          "Sum of within-cluster sum of squares.",
          "Lower indicates tighter clusters."
        )
      )
    ))
  }

  # Cluster Sizes
  if (!is.null(d$size) && length(d$size) > 0) {
    items <- c(items, list(
      detail_row(
        "Cluster Sizes",
        paste(d$size, collapse = ", "),
        NULL
      )
    ))
  }

  render_detail_card(items)
}

render_pam_extras <- function(d) {
  items <- list()
  if (!is.null(d$objective)) {
    items <- c(items, list(
      detail_row(
        "Objective (build, swap)",
        paste(
          round(d$objective, 2), collapse = ", "
        ),
        "PAM optimization objective values."
      )
    ))
  }
  if (length(items) == 0) return(NULL)

  # Medoids table
  medoids_ui <- NULL
  if (!is.null(d$medoids)) {
    med_df <- as.data.frame(round(d$medoids, 3))
    col_names <- colnames(med_df)
    header <- shiny$tags$tr(
      shiny$tags$th("Medoid"),
      lapply(col_names, function(cn) {
        shiny$tags$th(class = "text-end", cn)
      })
    )
    body_rows <- lapply(
      seq_len(nrow(med_df)),
      function(i) {
        shiny$tags$tr(
          shiny$tags$td(
            cluster_badge_tag(i)
          ),
          lapply(col_names, function(cn) {
            shiny$tags$td(
              class = "text-end",
              sprintf("%.3f", med_df[i, cn])
            )
          })
        )
      }
    )
    medoids_ui <- shiny$tags$div(
      class = "mt-2",
      shiny$tags$small(
        class = "text-muted d-block mb-1",
        paste(
          "Medoids: representative observations",
          "closest to each cluster center."
        )
      ),
      shiny$tags$div(
        class = "table-responsive",
        shiny$tags$table(
          class = paste(
            "table table-sm table-striped",
            "table-hover mb-0"
          ),
          shiny$tags$thead(header),
          shiny$tags$tbody(body_rows)
        )
      )
    )
  }

  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("info-circle", class = "me-1"),
      "PAM Details"
    ),
    render_detail_card(items),
    medoids_ui
  )
}

render_hclust_extras <- function(d) {
  items <- list(
    detail_row(
      "Linkage Method",
      format_method_label(d$method),
      NULL
    ),
    detail_row(
      "Distance Metric",
      d$metric,
      NULL
    )
  )

  if (!is.null(d$merge)) {
    n_merges <- nrow(d$merge)
    items <- c(items, list(
      detail_row(
        "Merge Steps",
        n_merges,
        paste(
          "Number of agglomerative merge steps.",
          "Negative values = individual observations,",
          "positive = previously formed clusters."
        )
      )
    ))
  }

  if (!is.null(d$height) && length(d$height) > 0) {
    items <- c(items, list(
      detail_row(
        "Height Range",
        sprintf(
          "%.3f \u2013 %.3f",
          min(d$height), max(d$height)
        ),
        paste(
          "Distance at which clusters were merged.",
          "Large jumps suggest natural cluster",
          "boundaries."
        )
      )
    ))
  }

  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("info-circle", class = "me-1"),
      "Hierarchical Details"
    ),
    render_detail_card(items)
  )
}

render_dbscan_extras <- function(d) {
  items <- list()
  if (!is.null(d$eps)) {
    items <- c(items, list(
      detail_row(
        "eps (neighborhood radius)",
        sprintf("%.4f", d$eps),
        "Auto-computed from k-NN distance knee."
      )
    ))
  }
  if (!is.null(d$min_pts)) {
    items <- c(items, list(
      detail_row(
        "minPts",
        d$min_pts,
        paste(
          "Minimum points to form a dense region.",
          "Set to dimensions + 1."
        )
      )
    ))
  }
  if (!is.null(d$n_noise)) {
    items <- c(items, list(
      detail_row(
        "Noise Points",
        paste0(
          d$n_noise,
          " (", d$n_clusters_found,
          " clusters found)"
        ),
        "Points not assigned to any cluster."
      )
    ))
  }
  if (!is.null(d$border_points)) {
    items <- c(items, list(
      detail_row(
        "Border Points",
        if (d$border_points) "Enabled" else "Disabled",
        paste(
          "Whether border points are assigned",
          "to clusters or treated as noise."
        )
      )
    ))
  }
  if (!is.null(d$db_metric)) {
    items <- c(items, list(
      detail_row(
        "Distance Metric",
        d$db_metric,
        NULL
      )
    ))
  }
  if (length(items) == 0) return(NULL)

  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("info-circle", class = "me-1"),
      "DBSCAN Details"
    ),
    render_detail_card(items)
  )
}

detail_row <- function(label, value, description) {
  list(
    label = label,
    value = value,
    description = description
  )
}

render_detail_card <- function(items) {
  if (length(items) == 0) return(NULL)

  rows <- lapply(items, function(item) {
    desc_td <- if (!is.null(item$description)) {
      shiny$tags$td(
        class = "text-muted small",
        item$description
      )
    } else {
      shiny$tags$td()
    }

    shiny$tags$tr(
      shiny$tags$td(
        class = "fw-semibold",
        item$label
      ),
      shiny$tags$td(item$value),
      desc_td
    )
  })

  shiny$tags$div(
    class = "table-responsive",
    shiny$tags$table(
      class = "table table-sm table-hover mb-0",
      shiny$tags$tbody(rows)
    )
  )
}

render_cluster_profile <- function(cs) {
  if (is.null(cs)) return(NULL)

  means_df <- as.data.frame(round(cs$means, 3))
  col_names <- colnames(cs$means)
  cluster_ids <- cs$cluster_ids
  n_per <- cs$n_per_cluster

  header_cells <- c(
    list(
      shiny$tags$th("Cluster"),
      shiny$tags$th(class = "text-end", "n")
    ),
    lapply(col_names, function(cn) {
      shiny$tags$th(class = "text-end", cn)
    })
  )

  body_rows <- lapply(
    seq_along(cluster_ids),
    function(i) {
      cells <- c(
        list(
          shiny$tags$td(
            cluster_badge_tag(cluster_ids[i])
          ),
          shiny$tags$td(
            class = "text-end text-muted",
            n_per[i]
          )
        ),
        lapply(col_names, function(cn) {
          shiny$tags$td(
            class = "text-end",
            sprintf("%.3f", means_df[i, cn])
          )
        })
      )
      shiny$tags$tr(cells)
    }
  )

  # Overall mean row
  overall_cells <- c(
    list(
      shiny$tags$td(
        class = "fw-semibold text-muted",
        "Overall"
      ),
      shiny$tags$td(
        class = "text-end text-muted",
        sum(n_per)
      )
    ),
    lapply(col_names, function(cn) {
      shiny$tags$td(
        class = "text-end text-muted",
        sprintf("%.3f", round(cs$overall_mean[cn], 3))
      )
    })
  )
  overall_row <- shiny$tags$tr(
    class = "table-light",
    overall_cells
  )

  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("bullseye", class = "me-1"),
      "Cluster Profile (Variable Means)"
    ),
    shiny$tags$small(
      class = "text-muted d-block mb-2",
      paste(
        "Mean of each variable per cluster.",
        "Compare against the overall mean to",
        "characterize what distinguishes each cluster."
      )
    ),
    shiny$tags$div(
      class = "table-responsive",
      style = "max-height: 400px; overflow-y: auto;",
      shiny$tags$table(
        class = paste(
          "table table-sm table-striped",
          "table-hover mb-0"
        ),
        shiny$tags$thead(
          class = "sticky-top bg-white",
          shiny$tags$tr(header_cells)
        ),
        shiny$tags$tbody(
          body_rows,
          overall_row
        )
      )
    )
  )
}

render_membership_placeholder <- function(ns) {
  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("table", class = "me-1"),
      "Cluster Membership"
    ),
    shiny$tags$small(
      class = "text-muted d-block mb-2",
      paste(
        "Original data with cluster assignments.",
        "Download for further analysis or plotting."
      )
    ),
    DT$dataTableOutput(ns("membership_table"))
  )
}

#' Render membership DT datatable with colored cluster badges
#'
#' Call this from the parent server to populate the
#' membership_table DT output.
#'
#' @param md Data frame with a Cluster column
#' @return DT datatable object
#' @export
render_membership_dt <- function(md) {
  if (is.null(md)) return(NULL)

  display_df <- md

  # Round numeric columns for display
  num_cols <- vapply(
    display_df, is.numeric, logical(1)
  )
  # Don't round the Cluster column
  num_cols["Cluster"] <- FALSE
  for (cn in names(num_cols)[num_cols]) {
    display_df[[cn]] <- round(display_df[[cn]], 3)
  }

  # Replace Cluster values with colored badge HTML
  cluster_vals <- display_df$Cluster
  display_df$Cluster <- cluster_badge_html(
    cluster_vals
  )

  n_rows <- nrow(display_df)
  n_cols <- ncol(display_df)

  # Find index of numeric columns (0-based for DT)
  num_targets <- which(num_cols) - 1L

  DT$datatable(
    display_df,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = "tip",
      order = list(),
      columnDefs = list(
        list(
          className = "dt-right",
          targets = as.list(num_targets)
        ),
        list(
          className = "dt-center",
          targets = list(
            which(names(display_df) == "Cluster") - 1L
          )
        )
      )
    ),
    class = paste(
      "table table-sm table-striped",
      "table-hover compact"
    )
  )
}

render_download_section <- function(ns) {
  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("download", class = "me-1"),
      "Download Results"
    ),
    shiny$downloadButton(
      ns("cluster_dl_excel"),
      label = shiny$tags$span(
        bsicons$bs_icon(
          "file-earmark-excel", class = "me-1"
        ),
        "Download Excel"
      ),
      class = "btn btn-outline-primary btn-sm"
    ),
    shiny$tags$small(
      class = "text-muted mt-1 d-block",
      paste(
        "Excel file with two sheets:",
        "Membership (raw data + cluster assignments)",
        "and Cluster Profile (per-cluster variable means)."
      )
    )
  )
}

interpret_silhouette <- function(sil_val) {
  if (sil_val >= 0.71) {
    list(
      label = "Strong",
      badge_class = "bg-success"
    )
  } else if (sil_val >= 0.51) {
    list(
      label = "Reasonable",
      badge_class = "bg-success"
    )
  } else if (sil_val >= 0.26) {
    list(
      label = "Weak",
      badge_class = "bg-warning text-dark"
    )
  } else {
    list(
      label = "No structure",
      badge_class = "bg-danger"
    )
  }
}

cluster_color <- function(cluster_id) {
  palette <- c(
    "#0d6efd", "#198754", "#dc3545", "#fd7e14",
    "#6f42c1", "#20c997", "#d63384", "#0dcaf0",
    "#6610f2", "#ffc107"
  )
  idx <- ((as.integer(cluster_id) - 1L) %%
    length(palette)) + 1L
  palette[idx]
}

cluster_badge_html <- function(cluster_vals) {
  vapply(cluster_vals, function(v) {
    col <- cluster_color(v)
    sprintf(
      paste0(
        "<span class=\"badge\" style=\"",
        "background-color:%s;\">%s</span>"
      ),
      col, v
    )
  }, character(1))
}

cluster_badge_tag <- function(cluster_id) {
  col <- cluster_color(cluster_id)
  shiny$tags$span(
    class = "badge",
    style = paste0("background-color:", col, ";"),
    cluster_id
  )
}

format_method_label <- function(method) {
  switch(
    method,
    "ward.D2" = "Ward's D2",
    "ward.D"  = "Ward's D",
    "single"  = "Single Linkage",
    "complete" = "Complete Linkage",
    "average" = "Average (UPGMA)",
    "mcquitty" = "McQuitty (WPGMA)",
    "median"  = "Median (WPGMC)",
    "centroid" = "Centroid (UPGMC)",
    method
  )
}
