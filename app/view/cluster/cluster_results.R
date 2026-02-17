box::use(
  bsicons,
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
                                   membership_df = NULL) {
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
    render_cluster_profile(cluster_result),
    render_membership_preview(membership_df),
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

  n_obs <- if (is.data.frame(res$data)) {
    nrow(res$data)
  } else {
    length(res$clusters)
  }

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
      paste("Cluster", cid)
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

  shiny$tags$div(
    class = "mt-3",
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("info-circle", class = "me-1"),
      "PAM Details"
    ),
    render_detail_card(items)
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

render_cluster_profile <- function(res) {
  cs <- res$cluster_summary
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
            shiny$tags$span(
              class = "badge bg-primary",
              cluster_ids[i]
            )
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

render_membership_preview <- function(md) {
  if (is.null(md)) return(NULL)

  n_show <- min(nrow(md), 10)
  preview <- md[seq_len(n_show), , drop = FALSE]

  col_names <- names(preview)
  header_cells <- lapply(col_names, function(cn) {
    cls <- if (cn == "Cluster") {
      "text-center"
    } else if (is.numeric(preview[[cn]])) {
      "text-end"
    } else {
      ""
    }
    shiny$tags$th(class = cls, cn)
  })

  body_rows <- lapply(
    seq_len(n_show),
    function(i) {
      cells <- lapply(col_names, function(cn) {
        val <- preview[i, cn]
        if (cn == "Cluster") {
          shiny$tags$td(
            class = "text-center",
            shiny$tags$span(
              class = "badge bg-primary",
              val
            )
          )
        } else if (is.numeric(val)) {
          shiny$tags$td(
            class = "text-end",
            sprintf("%.3f", val)
          )
        } else {
          shiny$tags$td(as.character(val))
        }
      })
      shiny$tags$tr(cells)
    }
  )

  more_note <- if (nrow(md) > n_show) {
    shiny$tags$small(
      class = "text-muted mt-1 d-block",
      sprintf(
        "Showing %d of %d rows. Download Excel for full data.",
        n_show, nrow(md)
      )
    )
  }

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
    shiny$tags$div(
      class = "table-responsive",
      style = "max-height: 300px; overflow-y: auto;",
      shiny$tags$table(
        class = paste(
          "table table-sm table-striped",
          "table-hover mb-0"
        ),
        shiny$tags$thead(
          class = "sticky-top bg-white",
          shiny$tags$tr(header_cells)
        ),
        shiny$tags$tbody(body_rows)
      )
    ),
    more_note
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
