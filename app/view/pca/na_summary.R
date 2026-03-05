box::use(
  bsicons,
  bslib,
  shiny,
)

#' Render combined preprocessing summary banner
#'
#' Shows a single informational alert summarising both
#' NA handling (rows removed, columns with NAs) and
#' skewness correction (transformed columns, methods used).
#' Returns NULL if there is nothing to report.
#'
#' @param na_result List from clean_na_rows() with $rows_removed,
#'   $rows_before, $rows_after, $na_summary, $meta_na_summary.
#'   May be NULL.
#' @param transform_result List from transform_skewed()$result
#'   with $transformed_cols and $skipped_cols. May be NULL.
#' @param n_measure_cols Integer, total number of measurement
#'   columns (used for transform header). Only needed when
#'   transform_result is non-NULL.
#' @return Shiny tags object or NULL
#' @export
render_na_summary <- function(na_result,
                              transform_result = NULL,
                              n_measure_cols = NULL) {
  # --- NA flags ---
  has_meas_na <- !is.null(na_result) &&
    nrow(na_result$na_summary) > 0
  has_meta_na <- !is.null(na_result) &&
    !is.null(na_result$meta_na_summary) &&
    nrow(na_result$meta_na_summary) > 0
  has_na <- has_meas_na || has_meta_na

  # --- Transform flags ---
  has_transformed <- !is.null(transform_result) &&
    !is.null(transform_result$transformed_cols) &&
    nrow(transform_result$transformed_cols) > 0
  has_skipped <- !is.null(transform_result) &&
    length(transform_result$skipped_cols) > 0
  has_transform <- has_transformed || has_skipped

  if (!has_na && !has_transform) return(NULL)

  # ==========================================================================
  # NA section
  # ==========================================================================
  na_header <- NULL
  meas_section <- NULL
  meta_section <- NULL

  if (!is.null(na_result)) {
    na_header <- if (na_result$rows_removed > 0) {
      pct <- round(
        na_result$rows_removed /
          na_result$rows_before * 100,
        1
      )
      shiny$tags$div(
        class = "d-flex align-items-center mb-1",
        bsicons$bs_icon(
          "info-circle-fill", class = "me-2"
        ),
        shiny$tags$strong(
          paste0(
            na_result$rows_removed, " of ",
            na_result$rows_before,
            " rows removed (", pct, "%)"
          )
        ),
        shiny$tags$span(
          class = "text-muted ms-2",
          paste0(
            na_result$rows_after,
            " rows remaining for analysis"
          )
        )
      )
    } else {
      shiny$tags$div(
        class = "d-flex align-items-center mb-1",
        bsicons$bs_icon(
          "info-circle-fill", class = "me-2"
        ),
        shiny$tags$strong("No rows removed"),
        shiny$tags$span(
          class = "text-muted ms-2",
          paste0(
            na_result$rows_before,
            " rows used for analysis"
          )
        )
      )
    }

    meas_section <- if (has_meas_na) {
      shiny$tags$details(
        class = "mt-1",
        shiny$tags$summary(
          class = "small",
          paste0(
            "Measurement columns with NAs (",
            nrow(na_result$na_summary), ")"
          )
        ),
        na_table(na_result$na_summary),
        shiny$tags$p(
          class = "text-muted small mt-2 mb-0",
          paste(
            "Rows with NAs in measurement columns",
            "are removed before analysis.",
            "Columns with high NA percentages reduce",
            "your dataset significantly.",
            "Consider deselecting them."
          )
        )
      )
    }

    meta_section <- if (has_meta_na) {
      shiny$tags$details(
        class = "mt-1",
        shiny$tags$summary(
          class = "small",
          paste0(
            "Descriptive columns with NAs (",
            nrow(na_result$meta_na_summary), ")"
          )
        ),
        na_table(na_result$meta_na_summary),
        shiny$tags$p(
          class = "text-muted small mt-2 mb-0",
          paste(
            "NAs in descriptive columns do not cause",
            "row removal but may affect grouping",
            "or labelling in plots."
          )
        )
      )
    }
  }

  # ==========================================================================
  # Skewness transform section
  # ==========================================================================
  transform_header <- NULL
  transformed_section <- NULL
  skipped_section <- NULL

  if (has_transform) {
    n_transformed <- if (has_transformed) {
      nrow(transform_result$transformed_cols)
    } else {
      0L
    }
    n_total <- n_measure_cols %||% n_transformed

    transform_header <- shiny$tags$div(
      class = "d-flex align-items-center mb-1",
      bsicons$bs_icon(
        "arrow-left-right", class = "me-2"
      ),
      shiny$tags$strong(
        paste0(
          n_transformed, " of ", n_total,
          " measurement column",
          if (n_total != 1) "s" else "",
          " transformed (skewness correction)"
        )
      )
    )

    transformed_section <- if (has_transformed) {
      shiny$tags$details(
        class = "mt-1",
        shiny$tags$summary(
          class = "small",
          paste0(
            "Transformed columns (",
            n_transformed, ")"
          )
        ),
        transform_table(
          transform_result$transformed_cols
        ),
        shiny$tags$p(
          class = "text-muted small mt-2 mb-0",
          shiny$tags$em(
            "Please verify the transformations",
            " in the ",
            shiny$tags$strong("Load Data"),
            " \u2192 ",
            shiny$tags$strong("Data Preview"),
            " panel to ensure the skewness was",
            " detected correctly."
          )
        )
      )
    }

    skipped_section <- if (has_skipped) {
      shiny$tags$details(
        class = "mt-1",
        shiny$tags$summary(
          class = "small",
          paste0(
            "Skipped columns (",
            length(transform_result$skipped_cols),
            ")"
          )
        ),
        shiny$tags$p(
          class = "text-muted small mt-1 mb-0",
          paste(
            "The following columns were skewed but",
            "could not be transformed:",
            paste(
              transform_result$skipped_cols,
              collapse = ", "
            )
          )
        )
      )
    }
  }

  # ==========================================================================
  # Divider between sections (only if both present)
  # ==========================================================================
  divider <- if (has_na && has_transform) {
    shiny$tags$hr(class = "my-2")
  }

  shiny$tags$div(
    class = "alert alert-info",
    role = "alert",
    na_header,
    meas_section,
    meta_section,
    divider,
    transform_header,
    transformed_section,
    skipped_section
  )
}

#' Render skewness warning banner (when correction is disabled)
#'
#' Shows a warning alert when highly skewed columns are detected
#' but skewness correction is disabled. Informs the user which
#' columns are affected and suggests enabling normalization.
#'
#' @param skew_result Data frame from detect_skewness() with
#'   $column, $skewness, $direction, $is_skewed columns.
#' @param n_measure_cols Integer, total number of measurement columns.
#' @return Shiny tags object or NULL if no skewed columns
#' @export
render_skewness_warning <- function(skew_result,
                                    n_measure_cols = NULL) {
  if (is.null(skew_result)) return(NULL)

  skewed <- skew_result[skew_result$is_skewed, , drop = FALSE]
  if (nrow(skewed) == 0) return(NULL)

  n_skewed <- nrow(skewed)
  n_total <- n_measure_cols %||% nrow(skew_result)

  header <- shiny$tags$div(
    class = "d-flex align-items-center mb-1",
    bsicons$bs_icon(
      "exclamation-triangle-fill", class = "me-2"
    ),
    shiny$tags$strong(
      paste0(
        n_skewed, " of ", n_total,
        " measurement column",
        if (n_total != 1) "s" else "",
        " highly skewed (|skewness| > 2)"
      )
    )
  )

  skewed_table <- shiny$tags$details(
    class = "mt-1",
    shiny$tags$summary(
      class = "small",
      paste0("Skewed columns (", n_skewed, ")")
    ),
    skewness_info_table(skewed),
    shiny$tags$p(
      class = "text-muted small mt-2 mb-0",
      shiny$tags$em(
        "Consider enabling ",
        shiny$tags$strong("Normalize skewed variables"),
        " if these outliers are measurement errors.",
        " If skewness reflects real signal, leave disabled."
      )
    )
  )

  shiny$tags$div(
    class = "alert alert-warning",
    role = "alert",
    header,
    skewed_table
  )
}

#' Internal: render table of skewed columns (info only)
skewness_info_table <- function(skewed_df) {
  col_rows <- lapply(
    seq_len(nrow(skewed_df)),
    function(i) {
      row <- skewed_df[i, ]
      dir_badge <- if (row$direction == "right") {
        shiny$tags$span(
          class = "badge bg-warning text-dark",
          "right-skewed"
        )
      } else {
        shiny$tags$span(
          class = "badge bg-info",
          "left-skewed"
        )
      }
      shiny$tags$tr(
        shiny$tags$td(shiny$tags$code(row$column)),
        shiny$tags$td(dir_badge),
        shiny$tags$td(
          class = "text-end",
          as.character(row$skewness)
        )
      )
    }
  )

  shiny$tags$table(
    class = "table table-sm table-striped mb-0 mt-1",
    shiny$tags$thead(
      shiny$tags$tr(
        shiny$tags$th("Column"),
        shiny$tags$th("Direction"),
        shiny$tags$th(class = "text-end", "Skewness")
      )
    ),
    shiny$tags$tbody(col_rows)
  )
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

na_table <- function(na_df) {
  col_rows <- lapply(seq_len(nrow(na_df)), function(i) {
    row <- na_df[i, ]
    badge_class <- if (row$na_percent >= 30) {
      "bg-danger"
    } else if (row$na_percent >= 10) {
      "bg-warning text-dark"
    } else {
      "bg-secondary"
    }
    shiny$tags$tr(
      shiny$tags$td(shiny$tags$code(row$column)),
      shiny$tags$td(
        class = "text-end",
        as.character(row$na_count)
      ),
      shiny$tags$td(
        class = "text-end",
        shiny$tags$span(
          class = paste("badge", badge_class),
          paste0(row$na_percent, "%")
        )
      )
    )
  })

  shiny$tags$table(
    class = "table table-sm table-striped mb-0 mt-1",
    shiny$tags$thead(
      shiny$tags$tr(
        shiny$tags$th("Column"),
        shiny$tags$th(class = "text-end", "NA count"),
        shiny$tags$th(class = "text-end", "NA %")
      )
    ),
    shiny$tags$tbody(col_rows)
  )
}

transform_table <- function(transformed_df) {
  col_rows <- lapply(
    seq_len(nrow(transformed_df)),
    function(i) {
      row <- transformed_df[i, ]
      dir_badge <- if (row$direction == "right") {
        shiny$tags$span(
          class = "badge bg-warning text-dark",
          "right-skewed"
        )
      } else {
        shiny$tags$span(
          class = "badge bg-info",
          "left-skewed"
        )
      }
      shiny$tags$tr(
        shiny$tags$td(shiny$tags$code(row$column)),
        shiny$tags$td(dir_badge),
        shiny$tags$td(
          class = "text-end",
          as.character(row$skewness_before)
        ),
        shiny$tags$td(
          class = "text-end",
          as.character(row$skewness_after)
        ),
        shiny$tags$td(
          shiny$tags$small(
            class = "text-muted",
            row$method_used
          )
        )
      )
    }
  )

  shiny$tags$table(
    class = "table table-sm table-striped mb-0 mt-1",
    shiny$tags$thead(
      shiny$tags$tr(
        shiny$tags$th("Column"),
        shiny$tags$th("Direction"),
        shiny$tags$th(class = "text-end", "Before"),
        shiny$tags$th(class = "text-end", "After"),
        shiny$tags$th("Method")
      )
    ),
    shiny$tags$tbody(col_rows)
  )
}
