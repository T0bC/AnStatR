box::use(
  rhino,
  stats,
)

# =============================================================================
# Pure logic functions for NA handling
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Analyse NA distribution in measurement columns
#'
#' Returns per-column NA counts and percentages for the selected
#' measurement columns. This information is shown to the user
#' before row removal so they can decide whether to deselect
#' columns with many NAs.
#'
#' @param data Data frame
#' @param measurement_cols Character vector of measurement column names
#' @return Data frame with columns: column, na_count, na_percent,
#'   sorted by na_count descending. Only includes columns that
#'   have at least one NA.
#' @export
analyse_na <- function(data, measurement_cols) {
  subset <- data[, measurement_cols, drop = FALSE]
  n_rows <- nrow(subset)

  na_info <- vapply(subset, function(x) sum(is.na(x)), integer(1))
  cols_with_na <- na_info[na_info > 0]

  if (length(cols_with_na) == 0) {
    return(data.frame(
      column = character(0),
      na_count = integer(0),
      na_percent = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- data.frame(
    column = names(cols_with_na),
    na_count = as.integer(cols_with_na),
    na_percent = round(cols_with_na / n_rows * 100, 1),
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$na_count), ]
  rownames(result) <- NULL
  result
}

#' Remove rows with NAs in measurement columns and grouping column
#'
#' Removes rows where any of the selected measurement columns
#' or the grouping column contain NA. Metadata columns are ignored
#' for row removal but their NA distribution is reported for user awareness.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param meta_cols Character vector of descriptive column names
#'   (optional). NAs in these columns are reported but do not
#'   trigger row removal.
#' @param grouping_col Character, name of the grouping column (optional).
#'   If provided, rows with NA in this column are also removed.
#' @return List with:
#'   - $data: cleaned data frame (all columns preserved)
#'   - $rows_before: integer, original row count
#'   - $rows_after: integer, row count after removal
#'   - $rows_removed: integer, number of rows removed
#'   - $na_summary: data frame from analyse_na (measurement cols)
#'   - $meta_na_summary: data frame from analyse_na (descriptive cols)
#' @export
clean_na_rows <- function(data, measurement_cols,
                          meta_cols = character(0),
                          grouping_col = NULL) {
  rows_before <- nrow(data)
  na_summary <- analyse_na(data, measurement_cols)

  # Analyse descriptive columns (informational only)
  meta_na_summary <- if (length(meta_cols) > 0) {
    analyse_na(data, meta_cols)
  } else {
    data.frame(
      column = character(0),
      na_count = integer(0),
      na_percent = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # Include grouping column in completeness check if provided
  cols_for_complete <- measurement_cols
  if (!is.null(grouping_col) && grouping_col %in% names(data)) {
    cols_for_complete <- c(cols_for_complete, grouping_col)
  }

  subset <- data[, cols_for_complete, drop = FALSE]
  complete <- stats$complete.cases(subset)
  cleaned <- data[complete, , drop = FALSE]

  rows_after <- nrow(cleaned)
  rows_removed <- rows_before - rows_after

  if (rows_removed > 0) {
    rhino$log$info(
      "NA handling: removed {rows_removed} of",
      " {rows_before} rows",
      " ({round(rows_removed / rows_before * 100, 1)}%)"
    )
    if (nrow(na_summary) > 0) {
      col_info <- paste(
        na_summary$column,
        paste0("(", na_summary$na_count, " NAs)"),
        collapse = ", "
      )
      rhino$log$info(
        "NA handling: measurement NAs: {col_info}"
      )
    }
  } else {
    rhino$log$info("NA handling: no NAs found, 0 rows removed")
  }

  if (nrow(meta_na_summary) > 0) {
    meta_info <- paste(
      meta_na_summary$column,
      paste0("(", meta_na_summary$na_count, " NAs)"),
      collapse = ", "
    )
    rhino$log$info(
      "NA handling: descriptive column NAs: {meta_info}"
    )
  }

  list(
    data = cleaned,
    rows_before = rows_before,
    rows_after = rows_after,
    rows_removed = rows_removed,
    na_summary = na_summary,
    meta_na_summary = meta_na_summary
  )
}
