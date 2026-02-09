box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Quality filter logic for median calculation
# Applies quality filtering with awareness of grouping structure.
# When grouping is defined, bad values are only removed if the group
# contains at least one good value. Groups with only bad values are
# kept intact.
# =============================================================================

#' Apply quality filter to data
#' @param data Data frame to filter
#' @param quality_settings List with filter settings:
#'   enabled (logical), column (character or NULL),
#'   type ("none", "categorical", "percentage_decimal",
#'         "percentage_100", "numeric"),
#'   bad_values (character vector, for categorical),
#'   threshold (numeric, for numeric types)
#' @param grouping_cols Character vector of grouping column names
#' @return List with $data (filtered data frame) and $message
#' @export
apply_quality_filter <- function(data, quality_settings,
                                 grouping_cols) {
  if (!quality_settings$enabled ||
      is.null(quality_settings$column)) {
    return(list(
      data = data,
      message = "No quality filtering applied."
    ))
  }

  col <- quality_settings$column
  if (!col %in% names(data)) {
    rhino$log$warn(
      "Median quality filter: column '{col}' not found"
    )
    return(list(
      data = data,
      message = paste0(
        "Quality column '", col, "' not found in data."
      )
    ))
  }

  if (quality_settings$type == "categorical") {
    bad_values <- quality_settings$bad_values
    if (is.null(bad_values) || length(bad_values) == 0) {
      return(list(
        data = data,
        message = paste0(
          "Quality column selected but no bad values ",
          "specified."
        )
      ))
    }
    filter_categorical(data, col, bad_values, grouping_cols)
  } else {
    threshold <- quality_settings$threshold
    if (is.null(threshold)) {
      return(list(
        data = data,
        message = paste0(
          "Quality column selected but no threshold ",
          "specified."
        )
      ))
    }
    filter_numeric(data, col, threshold, grouping_cols)
  }
}

#' Filter by categorical bad values
#' @param data Data frame
#' @param col Character, quality column name
#' @param bad_values Character vector of bad values
#' @param grouping_cols Character vector of grouping columns
#' @return List with $data and $message
#' @export
filter_categorical <- function(data, col, bad_values,
                               grouping_cols) {
  rows_before <- nrow(data)

  if (!is.null(grouping_cols) && length(grouping_cols) > 0) {
    data$.is_bad <- data[[col]] %in% bad_values

    group_has_good <- stats$aggregate(
      data$.is_bad,
      by = data[grouping_cols],
      FUN = function(x) !all(x)
    )
    names(group_has_good)[ncol(group_has_good)] <-
      ".group_has_good"

    data <- merge(
      data, group_has_good,
      by = grouping_cols, all.x = TRUE, sort = FALSE
    )

    filtered <- data[
      !data$.group_has_good | !data$.is_bad,
    ]

    n_groups_total <- nrow(unique(data[grouping_cols]))
    n_groups_all_bad <- sum(!group_has_good$.group_has_good)
    n_groups_filtered <- n_groups_total - n_groups_all_bad

    filtered$.is_bad <- NULL
    filtered$.group_has_good <- NULL
    rows_after <- nrow(filtered)

    message <- paste0(
      "Categorical quality filter applied.\n",
      "Groups: ", n_groups_total, " total - ",
      n_groups_all_bad,
      " kept intact (only bad values), ",
      n_groups_filtered,
      " cleaned (bad rows removed).\n",
      "Rows: ", rows_before, " -> ", rows_after,
      " (", rows_before - rows_after, " bad rows removed)"
    )
  } else {
    filtered <- data[!data[[col]] %in% bad_values, ]
    rows_after <- nrow(filtered)

    message <- paste0(
      "Categorical quality filter applied (no grouping).\n",
      "Rows: ", rows_before, " -> ", rows_after,
      " (", rows_before - rows_after,
      " bad values removed)"
    )
  }

  rhino$log$info(
    "Median quality filter: {rows_before} -> {rows_after} rows"
  )
  list(data = filtered, message = message)
}

#' Filter by numeric threshold
#' @param data Data frame
#' @param col Character, quality column name
#' @param threshold Numeric, minimum threshold
#' @param grouping_cols Character vector of grouping columns
#' @return List with $data and $message
#' @export
filter_numeric <- function(data, col, threshold,
                           grouping_cols) {
  rows_before <- nrow(data)

  if (!is.null(grouping_cols) && length(grouping_cols) > 0) {
    data$.is_bad <- data[[col]] < threshold |
      is.na(data[[col]])

    group_has_good <- stats$aggregate(
      data$.is_bad,
      by = data[grouping_cols],
      FUN = function(x) !all(x)
    )
    names(group_has_good)[ncol(group_has_good)] <-
      ".group_has_good"

    data <- merge(
      data, group_has_good,
      by = grouping_cols, all.x = TRUE, sort = FALSE
    )

    filtered <- data[
      !data$.group_has_good | !data$.is_bad,
    ]

    n_groups_total <- nrow(unique(data[grouping_cols]))
    n_groups_all_bad <- sum(!group_has_good$.group_has_good)
    n_groups_filtered <- n_groups_total - n_groups_all_bad

    filtered$.is_bad <- NULL
    filtered$.group_has_good <- NULL
    rows_after <- nrow(filtered)

    message <- paste0(
      "Numeric quality filter applied ",
      "(threshold >= ", threshold, ").\n",
      "Groups: ", n_groups_total, " total - ",
      n_groups_all_bad,
      " kept intact (all below threshold), ",
      n_groups_filtered,
      " cleaned (below-threshold rows removed).\n",
      "Rows: ", rows_before, " -> ", rows_after,
      " (", rows_before - rows_after, " rows removed)"
    )
  } else {
    filtered <- data[
      data[[col]] >= threshold & !is.na(data[[col]]),
    ]
    rows_after <- nrow(filtered)

    message <- paste0(
      "Numeric quality filter applied ",
      "(threshold >= ", threshold, ", no grouping).\n",
      "Rows: ", rows_before, " -> ", rows_after,
      " (", rows_before - rows_after,
      " below threshold removed)"
    )
  }

  rhino$log$info(
    "Median quality filter: {rows_before} -> {rows_after} rows"
  )
  list(data = filtered, message = message)
}
