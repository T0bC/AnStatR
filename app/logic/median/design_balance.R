# =============================================================================
# Design balance and group-level missingness (pure logic, no Shiny)
#
# Both functions operate on the *pre-median* input data. The point is to
# show the user what their grouping selection means before the data is
# aggregated away: how many observations land in each combination, which
# combinations never occur, and where the measurement gaps sit.
# =============================================================================

box::use(
  app/logic/shared/column_utils,
  app/logic/shared/data_utils,
)

#' Build the nested group label for a set of grouping columns
#'
#' Columns are reversed before building the interaction so that the
#' first-selected column becomes the outermost bracket on a
#' legendry nested axis, matching the convention in
#' `app/logic/plotting/plot_helpers.R:178`.
#'
#' @param data A data frame
#' @param grouping_cols Character vector of column names
#' @return A factor of group labels, one per row
group_labels <- function(data, grouping_cols) {
  data_utils$create_interaction(data, base::rev(grouping_cols))
}

#' Empty balance result used for invalid or absent input
#' @param grouping_cols Character vector of column names
#' @return List in the shape returned by `compute_group_balance()`
empty_balance <- function(grouping_cols = character(0)) {
  list(
    data = data.frame(
      group_label = character(0),
      n = numeric(0),
      is_thin = logical(0),
      stringsAsFactors = FALSE
    ),
    grouping_cols = grouping_cols,
    n_combinations = 0,
    n_possible = 0,
    n_empty = 0,
    pct_coverage = 0,
    n_thin = 0,
    thin_threshold = 3
  )
}

#' Count observations per grouping-column combination
#'
#' Reports the observed combinations alongside how many of the possible
#' combinations never occur, which is the usual situation for material
#' that is incomplete by origin (missing teeth, unscannable facets,
#' unknown sex).
#'
#' @param data A data frame
#' @param grouping_cols Character vector of grouping column names
#' @param thin_threshold Groups with fewer observations than this are
#'   flagged as thin
#' @return List with `data` (group_label, n, is_thin) and summary counts
#' @export
compute_group_balance <- function(data, grouping_cols,
                                  thin_threshold = 3) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(empty_balance(grouping_cols))
  }
  if (is.null(grouping_cols) || length(grouping_cols) == 0) {
    return(empty_balance(character(0)))
  }

  grouping_cols <- grouping_cols[grouping_cols %in% names(data)]
  if (length(grouping_cols) == 0) {
    return(empty_balance(character(0)))
  }

  labels <- group_labels(data, grouping_cols)
  counts <- table(labels)

  result <- data.frame(
    group_label = names(counts),
    n = as.numeric(counts),
    stringsAsFactors = FALSE
  )
  result$is_thin <- result$n < thin_threshold
  # Preserve the hierarchical level order produced by interaction().
  result$group_label <- factor(
    result$group_label,
    levels = levels(labels)[levels(labels) %in% result$group_label]
  )
  result <- result[order(result$group_label), , drop = FALSE]
  rownames(result) <- NULL

  # Full crossing size, computed from level counts rather than
  # materialised, so a high-cardinality column cannot blow up memory.
  n_possible <- prod(vapply(
    grouping_cols,
    function(col) {
      values <- data[[col]]
      values[is.na(values)] <- "NA"
      as.numeric(length(unique(values)))
    },
    numeric(1)
  ))

  n_combinations <- nrow(result)

  list(
    data = result,
    grouping_cols = grouping_cols,
    n_combinations = n_combinations,
    n_possible = n_possible,
    n_empty = max(0, n_possible - n_combinations),
    pct_coverage = if (n_possible == 0) {
      0
    } else {
      round(100 * n_combinations / n_possible, 1)
    },
    n_thin = sum(result$is_thin),
    thin_threshold = thin_threshold
  )
}

#' Empty missingness result used for invalid or absent input
#' @param grouping_cols Character vector of column names
#' @return List in the shape returned by `compute_group_missingness()`
empty_missingness <- function(grouping_cols = character(0)) {
  list(
    data = data.frame(
      group_label = character(0),
      column = character(0),
      pct_missing = numeric(0),
      n_missing = numeric(0),
      n_total = numeric(0),
      stringsAsFactors = FALSE
    ),
    grouping_cols = grouping_cols,
    n_groups = 0,
    n_cols = 0
  )
}

#' Percentage of missing measurement values per group
#'
#' Only measurement columns that contain at least one NA are included,
#' for the same reason the Load tab filters its bar chart: an all-complete
#' column contributes nothing but noise to a 48-column heatmap.
#'
#' @param data A data frame
#' @param grouping_cols Character vector of grouping column names
#' @param measurement_cols Optional character vector; defaults to the
#'   app-wide measurement column convention
#' @return List with `data` (group_label, column, pct_missing, n_missing,
#'   n_total) and summary counts
#' @export
compute_group_missingness <- function(data, grouping_cols,
                                      measurement_cols = NULL) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(empty_missingness(grouping_cols))
  }
  if (is.null(grouping_cols) || length(grouping_cols) == 0) {
    return(empty_missingness(character(0)))
  }

  grouping_cols <- grouping_cols[grouping_cols %in% names(data)]
  if (length(grouping_cols) == 0) {
    return(empty_missingness(character(0)))
  }

  if (is.null(measurement_cols)) {
    measurement_cols <- column_utils$get_measurement_cols(data)
  }
  measurement_cols <- measurement_cols[measurement_cols %in% names(data)]
  affected <- measurement_cols[
    vapply(measurement_cols, function(col) anyNA(data[[col]]), logical(1))
  ]

  if (length(affected) == 0) {
    return(empty_missingness(grouping_cols))
  }

  labels <- group_labels(data, grouping_cols)
  present_levels <- levels(labels)[levels(labels) %in% as.character(labels)]

  per_col <- lapply(affected, function(col) {
    n_missing <- tapply(is.na(data[[col]]), labels, sum)
    n_total <- tapply(is.na(data[[col]]), labels, length)
    n_missing <- n_missing[present_levels]
    n_total <- n_total[present_levels]
    data.frame(
      group_label = present_levels,
      column = col,
      n_missing = as.numeric(n_missing),
      n_total = as.numeric(n_total),
      pct_missing = round(100 * as.numeric(n_missing) /
                            as.numeric(n_total), 1),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, per_col)
  result$group_label <- factor(
    result$group_label,
    levels = present_levels
  )
  result$column <- factor(result$column, levels = affected)
  rownames(result) <- NULL

  list(
    data = result,
    grouping_cols = grouping_cols,
    n_groups = length(present_levels),
    n_cols = length(affected)
  )
}
