box::use(
  rhino,
  stats,
)

box::use(
  app/logic/column_utils,
  app/logic/error_handling,
)

# =============================================================================
# Median computation logic
# Calculates medians per group for measurement columns.
# Descriptive columns that vary within groups are removed.
# =============================================================================

#' Compute median results from filtered data
#' @param data Data frame (already quality-filtered)
#' @param grouping_cols Character vector of grouping column names
#' @param quality_col Character or NULL, quality column to exclude
#' @return List with $success, $result (data frame or NULL),
#'   $removed_cols (character vector), $message (character)
#' @export
compute_medians <- function(data, grouping_cols,
                            quality_col = NULL) {
  measurement_col_names <- column_utils$get_measurement_cols(
    data
  )
  descriptive_col_names <- column_utils$get_descriptive_cols(
    data
  )

  # Remove quality column from descriptive columns
  if (!is.null(quality_col) &&
      quality_col != "None" &&
      quality_col %in% descriptive_col_names) {
    descriptive_col_names <- setdiff(
      descriptive_col_names, quality_col
    )
  }

  # Filter to only numeric measurement columns
  measurement_col_names <- measurement_col_names[
    vapply(measurement_col_names, function(col) {
      is.numeric(data[[col]])
    }, logical(1))
  ]

  if (length(measurement_col_names) == 0) {
    rhino$log$warn("Median: no numeric measurement columns")
    return(list(
      success = TRUE,
      result = NULL,
      removed_cols = character(0),
      message = paste0(
        "No numeric measurement columns found ",
        "in the data."
      )
    ))
  }

  # NO GROUPING: return filtered data as-is
  if (is.null(grouping_cols) ||
      length(grouping_cols) == 0) {
    result <- data
    if (!is.null(quality_col) &&
        quality_col != "None" &&
        quality_col %in% names(result)) {
      result <- result[
        , setdiff(names(result), quality_col),
        drop = FALSE
      ]
    }
    rhino$log$info(
      "Median: no grouping, returning {nrow(result)} rows"
    )
    return(list(
      success = TRUE,
      result = result,
      removed_cols = character(0),
      message = paste0(
        "No grouping selected. Showing filtered data ",
        "without median calculation."
      )
    ))
  }

  # WITH GROUPING: calculate medians
  compute_result <- error_handling$safe_execute(
    expr = {
      other_descriptive <- setdiff(
        descriptive_col_names, grouping_cols
      )

      # Find columns that vary within groups
      cols_to_remove <- character(0)
      if (length(other_descriptive) > 0) {
        for (col in other_descriptive) {
          varies <- any(
            base::tapply(
              data[[col]],
              base::interaction(
                data[grouping_cols], drop = TRUE
              ),
              function(x) length(unique(x)) > 1
            )
          )
          if (varies) {
            cols_to_remove <- c(cols_to_remove, col)
          }
        }
      }

      constant_descriptive <- setdiff(
        other_descriptive, cols_to_remove
      )

      # Calculate medians grouped by selected columns
      median_data <- stats$aggregate(
        data[measurement_col_names],
        by = data[grouping_cols],
        FUN = function(x) stats$median(x, na.rm = TRUE)
      )

      # Merge constant descriptive columns back
      if (length(constant_descriptive) > 0) {
        constant_data <- unique(
          data[c(grouping_cols, constant_descriptive)]
        )
        results <- merge(
          median_data, constant_data,
          by = grouping_cols, all.x = TRUE
        )
        col_order <- c(
          grouping_cols, constant_descriptive,
          measurement_col_names
        )
        col_order <- col_order[col_order %in% names(results)]
        results <- results[, col_order, drop = FALSE]
      } else {
        results <- median_data
      }

      # Round numeric measurement columns
      for (col in measurement_col_names) {
        if (col %in% names(results)) {
          results[[col]] <- round(results[[col]], 4)
        }
      }

      list(results = results, removed = cols_to_remove)
    },
    operation_name = "Median Calculation",
    context = list(
      grouping = paste(grouping_cols, collapse = ", "),
      n_measurements = length(measurement_col_names)
    )
  )

  if (!compute_result$success) {
    return(list(
      success = FALSE,
      result = NULL,
      removed_cols = character(0),
      message = compute_result$error$message,
      error = compute_result$error
    ))
  }

  results <- compute_result$result$results
  removed <- compute_result$result$removed

  n_groups <- nrow(results)
  rhino$log$info(
    "Median: computed for {n_groups} groups, ",
    "{length(removed)} columns removed"
  )

  list(
    success = TRUE,
    result = results,
    removed_cols = removed,
    message = paste0(
      "Median calculated for ", n_groups, " groups."
    )
  )
}
