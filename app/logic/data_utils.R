#' Create interaction term from multiple columns
#'
#' Combines multiple factor columns into a single interaction term.
#' Useful for grouping data by multiple categorical variables.
#'
#' @param df Data frame containing the columns
#' @param cols Character vector of column names to combine
#' @return Factor vector representing the interaction of all specified columns
#' @export
create_interaction <- function(df, cols) {
  if (length(cols) == 0) {
    stop("At least one column must be provided.")
  }

  # Replace NA with "NA" string to preserve rows in plots
  factor_cols <- lapply(cols, function(col) {
    values <- df[[col]]
    values[is.na(values)] <- "NA"
    as.factor(values)
  })

  if (length(cols) == 1) return(factor_cols[[1]])

  interaction(factor_cols, drop = TRUE)
}

#' Get unique choices for a filter column, with NA shown as "NA"
#'
#' @param values Vector of column values
#' @return Character vector of unique values (NA replaced by "NA" string)
#' @export
get_filter_choices <- function(values) {
  choices <- unique(values)
  has_na <- any(is.na(choices))
  choices <- choices[!is.na(choices)]
  if (has_na) choices <- c(choices, "NA")
  as.character(choices)
}

#' Filter a data frame by selected values per column
#'
#' For each column in `filters`, keeps only rows whose value is in the
#' selected set. The special value "NA" in a selection matches actual
#' NA values in the data.
#'
#' @param data Data frame to filter
#' @param filters Named list where names are column names and values
#'   are character vectors of selected values (may include "NA")
#' @return Filtered data frame
#' @export
filter_data <- function(data, filters) {
  if (length(filters) == 0) return(data)

  for (col in names(filters)) {
    selected_values <- filters[[col]]
    if (is.null(selected_values) || length(selected_values) == 0) {
      next
    }

    col_values <- data[[col]]
    include_na <- "NA" %in% selected_values
    selected_values <- selected_values[selected_values != "NA"]

    matches <- col_values %in% selected_values
    matches[is.na(matches)] <- FALSE
    if (include_na) {
      matches[is.na(col_values)] <- TRUE
    }
    data <- data[matches, , drop = FALSE]
  }

  data
}

#' Generate a default color palette for n groups
#'
#' Returns a character vector of hex colors. Uses scales::hue_pal()
#' for <= 8 groups, otherwise interpolates a fixed 8-color ramp.
#'
#' @param n Integer, number of colors needed
#' @return Character vector of hex color strings
#' @export
default_palette <- function(n) {
  if (n <= 0) return(character(0))
  if (n <= 8) {
    scales::hue_pal()(n)
  } else {
    grDevices::colorRampPalette(
      c(
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
        "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"
      )
    )(n)
  }
}
