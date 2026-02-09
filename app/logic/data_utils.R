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
