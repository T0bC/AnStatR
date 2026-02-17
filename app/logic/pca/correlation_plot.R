box::use(
  ggplot2,
  ggiraph,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for correlation plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute correlation data for plotting
#'
#' Validates inputs and computes the correlation matrix with
#' hierarchical clustering. Returns a safe_execute result with
#' cor_long (long-format data frame) and ordered_cols.
#'
#' @param data Data frame containing the measurement columns
#' @param measurement_cols Character vector of column names
#' @return List with $success, $result or $error
#' @export
compute_correlation_data <- function(data, measurement_cols) {
  error_handling$safe_execute(
    expr = {
      validate_correlation_inputs(data, measurement_cols)
      cor_data <- data[, measurement_cols, drop = FALSE]
      cor_matrix <- compute_cor_matrix(cor_data)
      ordered_cols <- cluster_columns(cor_matrix, measurement_cols)
      cor_long <- cor_matrix_to_long(cor_matrix, ordered_cols)

      rhino$log$info(
        "Correlation plot: computed ({length(measurement_cols)} cols)"
      )

      list(
        cor_long = cor_long,
        ordered_cols = ordered_cols,
        n_cols = length(measurement_cols)
      )
    },
    operation_name = "Correlation Plot",
    context = list(
      n_columns = length(measurement_cols),
      columns = paste(measurement_cols, collapse = ", ")
    ),
    error_parser = correlation_error_parser
  )
}

#' Render correlation plot as ggiraph
#'
#' Takes pre-computed correlation data and renders it as an
#' interactive ggiraph heatmap. Should not fail if
#' compute_correlation_data() succeeded. Text sizes, axis
#' label sizes, and SVG dimensions adapt to the number of
#' variables.
#'
#' @param cor_data List with $cor_long, $ordered_cols, $n_cols
#' @return ggiraph interactive plot object
#' @export
render_correlation_girafe <- function(cor_data) {
  cor_long <- cor_data$cor_long
  n_cols <- cor_data$n_cols

  # Adaptive text size for cell labels
  # Hide labels when grid is extremely dense
  show_labels <- n_cols <= 35
  cell_text_size <- if (n_cols <= 6) {
    5
  } else if (n_cols <= 10) {
    4.5
  } else if (n_cols <= 15) {
    4
  } else if (n_cols <= 20) {
    3.8
  } else if (n_cols <= 30) {
    3.5
  } else {
    3
  }

  # Adaptive axis label size
  axis_text_size <- if (n_cols <= 10) {
    13
  } else if (n_cols <= 15) {
    12
  } else if (n_cols <= 20) {
    11
  } else if (n_cols <= 30) {
    10
  } else {
    9
  }

  p <- ggplot2$ggplot(
    cor_long,
    ggplot2$aes(x = Var1, y = Var2, fill = correlation)
  ) +
    ggiraph$geom_tile_interactive(
      ggplot2$aes(tooltip = tooltip, data_id = data_id),
      color = "white",
      linewidth = 0.5
    ) +
    ggplot2$scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    { if (show_labels) {
      ggplot2$geom_text(
        ggplot2$aes(label = sprintf("%.2f", correlation)),
        color = ifelse(
          abs(cor_long$correlation) > 0.5, "white", "black"
        ),
        size = cell_text_size
      )
    } } +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(
        angle = 45, hjust = 1, vjust = 1,
        size = axis_text_size
      ),
      axis.text.y = ggplot2$element_text(
        size = axis_text_size
      ),
      axis.title = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank(),
      legend.position = "right",
      legend.title = ggplot2$element_text(size = 12),
      legend.text = ggplot2$element_text(size = 10)
    )

  # Adaptive SVG dimensions — smaller SVG = larger text
  # after browser scales it to fit the panel
  width_svg <- min(max(n_cols * 0.5 + 2, 6), 10)
  height_svg <- min(max(n_cols * 0.4 + 2, 5), 8)

  ggiraph$girafe(
    ggobj = p,
    width_svg = width_svg,
    height_svg = height_svg,
    options = list(
      ggiraph$opts_hover(
        css = paste0(
          "fill-opacity:0.8;",
          "stroke:black;stroke-width:2px;"
        )
      ),
      ggiraph$opts_tooltip(
        css = paste0(
          "background-color:white;padding:8px;",
          "border-radius:4px;border:1px solid #ccc;",
          "font-family:sans-serif;"
        ),
        use_fill = FALSE
      ),
      ggiraph$opts_selection(type = "none")
    )
  )
}

#' Error parser for correlation plot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
correlation_error_parser <- function(error_msg,
                                     operation_name = "Correlation Plot") {
  if (grepl(
    "constant|singular|invertible",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Cannot compute correlations -",
      " data may contain constant columns."
    )
  } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains too many missing values.",
      " Please handle missing data first."
    )
  } else if (grepl(
    "columns|measurement|at least 2",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": At least 2 measurement columns are required."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

validate_correlation_inputs <- function(data, measurement_cols) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is NULL or empty")
  }

  if (is.null(measurement_cols) || length(measurement_cols) < 2) {
    stop("At least 2 measurement columns are required")
  }

  missing_cols <- setdiff(measurement_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Columns not found in data:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  cor_data <- data[, measurement_cols, drop = FALSE]

  non_numeric <- names(cor_data)[!vapply(
    cor_data, is.numeric, logical(1)
  )]
  if (length(non_numeric) > 0) {
    stop(paste(
      "All columns must be numeric. Non-numeric columns:",
      paste(non_numeric, collapse = ", ")
    ))
  }

  constant_cols <- names(cor_data)[vapply(cor_data, function(x) {
    v <- stats$var(x, na.rm = TRUE)
    is.na(v) || v == 0
  }, logical(1))]
  if (length(constant_cols) > 0) {
    stop(paste(
      "Cannot compute correlations -",
      "constant or all-NA columns found:",
      paste(constant_cols, collapse = ", ")
    ))
  }

  complete_rows <- stats$complete.cases(cor_data)
  if (sum(complete_rows) < 2) {
    stop(
      "Not enough complete observations",
      " (need at least 2) to compute correlations"
    )
  }

  invisible(TRUE)
}

compute_cor_matrix <- function(cor_data) {
  cor_matrix <- stats$cor(
    cor_data, use = "pairwise.complete.obs"
  )

  if (any(is.na(cor_matrix))) {
    complete_rows <- stats$complete.cases(cor_data)
    if (sum(complete_rows) >= 2) {
      cor_matrix <- stats$cor(
        cor_data[complete_rows, ], use = "everything"
      )
    }
    if (any(is.na(cor_matrix))) {
      stop(
        "Unable to compute correlations due to missing values"
      )
    }
  }

  cor_matrix
}

cluster_columns <- function(cor_matrix, measurement_cols) {
  dist_matrix <- stats$as.dist(1 - cor_matrix)
  hc <- stats$hclust(dist_matrix, method = "complete")
  measurement_cols[hc$order]
}

cor_matrix_to_long <- function(cor_matrix, ordered_cols) {
  cor_df <- as.data.frame(cor_matrix)
  cor_df$Var1 <- rownames(cor_matrix)

  # Reshape to long format using base R
  var_cols <- setdiff(names(cor_df), "Var1")
  cor_long <- data.frame(
    Var1 = rep(cor_df$Var1, times = length(var_cols)),
    Var2 = rep(var_cols, each = nrow(cor_df)),
    correlation = unlist(cor_df[, var_cols], use.names = FALSE),
    stringsAsFactors = FALSE
  )

  cor_long$Var1 <- factor(cor_long$Var1, levels = ordered_cols)
  cor_long$Var2 <- factor(cor_long$Var2, levels = ordered_cols)

  # Keep only lower triangle (including diagonal)
  cor_long <- cor_long[
    as.integer(cor_long$Var1) >= as.integer(cor_long$Var2),
  ]

  cor_long$tooltip <- sprintf(
    "<b>%s</b> vs <b>%s</b><br/>r = %.3f",
    as.character(cor_long$Var1),
    as.character(cor_long$Var2),
    cor_long$correlation
  )

  cor_long$data_id <- paste(
    as.character(cor_long$Var1),
    as.character(cor_long$Var2),
    sep = "_"
  )

  cor_long
}
