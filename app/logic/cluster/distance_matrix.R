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
# Pure logic functions for distance matrix plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute distance matrix data for plotting
#'
#' Validates inputs and computes the distance matrix with
#' hierarchical clustering. Expects data to be already cleaned
#' (NAs removed) and scaled as needed by the caller.
#'
#' @param data Data frame containing the measurement columns (already cleaned and scaled)
#' @param measurement_cols Character vector of column names
#' @param metric Character, distance metric ("euclidean" or "manhattan")
#' @return List with $success, $result or $error
#' @export
compute_distance_matrix <- function(data, measurement_cols, metric) {
  error_handling$safe_execute(
    expr = {
      validate_distance_inputs(data, measurement_cols, metric)
      
      # Extract measurement data
      dist_data <- data[, measurement_cols, drop = FALSE]
      
      # Compute distance matrix
      dist_matrix <- compute_dist_matrix(dist_data, metric)
      
      # Order samples using hierarchical clustering
      ordered_samples <- cluster_samples(dist_matrix)
      
      # Convert to long format for plotting
      dist_long <- dist_matrix_to_long(dist_matrix, ordered_samples)
      
      rhino$log$info(
        "Distance matrix: computed ({nrow(data)} samples, {length(measurement_cols)} cols, {metric} metric)"
      )
      
      list(
        dist_long = dist_long,
        ordered_samples = ordered_samples,
        n_samples = nrow(data),
        metric = metric
      )
    },
    operation_name = "Distance Matrix",
    context = list(
      n_samples = nrow(data),
      n_columns = length(measurement_cols),
      metric = metric
    ),
    error_parser = distance_error_parser
  )
}

#' Create the ggplot object for the distance matrix heatmap
#'
#' Builds the ggplot (without girafe wrapping) so it can
#' be reused for both interactive display and static export.
#'
#' @param distance_data List with $dist_long, $ordered_samples, $n_samples, $metric
#' @return ggplot object
#' @export
create_distance_ggplot <- function(distance_data) {
  dist_long <- distance_data$dist_long
  n_samples <- distance_data$n_samples
  metric <- distance_data$metric
  
  show_labels <- n_samples <= 50
  cell_text_size <- if (n_samples <= 6) {
    4
  } else if (n_samples <= 10) {
    3.5
  } else if (n_samples <= 15) {
    3
  } else if (n_samples <= 20) {
    2.5
  } else {
    2
  }
  
  axis_text_size <- if (n_samples <= 10) {
    11
  } else if (n_samples <= 20) {
    10
  } else if (n_samples <= 30) {
    9
  } else if (n_samples <= 50) {
    8
  } else {
    7
  }
  
  # Find max distance for color scale
  max_dist <- max(dist_long$distance, na.rm = TRUE)
  
  p <- ggplot2$ggplot(
    dist_long,
    ggplot2$aes(x = Sample1, y = Sample2, fill = distance)
  ) +
    ggiraph$geom_tile_interactive(
      ggplot2$aes(tooltip = tooltip, data_id = data_id),
      color = "white",
      linewidth = 0.5
    ) +
    ggplot2$scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B2182B",
      midpoint = max_dist / 2,
      limits = c(0, max_dist),
      name = paste0("Distance\n(", metric, ")")
    ) +
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
  
  # Add text labels for small datasets
  if (show_labels && n_samples <= 20) {
    p <- p + ggplot2$geom_text(
      ggplot2$aes(label = sprintf("%.2f", distance)),
      color = ifelse(
        dist_long$distance > (max_dist * 0.6), "white", "black"
      ),
      size = cell_text_size
    )
  }
  
  p
}

#' Render distance matrix plot as ggiraph
#'
#' Takes pre-computed distance data and renders it as an
#' interactive ggiraph heatmap. Should not fail if
#' compute_distance_matrix() succeeded. Text sizes, axis
#' label sizes, and SVG dimensions adapt to the number of
#' samples.
#'
#' @param distance_data List with $dist_long, $ordered_samples, $n_samples, $metric
#' @return ggiraph interactive plot object
#' @export
render_distance_girafe <- function(distance_data) {
  n_samples <- distance_data$n_samples
  
  p <- create_distance_ggplot(distance_data)
  
  # Adaptive SVG dimensions — smaller SVG = larger text
  # after browser scales it to fit the panel
  width_svg <- min(max(n_samples * 0.3 + 3, 6), 10)
  height_svg <- min(max(n_samples * 0.25 + 2.5, 5), 8)
  
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

#' Error parser for distance matrix errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
distance_error_parser <- function(error_msg,
                                   operation_name = "Distance Matrix") {
  if (grepl(
    "constant|singular|zero variance",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Cannot compute distances - ",
      "data may contain constant columns."
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
      ": Data contains too many missing values. ",
      "Please handle missing data first."
    )
  } else if (grepl(
    "columns|measurement|at least 1",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": At least 1 measurement column is required."
    )
  } else if (grepl(
    "metric|distance method",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid distance metric specified."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

validate_distance_inputs <- function(data, measurement_cols, metric) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is NULL or empty")
  }
  
  if (is.null(measurement_cols) || length(measurement_cols) < 1) {
    stop("At least 1 measurement column is required")
  }
  
  missing_cols <- setdiff(measurement_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Columns not found in data:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  dist_data <- data[, measurement_cols, drop = FALSE]
  
  non_numeric <- names(dist_data)[!vapply(
    dist_data, is.numeric, logical(1)
  )]
  if (length(non_numeric) > 0) {
    stop(paste(
      "All columns must be numeric. Non-numeric columns:",
      paste(non_numeric, collapse = ", ")
    ))
  }
  
  # Note: NA checks removed - data should be cleaned upstream
  # following PCA module pattern
  
  valid_metrics <- c("euclidean", "manhattan")
  if (!metric %in% valid_metrics) {
    stop(paste(
      "Invalid distance metric. Must be one of:",
      paste(valid_metrics, collapse = ", ")
    ))
  }
  
  invisible(TRUE)
}

compute_dist_matrix <- function(data, metric) {
  # Data should already be cleaned (NAs removed) and scaled upstream
  if (nrow(data) < 2) {
    stop("Not enough observations to compute distances")
  }
  
  # Compute distance matrix
  dist_obj <- stats$dist(data, method = metric)
  dist_matrix <- as.matrix(dist_obj)
  
  # Set row and column names
  if (is.null(rownames(data))) {
    sample_names <- paste0("Sample_", seq_len(nrow(data)))
  } else {
    sample_names <- rownames(data)
  }
  
  rownames(dist_matrix) <- sample_names
  colnames(dist_matrix) <- sample_names
  
  dist_matrix
}

cluster_samples <- function(dist_matrix) {
  dist_obj <- stats$as.dist(dist_matrix)
  hc <- stats$hclust(dist_obj, method = "complete")
  rownames(dist_matrix)[hc$order]
}

dist_matrix_to_long <- function(dist_matrix, ordered_samples) {
  dist_df <- as.data.frame(dist_matrix)
  dist_df$Sample1 <- rownames(dist_matrix)
  
  # Reshape to long format using base R
  sample_cols <- setdiff(names(dist_df), "Sample1")
  dist_long <- data.frame(
    Sample1 = rep(dist_df$Sample1, times = length(sample_cols)),
    Sample2 = rep(sample_cols, each = nrow(dist_df)),
    distance = unlist(dist_df[, sample_cols], use.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  dist_long$Sample1 <- factor(dist_long$Sample1, levels = ordered_samples)
  dist_long$Sample2 <- factor(dist_long$Sample2, levels = ordered_samples)
  
  # Keep only lower triangle (including diagonal)
  dist_long <- dist_long[
    as.integer(dist_long$Sample1) >= as.integer(dist_long$Sample2),
  ]
  
  dist_long$tooltip <- sprintf(
    "<b>%s</b> vs <b>%s</b><br/>Distance = %.3f",
    as.character(dist_long$Sample1),
    as.character(dist_long$Sample2),
    dist_long$distance
  )
  
  dist_long$data_id <- paste(
    as.character(dist_long$Sample1),
    as.character(dist_long$Sample2),
    sep = "_"
  )
  
  dist_long
}
