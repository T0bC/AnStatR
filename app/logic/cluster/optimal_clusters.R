box::use(
  cluster,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for optimal number of clusters
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Calculate optimal number of clusters using multiple methods
#'
#' Computes Elbow (WSS), Silhouette, and Gap statistic for
#' k = 2..max_k, then returns the median of the three optimal
#' values as the recommended number of clusters.
#'
#' Data should already be cleaned (NAs removed) and scaled
#' as needed by the caller.
#'
#' @param data Data frame with numeric measurement columns
#'   (already cleaned and scaled)
#' @param measurement_cols Character vector of column names
#' @param max_k Integer, maximum number of clusters to evaluate
#'   (default 10, clamped to nrow - 1)
#' @return List with $success, $result or $error.
#'   Result contains $methods, $summary, $plot_data.
#' @export
compute_optimal_clusters <- function(data, measurement_cols,
                                      max_k = 10) {
  error_handling$safe_execute(
    expr = {
      validate_optimal_inputs(data, measurement_cols)

      num_data <- as.matrix(
        data[, measurement_cols, drop = FALSE]
      )
      n <- nrow(num_data)
      max_k <- min(max_k, n - 1)

      if (max_k < 2) {
        stop(
          "Not enough observations to evaluate ",
          "multiple cluster solutions (need at least 3 rows)"
        )
      }

      k_range <- 2:max_k

      # Method 1: Elbow (within-cluster sum of squares)
      wss_result <- compute_wss(num_data, k_range)

      # Method 2: Silhouette
      sil_result <- compute_silhouette(num_data, k_range)

      # Method 3: Gap statistic
      gap_result <- compute_gap(num_data, max_k)

      # Build methods list
      methods <- list()
      methods$elbow <- list(
        name = "Elbow (WSS)",
        optimal_k = wss_result$optimal_k,
        values = wss_result$values,
        description = paste(
          "Minimizes within-cluster sum of squares;",
          "optimal k at the elbow point"
        )
      )
      methods$silhouette <- list(
        name = "Silhouette",
        optimal_k = sil_result$optimal_k,
        values = sil_result$values,
        description = paste(
          "Maximizes average silhouette width;",
          "measures how well objects fit their cluster"
        )
      )

      if (gap_result$success) {
        methods$gap <- list(
          name = "Gap Statistic",
          optimal_k = gap_result$optimal_k,
          values = gap_result$values,
          se = gap_result$se,
          description = paste(
            "Compares within-cluster dispersion to a",
            "null reference distribution"
          )
        )
      } else {
        methods$gap <- list(
          name = "Gap Statistic",
          optimal_k = NA,
          error = gap_result$error,
          description = paste(
            "Compares within-cluster dispersion to a",
            "null reference distribution"
          )
        )
      }

      # Summary: median of valid optimal k values
      valid_ks <- vapply(methods, function(m) {
        if (
          !is.null(m$optimal_k) && !is.na(m$optimal_k)
        ) {
          m$optimal_k
        } else {
          NA_real_
        }
      }, numeric(1))
      valid_ks <- valid_ks[!is.na(valid_ks)]

      summary <- if (length(valid_ks) > 0) {
        list(
          min_k = min(valid_ks),
          max_k = max(valid_ks),
          median_k = round(stats$median(valid_ks)),
          methods_computed = length(valid_ks)
        )
      } else {
        list(
          min_k = 2,
          max_k = max_k,
          median_k = 2,
          methods_computed = 0
        )
      }

      # Build unified plot data
      plot_data <- build_plot_data(
        k_range, wss_result, sil_result, gap_result
      )

      rhino$log$info(
        "Optimal clusters: ",
        "Elbow={methods$elbow$optimal_k}, ",
        "Silhouette={methods$silhouette$optimal_k}, ",
        "Gap={methods$gap$optimal_k}, ",
        "Median={summary$median_k} ",
        "({n} obs, {length(measurement_cols)} vars)"
      )

      list(
        methods = methods,
        summary = summary,
        plot_data = plot_data,
        k_range = k_range
      )
    },
    operation_name = "Optimal Clusters",
    context = list(
      n_samples = nrow(data),
      n_columns = length(measurement_cols),
      max_k = max_k
    ),
    error_parser = optimal_clusters_error_parser
  )
}

#' Create the ggplot object for the optimal clusters plot
#'
#' Builds a faceted ggplot showing Elbow (WSS), Silhouette,
#' and Gap statistic across k values, with optimal k markers.
#' Reusable for both interactive display and static export.
#'
#' @param optimal_data Result list from compute_optimal_clusters
#' @return ggplot object
#' @export
create_optimal_clusters_ggplot <- function(optimal_data) {
  # Imported here to keep box::use at top level clean
  # (ggplot2/ggiraph only needed for plotting)
  box::use(
    ggiraph,
    ggplot2,
  )

  plot_df <- optimal_data$plot_data
  methods <- optimal_data$methods

  # Build optimal k markers
  markers <- data.frame(
    k = integer(0),
    value = numeric(0),
    method = character(0),
    stringsAsFactors = FALSE
  )

  if (
    !is.null(methods$elbow$optimal_k) &&
    !is.na(methods$elbow$optimal_k)
  ) {
    idx <- which(
      plot_df$k == methods$elbow$optimal_k &
      plot_df$method == "WSS (Elbow)"
    )
    if (length(idx) > 0) {
      markers <- rbind(markers, data.frame(
        k = methods$elbow$optimal_k,
        value = plot_df$value[idx[1]],
        method = "WSS (Elbow)",
        stringsAsFactors = FALSE
      ))
    }
  }

  if (
    !is.null(methods$silhouette$optimal_k) &&
    !is.na(methods$silhouette$optimal_k)
  ) {
    idx <- which(
      plot_df$k == methods$silhouette$optimal_k &
      plot_df$method == "Silhouette"
    )
    if (length(idx) > 0) {
      markers <- rbind(markers, data.frame(
        k = methods$silhouette$optimal_k,
        value = plot_df$value[idx[1]],
        method = "Silhouette",
        stringsAsFactors = FALSE
      ))
    }
  }

  if (
    !is.null(methods$gap$optimal_k) &&
    !is.na(methods$gap$optimal_k)
  ) {
    idx <- which(
      plot_df$k == methods$gap$optimal_k &
      plot_df$method == "Gap Statistic"
    )
    if (length(idx) > 0) {
      markers <- rbind(markers, data.frame(
        k = methods$gap$optimal_k,
        value = plot_df$value[idx[1]],
        method = "Gap Statistic",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Tooltip for all points
  plot_df$tooltip <- sprintf(
    "k = %d\nValue = %.4f",
    plot_df$k, plot_df$value
  )
  plot_df$data_id <- paste(
    plot_df$method, plot_df$k, sep = "_"
  )

  # Marker tooltips
  if (nrow(markers) > 0) {
    markers$tooltip <- sprintf(
      "Optimal k = %d\nValue = %.4f",
      markers$k, markers$value
    )
    markers$data_id <- paste(
      "optimal", markers$method, markers$k,
      sep = "_"
    )
  }

  # Determine method levels for facet ordering
  method_levels <- c(
    "WSS (Elbow)", "Silhouette", "Gap Statistic"
  )
  present_levels <- method_levels[
    method_levels %in% unique(plot_df$method)
  ]
  plot_df$method <- factor(
    plot_df$method, levels = present_levels
  )
  if (nrow(markers) > 0) {
    markers$method <- factor(
      markers$method, levels = present_levels
    )
  }

  k_range <- optimal_data$k_range

  p <- ggplot2$ggplot(
    plot_df,
    ggplot2$aes(x = k, y = value)
  ) +
    ggplot2$geom_line(
      color = "#6c757d", linewidth = 0.8
    ) +
    ggiraph$geom_point_interactive(
      ggplot2$aes(
        tooltip = tooltip,
        data_id = data_id
      ),
      size = 3, color = "#212529"
    )

  # Add optimal k markers
  if (nrow(markers) > 0) {
    p <- p +
      ggiraph$geom_point_interactive(
        data = markers,
        ggplot2$aes(
          x = k, y = value,
          tooltip = tooltip,
          data_id = data_id
        ),
        size = 5, color = "#dc3545", shape = 18
      )
  }

  # Add gap SE error bars if present
  gap_rows <- plot_df[plot_df$method == "Gap Statistic", ]
  if (nrow(gap_rows) > 0 && !all(is.na(gap_rows$se))) {
    p <- p +
      ggplot2$geom_errorbar(
        data = gap_rows,
        ggplot2$aes(
          ymin = value - se,
          ymax = value + se
        ),
        width = 0.2, color = "#6c757d", alpha = 0.6
      )
  }

  p <- p +
    ggplot2$facet_wrap(
      ~method, scales = "free_y", ncol = 1
    ) +
    ggplot2$scale_x_continuous(
      breaks = k_range
    ) +
    ggplot2$labs(
      title = "Optimal Number of Clusters",
      x = "Number of Clusters (k)",
      y = "Value"
    ) +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        size = 12, face = "bold"
      ),
      strip.text = ggplot2$element_text(
        size = 11, face = "bold"
      ),
      panel.grid.minor = ggplot2$element_blank()
    )

  p
}

#' Error parser for optimal clusters errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
optimal_clusters_error_parser <- function(
    error_msg,
    operation_name = "Optimal Clusters") {
  if (grepl(
    "constant|variance|zero",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains constant columns with zero variance."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values."
    )
  } else if (grepl(
    "observations|rows|enough",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Not enough observations for cluster evaluation."
    )
  } else if (grepl(
    "numeric|non-numeric",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

validate_optimal_inputs <- function(data, measurement_cols) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is NULL or empty")
  }

  if (
    is.null(measurement_cols) ||
    length(measurement_cols) < 1
  ) {
    stop("At least 1 measurement column is required")
  }

  missing_cols <- setdiff(measurement_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Columns not found in data:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  num_data <- data[, measurement_cols, drop = FALSE]
  non_numeric <- names(num_data)[!vapply(
    num_data, is.numeric, logical(1)
  )]
  if (length(non_numeric) > 0) {
    stop(paste(
      "All columns must be numeric. Non-numeric columns:",
      paste(non_numeric, collapse = ", ")
    ))
  }

  invisible(TRUE)
}

compute_wss <- function(data, k_range) {
  wss_values <- vapply(k_range, function(k) {
    km <- stats$kmeans(data, centers = k, nstart = 10)
    km$tot.withinss
  }, numeric(1))

  # Detect elbow via second derivative
  optimal_k <- detect_elbow_k(k_range, wss_values)

  list(
    values = wss_values,
    optimal_k = optimal_k
  )
}

compute_silhouette <- function(data, k_range) {
  dist_matrix <- stats$dist(data)

  sil_values <- vapply(k_range, function(k) {
    km <- stats$kmeans(data, centers = k, nstart = 10)
    sil <- cluster$silhouette(km$cluster, dist_matrix)
    mean(sil[, "sil_width"])
  }, numeric(1))

  # Optimal k = max silhouette
  optimal_k <- k_range[which.max(sil_values)]

  list(
    values = sil_values,
    optimal_k = optimal_k
  )
}

compute_gap <- function(data, max_k) {
  tryCatch(
    {
      gap_stat <- cluster$clusGap(
        data,
        FUNcluster = stats$kmeans,
        nstart = 10,
        K.max = max_k,
        B = 50
      )

      gap_values <- gap_stat$Tab[, "gap"]
      se_values <- gap_stat$Tab[, "SE.sim"]

      # Use firstSEmax method (Tibshirani et al.)
      optimal_k <- cluster$maxSE(
        gap_values, se_values, method = "firstSEmax"
      )

      list(
        success = TRUE,
        values = gap_values,
        se = se_values,
        optimal_k = optimal_k
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        error = conditionMessage(e)
      )
    }
  )
}

detect_elbow_k <- function(k_range, values) {
  n <- length(values)
  if (n < 3) return(k_range[1])

  first_diff <- diff(values)
  second_diff <- diff(first_diff)

  if (length(second_diff) > 0) {
    # For WSS (decreasing), elbow is where second
    # derivative is most positive (biggest deceleration)
    elbow_idx <- which.max(second_diff) + 1
    elbow_idx <- max(1, min(elbow_idx, n))
    k_range[elbow_idx]
  } else {
    k_range[1]
  }
}

build_plot_data <- function(k_range, wss_result,
                             sil_result, gap_result) {
  df <- data.frame(
    k = rep(k_range, 2),
    value = c(wss_result$values, sil_result$values),
    method = c(
      rep("WSS (Elbow)", length(k_range)),
      rep("Silhouette", length(k_range))
    ),
    se = NA_real_,
    stringsAsFactors = FALSE
  )

  if (gap_result$success) {
    # Gap stat includes k=1..max_k, we need k=2..max_k
    gap_vals <- gap_result$values
    gap_se <- gap_result$se
    # clusGap returns values for k=1..K.max
    # We only plot k=2..max_k to match our k_range
    if (length(gap_vals) >= max(k_range)) {
      gap_df <- data.frame(
        k = k_range,
        value = gap_vals[k_range],
        method = "Gap Statistic",
        se = gap_se[k_range],
        stringsAsFactors = FALSE
      )
      df <- rbind(df, gap_df)
    }
  }

  df
}
