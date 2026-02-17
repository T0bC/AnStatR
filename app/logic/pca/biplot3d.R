box::use(
  plotly,
  rhino,
  scales,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for 3D PCA biplot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a 3D PCA biplot using plotly
#'
#' Builds an interactive 3D scatter plot of individual scores
#' with variable loadings as arrows from the origin. Grouping
#' is applied via color. Returns a plotly object.
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param dim_x Character, dimension for x-axis (e.g. "Dim.1")
#' @param dim_y Character, dimension for y-axis (e.g. "Dim.2")
#' @param dim_z Character, dimension for z-axis (e.g. "Dim.3")
#' @param group_cols Character vector, column name(s) in ind$meta
#'   for grouping. Multiple columns are combined via interaction().
#'   NULL for no grouping.
#' @return List with $success, $result (plotly) or $error
#' @export
create_biplot3d <- function(pca_result,
                            dim_x = "Dim.1",
                            dim_y = "Dim.2",
                            dim_z = "Dim.3",
                            group_cols = NULL) {
  error_context <- list(
    dim_x = dim_x,
    dim_y = dim_y,
    dim_z = dim_z,
    group_cols = paste(
      group_cols %||% "none", collapse = ", "
    )
  )

  error_handling$safe_execute(
    expr = {
      validate_biplot3d_inputs(
        pca_result, dim_x, dim_y, dim_z
      )

      dims <- c(dim_x, dim_y, dim_z)
      eig <- pca_result$eig

      # Build data frames
      ind_data <- build_ind_data(
        pca_result, dims, group_cols
      )
      var_data <- build_var_data(
        pca_result, dims, ind_data
      )

      # Axis ranges with 10% buffer
      axis_ranges <- compute_axis_ranges(
        ind_data, var_data, dims
      )

      # Axis labels with variance %
      x_label <- axis_label(dim_x, eig)
      y_label <- axis_label(dim_y, eig)
      z_label <- axis_label(dim_z, eig)

      # Color palette
      groups <- unique(ind_data$group)
      n_groups <- length(groups)
      col_vec <- scales$hue_pal()(n_groups)

      # Hover text
      hover_text <- build_hover_text(
        pca_result, ind_data, dims
      )

      # Build plotly figure
      fig <- plotly$plot_ly()

      # Individuals: colored by group
      fig <- fig |>
        plotly$add_trace(
          data = ind_data,
          x = ~get(dim_x),
          y = ~get(dim_y),
          z = ~get(dim_z),
          type = "scatter3d",
          mode = "markers",
          opacity = 0.8,
          marker = list(
            size = 6,
            line = list(width = 1, color = "black")
          ),
          color = ~group,
          colors = col_vec,
          text = hover_text,
          hoverinfo = "text"
        )

      # Variable loadings as lines from origin
      loading_colors <- scales$hue_pal()(
        nrow(var_data)
      )
      for (i in seq_len(nrow(var_data))) {
        fig <- fig |>
          plotly$add_trace(
            x = c(0, var_data[i, dim_x]),
            y = c(0, var_data[i, dim_y]),
            z = c(0, var_data[i, dim_z]),
            type = "scatter3d",
            mode = "lines",
            line = list(
              width = 4, color = loading_colors[i]
            ),
            showlegend = FALSE,
            hoverinfo = "none"
          )
        # Label at arrow tip
        fig <- fig |>
          plotly$add_trace(
            x = var_data[i, dim_x],
            y = var_data[i, dim_y],
            z = var_data[i, dim_z],
            type = "scatter3d",
            mode = "text",
            text = rownames(var_data)[i],
            textposition = "top center",
            textfont = list(
              size = 11, color = loading_colors[i]
            ),
            showlegend = FALSE,
            hoverinfo = "none"
          )
      }

      # Origin axis lines
      fig <- add_origin_axes(
        fig, axis_ranges, dims
      )

      # Layout
      fig <- fig |>
        plotly$layout(
          scene = list(
            xaxis = list(title = x_label),
            yaxis = list(title = y_label),
            zaxis = list(title = z_label)
          ),
          title = "PCA \u2014 3D Biplot"
        )

      rhino$log$info(
        "3D Biplot: created ({dim_x}, {dim_y},",
        " {dim_z}, {n_groups} groups)"
      )

      fig
    },
    operation_name = "3D Biplot",
    context = error_context,
    error_parser = biplot3d_error_parser
  )
}

#' Error parser for 3D biplot-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
biplot3d_error_parser <- function(error_msg,
                                  operation_name =
                                    "3D Biplot") {
  if (grepl(
    "at least 3",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Need at least 3 PCA dimensions.",
      " Add more measurement variables."
    )
  } else if (grepl(
    "dimension|dim_|not found",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid dimension selection.",
      " Please check available components."
    )
  } else if (grepl(
    "NULL|missing|pca_result",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": No PCA result available.",
      " Please compute PCA first."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Validate 3D biplot inputs
validate_biplot3d_inputs <- function(pca_result,
                                     dim_x, dim_y,
                                     dim_z) {
  if (is.null(pca_result)) {
    stop("pca_result is NULL")
  }

  available_dims <- colnames(pca_result$var$coord)
  if (length(available_dims) < 3) {
    stop(paste(
      "Need at least 3 PCA dimensions, but only",
      length(available_dims), "available."
    ))
  }

  for (d in c(dim_x, dim_y, dim_z)) {
    if (!d %in% available_dims) {
      stop(paste("Dimension not found:", d))
    }
  }

  if (length(unique(c(dim_x, dim_y, dim_z))) < 3) {
    stop("All three dimensions must be different.")
  }
}

#' Build individual scores data frame
build_ind_data <- function(pca_result, dims,
                           group_cols) {
  coord <- pca_result$ind$coord
  meta <- pca_result$ind$meta

  df <- as.data.frame(
    coord[, dims, drop = FALSE]
  )

  # Group column
  if (!is.null(group_cols) &&
      length(group_cols) > 0 &&
      !is.null(meta)) {
    valid_cols <- intersect(group_cols, names(meta))
    if (length(valid_cols) == 1) {
      df$group <- as.factor(meta[[valid_cols]])
    } else if (length(valid_cols) > 1) {
      df$group <- interaction(
        meta[, valid_cols, drop = FALSE],
        sep = " / ", drop = TRUE
      )
    } else {
      df$group <- factor(
        rep("No Grouping", nrow(df))
      )
    }
  } else {
    df$group <- factor(
      rep("No Grouping", nrow(df))
    )
  }

  df
}

#' Build variable loadings data frame, scaled to
#' match the range of individual scores
build_var_data <- function(pca_result, dims,
                           ind_data) {
  var_coord <- pca_result$var$coord
  df <- as.data.frame(
    var_coord[, dims, drop = FALSE]
  )

  # Scale: max_ind / max_var so arrows fit the data
  max_ind <- max(
    abs(unlist(ind_data[, dims])), na.rm = TRUE
  )
  max_var <- max(
    abs(unlist(df)), na.rm = TRUE
  )
  if (max_var > 0) {
    scale_factor <- max_ind / max_var
    df <- df * scale_factor
  }

  df
}

#' Compute axis ranges with buffer
compute_axis_ranges <- function(ind_data, var_data,
                                dims) {
  buffer <- 0.1
  ranges <- lapply(dims, function(d) {
    all_vals <- c(ind_data[[d]], var_data[[d]], 0)
    r <- range(all_vals, na.rm = TRUE)
    span <- r[2] - r[1]
    c(min = r[1] - span * buffer,
      max = r[2] + span * buffer)
  })
  names(ranges) <- dims
  ranges
}

#' Build hover text for individuals
build_hover_text <- function(pca_result, ind_data,
                             dims) {
  meta <- pca_result$ind$meta
  meta_cols <- if (!is.null(meta) &&
      !("Row" %in% names(meta) &&
        ncol(meta) == 1)) {
    names(meta)
  } else {
    character(0)
  }

  vapply(
    seq_len(nrow(ind_data)),
    function(i) {
      parts <- character(0)
      # Metadata
      if (length(meta_cols) > 0) {
        for (col in meta_cols) {
          parts <- c(parts, paste0(
            col, ": ",
            as.character(meta[i, col])
          ))
        }
      }
      # Dimension values
      for (d in dims) {
        parts <- c(parts, paste0(
          d, ": ",
          sprintf("%.3f", ind_data[i, d])
        ))
      }
      # Group
      parts <- c(parts, paste0(
        "Group: ", as.character(ind_data$group[i])
      ))
      paste(parts, collapse = "<br>")
    },
    character(1)
  )
}

#' Add origin axis lines to the plotly figure
add_origin_axes <- function(fig, axis_ranges, dims) {
  # X axis line
  fig <- fig |>
    plotly$add_trace(
      x = c(
        axis_ranges[[dims[1]]]["min"],
        axis_ranges[[dims[1]]]["max"]
      ),
      y = c(0, 0),
      z = c(0, 0),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "black", width = 2),
      showlegend = FALSE,
      hoverinfo = "none"
    )
  # Y axis line
  fig <- fig |>
    plotly$add_trace(
      x = c(0, 0),
      y = c(
        axis_ranges[[dims[2]]]["min"],
        axis_ranges[[dims[2]]]["max"]
      ),
      z = c(0, 0),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "black", width = 2),
      showlegend = FALSE,
      hoverinfo = "none"
    )
  # Z axis line
  fig <- fig |>
    plotly$add_trace(
      x = c(0, 0),
      y = c(0, 0),
      z = c(
        axis_ranges[[dims[3]]]["min"],
        axis_ranges[[dims[3]]]["max"]
      ),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "black", width = 2),
      showlegend = FALSE,
      hoverinfo = "none"
    )
  fig
}

#' Build axis label with variance percentage
axis_label <- function(dim_name, eig) {
  dim_idx <- which(rownames(eig) == dim_name)
  if (length(dim_idx) == 1) {
    var_pct <- eig[dim_idx, "variance.percent"]
    sprintf("%s (%.1f%%)", dim_name, var_pct)
  } else {
    dim_name
  }
}
