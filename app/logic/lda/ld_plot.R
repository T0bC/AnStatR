box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/error_handling,
  app/logic/lda/lda_diagnostics[
    add_diagnostics_overlay,
    add_boundaries_overlay,
    compute_1d_boundary,
  ],
)

# =============================================================================
# Pure logic functions for LDA discriminant scores plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create an interactive LD scores scatter plot
#'
#' Builds a ggplot with ggiraph interactive points showing
#' individuals projected onto linear discriminant axes.
#' Points are colored by the grouping variable.
#' Falls back to a 1D jittered strip plot when only one
#' LD axis exists (2 groups).
#'
#' @param lda_result Result list from run_lda() (must have
#'   $scores, $meta, $grouping_col, $proportion_of_trace)
#' @param dim_x Character, LD column for x-axis (e.g. "LD1")
#' @param dim_y Character, LD column for y-axis (e.g. "LD2").
#'   Ignored when only 1 LD axis exists.
#' @param show_diagnostics Logical, overlay assumption
#'   diagnostics (covariance ellipses + group means)
#' @param show_boundaries Logical, overlay decision
#'   boundary regions and contour lines (LDA only,
#'   requires a fitted model)
#' @return List with $success, $result (ggplot) or $error
#' @export
create_ld_plot <- function(lda_result,
                           dim_x = "LD1",
                           dim_y = "LD2",
                           show_diagnostics = FALSE,
                           show_boundaries = FALSE) {
  error_handling$safe_execute(
    expr = {
      scores <- lda_result$scores
      if (is.null(scores) || ncol(scores) == 0) {
        stop(
          "No LD scores available. ",
          "LD plot requires an LDA model ",
          "(not QDA or CV mode)."
        )
      }

      meta <- lda_result$meta
      grouping_col <- lda_result$grouping_col
      n_ld <- ncol(scores)

      # Determine if 1D fallback is needed
      is_1d <- n_ld == 1

      if (is_1d) {
        dim_x <- colnames(scores)[1]
        build_1d_plot(
          scores, meta, grouping_col,
          dim_x, lda_result$proportion_of_trace,
          lda_result = lda_result,
          show_boundaries = show_boundaries
        )
      } else {
        # Validate requested dimensions exist
        if (!dim_x %in% colnames(scores)) {
          stop(paste("Dimension not found:", dim_x))
        }
        if (!dim_y %in% colnames(scores)) {
          stop(paste("Dimension not found:", dim_y))
        }
        build_2d_plot(
          scores, meta, grouping_col,
          dim_x, dim_y,
          lda_result$proportion_of_trace,
          lda_result = lda_result,
          show_diagnostics = show_diagnostics,
          show_boundaries = show_boundaries
        )
      }
    },
    operation_name = "LD Plot",
    error_parser = ld_plot_error_parser
  )
}


#' @export
ld_plot_error_parser <- function(error_msg,
                                 operation_name = "LD Plot") {
  if (grepl(
    "dimension|not found",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid dimension selection.",
      " Please check available LD axes."
    )
  } else if (grepl(
    "scores|NULL",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": No LD scores available.",
      " Run LDA (not QDA) without CV to get scores."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

build_2d_plot <- function(scores, meta, grouping_col,
                          dim_x, dim_y,
                          proportion_of_trace,
                          lda_result = NULL,
                          show_diagnostics = FALSE,
                          show_boundaries = FALSE) {
  df <- data.frame(
    x = scores[[dim_x]],
    y = scores[[dim_y]],
    stringsAsFactors = FALSE
  )

  # Group column
  group_vals <- get_group_values(meta, grouping_col)
  df$group <- as.factor(group_vals)

  # Tooltip
  df$tooltip <- build_tooltips(
    scores, meta, grouping_col, dim_x, dim_y
  )
  df$data_id <- paste0("ld_", seq_len(nrow(df)))

  # Axis labels with proportion of trace
  x_label <- axis_label(dim_x, proportion_of_trace)
  y_label <- axis_label(dim_y, proportion_of_trace)

  p <- ggplot2$ggplot(
    df,
    ggplot2$aes(
      x = x, y = y,
      fill = group
    )
  ) +
    ggiraph$geom_point_interactive(
      ggplot2$aes(
        tooltip = tooltip,
        data_id = data_id
      ),
      shape = 21,
      color = "white",
      stroke = 0.6,
      size = 3,
      alpha = 0.85
    ) +
    ggplot2$labs(
      x = x_label,
      y = y_label,
      fill = "Group",
      title = paste0("LD Scores \u2014 ", dim_x, " vs ", dim_y)
    ) +
    ld_theme()

  # Overlay decision boundaries when requested
  has_model <- !is.null(lda_result) &&
    !is.null(lda_result$model)
  if (isTRUE(show_boundaries) && has_model) {
    p <- add_boundaries_overlay(
      p, lda_result, dim_x, dim_y
    )
  }

  # Overlay assumption diagnostics when requested
  if (isTRUE(show_diagnostics) && ncol(scores) >= 2) {
    group_vals <- get_group_values(meta, grouping_col)
    p <- add_diagnostics_overlay(
      p, scores, group_vals, dim_x, dim_y
    )
  }

  # Build combined subtitle
  subtitle_parts <- character(0)
  if (isTRUE(show_boundaries) && has_model) {
    subtitle_parts <- c(
      subtitle_parts, "shaded: decision regions"
    )
  }
  if (isTRUE(show_diagnostics) && ncol(scores) >= 2) {
    subtitle_parts <- c(
      subtitle_parts,
      "solid: per-group VC, dashed: pooled VC (1.5 SD)"
    )
  }
  if (length(subtitle_parts) > 0) {
    p <- p + ggplot2$labs(
      subtitle = paste(
        subtitle_parts, collapse = " | "
      )
    )
  }

  p
}


build_1d_plot <- function(scores, meta, grouping_col,
                          dim_x, proportion_of_trace,
                          lda_result = NULL,
                          show_boundaries = FALSE) {
  df <- data.frame(
    x = scores[[dim_x]],
    stringsAsFactors = FALSE
  )

  group_vals <- get_group_values(meta, grouping_col)
  df$group <- as.factor(group_vals)

  # Tooltip (1D: only one dim)
  df$tooltip <- build_tooltips(
    scores, meta, grouping_col, dim_x, NULL
  )
  df$data_id <- paste0("ld_", seq_len(nrow(df)))

  x_label <- axis_label(dim_x, proportion_of_trace)

  p <- ggplot2$ggplot(
    df,
    ggplot2$aes(
      x = x, y = group,
      fill = group
    )
  ) +
    ggiraph$geom_point_interactive(
      ggplot2$aes(
        tooltip = tooltip,
        data_id = data_id
      ),
      shape = 21,
      color = "white",
      stroke = 0.6,
      size = 3,
      alpha = 0.85,
      position = ggplot2$position_jitter(
        height = 0.25, seed = 42
      )
    ) +
    ggplot2$labs(
      x = x_label,
      y = "Group",
      fill = "Group",
      title = paste0("LD Scores \u2014 ", dim_x)
    ) +
    ld_theme() +
    ggplot2$theme(aspect.ratio = 0.4)

  # 1D decision boundary: vertical line
  has_model <- !is.null(lda_result) &&
    !is.null(lda_result$model)
  if (isTRUE(show_boundaries) && has_model) {
    boundary_x <- compute_1d_boundary(lda_result)
    p <- p +
      ggplot2$geom_vline(
        xintercept = boundary_x,
        colour = "grey30",
        linewidth = 0.8,
        linetype = "dashed"
      ) +
      ggplot2$labs(
        subtitle = paste0(
          "dashed line: decision boundary at ",
          round(boundary_x, 3)
        )
      )
  }

  p
}


#' @export
get_group_values <- function(meta, grouping_col) {
  if (
    !is.null(grouping_col) &&
    !is.null(meta) &&
    grouping_col %in% names(meta)
  ) {
    as.character(meta[[grouping_col]])
  } else {
    rep("All", nrow(meta))
  }
}


#' @export
build_tooltips <- function(scores, meta, grouping_col,
                           dim_x, dim_y) {
  n <- nrow(scores)
  parts <- character(n)

  # Row label: first meta column or row number
  row_label <- if (
    !is.null(meta) && ncol(meta) > 0
  ) {
    as.character(meta[[1]])
  } else {
    as.character(seq_len(n))
  }

  for (i in seq_len(n)) {
    tip <- paste0("<b>", row_label[i], "</b>")

    # LD values
    tip <- paste0(
      tip, "<br/>", dim_x, ": ",
      round(scores[[dim_x]][i], 3)
    )
    if (!is.null(dim_y) && dim_y %in% colnames(scores)) {
      tip <- paste0(
        tip, "<br/>", dim_y, ": ",
        round(scores[[dim_y]][i], 3)
      )
    }

    # Group
    if (
      !is.null(grouping_col) &&
      !is.null(meta) &&
      grouping_col %in% names(meta)
    ) {
      tip <- paste0(
        tip, "<br/>Group: ",
        as.character(meta[[grouping_col]][i])
      )
    }

    # All other metadata columns
    if (!is.null(meta)) {
      other_cols <- setdiff(
        names(meta),
        c(grouping_col, names(meta)[1])
      )
      for (col in other_cols) {
        tip <- paste0(
          tip, "<br/>", col, ": ",
          as.character(meta[[col]][i])
        )
      }
    }

    parts[i] <- tip
  }

  parts
}


#' @export
axis_label <- function(dim_name, proportion_of_trace) {
  if (is.null(proportion_of_trace)) {
    return(dim_name)
  }
  row_idx <- which(
    proportion_of_trace$LD == dim_name
  )
  if (length(row_idx) == 1) {
    pct <- proportion_of_trace$Proportion[row_idx] * 100
    sprintf("%s (%.1f%%)", dim_name, pct)
  } else {
    dim_name
  }
}


#' @export
ld_theme <- function() {
  ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        hjust = 0.5, size = 14, face = "bold"
      ),
      aspect.ratio = 1,
      axis.title = ggplot2$element_text(size = 12),
      axis.text = ggplot2$element_text(size = 10),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = ggplot2$element_text(size = 11),
      legend.text = ggplot2$element_text(size = 10),
      panel.grid.minor = ggplot2$element_blank()
    )
}
