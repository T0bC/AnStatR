box::use(
  colorspace,
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/error_handling,
  app/logic/lda/ld_plot[
    get_group_values, build_tooltips,
    ld_theme, axis_label,
  ],
)

# =============================================================================
# Pure logic functions for LDA assumption diagnostics
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute pooled within-group variance-covariance matrix
#'
#' Averages per-group covariance matrices weighted by (n_k - 1),
#' matching the pooled estimate used by MASS::lda().
#'
#' @param scores Data frame of LD scores (numeric columns only)
#' @param groups Factor or character vector of group labels
#' @return Pooled variance-covariance matrix
#' @export
compute_pooled_vc <- function(scores, groups) {
  groups <- as.factor(groups)
  lvls <- levels(groups)
  p <- ncol(scores)
  pooled <- matrix(0, nrow = p, ncol = p)
  total_df <- 0

  for (g in lvls) {
    idx <- which(groups == g)
    n_g <- length(idx)
    if (n_g < 2) next
    g_scores <- scores[idx, , drop = FALSE]
    g_cov <- stats::cov(g_scores)
    pooled <- pooled + (n_g - 1) * g_cov
    total_df <- total_df + (n_g - 1)
  }

  if (total_df > 0) {
    pooled <- pooled / total_df
  }

  colnames(pooled) <- colnames(scores)
  rownames(pooled) <- colnames(scores)
  pooled
}


#' Generate points on a 2D ellipse from a covariance matrix
#'
#' Uses eigendecomposition to draw an ellipse at n_std
#' standard deviations, centered at the given point.
#'
#' @param vc_matrix 2x2 variance-covariance matrix
#' @param center Numeric vector of length 2 (center x, y)
#' @param n_points Number of points on the ellipse boundary
#' @param n_std Number of standard deviations for the radius
#' @return Data frame with columns x and y
#' @export
generate_ellipse_points <- function(vc_matrix,
                                    center = c(0, 0),
                                    n_points = 100,
                                    n_std = 1.5) {
  eig <- eigen(vc_matrix, symmetric = TRUE)
  vals <- eig$values
  vecs <- eig$vectors

  # Clamp negative eigenvalues to zero (numerical noise)
  vals[vals < 0] <- 0

  theta <- seq(0, 2 * pi, length.out = n_points + 1)
  theta <- theta[-length(theta)]  # remove duplicate endpoint

  # Unit circle scaled by sqrt(eigenvalues) * n_std
  circle <- cbind(
    n_std * sqrt(vals[1]) * cos(theta),
    n_std * sqrt(vals[2]) * sin(theta)
  )

  # Rotate by eigenvectors and translate to center
  pts <- circle %*% t(vecs)
  pts[, 1] <- pts[, 1] + center[1]
  pts[, 2] <- pts[, 2] + center[2]

  data.frame(x = pts[, 1], y = pts[, 2])
}


#' Create combined assumption diagnostics plot
#'
#' Shows per-group covariance ellipses (solid lines)
#' and pooled within-group covariance ellipses (dashed
#' lines, darker shade) on the same LD scores scatter
#' plot, allowing direct comparison.
#'
#' @param lda_result Result list from run_lda()
#' @param dim_x Character, LD column for x-axis
#' @param dim_y Character, LD column for y-axis
#' @return List with $success, $result (ggplot) or $error
#' @export
create_assumption_plot <- function(lda_result,
                                   dim_x = "LD1",
                                   dim_y = "LD2") {
  error_handling$safe_execute(
    expr = {
      scores <- lda_result$scores
      meta <- lda_result$meta
      grouping_col <- lda_result$grouping_col

      validate_diag_inputs(scores, dim_x, dim_y)

      df <- build_diag_df(
        scores, meta, grouping_col, dim_x, dim_y
      )

      group_vals <- get_group_values(
        meta, grouping_col
      )
      groups <- as.factor(group_vals)

      x_label <- axis_label(
        dim_x, lda_result$proportion_of_trace
      )
      y_label <- axis_label(
        dim_y, lda_result$proportion_of_trace
      )

      # stat_ellipse level for ~1.5 SD in bivariate normal
      # P(chi-sq(2) <= 1.5^2) = 1 - exp(-1.5^2/2) ≈ 0.6753
      ellipse_level <- 1 - exp(-1.5^2 / 2)

      # --- Base plot: data points ---
      p <- ggplot2$ggplot(
        df,
        ggplot2$aes(x = x, y = y, fill = group)
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
          alpha = 0.75
        )

      # --- Per-group covariance ellipses (solid) ---
      group_counts <- table(df$group)
      valid_groups <- names(
        group_counts[group_counts >= 4]
      )

      if (length(valid_groups) > 0) {
        ellipse_data <- df[
          df$group %in% valid_groups, ,
          drop = FALSE
        ]
        skipped <- setdiff(
          levels(df$group), valid_groups
        )
        if (length(skipped) > 0) {
          rhino$log$warn(
            "LDA diagnostics: skipping ellipse for",
            " groups with < 4 points: ",
            paste(skipped, collapse = ", ")
          )
        }
        p <- p + ggplot2$stat_ellipse(
          data = ellipse_data,
          ggplot2$aes(
            x = x, y = y, colour = group
          ),
          type = "norm",
          level = ellipse_level,
          linewidth = 0.9,
          linetype = "solid",
          show.legend = FALSE
        )
      }

      # --- Pooled covariance ellipses (dashed) ---
      scores_2d <- data.frame(
        x = scores[[dim_x]],
        y = scores[[dim_y]]
      )
      pooled_vc <- compute_pooled_vc(
        scores_2d, groups
      )

      pooled_frames <- list()
      for (g in levels(groups)) {
        idx <- which(groups == g)
        if (length(idx) < 2) next
        g_mean <- c(
          mean(scores_2d$x[idx]),
          mean(scores_2d$y[idx])
        )
        ell_pts <- generate_ellipse_points(
          pooled_vc, center = g_mean,
          n_points = 100, n_std = 1.5
        )
        ell_pts$group <- g
        pooled_frames[[length(pooled_frames) + 1]] <-
          ell_pts
      }
      pooled_df <- do.call(rbind, pooled_frames)
      pooled_df$group <- factor(
        pooled_df$group, levels = levels(groups)
      )

      # Darkened group colours for pooled ellipses
      n_grp <- nlevels(groups)
      default_hues <- scales::hue_pal()(n_grp)
      dark_hues <- colorspace$darken(
        default_hues, amount = 0.4
      )
      names(dark_hues) <- levels(groups)

      for (g in levels(groups)) {
        g_data <- pooled_df[
          pooled_df$group == g, , drop = FALSE
        ]
        if (nrow(g_data) == 0) next
        p <- p + ggplot2$geom_path(
          data = g_data,
          ggplot2$aes(x = x, y = y),
          colour = dark_hues[[g]],
          linewidth = 1.1,
          linetype = "dashed",
          inherit.aes = FALSE
        )
      }

      # --- Group mean markers (darkened) ---
      for (g in levels(groups)) {
        idx <- which(groups == g)
        if (length(idx) < 2) next
        g_mean_df <- data.frame(
          x = mean(scores_2d$x[idx]),
          y = mean(scores_2d$y[idx])
        )
        p <- p + ggplot2$geom_point(
          data = g_mean_df,
          ggplot2$aes(x = x, y = y),
          colour = dark_hues[[g]],
          shape = 3, size = 4, stroke = 1.5,
          inherit.aes = FALSE
        )
      }

      p <- p +
        ggplot2$labs(
          x = x_label,
          y = y_label,
          fill = "Group",
          title = "Assumption Diagnostics (1.5 SD)",
          subtitle = paste0(
            dim_x, " vs ", dim_y,
            " \u2014 solid: per-group VC,",
            " dashed: pooled VC"
          )
        ) +
        ld_theme()

      p
    },
    operation_name = "Assumption Diagnostics Plot",
    error_parser = diag_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

validate_diag_inputs <- function(scores, dim_x, dim_y) {
  if (is.null(scores) || ncol(scores) < 2) {
    stop(
      "At least 2 LD axes are required for ",
      "assumption diagnostics."
    )
  }
  if (!dim_x %in% colnames(scores)) {
    stop(paste("Dimension not found:", dim_x))
  }
  if (!dim_y %in% colnames(scores)) {
    stop(paste("Dimension not found:", dim_y))
  }
}


build_diag_df <- function(scores, meta, grouping_col,
                          dim_x, dim_y) {
  df <- data.frame(
    x = scores[[dim_x]],
    y = scores[[dim_y]],
    stringsAsFactors = FALSE
  )
  group_vals <- get_group_values(meta, grouping_col)
  df$group <- as.factor(group_vals)
  df$tooltip <- build_tooltips(
    scores, meta, grouping_col, dim_x, dim_y
  )
  df$data_id <- paste0("diag_", seq_len(nrow(df)))
  df
}


diag_error_parser <- function(error_msg,
                              operation_name =
                                "LDA Diagnostics") {
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
    "2 LD axes",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Need at least 3 groups (2 LD axes)",
      " for assumption diagnostic plots."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
