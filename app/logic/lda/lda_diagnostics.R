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


#' Add assumption diagnostics overlay to an LD plot
#'
#' Overlays per-group covariance ellipses (solid),
#' pooled within-group covariance ellipses (dashed,
#' darker shade), and group mean markers onto an
#' existing LD scores ggplot.
#'
#' @param p A ggplot object (the LD scores plot)
#' @param scores Data frame of LD scores
#' @param groups Factor of group labels
#' @param dim_x Character, LD column for x-axis
#' @param dim_y Character, LD column for y-axis
#' @return The ggplot with diagnostics layers added
#' @export
add_diagnostics_overlay <- function(p, scores, groups,
                                     dim_x, dim_y) {
  groups <- as.factor(groups)

  scores_2d <- data.frame(
    x = scores[[dim_x]],
    y = scores[[dim_y]],
    group = groups
  )

  # stat_ellipse level for ~1.5 SD in bivariate normal
  # P(chi-sq(2) <= 1.5^2) = 1 - exp(-1.5^2/2) ≈ 0.6753
  ellipse_level <- 1 - exp(-1.5^2 / 2)

  # --- Per-group covariance ellipses (solid) ---
  group_counts <- table(scores_2d$group)
  valid_groups <- names(
    group_counts[group_counts >= 4]
  )

  if (length(valid_groups) > 0) {
    ellipse_data <- scores_2d[
      scores_2d$group %in% valid_groups, ,
      drop = FALSE
    ]
    skipped <- setdiff(
      levels(scores_2d$group), valid_groups
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
  pooled_vc <- compute_pooled_vc(
    scores_2d[, c("x", "y"), drop = FALSE],
    groups
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

  p
}


#' Add decision boundary overlay to an LD scores plot
#'
#' Creates a dense grid in the 2D LD space, back-projects
#' each grid point to original variable space via the
#' scaling matrix, predicts the class using the fitted
#' LDA model, and renders coloured tile regions plus
#' boundary contour lines underneath the scatter points.
#'
#' @param p A ggplot object (the LD scores plot)
#' @param lda_result Result list from run_lda() (needs
#'   $model, $scores, $columns, $scaling)
#' @param dim_x Character, LD column for x-axis
#' @param dim_y Character, LD column for y-axis
#' @param grid_n Integer, resolution per axis (default 150)
#' @return The ggplot with boundary layers added
#' @export
add_boundaries_overlay <- function(p, lda_result,
                                    dim_x, dim_y,
                                    grid_n = 150) {
  model <- lda_result$model
  scores <- lda_result$scores
  columns <- lda_result$columns
  is_mda <- identical(
    lda_result$analysis_type, "mda"
  )

  if (is_mda) {
    # MDA: build grid in original variable space, predict
    # classes there, then project to variate space.
    # ginv(coef()) does NOT correctly invert the MDA
    # variate projection, so back-projection is unreliable.
    orig_data <- lda_result$numeric_data
    if (is.null(orig_data)) {
      return(p) # cannot draw boundaries without data
    }

    # Build grid over each original variable's range
    col_ranges <- lapply(columns, function(col) {
      rng <- range(orig_data[[col]])
      pad <- diff(rng) * 0.05
      seq(rng[1] - pad, rng[2] + pad,
          length.out = grid_n)
    })
    names(col_ranges) <- columns
    orig_grid <- expand.grid(col_ranges)

    # Predict class in original space
    pred_class <- stats::predict(model, orig_grid)

    # Project grid points to variate space
    variates <- stats::predict(
      model, orig_grid, type = "variates"
    )
    if (is.null(variates) ||
        ncol(variates) < 2) {
      return(p) # cannot map to 2D variate space
    }
    variates_df <- as.data.frame(variates)
    colnames(variates_df) <- paste0(
      "LD", seq_len(ncol(variates_df))
    )

    grid_df <- data.frame(
      x = variates_df[[dim_x]],
      y = variates_df[[dim_y]]
    )
    grid_df$class <- pred_class
    grid_df$class_num <- as.numeric(grid_df$class)
  } else {
    # LDA: back-project from LD space to original space
    # (scaling from LDA is a true linear projection)
    x_range <- range(scores[[dim_x]])
    y_range <- range(scores[[dim_y]])
    x_pad <- diff(x_range) * 0.05
    y_pad <- diff(y_range) * 0.05

    x_seq <- seq(
      x_range[1] - x_pad, x_range[2] + x_pad,
      length.out = grid_n
    )
    y_seq <- seq(
      y_range[1] - y_pad, y_range[2] + y_pad,
      length.out = grid_n
    )
    grid_df <- expand.grid(x = x_seq, y = y_seq)

    ld_names <- colnames(scores)
    n_ld <- length(ld_names)
    grid_ld <- matrix(
      0, nrow = nrow(grid_df), ncol = n_ld
    )
    colnames(grid_ld) <- ld_names
    grid_ld[, dim_x] <- grid_df$x
    grid_ld[, dim_y] <- grid_df$y

    col_means <- colMeans(model$means)
    scaling <- as.matrix(lda_result$scaling)
    scaling_inv <- MASS::ginv(scaling)
    original_grid <- grid_ld %*% scaling_inv
    n_orig <- ncol(original_grid)
    if (length(col_means) == n_orig) {
      original_grid <- sweep(
        original_grid, 2, col_means, "+"
      )
    }
    original_grid <- as.data.frame(original_grid)
    colnames(original_grid) <- columns

    pred <- stats::predict(model, original_grid)
    pred_class <- pred$class
    grid_df$class <- pred_class
    grid_df$class_num <- as.numeric(grid_df$class)
  }

  group_levels <- lda_result$group_levels
  grid_df$class <- factor(
    grid_df$class, levels = group_levels
  )

  if (is_mda) {
    # MDA: projected grid is irregular in variate space,
    # so use small scatter points for decision regions
    p <- p +
      ggplot2$geom_point(
        data = grid_df,
        ggplot2$aes(
          x = x, y = y, colour = class
        ),
        size = 0.3,
        alpha = 0.15,
        shape = 15,
        inherit.aes = FALSE
      ) +
      ggplot2$scale_colour_discrete(guide = "none")
  } else {
    # LDA: regular grid — use tiles + boundary segments
    p <- p +
      ggplot2$geom_tile(
        data = grid_df,
        ggplot2$aes(
          x = x, y = y, fill = class
        ),
        alpha = 0.15,
        inherit.aes = FALSE
      )

    dx <- x_seq[2] - x_seq[1]
    dy <- y_seq[2] - y_seq[1]
    class_mat <- matrix(
      grid_df$class_num,
      nrow = grid_n, ncol = grid_n
    )

    seg_list <- vector("list", 2 * grid_n * grid_n)
    k <- 0L
    for (i in seq_len(grid_n)) {
      for (j in seq_len(grid_n)) {
        # Horizontal neighbour (right)
        if (i < grid_n &&
            class_mat[i, j] != class_mat[i + 1, j]) {
          k <- k + 1L
          mid_x <- x_seq[i] + dx / 2
          seg_list[[k]] <- data.frame(
            x = mid_x, xend = mid_x,
            y = y_seq[j] - dy / 2,
            yend = y_seq[j] + dy / 2
          )
        }
        # Vertical neighbour (above)
        if (j < grid_n &&
            class_mat[i, j] != class_mat[i, j + 1]) {
          k <- k + 1L
          mid_y <- y_seq[j] + dy / 2
          seg_list[[k]] <- data.frame(
            x = x_seq[i] - dx / 2,
            xend = x_seq[i] + dx / 2,
            y = mid_y, yend = mid_y
          )
        }
      }
    }

    if (k > 0) {
      seg_df <- do.call(rbind, seg_list[seq_len(k)])
      p <- p +
        ggplot2$geom_segment(
          data = seg_df,
          ggplot2$aes(
            x = x, xend = xend,
            y = y, yend = yend
          ),
          colour = "grey50",
          linewidth = 0.45,
          inherit.aes = FALSE
        )
    }
  }

  p
}


#' Compute 1D decision boundary for a 2-group LDA
#'
#' Returns the LD1 score at which the classification
#' switches between the two groups, accounting for
#' prior probabilities.
#'
#' @param lda_result Result list from run_lda()
#' @return Numeric scalar, the decision threshold on LD1
#' @export
compute_1d_boundary <- function(lda_result) {
  model <- lda_result$model
  scores <- lda_result$scores
  dim_x <- colnames(scores)[1]
  columns <- lda_result$columns
  is_mda <- identical(
    lda_result$analysis_type, "mda"
  )

  if (is_mda) {
    # MDA: build grid in original variable space, predict
    # classes, project to variate space, find transition
    # on LD1.
    orig_data <- lda_result$numeric_data
    if (is.null(orig_data)) {
      # Fallback: midpoint of group means in LD1
      groups <- as.factor(
        get_group_values(
          lda_result$meta,
          lda_result$grouping_col
        )
      )
      g_means <- tapply(
        scores[[dim_x]], groups, mean
      )
      return(mean(g_means))
    }

    # Build a 1D scan grid using the first original
    # variable, holding others at their mean
    grid_n <- 500
    col_ranges <- lapply(columns, function(col) {
      rng <- range(orig_data[[col]])
      pad <- diff(rng) * 0.05
      seq(rng[1] - pad, rng[2] + pad,
          length.out = grid_n)
    })
    names(col_ranges) <- columns
    orig_grid <- expand.grid(col_ranges)

    # Predict class in original space
    pred_class <- as.integer(
      stats::predict(model, orig_grid)
    )

    # Project to variate space to get LD1 values
    variates <- stats::predict(
      model, orig_grid, type = "variates"
    )
    if (is.null(variates) || ncol(variates) < 1) {
      groups <- as.factor(
        get_group_values(
          lda_result$meta,
          lda_result$grouping_col
        )
      )
      g_means <- tapply(
        scores[[dim_x]], groups, mean
      )
      return(mean(g_means))
    }

    ld1_vals <- variates[, 1]

    # Sort by LD1 and find transition
    ord <- order(ld1_vals)
    ld1_sorted <- ld1_vals[ord]
    pred_sorted <- pred_class[ord]
    transitions <- which(
      diff(pred_sorted) != 0
    )

    if (length(transitions) > 0) {
      idx <- transitions[1]
      boundary <- (ld1_sorted[idx] +
        ld1_sorted[idx + 1]) / 2
    } else {
      groups <- as.factor(
        get_group_values(
          lda_result$meta,
          lda_result$grouping_col
        )
      )
      g_means <- tapply(
        scores[[dim_x]], groups, mean
      )
      boundary <- mean(g_means)
    }

    return(boundary)
  }

  # LDA: analytic boundary
  # Project group means to LD1
  group_means_orig <- as.data.frame(model$means)
  scaling <- as.matrix(model$scaling)
  # Center each group mean by grand mean, project
  grand_mean <- colMeans(group_means_orig)
  centered <- sweep(
    as.matrix(group_means_orig), 2,
    grand_mean, "-"
  )
  ld_means <- centered %*% scaling
  m1 <- ld_means[1, 1]
  m2 <- ld_means[2, 1]

  # Midpoint adjusted for priors:
  # boundary = (m1 + m2)/2 - log(pi1/pi2) * sigma^2 / (m2 - m1)
  # For equal priors this is just the midpoint
  priors <- model$prior
  if (abs(m2 - m1) > 1e-10) {
    log_ratio <- log(priors[1] / priors[2])
    # Within-group variance on LD1
    groups <- as.factor(
      get_group_values(
        lda_result$meta, lda_result$grouping_col
      )
    )
    ld_scores <- scores[[dim_x]]
    pooled_var <- compute_pooled_vc(
      data.frame(x = ld_scores), groups
    )[1, 1]
    boundary <- (m1 + m2) / 2 -
      log_ratio * pooled_var / (m2 - m1)
  } else {
    boundary <- (m1 + m2) / 2
  }

  boundary
}


#' Add QDA decision boundary overlay to a plot
#'
#' Works in either LD space (companion LDA projection)
#' or original variable space. Builds a grid, predicts
#' class via the QDA model, and renders tile regions
#' plus boundary segments.
#'
#' @param p A ggplot object
#' @param qda_result Result list from run_qda() (needs
#'   $model, $columns; optionally $lda_model, $lda_scaling)
#' @param dim_x Character, column name for x-axis
#' @param dim_y Character, column name for y-axis
#' @param plot_data Data frame with x, y columns used
#'   in the scatter (to derive grid range)
#' @param axis_type Character, "ld" or "original"
#' @param grid_n Integer, resolution per axis (default 150)
#' @return The ggplot with boundary layers added
#' @export
add_qda_boundaries_overlay <- function(
    p, qda_result, dim_x, dim_y,
    plot_data, axis_type = "ld",
    grid_n = 150) {
  qda_model <- qda_result$model

  # Grid range with 5% padding
  x_range <- range(plot_data$x)
  y_range <- range(plot_data$y)
  x_pad <- diff(x_range) * 0.05
  y_pad <- diff(y_range) * 0.05

  x_seq <- seq(
    x_range[1] - x_pad, x_range[2] + x_pad,
    length.out = grid_n
  )
  y_seq <- seq(
    y_range[1] - y_pad, y_range[2] + y_pad,
    length.out = grid_n
  )
  grid_df <- expand.grid(x = x_seq, y = y_seq)

  columns <- qda_result$columns

  if (axis_type == "ld") {
    # Back-project from LD space to original space
    lda_model <- qda_result$lda_model
    scaling <- as.matrix(qda_result$lda_scaling)
    lda_scores <- qda_result$lda_scores
    ld_names <- colnames(lda_scores)
    n_ld <- length(ld_names)

    grid_ld <- matrix(
      0, nrow = nrow(grid_df), ncol = n_ld
    )
    colnames(grid_ld) <- ld_names
    grid_ld[, dim_x] <- grid_df$x
    grid_ld[, dim_y] <- grid_df$y

    col_means <- colMeans(lda_model$means)
    scaling_inv <- MASS::ginv(scaling)
    original_grid <- grid_ld %*% scaling_inv
    original_grid <- sweep(
      original_grid, 2, col_means, "+"
    )
    original_grid <- as.data.frame(original_grid)
    colnames(original_grid) <- columns
  } else {
    # Original variable space — predict directly
    # Fill all columns with their mean, override
    # the two selected axes
    col_means <- colMeans(
      qda_result$numeric_data,
      na.rm = TRUE
    )
    original_grid <- as.data.frame(
      matrix(
        rep(col_means, each = nrow(grid_df)),
        nrow = nrow(grid_df)
      )
    )
    colnames(original_grid) <- columns
    original_grid[[dim_x]] <- grid_df$x
    original_grid[[dim_y]] <- grid_df$y
  }

  # Predict class for each grid point using QDA
  pred <- stats::predict(qda_model, original_grid)
  grid_df$class <- pred$class
  grid_df$class_num <- as.numeric(grid_df$class)

  group_levels <- qda_result$group_levels
  grid_df$class <- factor(
    grid_df$class, levels = group_levels
  )

  # Tile layer (soft fill)
  p <- p +
    ggplot2$geom_tile(
      data = grid_df,
      ggplot2$aes(
        x = x, y = y, fill = class
      ),
      alpha = 0.15,
      inherit.aes = FALSE
    )

  # Boundary segments: find adjacent cells that differ
  dx <- x_seq[2] - x_seq[1]
  dy <- y_seq[2] - y_seq[1]
  class_mat <- matrix(
    grid_df$class_num,
    nrow = grid_n, ncol = grid_n
  )

  seg_list <- vector("list", 2 * grid_n * grid_n)
  k <- 0L
  for (i in seq_len(grid_n)) {
    for (j in seq_len(grid_n)) {
      if (i < grid_n &&
          class_mat[i, j] != class_mat[i + 1, j]) {
        k <- k + 1L
        mid_x <- x_seq[i] + dx / 2
        seg_list[[k]] <- data.frame(
          x = mid_x, xend = mid_x,
          y = y_seq[j] - dy / 2,
          yend = y_seq[j] + dy / 2
        )
      }
      if (j < grid_n &&
          class_mat[i, j] != class_mat[i, j + 1]) {
        k <- k + 1L
        mid_y <- y_seq[j] + dy / 2
        seg_list[[k]] <- data.frame(
          x = x_seq[i] - dx / 2,
          xend = x_seq[i] + dx / 2,
          y = mid_y, yend = mid_y
        )
      }
    }
  }

  if (k > 0) {
    seg_df <- do.call(rbind, seg_list[seq_len(k)])
    p <- p +
      ggplot2$geom_segment(
        data = seg_df,
        ggplot2$aes(
          x = x, xend = xend,
          y = y, yend = yend
        ),
        colour = "grey50",
        linewidth = 0.45,
        inherit.aes = FALSE
      )
  }

  p
}

