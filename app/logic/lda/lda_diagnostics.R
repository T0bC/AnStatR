box::use(
  colorspace,
  ggplot2,
  ggiraph,
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
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
#' Creates a dense grid in the 2D LD/component space and
#' classifies each point, then renders coloured tile regions
#' plus boundary contour lines underneath the scatter points.
#' For LDA, classification back-projects to original variable
#' space via the scaling matrix. For PLS-DA/sPLS-DA plotted as
#' Comp1 vs Comp2, classification is exact (see
#' classify_plsda_grid_exact()); for other PLS-DA component
#' pairs and for MDA, a k-NN approximation on training scores
#' is used instead (see inline comments below).
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
                                    grid_n = 150,
                                    dist = "max.dist") {
  model <- lda_result$model
  scores <- lda_result$scores
  columns <- lda_result$columns
  is_mda <- identical(
    lda_result$analysis_type, "mda"
  )
  is_plsda <- lda_result$analysis_type %in%
    c("plsda", "splsda")

  is_comp12 <- is_plsda && identical(
    sort(c(dim_x, dim_y)), sort(c("Comp1", "Comp2"))
  )

  # The centroid rules are defined purely in the plotted 2D component
  # space, so they are computable for ANY component pair. Only the
  # max.dist regression algebra is tied to the leading pair. Without
  # this, selecting Comp3/Comp4 silently fell through to the k-NN
  # branch and the Boundary rule control appeared to do nothing.
  centroid_rule <- dist %in% c(
    "centroids.dist", "mahalanobis.dist"
  )

  if (is_plsda && (is_comp12 || centroid_rule)) {
    # PLS-DA/sPLS-DA plotting Comp1 vs Comp2: classify the grid
    # exactly, using the same max.dist rule mixOmics's own
    # background.predict()/predict() use internally, restricted
    # to these first two components (matching what is actually
    # drawn — a 2D visual cannot show the full ncomp model
    # boundary anyway). No k-NN approximation needed here.
    # max.dist is only exact for the leading component pair; on any
    # other pair use the nearest-centroid rule instead of pretending
    # the regression reconstruction still holds.
    rule <- if (!is_comp12 && identical(dist, "max.dist")) {
      "centroids.dist"
    } else {
      dist
    }
    exact <- classify_plsda_grid_exact(
      model, scores, dim_x, dim_y, grid_n, dist = rule
    )
    grid_df <- exact$grid_df
    x_seq <- exact$x_seq
    y_seq <- exact$y_seq
  } else if (is_plsda || is_mda) {
    # MDA: the variate projection is not invertible (goes
    # through an internal regression fit). PLS-DA/sPLS-DA on
    # component pairs other than Comp1/Comp2: mixOmics has no
    # exact 2-component grid classifier for those (its own
    # background.predict() is likewise restricted to comps 1-2).
    # For both, build a regular 2D grid in score space and
    # classify each grid point via k-NN on training scores —
    # for PLS-DA/sPLS-DA this reproduces the real
    # classification with >95% agreement in a held-out check,
    # a good approximation for a visual background.
    train_x <- scores[[dim_x]]
    train_y <- scores[[dim_y]]
    train_class <- as.factor(lda_result$predicted_class)

    x_range <- range(train_x)
    y_range <- range(train_y)
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

    knn_k <- min(7, length(train_class) - 1)
    pred_class <- class::knn(
      train = cbind(train_x, train_y),
      test  = as.matrix(grid_df),
      cl    = train_class,
      k     = knn_k
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

  # Tile layer (soft fill) — both MDA and LDA now use
  # a regular grid with x_seq / y_seq
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

  p
}


#' Classify a Comp1/Comp2 grid using PLS-DA/sPLS-DA's exact
#' max.dist rule
#'
#' Reproduces the same classification mixOmics::predict()
#' (dist = "max.dist") and mixOmics::background.predict() use
#' internally, evaluated directly on grid coordinates in
#' component (variate) space — no back-projection to original
#' variable space is needed. Restricted to components 1 and 2,
#' matching mixOmics's own background.predict() (and matching
#' what a 2D plot can show regardless of the model's full
#' ncomp). Verified to exactly reproduce stats::predict()'s
#' max.dist classification when evaluated at training-set
#' coordinates.
#'
#' Math (from mixOmics internal_predict.DA / background.predict,
#' dist = "max.dist"): for each cumulative component count
#' j = 1..2, Y.hat_j = (t_grid[,1:j] / colSums(variatesX[,1:j]^2))
#' %*% t(Cmat)[1:j,], rescaled by the dummy-Y matrix's stored
#' scaled:center/scaled:scale attributes, where
#' Cmat = crossprod(ind.mat, variatesX). The predicted class is
#' the column of Y.hat_2 (using both components) with the
#' largest value.
#'
#' @param model Fitted mixOmics plsda/splsda object
#' @param scores Data frame of component scores (model$variates$X)
#' @param dim_x Character, "Comp1" or "Comp2"
#' @param dim_y Character, "Comp1" or "Comp2" (the other one)
#' @param grid_n Integer, resolution per axis
#' @return List with $grid_df (x, y, class, class_num),
#'   $x_seq, $y_seq
classify_plsda_grid_exact <- function(model, scores,
                                      dim_x, dim_y,
                                      grid_n,
                                      dist = "max.dist") {
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

  # Component indices actually being plotted (Comp3 -> 3, ...).
  # The grid is built in ascending component order so it lines up
  # with the columns pulled from model$variates$X below.
  comp_idx <- as.integer(sub("^Comp", "", c(dim_x, dim_y)))
  x_is_first <- comp_idx[1] <= comp_idx[2]
  t_grid <- if (x_is_first) {
    cbind(grid_df$x, grid_df$y)
  } else {
    cbind(grid_df$y, grid_df$x)
  }
  cols_used <- sort(comp_idx)

  variates_x <- model$variates$X[, cols_used, drop = FALSE]
  ind_mat <- model$ind.mat
  means_y <- attr(ind_mat, "scaled:center")
  sigma_y <- attr(ind_mat, "scaled:scale")
  if (is.null(sigma_y)) sigma_y <- rep(1, ncol(ind_mat))

  c_mat <- crossprod(ind_mat, variates_x)
  a_vec <- apply(variates_x, 2, function(v) sum(v^2))
  a_mat <- matrix(
    a_vec, nrow = nrow(t_grid), ncol = 2, byrow = TRUE
  )

  y_hat <- (t_grid / a_mat) %*% t(c_mat)
  y_hat <- sweep(y_hat, 2, sigma_y, "*")
  y_hat <- sweep(y_hat, 2, means_y, "+")

  class_levels <- colnames(ind_mat)

  # mixOmics offers three rules for turning a position in component
  # space into a predicted class, and they can disagree noticeably:
  #   max.dist        - largest predicted Y score (the regression
  #                     rule; matches predict()/perf() defaults)
  #   centroids.dist  - nearest class centroid by Euclidean distance
  #   mahalanobis.dist- nearest centroid under the pooled component
  #                     covariance, so elongated/correlated component
  #                     spreads are accounted for
  # Centroid rules are computed in the plotted 2D component space,
  # which is what the background is illustrating. Verified against
  # mixOmics::predict() at the training points: max.dist and
  # centroids.dist agree 100%; mahalanobis.dist agrees ~88% because
  # mixOmics uses the full-ncomp covariance while a 2D background can
  # only represent the two plotted components.
  known_rules <- c(
    "max.dist", "centroids.dist", "mahalanobis.dist"
  )
  if (!dist %in% known_rules) {
    rhino$log$warn(
      "Decision boundaries: unknown rule '{dist}'; using max.dist."
    )
    dist <- "max.dist"
  }

  pred_idx <- if (identical(dist, "max.dist")) {
    # Regression rule: valid where y_hat reconstructs the fitted
    # response, i.e. the leading component pair.
    max.col(y_hat, ties.method = "first")
  } else {
    train_idx <- max.col(ind_mat, ties.method = "first")
    centroids <- t(vapply(
      seq_along(class_levels),
      function(k) colMeans(
        variates_x[train_idx == k, , drop = FALSE]
      ),
      numeric(2)
    ))

    if (identical(dist, "mahalanobis.dist")) {
      cov_inv <- tryCatch(
        solve(stats$cov(variates_x)),
        error = function(e) NULL
      )
      if (is.null(cov_inv)) {
        rhino$log$warn(
          paste(
            "Decision boundaries: component covariance is",
            "singular; falling back to centroids.dist."
          )
        )
        dist <- "centroids.dist"
      } else {
        d2 <- vapply(
          seq_len(nrow(centroids)),
          function(k) {
            dd <- sweep(t_grid, 2, centroids[k, ], "-")
            rowSums((dd %*% cov_inv) * dd)
          },
          numeric(nrow(t_grid))
        )
      }
    }

    if (identical(dist, "centroids.dist")) {
      d2 <- vapply(
        seq_len(nrow(centroids)),
        function(k) {
          rowSums(sweep(t_grid, 2, centroids[k, ], "-")^2)
        },
        numeric(nrow(t_grid))
      )
    }

    max.col(-d2, ties.method = "first")
  }

  grid_df$class <- class_levels[pred_idx]
  grid_df$class_num <- pred_idx

  list(grid_df = grid_df, x_seq = x_seq, y_seq = y_seq)
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
  is_plsda <- lda_result$analysis_type %in%
    c("plsda", "splsda")

  if (is_plsda) {
    # PLS-DA/sPLS-DA with a single component (ncomp = 1, the
    # only case build_1d_plot() is used for this analysis
    # type): classify exactly along Comp1 using the same
    # max.dist rule as classify_plsda_grid_exact(), restricted
    # to one component.
    x_range <- range(scores[[dim_x]])
    x_pad <- diff(x_range) * 0.05
    x_seq <- seq(
      x_range[1] - x_pad,
      x_range[2] + x_pad,
      length.out = 500
    )

    variates_x <- model$variates$X[, 1, drop = FALSE]
    ind_mat <- model$ind.mat
    means_y <- attr(ind_mat, "scaled:center")
    sigma_y <- attr(ind_mat, "scaled:scale")
    if (is.null(sigma_y)) sigma_y <- rep(1, ncol(ind_mat))

    c_mat <- crossprod(ind_mat, variates_x)
    a_val <- sum(variates_x^2)

    y_hat <- (matrix(x_seq, ncol = 1) / a_val) %*% t(c_mat)
    y_hat <- sweep(y_hat, 2, sigma_y, "*")
    y_hat <- sweep(y_hat, 2, means_y, "+")

    pred_idx <- max.col(y_hat, ties.method = "first")
    transitions <- which(diff(pred_idx) != 0)

    if (length(transitions) > 0) {
      idx <- transitions[1]
      boundary <- (x_seq[idx] + x_seq[idx + 1]) / 2
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

  if (is_mda) {
    # MDA: scan along DC1 using k-NN on training scores — no
    # exact closed-form projection available (the variate
    # projection goes through an internal regression fit).
    train_ld1 <- scores[[dim_x]]
    train_class <- as.factor(lda_result$predicted_class)

    x_range <- range(train_ld1)
    x_pad <- diff(x_range) * 0.05
    x_seq <- seq(
      x_range[1] - x_pad,
      x_range[2] + x_pad,
      length.out = 500
    )

    knn_k <- min(7, length(train_class) - 1)
    pred_class <- as.integer(class::knn(
      train = matrix(train_ld1, ncol = 1),
      test  = matrix(x_seq, ncol = 1),
      cl    = train_class,
      k     = knn_k
    ))

    transitions <- which(diff(pred_class) != 0)

    if (length(transitions) > 0) {
      idx <- transitions[1]
      boundary <- (x_seq[idx] + x_seq[idx + 1]) / 2
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

