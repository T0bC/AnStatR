box::use(
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Prediction diagnostics: quantitative confidence/distance metrics for
# unknown-sample predictions (T2/Q for PCA family, Mahalanobis/typicality
# for classifiers, distance-ratio for cluster assignment).
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute per-sample prediction diagnostics
#'
#' Dispatches to the appropriate diagnostic routine based on
#' bundle$analysis_type. Returns one row per unknown sample,
#' aligned with prediction_result$scores / $predicted_class
#' row order. A diagnostics failure (e.g. singular training
#' covariance) must never block the underlying prediction from
#' displaying -- callers should treat !success as "hide the
#' diagnostics panel", not as a hard error.
#'
#' @param bundle The prediction bundle (as loaded by load_bundle())
#' @param preprocessed_data Data frame, already preprocessed via
#'   preprocess_unknown() (transforms + center/scale applied)
#' @param prediction_result The $result from predict_unknown()
#'   ($scores, $predicted_class, $posterior, $analysis_type)
#' @return List with $success, $result (data frame, one row per
#'   unknown sample, or NULL if the bundle predates this feature)
#'   or $error
#' @export
compute_diagnostics <- function(bundle, preprocessed_data, prediction_result) {
  error_handling$safe_execute(
    expr = {
      analysis_type <- bundle$analysis_type
      switch(
        analysis_type,
        pca = ,
        spca = ,
        ipca = diagnostics_pca(bundle, preprocessed_data, prediction_result),
        lda = ,
        mda = ,
        qda = diagnostics_original_space(
          bundle, preprocessed_data, prediction_result
        ),
        plsda = ,
        splsda = diagnostics_component_space(bundle, prediction_result),
        cluster = diagnostics_cluster(bundle, prediction_result),
        stop(paste0(
          "Unsupported analysis type for diagnostics: '",
          analysis_type, "'"
        ))
      )
    },
    operation_name = "Prediction Diagnostics",
    error_parser = diagnostics_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Compute Hotelling's T2 and Q-residual for unknown PCA/sPCA/IPCA samples
#'
#' @param bundle Prediction bundle; must have $t2_q_ref (see pca_export.R's
#'   build_t2_q_reference()) with $score_cov_inv (k x k), $n_train, $k,
#'   $loadings (p x k), $q_threshold (95th percentile of training Q
#'   residuals)
#' @param preprocessed_data Data frame, already preprocessed (center/scale
#'   applied) -- used for Q-residual's reconstruction-vs-original comparison
#' @param prediction_result $result from predict_unknown() (uses $scores)
#' @return Data frame: T2, T2_p_value, T2_flag, Q_residual, Q_threshold,
#'   Q_flag -- one row per unknown sample, or NULL if the bundle predates
#'   this feature (no $t2_q_ref stored) or scores are unavailable
diagnostics_pca <- function(bundle, preprocessed_data, prediction_result) {
  ref <- bundle$t2_q_ref
  if (is.null(ref) || is.null(prediction_result$scores)) return(NULL)

  new_scores <- as.matrix(prediction_result$scores)
  t2 <- apply(new_scores, 1, function(row) {
    as.numeric(t(row) %*% ref$score_cov_inv %*% row)
  })
  f_stat <- t2 * (ref$n_train - ref$k) / (ref$k * (ref$n_train - 1))
  t2_p <- stats$pf(
    f_stat, df1 = ref$k, df2 = ref$n_train - ref$k, lower.tail = FALSE
  )

  new_x <- as.matrix(
    preprocessed_data[, rownames(ref$loadings), drop = FALSE]
  )
  recon <- new_scores %*% t(ref$loadings)
  resid <- new_x - recon
  q <- rowSums(resid^2)

  data.frame(
    T2 = t2,
    T2_p_value = t2_p,
    T2_flag = t2_p < 0.05,
    Q_residual = q,
    Q_threshold = ref$q_threshold,
    Q_flag = q > ref$q_threshold
  )
}

#' Compute Mahalanobis distance (original space) and typicality probability
#' for LDA/QDA/MDA predictions
#'
#' Uses true Mahalanobis distance in original measurement space, with
#' per-group covariance and per-group mean recomputed once at bundle-export
#' time (see lda_export.R's build_group_stats()). The LD-space-Euclidean
#' shortcut was checked numerically against true Mahalanobis distance and
#' does not hold (correlation 0.81, non-constant ratio) -- see plan notes.
#'
#' @param bundle Bundle with $group_stats (list of per-group $mean,
#'   $cov_inv)
#' @param preprocessed_data Preprocessed unknown data (center/scale
#'   applied) -- Mahalanobis distance is computed in this same space,
#'   consistent with how the bundle's $group_stats were built
#' @param prediction_result $result from predict_unknown() -- uses
#'   $predicted_class to label which distance is "to predicted group"
#' @return Data frame: Mahalanobis_to_predicted, Nearest_group,
#'   Mahalanobis_to_nearest, Typicality_p -- one row per unknown,
#'   or NULL if bundle predates this feature
diagnostics_original_space <- function(bundle, preprocessed_data, prediction_result) {
  group_stats <- bundle$group_stats
  if (is.null(group_stats) || length(group_stats) == 0) return(NULL)

  numeric_cols <- bundle$numeric_cols
  x_mat <- as.matrix(preprocessed_data[, numeric_cols, drop = FALSE])
  p <- ncol(x_mat)
  group_names <- names(group_stats)

  dist_mat <- vapply(group_stats, function(gs) {
    apply(x_mat, 1, function(row) {
      diff <- row - gs$mean
      sqrt(as.numeric(t(diff) %*% gs$cov_inv %*% diff))
    })
  }, numeric(nrow(x_mat)))
  # vapply collapses to a plain vector (not a matrix) when there is
  # exactly one unknown sample -- force back to n_unknown x n_groups.
  dist_mat <- matrix(
    dist_mat, nrow = nrow(x_mat), dimnames = list(NULL, group_names)
  )

  nearest_idx <- apply(dist_mat, 1, which.min)
  nearest_group <- group_names[nearest_idx]
  nearest_dist <- dist_mat[cbind(seq_len(nrow(dist_mat)), nearest_idx)]

  pred_class_chr <- as.character(prediction_result$predicted_class)
  pred_idx <- match(pred_class_chr, group_names)
  pred_dist <- dist_mat[cbind(seq_len(nrow(dist_mat)), pred_idx)]

  typicality_p <- stats$pchisq(nearest_dist^2, df = p, lower.tail = FALSE)

  data.frame(
    Mahalanobis_to_predicted = pred_dist,
    Nearest_group = nearest_group,
    Mahalanobis_to_nearest = nearest_dist,
    Typicality_p = typicality_p,
    stringsAsFactors = FALSE
  )
}

#' Compute Mahalanobis distance and typicality probability in PLS-DA/
#' sPLS-DA component space
#'
#' PLS-DA's classification decision genuinely lives in latent-variable
#' (model$variates$X) space, not original measurement space, so unlike
#' LDA/QDA/MDA, this reduced-space computation is the correct native
#' geometry for this method rather than a shortcut.
#'
#' @param bundle Bundle with $group_component_stats (list of per-group
#'   $mean, $cov_inv in component-score space)
#' @param prediction_result $result from predict_unknown() -- uses
#'   $scores (component scores) and $predicted_class
#' @return Data frame: same shape as diagnostics_original_space(), or
#'   NULL if bundle predates this feature
diagnostics_component_space <- function(bundle, prediction_result) {
  group_stats <- bundle$group_component_stats
  if (is.null(group_stats) || is.null(prediction_result$scores)) return(NULL)

  scores <- as.matrix(prediction_result$scores)
  k <- ncol(scores)
  group_names <- names(group_stats)

  dist_mat <- vapply(group_stats, function(gs) {
    apply(scores, 1, function(row) {
      diff <- row - gs$mean
      sqrt(as.numeric(t(diff) %*% gs$cov_inv %*% diff))
    })
  }, numeric(nrow(scores)))
  # vapply collapses to a plain vector (not a matrix) when there is
  # exactly one unknown sample -- force back to n_unknown x n_groups.
  dist_mat <- matrix(
    dist_mat, nrow = nrow(scores), dimnames = list(NULL, group_names)
  )

  nearest_idx <- apply(dist_mat, 1, which.min)
  nearest_group <- group_names[nearest_idx]
  nearest_dist <- dist_mat[cbind(seq_len(nrow(dist_mat)), nearest_idx)]

  pred_class_chr <- as.character(prediction_result$predicted_class)
  pred_idx <- match(pred_class_chr, group_names)
  pred_dist <- dist_mat[cbind(seq_len(nrow(dist_mat)), pred_idx)]

  typicality_p <- stats$pchisq(nearest_dist^2, df = k, lower.tail = FALSE)

  data.frame(
    Mahalanobis_to_predicted = pred_dist,
    Nearest_group = nearest_group,
    Mahalanobis_to_nearest = nearest_dist,
    Typicality_p = typicality_p,
    stringsAsFactors = FALSE
  )
}

#' Compute distance-ratio confidence proxy for cluster assignment
#'
#' Out-of-sample proxy for silhouette width: ratio of the distance to
#' the assigned (nearest) centroid/medoid vs. the second-nearest. Near
#' 0 means the sample sits much closer to its assigned cluster than any
#' alternative (confident); near 1 means the sample is roughly
#' equidistant between two clusters (ambiguous -- analogous to a
#' near-zero silhouette width). At an exact tie, the ratio is exactly 1
#' (both distances equal) -- this is the correct "maximally ambiguous"
#' signal, not an error case.
#'
#' Recomputes the full distance matrix independently rather than reusing
#' predict_cluster() (which discards all but the minimum distance) -- this
#' keeps predict.R's return contract untouched across every analysis type.
#'
#' @param bundle Bundle with $model ($centers or $medoids per $variant),
#'   $cluster_metric
#' @param prediction_result $result from predict_unknown() -- cluster's
#'   $scores IS the preprocessed numeric_data (per predict_cluster's own
#'   return shape), so no separate preprocessed_data arg is needed
#' @return Data frame: Dist_to_assigned, Dist_to_second_nearest,
#'   Distance_ratio (always in [0, 1] by construction)
diagnostics_cluster <- function(bundle, prediction_result) {
  variant <- bundle$variant
  ref_points <- if (variant == "pam") bundle$model$medoids else bundle$model$centers
  metric <- bundle$cluster_metric %||% "euclidean"
  num_mat <- as.matrix(prediction_result$scores)

  dist_fun <- if (metric == "manhattan") {
    function(x, y) sum(abs(x - y))
  } else {
    function(x, y) sqrt(sum((x - y)^2))
  }

  row_stats <- t(apply(num_mat, 1, function(row) {
    dists <- unname(apply(ref_points, 1, function(ref) dist_fun(row, ref)))
    sorted <- sort(dists)
    d1 <- sorted[1]
    d2 <- if (length(sorted) >= 2) sorted[2] else NA_real_
    ratio <- if (!is.na(d2) && d2 > 0) d1 / d2 else NA_real_
    # unname() above prevents ref_points' rownames from leaking into
    # these scalars, which would otherwise collide with the c(d1=...)
    # names below (e.g. "d1.1" instead of "d1") when ref_points has
    # rownames -- as centers/medoids from kmeans()/pam() do.
    c(d1 = d1, d2 = d2, ratio = ratio)
  }))

  data.frame(
    Dist_to_assigned = row_stats[, "d1"],
    Dist_to_second_nearest = row_stats[, "d2"],
    Distance_ratio = row_stats[, "ratio"]
  )
}

diagnostics_error_parser <- function(error_msg, operation_name = "Prediction Diagnostics") {
  if (grepl("singular|solve|invertible", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Could not compute distance-based diagnostics -- the training",
      " covariance is singular (too few training observations relative",
      " to the number of variables/components)."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
