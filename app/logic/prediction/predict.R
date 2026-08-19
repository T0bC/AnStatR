box::use(
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/preprocessing/skewness_transform[apply_stored_transforms],
)

# =============================================================================
# Prediction logic: preprocess unknowns and run predict
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Preprocess unknown data using stored bundle params
#'
#' Applies stored skewness transforms and scaling params
#' to unknown data so it matches the training pipeline.
#' All analysis types apply the same transforms + manual
#' scale — PCA/sPCA/IPCA use manual projection rather than a
#' predict() S3 method, so their center/scale must be applied
#' here just like LDA/MDA/QDA.
#'
#' @param unknown_data Data frame of unknown observations
#' @param bundle The prediction bundle
#' @return Data frame with preprocessed numeric columns
#' @export
preprocess_unknown <- function(unknown_data, bundle) {
  numeric_cols <- bundle$numeric_cols
  result <- unknown_data

  # Step 1: Apply stored skewness transforms
  if (length(bundle$transform_params) > 0) {
    result <- apply_stored_transforms(
      result, bundle$transform_params
    )
    rhino$log$info(
      "Prediction: applied ",
      "{length(bundle$transform_params)}",
      " stored transforms"
    )
  }

  # Step 2: Apply stored scaling
  if (!is.null(bundle$scale_params)) {
    sp <- bundle$scale_params
    numeric_subset <- result[
      , numeric_cols, drop = FALSE
    ]

    if (!is.null(sp$center)) {
      for (col in numeric_cols) {
        if (col %in% names(sp$center)) {
          numeric_subset[[col]] <-
            numeric_subset[[col]] - sp$center[[col]]
        }
      }
    }

    if (!is.null(sp$scale)) {
      for (col in numeric_cols) {
        if (
          col %in% names(sp$scale) &&
          sp$scale[[col]] != 0
        ) {
          numeric_subset[[col]] <-
            numeric_subset[[col]] / sp$scale[[col]]
        }
      }
    }

    result[, numeric_cols] <- numeric_subset
    rhino$log$info(
      "Prediction: applied stored scaling"
    )
  }

  result
}

#' Run prediction on preprocessed unknown data
#'
#' Dispatches to the appropriate predict method based
#' on the bundle's analysis_type.
#'
#' @param bundle The prediction bundle
#' @param preprocessed_data Data frame (already
#'   preprocessed via preprocess_unknown)
#' @return List with $success, $result or $error.
#'   $result contains: predicted_class, posterior,
#'   scores (where applicable), n_unknowns
#' @export
predict_unknown <- function(bundle, preprocessed_data) {
  error_handling$safe_execute(
    expr = {
      model <- bundle$model
      numeric_cols <- bundle$numeric_cols
      numeric_data <- preprocessed_data[
        , numeric_cols, drop = FALSE
      ]
      analysis_type <- bundle$analysis_type

      rhino$log$info(
        "Prediction: running {toupper(analysis_type)}",
        " predict on {nrow(numeric_data)} unknowns"
      )

      result <- switch(
        analysis_type,
        pca = predict_pca(model, numeric_data),
        spca = predict_spca(model, numeric_data),
        ipca = predict_ipca(model, numeric_data),
        lda = predict_lda(model, numeric_data),
        mda = predict_mda(model, numeric_data),
        qda = predict_qda(
          model, numeric_data, bundle
        ),
        cluster = predict_cluster(
          bundle, numeric_data
        ),
        plsda = predict_plsda(model, numeric_data),
        splsda = predict_plsda(model, numeric_data),
        stop(paste0(
          "Unsupported analysis type: '",
          analysis_type, "'"
        ))
      )

      result$analysis_type <- analysis_type
      result$n_unknowns <- nrow(numeric_data)

      rhino$log$info(
        "Prediction: complete — ",
        "{nrow(numeric_data)} observations predicted"
      )

      result
    },
    operation_name = "Prediction",
    error_parser = prediction_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Project new samples onto a fitted PCA model
#'
#' mixOmics pca() objects have no predict() S3 method, so new
#' samples are projected via matrix multiplication against the
#' fitted loadings (mathematically equivalent to
#' stats::predict.prcomp for plain, non-sparse PCA — a single
#' matrix multiply reproduces $variates$X exactly, since
#' loadings are orthogonal SVD vectors with no deflation
#' between components). Data is already centered/scaled by
#' preprocess_unknown() using the bundle's stored
#' scale_params.
#'
#' @param model Fitted mixOmics pca object
#' @param numeric_data Data frame, already preprocessed
#'   (transforms + center/scale applied by
#'   preprocess_unknown())
#' @return List with $scores, $predicted_class (NULL),
#'   $posterior (NULL)
predict_pca <- function(model, numeric_data) {
  loadings <- model$loadings$X
  x_mat <- as.matrix(numeric_data)[, rownames(loadings), drop = FALSE]
  scores <- as.data.frame(x_mat %*% loadings)
  colnames(scores) <- paste0(
    "Dim.", seq_len(ncol(scores))
  )
  list(
    scores = scores,
    predicted_class = NULL,
    posterior = NULL
  )
}

#' Project new samples onto a fitted sPCA model
#'
#' Unlike plain PCA, a single matrix multiply against the
#' loadings does NOT reproduce sPCA's scores beyond the first
#' component: mixOmics fits sPCA one component at a time via
#' NIPALS-style power iteration, deflating the data matrix
#' after each component by regressing out that component's
#' score (X <- X - u %*% t(crossprod(X, u) / crossprod(u)))
#' before fitting the next. Projecting new data must replay
#' the same per-component deflation to match. Verified exact
#' (zero numerical difference) against mixOmics' own
#' $variates$X on both refit and held-out data.
#'
#' @param model Fitted mixOmics spca object
#' @param numeric_data Data frame, already preprocessed
#'   (transforms + center/scale applied by
#'   preprocess_unknown())
#' @return List with $scores, $predicted_class (NULL),
#'   $posterior (NULL)
predict_spca <- function(model, numeric_data) {
  rotation <- model$rotation
  ncomp <- ncol(rotation)
  x_temp <- as.matrix(numeric_data)[
    , rownames(rotation), drop = FALSE
  ]
  scores <- matrix(
    0, nrow(x_temp), ncomp,
    dimnames = list(rownames(x_temp), NULL)
  )
  for (h in seq_len(ncomp)) {
    loadings_h <- rotation[, h]
    u <- as.vector(x_temp %*% loadings_h)
    cvec <- crossprod(x_temp, u) / drop(crossprod(u))
    x_temp <- x_temp - u %*% t(cvec)
    scores[, h] <- u
  }
  scores <- as.data.frame(scores)
  colnames(scores) <- paste0("Dim.", seq_len(ncomp))
  list(
    scores = scores,
    predicted_class = NULL,
    posterior = NULL
  )
}

#' Project new samples onto a fitted IPCA model
#'
#' mixOmics computes IPCA's first score as X %*% rotation[,1]
#' (unit-normalized), then each subsequent component's score
#' as the residual of X %*% rotation[,h] after regressing out
#' all prior scores (via lsfit), unit-normalized again. This
#' recursive residualization — not the unmixing/mixing
#' matrices — is what must be replayed to project new data.
#' Verified exact (zero numerical difference) against
#' mixOmics' own $x on refit data. Data is centered (not
#' scaled) by preprocess_unknown(), matching ipca()'s default
#' scale = FALSE.
#'
#' @param model Fitted mixOmics ipca object
#' @param numeric_data Data frame, already preprocessed
#' @return List with $scores, $predicted_class (NULL),
#'   $posterior (NULL)
predict_ipca <- function(model, numeric_data) {
  rotation <- model$rotation
  ncomp <- ncol(rotation)
  x_mat <- as.matrix(numeric_data)[
    , rownames(rotation), drop = FALSE
  ]
  n <- nrow(x_mat)
  scores <- matrix(
    NA_real_, n, ncomp,
    dimnames = list(rownames(x_mat), NULL)
  )
  scores[, 1] <- as.vector(x_mat %*% rotation[, 1])
  scores[, 1] <- scores[, 1] / sqrt(sum(scores[, 1]^2))
  if (ncomp >= 2) {
    for (h in 2:ncomp) {
      target <- as.vector(x_mat %*% rotation[, h])
      resid <- stats$lsfit(
        y = target, x = scores[, seq_len(h - 1), drop = FALSE],
        intercept = FALSE
      )$residuals
      scores[, h] <- resid / sqrt(sum(resid^2))
    }
  }
  scores <- as.data.frame(scores)
  colnames(scores) <- paste0("Dim.", seq_len(ncomp))
  list(
    scores = scores,
    predicted_class = NULL,
    posterior = NULL
  )
}

predict_lda <- function(model, numeric_data) {
  pred <- stats$predict(model, numeric_data)
  list(
    predicted_class = pred$class,
    posterior = as.data.frame(pred$posterior),
    scores = if (!is.null(pred$x)) {
      as.data.frame(pred$x)
    } else {
      NULL
    }
  )
}

predict_mda <- function(model, numeric_data) {
  pred_class <- stats$predict(
    model, numeric_data
  )
  pred_post <- stats$predict(
    model, numeric_data, type = "posterior"
  )
  pred_scores <- stats$predict(
    model, numeric_data, type = "variates"
  )

  scores_df <- if (!is.null(pred_scores)) {
    df <- as.data.frame(pred_scores)
    if (ncol(df) > 0) {
      colnames(df) <- paste0(
        "LD", seq_len(ncol(df))
      )
    }
    df
  } else {
    NULL
  }

  list(
    predicted_class = pred_class,
    posterior = as.data.frame(pred_post),
    scores = scores_df
  )
}

predict_qda <- function(model, numeric_data, bundle) {
  pred <- stats$predict(model, numeric_data)

  result <- list(
    predicted_class = pred$class,
    posterior = as.data.frame(pred$posterior),
    scores = NULL
  )

  # Project through companion LDA for LD scores
  if (!is.null(bundle$lda_model)) {
    lda_pred <- stats$predict(
      bundle$lda_model, numeric_data
    )
    if (!is.null(lda_pred$x)) {
      result$scores <- as.data.frame(lda_pred$x)
    }
    rhino$log$info(
      "Prediction: QDA companion LDA projection done"
    )
  }

  result
}

predict_plsda <- function(model, numeric_data) {
  pred <- stats$predict(model, as.matrix(numeric_data))
  n_comp <- ncol(pred$variates)

  scores_df <- as.data.frame(pred$variates)
  if (ncol(scores_df) > 0) {
    colnames(scores_df) <- paste0(
      "Comp", seq_len(ncol(scores_df))
    )
  }

  list(
    predicted_class = pred$class$max.dist[, n_comp],
    posterior = as.data.frame(pred$predict[, , n_comp]),
    scores = scores_df
  )
}

predict_cluster <- function(bundle, numeric_data) {
  variant <- bundle$variant
  ref_points <- if (variant == "pam") {
    bundle$model$medoids
  } else {
    bundle$model$centers
  }

  metric <- bundle$cluster_metric %||% "euclidean"
  num_mat <- as.matrix(numeric_data)

  dist_fun <- if (metric == "manhattan") {
    function(x, y) sum(abs(x - y))
  } else {
    function(x, y) sqrt(sum((x - y)^2))
  }

  nearest_idx <- apply(num_mat, 1, function(row) {
    dists <- apply(ref_points, 1, function(ref) {
      dist_fun(row, ref)
    })
    which.min(dists)
  })

  list(
    predicted_class = factor(
      paste0("Cluster ", nearest_idx)
    ),
    posterior = NULL,
    scores = numeric_data
  )
}

prediction_error_parser <- function(
    error_msg,
    operation_name = "Prediction") {
  if (grepl(
    "subscript|column|variable",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data columns do not match the model.",
      " Verify that the unknown data has the",
      " same columns as the training data."
    )
  } else if (grepl(
    "singular|invertible|rank",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": The data matrix is singular.",
      " Check for constant or highly",
      " correlated columns."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
