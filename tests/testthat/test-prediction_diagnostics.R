box::use(
  app/logic/pca/pca[run_pca],
  app/logic/pca/pca_export[create_pca_bundle],
  app/logic/prediction/diagnostics[compute_diagnostics],
  app/logic/prediction/predict[preprocess_unknown, predict_unknown],
)

# =============================================================================
# Tests for prediction diagnostics (T2/Q, Mahalanobis/typicality,
# distance-ratio). Fixture factories are duplicated (not shared) from
# test-prediction_predict.R per this project's one-file-per-script test
# isolation, and extended here to also populate group_stats/
# group_component_stats/confusion/confusion_source since the diagnostics
# functions read those bundle fields.
# =============================================================================

# --- Helper: build iris-based bundles for each type ---

make_iris_data <- function() {
  data <- iris[1:120, ]
  numeric_cols <- c(
    "Sepal.Length", "Sepal.Width",
    "Petal.Length", "Petal.Width"
  )
  list(data = data, numeric_cols = numeric_cols)
}

make_pca_bundle <- function(analysis_type = "pca",
                            keep_x = NULL,
                            ipca_mode = "deflation") {
  d <- make_iris_data()

  res <- run_pca(
    d$data, d$numeric_cols,
    meta_cols = "Species",
    center = TRUE, scale. = TRUE,
    ncp = if (analysis_type == "ipca") 3 else NULL,
    analysis_type = analysis_type,
    keep_x = keep_x, ipca_mode = ipca_mode
  )
  stopifnot(res$success)

  create_pca_bundle(
    res$result,
    raw_data = d$data,
    used_data = d$data,
    numeric_cols = d$numeric_cols,
    meta_cols = "Species",
    settings = list(
      skewness_correction = FALSE,
      scale_method = "scale_center"
    )
  )
}

make_lda_bundle <- function() {
  d <- make_iris_data()
  numeric_data <- d$data[, d$numeric_cols, drop = FALSE]
  lda_obj <- MASS::lda(
    Species ~ .,
    data = cbind(numeric_data, Species = d$data$Species)
  )
  resub_pred <- stats::predict(lda_obj, numeric_data)
  confusion <- build_confusion_stats_helper(
    d$data$Species, resub_pred$class
  )

  bundle <- list(
    analysis_type = "lda",
    model = lda_obj,
    raw_data = d$data,
    used_data = d$data,
    group_col = "Species",
    numeric_cols = d$numeric_cols,
    meta_cols = character(0),
    transform_params = list(),
    scale_params = NULL,
    settings = list(
      skewness_correction = FALSE,
      scale_method = "none",
      prior = "proportional"
    ),
    data_source = "raw",
    app_version = "2.0.0",
    created = Sys.time()
  )
  bundle$group_stats <- build_group_stats_helper(
    d$data, d$numeric_cols, "Species"
  )
  bundle$confusion <- confusion
  bundle$confusion_source <- "resubstitution"
  bundle
}

make_qda_bundle <- function() {
  d <- make_iris_data()
  numeric_data <- d$data[, d$numeric_cols, drop = FALSE]
  qda_obj <- MASS::qda(
    Species ~ .,
    data = cbind(numeric_data, Species = d$data$Species)
  )

  # Companion LDA
  lda_obj <- MASS::lda(
    Species ~ .,
    data = cbind(numeric_data, Species = d$data$Species)
  )
  lda_pred <- stats::predict(lda_obj, numeric_data)

  resub_pred <- stats::predict(qda_obj, numeric_data)
  confusion <- build_confusion_stats_helper(
    d$data$Species, resub_pred$class
  )

  bundle <- list(
    analysis_type = "qda",
    model = qda_obj,
    raw_data = d$data,
    used_data = d$data,
    group_col = "Species",
    numeric_cols = d$numeric_cols,
    meta_cols = character(0),
    transform_params = list(),
    scale_params = NULL,
    settings = list(
      skewness_correction = FALSE,
      scale_method = "none",
      prior = "proportional"
    ),
    data_source = "raw",
    lda_model = lda_obj,
    lda_scaling = as.data.frame(lda_obj$scaling),
    lda_svd = lda_obj$svd,
    lda_scores = as.data.frame(lda_pred$x),
    lda_proportion_of_trace = NULL,
    app_version = "2.0.0",
    created = Sys.time()
  )
  bundle$group_stats <- build_group_stats_helper(
    d$data, d$numeric_cols, "Species"
  )
  bundle$confusion <- confusion
  bundle$confusion_source <- "resubstitution"
  bundle
}

make_plsda_bundle <- function(sparse = FALSE) {
  d <- make_iris_data()
  numeric_data <- d$data[, d$numeric_cols, drop = FALSE]
  x_mat <- as.matrix(numeric_data)
  y <- d$data$Species

  model <- if (sparse) {
    mixOmics::splsda(
      x_mat, y, ncomp = 2,
      keepX = c(2, 2), scale = TRUE
    )
  } else {
    mixOmics::plsda(x_mat, y, ncomp = 2, scale = TRUE)
  }

  scores <- as.data.frame(model$variates$X)
  colnames(scores) <- paste0("Comp", seq_len(ncol(scores)))

  list(
    analysis_type = if (sparse) "splsda" else "plsda",
    model = model,
    raw_data = d$data,
    used_data = d$data,
    group_col = "Species",
    numeric_cols = d$numeric_cols,
    meta_cols = character(0),
    transform_params = list(),
    scale_params = NULL,
    settings = list(
      skewness_correction = FALSE,
      scale_method = "none",
      ncomp = 2,
      sparse = sparse
    ),
    data_source = "raw",
    app_version = "2.0.0",
    created = Sys.time(),
    group_component_stats = build_group_component_stats_helper(scores, y)
  )
}

make_cluster_bundle <- function(variant = "kmeans") {
  d <- make_iris_data()
  numeric_data <- d$data[, d$numeric_cols, drop = FALSE]

  if (variant == "pam") {
    model <- cluster::pam(
      numeric_data, k = 3, metric = "manhattan"
    )
    cluster_labels <- model$clustering
    metric <- "manhattan"
  } else {
    model <- stats::kmeans(
      numeric_data, centers = 3, nstart = 10
    )
    cluster_labels <- model$cluster
    metric <- "euclidean"
  }

  list(
    analysis_type = "cluster",
    variant = variant,
    model = model,
    raw_data = d$data,
    used_data = d$data,
    group_col = NULL,
    numeric_cols = d$numeric_cols,
    meta_cols = "Species",
    transform_params = list(),
    scale_params = NULL,
    cluster_metric = metric,
    n_clusters = 3,
    cluster_labels = as.integer(cluster_labels),
    settings = list(
      algorithm = "kmeans",
      metric = metric,
      n_clusters = 3
    ),
    data_source = "raw",
    app_version = "2.0.0",
    created = Sys.time()
  )
}

# Two well-separated blobs, for unambiguous round-trip checks
make_blob_cluster_bundle <- function(variant = "kmeans") {
  set.seed(42)
  n <- 30
  data <- data.frame(
    a = c(rnorm(n, 0, 0.5), rnorm(n, 10, 0.5)),
    b = c(rnorm(n, 0, 0.5), rnorm(n, 10, 0.5))
  )
  numeric_cols <- c("a", "b")

  if (variant == "pam") {
    model <- cluster::pam(
      data[, numeric_cols], k = 2, metric = "manhattan"
    )
    cluster_labels <- model$clustering
    metric <- "manhattan"
  } else {
    model <- stats::kmeans(
      data[, numeric_cols], centers = 2, nstart = 10
    )
    cluster_labels <- model$cluster
    metric <- "euclidean"
  }

  list(
    bundle = list(
      analysis_type = "cluster",
      variant = variant,
      model = model,
      raw_data = data,
      used_data = data,
      group_col = NULL,
      numeric_cols = numeric_cols,
      meta_cols = character(0),
      transform_params = list(),
      scale_params = NULL,
      cluster_metric = metric,
      n_clusters = 2,
      cluster_labels = as.integer(cluster_labels),
      settings = list(
        algorithm = "kmeans",
        metric = metric,
        n_clusters = 2
      ),
      data_source = "raw",
      app_version = "2.0.0",
      created = Sys.time()
    ),
    data = data
  )
}

# --- Local re-implementations of internal lda_export.R helpers ---
# (build_group_stats and build_confusion_stats are not exported;
# duplicated here rather than reaching into another module's namespace,
# consistent with this project's test isolation.)

build_group_stats_helper <- function(used_data, numeric_cols, group_col) {
  x <- as.matrix(used_data[, numeric_cols, drop = FALSE])
  groups <- used_data[[group_col]]
  group_names <- if (is.factor(groups)) {
    levels(groups)
  } else {
    sort(unique(as.character(groups)))
  }

  stats_list <- lapply(group_names, function(g) {
    idx <- as.character(groups) == g
    xg <- x[idx, , drop = FALSE]
    cov_g <- stats::cov(xg)
    list(mean = colMeans(xg), cov_inv = solve(cov_g))
  })
  names(stats_list) <- group_names
  stats_list
}

build_group_component_stats_helper <- function(scores, groups) {
  x <- as.matrix(scores)
  group_names <- sort(unique(as.character(groups)))

  stats_list <- lapply(group_names, function(g) {
    idx <- as.character(groups) == g
    xg <- x[idx, , drop = FALSE]
    cov_g <- stats::cov(xg)
    list(mean = colMeans(xg), cov_inv = solve(cov_g))
  })
  names(stats_list) <- group_names
  stats_list
}

build_confusion_stats_helper <- function(true_labels, predicted_labels) {
  cm <- table(True = true_labels, Predicted = predicted_labels)
  correct <- sum(diag(cm))
  total <- sum(cm)
  list(
    matrix = cm,
    accuracy = correct / total,
    per_class = data.frame(Class = rownames(cm))
  )
}

# --- T2/Q diagnostics: PCA / sPCA / IPCA ---

test_that("diagnostics_pca: training data reproduces small T2 and Q", {
  bundle <- make_pca_bundle("pca")
  train_data <- bundle$used_data
  preprocessed <- preprocess_unknown(train_data, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result
  expect_false(is.null(diag))
  expect_equal(nrow(diag), nrow(train_data))

  train_t2_95 <- stats::quantile(diag$T2, 0.95)
  expect_true(mean(diag$T2 <= train_t2_95) >= 0.90)
  expect_true(mean(diag$Q_residual) < 1e-6)
})

test_that("diagnostics_pca flags a synthetic far-outlier row", {
  bundle <- make_pca_bundle("pca")
  outlier <- bundle$used_data[1, , drop = FALSE]
  outlier[, bundle$numeric_cols] <- outlier[, bundle$numeric_cols] * 5

  preprocessed <- preprocess_unknown(outlier, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_true(isTRUE(diag$T2_flag[1]) || isTRUE(diag$Q_flag[1]))
})

test_that("diagnostics_pca returns NULL when bundle predates t2_q_ref", {
  bundle <- make_pca_bundle("pca")
  bundle$t2_q_ref <- NULL

  unknown <- bundle$used_data[1:5, ]
  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  expect_null(diag_res$result)
})

test_that("diagnostics_pca works for sPCA training data", {
  # Unlike plain PCA, sPCA's sparse loadings do not span the full variable
  # space even when all components are retained, so training Q-residuals
  # are not near-zero -- the correct training-set invariant is instead
  # that ~95% of training points fall at/below the fitted 95th-percentile
  # Q threshold (that is the threshold's own definition).
  bundle <- make_pca_bundle("spca", keep_x = c(2, 2, 2, 2))
  train_data <- bundle$used_data
  preprocessed <- preprocess_unknown(train_data, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result
  expect_false(is.null(diag))
  expect_true(mean(diag$Q_residual <= diag$Q_threshold) >= 0.90)
})

test_that("diagnostics_pca works for IPCA training data", {
  # Same rationale as sPCA above -- IPCA's components are not
  # variance-ranked and do not guarantee near-zero training residuals.
  bundle <- make_pca_bundle("ipca")
  train_data <- bundle$used_data
  preprocessed <- preprocess_unknown(train_data, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result
  expect_false(is.null(diag))
  expect_true(mean(diag$Q_residual <= diag$Q_threshold) >= 0.90)
})

# --- Mahalanobis/typicality: LDA / QDA / MDA ---

test_that("diagnostics_original_space: point at group mean has ~0 Mahalanobis", {
  bundle <- make_lda_bundle()
  mean_row <- bundle$group_stats[["setosa"]]$mean
  unknown <- as.data.frame(
    matrix(
      mean_row, nrow = 1,
      dimnames = list(NULL, bundle$numeric_cols)
    )
  )

  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_true(diag$Mahalanobis_to_nearest[1] < 1e-6)
  expect_equal(diag$Nearest_group[1], "setosa")
})

test_that("diagnostics_original_space: typicality_p bounded in [0, 1]", {
  bundle <- make_lda_bundle()
  unknown <- iris[121:150, bundle$numeric_cols]

  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_true(all(diag$Typicality_p >= 0 & diag$Typicality_p <= 1))
})

test_that("diagnostics_original_space flags an unrepresented-group despite a forced class", {
  bundle <- make_lda_bundle()
  outlier <- bundle$used_data[1, bundle$numeric_cols, drop = FALSE]
  # Push several SDs outside the Iris range on every measurement column
  sds <- vapply(bundle$used_data[, bundle$numeric_cols], stats::sd, numeric(1))
  outlier <- as.data.frame(
    matrix(
      unlist(outlier) + 10 * sds, nrow = 1,
      dimnames = list(NULL, bundle$numeric_cols)
    )
  )

  preprocessed <- preprocess_unknown(outlier, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)
  # The classifier still forces an assignment to some group
  expect_length(pred_res$result$predicted_class, 1)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_true(diag$Typicality_p[1] < 0.05)
})

test_that("diagnostics_original_space: QDA group_stats are per-group, not pooled", {
  bundle <- make_qda_bundle()
  expect_false(is.null(bundle$group_stats))
  expect_setequal(names(bundle$group_stats), levels(iris$Species))

  covs <- lapply(bundle$group_stats, function(gs) solve(gs$cov_inv))
  expect_false(isTRUE(all.equal(covs[[1]], covs[[2]])))
})

test_that("diagnostics_original_space returns NULL without group_stats", {
  bundle <- make_lda_bundle()
  bundle$group_stats <- NULL

  unknown <- iris[121:125, bundle$numeric_cols]
  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  expect_null(diag_res$result)
})

# --- Mahalanobis/typicality: PLS-DA / sPLS-DA ---

test_that("diagnostics_component_space works for PLS-DA", {
  bundle <- make_plsda_bundle(sparse = FALSE)
  unknown <- iris[121:150, bundle$numeric_cols]

  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_false(is.null(diag))
  expect_equal(nrow(diag), 30)
  expect_true(all(diag$Typicality_p >= 0 & diag$Typicality_p <= 1))
})

test_that("diagnostics_component_space works for sPLS-DA", {
  bundle <- make_plsda_bundle(sparse = TRUE)
  unknown <- iris[121:150, bundle$numeric_cols]

  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_false(is.null(diag))
  expect_equal(nrow(diag), 30)
})

test_that("diagnostics_component_space returns NULL without group_component_stats", {
  bundle <- make_plsda_bundle(sparse = FALSE)
  bundle$group_component_stats <- NULL

  unknown <- iris[121:125, bundle$numeric_cols]
  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  expect_null(diag_res$result)
})

# --- Distance-ratio: Cluster (K-Means / PAM) ---

test_that("diagnostics_cluster: point at a blob center has low distance-ratio", {
  built <- make_blob_cluster_bundle(variant = "kmeans")
  bundle <- built$bundle
  centers <- bundle$model$centers

  unknown <- as.data.frame(
    matrix(
      centers[1, ], nrow = 1,
      dimnames = list(NULL, bundle$numeric_cols)
    )
  )
  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_true(diag$Distance_ratio[1] < 0.5)
})

test_that("diagnostics_cluster: centroid-midpoint tie gives Distance_ratio == 1", {
  built <- make_blob_cluster_bundle(variant = "kmeans")
  bundle <- built$bundle
  centers <- bundle$model$centers

  midpoint <- colMeans(centers[1:2, , drop = FALSE])
  unknown <- as.data.frame(
    matrix(
      midpoint, nrow = 1,
      dimnames = list(NULL, bundle$numeric_cols)
    )
  )

  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_equal(diag$Distance_ratio[1], 1, tolerance = 1e-8)
})

test_that("diagnostics_cluster: Distance_ratio always bounded in [0, 1]", {
  bundle <- make_cluster_bundle(variant = "kmeans")
  unknown <- iris[121:150, bundle$numeric_cols]

  preprocessed <- preprocess_unknown(unknown, bundle)
  pred_res <- predict_unknown(bundle, preprocessed)
  expect_true(pred_res$success)

  diag_res <- compute_diagnostics(bundle, preprocessed, pred_res$result)
  expect_true(diag_res$success)
  diag <- diag_res$result

  expect_true(all(diag$Distance_ratio >= 0 & diag$Distance_ratio <= 1))
})
