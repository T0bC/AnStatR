box::use(
  app/logic/prediction/predict[
    preprocess_unknown, predict_unknown
  ],
  app/logic/preprocessing/skewness_transform[
    detect_skewness, transform_skewed,
    apply_stored_transform
  ],
  app/logic/pca/pca[run_pca],
  app/logic/pca/pca_export[create_pca_bundle],
)

# =============================================================================
# Tests for prediction preprocessing and predict
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
  numeric_data <- d$data[, d$numeric_cols, drop = FALSE]

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

  list(
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
  lda_pred <- predict(lda_obj, numeric_data)

  list(
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
    created = Sys.time()
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

# --- preprocess_unknown ---

test_that("preprocess_unknown returns data unchanged when no transforms/scaling", {
  bundle <- make_lda_bundle()
  unknown <- iris[121:150, ]

  result <- preprocess_unknown(unknown, bundle)
  expect_equal(
    result[, bundle$numeric_cols],
    unknown[, bundle$numeric_cols]
  )
})

test_that("preprocess_unknown applies stored scaling", {
  bundle <- make_lda_bundle()
  # Add scaling params
  means <- colMeans(
    iris[1:120, bundle$numeric_cols]
  )
  sds <- vapply(
    iris[1:120, bundle$numeric_cols],
    sd, numeric(1)
  )
  bundle$scale_params <- list(
    center = means, scale = sds
  )

  unknown <- iris[121:150, ]
  result <- preprocess_unknown(unknown, bundle)

  # Manually check first column
  col <- bundle$numeric_cols[1]
  expected <- (unknown[[col]] - means[[col]]) / sds[[col]]
  expect_equal(
    result[[col]], expected,
    tolerance = 1e-10
  )
})

test_that("preprocess_unknown applies stored transforms", {
  bundle <- make_lda_bundle()

  # Create a real bestNormalize object for the transform
  train_values <- iris$Sepal.Length[1:120]
  bn_obj <- suppressWarnings(
    bestNormalize::bestNormalize(train_values, quiet = TRUE)
  )

  bundle$transform_params <- list(
    list(
      column = "Sepal.Length",
      bn_object = bn_obj
    )
  )

  unknown <- iris[121:150, ]
  result <- preprocess_unknown(unknown, bundle)

  # Manual check using the same bestNormalize object
  expected <- as.numeric(
    stats::predict(bn_obj, newdata = unknown$Sepal.Length)
  )
  expect_equal(
    result$Sepal.Length, expected,
    tolerance = 1e-10
  )
})

# --- predict_unknown: PCA / sPCA / IPCA ---

test_that("predict_unknown works for PCA", {
  bundle <- make_pca_bundle("pca")
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "pca")
  expect_equal(nrow(result$result$scores), 30)
  expect_null(result$result$predicted_class)
})

test_that("predict_unknown works for sPCA", {
  bundle <- make_pca_bundle("spca", keep_x = c(2, 2, 2, 2))
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "spca")
  expect_equal(nrow(result$result$scores), 30)
  expect_null(result$result$predicted_class)
})

test_that("predict_unknown works for IPCA", {
  bundle <- make_pca_bundle("ipca")
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "ipca")
  expect_equal(nrow(result$result$scores), 30)
  expect_null(result$result$predicted_class)
})

test_that("sPCA prediction matches refit scores on training data", {
  # Regression guard for the deflation-aware projection math:
  # predicting the training data itself should reproduce the
  # fitted model's own scores exactly.
  bundle <- make_pca_bundle("spca", keep_x = c(2, 2, 2, 2))
  train_data <- bundle$used_data
  preprocessed <- preprocess_unknown(train_data, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)

  fitted_scores <- bundle$model$variates$X
  predicted_scores <- as.matrix(result$result$scores)
  dimnames(predicted_scores) <- dimnames(fitted_scores)
  expect_equal(
    predicted_scores, fitted_scores,
    tolerance = 1e-8, ignore_attr = TRUE
  )
})

test_that("IPCA prediction matches refit scores on training data", {
  bundle <- make_pca_bundle("ipca")
  train_data <- bundle$used_data
  preprocessed <- preprocess_unknown(train_data, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)

  fitted_scores <- bundle$model$x
  predicted_scores <- as.matrix(result$result$scores)
  dimnames(predicted_scores) <- dimnames(fitted_scores)
  expect_equal(
    predicted_scores, fitted_scores,
    tolerance = 1e-8, ignore_attr = TRUE
  )
})

# --- predict_unknown: LDA ---

test_that("predict_unknown works for LDA", {
  bundle <- make_lda_bundle()
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "lda")
  expect_length(result$result$predicted_class, 30)
  expect_equal(nrow(result$result$posterior), 30)
  expect_false(is.null(result$result$scores))
})

# --- predict_unknown: QDA ---

test_that("predict_unknown works for QDA with companion LDA", {
  bundle <- make_qda_bundle()
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "qda")
  expect_length(result$result$predicted_class, 30)
  expect_equal(nrow(result$result$posterior), 30)
  # Should have LD scores from companion LDA
  expect_false(is.null(result$result$scores))
})

# --- predict_unknown: PLS-DA / sPLS-DA ---

test_that("predict_unknown works for PLS-DA", {
  bundle <- make_plsda_bundle(sparse = FALSE)
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "plsda")
  expect_length(result$result$predicted_class, 30)
  expect_equal(nrow(result$result$posterior), 30)
  expect_false(is.null(result$result$scores))
  expect_equal(ncol(result$result$scores), 2)
  expect_equal(colnames(result$result$scores), c("Comp1", "Comp2"))
})

test_that("predict_unknown works for sPLS-DA", {
  bundle <- make_plsda_bundle(sparse = TRUE)
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "splsda")
  expect_length(result$result$predicted_class, 30)
  expect_false(is.null(result$result$scores))
})

# --- predict_unknown: Cluster (K-Means / PAM) ---

test_that("predict_unknown works for cluster kmeans (nearest centroid)", {
  bundle <- make_cluster_bundle(variant = "kmeans")
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "cluster")
  expect_length(result$result$predicted_class, 30)
  expect_true(all(grepl(
    "^Cluster [0-9]+$",
    as.character(result$result$predicted_class)
  )))
  expect_null(result$result$posterior)
  expect_true(ncol(result$result$scores) >= 2)
})

test_that("predict_unknown works for cluster pam (nearest medoid)", {
  bundle <- make_cluster_bundle(variant = "pam")
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "cluster")
  expect_length(result$result$predicted_class, 30)
  expect_null(result$result$posterior)
  expect_true(ncol(result$result$scores) >= 2)
})

test_that("predict_cluster assigns exact training points to their own cluster", {
  built <- make_blob_cluster_bundle(variant = "kmeans")
  bundle <- built$bundle
  data <- built$data

  preprocessed <- preprocess_unknown(data, bundle)
  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)

  predicted_idx <- as.integer(gsub(
    "Cluster ", "",
    as.character(result$result$predicted_class)
  ))
  expect_equal(predicted_idx, bundle$cluster_labels)
})

test_that("predict_cluster PAM assigns exact training points to their own cluster", {
  built <- make_blob_cluster_bundle(variant = "pam")
  bundle <- built$bundle
  data <- built$data

  preprocessed <- preprocess_unknown(data, bundle)
  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)

  predicted_idx <- as.integer(gsub(
    "Cluster ", "",
    as.character(result$result$predicted_class)
  ))
  expect_equal(predicted_idx, bundle$cluster_labels)
})

test_that("predict_cluster handles a centroid-midpoint tie without error", {
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
  result <- predict_unknown(bundle, preprocessed)

  expect_true(result$success)
  expect_length(result$result$predicted_class, 1)
})

# --- Skewness transform round-trip ---

test_that("stored transform params reproduce training transform", {
  # Use a right-skewed column
  set.seed(42)
  x_train <- rexp(100, rate = 0.5)
  train_df <- data.frame(val = x_train)

  # Detect and transform
  skew_info <- detect_skewness(
    train_df, "val", threshold = 0.5
  )
  if (any(skew_info$is_skewed)) {
    transform_res <- transform_skewed(
      train_df, "val", skew_info
    )
    expect_true(transform_res$success)

    params <- transform_res$result$transform_params
    expect_true(length(params) > 0)

    # Apply stored transform to new data
    x_new <- rexp(20, rate = 0.5)
    transformed_new <- apply_stored_transform(
      x_new, params[[1]]
    )

    # Should produce finite numeric values
    expect_true(all(is.finite(transformed_new)))
    expect_true(is.numeric(transformed_new))
  }
})
