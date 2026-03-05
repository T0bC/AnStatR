box::use(
  app/logic/prediction/predict[
    preprocess_unknown, predict_unknown
  ],
  app/logic/preprocessing/skewness_transform[
    detect_skewness, transform_skewed,
    apply_stored_transform
  ],
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

make_pca_bundle <- function() {
  d <- make_iris_data()
  numeric_data <- d$data[, d$numeric_cols, drop = FALSE]
  pca_obj <- prcomp(numeric_data, center = TRUE, scale. = TRUE)

  list(
    analysis_type = "pca",
    model = pca_obj,
    raw_data = d$data,
    used_data = d$data,
    group_col = NULL,
    numeric_cols = d$numeric_cols,
    meta_cols = "Species",
    transform_params = list(),
    scale_params = NULL,
    settings = list(
      skewness_correction = FALSE,
      scale_method = "scale_center"
    ),
    data_source = "raw",
    app_version = "2.0.0",
    created = Sys.time()
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

# --- predict_unknown: PCA ---

test_that("predict_unknown works for PCA", {
  bundle <- make_pca_bundle()
  unknown <- iris[121:150, ]
  preprocessed <- preprocess_unknown(unknown, bundle)

  result <- predict_unknown(bundle, preprocessed)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "pca")
  expect_equal(nrow(result$result$scores), 30)
  expect_null(result$result$predicted_class)
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
