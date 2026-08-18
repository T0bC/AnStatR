box::use(
  app/logic/prediction/bundle_io[
    load_bundle, validate_bundle
  ],
)

# =============================================================================
# Tests for prediction bundle I/O
# =============================================================================

# Helper: create a minimal valid bundle
make_test_bundle <- function(
    analysis_type = "lda",
    with_model = TRUE) {
  bundle <- list(
    analysis_type = analysis_type,
    model = if (with_model) list(dummy = TRUE) else NULL,
    raw_data = data.frame(x = 1:5, y = 6:10),
    used_data = data.frame(x = 1:5, y = 6:10),
    numeric_cols = c("x", "y"),
    meta_cols = character(0),
    transform_params = list(),
    scale_params = NULL,
    settings = list(),
    data_source = "raw",
    app_version = "2.0.0",
    created = Sys.time()
  )
  bundle
}

# --- validate_bundle ---

test_that("validate_bundle accepts a valid bundle", {
  bundle <- make_test_bundle()
  result <- validate_bundle(bundle)
  expect_true(result$valid)
  expect_null(result$message)
})

test_that("validate_bundle rejects non-list input", {
  result <- validate_bundle("not a list")
  expect_false(result$valid)
  expect_match(result$message, "named list")
})

test_that("validate_bundle rejects bundle with missing fields", {
  bundle <- list(analysis_type = "lda")
  result <- validate_bundle(bundle)
  expect_false(result$valid)
  expect_match(result$message, "missing required")
})

test_that("validate_bundle rejects unknown analysis type", {
  bundle <- make_test_bundle()
  bundle$analysis_type <- "unknown"
  result <- validate_bundle(bundle)
  expect_false(result$valid)
  expect_match(result$message, "Unknown analysis type")
})

test_that("validate_bundle rejects bundle without model", {
  bundle <- make_test_bundle(with_model = FALSE)
  result <- validate_bundle(bundle)
  expect_false(result$valid)
  expect_match(result$message, "does not contain a fitted model")
})

test_that("validate_bundle accepts all valid types", {
  for (type in c("pca", "lda", "mda", "qda", "cluster")) {
    bundle <- make_test_bundle(analysis_type = type)
    result <- validate_bundle(bundle)
    expect_true(
      result$valid,
      info = paste("Failed for type:", type)
    )
  }
})

# --- load_bundle ---

test_that("load_bundle returns error for non-existent file", {
  result <- load_bundle("/nonexistent/path/file.rds")
  expect_false(result$success)
})

test_that("load_bundle returns error for non-rds file", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", tmp)
  result <- load_bundle(tmp)
  expect_false(result$success)
  unlink(tmp)
})

test_that("load_bundle reads a valid bundle file", {
  bundle <- make_test_bundle()
  tmp <- tempfile(fileext = ".rds")
  saveRDS(bundle, tmp)

  result <- load_bundle(tmp)
  expect_true(result$success)
  expect_equal(result$result$analysis_type, "lda")
  expect_equal(result$result$numeric_cols, c("x", "y"))

  unlink(tmp)
})

test_that("load_bundle rejects invalid bundle content", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(list(bad = "data"), tmp)

  result <- load_bundle(tmp)
  expect_false(result$success)

  unlink(tmp)
})
