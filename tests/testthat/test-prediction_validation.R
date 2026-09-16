box::use(
  testthat[expect_false, expect_length, expect_true, test_that],
)

box::use(
  app/logic/prediction/validation[
    validate_unknown_data
  ],
)

# =============================================================================
# Tests for prediction validation
# =============================================================================

# Helper: create a minimal bundle for validation tests
make_bundle <- function() {
  list(
    analysis_type = "lda",
    model = list(dummy = TRUE),
    raw_data = data.frame(
      x = c(1, 2, 3, 4, 5),
      y = c(10, 20, 30, 40, 50),
      group = c("A", "A", "B", "B", "B")
    ),
    used_data = data.frame(
      x = c(1, 2, 3, 4, 5),
      y = c(10, 20, 30, 40, 50)
    ),
    numeric_cols = c("x", "y"),
    meta_cols = c("group"),
    transform_params = list(),
    scale_params = NULL,
    settings = list(),
    data_source = "raw",
    app_version = "2.0.0",
    created = Sys.time()
  )
}

test_that("validate_unknown_data passes with matching columns", {
  bundle <- make_bundle()
  unknown <- data.frame(x = c(2, 3), y = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  expect_length(result$errors, 0)
})

test_that("validate_unknown_data fails on missing columns", {
  bundle <- make_bundle()
  unknown <- data.frame(x = c(2, 3), z = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_false(result$valid)
  expect_true(any(grepl("Missing", result$errors)))
})

test_that("validate_unknown_data fails on non-numeric columns", {
  bundle <- make_bundle()
  unknown <- data.frame(
    x = c(2, 3),
    y = c("a", "b"),
    stringsAsFactors = FALSE
  )

  result <- validate_unknown_data(unknown, bundle)
  expect_false(result$valid)
  expect_true(any(grepl("numeric", result$errors)))
})

test_that("validate_unknown_data warns on missing meta columns", {
  bundle <- make_bundle()
  unknown <- data.frame(x = c(2, 3), y = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  expect_true(any(grepl("metadata", result$warnings)))
})

test_that("validate_unknown_data warns on out-of-range values", {
  bundle <- make_bundle()
  # x range in training is [1, 5], span = 4, margin = 0.8
  # So 100 is far outside
  unknown <- data.frame(x = c(100), y = c(30))

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  expect_true(any(grepl("extends beyond", result$warnings)))
})

test_that("validate_unknown_data no range warning for in-range data", {
  bundle <- make_bundle()
  unknown <- data.frame(x = c(2, 4), y = c(20, 40))

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  range_warnings <- grep(
    "extends beyond", result$warnings,
    value = TRUE
  )
  expect_length(range_warnings, 0)
})

# --- Regression guard: validation stays generic for cluster bundles ---

make_cluster_bundle <- function() {
  bundle <- make_bundle()
  bundle$analysis_type <- "cluster"
  bundle$variant <- "kmeans"
  bundle$cluster_metric <- "euclidean"
  bundle$n_clusters <- 2
  bundle$cluster_labels <- c(1L, 1L, 2L, 2L, 2L)
  bundle
}

test_that("validate_unknown_data works unchanged for a cluster bundle", {
  bundle <- make_cluster_bundle()
  unknown <- data.frame(x = c(2, 3), y = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  expect_length(result$errors, 0)
})

test_that("validate_unknown_data still flags missing columns for a cluster bundle", {
  bundle <- make_cluster_bundle()
  unknown <- data.frame(x = c(2, 3), z = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_false(result$valid)
  expect_true(any(grepl("Missing", result$errors)))
})

# =============================================================================
# Training filter (filter_spec) enforcement
#
# A model fitted on one tooth must not be applied to unknown data from
# a different tooth, so unlike the metadata check these are blocking.
# =============================================================================

# Helper: a bundle whose model was fitted on TOOTH == "M1" only
make_filtered_bundle <- function() {
  bundle <- make_bundle()
  bundle$raw_data$TOOTH <- c("M1", "M1", "M1", "M1", "M1")
  bundle$meta_cols <- c("group", "TOOTH")
  bundle$filter_spec <- list(
    version = 1L,
    reapply = list(TOOTH = "M1"),
    training_only = list(),
    n_rows_before = 15L,
    n_rows_after = 5L
  )
  bundle
}

test_that("a bundle without a filter_spec is unaffected", {
  # Backwards compatibility: bundles saved before training filters
  # existed must behave exactly as they did before.
  bundle <- make_bundle()
  unknown <- data.frame(x = c(2, 3), y = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_true(result$n_matching == 2)
  expect_true(result$n_uploaded == 2)
})

test_that("training filter passes and counts matching rows", {
  bundle <- make_filtered_bundle()
  unknown <- data.frame(
    x = c(2, 3, 4),
    y = c(15, 25, 35),
    TOOTH = c("M1", "M2", "M1")
  )

  result <- validate_unknown_data(unknown, bundle)
  expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_true(result$n_matching == 2)
  expect_true(result$n_uploaded == 3)
})

test_that("missing filter column blocks prediction", {
  bundle <- make_filtered_bundle()
  unknown <- data.frame(x = c(2, 3), y = c(15, 25))

  result <- validate_unknown_data(unknown, bundle)
  expect_false(result$valid)
  expect_true(any(grepl("TOOTH", result$errors, fixed = TRUE)))
})

test_that("case-mismatched filter levels block prediction", {
  # The most likely real-world failure: "M1" vs "m1". Without this it
  # would look like a legitimately empty dataset.
  bundle <- make_filtered_bundle()
  unknown <- data.frame(
    x = c(2, 3),
    y = c(15, 25),
    TOOTH = c("m1", "m1")
  )

  result <- validate_unknown_data(unknown, bundle)
  expect_false(result$valid)
  expect_true(any(grepl("Required: M1", result$errors, fixed = TRUE)))
  expect_true(any(grepl("m1", result$errors, fixed = TRUE)))
})

test_that("a filter matching no rows blocks prediction", {
  bundle <- make_filtered_bundle()
  unknown <- data.frame(
    x = c(2, 3),
    y = c(15, 25),
    TOOTH = c("M2", "P4")
  )

  result <- validate_unknown_data(unknown, bundle)
  expect_false(result$valid)
  expect_length(result$errors, 1)
})
