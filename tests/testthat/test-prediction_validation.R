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
    "extends beyond", result$warnings, value = TRUE
  )
  expect_length(range_warnings, 0)
})
