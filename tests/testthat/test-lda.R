box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/lda/lda,
  app/logic/lda/data_splitting,
)

# =============================================================================
# Helper: create test data with separable groups
# =============================================================================

make_test_data <- function(seed = 123) {
  set.seed(seed)
  data.frame(
    species = rep(c("A", "B", "C"), each = 15),
    site = rep(c("X", "Y", "Z"), 15),
    m1 = c(
      rnorm(15, mean = 0), rnorm(15, mean = 3),
      rnorm(15, mean = 6)
    ),
    m2 = c(
      rnorm(15, mean = 0), rnorm(15, mean = 2),
      rnorm(15, mean = 4)
    ),
    m3 = rnorm(45),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# validate_inputs
# =============================================================================

describe("validate_inputs", {
  it("returns valid = TRUE for valid inputs", {
    data <- make_test_data()
    result <- lda$validate_inputs(
      c("m1", "m2", "m3"), data, "species"
    )
    expect_true(result$valid)
  })

  it("returns valid = FALSE when no columns selected", {
    data <- make_test_data()
    result <- lda$validate_inputs(
      NULL, data, "species"
    )
    expect_true(!result$valid)
  })

  it("returns valid = FALSE for missing columns", {
    data <- make_test_data()
    result <- lda$validate_inputs(
      c("m1", "nonexistent"), data, "species"
    )
    expect_true(!result$valid)
  })

  it("returns valid = FALSE when no grouping column", {
    data <- make_test_data()
    result <- lda$validate_inputs(
      c("m1", "m2"), data, NULL
    )
    expect_true(!result$valid)
  })

  it("returns valid = FALSE for empty grouping column", {
    data <- make_test_data()
    result <- lda$validate_inputs(
      c("m1", "m2"), data, ""
    )
    expect_true(!result$valid)
  })

  it("returns valid = FALSE when grouping not in data", {
    data <- make_test_data()
    result <- lda$validate_inputs(
      c("m1", "m2"), data, "nonexistent"
    )
    expect_true(!result$valid)
  })

  it("returns valid = FALSE when grouping < 2 levels", {
    data <- make_test_data()
    data$species <- "A"
    result <- lda$validate_inputs(
      c("m1", "m2"), data, "species"
    )
    expect_true(!result$valid)
  })

  it("returns warnings when n < p for some groups", {
    set.seed(1)
    data <- data.frame(
      species = c(rep("A", 2), rep("B", 20)),
      m1 = rnorm(22), m2 = rnorm(22),
      m3 = rnorm(22), m4 = rnorm(22),
      m5 = rnorm(22),
      stringsAsFactors = FALSE
    )
    result <- lda$validate_inputs(
      c("m1", "m2", "m3", "m4", "m5"),
      data, "species"
    )
    expect_true(result$valid)
    expect_true(length(result$warnings) > 0)
  })
})

# =============================================================================
# run_lda
# =============================================================================

describe("run_lda", {
  it("returns success with model for valid data", {
    data <- make_test_data()
    result <- lda$run_lda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$model))
    expect_true(!is.null(result$result$scaling))
    expect_true(!is.null(result$result$means))
    expect_true(
      !is.null(result$result$proportion_of_trace)
    )
    expect_equal(result$result$n_groups, 3)
    expect_equal(result$result$n, 45)
  })

  it("returns confusion matrix with accuracy", {
    data <- make_test_data()
    result <- lda$run_lda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(!is.null(result$result$confusion))
    expect_true(
      result$result$confusion$accuracy > 0
    )
  })

  it("computes LD scores for all observations", {
    data <- make_test_data()
    result <- lda$run_lda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_equal(nrow(result$result$scores), 45)
  })

  it("works with LOO-CV", {
    data <- make_test_data()
    result <- lda$run_lda(
      data, c("m1", "m2"), "species", cv = TRUE
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$cv))
    expect_true(
      result$result$cv$accuracy > 0
    )
    expect_true(is.null(result$result$model))
  })

  it("works with equal prior", {
    data <- make_test_data()
    result <- lda$run_lda(
      data, c("m1", "m2"), "species",
      prior = "equal"
    )
    expect_true(result$success)
    priors <- result$result$prior
    expect_true(
      all(abs(priors - 1 / 3) < 0.001)
    )
  })

  it("includes metadata when meta_cols given", {
    data <- make_test_data()
    result <- lda$run_lda(
      data, c("m1", "m2"), "species",
      meta_cols = c("species", "site")
    )
    expect_true(result$success)
    expect_true("site" %in% names(
      result$result$meta
    ))
  })
})

# =============================================================================
# run_qda
# =============================================================================

describe("run_qda", {
  it("returns success for valid data", {
    data <- make_test_data()
    result <- lda$run_qda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$means))
    expect_true(
      !is.null(result$result$confusion)
    )
    expect_true(
      is.null(result$result$scaling)
    )
  })

  it("works with LOO-CV", {
    data <- make_test_data()
    result <- lda$run_qda(
      data, c("m1", "m2"), "species", cv = TRUE
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$cv))
  })
})

# =============================================================================
# run_predict
# =============================================================================

describe("run_predict", {
  it("predicts on test data from LDA model", {
    data <- make_test_data()
    train <- data[1:30, ]
    test <- data[31:45, ]
    fit <- lda$run_lda(
      train, c("m1", "m2", "m3"), "species"
    )
    pred <- lda$run_predict(
      fit$result, test, c("m1", "m2", "m3"),
      grouping_col = "species"
    )
    expect_true(pred$success)
    expect_equal(length(pred$result$predicted_class), 15)
    expect_true(!is.null(pred$result$confusion))
    expect_true(!is.null(pred$result$scores))
  })

  it("predicts on test data from QDA model", {
    data <- make_test_data()
    train <- data[1:30, ]
    test <- data[31:45, ]
    fit <- lda$run_qda(
      train, c("m1", "m2", "m3"), "species"
    )
    pred <- lda$run_predict(
      fit$result, test, c("m1", "m2", "m3"),
      grouping_col = "species"
    )
    expect_true(pred$success)
    expect_equal(length(pred$result$predicted_class), 15)
  })

  it("fails when model was fitted with CV", {
    data <- make_test_data()
    fit <- lda$run_lda(
      data, c("m1", "m2"), "species", cv = TRUE
    )
    pred <- lda$run_predict(
      fit$result, data, c("m1", "m2")
    )
    expect_true(!pred$success)
  })
})

# =============================================================================
# create_stratified_split
# =============================================================================

describe("create_stratified_split", {
  it("creates a valid split", {
    data <- make_test_data()
    result <- data_splitting$create_stratified_split(
      data, "species",
      train_fraction = 0.7, seed = 42
    )
    expect_true(result$success)
    n_train <- nrow(result$result$train_data)
    n_test <- nrow(result$result$test_data)
    expect_equal(n_train + n_test, 45)
    expect_true(n_train > n_test)
  })

  it("preserves all groups in both sets", {
    data <- make_test_data()
    result <- data_splitting$create_stratified_split(
      data, "species",
      train_fraction = 0.7, seed = 42
    )
    train_groups <- unique(
      result$result$train_data$species
    )
    test_groups <- unique(
      result$result$test_data$species
    )
    expect_equal(
      sort(train_groups), sort(test_groups)
    )
  })

  it("is reproducible with same seed", {
    data <- make_test_data()
    r1 <- data_splitting$create_stratified_split(
      data, "species", seed = 99
    )
    r2 <- data_splitting$create_stratified_split(
      data, "species", seed = 99
    )
    expect_equal(
      r1$result$train_idx, r2$result$train_idx
    )
  })

  it("returns split_summary with per-group counts", {
    data <- make_test_data()
    result <- data_splitting$create_stratified_split(
      data, "species", seed = 42
    )
    ss <- result$result$split_summary
    expect_true("Group" %in% names(ss))
    expect_true("Train" %in% names(ss))
    expect_true("Test" %in% names(ss))
    expect_equal(nrow(ss), 3)
  })
})

# =============================================================================
# lda_error_parser
# =============================================================================

describe("lda_error_parser", {
  it("parses singular matrix errors", {
    msg <- lda$lda_error_parser(
      "matrix is singular", "LDA"
    )
    expect_true(
      grepl("singular", msg, ignore.case = TRUE)
    )
  })

  it("parses NA errors", {
    msg <- lda$lda_error_parser(
      "data contains NA values", "LDA"
    )
    expect_true(
      grepl("missing", msg, ignore.case = TRUE)
    )
  })

  it("returns generic message for unknown errors", {
    msg <- lda$lda_error_parser(
      "something unexpected", "LDA"
    )
    expect_true(grepl("failed", msg))
  })

  it("parses convergence errors", {
    msg <- lda$lda_error_parser(
      "EM did not converge", "MDA"
    )
    expect_true(
      grepl("converge", msg, ignore.case = TRUE)
    )
  })
})

# =============================================================================
# run_mda
# =============================================================================

describe("run_mda", {
  it("returns success with model for valid data", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$model))
    expect_true(!is.null(result$result$means))
    expect_equal(result$result$analysis_type, "mda")
    expect_equal(result$result$n_groups, 3)
    expect_equal(result$result$n, 45)
  })

  it("returns confusion matrix with accuracy", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(!is.null(result$result$confusion))
    expect_true(
      result$result$confusion$accuracy > 0
    )
  })

  it("computes discriminant scores", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(!is.null(result$result$scores))
    expect_equal(nrow(result$result$scores), 45)
  })

  it("returns posterior probabilities", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(!is.null(result$result$posterior))
    expect_equal(nrow(result$result$posterior), 45)
    expect_equal(ncol(result$result$posterior), 3)
  })

  it("returns proportion of trace", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(
      !is.null(result$result$proportion_of_trace)
    )
  })

  it("works with LOO-CV", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2"), "species", cv = TRUE
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$cv))
    expect_true(
      result$result$cv$accuracy > 0
    )
    expect_true(is.null(result$result$model))
  })

  it("works with equal prior", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2"), "species",
      prior = "equal"
    )
    expect_true(result$success)
    priors <- result$result$prior
    expect_true(
      all(abs(priors - 1 / 3) < 0.001)
    )
  })

  it("includes metadata when meta_cols given", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2"), "species",
      meta_cols = c("species", "site")
    )
    expect_true(result$success)
    expect_true("site" %in% names(
      result$result$meta
    ))
  })

  it("works with custom subclasses", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species",
      subclasses = 2, iter = 10
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$model))
  })

  it("returns subclass priors", {
    data <- make_test_data()
    result <- lda$run_mda(
      data, c("m1", "m2", "m3"), "species"
    )
    expect_true(
      !is.null(result$result$sub_prior)
    )
  })
})

# =============================================================================
# run_predict with MDA
# =============================================================================

describe("run_predict with MDA", {
  it("predicts on test data from MDA model", {
    data <- make_test_data()
    train <- data[1:30, ]
    test <- data[31:45, ]
    fit <- lda$run_mda(
      train, c("m1", "m2", "m3"), "species"
    )
    pred <- lda$run_predict(
      fit$result, test, c("m1", "m2", "m3"),
      grouping_col = "species"
    )
    expect_true(pred$success)
    expect_equal(
      length(pred$result$predicted_class), 15
    )
    expect_true(!is.null(pred$result$confusion))
    expect_true(!is.null(pred$result$posterior))
  })

  it("fails when MDA model was fitted with CV", {
    data <- make_test_data()
    fit <- lda$run_mda(
      data, c("m1", "m2"), "species", cv = TRUE
    )
    pred <- lda$run_predict(
      fit$result, data, c("m1", "m2")
    )
    expect_true(!pred$success)
  })
})
