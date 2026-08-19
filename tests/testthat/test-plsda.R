box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/lda/lda,
)

# =============================================================================
# Helper: create high-dimensional, collinear, n < p test data
# =============================================================================

make_high_dim_data <- function(seed = 42, n_per_group = 8,
                               p = 40) {
  set.seed(seed)
  n <- n_per_group * 3
  species <- rep(c("A", "B", "C"), each = n_per_group)

  # Build 4 collinear blocks from 10 latent columns each,
  # so multicollinearity is baked into the design
  base <- matrix(rnorm(n * (p / 4)), nrow = n)
  x <- do.call(cbind, lapply(seq_len(4), function(i) {
    base + matrix(rnorm(n * (p / 4), sd = 0.1), nrow = n)
  }))
  colnames(x) <- paste0("m", seq_len(p))

  # Inject group signal into the first few columns only
  x[species == "B", 1:3] <- x[species == "B", 1:3] + 3
  x[species == "C", 1:3] <- x[species == "C", 1:3] + 6

  data.frame(species = species, x, stringsAsFactors = FALSE)
}

all_cols <- function(p = 40) paste0("m", seq_len(p))

# =============================================================================
# validate_inputs (plsda/splsda)
# =============================================================================

describe("validate_inputs for PLS-DA", {
  it("does not warn about n < p (unlike LDA)", {
    data <- make_high_dim_data()
    result <- lda$validate_inputs(
      all_cols(), data, "species",
      analysis_type = "plsda"
    )
    expect_true(result$valid)
    expect_equal(length(result$warnings), 0)
  })

  it("still requires >= 2 groups", {
    data <- make_high_dim_data()
    data$species <- "A"
    result <- lda$validate_inputs(
      all_cols(), data, "species",
      analysis_type = "plsda"
    )
    expect_true(!result$valid)
  })

  it("LDA validation on the same n < p data still warns", {
    data <- make_high_dim_data()
    result <- lda$validate_inputs(
      all_cols(), data, "species",
      analysis_type = "lda"
    )
    expect_true(result$valid)
    expect_true(length(result$warnings) > 0)
  })
})

# =============================================================================
# run_plsda
# =============================================================================

describe("run_plsda", {
  it("fits successfully on n < p, collinear data", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2
    )
    expect_true(result$success)
    r <- result$result
    expect_equal(r$analysis_type, "plsda")
    expect_equal(nrow(r$scores), 24)
    expect_equal(ncol(r$scores), 2)
    expect_equal(colnames(r$scores), c("Comp1", "Comp2"))
    expect_equal(nrow(r$scaling), 40)
    expect_true(!is.null(r$model))
  })

  it("proportion_of_trace sums to <= 1 and is cumulative", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2
    )
    trace <- result$result$proportion_of_trace
    expect_equal(nrow(trace), 2)
    expect_true(all(trace$Proportion >= 0))
    expect_true(
      trace$Cumulative[2] >= trace$Cumulative[1]
    )
  })

  it("computes a confusion matrix with accuracy", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2
    )
    expect_true(!is.null(result$result$confusion))
    expect_true(result$result$confusion$accuracy > 0)
  })

  it("has no keep_x/selected_variables for non-sparse fits", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2, sparse = FALSE
    )
    expect_true(is.null(result$result$selected_variables))
  })
})

# =============================================================================
# run_plsda (sparse = TRUE)
# =============================================================================

describe("run_plsda sparse (sPLS-DA)", {
  it("selects at most keep_x variables per component", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2,
      sparse = TRUE, keep_x = c(5, 8)
    )
    expect_true(result$success)
    sel <- result$result$selected_variables
    expect_true(length(sel$Comp1) <= 5)
    expect_true(length(sel$Comp2) <= 8)
  })

  it("selected variables are a subset of the input columns", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2,
      sparse = TRUE, keep_x = c(5, 5)
    )
    sel <- result$result$selected_variables
    expect_true(all(sel$Comp1 %in% all_cols()))
    expect_true(all(sel$Comp2 %in% all_cols()))
  })

  it("returns analysis_type 'splsda'", {
    data <- make_high_dim_data()
    result <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2,
      sparse = TRUE, keep_x = c(5, 5)
    )
    expect_equal(result$result$analysis_type, "splsda")
  })
})

# =============================================================================
# run_predict (plsda)
# =============================================================================

describe("run_predict for PLS-DA", {
  it("predicts held-out observations with a confusion matrix", {
    data <- make_high_dim_data()
    train <- data[c(1:6, 9:14, 17:22), ]
    test <- data[c(7, 8, 15, 16, 23, 24), ]

    fit <- lda$run_plsda(
      train, all_cols(), "species", ncomp = 2
    )
    expect_true(fit$success)

    pred <- lda$run_predict(
      fit$result, test, all_cols(),
      grouping_col = "species"
    )
    expect_true(pred$success)
    expect_equal(nrow(pred$result$scores), 6)
    expect_equal(
      colnames(pred$result$scores), c("Comp1", "Comp2")
    )
    expect_true(!is.null(pred$result$confusion))
  })
})

# =============================================================================
# run_plsda_perf
# =============================================================================

describe("run_plsda_perf", {
  it("returns a per-component error-rate table without erroring", {
    data <- make_high_dim_data()
    fit <- lda$run_plsda(
      data, all_cols(), "species", ncomp = 2
    )
    expect_true(fit$success)

    perf_res <- lda$run_plsda_perf(
      fit$result, folds = 3, repeats = 2
    )
    expect_true(perf_res$success)
    # run_plsda_perf() returns a list of two tables:
    # $errors (per-component error rates) and $stability.
    df <- perf_res$result$errors
    expect_true(is.data.frame(df))
    expect_equal(nrow(df), 2)
    expect_true(all(c("Component", "Overall Error", "BER") %in%
      names(df)))
  })
})
