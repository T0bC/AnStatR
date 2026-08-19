box::use(
  testthat[describe, expect_false, expect_true, it],
  shiny[NS],
)

box::use(
  app/view/lda/results_display,
  app/logic/lda/lda[run_lda, run_mda, run_predict, run_qda],
  app/logic/lda/data_splitting[create_stratified_split],
)

# =============================================================================
# Validation reporting in the Summary panel.
#
# Unvalidated results must be labelled as such; validated results must show
# the resubstitution figure alongside so the overfitting gap is visible
# without running the analysis twice.
# =============================================================================

ns <- NS("test")

separable_data <- function(seed = 1, n = 30) {
  set.seed(seed)
  data.frame(
    CLASS = rep(c("a", "b", "c"), each = n),
    m1 = c(rnorm(n), rnorm(n, 4), rnorm(n, 8)),
    m2 = c(rnorm(n), rnorm(n, 3), rnorm(n, 6)),
    m3 = rnorm(3 * n),
    m4 = rnorm(3 * n),
    stringsAsFactors = FALSE
  )
}

# Pure noise with more variables than observations per group: the model
# can memorise the training data but cannot generalise.
noise_data <- function(seed = 2, n = 8, p = 12) {
  set.seed(seed)
  d <- data.frame(
    CLASS = rep(c("a", "b", "c"), each = n),
    stringsAsFactors = FALSE
  )
  d[paste0("v", seq_len(p))] <- matrix(
    rnorm(3 * n * p), 3 * n, p
  )
  d
}

measure_cols <- paste0("m", 1:4)

render <- function(result, test_result = NULL) {
  as.character(results_display$render_lda_results(
    result, ns, test_result = test_result
  ))
}


describe("unvalidated results", {
  it("warns that the accuracy is not a performance estimate", {
    res <- run_lda(
      separable_data(), measure_cols, "CLASS", meta_cols = "CLASS"
    )
    expect_true(res$success)
    html <- render(res$result)
    expect_true(grepl("Not validated", html, fixed = TRUE))
    expect_true(grepl("should not be reported", html, fixed = TRUE))
  })

  it("points LDA users at leave-one-out CV", {
    lda_html <- render(run_lda(
      separable_data(), measure_cols, "CLASS", meta_cols = "CLASS"
    )$result)
    expect_true(grepl("Leave-one-out CV", lda_html, fixed = TRUE))
  })
})


describe("validated results", {
  it("shows resubstitution alongside the LOO-CV figure", {
    res <- run_lda(
      separable_data(), measure_cols, "CLASS",
      cv = TRUE, meta_cols = "CLASS"
    )
    expect_true(res$success)
    expect_true(!is.null(res$result$resubstitution))

    html <- render(res$result)
    expect_false(grepl("Not validated", html, fixed = TRUE))
    expect_true(grepl("Validated:", html, fixed = TRUE))
    expect_true(grepl(
      "Resubstitution (on all data)", html, fixed = TRUE
    ))
  })

  it("computes resubstitution for QDA and MDA under LOO-CV", {
    qda_res <- run_qda(
      separable_data(), measure_cols, "CLASS",
      cv = TRUE, meta_cols = "CLASS"
    )
    expect_true(qda_res$success)
    expect_true(grepl("Validated:", render(qda_res$result), fixed = TRUE))

    mda_res <- run_mda(
      separable_data(), measure_cols, "CLASS",
      cv = TRUE, subclasses = 2, meta_cols = "CLASS"
    )
    expect_true(mda_res$success)
    # MDA in CV mode has no group means; the panel must be skipped
    # rather than erroring.
    expect_true(grepl("Validated:", render(mda_res$result), fixed = TRUE))
  })

  it("flags a large gap as overfitting", {
    res <- run_lda(
      noise_data(), paste0("v", 1:12), "CLASS",
      cv = TRUE, meta_cols = "CLASS"
    )
    expect_true(res$success)
    html <- render(res$result)
    # Noise data: near-chance validated accuracy, high resubstitution.
    expect_true(grepl("alert-danger", html, fixed = TRUE))
    expect_true(grepl("fitting noise", html, fixed = TRUE))
  })

  it("reports agreement when the model generalises", {
    res <- run_lda(
      separable_data(), measure_cols, "CLASS",
      cv = TRUE, meta_cols = "CLASS"
    )
    html <- render(res$result)
    expect_true(grepl("alert-success", html, fixed = TRUE))
    expect_true(grepl("generalises well", html, fixed = TRUE))
  })

  it("shows the training-set figure in split mode", {
    d <- separable_data()
    split <- create_stratified_split(
      d, "CLASS", train_fraction = 0.7, seed = 42
    )
    expect_true(split$success)
    train <- split$result$train_data
    test <- split$result$test_data

    fit <- run_lda(train, measure_cols, "CLASS", meta_cols = "CLASS")
    expect_true(fit$success)
    pred <- run_predict(
      fit$result, test, measure_cols,
      grouping_col = "CLASS", meta_cols = "CLASS"
    )
    expect_true(pred$success)

    html <- render(fit$result, test_result = pred$result)
    expect_false(grepl("Not validated", html, fixed = TRUE))
    expect_true(grepl(
      "Resubstitution (on the training set)", html, fixed = TRUE
    ))
  })
})
