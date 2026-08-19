box::use(
  testthat[describe, expect_false, expect_true, it],
  shiny[NS],
)

box::use(
  app/view/lda/results_display,
  app/logic/lda/lda[run_lda, run_plsda, run_qda],
)

# =============================================================================
# Smoke tests for the LDA results panel.
#
# The view layer has no other automated coverage, so these render the full
# panel for each analysis type and assert it does not error. This catches
# undefined-variable and bad-argument mistakes that parsing alone misses.
# =============================================================================

make_data <- function(seed = 1, n = 20) {
  set.seed(seed)
  data.frame(
    CLASS = rep(c("A", "B", "C"), each = n),
    m1 = c(rnorm(n), rnorm(n, 3), rnorm(n, 6)),
    m2 = c(rnorm(n), rnorm(n, 2), rnorm(n, 4)),
    m3 = rnorm(3 * n),
    m4 = rnorm(3 * n),
    m5 = rnorm(3 * n),
    stringsAsFactors = FALSE
  )
}

measure_cols <- paste0("m", 1:5)
ns <- NS("test")

render_ok <- function(result) {
  html <- as.character(
    results_display$render_lda_results(result, ns)
  )
  nchar(html) > 0
}


describe("render_lda_results", {
  it("renders LDA results without error", {
    res <- run_lda(
      make_data(), measure_cols, "CLASS", meta_cols = "CLASS"
    )
    expect_true(res$success)
    expect_true(render_ok(res$result))
  })

  it("renders QDA results without error", {
    res <- run_qda(
      make_data(), measure_cols, "CLASS", meta_cols = "CLASS"
    )
    expect_true(res$success)
    expect_true(render_ok(res$result))
  })

  it("renders PLS-DA results without error", {
    res <- run_plsda(
      make_data(), measure_cols, "CLASS",
      ncomp = 2, sparse = FALSE, meta_cols = "CLASS"
    )
    expect_true(res$success)
    expect_true(render_ok(res$result))
  })

  it("renders sPLS-DA results and reports untuned keepX", {
    res <- run_plsda(
      make_data(), measure_cols, "CLASS",
      ncomp = 2, sparse = TRUE, keep_x = c(3, 3),
      meta_cols = "CLASS"
    )
    expect_true(res$success)
    res$result$keepx_tuned <- FALSE

    html <- as.character(
      results_display$render_lda_results(res$result, ns)
    )
    expect_true(grepl("keepX not tuned", html, fixed = TRUE))
    expect_true(grepl("keepX was not tuned", html, fixed = TRUE))
  })

  it("renders sPLS-DA results and reports tuned keepX", {
    res <- run_plsda(
      make_data(), measure_cols, "CLASS",
      ncomp = 2, sparse = TRUE, keep_x = c(3, 3),
      meta_cols = "CLASS"
    )
    expect_true(res$success)
    res$result$keepx_tuned <- TRUE

    html <- as.character(
      results_display$render_lda_results(res$result, ns)
    )
    expect_true(grepl("keepX was tuned", html, fixed = TRUE))
    expect_false(grepl("keepX was not tuned", html, fixed = TRUE))
  })
})
