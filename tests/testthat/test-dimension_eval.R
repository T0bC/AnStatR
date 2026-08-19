box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it],
)

box::use(
  app/logic/lda/dimension_eval[evaluate_dimensions],
  app/logic/lda/lda[run_lda],
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

measurement_cols <- c("m1", "m2", "m3")


describe("evaluate_dimensions", {
  it("succeeds when the grouping column is present in metadata", {
    data <- make_test_data()
    res <- run_lda(
      data,
      columns = measurement_cols,
      grouping_col = "species",
      meta_cols = c("species", "site")
    )
    expect_true(res$success)

    dim_eval <- evaluate_dimensions(res$result)
    expect_true(dim_eval$success)
    expect_true(is.data.frame(dim_eval$result))
    expect_true(nrow(dim_eval$result) > 0)
    expect_true(all(
      c("Dimension", "F", "p.value", "R2") %in% names(dim_eval$result)
    ))
  })

  it("fails explicitly when the grouping column is absent from metadata", {
    data <- make_test_data()
    res <- run_lda(
      data,
      columns = measurement_cols,
      grouping_col = "species",
      meta_cols = "site"
    )
    expect_true(res$success)
    # Precondition: the true labels really are unavailable.
    expect_false("species" %in% names(res$result$meta))

    dim_eval <- evaluate_dimensions(res$result)
    expect_false(isTRUE(dim_eval$success))
    expect_true(grepl(
      "Descriptive columns", dim_eval$error$message, fixed = TRUE
    ))
  })

  it("does not substitute predicted labels for true group labels", {
    data <- make_test_data()
    res <- run_lda(
      data,
      columns = measurement_cols,
      grouping_col = "species",
      meta_cols = character(0)
    )
    expect_true(res$success)
    # predicted_class exists, so the old fallback would have fired here.
    expect_true(!is.null(res$result$predicted_class))

    dim_eval <- evaluate_dimensions(res$result)
    expect_false(isTRUE(dim_eval$success))
  })
})
