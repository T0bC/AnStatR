box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/var_contrib,
  app/logic/pca/pca,
)

impl <- attr(var_contrib, "namespace")

# =============================================================================
# Shared test fixtures
# =============================================================================

make_pca_result <- function(n = 20) {
  set.seed(42)
  test_data <- data.frame(
    a = rnorm(n, mean = 10, sd = 2),
    b = rnorm(n, mean = 5, sd = 1),
    c = rnorm(n, mean = 0, sd = 3),
    d = rnorm(n, mean = 20, sd = 5),
    stringsAsFactors = FALSE
  )
  res <- pca$run_pca(
    test_data, c("a", "b", "c", "d")
  )
  res$result
}

# =============================================================================
# create_var_contrib_plot
# =============================================================================

describe("create_var_contrib_plot", {
  pca_res <- make_pca_result()

  it("returns a ggplot for Dim.1", {
    res <- var_contrib$create_var_contrib_plot(
      pca_res, dim = "Dim.1"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("returns a ggplot for Dim.2", {
    res <- var_contrib$create_var_contrib_plot(
      pca_res, dim = "Dim.2"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("includes title when show_title = TRUE", {
    res <- var_contrib$create_var_contrib_plot(
      pca_res, dim = "Dim.1", show_title = TRUE
    )
    expect_true(res$success)
    expect_true(!is.null(res$result$labels$title))
  })

  it("omits title when show_title = FALSE", {
    res <- var_contrib$create_var_contrib_plot(
      pca_res, dim = "Dim.1", show_title = FALSE
    )
    expect_true(res$success)
    expect_true(is.null(res$result$labels$title))
  })
})

# =============================================================================
# create_var_contrib_plot — error cases
# =============================================================================

describe("create_var_contrib_plot error cases", {
  pca_res <- make_pca_result()

  it("returns error for NULL pca_result", {
    res <- var_contrib$create_var_contrib_plot(
      NULL, dim = "Dim.1"
    )
    expect_false(res$success)
    expect_true(res$error$is_error)
  })

  it("returns error for invalid dimension", {
    res <- var_contrib$create_var_contrib_plot(
      pca_res, dim = "Dim.99"
    )
    expect_false(res$success)
    expect_true(res$error$is_error)
  })
})

# =============================================================================
# var_contrib_error_parser
# =============================================================================

describe("var_contrib_error_parser", {
  it("parses dimension errors", {
    msg <- var_contrib$var_contrib_error_parser(
      "Dimension not found: Dim.99"
    )
    expect_true(grepl("dimension", msg, ignore.case = TRUE))
  })

  it("parses NULL pca_result errors", {
    msg <- var_contrib$var_contrib_error_parser(
      "pca_result is NULL"
    )
    expect_true(grepl("PCA result", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- var_contrib$var_contrib_error_parser(
      "something unexpected"
    )
    expect_equal(
      msg,
      "Variable Contribution Plot failed: something unexpected"
    )
  })
})

# =============================================================================
# Internal helpers via namespace
# =============================================================================

describe("build_var_contrib_data", {
  pca_res <- make_pca_result()

  it("returns data frame with expected columns", {
    df <- impl$build_var_contrib_data(pca_res, "Dim.1")
    expect_true(is.data.frame(df))
    expect_true(all(c(
      "variable", "contrib", "above_avg",
      "tooltip", "data_id"
    ) %in% names(df)))
    expect_equal(nrow(df), 4)
  })

  it("above_avg reflects expected average threshold", {
    df <- impl$build_var_contrib_data(pca_res, "Dim.1")
    threshold <- impl$expected_average(pca_res)
    above <- df$contrib >= threshold
    expect_equal(
      df$above_avg == "Above average",
      above
    )
  })

  it("contributions are non-negative", {
    df <- impl$build_var_contrib_data(pca_res, "Dim.1")
    expect_true(all(df$contrib >= 0))
  })
})

describe("expected_average", {
  pca_res <- make_pca_result()

  it("returns 100/p for 4 variables", {
    avg <- impl$expected_average(pca_res)
    expect_equal(avg, 25)
  })
})

describe("axis_label_with_variance", {
  pca_res <- make_pca_result()

  it("includes variance percentage", {
    label <- impl$axis_label_with_variance(
      "Dim.1", pca_res$eig
    )
    expect_true(grepl("Dim\\.1", label))
    expect_true(grepl("%", label))
  })

  it("falls back for unknown dimension", {
    label <- impl$axis_label_with_variance(
      "Dim.99", pca_res$eig
    )
    expect_equal(label, "Dim.99")
  })
})
