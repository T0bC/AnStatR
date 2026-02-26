box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/var_contrib_jitter,
  app/logic/pca/pca,
)

impl <- attr(var_contrib_jitter, "namespace")

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
# create_var_contrib_jitter_plot
# =============================================================================

describe("create_var_contrib_jitter_plot", {
  pca_res <- make_pca_result()

  it("returns a ggplot with default settings", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      pca_res, display_ncp = 2L
    )
    expect_true(res$success)
    expect_true(inherits(res$result$plot, "ggplot"))
  })

  it("includes title when show_title = TRUE", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      pca_res, display_ncp = 2L, show_title = TRUE
    )
    expect_true(res$success)
    expect_true(!is.null(res$result$plot$labels$title))
  })

  it("omits title when show_title = FALSE", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      pca_res, display_ncp = 2L, show_title = FALSE
    )
    expect_true(res$success)
    expect_true(is.null(res$result$plot$labels$title))
  })

  it("clamps display_ncp to available dims", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      pca_res, display_ncp = 10L
    )
    expect_true(res$success)
    expect_true(res$result$n_dims_shown <= 4L)
  })

  it("returns filter metadata", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      pca_res, display_ncp = 3L
    )
    expect_true(res$success)
    expect_true("filter_applied" %in% names(res$result))
    expect_true("n_vars_total" %in% names(res$result))
  })
})

# =============================================================================
# create_var_contrib_jitter_plot — error cases
# =============================================================================

describe("create_var_contrib_jitter_plot error cases", {
  it("returns error for NULL pca_result", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      NULL, display_ncp = 2L
    )
    expect_false(res$success)
    expect_true(res$error$is_error)
  })
})

# =============================================================================
# var_contrib_jitter_error_parser
# =============================================================================

describe("var_contrib_jitter_error_parser", {
  it("parses dimension errors", {
    msg <- var_contrib_jitter$var_contrib_jitter_error_parser(
      "Dimension not found: Dim.99"
    )
    expect_true(grepl("dimension", msg, ignore.case = TRUE))
  })

  it("parses NULL pca_result errors", {
    msg <- var_contrib_jitter$var_contrib_jitter_error_parser(
      "pca_result is NULL"
    )
    expect_true(grepl("PCA result", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- var_contrib_jitter$var_contrib_jitter_error_parser(
      "something unexpected"
    )
    expect_equal(
      msg,
      "Variable Contribution Jitter Plot failed: something unexpected"
    )
  })
})

# =============================================================================
# Internal helpers via namespace
# =============================================================================

describe("select_label_vars", {
  pca_res <- make_pca_result()

  it("labels all variables when n_vars <= 10", {
    res <- var_contrib_jitter$create_var_contrib_jitter_plot(
      pca_res, display_ncp = 2L
    )
    expect_true(res$success)
    # 4 variables < 10, so all should get labels
    plot_data <- res$result$plot$data
    expect_true(all(plot_data$show_label))
  })
})
