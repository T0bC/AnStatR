box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/correlation_plot,
)

impl <- attr(correlation_plot, "namespace")

# =============================================================================
# compute_correlation_data
# =============================================================================

describe("compute_correlation_data", {
  it("returns success with valid numeric data", {
    data <- data.frame(a = 1:10, b = 10:1, c = rnorm(10))
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "b", "c")
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$cor_long))
    expect_true(!is.null(result$result$ordered_cols))
    expect_equal(length(result$result$ordered_cols), 3)
  })

  it("returns long-format data with expected columns", {
    data <- data.frame(x = 1:5, y = 5:1)
    result <- correlation_plot$compute_correlation_data(
      data, c("x", "y")
    )
    expect_true(result$success)
    cor_long <- result$result$cor_long
    expect_true("Var1" %in% names(cor_long))
    expect_true("Var2" %in% names(cor_long))
    expect_true("correlation" %in% names(cor_long))
    expect_true("tooltip" %in% names(cor_long))
    expect_true("data_id" %in% names(cor_long))
    # Lower triangle including diagonal: n*(n+1)/2 = 3
    expect_equal(nrow(cor_long), 3)
  })

  it("returns error for fewer than 2 columns", {
    data <- data.frame(a = 1:5)
    result <- correlation_plot$compute_correlation_data(
      data, c("a")
    )
    expect_false(result$success)
    expect_true(!is.null(result$error))
    expect_true(result$error$is_error)
  })

  it("returns error for NULL data", {
    result <- correlation_plot$compute_correlation_data(
      NULL, c("a", "b")
    )
    expect_false(result$success)
    expect_true(result$error$is_error)
  })

  it("returns error for empty data frame", {
    data <- data.frame(a = numeric(0), b = numeric(0))
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "b")
    )
    expect_false(result$success)
    expect_true(result$error$is_error)
  })

  it("returns error for non-numeric columns", {
    data <- data.frame(
      a = 1:5, b = letters[1:5],
      stringsAsFactors = FALSE
    )
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "b")
    )
    expect_false(result$success)
    expect_true(grepl(
      "numeric", result$error$message, ignore.case = TRUE
    ))
  })

  it("returns error for constant columns", {
    data <- data.frame(a = rep(1, 5), b = 1:5)
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "b")
    )
    expect_false(result$success)
    expect_true(grepl(
      "constant", result$error$message, ignore.case = TRUE
    ))
  })

  it("returns error for missing columns", {
    data <- data.frame(a = 1:5, b = 5:1)
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "z")
    )
    expect_false(result$success)
    expect_true(result$error$is_error)
  })

  it("handles NA values with pairwise complete obs", {
    data <- data.frame(
      a = c(1, 2, NA, 4, 5),
      b = c(5, NA, 3, 2, 1),
      c = c(1, 2, 3, 4, 5)
    )
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "b", "c")
    )
    expect_true(result$success)
  })

  it("orders columns by hierarchical clustering", {
    set.seed(42)
    data <- data.frame(
      a = 1:20,
      b = 1:20 + rnorm(20, sd = 0.1),
      c = 20:1
    )
    result <- correlation_plot$compute_correlation_data(
      data, c("a", "b", "c")
    )
    expect_true(result$success)
    ordered <- result$result$ordered_cols
    expect_equal(length(ordered), 3)
    expect_true(all(ordered %in% c("a", "b", "c")))
  })

  it("includes context in error objects", {
    result <- correlation_plot$compute_correlation_data(
      NULL, c("a", "b")
    )
    expect_false(result$success)
    expect_true(!is.null(result$error$context))
    expect_equal(result$error$context$n_columns, 2)
  })
})

# =============================================================================
# correlation_error_parser
# =============================================================================

describe("correlation_error_parser", {
  it("parses missing value errors", {
    msg <- correlation_plot$correlation_error_parser(
      "missing values in data", "Correlation Plot"
    )
    expect_true(grepl("missing values", msg, ignore.case = TRUE))
  })

  it("parses non-numeric errors", {
    msg <- correlation_plot$correlation_error_parser(
      "All columns must be numeric", "Correlation Plot"
    )
    expect_true(grepl("numeric", msg))
  })

  it("parses constant column errors", {
    msg <- correlation_plot$correlation_error_parser(
      "constant columns found", "Correlation Plot"
    )
    expect_true(grepl("constant", msg, ignore.case = TRUE))
  })

  it("parses column count errors", {
    msg <- correlation_plot$correlation_error_parser(
      "At least 2 measurement columns required",
      "Correlation Plot"
    )
    expect_true(grepl("2 measurement", msg))
  })

  it("falls back for unknown errors", {
    msg <- correlation_plot$correlation_error_parser(
      "something unexpected", "Correlation Plot"
    )
    expect_equal(
      msg, "Correlation Plot failed: something unexpected"
    )
  })
})

# =============================================================================
# Internal helpers via namespace
# =============================================================================

describe("validate_correlation_inputs", {
  it("passes for valid data", {
    data <- data.frame(a = 1:5, b = 5:1)
    expect_true(
      impl$validate_correlation_inputs(data, c("a", "b"))
    )
  })

  it("stops for NULL data", {
    expect_error(
      impl$validate_correlation_inputs(NULL, c("a", "b"))
    )
  })

  it("stops for single column", {
    data <- data.frame(a = 1:5)
    expect_error(
      impl$validate_correlation_inputs(data, c("a"))
    )
  })
})

describe("cor_matrix_to_long", {
  it("converts matrix to long format correctly", {
    mat <- matrix(
      c(1, 0.5, 0.5, 1), nrow = 2,
      dimnames = list(c("a", "b"), c("a", "b"))
    )
    result <- impl$cor_matrix_to_long(mat, c("a", "b"))
    # Lower triangle including diagonal: n*(n+1)/2 = 3
    expect_equal(nrow(result), 3)
    expect_true(is.factor(result$Var1))
    expect_true(is.factor(result$Var2))
  })
})

describe("cluster_columns", {
  it("returns all columns in reordered form", {
    mat <- matrix(
      c(1, -0.9, 0.1, -0.9, 1, -0.1, 0.1, -0.1, 1),
      nrow = 3,
      dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
    )
    result <- impl$cluster_columns(mat, c("a", "b", "c"))
    expect_equal(length(result), 3)
    expect_true(all(result %in% c("a", "b", "c")))
  })
})
