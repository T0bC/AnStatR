box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/cluster/hopkins,
)

impl <- attr(hopkins, "namespace")

# =============================================================================
# compute_hopkins
# =============================================================================

describe("compute_hopkins", {
  it("returns success with valid data (n > 100)", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(150),
      b = rnorm(150),
      c = rnorm(150)
    )
    result <- hopkins$compute_hopkins(data, c("a", "b", "c"))
    expect_true(result$success)
    expect_true(result$result$H >= 0 && result$result$H <= 1)
    expect_equal(result$result$n, 150)
    expect_equal(result$result$n_dims, 3)
  })

  it("returns success with small data (n < 100)", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(30),
      b = rnorm(30)
    )
    result <- hopkins$compute_hopkins(data, c("a", "b"))
    expect_true(result$success)
    expect_equal(result$result$n, 30)
    expect_true(!is.null(result$result$warnings$small_n))
  })

  it("uses m = 10% of n for large datasets", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(200),
      b = rnorm(200)
    )
    result <- hopkins$compute_hopkins(data, c("a", "b"))
    expect_true(result$success)
    expect_equal(result$result$m, ceiling(200 * 0.1))
  })

  it("uses m = 5% of n for small datasets", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(50),
      b = rnorm(50)
    )
    result <- hopkins$compute_hopkins(data, c("a", "b"))
    expect_true(result$success)
    expect_equal(result$result$m, max(ceiling(50 * 0.05), 1))
  })

  it("returns error for NULL data", {
    result <- hopkins$compute_hopkins(NULL, c("a"))
    expect_true(!result$success)
  })

  it("returns error for empty columns", {
    data <- data.frame(a = 1:10)
    result <- hopkins$compute_hopkins(data, character(0))
    expect_true(!result$success)
  })

  it("returns error for non-numeric columns", {
    data <- data.frame(
      a = letters[1:10],
      b = 1:10,
      stringsAsFactors = FALSE
    )
    result <- hopkins$compute_hopkins(data, c("a", "b"))
    expect_true(!result$success)
  })

  it("returns error for missing columns", {
    data <- data.frame(a = 1:10)
    result <- hopkins$compute_hopkins(data, c("a", "z"))
    expect_true(!result$success)
  })

  it("includes high_dims warning for > 10 dimensions", {
    set.seed(42)
    cols <- paste0("v", 1:12)
    data <- as.data.frame(
      matrix(rnorm(150 * 12), ncol = 12)
    )
    names(data) <- cols
    result <- hopkins$compute_hopkins(data, cols)
    expect_true(result$success)
    expect_true(!is.null(result$result$warnings$high_dims))
  })
})

# =============================================================================
# interpret_hopkins (internal)
# =============================================================================

describe("interpret_hopkins", {
  it("returns success for H >= 0.75", {
    interp <- impl$interpret_hopkins(0.85)
    expect_equal(interp$level, "success")
    expect_equal(interp$label, "Highly clusterable")
  })

  it("returns warning for 0.5 <= H < 0.75", {
    interp <- impl$interpret_hopkins(0.6)
    expect_equal(interp$level, "warning")
    expect_equal(interp$label, "Moderately clusterable")
  })

  it("returns danger for H < 0.5", {
    interp <- impl$interpret_hopkins(0.3)
    expect_equal(interp$level, "danger")
    expect_equal(interp$label, "Not clusterable")
  })
})

# =============================================================================
# build_warnings (internal)
# =============================================================================

describe("build_warnings", {
  it("returns small_n warning when n <= 100", {
    warns <- impl$build_warnings(50, 3, 3)
    expect_true(!is.null(warns$small_n))
  })

  it("does not return small_n warning when n > 100", {
    warns <- impl$build_warnings(150, 3, 15)
    expect_true(is.null(warns$small_n))
  })

  it("returns high_dims warning when dims > 10", {
    warns <- impl$build_warnings(150, 12, 15)
    expect_true(!is.null(warns$high_dims))
  })

  it("does not return high_dims warning when dims <= 10", {
    warns <- impl$build_warnings(150, 5, 15)
    expect_true(is.null(warns$high_dims))
  })

  it("returns empty list when no warnings apply", {
    warns <- impl$build_warnings(150, 5, 15)
    expect_equal(length(warns), 0)
  })
})

# =============================================================================
# hopkins_error_parser
# =============================================================================

describe("hopkins_error_parser", {
  it("parses numeric errors", {
    msg <- hopkins$hopkins_error_parser(
      "non-numeric data", "Hopkins"
    )
    expect_true(grepl("numeric", msg, ignore.case = TRUE))
  })

  it("parses NA errors", {
    msg <- hopkins$hopkins_error_parser(
      "data contains NA values", "Hopkins"
    )
    expect_true(grepl("missing", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- hopkins$hopkins_error_parser(
      "something unexpected", "Hopkins"
    )
    expect_true(grepl("failed", msg))
  })
})
