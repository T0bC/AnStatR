box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/kmo,
)

# =============================================================================
# calculate_kmo
# =============================================================================

describe("calculate_kmo", {
  it("returns success with valid numeric data", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(50),
      b = rnorm(50),
      c = rnorm(50),
      d = rnorm(50)
    )
    result <- kmo$calculate_kmo(data)
    expect_true(result$success)
    expect_true(!is.null(result$result$overall))
    expect_true(!is.null(result$result$individual))
    expect_true(
      result$result$overall >= 0 && result$result$overall <= 1
    )
    expect_equal(length(result$result$individual), 4)
  })

  it("returns individual KMO for each variable", {
    set.seed(123)
    data <- data.frame(
      x = rnorm(50),
      y = rnorm(50),
      z = rnorm(50)
    )
    result <- kmo$calculate_kmo(data)
    expect_true(result$success)
    expect_equal(
      sort(names(result$result$individual)),
      c("x", "y", "z")
    )
  })

  it("returns error for non-numeric columns", {
    data <- data.frame(
      a = letters[1:10],
      b = LETTERS[1:10],
      stringsAsFactors = FALSE
    )
    result <- kmo$calculate_kmo(data)
    expect_false(result$success)
    expect_true(result$error$is_error)
  })

  it("returns error for constant columns (NaN MSA)", {
    data <- data.frame(a = rep(1, 10), b = rep(2, 10))
    result <- suppressWarnings(kmo$calculate_kmo(data))
    expect_false(result$success)
    expect_true(result$error$is_error)
    expect_true(grepl("\\ba\\b", result$error$message))
    expect_true(grepl("\\bb\\b", result$error$message))
  })

  it("includes context in error objects", {
    data <- data.frame(a = rep(1, 10), b = rep(2, 10))
    result <- suppressWarnings(kmo$calculate_kmo(data))
    expect_false(result$success)
    expect_true(!is.null(result$error$context))
    expect_equal(result$error$context$n_variables, 2)
    expect_equal(result$error$context$n_observations, 10)
    expect_true(!is.null(
      result$error$context$problematic_variables
    ))
  })
})

# =============================================================================
# kmo_error_parser
# =============================================================================

describe("kmo_error_parser", {
  it("parses singular matrix errors", {
    msg <- kmo$kmo_error_parser(
      "matrix is singular", "KMO"
    )
    expect_true(grepl("singular", msg, ignore.case = TRUE))
  })

  it("parses missing value errors", {
    msg <- kmo$kmo_error_parser(
      "missing values in data", "KMO"
    )
    expect_true(grepl("missing", msg, ignore.case = TRUE))
  })

  it("parses non-numeric errors", {
    msg <- kmo$kmo_error_parser(
      "data must be numeric", "KMO"
    )
    expect_true(grepl("numeric", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- kmo$kmo_error_parser(
      "something unexpected", "KMO"
    )
    expect_equal(
      msg, "KMO calculation failed: something unexpected"
    )
  })
})

# =============================================================================
# kmo_interpretation
# =============================================================================

describe("kmo_interpretation", {
  it("returns Marvelous for >= 0.9", {
    expect_equal(kmo$kmo_interpretation(0.95), "Marvelous")
    expect_equal(kmo$kmo_interpretation(0.9), "Marvelous")
  })

  it("returns Meritorious for >= 0.8", {
    expect_equal(kmo$kmo_interpretation(0.85), "Meritorious")
    expect_equal(kmo$kmo_interpretation(0.8), "Meritorious")
  })

  it("returns Middling for >= 0.7", {
    expect_equal(kmo$kmo_interpretation(0.75), "Middling")
    expect_equal(kmo$kmo_interpretation(0.7), "Middling")
  })

  it("returns Mediocre for >= 0.6", {
    expect_equal(kmo$kmo_interpretation(0.65), "Mediocre")
    expect_equal(kmo$kmo_interpretation(0.6), "Mediocre")
  })

  it("returns Miserable for >= 0.5", {
    expect_equal(kmo$kmo_interpretation(0.55), "Miserable")
    expect_equal(kmo$kmo_interpretation(0.5), "Miserable")
  })

  it("returns Unacceptable for < 0.5", {
    expect_equal(kmo$kmo_interpretation(0.4), "Unacceptable")
    expect_equal(kmo$kmo_interpretation(0.0), "Unacceptable")
  })

  it("returns N/A for NaN or NA", {
    expect_equal(kmo$kmo_interpretation(NaN), "N/A")
    expect_equal(kmo$kmo_interpretation(NA), "N/A")
  })
})

# =============================================================================
# kmo_badge_class
# =============================================================================

describe("kmo_badge_class", {
  it("returns bg-success for >= 0.8", {
    expect_equal(kmo$kmo_badge_class(0.9), "bg-success")
    expect_equal(kmo$kmo_badge_class(0.8), "bg-success")
  })

  it("returns bg-warning for >= 0.6", {
    expect_equal(
      kmo$kmo_badge_class(0.7), "bg-warning text-dark"
    )
    expect_equal(
      kmo$kmo_badge_class(0.6), "bg-warning text-dark"
    )
  })

  it("returns bg-danger for < 0.6", {
    expect_equal(kmo$kmo_badge_class(0.5), "bg-danger")
    expect_equal(kmo$kmo_badge_class(0.3), "bg-danger")
  })

  it("returns bg-secondary for NaN or NA", {
    expect_equal(kmo$kmo_badge_class(NaN), "bg-secondary")
    expect_equal(kmo$kmo_badge_class(NA), "bg-secondary")
  })
})
