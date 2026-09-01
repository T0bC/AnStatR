box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/scaling,
)

# =============================================================================
# scale_data
# =============================================================================

describe("scale_data", {
  it("returns success with scaled data", {
    data <- data.frame(
      meta = c("a", "b", "c", "d"),
      x = c(10, 20, 30, 40),
      y = c(100, 200, 300, 400)
    )
    result <- scaling$scale_data(data, c("x", "y"))
    expect_true(result$success)
    expect_equal(ncol(result$result), 3)
    expect_equal(
      round(mean(result$result$x), 10), 0
    )
    expect_equal(
      round(sd(result$result$x), 10), 1
    )
  })

  it("preserves metadata columns unchanged", {
    data <- data.frame(
      meta = c("a", "b", "c"),
      x = c(10, 20, 30)
    )
    result <- scaling$scale_data(data, "x")
    expect_true(result$success)
    expect_equal(result$result$meta, c("a", "b", "c"))
  })

  it("preserves row count", {
    data <- data.frame(x = 1:10, y = 11:20)
    result <- scaling$scale_data(data, c("x", "y"))
    expect_true(result$success)
    expect_equal(nrow(result$result), 10)
  })

  it("returns error for constant columns (zero variance)", {
    data <- data.frame(
      x = c(5, 5, 5),
      y = c(1, 2, 3)
    )
    result <- scaling$scale_data(data, c("x", "y"))
    expect_false(result$success)
    expect_true(result$error$is_error)
    expect_true(grepl("zero variance", result$error$message,
                       ignore.case = TRUE))
  })

  it("centers only when scale = FALSE", {
    data <- data.frame(x = c(10, 20, 30))
    result <- scaling$scale_data(
      data, "x", center = TRUE, scale = FALSE
    )
    expect_true(result$success)
    expect_equal(
      round(mean(result$result$x), 10), 0
    )
    expect_true(sd(result$result$x) != 1)
  })

  it("scales only when center = FALSE", {
    data <- data.frame(x = c(10, 20, 30))
    result <- scaling$scale_data(
      data, "x", center = FALSE, scale = TRUE
    )
    expect_true(result$success)
    expect_true(mean(result$result$x) != 0)
  })
})

# =============================================================================
# scaling_error_parser
# =============================================================================

describe("scaling_error_parser", {
  it("parses zero variance errors", {
    msg <- scaling$scaling_error_parser(
      "zero variance columns found"
    )
    expect_true(grepl("zero variance", msg,
                       ignore.case = TRUE))
  })

  it("returns generic message for unknown errors", {
    msg <- scaling$scaling_error_parser("something broke")
    expect_true(grepl("something broke", msg))
  })
})

# =============================================================================
# residualize_data
# =============================================================================

describe("residualize_data", {
  it("returns success and centers each group at zero mean", {
    data <- data.frame(
      site = c("A", "A", "A", "B", "B", "B"),
      x = c(10, 12, 14, 100, 102, 104)
    )
    result <- scaling$residualize_data(data, "x", "site")
    expect_true(result$success)

    resid <- result$result$x
    grp_a_mean <- mean(resid[data$site == "A"])
    grp_b_mean <- mean(resid[data$site == "B"])
    expect_equal(round(grp_a_mean, 10), 0)
    expect_equal(round(grp_b_mean, 10), 0)
  })

  it("preserves metadata columns unchanged", {
    data <- data.frame(
      meta = c("a", "b", "c", "d"),
      site = c("X", "X", "Y", "Y"),
      x = c(10, 20, 30, 40)
    )
    result <- scaling$residualize_data(data, "x", "site")
    expect_true(result$success)
    expect_equal(result$result$meta, c("a", "b", "c", "d"))
  })

  it("preserves row count", {
    data <- data.frame(
      site = rep(c("X", "Y"), 5),
      x = 1:10, y = 11:20
    )
    result <- scaling$residualize_data(
      data, c("x", "y"), "site"
    )
    expect_true(result$success)
    expect_equal(nrow(result$result), 10)
  })

  it("residualizes multiple columns independently", {
    data <- data.frame(
      site = c("A", "A", "B", "B"),
      x = c(10, 20, 100, 200),
      y = c(1, 3, 5, 7)
    )
    result <- scaling$residualize_data(
      data, c("x", "y"), "site"
    )
    expect_true(result$success)
    expect_equal(
      round(mean(result$result$x[data$site == "A"]), 10), 0
    )
    expect_equal(
      round(mean(result$result$y[data$site == "B"]), 10), 0
    )
  })

  it("returns error when grouping column is not found", {
    data <- data.frame(x = c(1, 2, 3, 4))
    result <- scaling$residualize_data(data, "x", "missing_col")
    expect_false(result$success)
    expect_true(result$error$is_error)
    expect_true(grepl("not found", result$error$message,
                       ignore.case = TRUE))
  })

  it("returns error when grouping column has fewer than 2 levels", {
    data <- data.frame(
      site = c("A", "A", "A"),
      x = c(1, 2, 3)
    )
    result <- scaling$residualize_data(data, "x", "site")
    expect_false(result$success)
    expect_true(grepl("only one group", result$error$message,
                       ignore.case = TRUE))
  })

  it("returns error when grouping column has missing values", {
    # ave() treats an NA group as its own singleton group,
    # which would silently zero out that row rather than
    # flagging it -- residualize_data() must reject this
    # upfront instead of producing a silently wrong value.
    data <- data.frame(
      site = c("A", "A", NA, "B", "B"),
      x = c(10, 12, 50, 100, 102)
    )
    result <- scaling$residualize_data(data, "x", "site")
    expect_false(result$success)
    expect_true(grepl("missing values", result$error$message,
                       ignore.case = TRUE))
  })
})

# =============================================================================
# residualize_error_parser
# =============================================================================

describe("residualize_error_parser", {
  it("parses not-found errors", {
    msg <- scaling$residualize_error_parser(
      "Grouping column 'site' not found in data"
    )
    expect_true(grepl("not found", msg, ignore.case = TRUE))
  })

  it("parses too-few-levels errors", {
    msg <- scaling$residualize_error_parser(
      "Grouping column 'site' has fewer than 2 levels; nothing to residualize"
    )
    expect_true(grepl("only one group", msg, ignore.case = TRUE))
  })

  it("parses missing-values errors", {
    msg <- scaling$residualize_error_parser(
      "Grouping column 'site' contains missing values; ave() ..."
    )
    expect_true(grepl("missing values", msg, ignore.case = TRUE))
  })

  it("returns generic message for unknown errors", {
    msg <- scaling$residualize_error_parser("something broke")
    expect_true(grepl("something broke", msg))
  })
})
