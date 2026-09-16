box::use(
  testthat[
    describe,
    expect_false,
    expect_gt,
    expect_null,
    expect_true,
    it
  ],
)

box::use(
  app/logic/load_data/example_data,
)

# =============================================================================
# list_examples
# =============================================================================

describe("list_examples", {
  it("returns a named character vector", {
    examples <- example_data$list_examples()
    expect_true(is.character(examples))
    expect_gt(length(examples), 0)
    expect_false(is.null(names(examples)))
  })

  it("includes iris, wine, and penguins", {
    examples <- example_data$list_examples()
    expect_true("iris.xlsx" %in% examples)
    expect_true("wine.xlsx" %in% examples)
    expect_true("penguins.xlsx" %in% examples)
  })

  it("has display names as names", {
    examples <- example_data$list_examples()
    expect_true("Iris" %in% names(examples))
    expect_true("Wine" %in% names(examples))
    expect_true("Penguins" %in% names(examples))
  })
})

# =============================================================================
# example_path
# =============================================================================

describe("example_path", {
  it("returns a path ending with the filename", {
    path <- example_data$example_path("iris.xlsx")
    expect_true(grepl("iris\\.xlsx$", path))
  })

  it("returns a path under the example_data directory", {
    path <- example_data$example_path("wine.xlsx")
    expect_true(grepl("example_data", path))
  })
})

# =============================================================================
# load_example
# =============================================================================

describe("load_example", {
  it("loads iris.xlsx successfully", {
    result <- example_data$load_example("iris.xlsx")
    expect_true(result$success)
    expect_null(result$error)
    expect_true(is.data.frame(result$data))
    expect_gt(nrow(result$data), 0)
  })

  it("loads wine.xlsx successfully", {
    result <- example_data$load_example("wine.xlsx")
    expect_true(result$success)
    expect_null(result$error)
    expect_true(is.data.frame(result$data))
    expect_gt(nrow(result$data), 0)
  })

  it("loads penguins.xlsx successfully", {
    result <- example_data$load_example("penguins.xlsx")
    expect_true(result$success)
    expect_null(result$error)
    expect_true(is.data.frame(result$data))
    expect_gt(nrow(result$data), 0)
  })

  it("returns error for non-existent file", {
    result <- example_data$load_example("does_not_exist.xlsx")
    expect_false(result$success)
    expect_null(result$data)
    expect_false(is.null(result$error))
  })
})
