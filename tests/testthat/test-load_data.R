box::use(
  testthat[describe, expect_equal, expect_false, expect_null,
           expect_true, it, test_that],
)

box::use(
  app/logic/error_handling,
  app/logic/load_data,
)

# Helper: resolve path to test fixture files
fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}

# =============================================================================
# validate_file_extension
# =============================================================================

describe("validate_file_extension", {
  it("accepts .csv files", {
    result <- load_data$validate_file_extension("data.csv")
    expect_true(result$valid)
    expect_equal(result$ext, "csv")
  })

  it("accepts .xlsx files", {
    result <- load_data$validate_file_extension("testData_with_perc.xlsx")
    expect_true(result$valid)
    expect_equal(result$ext, "xlsx")
  })

  it("accepts uppercase extensions", {
    result <- load_data$validate_file_extension("DATA.CSV")
    expect_true(result$valid)
    expect_equal(result$ext, "csv")
  })

  it("rejects unsupported extensions", {
    result <- load_data$validate_file_extension("image.png")
    expect_false(result$valid)
    expect_equal(result$ext, "png")
  })

  it("rejects files with no extension", {
    result <- load_data$validate_file_extension("noext")
    expect_false(result$valid)
  })
})

# =============================================================================
# normalize_quote_char
# =============================================================================

describe("normalize_quote_char", {
  it("returns empty string for NULL input", {
    expect_equal(load_data$normalize_quote_char(NULL), "")
  })

  it("returns empty string for empty string input", {
    expect_equal(load_data$normalize_quote_char(""), "")
  })

  it("returns empty string for 'None'", {
    expect_equal(load_data$normalize_quote_char("None"), "")
  })

  it("passes through double quote", {
    expect_equal(load_data$normalize_quote_char('"'), '"')
  })

  it("passes through single quote", {
    expect_equal(load_data$normalize_quote_char("'"), "'")
  })
})

# =============================================================================
# read_data_file — CSV
# =============================================================================

describe("read_data_file with CSV", {
  it("reads a valid comma-delimited CSV", {
    result <- load_data$read_data_file(
      path = fixture_path("valid.csv"),
      ext = "csv",
      header = TRUE,
      delimiter = ",",
      quote_char = '"'
    )
    expect_true(result$success)
    expect_null(result$error)
    expect_equal(nrow(result$data), 4)
    expect_equal(ncol(result$data), 3)
    expect_equal(names(result$data), c("name", "value", "group"))
  })

  it("reads a semicolon-delimited CSV", {
    result <- load_data$read_data_file(
      path = fixture_path("semicolon.csv"),
      ext = "csv",
      header = TRUE,
      delimiter = ";",
      quote_char = '"'
    )
    expect_true(result$success)
    expect_equal(nrow(result$data), 2)
    expect_equal(names(result$data), c("name", "value", "group"))
  })

  it("returns structured error for non-existent file", {
    result <- load_data$read_data_file(
      path = "does_not_exist.csv",
      ext = "csv"
    )
    expect_false(result$success)
    expect_null(result$data)
    expect_true(error_handling$is_app_error(result$error))
    expect_equal(result$error$operation_name, "Data Import")
    expect_true(nchar(result$error$message) > 0)
  })
})

# =============================================================================
# validate_data
# =============================================================================

describe("validate_data", {
  it("accepts a valid data.frame", {
    df <- data.frame(a = 1:3, b = letters[1:3])
    result <- load_data$validate_data(df)
    expect_true(result$valid)
    expect_null(result$error)
  })

  it("rejects an empty data.frame with structured error", {
    df <- data.frame(a = character(0), b = numeric(0))
    result <- load_data$validate_data(df)
    expect_false(result$valid)
    expect_true(error_handling$is_app_error(result$error))
    expect_true(nchar(result$error$message) > 0)
  })

  it("rejects a non-data.frame with structured error", {
    result <- load_data$validate_data("not a data frame")
    expect_false(result$valid)
    expect_true(error_handling$is_app_error(result$error))
  })

  it("rejects NULL with structured error", {
    result <- load_data$validate_data(NULL)
    expect_false(result$valid)
    expect_true(error_handling$is_app_error(result$error))
  })
})
