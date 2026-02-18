box::use(
  testthat[describe, expect_equal, expect_false, expect_null,
           expect_true, it, test_that],
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# is_app_error
# =============================================================================

describe("is_app_error", {
  it("returns TRUE for a structured error object", {
    err <- list(is_error = TRUE, message = "fail")
    expect_true(error_handling$is_app_error(err))
  })

  it("returns FALSE for a regular list", {
    expect_false(error_handling$is_app_error(list(a = 1, b = 2)))
  })

  it("returns FALSE for a data.frame", {
    df <- data.frame(is_error = TRUE, x = 1)
    expect_false(error_handling$is_app_error(df))
  })

  it("returns FALSE for NULL", {
    expect_false(error_handling$is_app_error(NULL))
  })

  it("returns FALSE for a string", {
    expect_false(error_handling$is_app_error("error"))
  })

  it("returns FALSE when is_error is FALSE", {
    expect_false(error_handling$is_app_error(list(is_error = FALSE)))
  })
})

# =============================================================================
# create_app_error
# =============================================================================

describe("create_app_error", {
  it("creates a structured error with all fields", {
    err <- error_handling$create_app_error(
      user_msg = "Something went wrong",
      raw_msg = "underlying R error",
      operation_name = "Test Op",
      context = list(file = "data.csv", rows = 100)
    )
    expect_true(err$is_error)
    expect_equal(err$message, "Something went wrong")
    expect_equal(err$raw_message, "underlying R error")
    expect_equal(err$operation_name, "Test Op")
    expect_equal(err$context$file, "data.csv")
    expect_equal(err$context$rows, 100)
    expect_true(!is.null(err$timestamp))
  })

  it("has NULL stack_trace when no error_obj provided", {
    err <- error_handling$create_app_error(
      user_msg = "validation fail",
      operation_name = "Validation"
    )
    expect_null(err$traces$stack_trace)
  })

  it("is recognized by is_app_error", {
    err <- error_handling$create_app_error(
      user_msg = "test",
      operation_name = "Test"
    )
    expect_true(error_handling$is_app_error(err))
  })
})

# =============================================================================
# simple_error
# =============================================================================

describe("simple_error", {
  it("creates a structured error without stack trace", {
    err <- error_handling$simple_error(
      message = "No data available",
      operation_name = "Data Check",
      context = list(filter = TRUE)
    )
    expect_true(error_handling$is_app_error(err))
    expect_equal(err$message, "No data available")
    expect_equal(err$operation_name, "Data Check")
    expect_equal(err$raw_message, "No data available")
    expect_null(err$traces$stack_trace)
  })

  it("uses default operation_name 'Validation'", {
    err <- error_handling$simple_error(message = "bad input")
    expect_equal(err$operation_name, "Validation")
  })
})

# =============================================================================
# safe_execute
# =============================================================================

describe("safe_execute", {
  it("returns success for a valid expression", {
    result <- error_handling$safe_execute(
      expr = 1 + 1,
      operation_name = "Addition"
    )
    expect_true(result$success)
    expect_equal(result$result, 2)
    expect_null(result$error)
  })

  it("returns structured error for a failing expression", {
    result <- error_handling$safe_execute(
      expr = stop("intentional failure"),
      operation_name = "Failing Op",
      context = list(step = "test")
    )
    expect_false(result$success)
    expect_null(result$result)
    expect_true(error_handling$is_app_error(result$error))
    expect_equal(result$error$operation_name, "Failing Op")
    expect_equal(result$error$raw_message, "intentional failure")
    expect_equal(result$error$context$step, "test")
  })

  it("uses custom error_parser when provided", {
    my_parser <- function(error_msg, operation_name) {
      paste0("Custom: ", error_msg)
    }
    result <- error_handling$safe_execute(
      expr = stop("boom"),
      operation_name = "Parsed Op",
      error_parser = my_parser
    )
    expect_false(result$success)
    expect_equal(result$error$message, "Custom: boom")
  })

  it("uses default message format when no parser provided", {
    result <- error_handling$safe_execute(
      expr = stop("oops"),
      operation_name = "Default Op"
    )
    expect_equal(result$error$message, "Default Op failed: oops")
  })
})

# =============================================================================
# default_error_parser
# =============================================================================

describe("default_error_parser", {
  it("parses file-not-found errors", {
    msg <- error_handling$default_error_parser(
      "cannot open file 'x.csv'", "Import"
    )
    expect_true(grepl("File not found", msg))
  })

  it("parses permission errors", {
    msg <- error_handling$default_error_parser(
      "permission denied", "Import"
    )
    expect_true(grepl("Permission denied", msg))
  })

  it("parses memory errors", {
    msg <- error_handling$default_error_parser(
      "cannot allocate vector", "Compute"
    )
    expect_true(grepl("Out of memory", msg))
  })

  it("falls back to raw message for unknown errors", {
    msg <- error_handling$default_error_parser(
      "something weird", "Op"
    )
    expect_equal(msg, "Op failed: something weird")
  })
})

# =============================================================================
# stat_error_parser
# =============================================================================

describe("stat_error_parser", {
  it("parses group-related errors", {
    msg <- error_handling$stat_error_parser(
      "groups must have at least 2 levels", "t1way"
    )
    expect_true(grepl("Insufficient groups", msg))
  })

  it("parses sample size errors", {
    msg <- error_handling$stat_error_parser(
      "insufficient sample size", "t1way"
    )
    expect_true(grepl("Insufficient sample size", msg))
  })

  it("parses NA-related errors", {
    msg <- error_handling$stat_error_parser(
      "missing values in data with NAs", "t1way"
    )
    expect_true(grepl("missing values", msg))
  })

  it("parses variance errors", {
    msg <- error_handling$stat_error_parser(
      "zero variance in group", "t1way"
    )
    expect_true(grepl("zero variance", msg))
  })

  it("falls back for unknown stat errors", {
    msg <- error_handling$stat_error_parser(
      "unknown stat error", "t1way"
    )
    expect_equal(msg, "t1way failed: unknown stat error")
  })
})
