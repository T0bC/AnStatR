box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/settings,
)

# =============================================================================
# get_version_info
# =============================================================================

describe("get_version_info", {
  it("returns a list with version and date", {
    info <- settings$get_version_info()
    expect_true(is.list(info))
    expect_true("version" %in% names(info))
    expect_true("date" %in% names(info))
  })

  it("returns valid version format or 'unknown'", {
    info <- settings$get_version_info()
    valid_version <- grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", info$version) ||
      info$version == "unknown"
    expect_true(valid_version)
  })

  it("returns valid date format or 'unknown'", {
    info <- settings$get_version_info()
    valid_date <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", info$date) ||
      info$date == "unknown"
    expect_true(valid_date)
  })
})

# =============================================================================
# get_version_string
# =============================================================================

describe("get_version_string", {
  it("returns a character string", {
    version_str <- settings$get_version_string()
    expect_true(is.character(version_str))
  })

  it("contains version and date in parentheses", {
    version_str <- settings$get_version_string()
    expect_true(grepl("\\(.*\\)", version_str))
  })
})

# =============================================================================
# get_changelog_markdown
# =============================================================================

describe("get_changelog_markdown", {
  it("returns a character string", {
    changelog <- settings$get_changelog_markdown()
    expect_true(is.character(changelog))
  })
})

# =============================================================================
# app_version
# =============================================================================

describe("app_version", {
  it("is a character string", {
    expect_true(is.character(settings$app_version))
  })
})

# =============================================================================
# get_default_theme
# =============================================================================

describe("get_default_theme", {
  it("returns a bs_theme object", {
    theme <- settings$get_default_theme()
    expect_true(inherits(theme, "bs_theme"))
  })
})
