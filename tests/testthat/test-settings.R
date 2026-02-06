box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/settings,
)

# =============================================================================
# available_themes
# =============================================================================

describe("available_themes", {
  it("contains 8 themes", {
    expect_equal(length(settings$available_themes), 8)
  })

  it("has named entries", {
    nms <- names(settings$available_themes)
    expect_true(all(nchar(nms) > 0))
  })
})

# =============================================================================
# default_theme_name
# =============================================================================

describe("default_theme_name", {
  it("is 'Cosmo (Light)'", {
    expect_equal(settings$default_theme_name, "Cosmo (Light)")
  })

  it("exists in available_themes", {
    expect_true(settings$default_theme_name %in% names(settings$available_themes))
  })
})

# =============================================================================
# get_theme_names
# =============================================================================

describe("get_theme_names", {
  it("returns a character vector of theme names", {
    nms <- settings$get_theme_names()
    expect_true(is.character(nms))
    expect_equal(length(nms), 8)
  })

  it("includes both light and dark themes", {
    nms <- settings$get_theme_names()
    expect_true(any(grepl("Light", nms)))
    expect_true(any(grepl("Dark", nms)))
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

# =============================================================================
# get_theme
# =============================================================================

describe("get_theme", {
  it("returns the correct theme by name", {
    theme <- settings$get_theme("Darkly (Dark)")
    expect_true(inherits(theme, "bs_theme"))
  })

  it("falls back to default for NULL", {
    theme <- settings$get_theme(NULL)
    expect_true(inherits(theme, "bs_theme"))
  })

  it("falls back to default for invalid name", {
    theme <- settings$get_theme("Nonexistent Theme")
    expect_true(inherits(theme, "bs_theme"))
  })
})
