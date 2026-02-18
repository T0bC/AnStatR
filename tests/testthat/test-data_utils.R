box::use(
  testthat[describe, expect_equal, expect_error, expect_length,
           expect_true, expect_false, it],
)

box::use(
  app/logic/data_utils,
)

# =============================================================================
# create_interaction
# =============================================================================

describe("create_interaction", {
  it("returns a factor for a single column", {
    df <- data.frame(species = c("cat", "dog", "cat"))
    result <- data_utils$create_interaction(df, "species")
    expect_true(is.factor(result))
    expect_equal(levels(result), c("cat", "dog"))
  })

  it("creates interaction for multiple columns", {
    df <- data.frame(
      species = c("cat", "dog", "cat"),
      diet = c("wet", "dry", "dry")
    )
    result <- data_utils$create_interaction(df, c("species", "diet"))
    expect_true(is.factor(result))
    expect_equal(length(result), 3)
    expect_true("cat.wet" %in% as.character(result))
    expect_true("cat.dry" %in% as.character(result))
    expect_true("dog.dry" %in% as.character(result))
  })

  it("replaces NA with 'NA' string", {
    df <- data.frame(species = c("cat", NA, "dog"))
    result <- data_utils$create_interaction(df, "species")
    expect_true("NA" %in% levels(result))
    expect_equal(as.character(result[2]), "NA")
  })

  it("errors on empty cols", {
    df <- data.frame(a = 1)
    expect_error(
      data_utils$create_interaction(df, character(0)),
      "At least one column"
    )
  })

  it("drops unused interaction levels", {
    df <- data.frame(
      a = c("x", "x", "y"),
      b = c("1", "2", "1")
    )
    result <- data_utils$create_interaction(df, c("a", "b"))
    # y.2 is not present in data, should be dropped
    expect_false("y.2" %in% levels(result))
  })
})

# =============================================================================
# get_filter_choices
# =============================================================================

describe("get_filter_choices", {
  it("returns unique values as character", {
    result <- data_utils$get_filter_choices(c("a", "b", "a", "c"))
    expect_equal(sort(result), c("a", "b", "c"))
  })

  it("replaces NA with 'NA' string at end", {
    result <- data_utils$get_filter_choices(c("a", NA, "b"))
    expect_true("NA" %in% result)
    expect_equal(result[length(result)], "NA")
  })

  it("handles all-NA input", {
    result <- data_utils$get_filter_choices(c(NA, NA))
    expect_equal(result, "NA")
  })

  it("handles numeric values", {
    result <- data_utils$get_filter_choices(c(1, 2, 1, 3))
    expect_equal(sort(result), c("1", "2", "3"))
  })
})

# =============================================================================
# filter_data
# =============================================================================

describe("filter_data", {
  sample_df <- data.frame(
    species = c("cat", "dog", "cat", "bird"),
    diet = c("wet", "dry", "dry", "seed"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  it("returns data unchanged with empty filters", {
    result <- data_utils$filter_data(sample_df, list())
    expect_equal(nrow(result), 4)
  })

  it("filters by a single column", {
    result <- data_utils$filter_data(
      sample_df, list(species = "cat")
    )
    expect_equal(nrow(result), 2)
    expect_true(all(result$species == "cat"))
  })

  it("filters by multiple values in one column", {
    result <- data_utils$filter_data(
      sample_df, list(species = c("cat", "bird"))
    )
    expect_equal(nrow(result), 3)
  })

  it("filters by multiple columns (intersection)", {
    result <- data_utils$filter_data(
      sample_df,
      list(species = c("cat", "dog"), diet = "dry")
    )
    expect_equal(nrow(result), 2)
    expect_true(all(result$diet == "dry"))
  })

  it("handles NA values with 'NA' marker", {
    df_na <- data.frame(
      species = c("cat", NA, "dog", NA),
      value = 1:4,
      stringsAsFactors = FALSE
    )

    # Select only NA rows
    result <- data_utils$filter_data(
      df_na, list(species = "NA")
    )
    expect_equal(nrow(result), 2)
    expect_true(all(is.na(result$species)))
  })

  it("handles NA values mixed with regular values", {
    df_na <- data.frame(
      species = c("cat", NA, "dog", NA),
      value = 1:4,
      stringsAsFactors = FALSE
    )

    result <- data_utils$filter_data(
      df_na, list(species = c("cat", "NA"))
    )
    expect_equal(nrow(result), 3)
  })

  it("excludes NA rows when 'NA' not selected", {
    df_na <- data.frame(
      species = c("cat", NA, "dog"),
      value = 1:3,
      stringsAsFactors = FALSE
    )

    result <- data_utils$filter_data(
      df_na, list(species = "cat")
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$species, "cat")
  })

  it("skips columns with NULL selections", {
    result <- data_utils$filter_data(
      sample_df, list(species = NULL)
    )
    expect_equal(nrow(result), 4)
  })

  it("skips columns with empty selections", {
    result <- data_utils$filter_data(
      sample_df, list(species = character(0))
    )
    expect_equal(nrow(result), 4)
  })

  it("returns zero rows when no values match", {
    result <- data_utils$filter_data(
      sample_df, list(species = "fish")
    )
    expect_equal(nrow(result), 0)
  })
})

# =============================================================================
# default_palette
# =============================================================================

describe("default_palette", {
  it("returns empty vector for n <= 0", {
    expect_equal(data_utils$default_palette(0), character(0))
    expect_equal(data_utils$default_palette(-1), character(0))
  })

  it("returns n colors for small n", {
    result <- data_utils$default_palette(3)
    expect_length(result, 3)
    expect_true(all(grepl("^#", result)))
  })

  it("returns n colors for large n", {
    result <- data_utils$default_palette(15)
    expect_length(result, 15)
    expect_true(all(grepl("^#", result)))
  })
})
