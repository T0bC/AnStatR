box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/biplot,
  app/logic/pca/pca,
)

impl <- attr(biplot, "namespace")

# =============================================================================
# Shared test fixtures
# =============================================================================

make_pca_result <- function(n = 20, with_group = FALSE) {
  set.seed(42)
  test_data <- data.frame(
    group = sample(c("A", "B"), n, replace = TRUE),
    x = rnorm(n, mean = 10, sd = 2),
    y = rnorm(n, mean = 5, sd = 1),
    z = rnorm(n, mean = 0, sd = 3),
    stringsAsFactors = FALSE
  )
  meta <- if (with_group) "group" else character(0)
  res <- pca$run_pca(
    test_data, c("x", "y", "z"),
    meta_cols = meta
  )
  res$result
}

# =============================================================================
# create_biplot — layer variants
# =============================================================================

describe("create_biplot", {
  pca_res <- make_pca_result()

  it("returns a ggplot for layer = 'individuals'", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("returns a ggplot for layer = 'variables'", {
    res <- biplot$create_biplot(
      pca_res, layer = "variables"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("returns a ggplot for layer = 'combined'", {
    res <- biplot$create_biplot(
      pca_res, layer = "combined"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })
})

# =============================================================================
# create_biplot — grouping
# =============================================================================

describe("create_biplot with grouping", {
  pca_res <- make_pca_result(n = 20, with_group = TRUE)

  it("handles group_col for individuals layer", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      group_col = "group"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("handles group_col for combined layer", {
    res <- biplot$create_biplot(
      pca_res, layer = "combined",
      group_col = "group"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("handles missing group_col gracefully", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      group_col = NULL
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("handles non-existent group_col gracefully", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      group_col = "nonexistent"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })
})

# =============================================================================
# create_biplot — convex hull vs ellipse
# =============================================================================

describe("create_biplot hull/ellipse toggle", {
  pca_res <- make_pca_result(n = 30, with_group = TRUE)

  it("renders with convex hull when toggled", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      group_col = "group",
      show_convex_hull = TRUE
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("renders with ellipse (default)", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      group_col = "group",
      show_convex_hull = FALSE
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })
})

# =============================================================================
# create_biplot — contribution mapping
# =============================================================================

describe("create_biplot contribution mapping", {
  pca_res <- make_pca_result()

  it("maps alpha to contribution", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      point_alpha = "Contribution",
      point_size = 3
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("maps size to contribution", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      point_alpha = 0.7,
      point_size = "Contribution"
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })

  it("uses fixed alpha and size", {
    res <- biplot$create_biplot(
      pca_res, layer = "individuals",
      point_alpha = 0.5,
      point_size = 4
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "ggplot"))
  })
})

# =============================================================================
# create_biplot — error cases
# =============================================================================

describe("create_biplot error cases", {
  pca_res <- make_pca_result()

  it("returns error for NULL pca_result", {
    res <- biplot$create_biplot(
      NULL, layer = "individuals"
    )
    expect_false(res$success)
    expect_true(res$error$is_error)
  })

  it("returns error for invalid dimension name", {
    res <- biplot$create_biplot(
      pca_res, dim_x = "Dim.99", layer = "individuals"
    )
    expect_false(res$success)
    expect_true(res$error$is_error)
  })

  it("returns error for invalid layer", {
    res <- biplot$create_biplot(
      pca_res, layer = "invalid_layer"
    )
    expect_false(res$success)
    expect_true(res$error$is_error)
  })
})

# =============================================================================
# create_biplot — title toggle
# =============================================================================

describe("create_biplot title toggle", {
  pca_res <- make_pca_result()

  it("includes title when show_title = TRUE", {
    res <- biplot$create_biplot(
      pca_res, layer = "combined", show_title = TRUE
    )
    expect_true(res$success)
    expect_true(!is.null(res$result$labels$title))
  })

  it("omits title when show_title = FALSE", {
    res <- biplot$create_biplot(
      pca_res, layer = "combined", show_title = FALSE
    )
    expect_true(res$success)
    expect_true(is.null(res$result$labels$title))
  })
})

# =============================================================================
# biplot_error_parser
# =============================================================================

describe("biplot_error_parser", {
  it("parses dimension errors", {
    msg <- biplot$biplot_error_parser(
      "Dimension not found: Dim.99"
    )
    expect_true(grepl("dimension", msg, ignore.case = TRUE))
  })

  it("parses NULL pca_result errors", {
    msg <- biplot$biplot_error_parser(
      "pca_result is NULL"
    )
    expect_true(grepl("PCA result", msg, ignore.case = TRUE))
  })

  it("parses layer errors", {
    msg <- biplot$biplot_error_parser(
      "Invalid layer: foo"
    )
    expect_true(grepl("layer", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- biplot$biplot_error_parser(
      "something unexpected"
    )
    expect_equal(
      msg, "Biplot failed: something unexpected"
    )
  })
})

# =============================================================================
# Internal helpers via namespace
# =============================================================================

describe("build_ind_plot_data", {
  pca_res <- make_pca_result(n = 10, with_group = TRUE)

  it("returns data frame with expected columns", {
    df <- impl$build_ind_plot_data(
      pca_res, "Dim.1", "Dim.2",
      "group", "Contribution", "Contribution"
    )
    expect_true(is.data.frame(df))
    expect_true(all(c(
      "x", "y", "label", "tooltip", "data_id",
      "alpha_val", "size_val", "group"
    ) %in% names(df)))
    expect_equal(nrow(df), 10)
  })

  it("omits group when group_col is NULL", {
    df <- impl$build_ind_plot_data(
      pca_res, "Dim.1", "Dim.2",
      NULL, 0.5, 3
    )
    expect_false("group" %in% names(df))
    expect_false("alpha_val" %in% names(df))
    expect_false("size_val" %in% names(df))
  })
})

describe("build_var_plot_data", {
  pca_res <- make_pca_result()

  it("returns data frame with expected columns", {
    df <- impl$build_var_plot_data(
      pca_res, "Dim.1", "Dim.2"
    )
    expect_true(is.data.frame(df))
    expect_true(all(c(
      "xend", "yend", "label", "tooltip", "data_id"
    ) %in% names(df)))
    expect_equal(nrow(df), 3)
  })
})

describe("build_hull_data", {
  it("returns hull polygons for grouped data", {
    set.seed(42)
    ind_data <- data.frame(
      x = rnorm(20),
      y = rnorm(20),
      group = factor(rep(c("A", "B"), each = 10))
    )
    result <- impl$build_hull_data(ind_data)
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(all(c("x", "y", "group") %in% names(result)))
  })

  it("returns NULL when no group column", {
    ind_data <- data.frame(x = 1:5, y = 1:5)
    result <- impl$build_hull_data(ind_data)
    expect_true(is.null(result))
  })

  it("skips groups with fewer than 3 points", {
    ind_data <- data.frame(
      x = c(1, 2, 3, 4, 5, 6, 7),
      y = c(1, 2, 3, 4, 5, 6, 7),
      group = factor(c("A", "A", "B", "B", "B", "B", "B"))
    )
    result <- impl$build_hull_data(ind_data)
    expect_true(is.data.frame(result))
    # Only group B should have hull
    expect_true(all(result$group == "B"))
  })
})

describe("axis_label_with_variance", {
  pca_res <- make_pca_result()

  it("includes variance percentage", {
    label <- impl$axis_label_with_variance(
      "Dim.1", pca_res$eig
    )
    expect_true(grepl("Dim\\.1", label))
    expect_true(grepl("%", label))
  })

  it("falls back for unknown dimension", {
    label <- impl$axis_label_with_variance(
      "Dim.99", pca_res$eig
    )
    expect_equal(label, "Dim.99")
  })
})
