box::use(
  testthat[describe, expect_equal, expect_s3_class, expect_true,
           expect_type, it],
)

box::use(
  app/logic/plotting/scatter,
)

# =============================================================================
# Helper: minimal test data
# =============================================================================

make_test_data <- function(n = 30) {
  set.seed(42)
  data.frame(
    Treatment = rep(c("A", "B", "C"), each = n / 3),
    Site      = rep(c("X", "Y"), times = n / 2),
    Value1    = rnorm(n, mean = 10, sd = 2),
    Value2    = rnorm(n, mean = 5, sd = 1),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# create_empty_plot
# =============================================================================

describe("create_empty_plot", {
  it("returns a ggplot object", {
    p <- scatter$create_empty_plot("test message")
    expect_s3_class(p, "gg")
  })

  it("uses default message when none provided", {
    p <- scatter$create_empty_plot()
    expect_s3_class(p, "gg")
  })
})

# =============================================================================
# build_tooltip_text
# =============================================================================

describe("build_tooltip_text", {
  it("returns character vector with correct length", {
    df <- make_test_data()
    df$.is_trimmed <- FALSE
    df$.is_outlier <- FALSE

    tips <- scatter$build_tooltip_text(
      data = df, x_var = "Treatment", x_label = "Treatment",
      y_col = "Value1"
    )
    expect_type(tips, "character")
    expect_equal(length(tips), nrow(df))
  })

  it("includes extra tooltip columns", {
    df <- make_test_data()
    df$.is_trimmed <- FALSE
    df$.is_outlier <- FALSE

    tips <- scatter$build_tooltip_text(
      data = df, x_var = "Treatment", x_label = "Treatment",
      y_col = "Value1", tooltip_cols = "Site"
    )
    expect_true(all(grepl("Site", tips)))
  })

  it("marks trimmed points in tooltip", {
    df <- make_test_data(6)
    df$.is_trimmed <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    df$.is_outlier <- FALSE

    tips <- scatter$build_tooltip_text(
      data = df, x_var = "Treatment", x_label = "Treatment",
      y_col = "Value1"
    )
    expect_true(grepl("Trimmed", tips[1]))
    expect_true(!grepl("Trimmed", tips[2]))
  })

  it("marks outlier points in tooltip", {
    df <- make_test_data(6)
    df$.is_trimmed <- FALSE
    df$.is_outlier <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    tips <- scatter$build_tooltip_text(
      data = df, x_var = "Treatment", x_label = "Treatment",
      y_col = "Value1"
    )
    expect_true(grepl("Outlier", tips[3]))
    expect_true(!grepl("Outlier", tips[1]))
  })
})

# =============================================================================
# create_scatter_plot — validation
# =============================================================================

describe("create_scatter_plot validation", {
  it("returns empty plot for NULL data", {
    p <- scatter$create_scatter_plot(
      data = NULL, x_cols = "Treatment", y_col = "Value1"
    )
    expect_s3_class(p, "gg")
  })

  it("returns empty plot for empty data frame", {
    p <- scatter$create_scatter_plot(
      data = data.frame(), x_cols = "Treatment", y_col = "Value1"
    )
    expect_s3_class(p, "gg")
  })

  it("returns empty plot for missing x column", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "NonExistent", y_col = "Value1"
    )
    expect_s3_class(p, "gg")
  })

  it("returns empty plot for missing y column", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "NonExistent"
    )
    expect_s3_class(p, "gg")
  })

  it("returns empty plot when x_cols is NULL", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = NULL, y_col = "Value1"
    )
    expect_s3_class(p, "gg")
  })
})

# =============================================================================
# create_scatter_plot — basic plot
# =============================================================================

describe("create_scatter_plot basic", {
  it("returns a ggplot for single x column", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1"
    )
    expect_s3_class(p, "gg")
  })

  it("returns a ggplot for multiple x columns (nested axis)", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = c("Treatment", "Site"), y_col = "Value1"
    )
    expect_s3_class(p, "gg")
  })

  it("applies custom color_map", {
    df <- make_test_data()
    cmap <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      color_map = cmap
    )
    expect_s3_class(p, "gg")
  })

  it("handles separate color_cols", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = c("Treatment", "Site"),
      y_col = "Value1", color_cols = "Treatment"
    )
    expect_s3_class(p, "gg")
  })
})

# =============================================================================
# create_scatter_plot — processing options
# =============================================================================

describe("create_scatter_plot with processing", {
  it("works with trimming enabled", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      processing = list(trim_percent = 10)
    )
    expect_s3_class(p, "gg")
  })

  it("works with outlier detection enabled", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      processing = list(outlier_enabled = TRUE, outlier_method = "IQR")
    )
    expect_s3_class(p, "gg")
  })

  it("works with KDE outlier detection (probability factor)", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      processing = list(
        outlier_enabled = TRUE,
        outlier_method = "kde",
        outlier_factor = 0.05
      )
    )
    expect_s3_class(p, "gg")
  })

  it("works with both trimming and outlier detection", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      processing = list(
        trim_percent = 10,
        outlier_enabled = TRUE,
        outlier_method = "IQR",
        outlier_factor = 1.5
      )
    )
    expect_s3_class(p, "gg")
  })
})

# =============================================================================
# create_scatter_plot — style options
# =============================================================================

describe("create_scatter_plot style options", {
  it("respects grid_legend settings", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      grid_legend = list(
        h_grid = FALSE, v_grid = FALSE,
        show_median = FALSE, show_sd = FALSE,
        legend_position = "right"
      )
    )
    expect_s3_class(p, "gg")
  })

  it("respects aspect_ratio = TRUE", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      grid_legend = list(aspect_ratio = TRUE)
    )
    expect_s3_class(p, "gg")
  })

  it("respects top_right_borders = FALSE", {
    df <- make_test_data()
    p <- scatter$create_scatter_plot(
      data = df, x_cols = "Treatment", y_col = "Value1",
      grid_legend = list(top_right_borders = FALSE)
    )
    expect_s3_class(p, "gg")
  })
})
