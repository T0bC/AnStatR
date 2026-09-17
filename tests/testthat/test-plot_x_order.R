box::use(
  ggplot2[ggplot_build],
  testthat[
    describe,
    expect_equal,
    it,
  ],
)

box::use(
  app/logic/plotting/plot_factory,
)

# =============================================================================
# Custom x-axis ordering vs. shape splitting
#
# Point layers are split by shape family (fillable 21-25 vs. non-fillable
# 0-14).  Without pinned scale limits ggplot2 alphabetises the discrete x
# scale as soon as those layers cover disjoint subsets of the x factor.
# =============================================================================

custom_order <- c("virginica", "setosa", "versicolor")

# Uploaded data arrives with character columns, so mirror that here
iris_chr <- datasets::iris
iris_chr$Species <- as.character(iris_chr$Species)

x_limits <- function(shape_map, plot_type = "scatter") {
  p <- plot_factory$create_plot(
    plot_type = plot_type,
    data = iris_chr,
    x_cols = "Species",
    y_col = "Sepal.Length",
    color_map = c(
      setosa = "#ff0000", versicolor = "#00ff00", virginica = "#0000ff"
    ),
    shape_map = shape_map,
    color_cols = "Species",
    factor_order = list(Species = custom_order)
  )
  ggplot_build(p)$layout$panel_params[[1]]$x$get_limits()
}

describe("custom x-axis order", {
  it("is kept when every group uses the default shape", {
    shapes <- c(setosa = 21L, versicolor = 21L, virginica = 21L)
    expect_equal(x_limits(shapes), custom_order)
  })

  it("is kept when groups mix fillable and non-fillable shapes", {
    shapes <- c(setosa = 0L, versicolor = 2L, virginica = 21L)
    expect_equal(x_limits(shapes), custom_order)
  })

  it("is kept for boxplot and violin point layers", {
    shapes <- c(setosa = 0L, versicolor = 2L, virginica = 21L)
    expect_equal(x_limits(shapes, "boxplot_points"), custom_order)
    expect_equal(x_limits(shapes, "violin_points"), custom_order)
  })

  it("is kept when no custom shapes are set", {
    expect_equal(x_limits(NULL), custom_order)
  })
})
