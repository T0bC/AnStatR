box::use(
  ggplot2[ggplot_build],
  testthat[
    describe,
    expect_equal,
    expect_false,
    expect_true,
    it,
  ],
)

box::use(
  app/logic/plotting/plot_factory,
)

# =============================================================================
# Shapes group by "Shape by" when set, otherwise by the X-axis columns --
# independently of "Color by".
# =============================================================================

fixture <- data.frame(
  SPECIES = c(rep("Adelie", 6), rep("Gentoo", 3), rep("Chinstrap", 3)),
  ISLAND = c(
    rep(c("Biscoe", "Dream", "Torgersen"), 2),
    rep("Biscoe", 3),
    rep("Dream", 3)
  ),
  SEX = rep(c("f", "m"), 6),
  Val = as.numeric(1:12),
  stringsAsFactors = FALSE
)

build <- function(shape_map, shape_cols = NULL, color_cols = "SPECIES") {
  plot_factory$create_plot(
    plot_type = "scatter",
    data = fixture,
    x_cols = c("SPECIES", "ISLAND"),
    y_col = "Val",
    shape_map = shape_map,
    color_cols = color_cols,
    point_style = list(shape_cols = shape_cols),
    grid_legend = list(legend_position = "right")
  )
}

# pch values the point layers actually drew
drawn_shapes <- function(p) {
  layers <- ggplot_build(p)$data
  drawn <- unlist(lapply(layers, function(l) {
    if ("shape" %in% names(l)) l$shape else NULL
  }))
  sort(unique(drawn))
}

has_scale <- function(p, aesthetic) {
  scales <- ggplot_build(p)$plot$scales$scales
  any(vapply(
    scales,
    function(s) aesthetic %in% s$aesthetics,
    logical(1)
  ))
}

describe("custom shapes", {
  it("key off the X-axis grouping when Shape by is empty", {
    shapes <- c(
      "Adelie.Biscoe" = 22L, "Adelie.Dream" = 23L,
      "Adelie.Torgersen" = 24L, "Gentoo.Biscoe" = 0L,
      "Chinstrap.Dream" = 2L
    )
    drawn <- drawn_shapes(build(shapes))
    expect_true(all(c(0L, 2L, 22L, 23L, 24L) %in% drawn))
  })

  it("key off Shape by even when Color by names a different column", {
    # Colour by SPECIES, shape by SEX: the two must not interfere
    p <- build(c(f = 24L, m = 25L), shape_cols = "SEX")
    expect_equal(drawn_shapes(p), c(24L, 25L))
    # "Shape by" also puts a shape legend on the plot
    expect_true(has_scale(p, "shape"))
  })

  it("keep the colour scale when a non-fillable shape is used", {
    # pch 3 has no interior, so the colour group must drive the stroke too
    p <- build(c(f = 3L, m = 21L), shape_cols = "SEX")
    expect_equal(drawn_shapes(p), c(3L, 21L))
    expect_true(has_scale(p, "colour"))
  })

  it("drop the colour scale when every shape is fillable", {
    p <- build(c(f = 24L, m = 25L), shape_cols = "SEX")
    expect_false(has_scale(p, "colour"))
  })

  it("fall back to pch 21 for groups with no entry", {
    p <- build(c("Adelie.Biscoe" = 22L))
    expect_true(21L %in% drawn_shapes(p))
  })
})
