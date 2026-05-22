box::use(
  app/logic/plotting/plot_factory,
  app/logic/plotting/plot_helpers,
)

#' @export
create_plot <- plot_factory$create_plot

#' @export
PLOT_TYPES <- plot_factory$PLOT_TYPES

#' @export
get_plot_type_choices <- plot_factory$get_plot_type_choices

#' @export
shows_points <- plot_factory$shows_points

#' @export
shows_stat_overlays <- plot_factory$shows_stat_overlays

#' @export
is_boxplot_type <- plot_factory$is_boxplot_type

#' @export
is_violin_type <- plot_factory$is_violin_type

#' @export
create_empty_plot <- plot_helpers$create_empty_plot
