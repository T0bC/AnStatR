# View: Statistics module and related sub-modules.
# Re-exports ui/server so callers can use app/view/statistics directly.

#' @export
box::use(
  app/view/statistics/statistics[ui, server],
)
