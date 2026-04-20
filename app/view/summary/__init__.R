# View: Summary module and related sub-modules.
# Re-exports ui/server so callers can use app/view/summary directly.

#' @export
box::use(
  app/view/summary/summary[ui, server],
)
