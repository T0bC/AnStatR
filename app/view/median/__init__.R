# View: Median module and related sub-modules.
# Re-exports ui/server so callers can use app/view/median directly.

#' @export
box::use(
  app/view/median/median[ui, server],
)
