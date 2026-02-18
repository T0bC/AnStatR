# View: Plotting module and related sub-modules.
# Re-exports ui/server so callers can use app/view/plotting directly.

#' @export
box::use(
  app/view/plotting/plotting[ui, server],
)
