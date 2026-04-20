# View: Load data module and related sub-modules.
# Re-exports ui/server so callers can use app/view/load_data directly.

#' @export
box::use(
  app/view/load_data/load_data[ui, server],
)
