# View: Settings modal module and related sub-modules.
# Re-exports ui/server so callers can use app/view/settings_modal directly.

#' @export
box::use(
  app/view/settings_modal/settings_modal[ui, server],
)
