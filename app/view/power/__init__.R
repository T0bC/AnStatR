# View: Power Analysis module and related sub-modules.
# Re-exports ui/server so callers can use app/view/power directly.

#' @export
box::use(
  app/view/power/power[ui, server],
)
