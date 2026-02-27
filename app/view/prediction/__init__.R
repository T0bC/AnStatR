# View: Prediction module and related sub-modules.
# Re-exports ui/server so callers can use app/view/prediction directly.

#' @export
box::use(
  app/view/prediction/prediction[ui, server],
)
