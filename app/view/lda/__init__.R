# View: LDA module and related sub-modules.
# Re-exports ui/server so callers can use app/view/lda directly.

#' @export
box::use(
  app/view/lda/lda[ui, server],
)
