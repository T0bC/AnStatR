# View: PCA module and related sub-modules.
# Re-exports ui/server so callers can use app/view/pca directly.

#' @export
box::use(
  app/view/pca/pca[ui, server],
)
