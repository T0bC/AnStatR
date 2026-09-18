box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/preprocessing[default_na_cap_percent],
)

# =============================================================================
# Shared "impute missing values" checkbox, used by the PCA, LDA and Cluster
# data-selection sidebars. Same id, label and position in all three.
#
# The tooltip deliberately says only what is true in every module: without
# imputation the row is removed. How much imputation buys varies by engine
# (PCA/PLS-DA run on NIPALS and tolerate gaps natively, while IPCA, MASS
# LDA/QDA, k-means and DBSCAN cannot take an NA at all) — and since the
# engine is chosen further down the same sidebar, a tooltip claiming one
# or the other would be wrong half the time. That nuance belongs in the
# results info layer, which reports what actually happened.
# =============================================================================

#' Render the "impute missing values" checkbox
#'
#' @param ns Namespace function from the parent module
#' @return Shiny checkbox input
#' @export
impute_checkbox <- function(ns) {
  shiny$checkboxInput(
    inputId = ns("impute_missing"),
    label = shiny$tags$span(
      "Impute missing values ",
      bslib$tooltip(
        bsicons$bs_icon(
          "info-circle",
          class = "text-muted"
        ),
        paste(
          "Reconstructs missing measurements from the other",
          "columns (mixOmics NIPALS) instead of removing every",
          "row that holds a gap. Imputed values are estimates,",
          "not observations, and are counted in the results",
          "summary.",
          paste0(
            "Columns missing more than ", default_na_cap_percent,
            "% of their values are refused rather than invented."
          )
        )
      )
    ),
    value = FALSE
  )
}
