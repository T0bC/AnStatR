box::use(
  openxlsx,
  rhino,
)

# =============================================================================
# Pure logic functions for PCA result export
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Export PCA results to a formatted Excel workbook
#'
#' Creates a multi-sheet Excel file with eigenvalues, variable
#' coordinates / contributions / cos2, and individual coordinates /
#' contributions / cos2.
#'
#' @param pca_result PCA result list from run_pca()
#'   (the $result field, not the wrapper)
#' @param file Path to save the Excel file
#' @return NULL (side effect: writes file)
#' @export
create_pca_excel <- function(pca_result, file) {
  wb <- openxlsx$createWorkbook()

  # Sheet 1: Eigenvalues
  eig <- as.data.frame(pca_result$eig)
  eig <- cbind(
    Component = rownames(eig),
    round(eig, 4)
  )
  rownames(eig) <- NULL
  names(eig) <- c(
    "Component", "Eigenvalue",
    "Variance (%)", "Cumulative Variance (%)"
  )
  add_sheet(wb, "Eigenvalues", eig)

  # Sheet 2: Variable Coordinates
  var_coord <- matrix_to_df(
    pca_result$var$coord, "Variable"
  )
  add_sheet(wb, "Variable Coordinates", var_coord)

  # Sheet 3: Variable Contributions
  var_contrib <- matrix_to_df(
    pca_result$var$contrib, "Variable"
  )
  add_sheet(wb, "Variable Contributions", var_contrib)

  # Sheet 4: Variable Cos2
  var_cos2 <- matrix_to_df(
    pca_result$var$cos2, "Variable"
  )
  add_sheet(wb, "Variable Cos2", var_cos2)

  # Sheet 5: Individual Coordinates
  ind_coord <- matrix_to_df(
    pca_result$ind$coord, "Individual"
  )
  add_sheet(wb, "Individual Coordinates", ind_coord)

  # Sheet 6: Individual Contributions
  ind_contrib <- matrix_to_df(
    pca_result$ind$contrib, "Individual"
  )
  add_sheet(wb, "Individual Contributions", ind_contrib)

  # Sheet 7: Individual Cos2
  ind_cos2 <- matrix_to_df(
    pca_result$ind$cos2, "Individual"
  )
  add_sheet(wb, "Individual Cos2", ind_cos2)

  openxlsx$saveWorkbook(wb, file, overwrite = TRUE)

  rhino$log$info(
    "PCA export: Excel saved ({7} sheets)"
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

matrix_to_df <- function(mat, row_label = "Item") {
  df <- as.data.frame(mat)
  df <- cbind(Item = rownames(df), round(df, 4))
  rownames(df) <- NULL
  names(df)[1] <- row_label
  df
}

add_sheet <- function(wb, sheet_name, data) {
  openxlsx$addWorksheet(wb, sheet_name)
  openxlsx$writeData(wb, sheet_name, data)
  openxlsx$setColWidths(
    wb, sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}
