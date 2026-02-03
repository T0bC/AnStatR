#' PCA Utility Functions
#'
#' Provides data preparation utilities for PCA analysis.

#' Prepare data for PCA analysis
#'
#' Subsets to measurement columns, removes rows with any NA values,
#' and optionally scales the data. The cleaned data is used for all
#' subsequent PCA calculations (KMO, correlation plot, etc.).
#'
#' @param data Data frame with measurement columns
#' @param measure_cols Character vector of column names to include
#' @param scale Logical, whether to scale the data
#' @return List with:
#'   - data: Data frame with selected columns, NA rows removed, optionally scaled
#'   - rows_removed: Number of rows removed due to NA values
#'   - original_rows: Original number of rows
#' @export
prepare_pca_data <- function(data, measure_cols, scale = TRUE) {
    # Validate that all requested columns exist
    missing_cols <- setdiff(measure_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
    }
    
    pca_data <- data[, measure_cols, drop = FALSE]
    original_rows <- nrow(pca_data)
    
    # Remove rows with any NA in measurement columns
    complete_rows <- complete.cases(pca_data)
    pca_data <- pca_data[complete_rows, , drop = FALSE]
    rows_removed <- original_rows - nrow(pca_data)
    
    if (scale && nrow(pca_data) > 0) {
        pca_data <- as.data.frame(scale(pca_data))
    }
    
    list(
        data = pca_data,
        rows_removed = rows_removed,
        original_rows = original_rows
    )
}


#' Create PCA Excel Output
#'
#' Exports PCA results to a formatted Excel workbook with multiple sheets.
#'
#' @param pca_result PCA result object from FactoMineR::PCA
#' @param file Path to save the Excel file
#' @return NULL (side effect: writes file)
#' @export
createPCAExcelOutput <- function(pca_result, file) {
    wb <- openxlsx::createWorkbook()
    
    # Sheet 1: Eigenvalues
    eig <- as.data.frame(pca_result$eig)
    eig <- cbind(Component = paste0("Dim.", seq_len(nrow(eig))), eig)
    names(eig) <- c("Component", "Eigenvalue", "Variance (%)", "Cumulative Variance (%)")
    eig[, 2:4] <- round(eig[, 2:4], 4)
    
    openxlsx::addWorksheet(wb, "Eigenvalues")
    openxlsx::writeData(wb, "Eigenvalues", eig)
    openxlsx::setColWidths(wb, "Eigenvalues", cols = seq_len(ncol(eig)), widths = "auto")
    
    # Sheet 2: Variable Coordinates
    var_coord <- as.data.frame(pca_result$var$coord)
    var_coord <- cbind(Variable = rownames(var_coord), round(var_coord, 4))
    rownames(var_coord) <- NULL
    
    openxlsx::addWorksheet(wb, "Variable Coordinates")
    openxlsx::writeData(wb, "Variable Coordinates", var_coord)
    openxlsx::setColWidths(wb, "Variable Coordinates", cols = seq_len(ncol(var_coord)), widths = "auto")
    
    # Sheet 3: Variable Contributions
    var_contrib <- as.data.frame(pca_result$var$contrib)
    var_contrib <- cbind(Variable = rownames(var_contrib), round(var_contrib, 4))
    rownames(var_contrib) <- NULL
    
    openxlsx::addWorksheet(wb, "Variable Contributions")
    openxlsx::writeData(wb, "Variable Contributions", var_contrib)
    openxlsx::setColWidths(wb, "Variable Contributions", cols = seq_len(ncol(var_contrib)), widths = "auto")
    
    # Sheet 4: Variable Cos2
    var_cos2 <- as.data.frame(pca_result$var$cos2)
    var_cos2 <- cbind(Variable = rownames(var_cos2), round(var_cos2, 4))
    rownames(var_cos2) <- NULL
    
    openxlsx::addWorksheet(wb, "Variable Cos2")
    openxlsx::writeData(wb, "Variable Cos2", var_cos2)
    openxlsx::setColWidths(wb, "Variable Cos2", cols = seq_len(ncol(var_cos2)), widths = "auto")
    
    # Sheet 5: Individual Coordinates
    ind_coord <- as.data.frame(pca_result$ind$coord)
    ind_coord <- cbind(Individual = rownames(ind_coord), round(ind_coord, 4))
    rownames(ind_coord) <- NULL
    
    openxlsx::addWorksheet(wb, "Individual Coordinates")
    openxlsx::writeData(wb, "Individual Coordinates", ind_coord)
    openxlsx::setColWidths(wb, "Individual Coordinates", cols = seq_len(ncol(ind_coord)), widths = "auto")
    
    # Sheet 6: Individual Contributions
    ind_contrib <- as.data.frame(pca_result$ind$contrib)
    ind_contrib <- cbind(Individual = rownames(ind_contrib), round(ind_contrib, 4))
    rownames(ind_contrib) <- NULL
    
    openxlsx::addWorksheet(wb, "Individual Contributions")
    openxlsx::writeData(wb, "Individual Contributions", ind_contrib)
    openxlsx::setColWidths(wb, "Individual Contributions", cols = seq_len(ncol(ind_contrib)), widths = "auto")
    
    # Sheet 7: Individual Cos2
    ind_cos2 <- as.data.frame(pca_result$ind$cos2)
    ind_cos2 <- cbind(Individual = rownames(ind_cos2), round(ind_cos2, 4))
    rownames(ind_cos2) <- NULL
    
    openxlsx::addWorksheet(wb, "Individual Cos2")
    openxlsx::writeData(wb, "Individual Cos2", ind_cos2)
    openxlsx::setColWidths(wb, "Individual Cos2", cols = seq_len(ncol(ind_cos2)), widths = "auto")
    
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
