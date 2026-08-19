box::use(
  openxlsx,
  rhino,
)

box::use(
  app/logic/shared/settings[app_version],
  app/logic/pca/pca_stats[
    compute_var_coord, compute_var_contrib, compute_var_cos2,
    compute_ind_contrib, compute_ind_cos2
  ],
)

# =============================================================================
# Pure logic functions for PCA result export
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Export PCA results to a formatted Excel workbook
#'
#' Creates a multi-sheet Excel file with variance explained,
#' variable coordinates / loadings, and individual (sample)
#' scores. For PCA and sPCA, also includes contribution % and
#' cos2 (quality of representation) sheets — these derived
#' statistics are not meaningful for IPCA (components are not
#' variance-ranked) and are omitted for it.
#'
#' @param pca_result PCA result list from run_pca()
#'   (the $result field, not the wrapper)
#' @param file Path to save the Excel file
#' @return NULL (side effect: writes file)
#' @export
create_pca_excel <- function(pca_result, file) {
  wb <- openxlsx$createWorkbook()
  analysis_type <- pca_result$analysis_type
  has_contrib <- analysis_type != "ipca"

  # Sheet 1: Variance Explained
  variance <- pca_result$variance
  variance_out <- data.frame(
    Component = rownames(variance),
    `Variance (%)` = round(variance$variance_percent, 4),
    `Cumulative Variance (%)` = round(
      variance$cumulative_variance_percent, 4
    ),
    check.names = FALSE
  )
  add_sheet(wb, "Variance Explained", variance_out)

  # Sheet 2: Variable Loadings
  var_loadings <- matrix_to_df(
    pca_result$loadings, "Variable"
  )
  add_sheet(wb, "Variable Loadings", var_loadings)

  if (has_contrib) {
    var_coord <- compute_var_coord(
      pca_result$loadings, pca_result$scores
    )
    var_contrib <- compute_var_contrib(pca_result$loadings)
    var_cos2 <- compute_var_cos2(var_coord)

    add_sheet(
      wb, "Variable Contributions",
      matrix_to_df(var_contrib, "Variable")
    )
    add_sheet(
      wb, "Variable Cos2",
      matrix_to_df(var_cos2, "Variable")
    )
  }

  # Individual metadata (if available)
  ind_meta <- pca_result$ind_meta

  # Sheet: Individual Scores
  ind_coord <- ind_matrix_to_df(
    pca_result$scores, ind_meta
  )
  add_sheet(wb, "Individual Scores", ind_coord)

  if (has_contrib) {
    scores <- pca_result$scores
    ind_contrib <- compute_ind_contrib(scores)
    ind_cos2 <- compute_ind_cos2(scores, scores)

    add_sheet(
      wb, "Individual Contributions",
      ind_matrix_to_df(ind_contrib, ind_meta)
    )
    add_sheet(
      wb, "Individual Cos2",
      ind_matrix_to_df(ind_cos2, ind_meta)
    )
  }

  openxlsx$saveWorkbook(wb, file, overwrite = TRUE)

  rhino$log$info(
    "PCA export: Excel saved ({length(wb$sheet_names)} sheets)"
  )
}


#' Create a standardized RDS bundle for PCA export
#'
#' Builds the named list that the prediction module
#' expects when loading a PCA/sPCA/IPCA .rds file. Stores the
#' fit-time center/scale vectors explicitly, since mixOmics
#' only retains them on plain pca() model objects (not on
#' spca()/ipca()) — the Prediction module needs them for
#' manual projection of new samples regardless of method.
#'
#' @param pca_result PCA result list from run_pca()
#'   (the $result field, not the wrapper)
#' @param raw_data Data frame, original data before
#'   any transforms
#' @param used_data Data frame, data actually passed
#'   to the mixOmics fit (after transform + NA removal)
#' @param numeric_cols Character vector of measurement
#'   column names
#' @param meta_cols Character vector of metadata column
#'   names
#' @param transform_params List of per-column transform
#'   param lists (from transform_skewed), or empty list
#' @param settings List with skewness_correction,
#'   scale_method, etc.
#' @return Named list (the bundle)
#' @export
create_pca_bundle <- function(pca_result, raw_data,
                              used_data, numeric_cols,
                              meta_cols,
                              transform_params = list(),
                              settings = list()) {
  bundle <- list(
    analysis_type = pca_result$analysis_type,
    model = pca_result$model,
    raw_data = raw_data,
    used_data = used_data,
    group_col = NULL,
    numeric_cols = numeric_cols,
    meta_cols = meta_cols,
    transform_params = transform_params,
    scale_params = list(
      center = pca_result$center,
      scale = pca_result$scale
    ),
    settings = settings,
    data_source = "raw",
    app_version = app_version,
    created = Sys.time()
  )

  rhino$log$info(
    "{toupper(pca_result$analysis_type)} bundle: created",
    " ({length(numeric_cols)} vars, {nrow(used_data)} obs)"
  )

  bundle
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

ind_matrix_to_df <- function(mat, meta) {
  df <- as.data.frame(round(mat, 4))
  if (!is.null(meta) && nrow(meta) == nrow(df) &&
      !("Row" %in% names(meta) && ncol(meta) == 1)) {
    # Prepend metadata columns before PCA dimensions
    df <- cbind(meta, df)
    rownames(df) <- NULL
  } else {
    df <- cbind(
      Individual = rownames(df), df
    )
    rownames(df) <- NULL
  }
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
