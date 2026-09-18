box::use(
  mixOmics,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Pure logic functions for NIPALS imputation of missing measurement values.
# No Shiny dependencies allowed in this file.
#
# This is the opt-in alternative to listwise row removal (see
# na_handling$clean_na_rows). It reconstructs the measurement matrix from
# its first `ncomp` principal components and reads the missing cells off
# that reconstruction, which is what mixOmics::impute.nipals() does.
#
# Imputation invents values. Everything here is built so the caller can
# tell the user exactly how many cells were invented and refuse the cases
# where the invention would dominate the result.
# =============================================================================

#' Default per-column missingness cap, in percent
#'
#' mixOmics' own guidance is that NIPALS imputation is appropriate below
#' roughly 20% missing values. Applied per column rather than globally:
#' a dataset at 5% overall can still hide one column at 60%, and that
#' column would be mostly reconstruction driving the loadings.
#' @export
default_na_cap_percent <- 20

#' Assess whether a dataset can be imputed
#'
#' Pure inspection — computes the numbers a caller needs to decide, and
#' never modifies data. Used both by impute_missing() for its guards and
#' by the view layer to describe the consequences before the user commits.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param max_col_na_percent Numeric, per-column cap. Default
#'   default_na_cap_percent.
#' @return List with:
#'   - $n_cells: integer, measurement cells in total
#'   - $n_missing: integer, missing measurement cells
#'   - $percent_missing: numeric, overall missingness
#'   - $rows_all_na: integer vector, row indices with no observed value
#'   - $cols_all_na: character vector, columns with no observed value
#'   - $cols_over_cap: data frame (column, na_percent) above the cap
#'   - $can_impute: logical, TRUE when imputation may proceed
#' @export
assess_imputation <- function(data, measurement_cols,
                              max_col_na_percent = default_na_cap_percent) {
  subset <- data[, measurement_cols, drop = FALSE]
  n_rows <- nrow(subset)
  n_cells <- n_rows * length(measurement_cols)
  n_missing <- sum(is.na(subset))

  na_per_col <- vapply(
    subset, function(x) sum(is.na(x)), integer(1)
  )
  pct_per_col <- if (n_rows > 0) {
    round(na_per_col / n_rows * 100, 1)
  } else {
    rep(0, length(measurement_cols))
  }

  cols_all_na <- names(na_per_col)[na_per_col == n_rows & n_rows > 0]

  # Columns blocked by the cap, excluding the all-NA ones which get
  # their own, clearer message.
  over <- pct_per_col > max_col_na_percent &
    !(names(na_per_col) %in% cols_all_na)
  cols_over_cap <- data.frame(
    column = names(na_per_col)[over],
    na_percent = as.numeric(pct_per_col[over]),
    stringsAsFactors = FALSE
  )
  cols_over_cap <- cols_over_cap[
    order(-cols_over_cap$na_percent), ,
    drop = FALSE
  ]
  rownames(cols_over_cap) <- NULL

  rows_all_na <- which(
    rowSums(!is.na(subset)) == 0
  )

  list(
    n_cells = as.integer(n_cells),
    n_missing = as.integer(n_missing),
    percent_missing = if (n_cells > 0) {
      round(n_missing / n_cells * 100, 1)
    } else {
      0
    },
    rows_all_na = as.integer(rows_all_na),
    cols_all_na = cols_all_na,
    cols_over_cap = cols_over_cap,
    can_impute = length(cols_all_na) == 0 &&
      nrow(cols_over_cap) == 0
  )
}

#' Choose a component count for imputation
#'
#' mixOmics warn that too few components impute poorly, since the missing
#' cells are read off a rank-`ncomp` reconstruction. Capped by the matrix
#' dimensions, which NIPALS cannot exceed.
#'
#' @param n_rows Integer, observation count
#' @param n_cols Integer, measurement column count
#' @param requested Integer or NULL. NULL picks a default of 5.
#' @return Integer, component count (at least 1)
#' @export
choose_impute_ncomp <- function(n_rows, n_cols, requested = NULL) {
  wanted <- if (is.null(requested)) 5 else requested
  max(1, min(wanted, n_cols - 1, n_rows - 1))
}

#' Impute missing measurement values using NIPALS
#'
#' Rows with no observed measurement at all are dropped first: they carry
#' no information to reconstruct from, and mixOmics::impute.nipals() errors
#' on them. Observed values are never altered.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param ncomp Integer or NULL, components used for the reconstruction
#' @param max_col_na_percent Numeric, per-column cap. Default
#'   default_na_cap_percent.
#' @return List with $success, $result or $error. $result contains
#'   $data, $n_imputed (cells), $rows_affected, $rows_dropped,
#'   $ncomp_used and $percent_missing.
#' @export
impute_missing <- function(data, measurement_cols,
                           ncomp = NULL,
                           max_col_na_percent = default_na_cap_percent) {
  error_handling$safe_execute(
    expr = {
      if (length(measurement_cols) < 2) {
        stop(
          "NIPALS imputation needs at least 2 measurement",
          " columns to reconstruct from."
        )
      }

      non_numeric <- measurement_cols[
        !vapply(
          data[, measurement_cols, drop = FALSE],
          is.numeric, logical(1)
        )
      ]
      if (length(non_numeric) > 0) {
        stop(
          "Non-numeric measurement column(s): ",
          paste(non_numeric, collapse = ", ")
        )
      }

      assessment <- assess_imputation(
        data, measurement_cols, max_col_na_percent
      )

      if (length(assessment$cols_all_na) > 0) {
        stop(
          "Column(s) with no observed values at all: ",
          paste(assessment$cols_all_na, collapse = ", "),
          ". Deselect them before imputing."
        )
      }

      if (nrow(assessment$cols_over_cap) > 0) {
        detail <- paste0(
          assessment$cols_over_cap$column, " (",
          assessment$cols_over_cap$na_percent, "%)",
          collapse = ", "
        )
        stop(
          "Column(s) above the ", max_col_na_percent,
          "% missing-value cap: ", detail,
          ". Imputing these would invent more than it recovers",
          " — deselect them or remove rows instead."
        )
      }

      # Nothing to do — return the data untouched rather than
      # running a reconstruction that would change nothing.
      if (assessment$n_missing == 0) {
        rhino$log$info(
          "Imputation: no missing values, data unchanged"
        )
        return(list(
          data = data,
          n_imputed = 0L,
          rows_affected = 0L,
          rows_dropped = 0L,
          ncomp_used = 0L,
          percent_missing = 0
        ))
      }

      # Rows with nothing observed cannot be reconstructed.
      rows_dropped <- length(assessment$rows_all_na)
      working <- if (rows_dropped > 0) {
        rhino$log$info(
          "Imputation: dropping {rows_dropped} row(s) with no",
          " observed measurements"
        )
        data[-assessment$rows_all_na, , drop = FALSE]
      } else {
        data
      }

      x_mat <- as.matrix(
        working[, measurement_cols, drop = FALSE]
      )
      missing_idx <- is.na(x_mat)
      n_imputed <- sum(missing_idx)
      rows_affected <- sum(rowSums(missing_idx) > 0)

      ncomp_used <- choose_impute_ncomp(
        nrow(x_mat), ncol(x_mat), ncomp
      )

      rhino$log$info(
        "Imputation: running mixOmics::impute.nipals() —",
        " {n_imputed} cells in {rows_affected} rows,",
        " {ncomp_used} components"
      )

      imputed <- mixOmics$impute.nipals(
        X = x_mat, ncomp = ncomp_used
      )

      if (anyNA(imputed)) {
        stop(
          "NIPALS returned missing values; the data may be too",
          " sparse to reconstruct."
        )
      }

      # Belt and braces: only the gaps are taken from the
      # reconstruction, so observed values are bit-identical.
      result_mat <- x_mat
      result_mat[missing_idx] <- imputed[missing_idx]

      result <- working
      result[, measurement_cols] <- as.data.frame(result_mat)

      rhino$log$info(
        "Imputation: complete ({n_imputed} cells imputed,",
        " {assessment$percent_missing}% of measurements)"
      )

      list(
        data = result,
        n_imputed = as.integer(n_imputed),
        rows_affected = as.integer(rows_affected),
        rows_dropped = as.integer(rows_dropped),
        ncomp_used = as.integer(ncomp_used),
        percent_missing = assessment$percent_missing
      )
    },
    operation_name = "NIPALS Imputation",
    error_parser = impute_error_parser
  )
}

#' Strip the data frame out of an imputation result for storage
#'
#' The bundle records what imputation did, never its output — the
#' imputed frame is already stored as $used_data. Returns NULL when
#' nothing was imputed, so a bundle from complete data carries no
#' imputation field at all.
#'
#' @param impute_result List from impute_missing()$result, or NULL
#' @return Named list without $data, or NULL
#' @export
build_impute_spec <- function(impute_result) {
  if (is.null(impute_result) ||
        !isTRUE(impute_result$n_imputed > 0)) {
    return(NULL)
  }
  impute_result[
    c(
      "n_imputed", "rows_affected", "rows_dropped",
      "ncomp_used", "percent_missing"
    )
  ]
}

#' Error parser for imputation-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
impute_error_parser <- function(error_msg,
                                operation_name =
                                  "NIPALS Imputation") {
  if (grepl("cap", error_msg, fixed = TRUE) ||
        grepl("no observed values", error_msg, fixed = TRUE)) {
    # Already written for the user, including the column names.
    paste0(operation_name, ": ", error_msg)
  } else if (grepl("at least 2", error_msg, fixed = TRUE)) {
    paste0(
      operation_name,
      ": Select at least 2 measurement columns —",
      " NIPALS reconstructs each gap from the others."
    )
  } else if (grepl("Non-numeric", error_msg, fixed = TRUE)) {
    paste0(operation_name, ": ", error_msg)
  } else if (grepl("too sparse|returned missing",
                   error_msg)) {
    paste0(
      operation_name,
      ": Could not reconstruct the missing values.",
      " Try removing rows instead, or deselect the",
      " columns with the most gaps."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
