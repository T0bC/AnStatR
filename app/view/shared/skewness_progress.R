box::use(
  shiny,
)

box::use(
  app/logic/preprocessing/skewness_transform[
    transform_skewed
  ],
)

# =============================================================================
# Shared progress wrapper around the skewness correction, used by the PCA,
# LDA and Cluster modules.
#
# bestNormalize fits one column at a time and takes roughly a second per
# column, so a 20-column run leaves the app silent for half a minute after
# the Compute button is pressed. This wrapper puts a determinate bar in
# front of that wait, naming the column currently being fitted.
#
# A standalone shiny$Progress object is used rather than incProgress() so
# the wrapper works both inside an enclosing withProgress() (Cluster) and
# outside one (PCA, LDA).
# =============================================================================

#' Run skewness correction behind a progress bar
#'
#' Thin wrapper over `transform_skewed()`: identical arguments and return
#' value, plus a progress bar covering the per-column bestNormalize fits.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param skew_result Data frame from `detect_skewness()`
#' @param session Shiny session object, for the progress bar
#' @return The `transform_skewed()` result list ($success, $result/$error)
#' @export
transform_skewed_with_progress <- function(data, measurement_cols,
                                           skew_result,
                                           session = shiny$getDefaultReactiveDomain()) {
  n_skewed <- sum(skew_result$is_skewed, na.rm = TRUE)

  # No bar for a run with nothing to fit — it would flash and vanish.
  if (n_skewed == 0 || is.null(session)) {
    return(transform_skewed(
      data, measurement_cols, skew_result
    ))
  }

  bar <- shiny$Progress$new(session = session, min = 0, max = n_skewed)
  on.exit(bar$close(), add = TRUE)

  bar$set(
    value = 0,
    message = "Correcting skewness",
    detail = paste0(
      "Fitting ", n_skewed,
      if (n_skewed == 1) " column…" else " columns…"
    )
  )

  transform_skewed(
    data, measurement_cols, skew_result,
    progress = function(i, n, col_name) {
      bar$set(
        value = i - 1,
        message = "Correcting skewness",
        detail = paste0(
          "Column ", i, " of ", n, ": ", col_name
        )
      )
    }
  )
}
