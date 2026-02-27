box::use(
  bsicons,
  bslib,
  DT,
  shiny,
)

# =============================================================================
# Results display helpers for the prediction module
# =============================================================================

#' Render prediction results as UI content
#'
#' Builds the appropriate results panel based on
#' analysis type: classification table for LDA/MDA/QDA,
#' scores table for PCA.
#'
#' @param prediction_result Result from predict_unknown()
#' @param bundle The prediction bundle
#' @param unknown_data The original unknown data
#' @param ns Namespace function
#' @return shiny tagList
#' @export
render_prediction_results <- function(
    prediction_result, bundle, unknown_data, ns) {
  analysis_type <- prediction_result$analysis_type

  if (analysis_type == "pca") {
    render_pca_results(
      prediction_result, unknown_data, ns
    )
  } else {
    render_classification_results(
      prediction_result, bundle, unknown_data, ns
    )
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_pca_results <- function(pred_result,
                               unknown_data, ns) {
  scores <- pred_result$scores
  if (is.null(scores)) {
    return(shiny$tags$p(
      class = "text-muted",
      "No PC scores available."
    ))
  }

  # Build display table: row labels + scores
  display_df <- scores
  display_df <- round(display_df, 4)

  # Prepend a row label
  display_df <- cbind(
    Sample = paste0("Unknown_", seq_len(nrow(scores))),
    display_df
  )

  shiny$tagList(
    shiny$tags$h6(
      bsicons$bs_icon("table", class = "me-1"),
      "PCA Scores for Unknown Samples"
    ),
    DT$DTOutput(ns("prediction_table")),
    shiny$tags$div(
      class = "d-flex gap-2 mt-2",
      shiny$downloadButton(
        ns("download_results_excel"),
        label = shiny$tags$span(
          bsicons$bs_icon(
            "file-earmark-excel", class = "me-1"
          ),
          "Excel"
        ),
        class = "btn btn-outline-secondary btn-sm"
      )
    )
  )
}

render_classification_results <- function(
    pred_result, bundle, unknown_data, ns) {
  # Build classification table
  pred_class <- pred_result$predicted_class
  posterior <- pred_result$posterior

  if (is.null(pred_class)) {
    return(shiny$tags$p(
      class = "text-muted",
      "No classification results available."
    ))
  }

  type_label <- toupper(
    pred_result$analysis_type
  )

  shiny$tagList(
    shiny$tags$h6(
      bsicons$bs_icon("table", class = "me-1"),
      paste0(type_label, " Classification Results")
    ),
    DT$DTOutput(ns("prediction_table")),
    shiny$tags$div(
      class = "d-flex gap-2 mt-2",
      shiny$downloadButton(
        ns("download_results_excel"),
        label = shiny$tags$span(
          bsicons$bs_icon(
            "file-earmark-excel", class = "me-1"
          ),
          "Excel"
        ),
        class = "btn btn-outline-secondary btn-sm"
      )
    )
  )
}

#' Build the prediction results data frame for DT
#'
#' @param prediction_result Result from predict_unknown
#' @param unknown_data Original unknown data
#' @param meta_col Optional metadata column for labels
#' @return Data frame suitable for DT rendering
#' @export
build_results_table <- function(prediction_result,
                                unknown_data,
                                meta_col = NULL) {
  analysis_type <- prediction_result$analysis_type

  # Row labels
  if (
    !is.null(meta_col) &&
    nchar(meta_col) > 0 &&
    meta_col %in% names(unknown_data)
  ) {
    labels <- as.character(unknown_data[[meta_col]])
  } else {
    labels <- paste0(
      "Unknown_", seq_len(nrow(unknown_data))
    )
  }

  if (analysis_type == "pca") {
    scores <- prediction_result$scores
    df <- cbind(
      Sample = labels,
      round(scores, 4)
    )
  } else {
    pred_class <- prediction_result$predicted_class
    posterior <- prediction_result$posterior

    df <- data.frame(
      Sample = labels,
      Predicted = as.character(pred_class),
      stringsAsFactors = FALSE
    )

    if (!is.null(posterior)) {
      post_rounded <- round(posterior, 4)
      colnames(post_rounded) <- paste0(
        "P(", colnames(post_rounded), ")"
      )
      df <- cbind(df, post_rounded)
    }

    # Add LD scores if available
    scores <- prediction_result$scores
    if (!is.null(scores) && ncol(scores) > 0) {
      df <- cbind(df, round(scores, 4))
    }
  }

  df
}
