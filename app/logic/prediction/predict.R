box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
  app/logic/skewness_transform[apply_stored_transforms],
)

# =============================================================================
# Prediction logic: preprocess unknowns and run predict
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Preprocess unknown data using stored bundle params
#'
#' Applies stored skewness transforms and scaling params
#' to unknown data so it matches the training pipeline.
#' For PCA: only transforms (predict.prcomp handles
#' center/scale automatically).
#' For LDA/MDA/QDA: transforms + manual scale.
#'
#' @param unknown_data Data frame of unknown observations
#' @param bundle The prediction bundle
#' @return Data frame with preprocessed numeric columns
#' @export
preprocess_unknown <- function(unknown_data, bundle) {
  numeric_cols <- bundle$numeric_cols
  result <- unknown_data

  # Step 1: Apply stored skewness transforms
  if (length(bundle$transform_params) > 0) {
    result <- apply_stored_transforms(
      result, bundle$transform_params
    )
    rhino$log$info(
      "Prediction: applied ",
      "{length(bundle$transform_params)}",
      " stored transforms"
    )
  }

  # Step 2: Apply stored scaling (LDA/MDA/QDA only)
  # PCA scaling is handled by predict.prcomp
  if (
    bundle$analysis_type != "pca" &&
    !is.null(bundle$scale_params)
  ) {
    sp <- bundle$scale_params
    numeric_subset <- result[
      , numeric_cols, drop = FALSE
    ]

    if (!is.null(sp$center)) {
      for (col in numeric_cols) {
        if (col %in% names(sp$center)) {
          numeric_subset[[col]] <-
            numeric_subset[[col]] - sp$center[[col]]
        }
      }
    }

    if (!is.null(sp$scale)) {
      for (col in numeric_cols) {
        if (
          col %in% names(sp$scale) &&
          sp$scale[[col]] != 0
        ) {
          numeric_subset[[col]] <-
            numeric_subset[[col]] / sp$scale[[col]]
        }
      }
    }

    result[, numeric_cols] <- numeric_subset
    rhino$log$info(
      "Prediction: applied stored scaling"
    )
  }

  result
}

#' Run prediction on preprocessed unknown data
#'
#' Dispatches to the appropriate predict method based
#' on the bundle's analysis_type.
#'
#' @param bundle The prediction bundle
#' @param preprocessed_data Data frame (already
#'   preprocessed via preprocess_unknown)
#' @return List with $success, $result or $error.
#'   $result contains: predicted_class, posterior,
#'   scores (where applicable), n_unknowns
#' @export
predict_unknown <- function(bundle, preprocessed_data) {
  error_handling$safe_execute(
    expr = {
      model <- bundle$model
      numeric_cols <- bundle$numeric_cols
      numeric_data <- preprocessed_data[
        , numeric_cols, drop = FALSE
      ]
      analysis_type <- bundle$analysis_type

      rhino$log$info(
        "Prediction: running {toupper(analysis_type)}",
        " predict on {nrow(numeric_data)} unknowns"
      )

      result <- switch(
        analysis_type,
        pca = predict_pca(model, numeric_data),
        lda = predict_lda(model, numeric_data),
        mda = predict_mda(model, numeric_data),
        qda = predict_qda(
          model, numeric_data, bundle
        ),
        stop(paste0(
          "Unsupported analysis type: '",
          analysis_type, "'"
        ))
      )

      result$analysis_type <- analysis_type
      result$n_unknowns <- nrow(numeric_data)

      rhino$log$info(
        "Prediction: complete — ",
        "{nrow(numeric_data)} observations predicted"
      )

      result
    },
    operation_name = "Prediction",
    error_parser = prediction_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

predict_pca <- function(model, numeric_data) {
  scores <- as.data.frame(
    stats$predict(model, numeric_data)
  )
  # Rename PC1..PCn to Dim.1..Dim.n to match
  # the PCA biplot convention used by create_biplot
  colnames(scores) <- paste0(
    "Dim.", seq_len(ncol(scores))
  )
  list(
    scores = scores,
    predicted_class = NULL,
    posterior = NULL
  )
}

predict_lda <- function(model, numeric_data) {
  pred <- stats$predict(model, numeric_data)
  list(
    predicted_class = pred$class,
    posterior = as.data.frame(pred$posterior),
    scores = if (!is.null(pred$x)) {
      as.data.frame(pred$x)
    } else {
      NULL
    }
  )
}

predict_mda <- function(model, numeric_data) {
  pred_class <- stats$predict(
    model, numeric_data
  )
  pred_post <- stats$predict(
    model, numeric_data, type = "posterior"
  )
  pred_scores <- stats$predict(
    model, numeric_data, type = "variates"
  )

  scores_df <- if (!is.null(pred_scores)) {
    df <- as.data.frame(pred_scores)
    if (ncol(df) > 0) {
      colnames(df) <- paste0(
        "LD", seq_len(ncol(df))
      )
    }
    df
  } else {
    NULL
  }

  list(
    predicted_class = pred_class,
    posterior = as.data.frame(pred_post),
    scores = scores_df
  )
}

predict_qda <- function(model, numeric_data, bundle) {
  pred <- stats$predict(model, numeric_data)

  result <- list(
    predicted_class = pred$class,
    posterior = as.data.frame(pred$posterior),
    scores = NULL
  )

  # Project through companion LDA for LD scores
  if (!is.null(bundle$lda_model)) {
    lda_pred <- stats$predict(
      bundle$lda_model, numeric_data
    )
    if (!is.null(lda_pred$x)) {
      result$scores <- as.data.frame(lda_pred$x)
    }
    rhino$log$info(
      "Prediction: QDA companion LDA projection done"
    )
  }

  result
}

prediction_error_parser <- function(
    error_msg,
    operation_name = "Prediction") {
  if (grepl(
    "subscript|column|variable",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data columns do not match the model.",
      " Verify that the unknown data has the",
      " same columns as the training data."
    )
  } else if (grepl(
    "singular|invertible|rank",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": The data matrix is singular.",
      " Check for constant or highly",
      " correlated columns."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
