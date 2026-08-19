box::use(
  mixOmics,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Pure logic functions for LDA / QDA
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Validate inputs before LDA/QDA computation
#'
#' Checks that measurement columns exist, a grouping column is
#' selected, and the grouping column has at least 2 levels.
#' Warns (but does not fail) if any group has fewer observations
#' than variables (n < p). For QDA, fails if any group has fewer
#' than p + 1 observations.
#'
#' @param columns Character vector of selected measurement column names
#' @param data Data frame to validate against
#' @param grouping_col Character, name of the grouping column
#' @param analysis_type Character, "lda", "qda", or "mda"
#' @param subclasses Integer, number of subclasses for MDA (default 3)
#' @return List with $valid (logical), $error (app_error or NULL),
#'   and $warnings (character vector, may be empty)
#' @export
validate_inputs <- function(columns, data, grouping_col,
                            analysis_type = "lda",
                            subclasses = 3) {
  warnings <- character(0)

  if (is.null(columns) || length(columns) == 0) {
    rhino$log$warn("LDA: no measurement columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one measurement column.",
        operation_name = "lda_validate_inputs"
      ),
      warnings = warnings
    ))
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "LDA: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "lda_validate_inputs"
      ),
      warnings = warnings
    ))
  }

  if (is.null(grouping_col) || length(grouping_col) == 0 ||
      grouping_col == "") {
    rhino$log$warn("LDA: no grouping column selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Please select a grouping column.",
          "LDA/QDA requires a categorical variable",
          "to define the groups."
        ),
        operation_name = "lda_validate_inputs"
      ),
      warnings = warnings
    ))
  }

  if (!(grouping_col %in% names(data))) {
    rhino$log$warn(
      "LDA: grouping column not found: {grouping_col}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Grouping column not found in data:",
          grouping_col
        ),
        operation_name = "lda_validate_inputs"
      ),
      warnings = warnings
    ))
  }

  groups <- as.character(data[[grouping_col]])
  unique_groups <- unique(groups[!is.na(groups)])
  n_groups <- length(unique_groups)

  if (n_groups < 2) {
    rhino$log$warn(
      "LDA: grouping column '{grouping_col}' has",
      " {n_groups} level(s), need >= 2"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste0(
          "Grouping column '", grouping_col,
          "' has ", n_groups, " unique level(s). ",
          "LDA/QDA requires at least 2 groups."
        ),
        operation_name = "lda_validate_inputs"
      ),
      warnings = warnings
    ))
  }

  # Check group sizes against number of variables
  group_counts <- table(groups)
  p <- length(columns)

  # QDA requires n >= p + 1 per group (separate covariance matrices)
  if (analysis_type == "qda") {
    small_groups <- names(group_counts)[group_counts < p + 1]
    if (length(small_groups) > 0) {
      rhino$log$warn(
        "QDA: groups too small for grouping column '{grouping_col}'"
      )
      return(list(
        valid = FALSE,
        error = error_handling$simple_error(
          message = paste0(
            "QDA requires at least ", p + 1,
            " observations per group (p + 1 = ",
            length(columns), " + 1). ",
            "The following groups in '", grouping_col,
            "' are too small: ",
            paste(
              small_groups,
              paste0("(n=", group_counts[small_groups], ")"),
              collapse = ", "
            ),
            ". Consider using LDA instead, or reduce ",
            "the number of variables via PCA."
          ),
          operation_name = "qda_validate_inputs"
        ),
        warnings = warnings
      ))
    }
  }

  # MDA requires each group to have enough observations for subclasses
  if (analysis_type == "mda") {
    # Each group needs at least subclasses observations
    # (ideally more for stable estimation)
    min_required <- max(subclasses, p + 1)
    small_groups <- names(group_counts)[group_counts < min_required]
    if (length(small_groups) > 0) {
      rhino$log$warn(
        "MDA: groups too small for grouping column '{grouping_col}'"
      )
      return(list(
        valid = FALSE,
        error = error_handling$simple_error(
          message = paste0(
            "MDA with ", subclasses, " subclasses requires at least ",
            min_required, " observations per group. ",
            "The following groups in '", grouping_col,
            "' are too small: ",
            paste(
              small_groups,
              paste0("(n=", group_counts[small_groups], ")"),
              collapse = ", "
            ),
            ". Consider reducing the number of subclasses, ",
            "using LDA instead, or reducing variables via PCA."
          ),
          operation_name = "mda_validate_inputs"
        ),
        warnings = warnings
      ))
    }
  }

  # Warn if any group has fewer observations than variables (LDA/MDA).
  # PLS-DA/sPLS-DA are designed for n < p and skip this warning.
  if (!analysis_type %in% c("plsda", "splsda")) {
    small_groups <- names(group_counts)[group_counts < p]
    if (length(small_groups) > 0) {
      warn_msg <- paste0(
        "Some groups have fewer observations than ",
        "variables (n < p = ", p, "): ",
        paste(
          small_groups,
          paste0("(n=", group_counts[small_groups], ")"),
          collapse = ", "
        ),
        ". LDA may fail or overfit. Consider using ",
        "PCA scores as input, or switch to PLS-DA/sPLS-DA ",
        "which are designed for n < p data."
      )
      rhino$log$warn("LDA: {warn_msg}")
      warnings <- c(warnings, warn_msg)
    }
  }

  rhino$log$info(
    "LDA: validation passed ({length(columns)} columns,",
    " grouping='{grouping_col}', {n_groups} groups)"
  )

  list(valid = TRUE, error = NULL, warnings = warnings)
}

#' Run Linear Discriminant Analysis
#'
#' Calls MASS::lda() on the supplied data. Supports both
#' standard model fitting and leave-one-out CV.
#'
#' @param data Data frame (cleaned, optionally scaled)
#' @param columns Character vector of measurement column names
#' @param grouping_col Character, name of the grouping column
#' @param prior Character, "proportional" or "equal"
#' @param tol Numeric, tolerance for singularity detection
#' @param method Character, estimation method
#' @param cv Logical, leave-one-out cross-validation
#' @param nu Numeric, degrees of freedom for method = "t"
#' @param meta_cols Character vector of metadata column names
#' @return List with $success, $result or $error
#' @export
run_lda <- function(data, columns, grouping_col,
                    prior = "proportional", tol = 1.0e-4,
                    method = "moment", cv = FALSE,
                    nu = NULL, meta_cols = character(0)) {
  error_handling$safe_execute(
    {
      grouping <- droplevels(as.factor(data[[grouping_col]]))
      numeric_data <- data[, columns, drop = FALSE]
      prior_vec <- build_prior(prior, grouping)

      args <- list(
        x = numeric_data,
        grouping = grouping,
        prior = prior_vec,
        tol = tol,
        method = method,
        CV = cv
      )
      if (method == "t" && !is.null(nu)) {
        args$nu <- nu
      }

      rhino$log$info(
        "LDA: running MASS::lda() — ",
        "{length(columns)} vars, {nlevels(grouping)}",
        " groups, method='{method}', CV={cv}"
      )

      lda_obj <- do.call(MASS::lda, args)

      build_lda_result(
        lda_obj, data, columns, grouping_col,
        meta_cols, cv, "lda"
      )
    },
    operation_name = "LDA",
    error_parser = lda_error_parser
  )
}

#' Run Quadratic Discriminant Analysis
#'
#' Calls MASS::qda() on the supplied data. Supports both
#' standard model fitting and leave-one-out CV.
#'
#' @param data Data frame (cleaned, optionally scaled)
#' @param columns Character vector of measurement column names
#' @param grouping_col Character, name of the grouping column
#' @param prior Character, "proportional" or "equal"
#' @param tol Numeric, tolerance for singularity detection
#' @param method Character, estimation method
#' @param cv Logical, leave-one-out cross-validation
#' @param nu Numeric, degrees of freedom for method = "t"
#' @param meta_cols Character vector of metadata column names
#' @return List with $success, $result or $error
#' @export
run_qda <- function(data, columns, grouping_col,
                    prior = "proportional", tol = 1.0e-4,
                    method = "moment", cv = FALSE,
                    nu = NULL, meta_cols = character(0)) {
  error_handling$safe_execute(
    {
      grouping <- droplevels(as.factor(data[[grouping_col]]))
      numeric_data <- data[, columns, drop = FALSE]
      prior_vec <- build_prior(prior, grouping)

      args <- list(
        x = numeric_data,
        grouping = grouping,
        prior = prior_vec,
        tol = tol,
        method = method,
        CV = cv
      )
      if (method == "t" && !is.null(nu)) {
        args$nu <- nu
      }

      rhino$log$info(
        "LDA: running MASS::qda() — ",
        "{length(columns)} vars, {nlevels(grouping)}",
        " groups, method='{method}', CV={cv}"
      )

      qda_obj <- do.call(MASS::qda, args)

      build_lda_result(
        qda_obj, data, columns, grouping_col,
        meta_cols, cv, "qda"
      )
    },
    operation_name = "QDA",
    error_parser = lda_error_parser
  )
}

#' Run Mixture Discriminant Analysis
#'
#' Calls mda::mda() on the supplied data. Supports both
#' standard model fitting and manual leave-one-out CV
#' (mda does not have a built-in CV parameter).
#'
#' @param data Data frame (cleaned, optionally scaled)
#' @param columns Character vector of measurement column names
#' @param grouping_col Character, name of the grouping column
#' @param prior Character, "proportional" or "equal"
#' @param cv Logical, leave-one-out cross-validation
#' @param meta_cols Character vector of metadata column names
#' @param subclasses Integer, number of subclasses per class
#' @param iter Integer, maximum number of EM iterations
#' @param dimension Integer or NULL, dimension of reduced model
#' @param eps Numeric or NULL, threshold for truncating dimension
#' @return List with $success, $result or $error
#' @export
run_mda <- function(data, columns, grouping_col,
                    prior = "proportional",
                    cv = FALSE,
                    meta_cols = character(0),
                    subclasses = 3, iter = 5,
                    dimension = NULL, eps = NULL) {
  error_handling$safe_execute(
    {
      grouping <- droplevels(as.factor(data[[grouping_col]]))
      numeric_data <- data[, columns, drop = FALSE]
      prior_vec <- build_prior(prior, grouping)

      rhino$log$info(
        "MDA: running mda::mda() — ",
        "{length(columns)} vars, {nlevels(grouping)}",
        " groups, subclasses={subclasses},",
        " iter={iter}, CV={cv}"
      )

      if (cv) {
        # Manual LOO-CV (mda has no built-in CV)
        build_mda_cv_result(
          data, numeric_data, grouping,
          columns, grouping_col, meta_cols,
          prior_vec, subclasses, iter,
          dimension, eps
        )
      } else {
        # Build formula: grouping ~ .
        fit_data <- cbind(
          numeric_data, .grouping. = grouping
        )

        mda_obj <- fit_mda(
          fit_data, subclasses, iter,
          dimension, eps
        )

        build_mda_result(
          mda_obj, data, numeric_data, columns,
          grouping_col, grouping, meta_cols,
          prior_vec
        )
      }
    },
    operation_name = "MDA",
    error_parser = lda_error_parser
  )
}


#' Run PLS-DA or sparse PLS-DA
#'
#' Calls mixOmics::plsda() or mixOmics::splsda() on the
#' supplied data. Designed for n < p (more variables than
#' specimens) and collinear predictors, unlike LDA/QDA/MDA.
#' Does not support cross-validated (CV=TRUE) fitting mode;
#' use run_plsda_perf() separately for component/keepX
#' diagnostics via repeated k-fold CV.
#'
#' @param data Data frame (cleaned, optionally scaled)
#' @param columns Character vector of measurement column names
#' @param grouping_col Character, name of the grouping column
#' @param ncomp Integer, number of components to compute
#' @param sparse Logical, TRUE for sPLS-DA (variable selection),
#'   FALSE for standard PLS-DA
#' @param keep_x Integer vector of length ncomp, number of
#'   variables to keep per component (sPLS-DA only)
#' @param scale Logical, scale variables to unit variance
#'   before fitting (mixOmics internal scaling)
#' @param meta_cols Character vector of metadata column names
#' @return List with $success, $result or $error
#' @export
run_plsda <- function(data, columns, grouping_col,
                      ncomp, sparse = FALSE,
                      keep_x = NULL, scale = TRUE,
                      meta_cols = character(0)) {
  error_handling$safe_execute(
    {
      if (sparse && (is.null(keep_x) || anyNA(keep_x))) {
        stop(
          "keepX invalid or incomplete: a numeric value is ",
          "required for every component in sPLS-DA."
        )
      }

      grouping <- droplevels(as.factor(data[[grouping_col]]))
      x_mat <- as.matrix(data[, columns, drop = FALSE])

      rhino$log$info(
        "PLS-DA: running mixOmics::",
        "{if (sparse) 'splsda' else 'plsda'}() — ",
        "{length(columns)} vars, {nlevels(grouping)}",
        " groups, ncomp={ncomp}, sparse={sparse}"
      )

      model <- if (sparse) {
        mixOmics$splsda(
          X = x_mat, Y = grouping, ncomp = ncomp,
          keepX = keep_x, scale = scale
        )
      } else {
        mixOmics$plsda(
          X = x_mat, Y = grouping, ncomp = ncomp,
          scale = scale
        )
      }

      build_plsda_result(
        model, data, columns, grouping_col,
        meta_cols, sparse, keep_x
      )
    },
    operation_name = if (sparse) "sPLS-DA" else "PLS-DA",
    error_parser = lda_error_parser
  )
}


#' Diagnose PLS-DA/sPLS-DA component count via repeated CV
#'
#' Wraps mixOmics::perf() to estimate classification error
#' rate (overall + balanced error rate, BER) per component
#' via repeated k-fold cross-validation. Used to justify the
#' chosen number of components, independent from the main
#' fit's validation setting.
#'
#' @param plsda_result The $result from run_plsda() (must
#'   contain a fitted $model, i.e. not already CV-only)
#' @param folds Integer, number of CV folds
#' @param repeats Integer, number of repeats of the CV scheme
#' @return List with $success, $result (list with $errors —
#'   data.frame of Component, Overall Error, BER using max.dist
#'   — and $stability — data.frame of Component, Variable,
#'   Frequency for sPLS-DA fits, or NULL for plain PLS-DA) or
#'   $error
#' @export
run_plsda_perf <- function(plsda_result, folds = 5,
                           repeats = 10) {
  error_handling$safe_execute(
    {
      model <- plsda_result$model
      if (is.null(model)) {
        stop("No fitted model available for CV diagnostics.")
      }

      rhino$log$info(
        "PLS-DA: running perf() — folds={folds},",
        " repeats={repeats}"
      )

      perf_res <- mixOmics$perf(
        model, validation = "Mfold", folds = folds,
        nrepeat = repeats, progressBar = FALSE
      )

      overall <- perf_res$error.rate$overall
      ber <- perf_res$error.rate$BER
      n_comp <- nrow(overall)

      errors_df <- data.frame(
        Component = paste0("Comp", seq_len(n_comp)),
        `Overall Error` = round(overall[, "max.dist"], 4),
        BER = round(ber[, "max.dist"], 4),
        check.names = FALSE
      )
      rownames(errors_df) <- NULL

      # Feature stability: how often each variable was selected
      # across CV folds/repeats (sPLS-DA only — perf() only
      # tracks $features$stable when the model used keepX).
      stability_df <- build_stability_table(perf_res, n_comp)

      list(errors = errors_df, stability = stability_df)
    },
    operation_name = "PLS-DA Component Diagnostics",
    error_parser = lda_error_parser
  )
}


#' Auto-tune sPLS-DA keepX per component via CV grid search
#'
#' Wraps mixOmics::tune.splsda() over a candidate grid of
#' keepX values, selecting the value per component that
#' minimises cross-validated balanced error rate (BER).
#' This is the slow, opt-in counterpart to manually setting
#' keepX in the UI.
#'
#' @param data Data frame (cleaned, optionally scaled)
#' @param columns Character vector of measurement column names
#' @param grouping_col Character, name of the grouping column
#' @param ncomp Integer, number of components
#' @param test_keep_x Integer vector, candidate keepX values
#'   to test (default a modest grid capped by column count)
#' @param folds Integer, number of CV folds
#' @param repeats Integer, number of CV repeats
#' @return List with $success, $result (named integer vector,
#'   one keepX per component) or $error
#' @export
run_plsda_tune_keepx <- function(data, columns, grouping_col,
                                 ncomp, test_keep_x = NULL,
                                 folds = 5, repeats = 10) {
  error_handling$safe_execute(
    {
      grouping <- droplevels(as.factor(data[[grouping_col]]))
      x_mat <- as.matrix(data[, columns, drop = FALSE])
      p <- length(columns)

      candidates <- test_keep_x %||% unique(pmin(
        p, c(5, 10, 15, 20, 30)
      ))

      rhino$log$info(
        "sPLS-DA: tuning keepX — ncomp={ncomp},",
        " candidates=[{paste(candidates, collapse=',')}],",
        " folds={folds}, repeats={repeats}"
      )

      tune_res <- mixOmics$tune.splsda(
        X = x_mat, Y = grouping, ncomp = ncomp,
        test.keepX = candidates,
        validation = "Mfold", folds = folds,
        nrepeat = repeats, progressBar = FALSE
      )

      keep_x <- as.integer(tune_res$choice.keepX[seq_len(ncomp)])
      names(keep_x) <- paste0("Comp", seq_len(ncomp))

      rhino$log$info(
        "sPLS-DA: tuning complete — ",
        "keepX=[{paste(keep_x, collapse=',')}]"
      )

      keep_x
    },
    operation_name = "sPLS-DA keepX Tuning",
    error_parser = lda_error_parser
  )
}


#' Predict on new data using a fitted LDA/QDA model
#'
#' Takes the result from run_lda()/run_qda() (with cv=FALSE)
#' and predicts classes for a test set. Returns classification,
#' posterior probabilities, LD scores (LDA only), and
#' confusion matrix if true labels are available.
#'
#' @param lda_result The $result from run_lda()/run_qda()
#' @param test_data Data frame with the same columns
#' @param columns Character vector of measurement column names
#' @param grouping_col Character, name of the grouping column
#'   (used to extract true labels for confusion matrix;
#'   NULL if no true labels available)
#' @param meta_cols Character vector of metadata column names
#' @return List with $success, $result or $error
#' @export
run_predict <- function(lda_result, test_data, columns,
                        grouping_col = NULL,
                        meta_cols = character(0)) {
  error_handling$safe_execute(
    {
      model <- lda_result$model
      if (is.null(model)) {
        stop(paste(
          "No model object available.",
          "Prediction requires a fitted model",
          "(run without cross-validation)."
        ))
      }

      numeric_test <- test_data[
        , columns, drop = FALSE
      ]

      rhino$log$info(
        "LDA: predicting on {nrow(numeric_test)}",
        " test observations"
      )

      is_mda <- lda_result$analysis_type == "mda"
      is_plsda <- lda_result$analysis_type %in%
        c("plsda", "splsda")

      if (is_plsda) {
        pred <- stats::predict(
          model, as.matrix(numeric_test)
        )
        n_comp <- ncol(pred$variates)
        pred_class <- pred$class$max.dist[, n_comp]
        pred_post <- as.data.frame(
          pred$predict[, , n_comp]
        )
        pred_scores <- pred$variates
      } else if (is_mda) {
        # MDA: predict returns factor directly
        pred_class <- stats::predict(
          model, numeric_test
        )
        pred_post <- stats::predict(
          model, numeric_test, type = "posterior"
        )
        pred_scores <- stats::predict(
          model, numeric_test, type = "variates"
        )
      } else {
        pred <- stats::predict(model, numeric_test)
        pred_class <- pred$class
        pred_post <- pred$posterior
        pred_scores <- pred$x
      }

      # Build meta for test set
      meta <- if (length(meta_cols) > 0) {
        test_data[, meta_cols, drop = FALSE]
      } else {
        data.frame(
          Row = seq_len(nrow(test_data))
        )
      }

      result <- list(
        analysis_type = lda_result$analysis_type,
        predicted_class = pred_class,
        posterior = as.data.frame(pred_post),
        meta = meta
      )

      # LD/Component scores (LDA, MDA, PLS-DA/sPLS-DA; not QDA)
      if (!is.null(pred_scores)) {
        scores_df <- as.data.frame(pred_scores)
        if (is_mda && ncol(scores_df) > 0) {
          colnames(scores_df) <- paste0(
            "LD", seq_len(ncol(scores_df))
          )
        }
        if (is_plsda && ncol(scores_df) > 0) {
          colnames(scores_df) <- paste0(
            "Comp", seq_len(ncol(scores_df))
          )
        }
        result$scores <- scores_df
      }

      # Confusion matrix if true labels available
      if (
        !is.null(grouping_col) &&
        grouping_col %in% names(test_data)
      ) {
        true_labels <- as.factor(
          test_data[[grouping_col]]
        )
        result$confusion <- build_confusion_stats(
          true_labels, pred_class
        )
      }

      result
    },
    operation_name = "LDA Prediction",
    error_parser = lda_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

build_stability_table <- function(perf_res, n_comp) {
  stable <- perf_res$features$stable
  if (is.null(stable) || length(stable) == 0) {
    return(NULL)
  }

  rows <- lapply(seq_along(stable), function(i) {
    comp_stable <- stable[[i]]
    if (is.null(comp_stable) || length(comp_stable) == 0) {
      return(NULL)
    }
    data.frame(
      Component = paste0("Comp", i),
      Variable = names(comp_stable),
      Frequency = round(as.numeric(comp_stable), 4),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df <- df[order(df$Component, -df$Frequency), ]
  rownames(df) <- NULL
  df
}


build_prior <- function(prior_choice, grouping) {
  # Drop unused factor levels (important after filtering)
  grouping <- droplevels(grouping)
  n_groups <- nlevels(grouping)
  if (prior_choice == "equal") {
    prior_vec <- rep(1 / n_groups, n_groups)
    names(prior_vec) <- levels(grouping)
    prior_vec
  } else {
    # "proportional" — compute from non-NA observations only
    # table() excludes NAs, so use sum(table()) not length(grouping)
    counts <- table(grouping)
    props <- counts / sum(counts)
    prior_vec <- as.numeric(props)
    names(prior_vec) <- levels(grouping)
    prior_vec
  }
}


fit_mda <- function(fit_data, subclasses, iter,
                    dimension, eps) {
  # mda::mda() uses formula interface which internally
  # calls stats::model.frame(). In box's isolated
  # environment this is not on the search path.
  # Evaluate in an environment that has stats attached.
  env <- new.env(parent = globalenv())
  env$fit_data <- fit_data
  env$subclasses <- subclasses
  env$iter <- iter
  env$dimension <- dimension
  env$eps <- eps
  eval(quote({
    args <- list(
      formula = .grouping. ~ .,
      data = fit_data,
      subclasses = subclasses,
      iter = iter
    )
    if (!is.null(dimension)) {
      args$dimension <- dimension
    }
    if (!is.null(eps)) args$eps <- eps
    do.call(mda::mda, args)
  }), envir = env)
}


build_lda_result <- function(obj, data, columns,
                             grouping_col, meta_cols,
                             cv, analysis_type) {
  grouping <- as.factor(data[[grouping_col]])
  n <- nrow(data)
  p <- length(columns)
  n_groups <- nlevels(grouping)

  # Build metadata
  meta <- if (length(meta_cols) > 0) {
    data[, meta_cols, drop = FALSE]
  } else {
    data.frame(Row = seq_len(n))
  }

  result <- list(
    analysis_type = analysis_type,
    grouping_col = grouping_col,
    columns = columns,
    n = n,
    p = p,
    n_groups = n_groups,
    group_levels = levels(grouping),
    prior = obj$prior,
    means = as.data.frame(obj$means),
    meta = meta
  )

  if (cv) {
    # CV mode: obj is a list with $class, $posterior
    result$cv <- build_cv_classification(
      obj, grouping
    )
    result$model <- NULL
    rhino$log$info(
      "LDA: LOO-CV complete — accuracy ",
      "{round(result$cv$accuracy * 100, 1)}%"
    )
  } else {
    # Model mode: obj is an lda/qda object
    result$model <- obj

    if (analysis_type == "lda") {
      result$scaling <- as.data.frame(obj$scaling)
      result$svd <- obj$svd
      n_ld <- length(obj$svd)
      prop_trace <- obj$svd^2 / sum(obj$svd^2)
      result$proportion_of_trace <- data.frame(
        LD = paste0("LD", seq_len(n_ld)),
        `Singular Value` = round(obj$svd, 4),
        `Proportion` = round(prop_trace, 4),
        `Cumulative` = round(cumsum(prop_trace), 4),
        check.names = FALSE
      )

      # Compute LD scores for all observations
      numeric_data <- data[, columns, drop = FALSE]
      pred_all <- stats::predict(obj, numeric_data)
      result$scores <- as.data.frame(pred_all$x)
      result$predicted_class <- pred_all$class
      result$posterior <- as.data.frame(
        pred_all$posterior
      )

      # Confusion on training data (resubstitution)
      result$confusion <- build_confusion_stats(
        grouping, pred_all$class
      )
    } else {
      # QDA has no scaling/svd
      numeric_data <- data[, columns, drop = FALSE]
      result$numeric_data <- numeric_data
      pred_all <- stats::predict(obj, numeric_data)
      result$predicted_class <- pred_all$class
      result$posterior <- as.data.frame(
        pred_all$posterior
      )
      result$confusion <- build_confusion_stats(
        grouping, pred_all$class
      )

      # Companion LDA fit for LD projection axes
      # (allows QDA results to be visualised in LD space)
      companion <- tryCatch(
        MASS::lda(
          grouping ~ .,
          data = cbind(numeric_data, grouping = grouping)
        ),
        error = function(e) NULL
      )
      if (!is.null(companion)) {
        result$lda_scaling <- as.data.frame(
          companion$scaling
        )
        result$lda_svd <- companion$svd
        n_ld <- length(companion$svd)
        prop_trace <- companion$svd^2 /
          sum(companion$svd^2)
        result$lda_proportion_of_trace <- data.frame(
          LD = paste0("LD", seq_len(n_ld)),
          `Singular Value` = round(companion$svd, 4),
          `Proportion` = round(prop_trace, 4),
          `Cumulative` = round(
            cumsum(prop_trace), 4
          ),
          check.names = FALSE
        )
        lda_pred <- stats::predict(
          companion, numeric_data
        )
        result$lda_scores <- as.data.frame(
          lda_pred$x
        )
        result$lda_model <- companion
        rhino$log$info(
          "QDA: companion LDA fit for LD projection ",
          "({n_ld} axes)"
        )
      }
    }

    rhino$log$info(
      "LDA: model fit complete — ",
      "resubstitution accuracy ",
      "{round(result$confusion$accuracy * 100, 1)}%"
    )
  }

  result
}


build_plsda_result <- function(model, data, columns,
                               grouping_col, meta_cols,
                               sparse, keep_x) {
  grouping <- as.factor(data[[grouping_col]])
  n <- nrow(data)
  p <- length(columns)
  n_groups <- nlevels(grouping)
  ncomp <- model$ncomp
  comp_names <- paste0("Comp", seq_len(ncomp))

  meta <- if (length(meta_cols) > 0) {
    data[, meta_cols, drop = FALSE]
  } else {
    data.frame(Row = seq_len(n))
  }

  # Group means (parity with LDA's Group Means table)
  numeric_data <- data[, columns, drop = FALSE]
  means <- as.data.frame(
    do.call(rbind, lapply(split(numeric_data, grouping), colMeans))
  )

  # Component scores
  scores <- as.data.frame(model$variates$X)
  colnames(scores) <- comp_names

  # Component loadings (used by lda_var_contrib.R adapter,
  # analogous to LDA's $scaling)
  scaling <- as.data.frame(model$loadings$X)
  colnames(scaling) <- comp_names

  # Explained variance per component -> same LD/Proportion/
  # Cumulative shape ld_plot.R's axis_label() expects
  prop_var <- as.numeric(model$prop_expl_var$X)
  proportion_of_trace <- data.frame(
    LD = comp_names,
    Proportion = round(prop_var, 4),
    Cumulative = round(cumsum(prop_var), 4),
    check.names = FALSE
  )

  # Resubstitution prediction (training data through its own model)
  pred_all <- stats::predict(model, as.matrix(numeric_data))
  pred_class <- pred_all$class$max.dist[, ncomp]
  pred_post <- as.data.frame(pred_all$predict[, , ncomp])

  result <- list(
    analysis_type = if (sparse) "splsda" else "plsda",
    grouping_col = grouping_col,
    columns = columns,
    n = n,
    p = p,
    n_groups = n_groups,
    group_levels = levels(grouping),
    prior = NULL,
    means = means,
    meta = meta,
    model = model,
    ncomp = ncomp,
    sparse = sparse,
    scores = scores,
    scaling = scaling,
    proportion_of_trace = proportion_of_trace,
    predicted_class = factor(pred_class, levels = levels(grouping)),
    posterior = pred_post
  )

  result$confusion <- build_confusion_stats(
    grouping, result$predicted_class
  )

  if (sparse) {
    result$keep_x <- keep_x
    result$selected_variables <- lapply(
      seq_len(ncomp),
      function(i) {
        loadings_i <- scaling[[i]]
        rownames(scaling)[loadings_i != 0]
      }
    )
    names(result$selected_variables) <- comp_names
  }

  # VIP (Variable Importance in Projection): aggregates each
  # variable's contribution across all components, weighted by
  # each component's explained variance in Y. Standard mixOmics
  # companion to loadings for ranking variables by discriminating
  # power (VIP > 1 conventionally flags above-average importance).
  vip_mat <- tryCatch(
    as.data.frame(mixOmics$vip(model)),
    error = function(e) NULL
  )
  if (!is.null(vip_mat)) {
    colnames(vip_mat) <- comp_names
    result$vip <- vip_mat
  }

  rhino$log$info(
    "{if (sparse) 'sPLS-DA' else 'PLS-DA'}: model fit ",
    "complete — resubstitution accuracy ",
    "{round(result$confusion$accuracy * 100, 1)}%"
  )

  result
}


build_cv_classification <- function(obj, grouping) {
  predicted <- obj$class
  posterior <- as.data.frame(obj$posterior)

  confusion <- build_confusion_stats(
    grouping, predicted
  )

  list(
    predicted_class = predicted,
    posterior = posterior,
    confusion = confusion,
    accuracy = confusion$accuracy
  )
}


build_mda_result <- function(mda_obj, data, numeric_data,
                             columns, grouping_col,
                             grouping, meta_cols,
                             prior_vec) {
  n <- nrow(data)
  p <- length(columns)
  n_groups <- nlevels(grouping)

  # Build metadata
  meta <- if (length(meta_cols) > 0) {
    data[, meta_cols, drop = FALSE]
  } else {
    data.frame(Row = seq_len(n))
  }

  # Predict on training data for scores + posterior
  pred_all <- stats::predict(
    mda_obj, numeric_data
  )
  pred_post <- stats::predict(
    mda_obj, numeric_data, type = "posterior"
  )

  # Discriminant scores from predict
  scores_mat <- stats::predict(
    mda_obj, numeric_data, type = "variates"
  )
  if (is.null(scores_mat)) {
    # Fallback: use companion LDA for projection
    scores_mat <- matrix(
      nrow = n, ncol = 0
    )
  }
  scores_df <- as.data.frame(scores_mat)
  if (ncol(scores_df) > 0) {
    colnames(scores_df) <- paste0(
      "DC", seq_len(ncol(scores_df))
    )
  }

  # Proportion of trace from percent.explained
  # NOTE: percent.explained is cumulative (e.g. 90, 97, 100)
  pct_explained <- mda_obj$percent.explained
  if (!is.null(pct_explained) && length(pct_explained) > 0) {
    n_dim <- length(pct_explained)
    cum_vals <- pct_explained / 100
    # Individual proportions by differencing
    prop_vals <- c(
      cum_vals[1],
      diff(cum_vals)
    )
    # percent.explained is documented as cumulative (non-decreasing),
    # but EM instability or dimension truncation in mda::mda() can
    # return a non-monotonic vector. Differencing that would yield a
    # negative "Proportion" and a decreasing "Cumulative" column, which
    # is nonsensical and would propagate silently into the Variable
    # Contributions plot via lda_to_pca_var_structure(). Clamp instead.
    if (any(prop_vals < 0, na.rm = TRUE)) {
      n_neg <- sum(prop_vals < 0, na.rm = TRUE)
      rhino$log$warn(
        paste(
          "MDA: mda::mda() returned a non-monotonic",
          "percent.explained; clamping {n_neg} negative",
          "proportion(s) to 0. Explained-variance figures for",
          "this fit may be unreliable - consider reducing",
          "the subclass count."
        )
      )
      prop_vals <- pmax(prop_vals, 0)
      # Keep Cumulative consistent with the clamped proportions.
      cum_vals <- cumsum(prop_vals)
    }
    proportion_of_trace <- data.frame(
      LD = paste0("DC", seq_len(n_dim)),
      `Proportion` = round(prop_vals, 4),
      `Cumulative` = round(cum_vals, 4),
      check.names = FALSE
    )
  } else {
    proportion_of_trace <- NULL
  }

  # Scaling coefficients from the fit component
  # coef.mda needs stats functions, so eval in globalenv
  scaling <- tryCatch(
    {
      env <- new.env(parent = globalenv())
      env$mda_obj <- mda_obj
      coefs <- eval(
        quote(coef(mda_obj)), envir = env
      )
      if (!is.null(coefs) && is.matrix(coefs)) {
        as.data.frame(coefs)
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  # Group means from the mda object
  means <- tryCatch(
    as.data.frame(mda_obj$means),
    error = function(e) NULL
  )

  # Subclass priors
  sub_prior <- mda_obj$sub.prior

  result <- list(
    analysis_type = "mda",
    grouping_col = grouping_col,
    columns = columns,
    n = n,
    p = p,
    n_groups = n_groups,
    group_levels = levels(grouping),
    prior = prior_vec,
    means = means,
    meta = meta,
    model = mda_obj,
    scores = scores_df,
    scaling = scaling,
    proportion_of_trace = proportion_of_trace,
    sub_prior = sub_prior,
    subclasses = mda_obj$subclasses,
    dimension = mda_obj$dimension,
    deviance = mda_obj$deviance,
    predicted_class = pred_all,
    posterior = as.data.frame(pred_post)
  )

  # SVD-like field for dimension count
  if (!is.null(proportion_of_trace)) {
    result$svd <- proportion_of_trace$Proportion
  }

  # Confusion on training data (resubstitution)
  result$confusion <- build_confusion_stats(
    grouping, pred_all
  )

  rhino$log$info(
    "MDA: model fit complete — ",
    "dimension={mda_obj$dimension}, ",
    "resubstitution accuracy ",
    "{round(result$confusion$accuracy * 100, 1)}%"
  )

  result
}


build_mda_cv_result <- function(data, numeric_data,
                                grouping, columns,
                                grouping_col, meta_cols,
                                prior_vec, subclasses,
                                iter, dimension, eps) {
  n <- nrow(data)
  n_groups <- nlevels(grouping)
  lvls <- levels(grouping)

  predicted <- factor(
    rep(NA_character_, n), levels = lvls
  )
  posterior <- matrix(
    NA_real_, nrow = n, ncol = n_groups
  )
  colnames(posterior) <- lvls

  for (i in seq_len(n)) {
    train_data <- numeric_data[-i, , drop = FALSE]
    train_g <- grouping[-i]
    test_obs <- numeric_data[i, , drop = FALSE]

    fit_data <- cbind(
      train_data, .grouping. = train_g
    )

    fold_fit <- tryCatch(
      fit_mda(
        fit_data, subclasses, iter,
        dimension, eps
      ),
      error = function(e) NULL
    )

    if (!is.null(fold_fit)) {
      fold_pred <- stats::predict(
        fold_fit, test_obs
      )
      fold_post <- stats::predict(
        fold_fit, test_obs, type = "posterior"
      )
      predicted[i] <- as.character(fold_pred)
      posterior[i, ] <- as.numeric(fold_post)
    }
  }

  posterior <- as.data.frame(posterior)

  # Build metadata
  meta <- if (length(meta_cols) > 0) {
    data[, meta_cols, drop = FALSE]
  } else {
    data.frame(Row = seq_len(n))
  }

  confusion <- build_confusion_stats(
    grouping, predicted
  )

  rhino$log$info(
    "MDA: LOO-CV complete — accuracy ",
    "{round(confusion$accuracy * 100, 1)}%"
  )

  list(
    analysis_type = "mda",
    grouping_col = grouping_col,
    columns = columns,
    n = n,
    p = length(columns),
    n_groups = n_groups,
    group_levels = lvls,
    prior = prior_vec,
    means = NULL,
    meta = meta,
    model = NULL,
    cv = list(
      predicted_class = predicted,
      posterior = posterior,
      confusion = confusion,
      accuracy = confusion$accuracy
    )
  )
}


build_confusion_stats <- function(true_labels,
                                  predicted_labels) {
  cm <- table(
    True = true_labels,
    Predicted = predicted_labels
  )
  correct <- sum(diag(cm))
  total <- sum(cm)
  accuracy <- correct / total

  # Per-class metrics
  levels_all <- union(
    levels(true_labels),
    levels(as.factor(predicted_labels))
  )
  per_class <- lapply(levels_all, function(cls) {
    tp <- if (
      cls %in% rownames(cm) && cls %in% colnames(cm)
    ) cm[cls, cls] else 0
    fn <- if (cls %in% rownames(cm)) {
      sum(cm[cls, ]) - tp
    } else {
      0
    }
    fp <- if (cls %in% colnames(cm)) {
      sum(cm[, cls]) - tp
    } else {
      0
    }
    precision <- if (tp + fp > 0) tp / (tp + fp) else NA
    recall <- if (tp + fn > 0) tp / (tp + fn) else NA
    f1 <- if (
      !is.na(precision) && !is.na(recall) &&
      (precision + recall) > 0
    ) {
      2 * precision * recall / (precision + recall)
    } else {
      NA
    }
    data.frame(
      Class = cls,
      N = if (cls %in% rownames(cm)) {
        sum(cm[cls, ])
      } else {
        0
      },
      Correct = tp,
      Precision = round(precision, 4),
      Recall = round(recall, 4),
      F1 = round(f1, 4),
      stringsAsFactors = FALSE
    )
  })
  per_class_df <- do.call(rbind, per_class)

  list(
    matrix = cm,
    accuracy = accuracy,
    per_class = per_class_df
  )
}


#' Error parser for LDA/QDA-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
lda_error_parser <- function(error_msg,
                             operation_name = "LDA") {
  if (grepl(
    "keepX",
    error_msg, ignore.case = FALSE
  )) {
    paste0(
      operation_name,
      ": Invalid 'keep variables' setting.",
      " Each component's keepX value must be between 1",
      " and the number of measurement columns.",
      " Reduce keepX or select more measurement columns."
    )
  } else if (grepl(
    "singular|rank deficien",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Within-group covariance matrix is singular.",
      " Some variables may be constant within groups",
      " or highly collinear.",
      " Try reducing dimensionality via PCA first."
    )
  } else if (grepl(
    "NAs are not allowed in subscripted",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Internal error with NA values. ",
      "This often occurs when some groups have too few observations ",
      "for the requested number of subclasses. ",
      "Try reducing the number of subclasses or using LDA instead."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values.",
      " Please handle missing data first."
    )
  } else if (grepl(
    "some group is too small",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Some group is too small. ",
      "QDA requires at least (p + 1) observations per group, ",
      "where p is the number of variables. ",
      "Consider using LDA instead, or reduce dimensionality via PCA."
    )
  } else if (grepl(
    "group|level|factor",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Problem with grouping variable.",
      " Ensure it has at least 2 levels",
      " with sufficient observations each."
    )
  } else if (grepl(
    "converg|iteration|EM",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": EM algorithm did not converge.",
      " Try increasing the number of iterations",
      " or reducing the number of subclasses."
    )
  } else if (grepl(
    "subclass|mixture",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Problem with subclass configuration.",
      " Ensure each group has enough observations",
      " for the requested number of subclasses."
    )
  } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": All measurement columns must be numeric."
    )
  } else if (grepl(
    "variables.*constant|zero variance",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Some variables have zero within-group variance.",
      " Remove constant columns or use PCA scores."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
