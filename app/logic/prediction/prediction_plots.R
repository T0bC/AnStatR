box::use(
  ggiraph,
  ggplot2,
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/lda/ld_plot[
    create_ld_plot, create_qda_plot,
    axis_label,
  ],
  app/logic/pca/biplot[create_biplot],
  app/logic/pca/pca[build_pca_result, build_ind_meta],
  app/logic/cluster/cluster_biplot[create_cluster_biplot],
)

# =============================================================================
# Prediction overlay plots
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a prediction overlay plot
#'
#' Recreates the training plot from bundled data and
#' overlays unknown sample predictions with distinct
#' visual encoding.
#'
#' @param bundle The prediction bundle
#' @param prediction_result Result from predict_unknown()
#' @param unknown_data Original unknown data frame
#'   (for metadata / labels)
#' @param dim_x Character, x-axis dimension name
#' @param dim_y Character, y-axis dimension name
#' @param meta_col Character or NULL, metadata column
#'   to use as unknown labels
#' @param group_cols Character vector or NULL, metadata
#'   columns for grouping training data (PCA only)
#' @param show_convex_hull Logical, use convex hull
#'   instead of 95% ellipse (PCA only)
#' @param point_alpha Character or numeric, point alpha
#'   for PCA biplot ("Contribution" or fixed, PCA only)
#' @param point_size Character or numeric, point size
#'   for PCA biplot ("Contribution" or fixed, PCA only)
#' @param layer Character, biplot layer: "individuals",
#'   "variables", or "combined" (PCA only)
#' @param show_diagnostics Logical, overlay assumption
#'   diagnostics (LDA/MDA only)
#' @param show_boundaries Logical, overlay decision
#'   boundary regions (LDA/MDA/QDA)
#' @return List with $success, $result (ggplot) or $error
#' @export
create_prediction_overlay_plot <- function(
    bundle, prediction_result, unknown_data,
    dim_x, dim_y, meta_col = NULL,
    group_cols = NULL,
    show_convex_hull = FALSE,
    point_alpha = 0.5,
    point_size = 2.5,
    layer = "individuals",
    show_diagnostics = FALSE,
    show_boundaries = FALSE) {
  error_handling$safe_execute(
    expr = {
      analysis_type <- bundle$analysis_type

      p <- switch(
        analysis_type,
        pca = build_pca_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col,
          group_cols = group_cols,
          show_convex_hull = show_convex_hull,
          point_alpha = point_alpha,
          point_size = point_size,
          layer = layer
        ),
        lda = build_ld_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col,
          show_diagnostics = show_diagnostics,
          show_boundaries = show_boundaries
        ),
        mda = build_ld_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col,
          show_diagnostics = show_diagnostics,
          show_boundaries = show_boundaries
        ),
        qda = build_qda_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col,
          show_boundaries = show_boundaries
        ),
        cluster = build_cluster_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col
        ),
        plsda = build_ld_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col,
          show_diagnostics = FALSE,
          show_boundaries = show_boundaries
        ),
        splsda = build_ld_overlay(
          bundle, prediction_result,
          unknown_data, dim_x, dim_y, meta_col,
          show_diagnostics = FALSE,
          show_boundaries = show_boundaries
        ),
        stop(paste0(
          "Unsupported analysis type: '",
          analysis_type, "'"
        ))
      )

      rhino$log$info(
        "Prediction plot: ",
        "{toupper(analysis_type)} overlay created"
      )

      p
    },
    operation_name = "Prediction Overlay Plot",
    error_parser = error_handling$default_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Build PCA overlay: reuse create_biplot for training
#' data, then layer unknown scores on top.
build_pca_overlay <- function(bundle, pred_result,
                              unknown_data, dim_x,
                              dim_y, meta_col,
                              group_cols = NULL,
                              show_convex_hull = FALSE,
                              point_alpha = 0.5,
                              point_size = 2.5,
                              layer = "individuals") {
  # Reconstruct pca_result from bundle so we can
  # delegate to create_biplot (single source of truth)
  pca_result <- reconstruct_pca_result(bundle)

  # Render training biplot via the PCA module
  biplot_res <- create_biplot(
    pca_result = pca_result,
    dim_x = dim_x,
    dim_y = dim_y,
    layer = layer,
    group_cols = group_cols,
    show_convex_hull = show_convex_hull,
    point_alpha = point_alpha,
    point_size = point_size,
    show_title = FALSE
  )

  if (!biplot_res$success) {
    stop(biplot_res$error$message %||% "Biplot failed")
  }

  p <- biplot_res$result

  # Build unknown scores data frame
  unknown_scores <- pred_result$scores
  unknown_df <- data.frame(
    x = unknown_scores[[dim_x]],
    y = unknown_scores[[dim_y]],
    stringsAsFactors = FALSE
  )

  # Labels from metadata
  if (
    !is.null(meta_col) &&
    meta_col %in% names(unknown_data)
  ) {
    unknown_df$label <- as.character(
      unknown_data[[meta_col]]
    )
  } else {
    unknown_df$label <- paste0(
      "Unknown_", seq_len(nrow(unknown_df))
    )
  }

  # Tooltips for unknowns
  unknown_df$tooltip <- paste0(
    "<b>", unknown_df$label, "</b><br>",
    dim_x, ": ", round(unknown_df$x, 3), "<br>",
    dim_y, ": ", round(unknown_df$y, 3)
  )
  unknown_df$data_id <- paste0(
    "unknown_", seq_len(nrow(unknown_df))
  )

  # Layer unknown points (triangles) on top of biplot
  p <- p +
    ggiraph$geom_point_interactive(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y,
        tooltip = tooltip,
        data_id = data_id
      ),
      shape = 24,
      fill = "red",
      color = "black",
      stroke = 0.8,
      size = 4,
      alpha = 0.95
    )

  # Labels for unknown points — use pca_result coords
  # for y range (p$data is empty in multi-layer ggplot)
  train_y <- pca_result$ind$coord[, dim_y]
  y_range <- diff(range(
    c(unknown_df$y, train_y),
    na.rm = TRUE
  ))
  p <- p +
    ggplot2$geom_text(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y, label = label
      ),
      nudge_y = -y_range * 0.03,
      size = 2.8,
      color = "grey30",
      fontface = "italic"
    )

  # Add title and subtitle
  n_train <- nrow(bundle$used_data)
  n_unknown <- nrow(unknown_df)
  p <- p + ggplot2$ggtitle(
    label = paste0(
      "PCA Prediction \u2014 ",
      dim_x, " vs ", dim_y
    ),
    subtitle = paste0(
      n_train, " training + ",
      n_unknown, " unknown samples"
    )
  )

  p
}

#' Build LDA/MDA overlay: delegate to create_ld_plot
#' for the training base layer, then overlay unknowns.
build_ld_overlay <- function(bundle, pred_result,
                             unknown_data, dim_x,
                             dim_y, meta_col,
                             show_diagnostics = FALSE,
                             show_boundaries = FALSE) {
  # Reconstruct lda_result-like structure from bundle
  lda_like <- reconstruct_lda_result(bundle)

  # Delegate training plot to create_ld_plot
  plot_res <- create_ld_plot(
    lda_result = lda_like,
    dim_x = dim_x,
    dim_y = dim_y,
    show_diagnostics = show_diagnostics,
    show_boundaries = show_boundaries
  )

  if (!plot_res$success) {
    stop(
      plot_res$error$message %||%
      "LD plot failed"
    )
  }

  p <- plot_res$result

  # Build unknown overlay data frame
  unknown_scores <- pred_result$scores
  unknown_df <- build_unknown_overlay_df(
    unknown_scores, pred_result, unknown_data,
    dim_x, dim_y, meta_col
  )

  # Layer unknown points (triangles) on top
  p <- p +
    ggiraph$geom_point_interactive(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y,
        fill = group,
        tooltip = tooltip,
        data_id = data_id
      ),
      shape = 24,
      color = "black",
      stroke = 0.8,
      size = 4,
      alpha = 0.95
    )

  # Add unknown labels
  prop_trace <- lda_like$proportion_of_trace
  y_range <- diff(range(
    c(unknown_df$y, lda_like$scores[[dim_y]]),
    na.rm = TRUE
  ))
  p <- p +
    ggplot2$geom_text(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y, label = label
      ),
      nudge_y = -y_range * 0.03,
      size = 2.8,
      color = "grey30",
      fontface = "italic"
    )

  # Update title and subtitle with prediction info
  n_train <- nrow(bundle$used_data)
  n_unknown <- nrow(unknown_df)
  x_label <- axis_label(dim_x, prop_trace)
  y_label <- axis_label(dim_y, prop_trace)

  title_prefix <- switch(
    bundle$analysis_type,
    mda = "MDA Prediction",
    plsda = "PLS-DA Prediction",
    splsda = "sPLS-DA Prediction",
    "LDA Prediction"
  )

  p <- p +
    ggplot2$labs(
      x = x_label,
      y = y_label,
      title = paste0(
        title_prefix, " \u2014 ",
        dim_x, " vs ", dim_y
      ),
      subtitle = paste0(
        n_train, " training + ",
        n_unknown, " unknown samples"
      )
    )

  p
}

#' Build QDA overlay: delegate to create_qda_plot
#' for the training base layer, then overlay unknowns.
build_qda_overlay <- function(bundle, pred_result,
                              unknown_data, dim_x,
                              dim_y, meta_col,
                              show_boundaries = FALSE) {
  # Reconstruct qda_result-like structure from bundle
  qda_like <- reconstruct_qda_result(bundle)

  # Delegate training plot to create_qda_plot
  plot_res <- create_qda_plot(
    qda_result = qda_like,
    dim_x = dim_x,
    dim_y = dim_y,
    show_boundaries = show_boundaries
  )

  if (!plot_res$success) {
    stop(
      plot_res$error$message %||%
      "QDA plot failed"
    )
  }

  p <- plot_res$result

  # Build unknown overlay data frame
  unknown_scores <- pred_result$scores
  if (is.null(unknown_scores)) {
    stop(paste(
      "No LD scores available for unknowns.",
      "QDA companion LDA projection failed."
    ))
  }

  unknown_df <- build_unknown_overlay_df(
    unknown_scores, pred_result, unknown_data,
    dim_x, dim_y, meta_col
  )

  # Layer unknown points (triangles) on top
  p <- p +
    ggiraph$geom_point_interactive(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y,
        fill = group,
        tooltip = tooltip,
        data_id = data_id
      ),
      shape = 24,
      color = "black",
      stroke = 0.8,
      size = 4,
      alpha = 0.95
    )

  # Add unknown labels
  all_y <- c(unknown_df$y, qda_like$lda_scores[[dim_y]])
  y_range <- diff(range(all_y, na.rm = TRUE))
  p <- p +
    ggplot2$geom_text(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y, label = label
      ),
      nudge_y = -y_range * 0.03,
      size = 2.8,
      color = "grey30",
      fontface = "italic"
    )

  # Update title and subtitle
  n_train <- nrow(bundle$used_data)
  n_unknown <- nrow(unknown_df)
  p <- p +
    ggplot2$labs(
      title = paste0(
        "QDA Prediction \u2014 ",
        dim_x, " vs ", dim_y
      ),
      subtitle = paste0(
        n_train, " training + ",
        n_unknown, " unknown samples"
      )
    )

  p
}


#' Build Cluster overlay: delegate to create_cluster_biplot
#' (raw mode) for the training base layer, then overlay
#' unknowns as nearest-centroid/medoid assignments.
build_cluster_overlay <- function(bundle, pred_result,
                                  unknown_data, dim_x,
                                  dim_y, meta_col) {
  biplot_res <- create_cluster_biplot(
    data = bundle$used_data,
    measure_cols = bundle$numeric_cols,
    clusters = bundle$cluster_labels,
    meta_cols = bundle$meta_cols %||% character(0),
    dim_x = dim_x,
    dim_y = dim_y,
    reduction_method = "raw",
    show_title = FALSE
  )

  if (!biplot_res$success) {
    stop(
      biplot_res$error$message %||%
      "Cluster biplot failed"
    )
  }

  p <- biplot_res$result$plot

  # Build unknown overlay data frame
  unknown_scores <- pred_result$scores
  unknown_df <- build_unknown_overlay_df(
    unknown_scores, pred_result, unknown_data,
    dim_x, dim_y, meta_col
  )

  # Layer unknown points (triangles) on top
  p <- p +
    ggiraph$geom_point_interactive(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y,
        tooltip = tooltip,
        data_id = data_id
      ),
      shape = 24,
      fill = "red",
      color = "black",
      stroke = 0.8,
      size = 4,
      alpha = 0.95
    )

  # Add unknown labels
  train_y <- bundle$used_data[[dim_y]]
  y_range <- diff(range(
    c(unknown_df$y, train_y), na.rm = TRUE
  ))
  p <- p +
    ggplot2$geom_text(
      data = unknown_df,
      ggplot2$aes(
        x = x, y = y, label = label
      ),
      nudge_y = -y_range * 0.03,
      size = 2.8,
      color = "grey30",
      fontface = "italic"
    )

  # Add title and subtitle
  n_train <- nrow(bundle$used_data)
  n_unknown <- nrow(unknown_df)
  variant_label <- if (bundle$variant == "pam") {
    "PAM"
  } else {
    "K-Means"
  }
  p <- p + ggplot2$ggtitle(
    label = paste0(
      variant_label, " Prediction — ",
      dim_x, " vs ", dim_y
    ),
    subtitle = paste0(
      n_train, " training + ",
      n_unknown, " unknown samples"
    )
  )

  p
}

#' Reconstruct a pca_result structure from a bundle
#'
#' Uses the stored prcomp model and training data to
#' rebuild the same structure that create_biplot expects.
reconstruct_pca_result <- function(bundle) {
  model <- bundle$model
  used_data <- bundle$used_data
  numeric_cols <- bundle$numeric_cols
  meta_cols <- bundle$meta_cols %||% character(0)
  n <- nrow(used_data)
  p <- length(numeric_cols)
  ncp <- min(p, n - 1)

  result <- build_pca_result(model, ncp, n, p)
  result$pca_obj <- model
  result$ind$meta <- build_ind_meta(
    used_data, meta_cols, n
  )
  result
}

#' Reconstruct an lda_result-like structure from a bundle
#'
#' Builds the list that create_ld_plot expects from the
#' bundle's stored model and training data.
reconstruct_lda_result <- function(bundle) {
  model <- bundle$model
  used_data <- bundle$used_data
  numeric_cols <- bundle$numeric_cols
  group_col <- bundle$group_col
  meta_cols <- bundle$meta_cols %||% character(0)
  is_mda <- bundle$analysis_type == "mda"
  is_plsda <- bundle$analysis_type %in% c("plsda", "splsda")

  train_numeric <- used_data[
    , numeric_cols, drop = FALSE
  ]

  # Compute training LD/component scores + predicted classes
  if (is_plsda) {
    train_pred <- stats$predict(
      model, as.matrix(train_numeric)
    )
    scores <- as.data.frame(model$variates$X)
    if (ncol(scores) > 0) {
      colnames(scores) <- paste0(
        "Comp", seq_len(ncol(scores))
      )
    }
    n_comp <- ncol(train_pred$variates)
    predicted_class <- train_pred$class$max.dist[, n_comp]
    scaling <- as.data.frame(model$loadings$X)
    if (ncol(scaling) > 0) {
      colnames(scaling) <- paste0(
        "Comp", seq_len(ncol(scaling))
      )
    }
  } else if (is_mda) {
    scores_raw <- stats$predict(
      model, train_numeric, type = "variates"
    )
    scores <- as.data.frame(scores_raw)
    if (ncol(scores) > 0) {
      colnames(scores) <- paste0(
        "LD", seq_len(ncol(scores))
      )
    }
    predicted_class <- stats$predict(
      model, train_numeric
    )
    scaling <- NULL
  } else {
    train_pred <- stats$predict(model, train_numeric)
    scores <- as.data.frame(train_pred$x)
    predicted_class <- train_pred$class
    scaling <- as.data.frame(model$scaling)
  }

  # Group levels from training data
  grouping_vals <- used_data[[group_col]]
  group_levels <- if (is.factor(grouping_vals)) {
    levels(grouping_vals)
  } else {
    sort(unique(as.character(grouping_vals)))
  }

  # Build metadata — must include grouping column
  # so create_ld_plot's get_group_values can find it
  all_meta <- unique(c(group_col, meta_cols))
  available <- intersect(all_meta, names(used_data))
  meta <- if (length(available) > 0) {
    used_data[, available, drop = FALSE]
  } else {
    data.frame(Row = seq_len(nrow(used_data)))
  }

  # Build proportion of trace / explained variance
  if (is_plsda) {
    prop_vals <- as.numeric(model$prop_expl_var$X)
    proportion_of_trace <- data.frame(
      LD = colnames(scores),
      Proportion = round(prop_vals, 4),
      Cumulative = round(cumsum(prop_vals), 4),
      check.names = FALSE
    )
  } else if (!is_mda) {
    n_ld <- length(model$svd)
    prop_vals <- model$svd^2 / sum(model$svd^2)
    proportion_of_trace <- data.frame(
      LD = paste0("LD", seq_len(n_ld)),
      `Singular Value` = round(model$svd, 4),
      Proportion = round(prop_vals, 4),
      Cumulative = round(cumsum(prop_vals), 4),
      check.names = FALSE
    )
  } else {
    # MDA: derive from percent.explained
    pct <- model$percent.explained
    if (!is.null(pct) && length(pct) > 0) {
      cum_vals <- pct / 100
      prop_vals <- c(cum_vals[1], diff(cum_vals))
      n_dim <- length(pct)
      proportion_of_trace <- data.frame(
        LD = paste0("LD", seq_len(n_dim)),
        Proportion = round(prop_vals, 4),
        Cumulative = round(cum_vals, 4),
        check.names = FALSE
      )
    } else {
      proportion_of_trace <- NULL
    }
  }

  list(
    analysis_type = bundle$analysis_type,
    grouping_col = group_col,
    columns = numeric_cols,
    scores = scores,
    meta = meta,
    model = model,
    scaling = scaling,
    predicted_class = predicted_class,
    group_levels = group_levels,
    proportion_of_trace = proportion_of_trace
  )
}

#' Reconstruct a qda_result-like structure from a bundle
#'
#' Builds the list that create_qda_plot expects from the
#' bundle's stored model and training data.
reconstruct_qda_result <- function(bundle) {
  model <- bundle$model
  used_data <- bundle$used_data
  numeric_cols <- bundle$numeric_cols
  group_col <- bundle$group_col
  meta_cols <- bundle$meta_cols %||% character(0)

  if (is.null(bundle$lda_scores)) {
    stop(paste(
      "QDA bundle does not contain companion LDA",
      "scores for visualization."
    ))
  }

  # Build metadata — must include grouping column
  all_meta <- unique(c(group_col, meta_cols))
  available <- intersect(all_meta, names(used_data))
  meta <- if (length(available) > 0) {
    used_data[, available, drop = FALSE]
  } else {
    data.frame(Row = seq_len(nrow(used_data)))
  }

  # Group levels from training data
  grouping_vals <- used_data[[group_col]]
  group_levels <- if (is.factor(grouping_vals)) {
    levels(grouping_vals)
  } else {
    sort(unique(as.character(grouping_vals)))
  }

  # Build companion LDA proportion of trace
  lda_prop <- bundle$lda_proportion_of_trace

  list(
    analysis_type = "qda",
    grouping_col = group_col,
    columns = numeric_cols,
    meta = meta,
    model = model,
    group_levels = group_levels,
    lda_scores = bundle$lda_scores,
    lda_scaling = bundle$lda_scaling,
    lda_model = bundle$lda_model,
    lda_proportion_of_trace = lda_prop,
    numeric_data = used_data[
      , numeric_cols, drop = FALSE
    ]
  )
}

#' Build unknown sample overlay data frame
#'
#' Creates the data frame used to layer unknown
#' predictions on top of the training base plot.
build_unknown_overlay_df <- function(
    unknown_scores, pred_result, unknown_data,
    dim_x, dim_y, meta_col) {
  unknown_df <- data.frame(
    x = unknown_scores[[dim_x]],
    y = unknown_scores[[dim_y]],
    group = as.character(
      pred_result$predicted_class
    ),
    stringsAsFactors = FALSE
  )

  # Labels
  if (
    !is.null(meta_col) &&
    meta_col %in% names(unknown_data)
  ) {
    unknown_df$label <- as.character(
      unknown_data[[meta_col]]
    )
  } else {
    unknown_df$label <- paste0(
      "Unknown_", seq_len(nrow(unknown_df))
    )
  }

  # Tooltips
  unknown_df$tooltip <- paste0(
    "<b>", unknown_df$label, "</b><br>",
    "Predicted: ", unknown_df$group, "<br>",
    dim_x, ": ", round(unknown_df$x, 3), "<br>",
    dim_y, ": ", round(unknown_df$y, 3)
  )
  unknown_df$data_id <- paste0(
    "unknown_", seq_len(nrow(unknown_df))
  )

  unknown_df$group <- factor(unknown_df$group)
  unknown_df
}
