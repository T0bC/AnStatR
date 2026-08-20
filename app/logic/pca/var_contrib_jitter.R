box::use(
  ggplot2,
  ggiraph,
  ggrepel,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/pca/pca_stats[
    compute_var_coord, compute_var_contrib, compute_var_cos2
  ],
)

# =============================================================================
# Variable contribution jitter/strip plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a faceted strip plot of variable contributions per dimension
#'
#' Each dimension is a facet with independent Y-axis (contribution %).
#' Points colored by cos2 (viridis), labeled with ggrepel.
#'
#' For high-dimensional data (20+ variables):
#' - Variables with cos2 < 0.2 are filtered out per dimension
#' - Dimensions where no variable survives the filter are dropped
#' - Facet strips annotated with surviving variable count
#' - Y-axis starts from data minimum (not 0) to maximize spread
#'
#' @param pca_result PCA result list (the $result field from
#'   run_pca()), with $loadings and $scores. Callers whose
#'   loadings are not unit-norm (e.g. LDA scaling coefficients,
#'   via lda_to_pca_var_structure()) may instead supply
#'   pre-computed $contrib / $coord / $cos2 directly on this
#'   list — when present, those are used as-is instead of being
#'   derived from $loadings / $scores.
#' @param display_ncp Integer, number of dimensions to show
#' @param show_title Logical, whether to show the plot title
#' @param cos2_threshold Numeric, cos2 cutoff for high-dim filtering
#' @return List with $success, $result (ggplot) or $error
#' @export
create_var_contrib_jitter_plot <- function(pca_result,
                                           display_ncp = 5L,
                                           show_title = TRUE,
                                           cos2_threshold = 0.2) {
  error_context <- list(display_ncp = display_ncp)

  error_handling$safe_execute(
    expr = {
      if (is.null(pca_result)) stop("pca_result is NULL")

      loadings <- pca_result$loadings
      contrib <- pca_result$contrib %||% compute_var_contrib(loadings)
      coord <- pca_result$coord %||%
        compute_var_coord(loadings, pca_result$scores)
      cos2 <- pca_result$cos2 %||% compute_var_cos2(coord)
      n_dims <- min(display_ncp, ncol(contrib))
      n_vars <- nrow(contrib)
      dims <- colnames(contrib)[seq_len(n_dims)]
      variance <- pca_result$variance
      high_dim <- n_vars >= 20

      # Build long-format data frame (all variables)
      rows <- list()
      for (i in seq_along(dims)) {
        d <- dims[i]
        loading <- as.numeric(coord[, d])
        rows[[i]] <- data.frame(
          variable = rownames(contrib),
          dim = d,
          dim_idx = i,
          contrib = as.numeric(contrib[, d]),
          cos2 = as.numeric(cos2[, d]),
          loading = loading,
          loading_sign = ifelse(
            loading >= 0, "positive", "negative"
          ),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
      df <- do.call(rbind, rows)

      # --- High-dimensional filtering (soft threshold) ---
      dropped_dims <- character(0)
      dropped_max_cos2 <- numeric(0)

      if (high_dim) {
        min_vars_per_dim <- 4L
        keep <- logical(nrow(df))

        for (d in dims) {
          idx <- which(df$dim == d)
          sub_cos2 <- df$cos2[idx]
          above <- sub_cos2 >= cos2_threshold

          if (sum(above) == 0) {
            # No variable passes threshold -> drop dim
            dropped_dims <- c(dropped_dims, d)
            dropped_max_cos2 <- c(
              dropped_max_cos2, max(sub_cos2)
            )
            next
          }

          if (sum(above) <= 2) {
            # Soft pad: include next-best by cos2
            rank_order <- order(
              sub_cos2, decreasing = TRUE
            )
            n_keep <- min(
              min_vars_per_dim, length(rank_order)
            )
            above[rank_order[seq_len(n_keep)]] <- TRUE
          }

          keep[idx] <- above
        }

        df <- df[keep, , drop = FALSE]

        if (nrow(df) == 0) {
          stop(
            "No variables with cos\u00b2 >= ",
            cos2_threshold,
            " in any dimension."
          )
        }

        # Drop dimensions with no surviving variables
        surviving_dims <- unique(df$dim)
        dims <- dims[dims %in% surviving_dims]
      }

      # Build dimension labels with variance %
      dim_labels <- vapply(dims, function(d) {
        idx <- which(rownames(variance) == d)
        if (length(idx) == 1) {
          sprintf(
            "%s (%.1f%%)", d, variance[idx, "variance_percent"]
          )
        } else {
          d
        }
      }, character(1))

      # Map dim -> label and set factor levels
      dim_label_map <- stats::setNames(
        dim_labels, dims
      )
      df$dim_label <- dim_label_map[df$dim]
      df$dim_label <- factor(
        df$dim_label, levels = dim_labels
      )

      # Pre-compute jittered x position (fixed seed)
      set.seed(42)
      df$x <- stats::runif(
        nrow(df), min = -0.25, max = 0.25
      )

      # Smart filtering: decide which points get labels
      n_vars_filtered <- if (high_dim) {
        max(vapply(
          dims,
          function(d) sum(df$dim == d),
          integer(1)
        ))
      } else {
        n_vars
      }
      df$show_label <- select_label_vars(
        df, n_vars_filtered, length(dims)
      )

      # Label text with direction prefix
      df$label_text <- ifelse(
        df$loading_sign == "positive",
        paste0("\u25B2 ", df$variable),
        paste0("\u25BC ", df$variable)
      )

      # Tooltips
      sign_txt <- ifelse(
        df$loading_sign == "positive",
        "positive", "negative"
      )
      df$tooltip <- sprintf(
        paste0(
          "<b>%s</b>",
          "<br/>%s",
          "<br/>Contribution: %.2f%%",
          "<br/>cos\u00b2: %.4f",
          "<br/>Loading: %s (%.4f)"
        ),
        df$variable,
        df$dim_label,
        df$contrib,
        df$cos2,
        sign_txt,
        df$loading
      )
      df$data_id <- paste0(
        "vcj_", df$variable, "_", df$dim
      )

      # Label data (subset for repel)
      label_df <- df[df$show_label, , drop = FALSE]

      # Adaptive point size
      point_size <- if (n_vars_filtered <= 15) 10 else 9

      p <- ggplot2$ggplot(
        df,
        ggplot2$aes(x = x, y = contrib)
      ) +
        ggiraph$geom_point_interactive(
          ggplot2$aes(
            fill = cos2,
            tooltip = tooltip,
            data_id = data_id
          ),
          shape = 21,
          color = "white",
          stroke = 0.6,
          size = point_size,
          position = "identity"
        ) +
        ggiraph$geom_text_repel_interactive(
          data = label_df,
          ggplot2$aes(
            label = label_text,
            color = loading_sign,
            tooltip = tooltip,
            data_id = data_id
          ),
          size = 3.8,
          fontface = "bold",
          max.overlaps = Inf,
          segment.color = "grey30",
          segment.size = 0.4,
          min.segment.length = 0.2,
          #segment.curvature = 0.1,
          #segment.ncp = 2,
          #segment.angle = 130,
          box.padding = 0.5,
          point.padding = 0.3,
          direction = "y",
          hjust = 0,
          xlim = c(0.4, NA),
          show.legend = FALSE
        ) +
        ggplot2$facet_wrap(
          ~ dim_label,
          nrow = 1,
          scales = "free_y"
        ) +
        ggplot2$scale_color_manual(
          values = c(
            "positive" = "#2166AC",
            "negative" = "#B2182B"
          ),
          guide = "none"
        ) +
        ggplot2$scale_fill_viridis_c(
          option = "viridis",
          name = "cos\u00b2",
          limits = c(0, 1)
        ) +
        ggplot2$scale_x_continuous(
          limits = c(-0.6, 1.2),
          expand = ggplot2$expansion(0)
        ) +
        ggplot2$scale_y_continuous(
          expand = ggplot2$expansion(mult = c(0.08, 0.08))
        ) +
        ggplot2$labs(x = NULL, y = NULL) +
        jitter_theme()

      if (show_title) {
        p <- p + ggplot2$ggtitle(
          "Variable Contributions by Dimension"
        )
      }

      # Return plot + filtering metadata
      list(
        plot = p,
        filter_applied = high_dim,
        cos2_threshold = cos2_threshold,
        n_vars_total = n_vars,
        dropped_dims = dropped_dims,
        dropped_max_cos2 = dropped_max_cos2,
        n_dims_shown = length(dims),
        n_dims_requested = n_dims
      )
    },
    operation_name = "Variable Contribution Jitter Plot",
    context = error_context,
    error_parser = var_contrib_jitter_error_parser
  )
}


#' Shared ggiraph options for the variable contribution jitter plot
#'
#' Centralized so the PCA and LDA/QDA/MDA views render an identical
#' interactive style. Hover CSS is split per element type via
#' \code{girafe_css()}: points get a black outline highlight, but
#' the repelled variable-name labels are excluded (only their color
#' is nudged) so a hovered label's own black stroke doesn't get
#' applied to its text and render it illegible.
#'
#' @return List of ggiraph options, for \code{girafe(options = ...)}
#' @export
var_contrib_jitter_girafe_opts <- function() {
  list(
    ggiraph$opts_sizing(rescale = TRUE, width = 1),
    ggiraph$opts_hover(
      css = ggiraph$girafe_css(
        css = "fill-opacity:1;stroke:black;stroke-width:2px;",
        text = "fill-opacity:1;stroke:none;"
      )
    ),
    ggiraph$opts_tooltip(
      css = paste0(
        "background-color:white;padding:8px;",
        "border-radius:4px;",
        "border:1px solid #ccc;",
        "font-family:sans-serif;"
      ),
      use_fill = FALSE
    ),
    ggiraph$opts_selection(type = "none")
  )
}


#' Error parser for variable contribution jitter plot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
var_contrib_jitter_error_parser <- function(
    error_msg,
    operation_name = "Variable Contribution Jitter Plot") {
  if (grepl(
    "dimension|dim|not found",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid dimension selection.",
      " Please check available components."
    )
  } else if (grepl(
    "NULL|missing|pca_result",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": No PCA result available.",
      " Please compute PCA first."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Select which variables get labels per dimension
#'
#' Strategy:
#' - <= 10 variables: label all
#' - > 10: per dimension, label variables whose contribution
#'   exceeds the expected average AND cos2 >= 0.1
#'   (at least top 3 per dim, at most 10)
select_label_vars <- function(df, n_vars, n_dims) {
  if (n_vars <= 10) {
    return(rep(TRUE, nrow(df)))
  }

  # Expected average contribution per variable
  threshold <- 100 / n_vars
  cos2_min <- 0.1

  show <- logical(nrow(df))
  dims <- levels(df$dim_label)

  for (d in dims) {
    idx <- which(df$dim_label == d)
    sub <- df[idx, , drop = FALSE]

    # Variables above contribution threshold with decent cos2
    above <- sub$contrib >= threshold & sub$cos2 >= cos2_min

    # Ensure at least top 3
    if (sum(above) < 3) {
      top_idx <- order(sub$contrib, decreasing = TRUE)
      above[top_idx[seq_len(min(3, length(top_idx)))]] <- TRUE
    }

    # Cap at 10 labels per dim to avoid clutter
    if (sum(above) > 10) {
      top_idx <- order(
        sub$contrib[above], decreasing = TRUE
      )
      keep <- which(above)[top_idx[seq_len(10)]]
      above <- logical(length(above))
      above[keep] <- TRUE
    }

    show[idx] <- above
  }

  show
}


#' Theme for the faceted jitter plot
jitter_theme <- function() {
  ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        hjust = 0.5, size = 14, face = "bold"
      ),
      # Remove X-axis entirely (dummy position)
      axis.title.x = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_blank(),
      axis.ticks.x = ggplot2$element_blank(),
      # Remove Y-axis (relative order matters, not values)
      axis.title.y = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_blank(),
      axis.ticks.y = ggplot2$element_blank(),
      # Facet strip styling
      strip.text = ggplot2$element_text(
        size = 12, face = "bold"
      ),
      strip.background = ggplot2$element_rect(
        fill = "grey95", color = NA
      ),
      # Tight spacing between facets
      panel.spacing = ggplot2$unit(0.15, "cm"),
      # Legend
      legend.position = "right",
      legend.key.height = ggplot2$unit(1.5, "cm"),
      legend.key.width = ggplot2$unit(0.4, "cm"),
      legend.title = ggplot2$element_text(size = 12),
      legend.text = ggplot2$element_text(size = 10),
      # Grid: only horizontal lines for contribution reference
      panel.grid.major.x = ggplot2$element_blank(),
      panel.grid.minor = ggplot2$element_blank(),
      panel.grid.major.y = ggplot2$element_blank()
    )
}
