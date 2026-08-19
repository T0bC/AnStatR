box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/pca/pca_stats[compute_ind_contrib, compute_ind_cos2],
)

# =============================================================================
# Individual contribution jitter plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Error parser for individual contribution plot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
ind_contrib_error_parser <- function(error_msg,
                                     operation_name =
                                       "Individual Contributions") {
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


#' Create a jitter plot of individual contributions
#'
#' Dimensions on X-axis, contribution % on Y-axis.
#' Each individual is a jittered point per dimension,
#' with interactive tooltips showing the individual
#' label, dimension, contribution, and cos2.
#' A horizontal reference line marks the expected
#' average contribution (100 / n).
#'
#' @param pca_result PCA result list ($result from run_pca)
#' @param display_ncp Integer, number of dimensions to show
#' @param group_cols Character vector, column name(s) in
#'   ind_meta for coloring. NULL for no grouping.
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_ind_contrib_plot <- function(pca_result,
                                    display_ncp = 5L,
                                    group_cols = NULL,
                                    show_title = TRUE) {
  error_context <- list(
    display_ncp = display_ncp,
    group_cols = paste(
      group_cols %||% "none", collapse = ", "
    )
  )

  error_handling$safe_execute(
    expr = {
      if (is.null(pca_result)) {
        stop("pca_result is NULL")
      }

      scores <- pca_result$scores
      contrib_full <- compute_ind_contrib(scores)
      meta <- pca_result$ind_meta
      variance <- pca_result$variance
      n_obs <- nrow(scores)
      n_dims <- min(display_ncp, ncol(scores))
      dims <- colnames(scores)[seq_len(n_dims)]
      contrib <- contrib_full[, dims, drop = FALSE]
      cos2 <- compute_ind_cos2(scores, scores[, dims, drop = FALSE])
      threshold <- 100 / n_obs

      # Build dimension labels with variance %
      dim_labels <- vapply(dims, function(d) {
        idx <- which(rownames(variance) == d)
        if (length(idx) == 1) {
          sprintf(
            "%s (%.1f%%)",
            d, variance[idx, "variance_percent"]
          )
        } else {
          d
        }
      }, character(1))

      # Build long-format data frame
      rows <- vector("list", n_dims)
      for (i in seq_along(dims)) {
        d <- dims[i]
        rows[[i]] <- data.frame(
          label = rownames(contrib),
          dim = d,
          dim_label = dim_labels[i],
          contrib = contrib[, d],
          cos2 = cos2[, d],
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
      df <- do.call(rbind, rows)

      # Preserve dimension order
      df$dim_label <- factor(
        df$dim_label, levels = dim_labels
      )

      # Grouping
      df <- add_group_column(
        df, meta, group_cols, n_obs, n_dims
      )

      # Tooltips
      df$tooltip <- sprintf(
        paste0(
          "<b>%s</b>",
          "<br/>%s",
          "<br/>Contribution: %.2f%%",
          "<br/>cos\u00b2: %.4f"
        ),
        df$label,
        df$dim_label,
        df$contrib,
        df$cos2
      )
      df$data_id <- paste0(
        "ic_", df$label, "_", df$dim
      )

      # Build ggplot
      has_group <- "group" %in% names(df)

      # Always provide a fill column so shape 21 renders
      if (!has_group) {
        df$group <- factor("All")
      }

      # Dummy x position for faceted strip layout
      df$x_pos <- factor("")

      base_aes <- ggplot2$aes(
        x = x_pos,
        y = contrib,
        fill = group,
        tooltip = tooltip,
        data_id = data_id
      )

      p <- ggplot2$ggplot(df, base_aes) +
        ggiraph$geom_jitter_interactive(
          width = 0.25,
          height = 0,
          size = 5,
          shape = 21,
          color = "white",
          stroke = 0.6
        ) +
        ggplot2$geom_hline(
          yintercept = threshold,
          linetype = "dashed",
          color = "firebrick",
          linewidth = 0.5
        ) +
        ggplot2$geom_text(
          data = data.frame(
            x_pos = factor(""),
            contrib = threshold,
            dim_label = factor(
              df$dim_label[1],
              levels = levels(df$dim_label)
            )
          ),
          ggplot2$aes(
            x = x_pos, y = contrib,
            label = sprintf(
              "Avg. (%.2f%%)", threshold
            )
          ),
          hjust = 0.5,
          vjust = -0.5,
          size = 3.2,
          color = "firebrick",
          fontface = "italic",
          inherit.aes = FALSE
        ) +
        ggplot2$facet_wrap(
          ~ dim_label,
          nrow = 1,
          scales = "free_y"
        ) +
        ggplot2$scale_y_continuous(
          labels = function(x) paste0(x, "%")
        ) +
        ggplot2$labs(
          x = NULL,
          y = "Contribution (%)"
        ) +
        ind_contrib_theme()

      if (show_title) {
        p <- p + ggplot2$ggtitle(
          "Individual Contributions per Dimension"
        )
      }

      if (has_group) {
        p <- p + ggplot2$labs(fill = "Group")
      } else {
        p <- p +
          ggplot2$scale_fill_manual(
            values = "steelblue"
          ) +
          ggplot2$guides(fill = "none")
      }

      rhino$log$info(
        "Individual Contributions: plot created",
        " ({n_obs} individuals, {n_dims} dims)"
      )

      p
    },
    operation_name = "Individual Contributions",
    context = error_context,
    error_parser = ind_contrib_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Add group column to long-format data frame
add_group_column <- function(df, meta, group_cols,
                             n_obs, n_dims) {
  if (is.null(group_cols) ||
      length(group_cols) == 0 ||
      is.null(meta)) {
    return(df)
  }

  valid_cols <- intersect(group_cols, names(meta))
  if (length(valid_cols) == 0) return(df)

  # Build group vector for one copy of individuals
  if (length(valid_cols) == 1) {
    grp <- as.factor(meta[[valid_cols]])
  } else {
    grp <- interaction(
      meta[, valid_cols, drop = FALSE],
      sep = " / ", drop = TRUE
    )
  }

  # Repeat for each dimension
  df$group <- rep(grp, n_dims)
  df
}

#' Theme for individual contributions plot
ind_contrib_theme <- function() {
  ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        hjust = 0.5, size = 14, face = "bold"
      ),
      axis.title.x = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_blank(),
      axis.ticks.x = ggplot2$element_blank(),
      axis.title.y = ggplot2$element_text(size = 12),
      axis.text.y = ggplot2$element_text(size = 10),
      strip.text = ggplot2$element_text(
        size = 12, face = "bold"
      ),
      strip.background = ggplot2$element_rect(
        fill = "grey95", color = NA
      ),
      panel.spacing = ggplot2$unit(0.15, "cm"),
      legend.position = "right",
      legend.key.height = ggplot2$unit(1.2, "cm"),
      legend.key.width = ggplot2$unit(0.4, "cm"),
      legend.title = ggplot2$element_text(size = 11),
      legend.text = ggplot2$element_text(size = 10),
      panel.grid.minor = ggplot2$element_blank(),
      panel.grid.major.x = ggplot2$element_blank()
    )
}
