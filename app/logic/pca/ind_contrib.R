box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/error_handling,
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
#'   ind$meta for coloring. NULL for no grouping.
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

      contrib <- pca_result$ind$contrib
      cos2 <- pca_result$ind$cos2
      meta <- pca_result$ind$meta
      eig <- pca_result$eig
      n_obs <- nrow(contrib)
      n_dims <- min(display_ncp, ncol(contrib))
      dims <- colnames(contrib)[seq_len(n_dims)]
      threshold <- 100 / n_obs

      # Build dimension labels with variance %
      dim_labels <- vapply(dims, function(d) {
        idx <- which(rownames(eig) == d)
        if (length(idx) == 1) {
          sprintf(
            "%s (%.1f%%)",
            d, eig[idx, "variance.percent"]
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
          stringsAsFactors = FALSE
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

      base_aes <- if (has_group) {
        ggplot2$aes(
          x = dim_label,
          y = contrib,
          color = group,
          tooltip = tooltip,
          data_id = data_id
        )
      } else {
        ggplot2$aes(
          x = dim_label,
          y = contrib,
          tooltip = tooltip,
          data_id = data_id
        )
      }

      p <- ggplot2$ggplot(df, base_aes) +
        ggiraph$geom_jitter_interactive(
          width = 0.25,
          height = 0,
          size = 2,
          alpha = 0.6
        ) +
        ggplot2$geom_hline(
          yintercept = threshold,
          linetype = "dashed",
          color = "firebrick",
          linewidth = 0.5
        ) +
        ggplot2$annotate(
          "text",
          x = 0.5,
          y = threshold,
          label = sprintf(
            "Expected avg. (%.2f%%)", threshold
          ),
          hjust = 0,
          vjust = -0.5,
          size = 3.2,
          color = "firebrick",
          fontface = "italic"
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
        p <- p + ggplot2$labs(color = "Group")
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
      axis.title = ggplot2$element_text(size = 12),
      axis.text.x = ggplot2$element_text(
        size = 11, angle = 45, hjust = 1
      ),
      axis.text.y = ggplot2$element_text(size = 10),
      legend.position = "right",
      legend.title = ggplot2$element_text(size = 11),
      legend.text = ggplot2$element_text(size = 10),
      panel.grid.minor = ggplot2$element_blank()
    )
}
