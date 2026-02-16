box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Variable contribution heatmap plot
# No Shiny dependencies allowed in this file.
# Legacy plots (bar chart, overview dot plot) are in var_contrib_legacy.R
# =============================================================================

#' Error parser for variable contribution plot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
var_contrib_error_parser <- function(error_msg,
                                     operation_name =
                                       "Variable Contribution Plot") {
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


#' Create a heatmap of variable contributions per dimension
#'
#' Variables on Y-axis (sorted by total contribution),
#' dimensions on X-axis, tile fill = contribution %.
#' Interactive tooltips show contribution, cos2, variable
#' and dimension info.
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param display_ncp Integer, number of dimensions to show
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_var_contrib_circles_plot <- function(pca_result,
                                            display_ncp = 5L,
                                            show_title = TRUE) {
  error_context <- list(display_ncp = display_ncp)

  error_handling$safe_execute(
    expr = {
      if (is.null(pca_result)) stop("pca_result is NULL")

      contrib <- pca_result$var$contrib
      cos2 <- pca_result$var$cos2
      n_dims <- min(display_ncp, ncol(contrib))
      n_vars <- nrow(contrib)
      dims <- colnames(contrib)[seq_len(n_dims)]
      eig <- pca_result$eig
      threshold <- expected_average(pca_result)

      # Build dimension labels with variance %
      dim_labels <- vapply(dims, function(d) {
        idx <- which(rownames(eig) == d)
        if (length(idx) == 1) {
          sprintf("%s (%.1f%%)", d, eig[idx, "variance.percent"])
        } else {
          d
        }
      }, character(1))

      # Build long-format data frame
      rows <- list()
      for (i in seq_along(dims)) {
        d <- dims[i]
        rows[[i]] <- data.frame(
          variable = rownames(contrib),
          dim = d,
          dim_label = dim_labels[i],
          contrib = contrib[, d],
          cos2 = cos2[, d],
          stringsAsFactors = FALSE
        )
      }
      df <- do.call(rbind, rows)

      # Sort variables by total contribution (descending)
      total_contrib <- rowSums(
        contrib[, dims, drop = FALSE]
      )
      var_order <- names(sort(total_contrib))
      df$variable <- factor(
        df$variable, levels = var_order
      )

      # Preserve dimension order (Dim.1 left to right)
      df$dim_label <- factor(
        df$dim_label, levels = dim_labels
      )

      # Tooltips
      df$tooltip <- sprintf(
        paste0(
          "<b>%s</b>",
          "<br/>%s",
          "<br/>Contribution: %.2f%%",
          "<br/>cos\u00b2: %.4f"
        ),
        df$variable,
        df$dim_label,
        df$contrib,
        df$cos2
      )
      df$data_id <- paste0(
        "vc_", df$variable, "_", df$dim
      )

      # Contribution value as label inside tiles
      df$label <- sprintf("%.1f", df$contrib)

      # Adaptive text size based on grid dimensions
      text_size <- if (n_vars <= 10 && n_dims <= 5) {
        6
      } else if (n_vars <= 20) {
        5
      } else if (n_vars <= 30) {
        4
      } else {
        4.0
      }

      p <- ggplot2$ggplot(
        df,
        ggplot2$aes(
          x = dim_label,
          y = variable
        )
      ) +
        ggiraph$geom_tile_interactive(
          ggplot2$aes(
            fill = contrib,
            tooltip = tooltip,
            data_id = data_id
          ),
          color = "white",
          linewidth = 0.5
        ) +
        ggplot2$geom_text(
          ggplot2$aes(label = label),
          size = text_size,
          color = "black"
        ) +
        ggplot2$scale_fill_distiller(
          palette = "YlOrRd",
          direction = 1,
          name = "Contribution (%)"
        ) +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          legend.position = "right",
          legend.key.height = ggplot2$unit(1.5, "cm"),
          legend.key.width = ggplot2$unit(0.4, "cm"),
          legend.text = ggplot2$element_text(size = 11),
          legend.title = ggplot2$element_text(size = 12),
          axis.title = ggplot2$element_blank(),
          axis.text.x = ggplot2$element_text(
            size = 11, angle = 45, hjust = 1
          ),
          axis.text.y = ggplot2$element_text(size = 11),
          panel.grid = ggplot2$element_blank()
        )

      p
    },
    operation_name = "Variable Contribution Heatmap",
    context = error_context,
    error_parser = var_contrib_error_parser
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Compute expected average contribution (100/p %)
expected_average <- function(pca_result) {
  p <- nrow(pca_result$var$contrib)
  100 / p
}
