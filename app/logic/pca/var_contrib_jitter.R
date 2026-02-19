box::use(
  ggplot2,
  ggiraph,
  ggrepel,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Variable contribution jitter/strip plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a jitter/strip plot of variable contributions per dimension
#'
#' Dimensions on X-axis, contribution % on Y-axis.
#' Points colored by cos2 (viridis), labeled with ggrepel.
#' Smart filtering: shows all variables when <= 10, otherwise
#' shows top contributors per dimension above a contribution
#' threshold and minimum cos2.
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param display_ncp Integer, number of dimensions to show
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_var_contrib_jitter_plot <- function(pca_result,
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
          contrib = as.numeric(contrib[, d]),
          cos2 = as.numeric(cos2[, d]),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
      df <- do.call(rbind, rows)

      # Preserve dimension order
      df$dim_label <- factor(
        df$dim_label, levels = dim_labels
      )

      # Dummy x position for strip layout
      df$x <- 0

      # Smart filtering: decide which points get labels
      df$show_label <- select_label_vars(
        df, n_vars, n_dims
      )

      # Tooltips (always on all points)
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
        "vcj_", df$variable, "_", df$dim
      )

      # Label data (subset for repel)
      label_df <- df[df$show_label, , drop = FALSE]

      # Adaptive point size
      point_size <- if (n_vars <= 15) 5 else 4

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
          position = ggplot2$position_jitter(
            width = 0.25, height = 0, seed = 42
          )
        ) +
        ggiraph$geom_text_repel_interactive(
          data = label_df,
          ggplot2$aes(
            label = variable,
            tooltip = tooltip,
            data_id = data_id
          ),
          size = 3.8,
          fontface = "bold",
          color = "grey20",
          max.overlaps = 20,
          segment.color = "grey50",
          segment.size = 0.4,
          min.segment.length = 0.2,
          box.padding = 0.5,
          point.padding = 0.3,
          direction = "y",
          hjust = 0,
          xlim = c(0.4, NA),
          position = ggplot2$position_jitter(
            width = 0.25, height = 0, seed = 42
          ),
          show.legend = FALSE
        ) +
        ggplot2$facet_wrap(
          ~ dim_label,
          nrow = 1,
          scales = "free_y"
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
        ggplot2$labs(x = NULL, y = NULL) +
        jitter_theme()

      if (show_title) {
        p <- p + ggplot2$ggtitle(
          "Variable Contributions by Dimension"
        )
      }

      p
    },
    operation_name = "Variable Contribution Jitter Plot",
    context = error_context,
    error_parser = var_contrib_jitter_error_parser
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
      panel.grid.major.y = ggplot2$element_line(
        color = "grey90", linewidth = 0.3
      )
    )
}
