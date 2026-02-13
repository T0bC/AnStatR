box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for variable contribution bar chart
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a variable contribution bar chart
#'
#' Horizontal bar chart showing each variable's contribution (%)
#' to a given dimension. Bars are colored by whether the
#' contribution exceeds the expected average (100/p %).
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param dim Character, dimension to display (e.g. "Dim.1")
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_var_contrib_plot <- function(pca_result,
                                    dim = "Dim.1",
                                    show_title = TRUE) {
  error_context <- list(dim = dim)

  error_handling$safe_execute(
    expr = {
      validate_var_contrib_inputs(pca_result, dim)

      plot_data <- build_var_contrib_data(pca_result, dim)
      threshold <- expected_average(pca_result)

      p <- ggplot2$ggplot(
        plot_data,
        ggplot2$aes(
          x = contrib,
          y = stats::reorder(variable, contrib)
        )
      ) +
        ggiraph$geom_col_interactive(
          ggplot2$aes(
            fill = above_avg,
            tooltip = tooltip,
            data_id = data_id
          ),
          width = 0.7
        ) +
        ggplot2$geom_vline(
          xintercept = threshold,
          linetype = "dashed",
          color = "grey40",
          linewidth = 0.5
        ) +
        ggplot2$scale_fill_manual(
          values = c(
            "Above average" = "#2166AC",
            "Below average" = "#D1E5F0"
          ),
          name = NULL
        ) +
        var_contrib_theme() +
        ggplot2$labs(
          x = "Contribution (%)",
          y = NULL
        )

      if (show_title) {
        eig <- pca_result$eig
        var_label <- axis_label_with_variance(dim, eig)
        p <- p + ggplot2$ggtitle(
          paste("Variable Contributions to", var_label)
        )
      }

      # Annotate the threshold line
      p <- p + ggplot2$annotate(
        "text",
        x = threshold,
        y = 0.5,
        label = sprintf("Expected avg = %.1f%%", threshold),
        hjust = -0.05,
        vjust = -0.5,
        size = 3,
        color = "grey40"
      )

      p
    },
    operation_name = "Variable Contribution Plot",
    context = error_context,
    error_parser = var_contrib_error_parser
  )
}

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


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Validate inputs for variable contribution plot
validate_var_contrib_inputs <- function(pca_result, dim) {
  if (is.null(pca_result)) {
    stop("pca_result is NULL")
  }

  available_dims <- colnames(pca_result$var$contrib)
  if (!dim %in% available_dims) {
    stop(paste("Dimension not found:", dim))
  }
}

#' Build plot data for variable contribution chart
build_var_contrib_data <- function(pca_result, dim) {
  contrib <- pca_result$var$contrib
  vals <- contrib[, dim]
  threshold <- expected_average(pca_result)

  df <- data.frame(
    variable = rownames(contrib),
    contrib = vals,
    above_avg = ifelse(
      vals >= threshold,
      "Above average",
      "Below average"
    ),
    stringsAsFactors = FALSE
  )

  df$tooltip <- sprintf(
    "<b>%s</b><br/>Contribution to %s: %.2f%%<br/>%s",
    df$variable, dim, df$contrib,
    ifelse(
      df$above_avg == "Above average",
      "Above expected average",
      "Below expected average"
    )
  )
  df$data_id <- paste0("vc_", seq_len(nrow(df)))

  df
}

#' Compute expected average contribution (100/p %)
expected_average <- function(pca_result) {
  p <- nrow(pca_result$var$contrib)
  100 / p
}

#' Theme for variable contribution chart
var_contrib_theme <- function() {
  ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        hjust = 0.5, size = 14, face = "bold"
      ),
      axis.title = ggplot2$element_text(size = 12),
      axis.text = ggplot2$element_text(size = 10),
      legend.position = "bottom",
      legend.text = ggplot2$element_text(size = 10),
      panel.grid.major.y = ggplot2$element_blank(),
      panel.grid.minor = ggplot2$element_blank()
    )
}

#' Build axis label with variance percentage
axis_label_with_variance <- function(dim_name, eig) {
  dim_idx <- which(rownames(eig) == dim_name)
  if (length(dim_idx) == 1) {
    var_pct <- eig[dim_idx, "variance.percent"]
    sprintf("%s (%.1f%%)", dim_name, var_pct)
  } else {
    dim_name
  }
}
