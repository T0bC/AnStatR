box::use(
  ggplot2,
  ggiraph,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for eigencorrelation plot
# (PC dimension scores vs metadata variables)
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute correlations between PC scores and metadata variables
#'
#' Extracts individual scores from the PCA result and correlates
#' them against the metadata columns. Non-numeric metadata is
#' coerced to numeric via as.numeric(as.factor(...)). Returns
#' correlation matrix, p-value matrix, and list of coerced columns.
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param display_ncp Integer, number of dimensions to include
#' @return List with $success, $result or $error.
#'   $result contains $cor_matrix, $pval_matrix, $coerced_cols,
#'   $dim_labels (named character vector)
#' @export
compute_eigencor_data <- function(pca_result, display_ncp = 5L) {
  error_context <- list(display_ncp = display_ncp)

  error_handling$safe_execute(
    expr = {
      if (is.null(pca_result)) stop("pca_result is NULL")

      meta <- pca_result$ind$meta
      validate_metadata(meta)

      scores <- pca_result$ind$coord
      n_dims <- min(display_ncp, ncol(scores))
      dims <- colnames(scores)[seq_len(n_dims)]
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

      xvals <- scores[, dims, drop = FALSE]

      # Prepare metadata: coerce non-numeric columns
      coerced_cols <- character(0)
      yvals <- meta
      for (col in names(yvals)) {
        if (!is.numeric(yvals[[col]])) {
          yvals[[col]] <- as.numeric(as.factor(yvals[[col]]))
          coerced_cols <- c(coerced_cols, col)
        }
      }
      yvals <- data.matrix(yvals)

      # Compute correlation matrix
      cor_matrix <- stats$cor(
        xvals, yvals,
        use = "pairwise.complete.obs"
      )

      # Compute p-value matrix via cor.test per pair
      pval_matrix <- matrix(
        NA_real_,
        nrow = ncol(xvals), ncol = ncol(yvals),
        dimnames = list(colnames(xvals), colnames(yvals))
      )
      for (i in seq_len(ncol(xvals))) {
        for (j in seq_len(ncol(yvals))) {
          test_res <- tryCatch(
            stats$cor.test(
              xvals[, i], yvals[, j],
              use = "pairwise.complete.obs"
            ),
            error = function(e) NULL
          )
          pval_matrix[i, j] <- if (!is.null(test_res)) {
            test_res$p.value
          } else {
            NA_real_
          }
        }
      }

      rhino$log$info(
        "Eigencorplot: {n_dims} dims x",
        " {ncol(yvals)} metadata vars",
        " ({length(coerced_cols)} coerced)"
      )

      list(
        cor_matrix = cor_matrix,
        pval_matrix = pval_matrix,
        coerced_cols = coerced_cols,
        dim_labels = dim_labels
      )
    },
    operation_name = "Eigencorrelation",
    context = error_context,
    error_parser = eigencor_error_parser
  )
}


#' Create the eigencorrelation heatmap plot
#'
#' Builds an interactive ggplot2 heatmap from pre-computed
#' correlation and p-value matrices. Cells show r-value with
#' significance stars. Diverging blue-white-red colour scale.
#' Adaptive text sizing based on grid dimensions.
#'
#' @param eigencor_data List from compute_eigencor_data()$result
#' @return List with $success, $result (ggplot) or $error
#' @export
create_eigencor_plot <- function(eigencor_data) {
  error_handling$safe_execute(
    expr = {
      cor_mat <- eigencor_data$cor_matrix
      pval_mat <- eigencor_data$pval_matrix
      dim_labels <- eigencor_data$dim_labels

      n_dims <- nrow(cor_mat)
      n_meta <- ncol(cor_mat)

      # Build long-format data frame
      rows <- list()
      k <- 0
      for (i in seq_len(n_dims)) {
        for (j in seq_len(n_meta)) {
          k <- k + 1
          dim_name <- rownames(cor_mat)[i]
          meta_name <- colnames(cor_mat)[j]
          r_val <- cor_mat[i, j]
          p_val <- pval_mat[i, j]
          stars <- significance_stars(p_val)

          rows[[k]] <- data.frame(
            dim = dim_name,
            dim_label = dim_labels[dim_name],
            meta = meta_name,
            r = r_val,
            pval = p_val,
            stars = stars,
            stringsAsFactors = FALSE
          )
        }
      }
      df <- do.call(rbind, rows)

      # Factor ordering: dimensions top-to-bottom (Dim.1 at top),
      # metadata left-to-right
      df$dim_label <- factor(
        df$dim_label,
        levels = rev(dim_labels)
      )
      df$meta <- factor(
        df$meta,
        levels = colnames(cor_mat)
      )

      # Cell label: "0.72***"
      df$label <- sprintf("%.2f%s", df$r, df$stars)

      # Tooltip
      df$tooltip <- sprintf(
        paste0(
          "<b>%s</b> vs <b>%s</b>",
          "<br/>r = %.3f",
          "<br/>p = %s",
          "<br/>%s"
        ),
        df$dim_label,
        df$meta,
        df$r,
        format_pval(df$pval),
        ifelse(
          nchar(df$stars) > 0,
          paste0("Significance: ", df$stars),
          "Not significant"
        )
      )
      df$data_id <- paste0("ec_", df$dim, "_", df$meta)

      # Adaptive text size
      total_cells <- n_dims * n_meta
      text_size <- if (total_cells <= 20) {
        4.5
      } else if (total_cells <= 50) {
        3.8
      } else if (total_cells <= 100) {
        3.2
      } else {
        2.8
      }

      # Symmetric colour limits
      max_abs <- max(abs(cor_mat), na.rm = TRUE)
      colour_limit <- min(max_abs + 0.05, 1)

      p <- ggplot2$ggplot(
        df,
        ggplot2$aes(x = meta, y = dim_label)
      ) +
        ggiraph$geom_tile_interactive(
          ggplot2$aes(
            fill = r,
            tooltip = tooltip,
            data_id = data_id
          ),
          color = "white",
          linewidth = 0.5
        ) +
        ggplot2$geom_text(
          ggplot2$aes(label = label),
          size = text_size,
          color = ifelse(
            abs(df$r) > 0.6, "white", "black"
          )
        ) +
        ggplot2$scale_fill_gradient2(
          low = "#2166AC",
          mid = "white",
          high = "#B2182B",
          midpoint = 0,
          limits = c(-colour_limit, colour_limit),
          name = "Correlation (r)"
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
    operation_name = "Eigencorrelation Plot",
    error_parser = eigencor_error_parser
  )
}


#' Error parser for eigencorrelation errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
eigencor_error_parser <- function(error_msg,
                                  operation_name =
                                    "Eigencorrelation") {
  if (grepl(
    "metadata|meta.*col|no valid",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": No valid metadata columns available.",
      " Please select descriptive columns",
      " in the Data Selection tab."
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
  } else if (grepl(
    "numeric|coerce|convert",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Could not process metadata columns.",
      " Ensure metadata contains valid values."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Validate that metadata is usable for eigencorrelation
#'
#' Checks that metadata exists, is not just a "Row" fallback,
#' and has at least one column.
#'
#' @param meta Data frame from pca_result$ind$meta
validate_metadata <- function(meta) {
  if (is.null(meta)) {
    stop("No metadata available for eigencorrelation")
  }
  if ("Row" %in% names(meta) && ncol(meta) == 1) {
    stop("No valid metadata columns selected")
  }
  if (ncol(meta) == 0) {
    stop("No metadata columns available")
  }
  invisible(TRUE)
}

#' Convert p-value to significance stars
#'
#' @param p Numeric, p-value
#' @return Character, significance stars
significance_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  ""
}

#' Format p-value for display
#'
#' @param p Numeric vector of p-values
#' @return Character vector of formatted p-values
format_pval <- function(p) {
  vapply(p, function(pv) {
    if (is.na(pv)) return("NA")
    if (pv < 0.001) return("< 0.001")
    sprintf("%.3f", pv)
  }, character(1))
}
