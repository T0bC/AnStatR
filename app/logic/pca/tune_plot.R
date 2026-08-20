box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Component-stability curve for sPCA keepX tuning (mixOmics::tune.spca).
# The sPCA counterpart of the PLS-DA error curve in app/logic/lda/perf_plot.R:
# it shows the evidence behind the chosen keepX rather than asserting it.
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Reshape tune.spca()'s correlation output into a long data frame
#'
#' mixOmics returns $cor.comp as a list with one element per
#' component, each a data frame of correlations over the tested
#' keepX grid. The exact column naming has varied between mixOmics
#' releases, so pick the numeric column defensively rather than by
#' a fixed name.
#'
#' @param cor_comp The $cor.comp element from run_pca_tune_keepx()
#' @param grid Integer vector of tested keepX values
#' @return Data frame with Component, keepX, Correlation — or NULL
#'   when the structure is not recognised
#' @export
tidy_cor_comp <- function(cor_comp, grid) {
  if (is.null(cor_comp) || length(cor_comp) == 0) return(NULL)

  rows <- lapply(seq_along(cor_comp), function(i) {
    block <- cor_comp[[i]]
    if (is.null(block)) return(NULL)

    values <- if (is.data.frame(block)) {
      # mixOmics 6.36 names this "cor.mean". Prefer an explicitly
      # named correlation column, then fall back to the first
      # numeric column that is not the keepX grid itself —
      # picking keepX would silently plot the x-axis as the y.
      named <- grep("^cor", names(block), ignore.case = TRUE)
      numeric_cols <- setdiff(
        which(vapply(block, is.numeric, logical(1))),
        which(names(block) == "keepX")
      )
      pick <- if (length(named) > 0) named[1] else {
        if (length(numeric_cols) == 0) return(NULL)
        numeric_cols[1]
      }
      block[[pick]]
    } else if (is.numeric(block)) {
      block
    } else {
      return(NULL)
    }

    values <- as.numeric(values)
    if (length(values) == 0) return(NULL)

    # Align to the grid: tune.spca evaluates one value per tested
    # keepX, but bail out rather than recycling if they disagree.
    keep_vals <- if (length(grid) == length(values)) {
      grid
    } else if (is.data.frame(block) && "keepX" %in% names(block)) {
      as.numeric(block[["keepX"]])
    } else {
      return(NULL)
    }

    data.frame(
      Component = paste0("Dim.", i),
      keepX = as.numeric(keep_vals),
      Correlation = values,
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, rows)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  rownames(df) <- NULL
  df
}


#' Build the keepX component-stability curve for sPCA
#'
#' Plots, per component, the correlation between the
#' cross-validated component and the full-data component across the
#' tested keepX grid. A high, flat curve means the selection is
#' stable; a curve that only climbs at the largest keepX means
#' sparsity is costing signal.
#'
#' @param cor_comp The $cor.comp element from run_pca_tune_keepx()
#' @param grid Integer vector of tested keepX values
#' @param chosen Named integer vector of chosen keepX per component
#'   (the $keep_x element), used to mark the selection
#' @return List with $success, $result (girafe object) or $error
#' @export
create_tune_spca_plot <- function(cor_comp, grid, chosen = NULL) {
  error_handling$safe_execute(
    {
      plot_df <- tidy_cor_comp(cor_comp, grid)
      if (is.null(plot_df)) {
        stop(
          "Tuning returned no component-stability values to plot."
        )
      }

      p <- ggplot2$ggplot(
        plot_df,
        ggplot2$aes(
          x = keepX,
          y = Correlation,
          colour = Component
        )
      )

      # Chosen values first so they sit behind the data.
      if (!is.null(chosen) && length(chosen) > 0) {
        marks <- data.frame(
          Component = paste0("Dim.", seq_along(chosen)),
          chosen_keepx = as.numeric(chosen),
          stringsAsFactors = FALSE
        )
        p <- p +
          ggplot2$geom_vline(
            data = marks,
            ggplot2$aes(
              xintercept = chosen_keepx, colour = Component
            ),
            linetype = "dashed",
            linewidth = 0.6,
            show.legend = FALSE
          )
      }

      p <- p +
        ggplot2$geom_line(
          ggplot2$aes(group = Component), linewidth = 0.7
        ) +
        ggiraph$geom_point_interactive(
          ggplot2$aes(
            tooltip = sprintf(
              "%s\nkeepX = %g\nStability: %.3f",
              Component, keepX, Correlation
            ),
            data_id = paste0(Component, "_", keepX)
          ),
          size = 2.5
        ) +
        ggplot2$scale_x_continuous(breaks = unique(plot_df$keepX)) +
        ggplot2$labs(
          title = "Component stability across keepX values",
          subtitle = paste(
            "Correlation between the cross-validated and",
            "full-data component (1.0 = identical). Dashed lines",
            "mark the selected keepX. A curve that plateaus early",
            "means fewer variables cost you nothing."
          ),
          x = "Variables kept per component (keepX)",
          y = "Component stability (correlation)",
          colour = NULL
        ) +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          plot.title = ggplot2$element_text(
            size = 12, face = "bold"
          ),
          plot.subtitle = ggplot2$element_text(
            size = 9, colour = "#6c757d"
          ),
          legend.position = "bottom",
          panel.grid.minor = ggplot2$element_blank()
        )

      rhino$log$info(
        "tune_plot: built keepX stability curve for ",
        "{length(unique(plot_df$Component))} component(s)"
      )

      ggiraph$girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
        options = list(
          ggiraph$opts_sizing(rescale = TRUE, width = 1),
          ggiraph$opts_hover(css = "stroke-width:3px;"),
          ggiraph$opts_tooltip(
            css = paste(
              "background-color:#212529;",
              "color:white;padding:8px;",
              "border-radius:4px;font-size:12px;"
            ),
            opacity = 0.9
          ),
          ggiraph$opts_selection(type = "none")
        )
      )
    },
    operation_name = "keepX Stability Curve"
  )
}
