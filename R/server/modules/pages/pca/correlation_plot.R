#' Correlation Plot Function for PCA Module
#'
#' Creates an interactive correlation heatmap using ggplot2 and ggiraph.
#' Mimics createPCAHeatmapCorrDendroPlot but with ggiraph interactivity.

#' Create an interactive correlation plot
#'
#' @param data Data frame containing the measurement columns
#' @param measurement_cols Character vector of column names to include in correlation
#' @return ggiraph interactive plot object
#' @export
create_correlation_plot <- function(data, measurement_cols) {
    # Validate inputs
    if (is.null(data) || nrow(data) == 0) {
        stop("Data is NULL or empty")
    }
    
    if (is.null(measurement_cols) || length(measurement_cols) < 2) {
        stop("At least 2 measurement columns are required")
    }
    
    # Subset to measurement columns only
    cor_data <- data[, measurement_cols, drop = FALSE]
    
    # Calculate correlation matrix (use pairwise complete observations for NA handling)
    cor_matrix <- stats::cor(cor_data, use = "pairwise.complete.obs")
    
    # Hierarchical clustering to reorder variables
    dist_matrix <- stats::as.dist(1 - cor_matrix)
    hc <- stats::hclust(dist_matrix, method = "complete")
    ordered_cols <- measurement_cols[hc$order]
    
    # Convert correlation matrix to long format for ggplot
    cor_df <- as.data.frame(cor_matrix)
    cor_df$Var1 <- rownames(cor_matrix)
    
    cor_long <- tidyr::pivot_longer(
        cor_df,
        cols = -Var1,
        names_to = "Var2",
        values_to = "correlation"
    )
    
    # Create factor levels using clustered order
    cor_long$Var1 <- factor(cor_long$Var1, levels = ordered_cols)
    cor_long$Var2 <- factor(cor_long$Var2, levels = ordered_cols)
    
    # Create tooltip text
    cor_long$tooltip <- sprintf(
        "<b>%s</b> vs <b>%s</b><br/>r = %.3f",
        cor_long$Var1,
        cor_long$Var2,
        cor_long$correlation
    )
    
    # Create data_id for ggiraph interactivity
    cor_long$data_id <- paste(cor_long$Var1, cor_long$Var2, sep = "_")
    
    # Build ggplot with ggiraph interactive elements
    p <- ggplot2::ggplot(cor_long, ggplot2::aes(x = Var1, y = Var2, fill = correlation)) +
        ggiraph::geom_tile_interactive(
            ggplot2::aes(tooltip = tooltip, data_id = data_id),
            color = "white",
            linewidth = 0.5
        ) +
        # Blue-White-Red color scale for correlations
        ggplot2::scale_fill_gradient2(
            low = "#2166AC",
            mid = "white",
            high = "#B2182B",
            midpoint = 0,
            limits = c(-1, 1),
            name = "Correlation"
        ) +
        # Add correlation coefficient text
        ggplot2::geom_text(
            ggplot2::aes(label = sprintf("%.2f", correlation)),
            color = ifelse(abs(cor_long$correlation) > 0.5, "white", "black"),
            size = 3
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            axis.title = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right"
        ) +
        ggplot2::coord_fixed()
    
    # Convert to ggiraph interactive plot
    ggiraph::girafe(
        ggobj = p,
        width_svg = 7,
        height_svg = 6,
        options = list(
            ggiraph::opts_hover(css = "fill-opacity:0.8;stroke:black;stroke-width:2px;"),
            ggiraph::opts_tooltip(
                css = "background-color:white;padding:8px;border-radius:4px;border:1px solid #ccc;font-family:sans-serif;",
                use_fill = FALSE
            ),
            ggiraph::opts_selection(type = "none")
        )
    )
}


#' Error parser for correlation plot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
correlation_error_parser <- function(error_msg, operation_name = "Correlation Plot") {
    if (grepl("NA|missing|NaN", error_msg, ignore.case = TRUE)) {
        "Correlation Plot: Data contains too many missing values. Please handle missing data first."
    } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
        "Correlation Plot: All selected columns must be numeric."
    } else if (grepl("singular|invertible", error_msg, ignore.case = TRUE)) {
        "Correlation Plot: Cannot compute correlations - data may contain constant columns."
    } else if (grepl("columns|measurement", error_msg, ignore.case = TRUE)) {
        "Correlation Plot: At least 2 measurement columns are required."
    } else {
        paste0("Correlation Plot failed: ", error_msg)
    }
}
