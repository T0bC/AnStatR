#' Correlation Plot Function for PCA Module
#'
#' Creates an interactive correlation heatmap using ggplot2 and ggiraph.
#' Mimics createPCAHeatmapCorrDendroPlot but with ggiraph interactivity.
#'
#' Architecture: Separates computation from rendering to enable proper error handling.
#' - compute_correlation_data(): Validates data and computes correlation matrix (may fail)
#' - render_correlation_girafe(): Renders pre-computed data as ggiraph (should not fail)

#' Compute correlation data for plotting
#'
#' Validates inputs and computes the correlation matrix with hierarchical clustering.
#' Returns prepared data for rendering or throws an error for invalid inputs.
#'
#' @param data Data frame containing the measurement columns
#' @param measurement_cols Character vector of column names to include in correlation
#' @return List with cor_long (data frame for plotting) and ordered_cols (clustered order)
#' @keywords internal
compute_correlation_data <- function(data, measurement_cols) {
    # Validate inputs
    if (is.null(data) || nrow(data) == 0) {
        stop("Data is NULL or empty")
    }
    
    if (is.null(measurement_cols) || length(measurement_cols) < 2) {
        stop("At least 2 measurement columns are required")
    }
    
    # Check if all columns exist
    missing_cols <- setdiff(measurement_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
    }
    
    # Subset to measurement columns only
    cor_data <- data[, measurement_cols, drop = FALSE]
    
    # Check if all columns are numeric
    non_numeric_cols <- names(cor_data)[!sapply(cor_data, is.numeric)]
    if (length(non_numeric_cols) > 0) {
        stop(paste("All columns must be numeric. Non-numeric columns:", paste(non_numeric_cols, collapse = ", ")))
    }
    
    # Check for constant columns (zero variance) - handle NA in variance calculation
    constant_cols <- names(cor_data)[sapply(cor_data, function(x) {
        v <- var(x, na.rm = TRUE)
        is.na(v) || v == 0
    })]
    if (length(constant_cols) > 0) {
        stop(paste("Cannot compute correlations - constant or all-NA columns found:", paste(constant_cols, collapse = ", ")))
    }
    
    # Check if we have enough complete observations
    complete_rows <- complete.cases(cor_data)
    if (sum(complete_rows) < 2) {
        stop("Not enough complete observations (need at least 2) to compute correlations")
    }
    
    # Calculate correlation matrix (use pairwise complete observations for NA handling)
    cor_matrix <- stats::cor(cor_data, use = "pairwise.complete.obs")
    
    # Check if correlation matrix contains valid values
    if (any(is.na(cor_matrix))) {
        # Try with complete observations only as fallback
        if (sum(complete_rows) >= 2) {
            cor_matrix <- stats::cor(cor_data[complete_rows, ], use = "everything")
            if (any(is.na(cor_matrix))) {
                stop("Unable to compute correlations due to missing values")
            }
        } else {
            stop("Unable to compute correlations - insufficient complete data")
        }
    }
    
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
    
    # Create tooltip text - ensure all values are valid strings
    cor_long$tooltip <- sprintf(
        "<b>%s</b> vs <b>%s</b><br/>r = %.3f",
        as.character(cor_long$Var1),
        as.character(cor_long$Var2),
        cor_long$correlation
    )
    
    # Create data_id for ggiraph interactivity
    cor_long$data_id <- paste(as.character(cor_long$Var1), as.character(cor_long$Var2), sep = "_")
    
    list(
        cor_long = cor_long,
        ordered_cols = ordered_cols
    )
}


#' Render correlation plot as ggiraph
#'
#' Takes pre-computed correlation data and renders it as an interactive ggiraph.
#' This function should not fail if compute_correlation_data() succeeded.
#'
#' @param cor_data List returned by compute_correlation_data()
#' @return ggiraph interactive plot object
#' @keywords internal
render_correlation_girafe <- function(cor_data) {
    cor_long <- cor_data$cor_long
    
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
        # Add correlation coefficient text with pre-computed colors
        ggplot2::geom_text(
            ggplot2::aes(label = sprintf("%.2f", correlation)),
            color = ifelse(abs(cor_long$correlation) > 0.5, "white", "black"),
            size = 4
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            legend.position = "right",
            legend.title = ggplot2::element_text(size = 12),
            legend.text = ggplot2::element_text(size = 10)
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


#' Create an interactive correlation plot
#'
#' Main entry point that combines computation and rendering.
#' For error handling, prefer using compute_correlation_data() with safe_execute()
#' followed by render_correlation_girafe().
#'
#' @param data Data frame containing the measurement columns
#' @param measurement_cols Character vector of column names to include in correlation
#' @return ggiraph interactive plot object
#' @export
create_correlation_plot <- function(data, measurement_cols) {
    cor_data <- compute_correlation_data(data, measurement_cols)
    render_correlation_girafe(cor_data)
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
