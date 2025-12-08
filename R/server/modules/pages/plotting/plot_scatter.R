#' Create a basic scatter plot for a single measurement variable
#'
#' @param data Data frame containing the data to plot
#' @param x_col Character vector of column name(s) for X-axis (will be combined if multiple)
#' @param y_col Character string of column name for Y-axis (measurement)
#' @param point_alpha Numeric, transparency of points (0-1)
#' @param point_size Numeric, size of points
#' @return A ggplot2 object
#' @export
create_scatter_plot <- function(data, 
                                 x_col, 
                                 y_col,
                                 point_alpha = 0.6,
                                 point_size = 1.5) {
    
    # Validate inputs
    if (is.null(data) || nrow(data) == 0) {
        return(create_empty_plot("No data available"))
    }
    
    if (is.null(x_col) || length(x_col) == 0) {
        return(create_empty_plot("No X-axis column selected"))
    }
    
    if (is.null(y_col) || !y_col %in% names(data)) {
        return(create_empty_plot(paste("Column", y_col, "not found")))
    }
    
    # Create combined X-axis if multiple columns selected
    if (length(x_col) > 1) {
        # Combine multiple x columns with separator
        data$.x_combined <- apply(data[, x_col, drop = FALSE], 1, paste, collapse = " | ")
        x_var <- ".x_combined"
        x_label <- paste(x_col, collapse = " | ")
    } else {
        x_var <- x_col
        x_label <- x_col
    }
    
    # Build the plot
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_col]])) +
        ggplot2::geom_point(
            alpha = point_alpha,
            size = point_size,
            color = "#0d6efd"  # Bootstrap primary color
        ) +
        ggplot2::labs(
            x = x_label,
            y = y_col
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
            panel.grid.minor = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(10, 10, 10, 10)
        )
    
    return(p)
}


#' Create an empty placeholder plot with a message
#'
#' @param message Character string to display
#' @return A ggplot2 object
create_empty_plot <- function(message = "No data to display") {
    ggplot2::ggplot() +
        ggplot2::annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = message,
            size = 4,
            color = "gray50"
        ) +
        ggplot2::theme_void() +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)
}
