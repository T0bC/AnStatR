#' Create an interactive scatter plot for a single measurement variable
#'
#' Uses ggiraph for interactivity with hover tooltips.
#'
#' @param data Data frame containing the data to plot
#' @param x_col Character vector of column name(s) for X-axis (will be combined if multiple)
#' @param y_col Character string of column name for Y-axis (measurement)
#' @param tooltip_cols Character vector of additional column names to show in tooltip (optional)
#' @param point_alpha Numeric, transparency of points (0-1)
#' @param point_size Numeric, size of points
#' @return A ggplot2 object with ggiraph interactive layer
#' @export
create_scatter_plot <- function(data, 
                                 x_col, 
                                 y_col,
                                 tooltip_cols = NULL,
                                 point_alpha = 0.6,
                                 point_size = 2) {
    
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
        data$.x_combined <- apply(data[, x_col, drop = FALSE], 1, paste, collapse = " | ")
        x_var <- ".x_combined"
        x_label <- paste(x_col, collapse = " | ")
    } else {
        x_var <- x_col
        x_label <- x_col
    }
    
    # Build tooltip text
    data$.tooltip <- build_tooltip_text(
        data = data,
        x_var = x_var,
        x_label = x_label,
        y_col = y_col,
        tooltip_cols = tooltip_cols
    )
    
    # Build the plot with ggiraph interactive points
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_col]])) +
        ggiraph::geom_point_interactive(
            ggplot2::aes(
                tooltip = .data$.tooltip,
                data_id = seq_len(nrow(data))
            ),
            alpha = point_alpha,
            size = point_size,
            color = "#0d6efd"
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


#' Build tooltip text for each data point
#'
#' @param data Data frame
#' @param x_var Name of x variable in data
#' @param x_label Display label for x axis
#' @param y_col Name of y column
#' @param tooltip_cols Additional columns to include
#' @return Character vector of tooltip HTML strings
build_tooltip_text <- function(data, x_var, x_label, y_col, tooltip_cols = NULL) {
    
    # Start with x and y values
    tooltip_parts <- paste0(
        "<strong>", x_label, ":</strong> ", data[[x_var]], "<br/>",
        "<strong>", y_col, ":</strong> ", round(data[[y_col]], 4)
    )
    
    # Add optional tooltip columns
    if (!is.null(tooltip_cols) && length(tooltip_cols) > 0) {
        # Filter to columns that exist in data
        valid_cols <- tooltip_cols[tooltip_cols %in% names(data)]
        
        if (length(valid_cols) > 0) {
            extra_info <- sapply(seq_len(nrow(data)), function(i) {
                parts <- sapply(valid_cols, function(col) {
                    paste0("<strong>", col, ":</strong> ", data[[col]][i])
                })
                paste(parts, collapse = "<br/>")
            })
            tooltip_parts <- paste0(tooltip_parts, "<br/>", extra_info)
        }
    }
    
    return(tooltip_parts)
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
