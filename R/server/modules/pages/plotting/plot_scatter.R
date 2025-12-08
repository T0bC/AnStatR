#' Create an interactive scatter plot for a single measurement variable
#'
#' Uses ggiraph for interactivity with hover tooltips.
#' Supports data trimming visualization where trimmed points are shown
#' with gray outline and no fill.
#'
#' @param data Data frame containing the data to plot
#' @param x_col Character vector of column name(s) for X-axis (will be combined if multiple)
#' @param y_col Character string of column name for Y-axis (measurement)
#' @param tooltip_cols Character vector of additional column names to show in tooltip (optional)
#' @param point_alpha Numeric, transparency of points (0-1)
#' @param point_size Numeric, size of points
#' @param trim_percent Numeric, percentage (0-100) to trim from each end per group (default 0)
#' @return A ggplot2 object with ggiraph interactive layer
#' @export
create_scatter_plot <- function(data, 
                                 x_col, 
                                 y_col,
                                 tooltip_cols = NULL,
                                 point_alpha = 0.6,
                                 point_size = 2,
                                 trim_percent = 0) {
    
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
    
    # Create interaction term for grouping (used for trimming)
    # Source the utility if not already available
    if (!exists("create_interaction", mode = "function")) {
        source("R/utils/data_utils.R", local = TRUE)
    }
    interaction_term <- create_interaction(data, x_col)
    
    # Mark trimmed data points
    if (!exists("mark_trimmed_data", mode = "function")) {
        source("R/utils/data_utils.R", local = TRUE)
    }
    data <- mark_trimmed_data(
        data = data,
        value_col = y_col,
        group_col = interaction_term,
        trim_percent = trim_percent
    )
    
    # Build tooltip text
    data$.tooltip <- build_tooltip_text(
        data = data,
        x_var = x_var,
        x_label = x_label,
        y_col = y_col,
        tooltip_cols = tooltip_cols
    )
    
    # Build the plot with ggiraph interactive points
    # Use two layers: trimmed points (gray outline, no fill) and retained points (colored)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_col]]))
    
    # Pre-compute indices to avoid ggplot2 warnings about data$ usage
    is_trimmed <- data[[".is_trimmed"]]
    trimmed_idx <- which(is_trimmed)
    retained_idx <- which(!is_trimmed)
    
    # Layer 1: Trimmed points (shown with gray outline, no fill)
    if (length(trimmed_idx) > 0) {
        trimmed_data <- data[trimmed_idx, , drop = FALSE]
        trimmed_data$.data_id <- trimmed_idx
        p <- p + ggiraph::geom_point_interactive(
            data = trimmed_data,
            ggplot2::aes(
                tooltip = .data[[".tooltip"]],
                data_id = .data[[".data_id"]]
            ),
            shape = 21,  # Circle with outline
            fill = NA,   # No fill (transparent)
            color = "gray40",
            alpha = point_alpha * 0.7,
            size = point_size,
            stroke = 0.8
        )
    }
    
    # Layer 2: Retained points (colored, filled)
    if (length(retained_idx) > 0) {
        retained_data <- data[retained_idx, , drop = FALSE]
        retained_data$.data_id <- retained_idx
        p <- p + ggiraph::geom_point_interactive(
            data = retained_data,
            ggplot2::aes(
                tooltip = .data[[".tooltip"]],
                data_id = .data[[".data_id"]]
            ),
            alpha = point_alpha,
            size = point_size,
            color = "#0d6efd"
        )
    }
    
    p <- p +
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
