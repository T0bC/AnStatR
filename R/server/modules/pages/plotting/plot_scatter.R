#' Create an interactive scatter plot for a single measurement variable
#'
#' Uses ggiraph for interactivity with hover tooltips.
#' Supports data trimming and outlier detection visualization where
#' excluded points are shown with gray outline and no fill.
#'
#' @param data Data frame containing the data to plot
#' @param x_col Character vector of column name(s) for X-axis (will be combined if multiple)
#' @param y_col Character string of column name for Y-axis (measurement)
#' @param tooltip_cols Character vector of additional column names to show in tooltip (optional)
#' @param point_style List with point styling: size, spread (jitter), alpha, shape_col
#' @param trim_percent Numeric, percentage (0-100) to trim from each end per group (default 0)
#' @param outlier_detection Logical, whether to detect outliers (default FALSE)
#' @param outlier_method Character, outlier detection method (default "IQR")
#' @param outlier_factor Numeric, threshold factor for outlier detection
#' @param bootstrap_samples Integer, number of bootstrap samples (for bootstrap method)
#' @param color_cols Character vector of column name(s) for color grouping (interaction-based)
#' @param color_map Named character vector mapping group names to hex colors
#' @param grid_legend List with grid/legend options: legend_position, h_grid, v_grid, 
#'   top_right_borders, show_median, show_sd, aspect_ratio
#' @param stat_line_style List with stat line styling: median_thickness, median_width,
#'   sd_thickness, sd_width
#' @param axis_style List with axis styling: tick_length, line_thickness
#' @return A ggplot2 object with ggiraph interactive layer
#' @export
create_scatter_plot <- function(data, 
                                 x_col, 
                                 y_col,
                                 tooltip_cols = NULL,
                                 point_style = list(size = 4, spread = 0.15, alpha = 0.6, shape_col = NULL),
                                 trim_percent = 0,
                                 outlier_detection = FALSE,
                                 outlier_method = "IQR",
                                 outlier_factor = 1.5,
                                 bootstrap_samples = 1000,
                                 color_cols = NULL,
                                 color_map = NULL,
                                 grid_legend = list(legend_position = "none", h_grid = TRUE, v_grid = TRUE,
                                                    top_right_borders = TRUE, show_median = TRUE, 
                                                    show_sd = TRUE, aspect_ratio = FALSE),
                                 stat_line_style = list(median_thickness = 0.5, median_width = 0.15,
                                                        sd_thickness = 0.5, sd_width = 0.15),
                                 axis_style = list(tick_length = 0.15, line_thickness = 0.5)) {
    
    # Extract point style with defaults
    point_size <- point_style$size %||% 4
    point_spread <- point_style$spread %||% 0.15
    point_alpha <- point_style$alpha %||% 0.6
    shape_col <- point_style$shape_col
    
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
    
    # Source utility functions if not already available
    if (!exists("create_interaction", mode = "function")) {
        source("R/utils/data_utils.R", local = TRUE)
    }
    
    # Create interaction term for grouping (used for x-axis, outlier detection, trimming)
    # create_interaction() uses "." separator which matches guide_axis_nested default
    interaction_term <- create_interaction(data, x_col)
    
    # Set up x-axis variable
    if (length(x_col) > 1) {
        # Multiple columns: use interaction term for nested axis
        data$.x_nested <- interaction_term
        x_var <- ".x_nested"
        x_label <- paste(x_col, collapse = " | ")
    } else {
        x_var <- x_col
        x_label <- x_col
    }
    
    # Create color interaction term (may differ from x-axis grouping)
    # Default to x_col if no color_cols specified
    if (is.null(color_cols) || length(color_cols) == 0) {
        color_cols <- x_col
    }
    color_interaction <- create_interaction(data, color_cols)
    data$.color_group <- as.character(color_interaction)
    
    # STEP 1: Detect outliers first (these are excluded from statistics)
    if (outlier_detection) {
        if (!exists("detect_outliers", mode = "function")) {
            source("R/utils/data_utils.R", local = TRUE)
        }
        data <- detect_outliers(
            data = data,
            value_col = y_col,
            group_col = interaction_term,
            method = outlier_method,
            factor = outlier_factor,
            bootstrap_samples = bootstrap_samples
        )
    } else {
        data$.is_outlier <- FALSE
    }
    
    # STEP 2: Mark trimmed data points (only on non-outlier data)
    # This matches WRS2 behavior: outliers removed first, then trimming applied
    if (!exists("mark_trimmed_data", mode = "function")) {
        source("R/utils/data_utils.R", local = TRUE)
    }
    
    # Initialize .is_trimmed for all rows
    data$.is_trimmed <- FALSE
    
    # Only apply trimming to non-outlier data
    if (trim_percent > 0) {
        non_outlier_idx <- which(!data$.is_outlier)
        if (length(non_outlier_idx) > 0) {
            # Create subset for trimming calculation
            non_outlier_data <- data[non_outlier_idx, , drop = FALSE]
            non_outlier_interaction <- interaction_term[non_outlier_idx]
            
            # Mark trimmed points within non-outlier subset
            non_outlier_data <- mark_trimmed_data(
                data = non_outlier_data,
                value_col = y_col,
                group_col = non_outlier_interaction,
                trim_percent = trim_percent
            )
            
            # Copy trimmed status back to main data
            data$.is_trimmed[non_outlier_idx] <- non_outlier_data$.is_trimmed
        }
    }
    
    # Build tooltip text (include trimmed/outlier status)
    data$.tooltip <- build_tooltip_text(
        data = data,
        x_var = x_var,
        x_label = x_label,
        y_col = y_col,
        tooltip_cols = tooltip_cols,
        is_trimmed = data$.is_trimmed,
        is_outlier = data$.is_outlier
    )
    
    # Build the plot with ggiraph interactive points
    # Three layers: retained (colored), trimmed (gray/white), outliers (gray with X)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_col]]))
    
    # Pre-compute indices for each point category
    is_trimmed <- data[[".is_trimmed"]]
    is_outlier <- data[[".is_outlier"]]
    
    # Indices for each layer
    retained_idx <- which(!is_trimmed & !is_outlier)
    trimmed_idx <- which(is_trimmed & !is_outlier)
    outlier_idx <- which(is_outlier)
    
    # Color legend title
    color_legend_title <- if (!is.null(color_cols) && length(color_cols) > 0) {
        paste(color_cols, collapse = " | ")
    } else {
        paste(x_col, collapse = " | ")
    }
    
    # Layer 1: Retained points (colored by group) - using geom_jitter_interactive
    if (length(retained_idx) > 0) {
        retained_data <- data[retained_idx, , drop = FALSE]
        retained_data$.data_id <- retained_idx
        
        p <- p + ggiraph::geom_jitter_interactive(
            data = retained_data,
            ggplot2::aes(
                tooltip = .data[[".tooltip"]],
                data_id = .data[[".data_id"]],
                color = .data[[".color_group"]],
                fill = .data[[".color_group"]]
            ),
            hover_nearest = TRUE,
            width = point_spread,
            height = 0,
            alpha = point_alpha,
            size = point_size
        )
    }
    
    # Layer 2: Trimmed points (gray outline, white fill) - shown but excluded from stats
    if (length(trimmed_idx) > 0) {
        trimmed_data <- data[trimmed_idx, , drop = FALSE]
        trimmed_data$.data_id <- trimmed_idx
        
        p <- p + ggiraph::geom_jitter_interactive(
            data = trimmed_data,
            ggplot2::aes(
                tooltip = .data[[".tooltip"]],
                data_id = .data[[".data_id"]]
            ),
            shape = 21,  # Circle with outline
            color = "gray40",
            fill = "white",
            hover_nearest = TRUE,
            width = point_spread,
            height = 0,
            alpha = point_alpha,
            size = point_size
        )
    }
    
    # Layer 3: Outlier points (gray with X shape)
    if (length(outlier_idx) > 0) {
        outlier_data <- data[outlier_idx, , drop = FALSE]
        outlier_data$.data_id <- outlier_idx
        
        p <- p + ggiraph::geom_jitter_interactive(
            data = outlier_data,
            ggplot2::aes(
                tooltip = .data[[".tooltip"]],
                data_id = .data[[".data_id"]]
            ),
            shape = 4,  # X shape for outliers
            color = "gray40",
            hover_nearest = TRUE,
            width = point_spread,
            height = 0,
            alpha = point_alpha,
            size = point_size
        )
    }
    
    # Add color scales
    if (!is.null(color_map) && length(color_map) > 0) {
        p <- p +
            ggplot2::scale_color_manual(values = color_map, name = color_legend_title) +
            ggplot2::scale_fill_manual(values = color_map, name = color_legend_title)
    } else {
        p <- p +
            ggplot2::scale_color_discrete(name = color_legend_title) +
            ggplot2::scale_fill_discrete(name = color_legend_title)
    }
    
    # Extract grid/legend options with defaults
    legend_pos <- grid_legend$legend_position %||% "none"
    h_grid <- grid_legend$h_grid %||% TRUE
    v_grid <- grid_legend$v_grid %||% TRUE
    top_right_borders <- grid_legend$top_right_borders %||% TRUE
    show_median <- grid_legend$show_median %||% TRUE
    show_sd <- grid_legend$show_sd %||% TRUE
    aspect_ratio <- grid_legend$aspect_ratio %||% FALSE
    
    # Extract stat line style with defaults
    median_thickness <- stat_line_style$median_thickness %||% 0.5
    median_width <- stat_line_style$median_width %||% 0.15
    sd_thickness <- stat_line_style$sd_thickness %||% 0.5
    sd_width <- stat_line_style$sd_width %||% 0.15
    
    # Extract axis style with defaults
    tick_length <- axis_style$tick_length %||% 0.15
    line_thickness <- axis_style$line_thickness %||% 0.5
    
    # Add median crossbar (only on retained data)
    if (show_median && length(retained_idx) > 0) {
        retained_data <- data[retained_idx, , drop = FALSE]
        p <- p + ggplot2::stat_summary(
            data = retained_data,
            fun = median,
            geom = "crossbar",
            width = median_width,
            color = "black",
            linewidth = median_thickness
        )
    }
    
    # Add SD error bars (only on retained data)
    if (show_sd && length(retained_idx) > 0) {
        retained_data <- data[retained_idx, , drop = FALSE]
        p <- p + ggplot2::stat_summary(
            data = retained_data,
            fun.data = ggplot2::mean_sdl,
            fun.args = list(mult = 1),
            geom = "errorbar",
            width = sd_width,
            color = "black",
            linewidth = sd_thickness
        )
    }
    
    # Labels
    p <- p + ggplot2::labs(
        x = paste(x_col, collapse = " | "),
        y = y_col,
        color = color_legend_title,
        fill = color_legend_title
    )
    
    # Theme with grid and border options
    p <- p +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = if (length(x_col) == 1) 45 else 0, 
                hjust = 1
            ),
            panel.grid.major.x = if (!v_grid) ggplot2::element_blank() else ggplot2::element_line(color = "gray90"),
            panel.grid.minor.x = if (!v_grid) ggplot2::element_blank() else ggplot2::element_line(color = "gray90"),
            panel.grid.major.y = if (!h_grid) ggplot2::element_blank() else ggplot2::element_line(color = "gray90"),
            panel.grid.minor.y = if (!h_grid) ggplot2::element_blank() else ggplot2::element_line(color = "gray90"),
            axis.ticks.length = ggplot2::unit(tick_length, "cm"),
            axis.line = ggplot2::element_line(colour = "black", linewidth = line_thickness),
            panel.border = if (!top_right_borders) ggplot2::element_blank() else ggplot2::element_rect(
                color = "black", 
                linewidth = line_thickness
            ),
            legend.position = legend_pos,
            plot.margin = ggplot2::margin(10, 10, 10, 10)
        )
    
    # Add aspect ratio if requested
    if (aspect_ratio) {
        p <- p + ggplot2::theme(aspect.ratio = 1)
    }
    
    # Add nested axis support using ggh4x if multiple x columns
    # guide_axis_nested parses the interaction labels by delimiter to create hierarchy
    if (length(x_col) > 1) {
        if (requireNamespace("ggh4x", quietly = TRUE)) {
            p <- p + 
                ggplot2::scale_x_discrete(guide = ggh4x::guide_axis_nested(delim = ".")) +
                ggplot2::theme(
                    ggh4x.axis.nestline.x = ggplot2::element_line(linewidth = 0.5)
                )
        }
    }
    
    return(p)
}


#' Build tooltip text for each data point
#'
#' @param data Data frame
#' @param x_var Name of x variable in data
#' @param x_label Display label for x axis
#' @param y_col Name of y column
#' @param tooltip_cols Additional columns to include
#' @param is_trimmed Logical vector indicating trimmed points
#' @param is_outlier Logical vector indicating outlier points
#' @return Character vector of tooltip HTML strings
build_tooltip_text <- function(data, x_var, x_label, y_col, tooltip_cols = NULL,
                               is_trimmed = NULL, is_outlier = NULL) {
    
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
    
    # Add status indicators for trimmed/outlier points
    if (!is.null(is_trimmed) || !is.null(is_outlier)) {
        status <- sapply(seq_len(nrow(data)), function(i) {
            flags <- c()
            if (!is.null(is_trimmed) && is_trimmed[i]) {
                flags <- c(flags, "<span style='color:#dc3545;'>Trimmed</span>")
            }
            if (!is.null(is_outlier) && is_outlier[i]) {
                flags <- c(flags, "<span style='color:#fd7e14;'>Outlier</span>")
            }
            if (length(flags) > 0) {
                paste0("<br/><em>", paste(flags, collapse = ", "), "</em>")
            } else {
                ""
            }
        })
        tooltip_parts <- paste0(tooltip_parts, status)
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
