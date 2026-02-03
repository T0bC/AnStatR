#' Color Pickers Component
#'
#' Handles color group detection and dynamic color picker UI rendering.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @name color_pickers
NULL

# Import data utilities for create_interaction function
box::use(../../utils/data_utils)


#' Create Color Groups Reactive
#'
#' Gets unique color groups based on interaction of selected color columns.
#'
#' @param filtered_data Reactive returning filtered data frame
#' @param color_cols Reactive returning selected color columns
#' @return Reactive returning sorted character vector of unique group names
#' @export
create_color_groups_reactive <- function(filtered_data, color_cols) {
    shiny::reactive({
        data <- filtered_data()
        cols <- color_cols()
        
        if (is.null(data) || nrow(data) == 0 || is.null(cols) || length(cols) == 0) {
            return(character(0))
        }
        
        # Use create_interaction to get unique group levels
        interaction_factor <- data_utils$create_interaction(data, cols)
        base::sort(as.character(unique(interaction_factor)))
    })
}


#' Create Custom Color Map Reactive
#'
#' Collects custom colors from dynamic color picker inputs.
#'
#' @param input Shiny input object from parent module
#'   - input$color_*: Dynamic color picker inputs for each group
#' @param color_groups Reactive returning unique group names
#' @return Reactive returning named character vector of colors
#' @export
create_custom_color_map_reactive <- function(input, color_groups) {
    shiny::reactive({
        groups <- color_groups()
        if (length(groups) == 0) return(NULL)
        
        # Build named vector of colors from inputs
        colors <- sapply(groups, function(group) {
            input_id <- paste0("color_", gsub("[^[:alnum:]]", "_", group))
            color <- input[[input_id]]
            if (is.null(color)) {
                NA_character_
            } else {
                color
            }
        })
        names(colors) <- groups
        
        # Fill in NA values with default palette
        na_idx <- is.na(colors)
        if (any(na_idx)) {
            default_colors <- if (length(groups) <= 8) {
                scales::hue_pal()(length(groups))
            } else {
                grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                              "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(length(groups))
            }
            colors[na_idx] <- default_colors[na_idx]
        }
        
        colors
    })
}


#' Setup Color Pickers Output
#'
#' Renders dynamic color pickers for unique groups in the data.
#'
#' @param output Shiny output object from parent module
#' @param ns Namespace function from parent module (session$ns)
#' @param filtered_data Reactive returning filtered data frame
#' @param color_cols Reactive returning selected color columns
#' @param color_groups Reactive returning unique group names
#' @param custom_color_map Reactive returning current color map
#' @return NULL (side effects only - registers output)
#' @export
setup_color_pickers_output <- function(output, 
                                        ns, 
                                        filtered_data, 
                                        color_cols, 
                                        color_groups, 
                                        custom_color_map) {
    output$colorPickers <- shiny::renderUI({
        data <- filtered_data()
        cols <- color_cols()
        
        # Need data and color column selection
        if (is.null(data) || nrow(data) == 0 || is.null(cols) || length(cols) == 0) {
            return(shiny::tags$p(
                class = "text-muted small fst-italic",
                "Select X-Axis columns to customize group colors."
            ))
        }
        
        # Get unique groups
        groups <- color_groups()
        
        if (length(groups) == 0) {
            return(shiny::tags$p(class = "text-muted small", "No groups found."))
        }
        
        # Generate default color palette
        existing_colors <- custom_color_map()
        default_colors <- if (length(groups) <= 8) {
            scales::hue_pal()(length(groups))
        } else {
            grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                          "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(length(groups))
        }
        
        # Create color pickers in a responsive grid
        num_cols <- min(3, length(groups))
        col_width <- 12 / num_cols
        
        color_inputs <- lapply(seq_along(groups), function(i) {
            group <- groups[i]
            input_id <- paste0("color_", gsub("[^[:alnum:]]", "_", group))
            
            # Use existing color if available, otherwise default
            current_color <- if (!is.null(existing_colors) && group %in% names(existing_colors)) {
                existing_colors[[group]]
            } else {
                default_colors[i]
            }
            
            shiny::column(
                width = col_width,
                colourpicker::colourInput(
                    inputId = ns(input_id),
                    label = group,
                    value = current_color,
                    showColour = "both",
                    allowTransparent = FALSE,
                    closeOnClick = TRUE
                )
            )
        })
        
        shiny::fluidRow(color_inputs)
    })
}
