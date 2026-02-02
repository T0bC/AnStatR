#' Filter Logic Component
#'
#' Handles filter column selection, checkbox UI rendering, and filtered data reactive.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @name filter_logic
NULL


#' Create Filter Columns Reactive
#'
#' @param input Shiny input object from parent module
#'   - input$metaData: Selected descriptive columns
#'   - input$hideCols: Columns to hide from filtering
#' @return Reactive returning character vector of columns to show for filtering
create_filter_cols_reactive <- function(input) {
    shiny::reactive({
        selected <- input$metaData
        hidden <- input$hideCols
        if (is.null(selected)) return(character(0))
        selected[!selected %in% hidden]
    })
}


#' Create Filtered Data Reactive
#'
#' Filters data based on checkbox selections for each filter column.
#'
#' @param input Shiny input object from parent module
#'   - input[[col]]: Checkbox group selections for each filter column
#' @param median_data Reactive containing the median-processed data
#' @param filter_cols Reactive returning columns to filter on
#' @return Reactive returning filtered data frame
create_filtered_data_reactive <- function(input, median_data, filter_cols) {
    shiny::reactive({
        data <- median_data()
        shiny::req(data)
        
        cols <- filter_cols()
        if (length(cols) == 0) return(data)
        
        # Apply filters from each checkbox group
        for (col in cols) {
            selected_values <- input[[col]]
            if (!is.null(selected_values) && length(selected_values) > 0) {
                # Handle "NA" marker: it represents NA values in the data
                col_values <- data[[col]]
                include_na <- "NA" %in% selected_values
                # Remove "NA" from selected_values for %in% comparison
                selected_values <- selected_values[selected_values != "NA"]
                
                # Match non-NA values
                matches <- col_values %in% selected_values
                # %in% returns NA for NA values, set to FALSE initially
                matches[is.na(matches)] <- FALSE
                # Include NA rows if "NA" was selected
                if (include_na) {
                    matches[is.na(col_values)] <- TRUE
                }
                data <- data[matches, , drop = FALSE]
            }
        }
        
        data
    })
}


#' Render Filter Checkboxes UI
#'
#' Creates dynamic checkbox groups for filtering based on selected columns.
#' Restores previously selected filter values when columns persist across median recalculations.
#'
#' @param output Shiny output object from parent module
#' @param ns Namespace function from parent module (session$ns)
#' @param median_data Reactive containing the median-processed data
#' @param filter_cols Reactive returning columns to show for filtering
#' @param saved_filter_state ReactiveVal containing saved filter selections (list of column -> selected values)
#' @return NULL (side effects only - registers output)
setup_filter_checkboxes_output <- function(output, ns, median_data, filter_cols, saved_filter_state = NULL) {
    output$checkboxes <- shiny::renderUI({
        data <- median_data()
        shiny::req(data)
        
        cols <- filter_cols()
        
        if (length(cols) == 0) {
            return(shiny::tags$p(
                class = "text-muted fst-italic small",
                "Select descriptive columns or unhide some to see filtering options."
            ))
        }
        
        # Get saved state (isolate to prevent re-render loops)
        saved_state <- if (!is.null(saved_filter_state)) shiny::isolate(saved_filter_state()) else list()
        
        # Helper to get choices with NA displayed as "NA"
        get_choices_with_na_label <- function(values) {
            choices <- unique(values)
            has_na <- any(is.na(choices))
            choices <- choices[!is.na(choices)]
            if (has_na) choices <- c(choices, "NA")
            choices
        }
        
        # Helper to determine selected values for a column
        get_selected_values <- function(col, choices) {
            if (!is.null(saved_state[[col]])) {
                # Intersect saved selections with available choices
                valid_selections <- intersect(saved_state[[col]], choices)
                if (length(valid_selections) > 0) return(valid_selections)
            }
            # Default: select all
            choices
        }
        
        # Split columns into two groups for side-by-side layout
        if (length(cols) > 1) {
            half <- ceiling(length(cols) / 2)
            cols1 <- cols[seq_len(half)]
            cols2 <- cols[-seq_len(half)]
            
            shiny::fluidRow(
                shiny::column(6, lapply(cols1, function(col) {
                    choices <- get_choices_with_na_label(data[[col]])
                    selected <- get_selected_values(col, choices)
                    shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = selected)
                })),
                shiny::column(6, lapply(cols2, function(col) {
                    choices <- get_choices_with_na_label(data[[col]])
                    selected <- get_selected_values(col, choices)
                    shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = selected)
                }))
            )
        } else {
            col <- cols[1]
            choices <- get_choices_with_na_label(data[[col]])
            selected <- get_selected_values(col, choices)
            shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = selected)
        }
    })
}
