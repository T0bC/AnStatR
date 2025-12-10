#' Median Parameters Component
#'
#' Creates a unified debounced reactive for all median calculation inputs.
#' This prevents double-renders by consolidating grouping and quality filter
#' inputs into a single reactive that only updates when values actually change.
#'
#' Following the explicit dependency injection pattern from the plotting module.
#'
#' @name median_params
NULL


#' Create Consolidated Median Parameters
#'
#' Creates a unified reactive that bundles grouping columns and quality settings
#' with caching and debouncing. Only triggers downstream updates when values
#' actually change.
#'
#' @param loaded_data Reactive containing the loaded data
#' @param input Shiny input object from parent module
#'   - input$grouping_columns: Selected grouping columns
#'   - input$quality_column: Selected quality column
#'   - input$bad_quality_values: Bad values for categorical quality
#'   - input$quality_threshold: Threshold for numeric quality
#' @param quality_col_info Reactive returning quality column analysis
#' @param debounce_ms Debounce delay in milliseconds (default: 400)
#' @return Reactive returning list with:
#'   - grouping_cols: Character vector of grouping columns
#'   - quality_settings: List with filter settings
create_median_params <- function(loaded_data, input, quality_col_info, debounce_ms = 400) {
    
    # Cache for median parameters
    cached_params <- shiny::reactiveVal(NULL)
    
    # Helper to create fingerprint for comparison
    make_fingerprint <- function(params) {
        paste(
            paste(params$grouping_cols %||% "NULL", collapse = ":"),
            params$quality_settings$enabled,
            params$quality_settings$column %||% "NULL",
            params$quality_settings$type,
            paste(params$quality_settings$bad_values %||% "NULL", collapse = ":"),
            params$quality_settings$threshold %||% "NULL",
            sep = "|"
        )
    }
    
    # Build quality settings from inputs
    build_quality_settings <- function(info) {
        if (is.null(input$quality_column) || input$quality_column == "None") {
            list(
                enabled = FALSE,
                column = NULL,
                type = "none"
            )
        } else if (info$type == "categorical") {
            list(
                enabled = TRUE,
                column = input$quality_column,
                type = "categorical",
                bad_values = input$bad_quality_values
            )
        } else {
            list(
                enabled = TRUE,
                column = input$quality_column,
                type = info$type,
                threshold = input$quality_threshold
            )
        }
    }
    
    # Observer that updates cache only when values change
    shiny::observe({
        # Require data to be loaded
        shiny::req(loaded_data())
        
        # Collect all inputs
        grouping_val <- input$grouping_columns
        info <- quality_col_info()
        quality_val <- build_quality_settings(info)
        
        new_params <- list(
            grouping_cols = grouping_val,
            quality_settings = quality_val
        )
        
        # Compare fingerprints
        current <- cached_params()
        new_fp <- make_fingerprint(new_params)
        old_fp <- if (!is.null(current)) make_fingerprint(current) else ""
        
        if (new_fp != old_fp) {
            cached_params(new_params)
        }
    }) |> shiny::debounce(debounce_ms)
    
    # Expose cached params as reactive
    shiny::reactive({ cached_params() })
}
