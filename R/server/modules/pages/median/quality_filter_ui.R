# Quality Filter UI rendering
# This file defines a function that renders the quality column selection and filtering UI
#
# @param output Shiny output object
# @param output_id Character string for the output ID
# @param loaded_data Reactive containing the loaded data
# @param input Shiny input object
# @param session Shiny session object
# @param quality_settings ReactiveVal to store quality filter settings
# @return NULL (side effects: creates output and updates quality_settings)

render_quality_filter_ui <- function(output, output_id, loaded_data, input, session, quality_settings) {
    ns <- session$ns
    
    # Reactive to get descriptive columns for quality selection (strict pattern)
    descriptive_cols <- shiny::reactive({
        shiny::req(loaded_data())
        get_descriptive_cols(loaded_data())
    })
    
    # Reactive to analyze selected quality column
    quality_col_info <- shiny::reactive({
        shiny::req(loaded_data())
        shiny::req(input$quality_column)
        
        if (input$quality_column == "None") {
            return(list(type = "none"))
        }
        
        data <- loaded_data()
        col_data <- data[[input$quality_column]]
        
        # Analyze the column
        unique_vals <- unique(col_data[!is.na(col_data)])
        n_unique <- length(unique_vals)
        
        # Check if numeric
        if (is.numeric(col_data)) {
            min_val <- min(col_data, na.rm = TRUE)
            max_val <- max(col_data, na.rm = TRUE)
            
            # Check if it's discrete integers with few unique values (like quality grades 1-4)
            # Treat as categorical if: all integers AND few unique values (≤10)
            all_integers <- all(col_data == floor(col_data), na.rm = TRUE)
            
            if (all_integers && n_unique <= 10) {
                # Treat as categorical (quality grades like 1, 2, 3, 4)
                return(list(
                    type = "categorical",
                    unique_values = sort(as.character(unique_vals)),
                    n_unique = n_unique,
                    hint = paste0("Quality grades detected (", n_unique, " levels): ",
                                  paste(sort(unique_vals), collapse = ", "), ".")
                ))
            }
            
            # Continuous numeric - detect percentage type
            if (min_val >= 0 && max_val <= 1) {
                return(list(
                    type = "percentage_decimal",
                    min = min_val,
                    max = max_val,
                    hint = paste0("Percentage values (", round(min_val, 2), " - ", round(max_val, 2), 
                                  ") in decimal format (0-1).")
                ))
            } else if (min_val >= 0 && max_val <= 100 && n_unique > 10) {
                return(list(
                    type = "percentage_100",
                    min = min_val,
                    max = max_val,
                    hint = paste0("Percentage values (", round(min_val, 2), " - ", round(max_val, 2), 
                                  ") in 0-100 format.")
                ))
            } else {
                return(list(
                    type = "numeric",
                    min = min_val,
                    max = max_val,
                    hint = paste0("Numeric values (", round(min_val, 2), " - ", round(max_val, 2), 
                                  "). Set minimum threshold for good quality.")
                ))
            }
        }
        
        # Categorical (non-numeric)
        list(
            type = "categorical",
            unique_values = sort(as.character(unique_vals)),
            n_unique = n_unique,
            hint = paste0("Categorical values (", n_unique, " levels): ",
                          paste(head(sort(as.character(unique_vals)), 5), collapse = ", "),
                          if (n_unique > 5) "..." else "")
        )
    })
    
    # Render quality column selection UI
    output[[output_id]] <- shiny::renderUI({
        shiny::req(loaded_data())
        
        cols <- descriptive_cols()
        
        shiny::tagList(
            shiny::tags$p(
                class = "text-muted small",
                "Optional: Select a column that indicates measurement quality."
            ),
            shiny::selectizeInput(
                inputId = ns("quality_column"),
                label = NULL,
                choices = c("None (no quality filtering)" = "None", cols),
                selected = "None",
                multiple = FALSE,
                options = list(placeholder = "Select quality column...")
            ),
            # Dynamic UI based on column type
            shiny::uiOutput(ns("quality_filter_options"))
        )
    })
    
    # Render filter options based on detected column type
    output$quality_filter_options <- shiny::renderUI({
        info <- quality_col_info()
        
        if (info$type == "none") {
            return(NULL)
        }
        
        shiny::tagList(
            # Hint text about detected type
            shiny::tags$p(
                class = "text-muted small fst-italic",
                info$hint
            ),
            
            # Conditional UI based on type
            if (info$type == "categorical") {
                shiny::tagList(
                    shiny::selectizeInput(
                        inputId = ns("bad_quality_values"),
                        label = "Select BAD quality values to filter out:",
                        choices = info$unique_values,
                        selected = NULL,
                        multiple = TRUE,
                        options = list(
                            placeholder = "Select values to exclude..."
                        )
                    )
                )
            } else {
                # Numeric/percentage - threshold input
                default_threshold <- if (info$type == "percentage_decimal") {
                    0.8
                } else if (info$type == "percentage_100") {
                    80
                } else {
                    info$min + (info$max - info$min) * 0.5  # Default to midpoint
                }
                
                shiny::tagList(
                    shiny::numericInput(
                        inputId = ns("quality_threshold"),
                        label = "Minimum quality threshold (keep values ≥):",
                        value = default_threshold,
                        min = info$min,
                        max = info$max,
                        step = if (info$type == "percentage_decimal") 0.05 else 1
                    )
                )
            }
        )
    })
    
    # Update quality settings reactive when inputs change
    shiny::observe({
        info <- quality_col_info()
        
        if (is.null(input$quality_column) || input$quality_column == "None") {
            quality_settings(list(
                enabled = FALSE,
                column = NULL,
                type = "none"
            ))
        } else if (info$type == "categorical") {
            quality_settings(list(
                enabled = TRUE,
                column = input$quality_column,
                type = "categorical",
                bad_values = input$bad_quality_values
            ))
        } else {
            quality_settings(list(
                enabled = TRUE,
                column = input$quality_column,
                type = info$type,
                threshold = input$quality_threshold
            ))
        }
    })
}
