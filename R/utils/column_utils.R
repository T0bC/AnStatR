# Column utility functions for identifying column types
# These functions are used throughout the app to distinguish between
# descriptive (categorical) columns and measurement (numeric) columns.
#
# Column naming conventions:
# - Descriptive columns: UPPERCASE with underscores only (e.g., SPECIES, SAMPLE_ID)
# - Measurement columns: Mixed case with numbers (e.g., Asfc, epLsar, Sq1)

#' Get measurement columns from a data frame
#'
#' Measurement columns are identified by NOT matching the uppercase-only pattern.
#' These are typically numeric columns with mixed case names containing numbers.
#'
#' @param data A data frame
#' @return Character vector of column names that are measurement columns
#' @export
get_measurement_cols <- function(data) {
  names(data)[which(!grepl("^[A-Z0-9_]+$", names(data)))]
}

#' Get descriptive columns from a data frame (strict)
#'
#' Descriptive columns follow the pattern: uppercase letters and underscores only.
#' No numbers allowed in column names.
#'
#' @param data A data frame
#' @return Character vector of column names that are descriptive columns
#' @export
get_descriptive_cols <- function(data) {
  names(data)[which(grepl("^[A-Z_]+$", names(data)))]
}

#' Get descriptive columns with threshold filter (lenient)
#'
#' This version allows numbers in column names but filters by unique value count.
#' Useful for columns like SAMPLE_1, GROUP_2 that are still categorical.
#'
#' @param data A data frame
#' @param threshold Maximum number of unique values for a column to be considered descriptive
#' @return Character vector of column names that are descriptive columns
#' @export
get_descriptive_cols_short <- function(data, threshold = 20) {
  valid_cols <- names(data)[which(grepl("^[A-Z0-9_]+$", names(data)))]
  valid_cols[sapply(valid_cols, function(col) length(unique(data[[col]])) < threshold)]
}

#' Validate column naming conventions
#'
#' Checks if all columns follow the expected naming patterns and identifies
#' any problematic columns that don't fit either category.
#'
#' @param data A data frame
#' @return A list with:
#'   - valid: logical, TRUE if all columns follow conventions
#'   - descriptive_cols: character vector of descriptive columns
#'   - measurement_cols: character vector of measurement columns
#'   - ambiguous_cols: character vector of columns that match both patterns
#'   - unclassified_cols: character vector of columns that match neither pattern
#' @export
validate_column_naming <- function(data) {
  col_names <- names(data)
  

  # Pattern for descriptive: uppercase letters and underscores only
  descriptive_pattern <- "^[A-Z_]+$"
  # Pattern for measurement: NOT matching uppercase+numbers+underscores (mixed case)
  uppercase_pattern <- "^[A-Z0-9_]+$"
  
  # Classify columns
  is_uppercase <- grepl(uppercase_pattern, col_names)
  is_strict_descriptive <- grepl(descriptive_pattern, col_names)
  
  # Descriptive = strict uppercase only (no numbers)
  descriptive_cols <- col_names[is_strict_descriptive]
  

  # Measurement = NOT matching uppercase pattern (mixed case with numbers)
  measurement_cols <- col_names[!is_uppercase]
  
  # Ambiguous = uppercase WITH numbers (could be either)
  # These match uppercase pattern but not strict descriptive
  ambiguous_cols <- col_names[is_uppercase & !is_strict_descriptive]
  
  # Check for issues
  has_issues <- length(ambiguous_cols) > 0
  
  list(
    valid = !has_issues,
    descriptive_cols = descriptive_cols,
    measurement_cols = measurement_cols,
    ambiguous_cols = ambiguous_cols,
    message = if (has_issues) {
      paste0(
        "Some columns have ambiguous naming (uppercase with numbers): ",
        paste(ambiguous_cols, collapse = ", "),
        ". These columns will be treated as descriptive if they have < 20 unique values."
      )
    } else {
      NULL
    }
  )
}

#' Create modal dialog for column validation warnings
#'
#' @param validation_result Result from validate_column_naming()
#' @param session Shiny session object (for ns function)
#' @return A Shiny modalDialog object
#' @export
create_column_validation_modal <- function(validation_result, session = NULL) {
  ns <- if (!is.null(session)) session$ns else identity
  
  shiny::modalDialog(
    title = shiny::tags$span(
      bsicons::bs_icon("exclamation-triangle-fill", class = "text-warning"),
      " Column Naming Convention Warning"
    ),
    size = "l",
    easyClose = TRUE,
    footer = shiny::modalButton("Understood"),
    
    shiny::tags$div(
      shiny::tags$p(
        "Your data contains columns that don't strictly follow the expected naming conventions."
      ),
      shiny::tags$hr(),
      
      shiny::tags$h5("Expected Conventions:"),
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$strong("Descriptive columns: "),
          "UPPERCASE letters and underscores only (e.g., SPECIES, SAMPLE_ID, DIET)"
        ),
        shiny::tags$li(
          shiny::tags$strong("Measurement columns: "),
          "Mixed case with numbers (e.g., Asfc, epLsar, Sq1, HAsfc9)"
        )
      ),
      
      shiny::tags$hr(),
      shiny::tags$h5("Ambiguous Columns Found:"),
      shiny::tags$p(
        class = "text-warning",
        paste(validation_result$ambiguous_cols, collapse = ", ")
      ),
      shiny::tags$p(
        shiny::tags$em(
          "These columns match the uppercase pattern but contain numbers. ",
          "They will be treated as descriptive columns if they have fewer than 20 unique values."
        )
      ),
      
      if (length(validation_result$descriptive_cols) > 0) {
        shiny::tags$div(
          shiny::tags$h5("Detected Descriptive Columns:"),
          shiny::tags$p(class = "text-success", 
                        paste(validation_result$descriptive_cols, collapse = ", "))
        )
      },
      
      if (length(validation_result$measurement_cols) > 0) {
        shiny::tags$div(
          shiny::tags$h5("Detected Measurement Columns:"),
          shiny::tags$p(class = "text-info", 
                        paste(validation_result$measurement_cols, collapse = ", "))
        )
      }
    )
  )
}
