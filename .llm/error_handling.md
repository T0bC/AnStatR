# Unified Error Handling Pattern

## Pattern Overview

Use **structured error objects** and **standardized UI components** to provide consistent, user-friendly error reporting with optional developer details (stack traces, context).

## The Problem

Without standardized error handling:
- Errors display inconsistently across modules
- Users see raw R error messages they can't understand
- Developers lack context for debugging (no stack traces, no parameter info)
- Error checking logic is duplicated throughout the codebase

## Solution: Two-Layer Architecture

### Layer 1: Error Handling (`app/logic/error_handling.R`)

Creates structured error objects with user-friendly messages and debugging context.

### Layer 2: Error Display (`app/view/error_display.R`)

Renders structured errors as consistent UI components with expandable details.

---

## Core Functions

### Creating Errors

```r
# For operations that might fail - wrap in safe_execute()
result <- safe_execute(
    expr = some_risky_operation(),
    operation_name = "Data Import",
    context = list(file = filename, rows = nrow(data)),
    error_parser = default_error_parser  # Optional custom parser
)

if (!result$success) {
    return(result$error)  # Returns structured error object
}
# Use result$result for successful operations
```

### Checking for Errors

```r
# Check if an object is a structured error
if (is_app_error(obj)) {
    # Handle error case
}
```

### Creating Simple Validation Errors

```r
# For validation failures (no R error condition)
if (nrow(data) == 0) {
    return(simple_error(
        message = "No data available after filtering.",
        operation_name = "Data Validation",
        context = list(filter_applied = TRUE)
    ))
}
```

### Domain-Specific Error Parsers

```r
# Use stat_error_parser for statistical tests
result <- safe_execute(
    expr = WRS2::t1way(formula, data, tr = 0.2),
    operation_name = "t1way",
    context = list(measure = "Asfc", groups = 3),
    error_parser = stat_error_parser
)

# Or create custom parsers for other domains
my_parser <- function(error_msg, operation_name) {
    if (grepl("specific pattern", error_msg)) {
        return("User-friendly message for this case")
    }
    paste0(operation_name, " failed: ", error_msg)
}
```

---

## Displaying Errors

### In renderUI / Output Contexts

```r
output$my_output <- renderUI({
    result <- compute_something()
    
    if (is_app_error(result)) {
        return(error_alert_structured(result, type = "danger"))
    }
    
    # Render normal output
})
```

### Simple Alert (No Structured Error)

```r
error_alert(
    message = "Please select at least one column.",
    type = "warning",  # "danger", "warning", "info"
    dismissible = TRUE
)
```

### Modal Dialog for Critical Errors

```r
show_error_modal(
    error_obj = structured_error,
    title = "Computation Failed",
    session = session
)

# Or simple message modal
show_error_message_modal(
    message = "File format not supported.",
    title = "Import Error"
)
```

---

## Structured Error Object Format

```r
list(
    is_error = TRUE,
    operation_name = "t1way",
    message = "t1way: Insufficient groups for comparison.",
    raw_message = "groups must have at least 2 levels",
    context = list(measure = "Asfc", n_groups = 1),
    traces = list(
        stack_trace = "<pre>formatted stack trace HTML</pre>"
    ),
    timestamp = "2024-01-15 10:30:00"
)
```

---

## Implementation Example

### Utility Function (app/logic/statistics_utils.R)

```r
box::use(
    app/logic/error_handling[safe_execute, stat_error_parser]
)

perform_t1way <- function(df, x_axis, measure_col, tr_value, ...) {
    # Build context for error reporting
    error_context <- list(
        measure = measure_col,
        grouping = x_axis,
        n_observations = nrow(df),
        trim = tr_value
    )
    
    # Wrap risky operation
    test_result <- safe_execute(
        expr = {
            WRS2::t1way(formula = formula_obj, data = df, tr = tr_value)
        },
        operation_name = "t1way",
        context = error_context,
        error_parser = stat_error_parser
    )
    
    # Return error if failed
    if (!test_result$success) {
        return(test_result$error)
    }
    
    # Process and return successful result
    test_result$result
}
```

### Output Rendering (app/view/statistics_output.R)

```r
box::use(
    app/logic/error_handling[is_app_error, render_app_error],
    shiny[tags]
)

# Check and render appropriately
if (is_app_error(res$result_t_way)) {
    tway_ui <- tags$div(
        class = "alert alert-danger",
        render_app_error(res$result_t_way)
    )
} else {
    tway_ui <- render_stats_table(res$result_t_way)
}
```

---

## Available Functions

| Function | Purpose |
|----------|---------|
| `safe_execute()` | Wrap risky operations, returns `{success, result, error}` |
| `create_app_error()` | Create structured error with full details |
| `simple_error()` | Create error for validation failures (no stack trace) |
| `is_app_error()` | Check if object is a structured error |
| `default_error_parser()` | Parse common R errors to user-friendly messages |
| `stat_error_parser()` | Parse statistical test errors |
| `render_app_error()` | Render structured error with expandable details |
| `error_alert()` | Simple Bootstrap alert |
| `error_alert_structured()` | Alert containing structured error |
| `show_error_modal()` | Modal dialog for structured errors |
| `show_error_message_modal()` | Modal dialog for simple messages |

---

## When to Apply

- **Wrap external package calls** (WRS2, readxl, etc.) in `safe_execute()`
- **Use `simple_error()`** for validation failures before operations
- **Always provide `context`** with relevant parameters for debugging
- **Use domain-specific parsers** (`stat_error_parser`) when available
- **Render with `render_app_error()`** to show expandable stack traces

---

## Box Import Guidelines for Error Handling

### In Logic Files (app/logic/)

```r
box::use(
    app/logic/error_handling[safe_execute, simple_error, is_app_error]
)
```

### In View Files (app/view/)

```r
box::use(
    app/logic/error_handling[is_app_error, render_app_error, error_alert],
    shiny[tags, showModal, modalDialog]
)
```

### In Module Files

```r
box::use(
    shiny[moduleServer, NS],
    app/logic/error_handling[safe_execute, is_app_error]
)

#' @export
ui <- function(id) {
    ns <- NS(id)
    # UI code here
}

#' @export
server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Module server code with error handling
    })
}
```
