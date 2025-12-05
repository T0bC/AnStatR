# Explicit Dependency Injection Pattern

## Pattern Overview

All modular server components use **explicit function calls with named parameters** instead of implicit variable scoping via `source()`.

## Understanding Shiny's Implicit Objects

When using `shiny::moduleServer()`, three objects are implicitly provided:

```r
server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # input  - Reactive list of UI inputs namespaced to this module
    # output - Reactive list for rendering outputs (tables, plots, etc.)
    # session - Shiny session object for namespace management
  })
}
```

These objects are **scoped to the module's namespace**. For example, `input$data_file` here corresponds to `fileInput(ns("data_file"))` in the matching UI function.

## Implementation Rules

1. **Component files define functions** - Each file in `R/server/modules/pages/[module]/` exports a function with explicit parameters
2. **Parameters are documented** - Use `@param` comments listing which inputs the function accesses
3. **Main module calls functions explicitly** - The parent module calls component functions with all required arguments

## Example Structure

### Component File (`file_upload.R`)

```r
# @param input Shiny input object from the parent module.
#   Contains reactive references to UI inputs defined in ui_load_data.R:
#   - input$data_file: The uploaded file from fileInput(ns("data_file"))
#   - input$csv_has_header: Checkbox for CSV header setting
#   - input$csv_delimiter: Radio button for CSV delimiter
# @param loaded_data ReactiveVal to store the loaded data

handle_file_upload <- function(input, loaded_data) {
  shiny::observeEvent(input$data_file, {
    # Access input$csv_delimiter, input$csv_has_header, etc.
  })
}
```

**Why pass `input` instead of individual values?**  
Shiny's `observeEvent()` and reactive expressions need reactive references to detect changes. Passing `input$data_file` directly captures its value at call time (often `NULL`), not a reactive reference. Passing the `input` object preserves reactivity.

### Parent Module (`server_load_data.R`)

```r
# Source the function definition
source("R/server/modules/pages/load_data/file_upload.R", local = TRUE)

# Call with explicit arguments - dependencies are immediately visible
handle_file_upload(
  input = input,  # Module input object from moduleServer()
  loaded_data = loaded_data
)
```

## Benefits

- **Traceability**: Function documentation lists which `input$*` elements each component uses
- **Self-documenting**: Function signatures and `@param` comments serve as API contracts
- **IDE support**: Developers can cross-reference UI inputs to server usage
- **Testability**: Functions can be tested with mock input objects

## When to Apply

Use this pattern for all new modular server components that require access to `input`, `output`, `session`, or shared reactive values.
