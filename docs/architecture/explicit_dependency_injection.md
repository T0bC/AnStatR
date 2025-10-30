# Explicit Dependency Injection Pattern

## Pattern Overview

All modular server components use **explicit function calls with named parameters** instead of implicit variable scoping via `source()`.

## Implementation Rules

1. **Component files define functions** - Each file in `R/server/modules/pages/[module]/` exports a function with explicit parameters
2. **Parameters are documented** - Use `@param` comments to document each parameter
3. **Main module calls functions explicitly** - The parent module calls component functions with all required arguments

## Example Structure

### Component File (`file_upload.R`)

```r
# @param data_file_input Reactive input from fileInput (input$data_file)
# @param csv_delimiter Reactive input for CSV delimiter
# @param loaded_data ReactiveVal to store the loaded data

handle_file_upload <- function(data_file_input, csv_delimiter, loaded_data) {
  shiny::observeEvent(data_file_input, {
    # Implementation using parameters
  })
}
```

### Parent Module (`server_load_data.R`)

```r
# Source the function definition
source("R/server/modules/pages/load_data/file_upload.R", local = TRUE)

# Call with explicit arguments - dependencies are immediately visible
handle_file_upload(
  data_file_input = input$data_file,
  csv_delimiter = input$csv_delimiter,
  loaded_data = loaded_data
)
```

## Benefits

- **Traceability**: See exactly which `input$*` elements each component uses
- **Self-documenting**: Function signatures serve as API contracts
- **IDE support**: Developers can cross-reference UI inputs to server usage
- **Testability**: Functions can be tested with mock parameters

## When to Apply

Use this pattern for all new modular server components that require access to `input`, `output`, `session`, or shared reactive values.
