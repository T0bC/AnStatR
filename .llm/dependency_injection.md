# Explicit Dependency Injection Pattern

## Pattern Overview

All modular server components use **explicit function calls with named parameters** instead of implicit variable scoping via `box::use` imports.

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

1. **Component files define functions** - Each file in `app/logic/` exports a function with explicit parameters
2. **Parameters are documented** - Use `@param` comments listing which inputs the function accesses
3. **Main module calls functions explicitly** - The parent module calls component functions with all required arguments

## Example Structure

### Component File (`app/logic/file_upload.R`)

```r
box::use(
  shiny[observeEvent, reactiveVal]
)

#' @param input Shiny input object from the parent module.
#'   Contains reactive references to UI inputs defined in ui_load_data.R:
#'   - input$data_file: The uploaded file from fileInput(ns("data_file"))
#'   - input$csv_has_header: Checkbox for CSV header setting
#'   - input$csv_delimiter: Radio button for CSV delimiter
#' @param loaded_data ReactiveVal to store the loaded data

#' @export
handle_file_upload <- function(input, loaded_data) {
  observeEvent(input$data_file, {
    # Access input$csv_delimiter, input$csv_has_header, etc.
  })
}
```

**Why pass `input` instead of individual values?**  
Shiny's `observeEvent()` and reactive expressions need reactive references to detect changes. Passing `input$data_file` directly captures its value at call time (often `NULL`), not a reactive reference. Passing the `input` object preserves reactivity.

### Parent Module (`app/logic/server_load_data.R`)

```r
box::use(
  shiny[moduleServer, reactiveVal],
  app/logic/file_upload[handle_file_upload]
)

server_load_data <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Create reactive values for this module
    loaded_data <- reactiveVal(NULL)
    
    # Call with explicit arguments - dependencies are immediately visible
    handle_file_upload(
      input = input,  # Module input object from moduleServer()
      loaded_data = loaded_data
    )
  })
}
```

## Benefits

- **Traceability**: Function documentation lists which `input$*` elements each component uses
- **Self-documenting**: Function signatures and `@param` comments serve as API contracts
- **IDE support**: Developers can cross-reference UI inputs to server usage
- **Testability**: Functions can be tested with mock input objects

## When to Apply

Use this pattern for all new modular server components that require access to `input`, `output`, `session`, or shared reactive values.

---

## Box Import Guidelines for Dependency Injection

### In Logic Files (app/logic/)

```r
box::use(
  shiny[moduleServer, observeEvent, reactiveVal],
  app/logic/another_module[some_function]
)
```

### In View Files (app/view/)

```r
box::use(
  shiny[NS, moduleServer, tags],
  app/logic/dependency_module[handle_dependency]
)
```

### Component Module Pattern

```r
# app/logic/component.R
box::use(
  shiny[observeEvent, reactiveVal]
)

#' @export
handle_component <- function(input, output, session, shared_reactive_vals) {
  # Component logic with explicit dependencies
}
```

### Parent Module Usage

```r
# app/logic/parent_module.R
box::use(
  shiny[moduleServer, reactiveVal],
  app/logic/component[handle_component]
)

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    shared_data <- reactiveVal(NULL)
    
    # Explicit dependency injection
    handle_component(
      input = input,
      output = output, 
      session = session,
      shared_reactive_vals = list(data = shared_data)
    )
  })
}
```
