
# Custom instructions for LLM tools

## Importing and exporting

Use only `box::use` for imports. Using `library` and `::` is forbidden.

`box::use` statement (if needed) should be located at the top of the file.

There can be two `box::use` statements per file. First one should include only R packages, second should only import other scripts.

Imports in `box::use` should be sorted alphabetically.

Using `[...]` is forbidden.

All external functions in a script should be imported. This includes operators, like `%>%`.

A script should only import functions that it uses.

## Importing Modules and Packages

### Ways of importing

**ALWAYS use the `$` approach for explicit function origin and debugging clarity:**

```r
# First: R packages only
box::use(
  dplyr,
  shiny,
  ggplot2
)

# Second: Custom app modules only  
box::use(
  app/logic/utils,
  app/view/components,
  app/static/styles
)

# Usage:
dplyr$filter(mtcars, cyl > 4)
shiny$moduleServer(id, ...)
app/logic/utils$my_function()
```

**Benefits of this approach:**

- `dplyr$filter()` → clearly from dplyr package
- `app/logic/utils$my_function()` → clearly from your utils module
- Stack traces show the full origin path
- Consistent debugging visibility across entire codebase

### Exporting

If a function is used only inside a script, it should not be exported.

If a function is used by other scripts, it should be exported by adding `#' @export` before the function.

## Rhino modules

When creating a new module in `app/view`, use the template:

```r
box::use(
  shiny[moduleServer, NS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
```

## Unit tests

All R unit tests are located in `tests/testthat`.

There should be only one test file per script, named `test-{script name}.R`.

If testing private functions (ones that are not exported), use this pattern:

```r
box::use(app/logic/mymod)

impl <- attr(mymod, "namespace")

test_that('{test description}', {
    expect_true(impl$this_works())
})
```

### Testing exported and non-exported functions

When testing a box module that contains both exported and non-exported functions:

1. Import the entire module without specifying individual functions:

```r
box::use(
  app/logic/mymodule,
)
```

2. Access exported functions using the module name with `$`:

```r
test_that("exported function works", {
  expect_equal(mymodule$exported_function(1), 2)
})
```

3. For testing non-exported functions, get the module's namespace at the start of the test file:

```r
impl <- attr(mymodule, "namespace")

test_that("non-exported function works", {
  expect_equal(impl$internal_function(1), 2)
})
```

This pattern allows testing both public and private functions while maintaining proper encapsulation.

## Logging

Use `rhino$log` for all application logging. Import `rhino` via `box::use(rhino)`.

### Log levels

| Level | Usage |
|-------|-------|
| `rhino$log$debug()` | Detailed tracing during development (variable values, flow) |
| `rhino$log$info()` | Normal operations (data loaded, module initialized) |
| `rhino$log$warn()` | Unexpected but recoverable (bad input, fallback used) |
| `rhino$log$error()` | Operation failed, user is affected |
| `rhino$log$fatal()` | App cannot continue |

### Where to log

- **Logic layer (`app/logic/`)**: Log outcomes of pure functions — success at `INFO`, failures at `WARN` or `ERROR`.
- **View layer (`app/view/`)**: Log lifecycle events — upload received, module initialized, user action triggered.
- **Never log sensitive data** (passwords, tokens, PII).

### Message format

Use glue-style interpolation (built into `rhino$log`):

```r
rhino$log$info("File read successfully: {ext} ({nrow(data)} rows, {ncol(data)} cols)")
rhino$log$warn("Unsupported file extension: '{ext}' from '{filename}'")
rhino$log$error("File read failed: {conditionMessage(e)}")
```

### Configuration

Logging is configured in `config.yml`:

```yaml
default:
  rhino_log_level: !expr Sys.getenv("RHINO_LOG_LEVEL", "INFO")
  rhino_log_file: !expr Sys.getenv("RHINO_LOG_FILE", NA)
```

- **Development**: `RHINO_LOG_LEVEL=INFO` (default), logs to console.
- **Production**: Set `RHINO_LOG_FILE=app.log` and `RHINO_LOG_LEVEL=WARN` as environment variables.
- `app.log` is excluded from version control via `.gitignore`.

## Error handling

Use the two-layer error handling pattern defined in `.llm/error_handling.md`:

- **Logic layer** (`app/logic/error_handling.R`): `safe_execute()`, `simple_error()`, `is_app_error()`, parsers
- **View layer** (`app/view/error_display.R`): `error_alert_structured()`, `render_app_error()`, modals

Wrap risky operations in `error_handling$safe_execute()`. Use `error_handling$simple_error()` for validation failures. Display errors in the UI via `error_display$error_alert_structured()`.

## Code style

The maximum line length is 100 characters.
