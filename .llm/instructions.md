
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

## Code style

The maximum line length is 100 characters.
