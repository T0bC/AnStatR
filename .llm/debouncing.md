# Unified Debouncing Pattern

## Pattern Overview

Consolidate multiple related inputs into a **single debounced reactive** to prevent double-renders when inputs change at different times.

## The Problem

When multiple inputs affect the same output (e.g., a table), debouncing them separately causes staggered invalidations:

```r
# BAD: Separate debounces = double render
input_a <- reactive({ input$a }) |> debounce(500)
input_b <- reactive({ input$b })  # No debounce

observe({
    # Fires twice: once for input_b, again 500ms later for input_a
    result <- compute(input_a(), input_b())
})
```

## Solution: Unified Parameters Reactive

Bundle all related inputs into one reactive with a single debounce:

### Implementation File (`app/logic/debouncing.R`)

```r
box::use(
  shiny[reactiveVal, observe, reactive, debounce]
)

#' Create a unified debounced reactive for multiple inputs
#' @param input Shiny input object from parent module
#' @param debounce_ms Debounce delay in milliseconds
#' @return A reactive function that returns consolidated parameters
#' @export
create_module_params <- function(input, debounce_ms = 400) {
    cached_params <- reactiveVal(NULL)
    
    # Fingerprint for change detection
    make_fingerprint <- function(params) {
        paste(params$input_a, params$input_b, sep = "|")
    }
    
    observe({
        new_params <- list(
            input_a = input$a,
            input_b = input$b
        )
        
        current <- cached_params()
        if (make_fingerprint(new_params) != make_fingerprint(current)) {
            cached_params(new_params)
        }
    }) |> debounce(debounce_ms)
    
    reactive({ cached_params() })
}
```

## Usage in Parent Module

### Parent Module (`app/logic/parent_module.R`)

```r
box::use(
  shiny[moduleServer, reactive, observe, req],
  app/logic/debouncing[create_module_params]
)

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Create unified params
    module_params <- create_module_params(input, debounce_ms = 400)

    # Extract individual values if needed downstream
    input_a <- reactive({ module_params()$input_a })

    # Downstream observers depend on unified params
    observe({
      req(module_params())
      # Single render after all inputs stabilize
      # Use module_params()$input_a, module_params()$input_b, etc.
    })
  })
}
```

## Key Elements

1. **Single observer** collects all related inputs
2. **Fingerprint comparison** prevents updates when values haven't changed
3. **Single debounce** on the observer ensures all inputs settle before triggering downstream
4. **Cached reactiveVal** stores the consolidated parameters

## When to Apply

Use when a module has multiple inputs that all affect the same output (tables, plots, computations) and you observe double-renders or slow UI response.

---

## Box Import Guidelines for Debouncing

### In Logic Files (app/logic/)

```r
box::use(
  shiny[reactiveVal, observe, reactive, debounce, req]
)
```

### In View Files (app/view/)

```r
box::use(
  shiny[NS, moduleServer, reactive, observe],
  app/logic/debouncing[create_module_params]
)
```

### Custom Debouncing Patterns

```r
# app/logic/custom_debounce.R
box::use(
  shiny[reactiveVal, observe, reactive, debounce]
)

#' Create custom debounced reactive for specific use case
#' @param input Shiny input object
#' @param input_names Vector of input names to consolidate
#' @param debounce_ms Debounce delay in milliseconds
#' @return A reactive function that returns consolidated parameters
#' @export
create_custom_params <- function(input, input_names, debounce_ms = 400) {
  cached_params <- reactiveVal(NULL)
  
  make_fingerprint <- function(params) {
    # Custom fingerprint logic for your inputs
    paste(purrr::map_chr(input_names, ~ params[[.x]]), collapse = "|")
  }
  
  observe({
    new_params <- purrr::map(input_names, ~ input[[.x]])
    names(new_params) <- input_names
    
    current <- cached_params()
    if (make_fingerprint(new_params) != make_fingerprint(current)) {
      cached_params(new_params)
    }
  }) |> debounce(debounce_ms)
  
  reactive({ cached_params() })
}
```
