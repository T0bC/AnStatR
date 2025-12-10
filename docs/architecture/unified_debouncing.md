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

```r
create_module_params <- function(input, debounce_ms = 400) {
    cached_params <- shiny::reactiveVal(NULL)
    
    # Fingerprint for change detection
    make_fingerprint <- function(params) {
        paste(params$input_a, params$input_b, sep = "|")
    }
    
    shiny::observe({
        new_params <- list(
            input_a = input$a,
            input_b = input$b
        )
        
        current <- cached_params()
        if (make_fingerprint(new_params) != make_fingerprint(current)) {
            cached_params(new_params)
        }
    }) |> shiny::debounce(debounce_ms)
    
    shiny::reactive({ cached_params() })
}
```

## Usage in Parent Module

```r
# Create unified params
module_params <- create_module_params(input, debounce_ms = 400)

# Extract individual values if needed downstream
input_a <- reactive({ module_params()$input_a })

# Downstream observers depend on unified params
observe({
    req(module_params())
    # Single render after all inputs stabilize
})
```

## Key Elements

1. **Single observer** collects all related inputs
2. **Fingerprint comparison** prevents updates when values haven't changed
3. **Single debounce** on the observer ensures all inputs settle before triggering downstream
4. **Cached reactiveVal** stores the consolidated parameters

## When to Apply

Use when a module has multiple inputs that all affect the same output (tables, plots, computations) and you observe double-renders or slow UI response.
