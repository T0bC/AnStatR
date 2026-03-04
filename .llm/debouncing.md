# Debouncing & Cascading Input Updates

## Critical Rule

`shiny$debounce()` only works on **reactives**, never on observers.

```r
# BROKEN — debounce is silently ignored, observer fires immediately
shiny$observe({ ... }) |> shiny$debounce(300)

# CORRECT — reactive is debounced, observer reads the debounced result
debounced <- shiny$reactive({ ... }) |> shiny$debounce(300)
shiny$observe({ debounced() })
```

---

## Pattern: Debounced Computation Pipeline

Use when multiple inputs feed an expensive computation (plot, table, PCA, etc.).

```r
# 1. Cache layer
cached_params <- shiny$reactiveVal(NULL)

# 2. Fingerprint — include ALL values that should trigger a recompute.
#    Use actual values, not just length(). E.g. for colors:
#    paste(params$color_map, collapse = ",")  NOT  length(params$color_map)
make_fingerprint <- function(params) {
  paste(params$a, paste(params$b, collapse = ","), sep = "|")
}

# 3. Debounced reactive — collects all inputs into a snapshot
debounced_snapshot <- shiny$reactive({
  list(a = input$a, b = input$b)
}) |> shiny$debounce(400)

# 4. Observer — propagates only on fingerprint change
shiny$observe({
  snap <- debounced_snapshot()
  shiny$req(snap)
  old_fp <- if (!is.null(cached_params())) make_fingerprint(cached_params()) else ""
  if (make_fingerprint(snap) != old_fp) {
    cached_params(snap)
  }
})

# 5. Downstream reads from cache (single invalidation point)
output$result <- renderPlot({ req(cached_params()); ... })
```

---

## Pattern: Cascading `updateSelectizeInput` Without Killing Dropdowns

`updateSelectizeInput` rebuilds the widget on the client, which **closes any open dropdown**. Avoid calling it on the widget the user is currently interacting with.

**Rules:**

1. **Never re-update a selectize from an observer that depends on that same input.**
   The `selected = input$foo[...]` pattern creates a reactive dep on `input$foo`,
   causing the observer to fire on every user selection → dropdown closes.

2. **Debounce cascading updates** (e.g. metaData → xAxis choices) so they don't
   fire mid-interaction:

```r
debounced_parent <- shiny$reactive({
  m <- input$parentSelect
  if (is.null(m)) character(0) else m
}) |> shiny$debounce(500)

shiny$observe({
  choices <- debounced_parent()
  cur <- shiny$isolate(input$childSelect)           # isolate! no reactive dep
  shiny$updateSelectizeInput(session, "childSelect",
    choices = choices, selected = cur[cur %in% choices]
  )
})
```

3. **Use `isolate()` when reading the child input's current value** inside the
   observer that updates it, to avoid creating a circular reactive dependency.

4. **Data-load updates** belong in a single `observeEvent(data_version(), ...)`
   with `ignoreInit = TRUE`. Don't duplicate them in a generic `observe()` on
   `input_data()`.

---

## JS: Selectize Dropdown Persistence

Even with server-side fixes, `conditionalPanel` DOM mutations can steal focus
from selectize controls. Patch `Selectize.prototype.close` to prevent closing
multi-select dropdowns while the control is focused:

```js
var origClose = Selectize.prototype.close;
Selectize.prototype.close = function () {
  if (this.settings.maxItems !== 1 && this.isFocused) return;
  return origClose.apply(this, arguments);
};
```

Also patch `addItem` to reopen after each selection (with a small delay to
survive the `shiny:busy` lock cycle):

```js
var origAddItem = Selectize.prototype.addItem;
Selectize.prototype.addItem = function (value, silent) {
  var result = origAddItem.apply(this, arguments);
  if (this.settings.maxItems !== 1) {
    var self = this;
    setTimeout(function () { self.open(); self.$control_input[0].focus(); }, 20);
  }
  return result;
};
```
