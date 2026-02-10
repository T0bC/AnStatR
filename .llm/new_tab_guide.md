# How to Add a New Tab Module

This guide explains how to scaffold a new tab in the TexAn 2.0 Rhino app.
Every tab follows the same pattern: a left sidebar with icon-only navset tabs,
and a right main content area with accordion panels or dynamic cards.

Template files are in `.llm/new_tab_templates/`.

## Placeholders

Replace these in all template files:

| Placeholder | Example | Description |
|-------------|---------|-------------|
| `{tab_name}` | `summary` | Lowercase, underscores for file names |
| `{TabName}` | `Summary` | TitleCase for display and function references |
| `{tab_icon}` | `table` | Bootstrap icon name for the navbar tab |

## Files to Create

For a new tab called `{tab_name}`, create these files:

```
app/logic/{tab_name}.R            <- Pure logic (no Shiny)
app/view/{tab_name}.R             <- UI + server module
tests/testthat/test-{tab_name}.R  <- Tests for logic layer
docs/help/{tab_name}.md           <- Offcanvas help content
```

## Step-by-Step

### 1. Create the logic module

Copy `.llm/new_tab_templates/logic_template.R` to `app/logic/{tab_name}.R`.

- Add pure R functions (computations, validations, transformations)
- Wrap risky operations in `error_handling$safe_execute()`
- Use `rhino$log` for logging outcomes
- Export functions with `#' @export`
- Use `app/logic/column_utils` for `get_descriptive_cols()` / `get_measurement_cols()`
- Keep Shiny-free — no `shiny$` calls allowed in logic files

### 2. Create the view module

Copy `.llm/new_tab_templates/view_template.R` to `app/view/{tab_name}.R`.

The template uses `sidebar_tabs$tab_layout()` which provides:
- Unified `.texan-sidebar` CSS class (no per-tab CSS needed)
- Icon-only navset tabs in the sidebar
- Optional action button below tabs
- Optional responsive plot JS (`enable_responsive_plots = TRUE`)
- Main content area

Key decisions per tab:
- **How many sidebar tabs?** Each is a `sidebar_tabs$create_tab()` call.
  Single-tab modules are fine (e.g. Summary has only one config tab).
- **Action button?** Pass `action_button = shiny$actionButton(...)` to `tab_layout()`
- **Responsive plots?** Set `enable_responsive_plots = TRUE, results_id = "main_content"`
- **Dynamic outputs?** Use `shiny$uiOutput(ns("main_content"))` as `main_content`
  and render cards/tables dynamically in `output$main_content`
- **Download buttons?** Use `shiny$downloadButton()` in sidebar,
  `shiny$downloadHandler()` in server. For XLSX: `openxlsx$write.xlsx()` or
  multi-sheet via `openxlsx$createWorkbook()` / `addWorksheet()` / `writeData()`

### 3. Create help documentation

Create `docs/help/{tab_name}.md` with markdown content for the offcanvas help
sidebar. The `help_modal` module automatically loads this file based on the
active tab's `value` attribute. The file name must match the tab value exactly.

### 4. Create tests

Copy `.llm/new_tab_templates/test_template.R` to `tests/testthat/test-{tab_name}.R`.

- Test pure logic functions only (no Shiny)
- Use `describe()` / `it()` blocks
- Access private (non-exported) functions via `impl <- attr(module, "namespace")`
- Create helper functions (e.g. `make_test_data()`) for reusable test fixtures

### 5. Wire into app/main.R

Add three things to `app/main.R`:

```r
# 1. Import (in second box::use block, alphabetical)
box::use(
  ...
  app/view/{tab_name},
)

# 2. Add nav_panel in UI (before nav_spacer)
bslib$nav_panel(
  title = shiny$tagList(
    bsicons$bs_icon("{tab_icon}"), "{TabName}"
  ),
  value = "{tab_name}",
  {tab_name}$ui(ns("{tab_name}"))
),

# 3. Call server (pass upstream data + any cross-module reactives)
{tab_name}$server("{tab_name}",
  input_data = ...,
  data_version = load_data_result$version
)
```

### 6. Configure tab visibility

Add the tab to the appropriate visibility tier in `app/main.R` server:

```r
# Tier 1: visible when data is loaded
shiny$observe({
  has_data <- !is.null(load_data_result$data())
  toggle <- if (has_data) bslib$nav_show else bslib$nav_hide
  toggle("active_page", target = "median")
  toggle("active_page", target = "plotting")
})

# Tier 2: visible when upstream selections exist (e.g. x_axis selected)
shiny$observe({
  x_axis <- plotting_result$x_axis()
  has_x <- !is.null(x_axis) && length(x_axis) > 0
  toggle <- if (has_x) bslib$nav_show else bslib$nav_hide
  toggle("active_page", target = "summary")
})
```

Current tiers:
- **Always visible**: Load Data
- **Tier 1** (data loaded): Median, Plotting
- **Tier 2** (plotting x_axis selected): Summary

For tabs that should be grayed out with a lock icon instead of fully hidden,
use the JS `tab_disabled_state` handler (see Disabled Tabs section below).

### 7. Install dependencies

If the new tab needs packages not yet in `dependencies.R`:

```r
rhino::pkg_install("package_name")
# Then add library(package_name) to dependencies.R
```

### 8. Verify

```r
rhino::build_sass()
rhino::test_r()
```

## CSS

No per-tab CSS is needed. All sidebar tab styling uses the shared `.texan-sidebar`
class defined in `app/styles/main.scss`. If you need to adjust margins, spacing,
or tab appearance, change it once in `main.scss` and it applies to all tabs.

## Responsive Plots

For tabs with interactive plots (ggiraph, plotly):

1. Set `enable_responsive_plots = TRUE` in `tab_layout()`
2. Use `.plot-card` / `.plot-card-body` / `.responsive-plot` CSS classes
3. Access window dimensions via `input$windowSize` in the server

## Disabled Tabs

For tabs that should appear grayed out with a lock icon (instead of hidden),
use the generic JS handler in `app/js/index.js`:

```r
# In app/main.R server:
session$sendCustomMessage("tab_disabled_state", list(
  tab     = "{tab_name}",
  enabled = TRUE/FALSE,
  reason  = "Explain what the user needs to do first."
))
```

CSS class `.disabled-tab` in `main.scss` provides: 50% opacity, lock emoji,
cursor: not-allowed, no hover effects. Clicking a disabled tab shows a modal
with the reason text.

## Data Flow

Upstream modules return reactive lists. Downstream modules receive them:

```
load_data$server()
  -> list(data, version)
      |
      v
median$server(input_data, data_version)
  -> reactive (median-processed data or NULL)
      |
      v
plotting_data <- reactive({ median_result() %||% load_data_result$data() })
      |
      v
plotting$server(input_data = plotting_data, data_version)
  -> list(x_axis, measure_cols)   # expose selections for downstream
      |
      v
summary$server(input_data = plotting_data, data_version,
               plotting_x_axis, plotting_measures)
```

### Returning reactives for downstream modules

If a module's selections are needed by other modules, return them from the
server function instead of `invisible(NULL)`:

```r
# In the module server:
list(
  x_axis = shiny$reactive({ input$xAxis }),
  measure_cols = shiny$reactive({ input$measureVar })
)
```

Then capture the return in `main.R` and pass to downstream modules.

### Accepting upstream selections

Downstream modules should accept optional reactive params with `NULL` defaults
for graceful fallback:

```r
server <- function(id, input_data, data_version,
                   plotting_x_axis = NULL,
                   plotting_measures = NULL) {
```

Use upstream values as defaults but allow the user to override locally.

Always reset module state when `data_version()` changes (new data loaded).

## Debounced Computation Pattern

For tabs that compute results reactively based on multiple inputs, use
debouncing to avoid excessive recomputation:

```r
debounced_inputs <- shiny$reactive({
  shiny$req(input_data())
  shiny$req(input$some_selection)
  list(
    param1 = input$some_selection,
    param2 = input$another_input %||% FALSE
  )
}) |> shiny$debounce(400)

shiny$observeEvent(debounced_inputs(), {
  params <- debounced_inputs()
  data <- shiny$isolate(input_data())
  # ... run computation ...
}, ignoreNULL = TRUE, ignoreInit = FALSE)
```

## Dynamic Table Outputs

For tabs that render a variable number of DT tables (one per measurement):

1. Store results as a list of `list(col, df)` in a `reactiveVal`
2. Use `shiny$observe()` with `local()` to create per-item `output[[id]]`
   bindings for both `DT$renderDataTable` and `shiny$downloadHandler`
3. Render the UI cards in `output$main_content` with matching output IDs

```r
shiny$observe({
  summaries <- summary_dfs()
  shiny$req(summaries)
  lapply(summaries, function(item) {
    local({
      local_item <- item
      output[[paste0("table_", local_item$col)]] <- DT$renderDataTable({ ... })
      output[[paste0("dl_", local_item$col)]] <- shiny$downloadHandler(...)
    })
  })
})
```
