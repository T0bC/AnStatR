# How to Add a New Tab Module

This guide explains how to scaffold a new tab in the TexAn 2.0 Rhino app.
Every tab follows the same pattern: a left sidebar with icon-only navset tabs,
and a right main content area with accordion panels.

Template files are in `.llm/new_tab_templates/`.

## Placeholders

Replace these in all template files:

| Placeholder | Example | Description |
|-------------|---------|-------------|
| `{tab_name}` | `plotting` | Lowercase, underscores for file names |
| `{TabName}` | `Plotting` | TitleCase for display and function references |
| `{tab_icon}` | `graph-up` | Bootstrap icon name for the navbar tab |

## Files to Create

For a new tab called `{tab_name}`, create these files:

```
app/logic/{tab_name}.R          <- Pure logic (no Shiny)
app/view/{tab_name}.R           <- UI + server module
tests/testthat/test-{tab_name}.R <- Tests for logic layer
```

## Step-by-Step

### 1. Create the logic module

Copy `.llm/new_tab_templates/logic_template.R` to `app/logic/{tab_name}.R`.

- Add pure R functions (computations, validations, transformations)
- Wrap risky operations in `error_handling$safe_execute()`
- Use `rhino$log` for logging outcomes
- Export functions with `#' @export`

### 2. Create the view module

Copy `.llm/new_tab_templates/view_template.R` to `app/view/{tab_name}.R`.

The template uses `sidebar_tabs$tab_layout()` which provides:
- Unified `.texan-sidebar` CSS class (no per-tab CSS needed)
- Icon-only navset tabs in the sidebar
- Optional action button below tabs
- Optional responsive plot JS (`enable_responsive_plots = TRUE`)
- Main content area

Key decisions per tab:
- **How many sidebar tabs?** Each is a `sidebar_tabs$create_tab()` call
- **Action button?** Pass `action_button = shiny$actionButton(...)` to `tab_layout()`
- **Responsive plots?** Set `enable_responsive_plots = TRUE, results_id = "main_content"`
- **Accordion panels?** Add them in the `renderUI` for main content

### 3. Create tests

Copy `.llm/new_tab_templates/test_template.R` to `tests/testthat/test-{tab_name}.R`.

- Test pure logic functions only (no Shiny)
- Use `describe()` / `it()` blocks
- Access private functions via `attr(module, "namespace")`

### 4. Wire into app/main.R

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

# 3. Call server (pass upstream data if needed)
{tab_name}$server("{tab_name}", input_data = load_data_result$data,
                   data_version = load_data_result$version)
```

### 5. Install dependencies

If the new tab needs packages not yet in `dependencies.R`:

```r
rhino::pkg_install("package_name")
# Then add library(package_name) to dependencies.R
```

### 6. Verify

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

## Data Flow

Upstream modules return reactive lists. Downstream modules receive them:

```
load_data$server() -> list(data, version)
    |
    v
plotting$server("plotting", input_data = load_data_result$data,
                data_version = load_data_result$version)
```

Always reset module state when `data_version()` changes (new data loaded).
