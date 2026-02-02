# New Tab Implementation Guide

This guide explains how to add a new tab to the TexAn 2.0 Shiny app with consistent UI/UX.

## File Structure Overview

```
R/
├── ui/modules/pages/
│   └── ui_{tabname}.R              # Main UI module
├── server/modules/pages/
│   ├── server_{tabname}.R          # Main server module
│   └── {tabname}/                   # Tab-specific components
│       ├── ui_tab_{section1}.R     # Sidebar tab 1 UI
│       ├── ui_tab_{section2}.R     # Sidebar tab 2 UI
│       └── ...                      # Additional logic files
app.R                                # Main app (add sources + nav_panel)
www/css/styles.css                   # Add sidebar class selectors
```

## Implementation Order

### Step 1: Create UI Module

**File:** `R/ui/modules/pages/ui_{tabname}.R`

```r
#' UI for the {TabName} page
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
UI_{tabname} <- function(id) {
    ns <- shiny::NS(id)
    
    # Source UI tab components
    source("R/server/modules/pages/{tabname}/ui_tab_{section1}.R", local = TRUE)
    source("R/server/modules/pages/{tabname}/ui_tab_{section2}.R", local = TRUE)

    shiny::tagList(
        # Optional: Initialize window size reporting
        shiny::tags$script(shiny::HTML(sprintf(
            "$(document).on('shiny:connected', function() { initializeWindowSize('%s', '%s'); });",
            ns("{tabname}_results"),
            ns("windowSize{TabName}")
        ))),
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = NULL,
                class = "{tabname}-sidebar",  # IMPORTANT: Used for CSS styling
                
                # Sidebar tabs with icons
                bslib::navset_tab(
                    id = ns("sidebar_tabs"),
                    create_{tabname}_{section1}_tab(ns),
                    create_{tabname}_{section2}_tab(ns)
                ),
                
                # Action button at bottom (always visible)
                shiny::tags$hr(),
                shiny::actionButton(
                    inputId = ns("action_button"),
                    label = "Run Action",
                    class = "btn-primary btn-sm w-100"
                )
            ),

            # Main content area
            shiny::uiOutput(ns("{tabname}_results"))
        )
    )
}
```

### Step 2: Create Sidebar Tab Components

**Directory:** `R/server/modules/pages/{tabname}/`

**File:** `ui_tab_{section1}.R`

```r
#' {Section1} Tab UI Component
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
create_{tabname}_{section1}_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("icon-name", size = "1.2em"),
            "Section 1 Tooltip"
        ),
        value = "section1_tab",
        shiny::tags$div(
            class = "pt-3",  # Consistent padding
            shiny::h6(class = "text-muted mb-3", "Section 1 Title"),
            # Add inputs here
            shiny::selectizeInput(
                inputId = ns("input1"),
                label = shiny::tags$span(
                    "Input Label ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Help text for this input."
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select...")
            )
        )
    )
}
```

### Step 3: Create Server Module

**File:** `R/server/modules/pages/server_{tabname}.R`

```r
#' Server module for {TabName} page
#'
#' @param id Module namespace ID
#' @param input_data Reactive data from upstream module
#' @param data_version Reactive integer for state reset
#' @return NULL or list of reactive outputs for downstream modules
server_{tabname} <- function(id, input_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Reactive state
        state <- shiny::reactiveValues(
            result = NULL
        )
        
        # Reset on new data
        shiny::observeEvent(data_version(), {
            state$result <- NULL
        }, ignoreInit = TRUE)
        
        # Action button handler
        shiny::observeEvent(input$action_button, {
            # Implementation here
        })
        
        # Render main output
        output${tabname}_results <- shiny::renderUI({
            shiny::div(
                class = "d-flex align-items-center justify-content-center h-100",
                style = "min-height: 400px;",
                shiny::p("Results will appear here.")
            )
        })
        
        invisible(NULL)
    })
}
```

### Step 4: Update app.R

Add these changes to `app.R`:

```r
# 1. Add UI source (after other UI sources)
source("R/ui/modules/pages/ui_{tabname}.R")

# 2. Add server source (after other server sources)
source("R/server/modules/pages/server_{tabname}.R")

# 3. Add nav_panel in app_ui (use appropriate bsicons icon)
bslib::nav_panel(
    title = shiny::tagList(bsicons::bs_icon("icon-name"), "{TabName}"),
    value = "{tabname}",
    UI_{tabname}("{tabname}_id")
),

# 4. Add server call in app_server
server_{tabname}("{tabname}_id",
                 input_data = upstream_result,
                 data_version = load_data_result$version)

# 5. Add to data_dependent_tabs if tab requires loaded data
data_dependent_tabs <- c("median", "plotting", "summary_stats", "{tabname}")
```

### Step 5: Update CSS (styles.css)

Add the new sidebar class to ALL relevant CSS selectors:

#### 5.1 Gap Spacing (around line 75)
```css
.tab-pane[data-value="plotting"].bslib-gap-spacing,
.tab-pane[data-value="statistics"].bslib-gap-spacing,
.tab-pane[data-value="{tabname}"].bslib-gap-spacing {
    gap: 0 !important;
    padding: 0 !important;
    margin: 0 !important;
}
```

#### 5.2 Layout Margin (around line 84)
```css
.tab-pane[data-value="plotting"]>.bslib-sidebar-layout,
.tab-pane[data-value="statistics"]>.bslib-sidebar-layout,
.tab-pane[data-value="{tabname}"]>.bslib-sidebar-layout {
    margin: 0 !important;
}
```

#### 5.3 Nav-tabs Positioning (around line 93)
```css
.plotting-sidebar .nav-tabs,
.statistics-sidebar .nav-tabs,
.{tabname}-sidebar .nav-tabs {
    margin: -0.25rem -1rem 0.5rem -1rem;
    padding: 0.25rem 1rem 0;
    border-bottom: 1px solid var(--bs-border-color);
    background: var(--bs-tertiary-bg);
}
```

#### 5.4 Nav-item Distribution (around line 103)
```css
.plotting-sidebar .nav-tabs .nav-item,
.statistics-sidebar .nav-tabs .nav-item,
.{tabname}-sidebar .nav-tabs .nav-item {
    flex: 1;
    text-align: center;
}
```

#### 5.5 Nav-link Styling (around line 111)
Add `.{tabname}-sidebar` to ALL nav-link selectors:
- `.sidebar-content .{tabname}-sidebar .nav-tabs .nav-link`
- `.{tabname}-sidebar .nav-tabs .nav-link`
- `.{tabname}-sidebar .nav-tabs .nav-link.active`
- `.{tabname}-sidebar .nav-tabs .nav-link:hover:not(.active)`
- `.{tabname}-sidebar .nav-tabs .nav-link svg`

#### 5.6 Overflow Visible (around line 239)
```css
.{tabname}-sidebar,
.{tabname}-sidebar .accordion,
.{tabname}-sidebar .accordion-item,
.{tabname}-sidebar .accordion-body,
.{tabname}-sidebar .accordion-collapse,
.{tabname}-sidebar .tab-content,
.{tabname}-sidebar .tab-pane,
```

## Checklist

- [ ] Create `R/ui/modules/pages/ui_{tabname}.R`
- [ ] Create `R/server/modules/pages/{tabname}/` directory
- [ ] Create sidebar tab UI files in the directory
- [ ] Create `R/server/modules/pages/server_{tabname}.R`
- [ ] Add UI source to `app.R`
- [ ] Add server source to `app.R`
- [ ] Add `nav_panel` to `app_ui`
- [ ] Add server call to `app_server`
- [ ] Add to `data_dependent_tabs` if needed
- [ ] Update CSS: gap spacing selector
- [ ] Update CSS: layout margin selector
- [ ] Update CSS: nav-tabs positioning
- [ ] Update CSS: nav-item distribution
- [ ] Update CSS: nav-link styling (5 selectors)
- [ ] Update CSS: overflow visible
- [ ] Test: Hard refresh browser (Ctrl+Shift+R)
- [ ] Test: Verify sidebar alignment matches other tabs

## Icons Reference

Common bsicons for tabs:
- `bar-chart-steps` - Analysis/PCA
- `graph-up` - Plotting
- `table` - Data/Tables
- `calculator` - Calculations
- `funnel` - Filtering
- `sliders` - Settings/Processing
- `palette` - Styling
- `gear` - Options
- `question-circle` - Help

Browse all icons: https://icons.getbootstrap.com/
