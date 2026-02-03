# TexAn 2.0 Codebase Structure

## Directory Layout

```
R/
├── server/{module}/          # Server logic per module
│   ├── server_{module}.R     # Main server module
│   └── *.R                   # Sub-components (sourced locally)
├── ui/{module}/              # UI definitions per module
│   ├── ui_{module}.R         # Main UI module
│   └── ui_tab_*.R            # Sidebar tab components
├── ui/components/            # Shared UI components
│   ├── error_display.R
│   └── settings_modal.R
└── utils/                    # Shared utilities (unchanged)
    └── statistics/           # Statistics-specific utils
```

## Module Structure

### Server Module (`R/server/{module}/server_{module}.R`)
- Entry point: `server_{module}(id, ...reactive_inputs...)`
- Sources sub-components with `source(..., local = TRUE)`
- Returns `NULL` or `list(reactive1 = ..., reactive2 = ...)` for downstream

### UI Module (`R/ui/{module}/ui_{module}.R`)
- Entry point: `UI_{module}(id)`
- Sources sidebar tab components with `source(..., local = TRUE)`
- Returns `bslib::layout_sidebar()` with `sidebar` and main content area

### Sidebar Tabs (`R/ui/{module}/ui_tab_*.R`)
- Function: `create_{module}_{section}_tab(ns)`
- Returns `bslib::nav_panel()` with icon tooltip and inputs

## Source Path Conventions

| Type | Path Pattern |
|------|--------------|
| Main UI | `R/ui/{module}/ui_{module}.R` |
| UI tabs | `R/ui/{module}/ui_tab_*.R` |
| Main server | `R/server/{module}/server_{module}.R` |
| Server logic | `R/server/{module}/*.R` |
| Components | `R/ui/components/*.R` |
| Utils | `R/utils/*.R` |

## app.R Source Order

```r
# 1. Utils
source("R/utils/error_handling.R")
source("R/utils/column_utils.R")
# ...

# 2. UI modules
source("R/ui/load_data/ui_load_data.R")
source("R/ui/median/ui_median.R")
# ...

# 3. Components
source("R/ui/components/settings_modal.R")
source("R/ui/components/error_display.R")

# 4. Server modules
source("R/server/load_data/server_load_data.R")
source("R/server/median/server_median.R")
# ...

# 5. Server sub-modules (sourced in app.R, not locally)
source("R/server/load_data/file_upload.R")
source("R/server/median/help_modal.R")
source("R/ui/median/grouping_ui.R")  # UI files in ui/ even if used by server
# ...
```

## Data Flow

```
load_data → median → plotting → summary_stats
                  ↘         ↘
                   pca       statistics
```

Each module receives:
- `data` or `processed_data`: Reactive data from upstream
- `data_version`: Reactive integer for state reset on new data

## Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Module ID | `{module}_id` | `"plotting_id"` |
| Server function | `server_{module}` | `server_plotting` |
| UI function | `UI_{module}` | `UI_plotting` |
| Tab creator | `create_{module}_{section}_tab` | `create_plotting_data_selection_tab` |
| Sidebar class | `{module}-sidebar` | `plotting-sidebar` |
| Nav panel value | `{module}` | `"plotting"` |

## Adding a New Module

1. Create `R/server/{module}/server_{module}.R`
2. Create `R/ui/{module}/ui_{module}.R`
3. Create `R/ui/{module}/ui_tab_*.R` for sidebar tabs
4. Add sources to `app.R`
5. Add `nav_panel` to `app_ui`
6. Add server call to `app_server`
7. Update `www/css/styles.css` with `.{module}-sidebar` selectors

See `docs/architecture/new_tab_implementation.md` for detailed guide.
