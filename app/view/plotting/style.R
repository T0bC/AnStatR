box::use(
  bsicons,
  bslib,
  colourpicker,
  rhino,
  shiny,
  sortable,
)

box::use(
  app/logic/shared/data_utils,
  app/logic/plotting/plot_factory,
  app/view/components/sidebar_tabs,
)

#' Build the style sidebar tab UI
#' @param ns Namespace function from the parent module
#' @return A sidebar tab created via sidebar_tabs$create_tab()
#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "palette",
    tooltip_text = "Plot Style",
    value = "style_tab",
    shiny$h6(class = "text-muted mb-3", "Plot Style"),
    bslib$accordion(
      id = ns("style_accordion"),
      open = "points",
      points_panel(ns),
      boxplot_panel(ns),
      violin_panel(ns),
      legend_grid_panel(ns),
      median_sd_panel(ns),
      axis_panel(ns),
      colors_panel(ns),
      export_panel(ns)
    )
  )
}

#' Server logic for the style tab
#'
#' Manages:
#' - pointShape choices (from metaData)
#' - pointColor choices (from xAxis)
#' - Dynamic nested tree with sortable factor levels and color pickers
#' - Custom color map and factor order reactives
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @return List with color_map and factor_order reactives
#' @export
tab_server <- function(input, output, session, input_data,
                       data_version) {
  ns <- session$ns

  # Persistent state for factor ordering (per column)
  saved_factor_order <- shiny$reactiveVal(list())

  # Update pointShape choices from metaData (debounced)
  debounced_meta <- shiny$reactive({
    m <- input$metaData
    if (is.null(m)) character(0) else m
  }) |> shiny$debounce(500)

  shiny$observe({
    selected_meta <- debounced_meta()
    cur_shape <- shiny$isolate(input$pointShape)

    shiny$updateSelectizeInput(
      session, "pointShape",
      choices = selected_meta,
      selected = cur_shape[cur_shape %in% selected_meta]
    )
  })

  # Update pointColor choices from xAxis (debounced)
  debounced_xaxis <- shiny$reactive({
    xa <- input$xAxis
    if (is.null(xa)) character(0) else xa
  }) |> shiny$debounce(500)

  shiny$observe({
    x_axis <- debounced_xaxis()
    if (length(x_axis) == 0) {
      shiny$updateSelectizeInput(
        session, "pointColor", choices = character(0)
      )
    } else {
      current <- shiny$isolate(input$pointColor)
      valid <- current[current %in% x_axis]
      shiny$updateSelectizeInput(
        session, "pointColor",
        choices = x_axis,
        selected = valid
      )
    }
  })

  # Color columns: pointColor if user explicitly picked subset,
  # otherwise all xAxis columns
  color_cols <- shiny$reactive({
    xa <- input$xAxis
    if (is.null(xa) || length(xa) == 0) return(character(0))
    pc <- input$pointColor
    if (!is.null(pc) && length(pc) > 0) return(pc)
    xa
  })

  # Factor levels per X-axis column (for ordering UI)
  x_axis_levels <- shiny$reactive({
    data <- input_data()
    xa <- input$xAxis
    if (is.null(data) || nrow(data) == 0 || length(xa) == 0) {
      return(list())
    }
    data_utils$get_factor_levels(data, xa)
  })

  # Current factor order: merge saved order with current data levels
  factor_order <- shiny$reactive({
    levels_list <- x_axis_levels()
    if (length(levels_list) == 0) return(list())

    saved <- saved_factor_order()
    result <- list()

    for (col in names(levels_list)) {
      data_levels <- levels_list[[col]]
      if (col %in% names(saved)) {
        # Keep saved order, append new levels at end
        saved_levels <- saved[[col]]
        valid_saved <- saved_levels[saved_levels %in% data_levels]
        new_levels <- setdiff(data_levels, valid_saved)
        result[[col]] <- c(valid_saved, new_levels)
      } else {
        result[[col]] <- data_levels
      }
    }
    result
  })

  # Unique color groups from interaction of color columns (with ordering)
  color_groups <- shiny$reactive({
    data <- input_data()
    cols <- color_cols()
    fo <- factor_order()
    if (is.null(data) || nrow(data) == 0 || length(cols) == 0) {
      return(character(0))
    }
    interaction_factor <- data_utils$create_interaction(data, cols, fo)
    groups <- levels(interaction_factor)
    if (is.null(groups)) {
      groups <- sort(as.character(unique(interaction_factor)))
    }
    rhino$log$info("Plotting style: {length(groups)} color group(s)")
    groups
  })

  # Render nested sortable tree with color pickers and shape dropdowns
  output$colorOrderTree <- shiny$renderUI({
    xa <- input$xAxis
    fo <- factor_order()
    groups <- color_groups()

    if (length(xa) == 0 || length(groups) == 0) {
      return(shiny$tags$p(
        class = "text-muted small fst-italic",
        "Select X-Axis columns to customize colors and order."
      ))
    }

    # Get existing colors and shapes (isolate to avoid re-render loop)
    existing_colors <- shiny$isolate(collect_colors(input, groups))
    existing_shapes <- shiny$isolate(collect_shapes(input, groups))
    defaults <- data_utils$default_palette(length(groups))

    # Check if "Shape by" is active (disables custom shape selection)
    shape_by_active <- !is.null(input$pointShape) && length(input$pointShape) > 0

    # Build nested tree UI
    build_nested_color_tree(
      ns, xa, fo, groups, existing_colors, existing_shapes,
      defaults, shape_by_active
    )
  })

  # Observer for sortable input changes (per column)
  shiny$observe({
    xa <- input$xAxis
    if (length(xa) == 0) return()

    new_order <- list()
    for (col in xa) {
      input_id <- paste0("order_", make.names(col))
      order_val <- input[[input_id]]
      if (!is.null(order_val) && length(order_val) > 0) {
        new_order[[col]] <- order_val
      }
    }

    if (length(new_order) > 0) {
      current <- shiny$isolate(saved_factor_order())
      # Merge with existing (preserve columns not in current xAxis)
      for (col in names(new_order)) {
        current[[col]] <- new_order[[col]]
      }
      saved_factor_order(current)
    }
  })

  # Custom color map reactive
  color_map <- shiny$reactive({
    groups <- color_groups()
    if (length(groups) == 0) return(NULL)
    collect_colors(input, groups)
  })

  # Custom shape map reactive (only active when Shape by is not used)
  shape_map <- shiny$reactive({
    # If "Shape by" is active, return NULL (shapes driven by column mapping)
    if (!is.null(input$pointShape) && length(input$pointShape) > 0) {
      return(NULL)
    }
    groups <- color_groups()
    if (length(groups) == 0) return(NULL)
    collect_shapes(input, groups)
  })

  list(
    color_map = color_map,
    shape_map = shape_map,
    color_groups = color_groups,
    factor_order = factor_order
  )
}

# ---- Internal helpers ----

# Sanitize group name to a valid Shiny input ID
color_input_id <- function(group) {
  paste0("color_", gsub("[^[:alnum:]]", "_", group))
}

# Sanitize group name to a valid Shiny input ID for shapes
shape_input_id <- function(group) {
  paste0("shape_", gsub("[^[:alnum:]]", "_", group))
}

# Collect current color values from dynamic inputs
collect_colors <- function(input, groups) {
  colors <- vapply(groups, function(group) {
    val <- input[[color_input_id(group)]]
    if (is.null(val)) NA_character_ else val
  }, character(1))
  names(colors) <- groups

  # Fill NAs with default palette
  na_idx <- is.na(colors)
  if (any(na_idx)) {
    defaults <- data_utils$default_palette(length(groups))
    colors[na_idx] <- defaults[na_idx]
  }
  colors
}

# Collect current shape values from dynamic inputs
collect_shapes <- function(input, groups, default_shape = 19L) {
  shapes <- vapply(groups, function(group) {
    val <- input[[shape_input_id(group)]]
    if (is.null(val)) NA_integer_ else as.integer(val)
  }, integer(1))
  names(shapes) <- groups

  # Fill NAs with default shape
  na_idx <- is.na(shapes)
  if (any(na_idx)) {
    shapes[na_idx] <- default_shape
  }
  shapes
}

# Generate shape dropdown choices (0-25 with Unicode symbol previews)
shape_choices <- function() {
  # Unicode approximations of R pch shapes
  symbols <- c(
    "\u25A1",
    "\u25CB",
    "\u25B3",
    "\u002B",
    "\u00D7",
    "\u25C7",
    "\u25BD",
    "\u2295",
    "\u2217",
    "\u22C4",
    "\u2A01",
    "\u2606",
    "\u271B",
    "\u2A02",
    "\u25A0",
    "\u25CF",
    "\u25B2",
    "\u25BC",
    "\u25C6",
    "\u2B24",
    "\u25CF",
    "\u25CF",
    "\u25A0",
    "\u25C6",
    "\u25B2",
    "\u25BC"
  )
  stats::setNames(as.character(0:25), symbols)
}

# Build nested sortable tree with color pickers at leaf nodes
# For single X-axis column: simple sortable list with color pickers
# For multiple columns: nested structure with sortable at each level
build_nested_color_tree <- function(ns, x_cols, factor_order, groups,
                                    existing_colors, existing_shapes,
                                    defaults, shape_by_active) {
  n_cols <- length(x_cols)

  if (n_cols == 1) {
    # Single column: simple sortable list with color pickers
    col <- x_cols[1]
    levels <- factor_order[[col]]
    return(build_single_level_sortable(
      ns, col, levels, groups, existing_colors, existing_shapes,
      defaults, shape_by_active
    ))
  }

  # Multiple columns: build nested structure
  # Outer column is first in x_cols, inner columns follow
  build_multi_level_tree(
    ns, x_cols, factor_order, groups, existing_colors, existing_shapes,
    defaults, shape_by_active
  )
}

# Build sortable list for a single X-axis column
build_single_level_sortable <- function(ns, col, levels, groups,
                                        existing_colors, existing_shapes,
                                        defaults, shape_by_active) {
  input_id <- paste0("order_", make.names(col))

  # Create list items with color pickers and shape dropdowns
  labels <- lapply(seq_along(levels), function(i) {
    level <- levels[i]
    # Find matching group (for single column, group == level)
    group_idx <- which(groups == level)
    color <- if (length(group_idx) > 0 && level %in% names(existing_colors)) {
      existing_colors[[level]]
    } else if (length(group_idx) > 0) {
      defaults[group_idx[1]]
    } else {
      "#808080"
    }

    # Get shape value (default 19 = filled circle)
    shape <- if (level %in% names(existing_shapes)) {
      existing_shapes[[level]]
    } else {
      19L
    }

    shiny$tags$div(
      class = "d-flex align-items-center gap-2 sortable-item",
      style = "padding: 4px 8px; background: #f8f9fa; border-radius: 4px; margin: 2px 0;",
      shiny$tags$span(
        class = "drag-handle text-muted",
        style = "cursor: grab;",
        bsicons$bs_icon("grip-vertical")
      ),
      shiny$tags$span(class = "flex-grow-1 small", level),
      shiny$tags$div(
        class = "shape-select-wrapper",
        title = if (shape_by_active) "Disabled: 'Shape by' is active" else NULL,
        shiny$selectInput(
          inputId = ns(shape_input_id(level)),
          label = NULL,
          choices = shape_choices(),
          selected = as.character(shape),
          width = "100%"
        )
      ),
      shiny$tags$div(
        class = "color-picker-wrapper",
        colourpicker$colourInput(
          inputId = ns(color_input_id(level)),
          label = NULL,
          value = color,
          showColour = "both",
          allowTransparent = FALSE,
          closeOnClick = TRUE
        )
      )
    )
  })
  names(labels) <- levels

  shiny$tags$div(
    class = paste("mb-2", if (shape_by_active) "shape-disabled" else ""),
    shiny$tags$label(class = "form-label small fw-semibold", col),
    sortable$rank_list(
      text = NULL,
      labels = labels,
      input_id = ns(input_id),
      options = sortable$sortable_options(
        handle = ".drag-handle",
        animation = 150
      ),
      class = "sortable-list"
    )
  )
}

# Build multi-level nested tree for multiple X-axis columns
build_multi_level_tree <- function(ns, x_cols, factor_order, groups,
                                   existing_colors, existing_shapes,
                                   defaults, shape_by_active) {
  # For nested structure, we need to:
  # 1. Create sortable for outer level (first column) with depth-0 handles
  # 2. For each outer level value, show inner levels with depth-1+ handles
  # 3. Color pickers only at leaf (innermost) level

  outer_col <- x_cols[1]
  inner_cols <- x_cols[-1]
  outer_levels <- factor_order[[outer_col]]

  outer_input_id <- paste0("order_", make.names(outer_col))

  # Build outer level items (depth 0)
  outer_labels <- lapply(outer_levels, function(outer_val) {
    # Build inner content for this outer value (starting at depth 1)
    inner_content <- build_inner_levels(
      ns, inner_cols, factor_order, outer_val,
      groups, existing_colors, existing_shapes, defaults,
      shape_by_active, depth = 1
    )

    shiny$tags$div(
      class = "nested-group",
      style = paste(
        "border: 1px solid #dee2e6; border-radius: 4px;",
        "margin: 4px 0; background: #fff;"
      ),
      shiny$tags$div(
        class = "d-flex align-items-center gap-2 nested-header",
        style = paste(
          "padding: 6px 8px; background: #e9ecef;",
          "border-radius: 4px 4px 0 0;"
        ),
        shiny$tags$span(
          class = "drag-handle drag-handle-depth-0 text-muted",
          style = "cursor: grab;",
          bsicons$bs_icon("grip-vertical")
        ),
        shiny$tags$span(
          class = "fw-semibold small flex-grow-1",
          outer_val
        )
      ),
      shiny$tags$div(
        class = "nested-content",
        style = "padding: 8px;",
        inner_content
      )
    )
  })
  names(outer_labels) <- outer_levels

  shiny$tags$div(
    shiny$tags$label(
      class = "form-label small fw-semibold mb-1",
      paste(x_cols, collapse = " \u2192 ")
    ),
    shiny$tags$p(
      class = "text-muted small mb-2",
      style = "font-size: 0.75rem;",
      "Drag handles to reorder each level independently."
    ),
    sortable$rank_list(
      text = NULL,
      labels = outer_labels,
      input_id = ns(outer_input_id),
      options = sortable$sortable_options(
        handle = ".drag-handle-depth-0",
        animation = 150,
        fallbackOnBody = TRUE,
        swapThreshold = 0.65
      ),
      class = "sortable-nested-outer"
    )
  )
}

# Recursively build inner levels of the tree
build_inner_levels <- function(ns, cols, factor_order, parent_prefix,
                               groups, existing_colors, existing_shapes,
                               defaults, shape_by_active, depth = 1) {
  if (length(cols) == 0) return(NULL)

  col <- cols[1]
  remaining_cols <- cols[-1]
  levels <- factor_order[[col]]
  is_leaf <- length(remaining_cols) == 0

  # Unique handle class per depth level to prevent drag conflicts

  handle_class <- paste0("drag-handle-depth-", depth)

  # Build items for this level
  labels <- lapply(levels, function(level) {
    current_prefix <- paste(parent_prefix, level, sep = ".")

    if (is_leaf) {
      # Leaf level: show color picker and shape dropdown
      group_idx <- which(groups == current_prefix)
      color <- if (length(group_idx) > 0 &&
                   current_prefix %in% names(existing_colors)) {
        existing_colors[[current_prefix]]
      } else if (length(group_idx) > 0) {
        defaults[group_idx[1]]
      } else {
        "#808080"
      }

      # Get shape value (default 19 = filled circle)
      shape <- if (current_prefix %in% names(existing_shapes)) {
        existing_shapes[[current_prefix]]
      } else {
        19L
      }

      shiny$tags$div(
        class = paste(
          "d-flex align-items-center gap-2 leaf-item",
          if (shape_by_active) "shape-disabled" else ""
        ),
        style = paste(
          "padding: 3px 6px; background: #f8f9fa;",
          "border-radius: 4px; margin: 2px 0;"
        ),
        shiny$tags$span(
          class = paste("drag-handle text-muted", handle_class),
          style = "cursor: grab; font-size: 0.8em;",
          bsicons$bs_icon("grip-vertical")
        ),
        shiny$tags$span(class = "flex-grow-1 small", level),
        shiny$tags$div(
          class = "shape-select-wrapper",
          title = if (shape_by_active) "Disabled: 'Shape by' is active" else NULL,
          shiny$selectInput(
            inputId = ns(shape_input_id(current_prefix)),
            label = NULL,
            choices = shape_choices(),
            selected = as.character(shape),
            width = "100%"
          )
        ),
        shiny$tags$div(
          class = "color-picker-wrapper",
          colourpicker$colourInput(
            inputId = ns(color_input_id(current_prefix)),
            label = NULL,
            value = color,
            showColour = "both",
            allowTransparent = FALSE,
            closeOnClick = TRUE
          )
        )
      )
    } else {
      # Non-leaf: recurse with incremented depth
      inner_content <- build_inner_levels(
        ns, remaining_cols, factor_order, current_prefix,
        groups, existing_colors, existing_shapes, defaults,
        shape_by_active, depth + 1
      )

      shiny$tags$div(
        class = "inner-group",
        style = "margin-left: 8px; border-left: 2px solid #dee2e6; padding-left: 8px; margin-top: 2px;",
        shiny$tags$div(
          class = "d-flex align-items-center gap-1 inner-group-header",
          style = "padding: 2px 0;",
          shiny$tags$span(
            class = paste("drag-handle text-muted", handle_class),
            style = "cursor: grab; font-size: 0.8em;",
            bsicons$bs_icon("grip-vertical")
          ),
          shiny$tags$span(class = "small fw-medium", level)
        ),
        inner_content
      )
    }
  })
  names(labels) <- levels

  # Create actual sortable rank_list for this level
  input_id <- paste0("order_", make.names(col))

  shiny$tags$div(
    class = paste0("inner-sortable inner-sortable-depth-", depth),
    sortable$rank_list(
      text = NULL,
      labels = labels,
      input_id = ns(input_id),
      options = sortable$sortable_options(
        handle = paste0(".", handle_class),
        animation = 150,
        fallbackOnBody = TRUE,
        swapThreshold = 0.65
      ),
      class = paste0("sortable-inner-", depth)
    )
  )
}

# Build hidden sortable inputs for inner columns to capture their order
# (No longer needed since we use actual rank_lists now, but kept for compatibility)
build_inner_sortable_inputs <- function(ns, cols, factor_order) {
  # Return empty - inner levels now have their own rank_lists
  NULL
}

# ---- Accordion panel helpers ----

points_panel <- function(ns) {
  # Condition for plot types that show points
  points_condition <- paste0(
    "input['", ns("plotType"), "'] == 'scatter' || ",
    "input['", ns("plotType"), "'] == 'boxplot_points' || ",
    "input['", ns("plotType"), "'] == 'violin_points'"
  )

  bslib$accordion_panel(
    title = "Points & Colors",
    value = "points",
    icon = bsicons$bs_icon("circle-fill"),
    # Point Size and Jitter - only for plot types with points
    shiny$conditionalPanel(
      condition = points_condition,
      shiny$fluidRow(
        shiny$column(
          6,
          shiny$numericInput(
            ns("pointSize"),
            bslib$tooltip(
              shiny$tags$span(
                "Size ",
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                )
              ),
              "Size of the plotted points"
            ),
            value = 4, min = 1, max = 20
          )
        ),
        shiny$column(
          6,
          shiny$numericInput(
            ns("pointSpread"),
            bslib$tooltip(
              shiny$tags$span(
                "Jitter ",
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                )
              ),
              paste(
                "Amount of horizontal spread",
                "(jittering) applied to points"
              )
            ),
            value = 0.15, step = 0.05, min = 0, max = 2
          )
        )
      )
    ),
    # Alpha - always visible (applies to all plot types)
    shiny$numericInput(
      ns("transparency"),
      bslib$tooltip(
        shiny$tags$span(
          "Alpha ",
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          )
        ),
        paste(
          "Transparency: 0 = fully transparent,",
          "1 = fully opaque"
        )
      ),
      value = 0.6, step = 0.05, min = 0, max = 1
    ),
    # Shape by - only for plot types with points
    shiny$conditionalPanel(
      condition = points_condition,
      shiny$selectizeInput(
        ns("pointShape"),
        bslib$tooltip(
          shiny$tags$span(
            "Shape by ",
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            )
          ),
          paste(
            "Column(s) to determine point shapes",
            "(max 6 unique combinations)"
          )
        ),
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "None", maxItems = 3
        )
      )
    ),
    # Color by - always visible (applies to all plot types)
    shiny$selectizeInput(
      ns("pointColor"),
      bslib$tooltip(
        shiny$tags$span(
          "Color by ",
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          )
        ),
        "Column(s) to determine point/fill colors"
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "X-Axis default"
      )
    )
  )
}

legend_grid_panel <- function(ns) {
  # Condition for scatter plot (only type that shows median/SD)
  scatter_condition <- paste0(
    "input['", ns("plotType"), "'] == 'scatter'"
  )

  bslib$accordion_panel(
    title = "Legend & Grid",
    value = "legend_grid",
    icon = bsicons$bs_icon("grid-3x3"),
    shiny$selectInput(
      ns("legendPosition"),
      "Legend Position",
      choices = c(
        "none", "right", "top", "bottom", "left"
      ),
      selected = "none"
    ),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$checkboxGroupInput(
          ns("gridOptions"),
          "Grid Lines",
          choices = c(
            "Horizontal" = "hGrid",
            "Vertical" = "vGrid",
            "Top/Right" = "topRightBorders"
          ),
          selected = c(
            "hGrid", "vGrid", "topRightBorders"
          )
        )
      ),
      shiny$column(
        6,
        # Median/SD only for scatter, Aspect Ratio for all
        shiny$conditionalPanel(
          condition = scatter_condition,
          shiny$checkboxGroupInput(
            ns("statOptions"),
            "Statistics",
            choices = c(
              "Median" = "showMedian",
              "SD" = "showSD",
              "Aspect Ratio" = "aspectRatio"
            ),
            selected = c("showMedian", "showSD")
          )
        ),
        shiny$conditionalPanel(
          condition = paste0("!(", scatter_condition, ")"),
          shiny$checkboxGroupInput(
            ns("statOptionsNonScatter"),
            "Statistics",
            choices = c(
              "Aspect Ratio" = "aspectRatio"
            ),
            selected = character(0)
          )
        )
      )
    )
  )
}

median_sd_panel <- function(ns) {
  # Condition for scatter plot (only type that shows median/SD)
  scatter_condition <- paste0(
    "input['", ns("plotType"), "'] == 'scatter'"
  )

  bslib$accordion_panel(
    title = "Median & SD Lines",
    value = "median_sd",
    icon = bsicons$bs_icon("dash-lg"),
    shiny$conditionalPanel(
      condition = scatter_condition,
      shiny$fluidRow(
        shiny$column(
          6,
          shiny$numericInput(
            ns("medianThickness"),
            "Median Thickness",
            value = 0.5, min = 0.1, max = 5, step = 0.1
          )
        ),
        shiny$column(
          6,
          shiny$numericInput(
            ns("medianWidth"),
            "Median Width",
            value = 0.15, min = 0.1, max = 1, step = 0.1
          )
        )
      ),
      shiny$fluidRow(
        shiny$column(
          6,
          shiny$numericInput(
            ns("sdThickness"),
            "SD Thickness",
            value = 0.5, min = 0.1, max = 5, step = 0.1
          )
        ),
        shiny$column(
          6,
          shiny$numericInput(
            ns("sdWidth"),
            "SD Width",
            value = 0.15, min = 0.1, max = 1, step = 0.1
          )
        )
      )
    ),
    shiny$conditionalPanel(
      condition = paste0("!(", scatter_condition, ")"),
      shiny$tags$p(
        class = "text-muted small fst-italic",
        "Median & SD lines are only available for Scatter plots."
      )
    )
  )
}

boxplot_panel <- function(ns) {
  # Condition for boxplot types
  boxplot_condition <- paste0(
    "input['", ns("plotType"), "'] == 'boxplot' || ",
    "input['", ns("plotType"), "'] == 'boxplot_points'"
  )

  bslib$accordion_panel(
    title = "Boxplot Settings",
    value = "boxplot",
    icon = bsicons$bs_icon("bar-chart-fill"),
    shiny$conditionalPanel(
      condition = boxplot_condition,
      shiny$fluidRow(
        shiny$column(
          6,
          shiny$numericInput(
            ns("boxWidth"),
            bslib$tooltip(
              shiny$tags$span(
                "Box Width ",
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                )
              ),
              "Width of the boxplot boxes (0-1)"
            ),
            value = 0.7, min = 0.1, max = 1, step = 0.1
          )
        ),
        shiny$column(
          6,
          shiny$checkboxInput(
            ns("showBoxOutliers"),
            bslib$tooltip(
              shiny$tags$span(
                "Show Outliers ",
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                )
              ),
              "Show outlier points on the boxplot"
            ),
            value = FALSE
          )
        )
      ),
      shiny$checkboxInput(
        ns("boxNotch"),
        bslib$tooltip(
          shiny$tags$span(
            "Notched ",
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            )
          ),
          "Show notches for median confidence interval"
        ),
        value = FALSE
      )
    ),
    shiny$conditionalPanel(
      condition = paste0("!(", boxplot_condition, ")"),
      shiny$tags$p(
        class = "text-muted small fst-italic",
        "Select a Boxplot type to configure these settings."
      )
    )
  )
}

violin_panel <- function(ns) {
  # Condition for violin types
  violin_condition <- paste0(
    "input['", ns("plotType"), "'] == 'violin' || ",
    "input['", ns("plotType"), "'] == 'violin_points'"
  )

  bslib$accordion_panel(
    title = "Violin Settings",
    value = "violin",
    icon = bsicons$bs_icon("symmetry-vertical"),
    shiny$conditionalPanel(
      condition = violin_condition,
      shiny$fluidRow(
        shiny$column(
          6,
          shiny$numericInput(
            ns("violinWidth"),
            bslib$tooltip(
              shiny$tags$span(
                "Violin Width ",
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                )
              ),
              "Width of the violin plots (0-1)"
            ),
            value = 0.9, min = 0.1, max = 1.5, step = 0.1
          )
        ),
        shiny$column(
          6,
          shiny$checkboxInput(
            ns("violinTrim"),
            bslib$tooltip(
              shiny$tags$span(
                "Trim ",
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                )
              ),
              "Trim violin tails to data range"
            ),
            value = TRUE
          )
        )
      ),
      shiny$selectInput(
        ns("violinScale"),
        bslib$tooltip(
          shiny$tags$span(
            "Scale ",
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            )
          ),
          paste(
            "How to scale violin widths:",
            "area = equal areas,",
            "count = proportional to n,",
            "width = equal max widths"
          )
        ),
        choices = c("area", "count", "width"),
        selected = "width"
      )
    ),
    shiny$conditionalPanel(
      condition = paste0("!(", violin_condition, ")"),
      shiny$tags$p(
        class = "text-muted small fst-italic",
        "Select a Violin type to configure these settings."
      )
    )
  )
}

axis_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Axis Settings",
    value = "axis",
    icon = bsicons$bs_icon("arrows-angle-expand"),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          ns("axisTickLength"),
          "Tick Length",
          value = 0.15, min = 0.1, max = 1, step = 0.1
        )
      ),
      shiny$column(
        6,
        shiny$numericInput(
          ns("axisLineThickness"),
          "Line Thickness",
          value = 0.5, min = 0.1, max = 5, step = 0.1
        )
      )
    )
  )
}

colors_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Colors & Order",
    value = "colors",
    icon = bsicons$bs_icon("palette"),
    shiny$tags$p(
      class = "small text-muted mb-2",
      "Drag items to reorder factor levels on X-axis."
    ),
    shiny$uiOutput(ns("colorOrderTree"))
  )
}

export_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Export Settings",
    value = "export",
    icon = bsicons$bs_icon("download"),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          ns("exportWidth"),
          bslib$tooltip(
            shiny$tags$span(
              "Width (cm) ",
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              )
            ),
            paste(
              "Plot width in cm for SVG export.",
              "16 cm fits typical Word documents."
            )
          ),
          value = 16, min = 1, max = 50
        )
      ),
      shiny$column(
        6,
        shiny$numericInput(
          ns("exportHeight"),
          bslib$tooltip(
            shiny$tags$span(
              "Height (cm) ",
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              )
            ),
            paste(
              "Plot height in cm for SVG export.",
              "10 cm with 16 cm width gives a",
              "nice ratio."
            )
          ),
          value = 10, min = 1, max = 50
        )
      )
    ),
    shiny$tags$p(
      class = "small text-muted mt-2",
      paste(
        "Use the download button on each plot",
        "card to export as SVG."
      )
    )
  )
}
