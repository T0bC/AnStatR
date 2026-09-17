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
      # Points & Colors: visible for scatter, boxplot+points, violin+points
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("plotType"), "'] == 'scatter' || ",
          "input['", ns("plotType"), "'] == 'boxplot_points' || ",
          "input['", ns("plotType"), "'] == 'violin_points'"
        ),
        points_panel(ns)
      ),
      # Boxplot Settings: visible for boxplot types
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("plotType"), "'] == 'boxplot' || ",
          "input['", ns("plotType"), "'] == 'boxplot_points'"
        ),
        boxplot_panel(ns)
      ),
      # Violin Settings: visible for violin types
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("plotType"), "'] == 'violin' || ",
          "input['", ns("plotType"), "'] == 'violin_points'"
        ),
        violin_panel(ns)
      ),
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

  # Set statOptions defaults when plot type changes
  shiny$observeEvent(input$plotType,
    {
      pt <- input$plotType %||% "scatter"
      shows_lines <- pt %in% c("scatter", "violin_points")

      if (shows_lines) {
        shiny$updateCheckboxGroupInput(
          session, "statOptions",
          choices = c(
            "Median" = "showMedian",
            "SD" = "showSD",
            "Aspect Ratio" = "aspectRatio"
          ),
          selected = c("showMedian", "showSD")
        )
      } else {
        shiny$updateCheckboxGroupInput(
          session, "statOptions",
          choices = c(
            "Median" = "showMedian",
            "SD" = "showSD",
            "Aspect Ratio" = "aspectRatio"
          ),
          selected = character(0)
        )
      }
    },
    ignoreNULL = TRUE
  )

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
        session, "pointColor",
        choices = character(0)
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
    if (is.null(xa) || length(xa) == 0) {
      return(character(0))
    }
    pc <- input$pointColor
    if (!is.null(pc) && length(pc) > 0) {
      return(pc)
    }
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
    if (length(levels_list) == 0) {
      return(list())
    }

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

  # X-axis level combinations that actually occur in the data.  Used to mark
  # tree entries that exist as a factor level but carry no observations
  # (e.g. Gentoo x Torgersen in the penguins data).
  present_combos <- shiny$reactive({
    data <- input_data()
    xa <- input$xAxis
    if (is.null(data) || nrow(data) == 0 || length(xa) == 0) {
      return(character(0))
    }
    as.character(unique(data_utils$create_interaction(data, xa)))
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
      defaults, shape_by_active, present_combos(), color_cols()
    )
  })

  # Observer for sortable input changes (per column)
  #
  # Nested columns render one sortable list per parent branch, each with its
  # own input id, and every one of them shows the same column-wide order.
  # After a re-render the untouched lists echo the new order back, so "value
  # differs from what is stored" alone cannot identify the list the user
  # dragged -- a stale echo would be adopted and revert the drag.  Comparing
  # each list against the value last seen *from that list* does identify it.
  last_order_seen <- new.env(parent = emptyenv())

  shiny$observe({
    xa <- input$xAxis
    if (length(xa) == 0) {
      return()
    }

    levels_list <- x_axis_levels()
    stored <- shiny$isolate(saved_factor_order())

    new_order <- list()
    for (col in xa) {
      input_ids <- order_input_ids(xa, col, levels_list)
      current <- as.character(stored[[col]])
      dragged <- NULL

      for (input_id in input_ids) {
        # Every list is read on every pass: taking the dependency only on
        # some of them leaves the others unable to ever trigger this observer
        # again.
        order_val <- input[[input_id]]
        if (is.null(order_val) || length(order_val) == 0) {
          next
        }
        order_val <- as.character(order_val)
        seen <- last_order_seen[[input_id]]
        assign(input_id, order_val, envir = last_order_seen)

        if (identical(seen, order_val)) {
          # Unchanged since last pass, so not the list that moved
          next
        }
        if (!identical(order_val, current)) {
          dragged <- order_val
        }
      }

      if (!is.null(dragged)) {
        new_order[[col]] <- dragged
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
    if (length(groups) == 0) {
      return(NULL)
    }
    collect_colors(input, groups)
  })

  # Custom shape map reactive (only active when Shape by is not used)
  shape_map <- shiny$reactive({
    # If "Shape by" is active, return NULL (shapes driven by column mapping)
    if (!is.null(input$pointShape) && length(input$pointShape) > 0) {
      return(NULL)
    }
    groups <- color_groups()
    if (length(groups) == 0) {
      return(NULL)
    }
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

# Sanitize an arbitrary group / branch name into an input-ID-safe token
sanitize_id <- function(x) {
  gsub("[^[:alnum:]]", "_", x)
}

# Sanitize group name to a valid Shiny input ID
color_input_id <- function(group) {
  paste0("color_", sanitize_id(group))
}

# Input IDs of every sortable list rendered for one X-axis column
#
# The first column renders a single list.  Deeper columns render one list per
# parent branch, each suffixed with the sanitized parent prefix, so that no
# two lists share a DOM id.
order_input_ids <- function(x_cols, col, levels_list) {
  base_id <- paste0("order_", make.names(col))
  idx <- match(col, x_cols)
  if (is.na(idx) || idx == 1) {
    return(base_id)
  }

  prefixes <- levels_list[[x_cols[1]]]
  if (idx > 2) {
    for (i in 2:(idx - 1)) {
      prefixes <- as.vector(
        outer(prefixes, levels_list[[x_cols[i]]], paste, sep = ".")
      )
    }
  }
  if (length(prefixes) == 0) {
    return(base_id)
  }
  paste0(base_id, "__", sanitize_id(prefixes))
}

# Does any observed X-axis combination sit under this branch prefix?
branch_has_data <- function(prefix, present) {
  if (length(present) == 0) {
    return(TRUE)
  }
  prefix %in% present || any(startsWith(present, paste0(prefix, ".")))
}

# Marker shown instead of the colour / shape controls for empty combinations
no_data_marker <- function() {
  shiny$tags$span(
    class = "level-unavailable-tag small",
    title = "No observations for this combination",
    "no data"
  )
}

# Stand-ins for the shape select and colour picker of an empty leaf.
#
# They keep the wrapper boxes at their normal width and height: SortableJS
# swaps on hover overlap, so a short item next to a tall one makes the list
# oscillate endlessly while dragging.  Every leaf must be the same size.
no_data_controls <- function() {
  shiny$tagList(
    shiny$tags$div(class = "shape-select-wrapper is-empty"),
    shiny$tags$div(
      class = "color-picker-wrapper is-empty",
      no_data_marker()
    )
  )
}

# Sanitize group name to a valid Shiny input ID for shapes
shape_input_id <- function(group) {
  paste0("shape_", sanitize_id(group))
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
collect_shapes <- function(input, groups, default_shape = 21L) {
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

# Generate shape dropdown choices
# Only exposes pch 0-14 (open/unfilled, color = stroke) and
# pch 21-25 (dual-property, fill = interior, color = border).
# Shapes 15-20 are omitted: they are solid-filled with no separate border,
# making white-border discrimination impossible.
shape_choices <- function() {
  pch_values <- c(0:14, 21:25) # nolint: unused_declared_object_linter.
  symbols <- c( # nolint: unused_declared_object_linter.
    "\u25A1", # pch  0: open square
    "\u25CB", # pch  1: open circle
    "\u25B3", # pch  2: open triangle up
    "\u002B", # pch  3: plus
    "\u00D7", # pch  4: cross (X)
    "\u25C7", # pch  5: open diamond
    "\u25BD", # pch  6: open triangle down
    "\u22A0", # pch  7: square with X inside
    "\u2217", # pch  8: asterisk
    "\u25C8", # pch  9: diamond with plus inside
    "\u2A01", # pch 10: circle with plus inside
    "\u2606", # pch 11: open star
    "\u229E", # pch 12: square with plus inside
    "\u2297", # pch 13: circle with X inside
    "\u29C4", # pch 14: square with triangle inside
    "\u25CF", # pch 21: filled circle (with border)
    "\u25A0", # pch 22: filled square (with border)
    "\u25C6", # pch 23: filled diamond (with border)
    "\u25B2", # pch 24: filled triangle up (with border)
    "\u25BC" # pch 25: filled triangle down (with border)
  )
  stats::setNames(as.character(pch_values), symbols)
}

# Depth at which the colour groups are fully determined
#
# Colour groups come from "Color by" (`color_cols`), the tree nests by X-axis
# columns (`x_cols`).  When the colour columns are exactly the first `k`
# X-axis columns, every group corresponds to one tree node at depth `k` and
# the controls can live there.  Any other selection (e.g. colouring by an
# inner column only) maps one group onto several tree positions, so the
# controls have to be rendered as a separate flat list instead.
color_group_depth <- function(x_cols, color_cols) {
  if (length(color_cols) == 0 || !all(color_cols %in% x_cols)) {
    return(NA_integer_)
  }
  k <- length(color_cols)
  if (setequal(color_cols, x_cols[seq_len(k)])) k else NA_integer_
}

# Shared state for every node of the tree
tree_context <- function(ns, groups, existing_colors, existing_shapes,
                         defaults, shape_by_active, present,
                         color_cols, color_depth) {
  list(
    ns = ns,
    groups = groups,
    existing_colors = existing_colors,
    existing_shapes = existing_shapes,
    defaults = defaults,
    shape_by_active = shape_by_active,
    present = present,
    color_cols = color_cols,
    color_depth = color_depth
  )
}

# Colour group a node stands for, given the X-axis values on its path
node_group <- function(ctx, path) {
  if (!all(ctx$color_cols %in% names(path))) {
    return(NA_character_)
  }
  paste(path[ctx$color_cols], collapse = ".")
}

# Shape select + colour picker for one colour group
group_controls <- function(ctx, group) {
  group_idx <- which(ctx$groups == group)
  color <- if (group %in% names(ctx$existing_colors)) {
    ctx$existing_colors[[group]]
  } else if (length(group_idx) > 0) {
    ctx$defaults[group_idx[1]]
  } else {
    "#808080"
  }
  shape <- if (group %in% names(ctx$existing_shapes)) {
    ctx$existing_shapes[[group]]
  } else {
    21L
  }

  shiny$tagList(
    shiny$tags$div(
      class = "shape-select-wrapper",
      title = if (ctx$shape_by_active) {
        "Disabled: 'Shape by' is active"
      } else {
        NULL
      },
      shiny$selectInput(
        inputId = ctx$ns(shape_input_id(group)),
        label = NULL,
        choices = shape_choices(),
        selected = as.character(shape),
        width = "100%"
      )
    ),
    shiny$tags$div(
      class = "color-picker-wrapper",
      colourpicker$colourInput(
        inputId = ctx$ns(color_input_id(group)),
        label = NULL,
        value = color,
        showColour = "both",
        allowTransparent = FALSE,
        closeOnClick = TRUE
      )
    )
  )
}

# Controls for a node, or size-preserving placeholders when it has no data
#
# `depth` is the number of X-axis columns resolved on the path to this node.
node_controls <- function(ctx, path, depth, has_data) {
  if (is.na(ctx$color_depth) || depth != ctx$color_depth) {
    return(NULL)
  }
  if (!has_data) {
    return(no_data_controls())
  }
  group <- node_group(ctx, path)
  if (is.na(group)) {
    return(NULL)
  }
  group_controls(ctx, group)
}

# Build nested sortable tree with colour / shape controls at the colour depth
# For single X-axis column: simple sortable list
# For multiple columns: nested structure with a sortable at each level
build_nested_color_tree <- function(ns, x_cols, factor_order, groups,
                                    existing_colors, existing_shapes,
                                    defaults, shape_by_active,
                                    present = character(0),
                                    color_cols = x_cols) {
  ctx <- tree_context(
    ns, groups, existing_colors, existing_shapes, defaults,
    shape_by_active, present, color_cols,
    color_group_depth(x_cols, color_cols)
  )

  tree <- if (length(x_cols) == 1) {
    build_single_level_sortable(ctx, x_cols[1], factor_order[[x_cols[1]]])
  } else {
    build_multi_level_tree(ctx, x_cols, factor_order)
  }

  # "Color by" that does not line up with the nesting gets its own list
  if (is.na(ctx$color_depth)) {
    return(shiny$tagList(tree, build_flat_color_list(ctx)))
  }
  tree
}

# Build sortable list for a single X-axis column
build_single_level_sortable <- function(ctx, col, levels) {
  input_id <- paste0("order_", make.names(col))

  labels <- lapply(levels, function(level) {
    path <- stats::setNames(level, col)

    shiny$tags$div(
      class = "d-flex align-items-center gap-2 sortable-item leaf-item",
      style = paste(
        "padding: 4px 8px; background: #f8f9fa;",
        "border-radius: 4px; margin: 2px 0;"
      ),
      shiny$tags$span(
        class = "drag-handle text-muted",
        style = "cursor: grab;",
        bsicons$bs_icon("grip-vertical")
      ),
      shiny$tags$span(class = "flex-grow-1 small", level),
      node_controls(ctx, path, depth = 1, has_data = TRUE)
    )
  })
  names(labels) <- levels

  shiny$tags$div(
    class = paste("mb-2", if (ctx$shape_by_active) "shape-disabled" else ""),
    shiny$tags$label(class = "form-label small fw-semibold", col),
    sortable$rank_list(
      text = NULL,
      labels = labels,
      input_id = ctx$ns(input_id),
      options = sortable$sortable_options(
        handle = ".drag-handle",
        animation = 150
      ),
      class = "sortable-list"
    )
  )
}

# Build multi-level nested tree for multiple X-axis columns
build_multi_level_tree <- function(ctx, x_cols, factor_order) {
  outer_col <- x_cols[1]
  inner_cols <- x_cols[-1]
  outer_levels <- factor_order[[outer_col]]
  outer_input_id <- paste0("order_", make.names(outer_col))

  # Build outer level items (handle depth 0)
  outer_labels <- lapply(outer_levels, function(outer_val) {
    path <- stats::setNames(outer_val, outer_col)
    has_data <- branch_has_data(outer_val, ctx$present)
    controls <- node_controls(ctx, path, depth = 1, has_data = has_data)

    inner_content <- build_inner_levels(
      ctx, inner_cols, factor_order, outer_val, path,
      depth = 1
    )

    shiny$tags$div(
      class = paste(
        "nested-group",
        if (has_data) "" else "level-unavailable"
      ),
      style = paste(
        "border: 1px solid #dee2e6; border-radius: 4px;",
        "margin: 4px 0; background: #fff;"
      ),
      shiny$tags$div(
        class = paste(
          "d-flex align-items-center gap-2 nested-header",
          if (ctx$shape_by_active) "shape-disabled" else ""
        ),
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
        ),
        controls,
        if (has_data || !is.null(controls)) NULL else no_data_marker()
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
      paste(x_cols, collapse = " → ")
    ),
    shiny$tags$p(
      class = "text-muted small mb-2",
      style = "font-size: 0.75rem;",
      "Drag handles to reorder each level independently."
    ),
    sortable$rank_list(
      text = NULL,
      labels = outer_labels,
      input_id = ctx$ns(outer_input_id),
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
#
# `parent_prefix` is the "." joined path used for input IDs and the no-data
# lookup, `path` carries the same values keyed by column name.
build_inner_levels <- function(ctx, cols, factor_order, parent_prefix,
                               path, depth = 1) {
  if (length(cols) == 0) {
    return(NULL)
  }

  col <- cols[1]
  remaining_cols <- cols[-1]
  levels <- factor_order[[col]]
  is_leaf <- length(remaining_cols) == 0

  # Unique handle class per depth level to prevent drag conflicts
  handle_class <- paste0("drag-handle-depth-", depth)

  labels <- lapply(levels, function(level) {
    current_prefix <- paste(parent_prefix, level, sep = ".")
    current_path <- c(path, stats::setNames(level, col))
    has_data <- branch_has_data(current_prefix, ctx$present)
    controls <- node_controls(ctx, current_path, depth + 1, has_data)

    if (is_leaf) {
      shiny$tags$div(
        class = paste(
          "d-flex align-items-center gap-2 leaf-item",
          if (ctx$shape_by_active) "shape-disabled" else "",
          if (has_data) "" else "level-unavailable"
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
        controls,
        if (has_data || !is.null(controls)) NULL else no_data_marker()
      )
    } else {
      inner_content <- build_inner_levels(
        ctx, remaining_cols, factor_order, current_prefix,
        current_path, depth + 1
      )

      shiny$tags$div(
        class = paste("inner-group", if (has_data) "" else "level-unavailable"),
        style = paste(
          "margin-left: 8px; border-left: 2px solid #dee2e6;",
          "padding-left: 8px; margin-top: 2px;"
        ),
        shiny$tags$div(
          class = "d-flex align-items-center gap-1 inner-group-header",
          style = "padding: 2px 0;",
          shiny$tags$span(
            class = paste("drag-handle text-muted", handle_class),
            style = "cursor: grab; font-size: 0.8em;",
            bsicons$bs_icon("grip-vertical")
          ),
          shiny$tags$span(class = "small fw-medium", level),
          controls,
          if (has_data || !is.null(controls)) NULL else no_data_marker()
        ),
        inner_content
      )
    }
  })
  names(labels) <- levels

  # Create actual sortable rank_list for this level.
  # One list is rendered per parent branch, so the id carries the parent
  # prefix -- without it every branch would reuse the same DOM id and only
  # the first one would ever get a SortableJS instance attached.
  input_id <- paste0(
    "order_", make.names(col), "__", sanitize_id(parent_prefix)
  )

  shiny$tags$div(
    class = paste0("inner-sortable inner-sortable-depth-", depth),
    sortable$rank_list(
      text = NULL,
      labels = labels,
      input_id = ctx$ns(input_id),
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

# Flat colour / shape list for colour groups that do not map onto the tree
build_flat_color_list <- function(ctx) {
  if (length(ctx$groups) == 0) {
    return(NULL)
  }

  rows <- lapply(ctx$groups, function(group) {
    shiny$tags$div(
      class = "d-flex align-items-center gap-2 leaf-item",
      style = paste(
        "padding: 3px 6px; background: #f8f9fa;",
        "border-radius: 4px; margin: 2px 0;"
      ),
      shiny$tags$span(class = "flex-grow-1 small", group),
      group_controls(ctx, group)
    )
  })

  shiny$tags$div(
    class = paste("mt-3", if (ctx$shape_by_active) "shape-disabled" else ""),
    shiny$tags$label(
      class = "form-label small fw-semibold mb-1",
      paste("Colors:", paste(ctx$color_cols, collapse = " | "))
    ),
    rows
  )
}


# ---- Accordion panel helpers ----

points_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Points & Colors",
    value = "points",
    icon = bsicons$bs_icon("circle-fill"),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          ns("pointSize"),
          bslib$tooltip(
            shiny$tags$span(
              "Size ",
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
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
                "info-circle",
                class = "text-muted"
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
    ),
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("plotType"), "'] != 'boxplot_points' && ",
        "input['", ns("plotType"), "'] != 'violin_points'"
      ),
      shiny$numericInput(
        ns("transparency"),
        bslib$tooltip(
          shiny$tags$span(
            "Alpha ",
            bsicons$bs_icon(
              "info-circle",
              class = "text-muted"
            )
          ),
          paste(
            "Transparency: 0 = fully transparent,",
            "1 = fully opaque"
          )
        ),
        value = 0.6, step = 0.05, min = 0, max = 1
      )
    ),
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("plotType"), "'] == 'boxplot_points' || ",
        "input['", ns("plotType"), "'] == 'violin_points'"
      ),
      shiny$fluidRow(
        shiny$column(
          6,
          shiny$numericInput(
            ns("transparencyPoints"),
            bslib$tooltip(
              shiny$tags$span(
                "Alpha Points ",
                bsicons$bs_icon(
                  "info-circle",
                  class = "text-muted"
                )
              ),
              paste(
                "Transparency of data points:",
                "0 = fully transparent, 1 = fully opaque"
              )
            ),
            value = 0.6, step = 0.05, min = 0, max = 1
          )
        ),
        shiny$column(
          6,
          shiny$numericInput(
            ns("transparencyBox"),
            bslib$tooltip(
              shiny$tags$span(
                "Alpha Box ",
                bsicons$bs_icon(
                  "info-circle",
                  class = "text-muted"
                )
              ),
              paste(
                "Transparency of box/violin fill:",
                "0 = fully transparent, 1 = fully opaque"
              )
            ),
            value = 0.6, step = 0.05, min = 0, max = 1
          )
        )
      )
    ),
    shiny$selectizeInput(
      ns("pointShape"),
      bslib$tooltip(
        shiny$tags$span(
          "Shape by ",
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
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
    ),
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("plotType"), "'] == 'boxplot_points' || ",
        "input['", ns("plotType"), "'] == 'violin_points'"
      ),
      shiny$checkboxInput(
        ns("blackPoints"),
        bslib$tooltip(
          shiny$tags$span(
            "Black data points ",
            bsicons$bs_icon(
              "info-circle",
              class = "text-muted"
            )
          ),
          "Show data points in black while keeping boxplot/violin colors"
        ),
        value = FALSE
      )
    ),
    shiny$selectizeInput(
      ns("pointColor"),
      bslib$tooltip(
        shiny$tags$span(
          "Color by ",
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
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
        shiny$checkboxGroupInput(
          ns("statOptions"),
          "Statistics",
          choices = c(
            "Median" = "showMedian",
            "SD" = "showSD",
            "Aspect Ratio" = "aspectRatio"
          ),
          selected = character(0)
        )
      )
    )
  )
}

median_sd_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Median & SD Lines",
    value = "median_sd",
    icon = bsicons$bs_icon("dash-lg"),
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
    ),
    shiny$tags$hr(class = "my-2"),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$checkboxInput(
          ns("showMedianPoint"),
          bslib$tooltip(
            shiny$tags$span(
              "Median Point ",
              bsicons$bs_icon("info-circle", class = "text-muted")
            ),
            "Overlay a median marker (\u25c6, pch 18) per group"
          ),
          value = FALSE
        )
      ),
      shiny$column(
        6,
        shiny$checkboxInput(
          ns("showMeanPoint"),
          bslib$tooltip(
            shiny$tags$span(
              "Mean Point ",
              bsicons$bs_icon("info-circle", class = "text-muted")
            ),
            "Overlay a mean marker (\u2295, pch 13) per group"
          ),
          value = FALSE
        )
      )
    )
  )
}

boxplot_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Boxplot Settings",
    value = "boxplot",
    icon = bsicons$bs_icon("bar-chart-fill"),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          ns("boxWidth"),
          bslib$tooltip(
            shiny$tags$span(
              "Box Width ",
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              )
            ),
            "Width of the boxplot boxes (0-1)"
          ),
          value = 0.7, min = 0.1, max = 1, step = 0.1
        )
      ),
      shiny$column(
        6,
        # Only show outlier checkbox for pure boxplot (not boxplot_points)
        shiny$conditionalPanel(
          condition = paste0(
            "input['", ns("plotType"), "'] == 'boxplot'"
          ),
          shiny$checkboxInput(
            ns("showBoxOutliers"),
            bslib$tooltip(
              shiny$tags$span(
                "Show Outliers ",
                bsicons$bs_icon(
                  "info-circle",
                  class = "text-muted"
                )
              ),
              paste0(
                "Show outliers detected by the configured algorithm as 'X' ",
                "marks (requires outlier detection enabled in Processing)"
              )
            ),
            value = FALSE
          )
        )
      )
    ),
    shiny$checkboxInput(
      ns("boxNotch"),
      bslib$tooltip(
        shiny$tags$span(
          "Notched ",
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          )
        ),
        "Show notches for median confidence interval"
      ),
      value = FALSE
    )
  )
}

violin_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Violin Settings",
    value = "violin",
    icon = bsicons$bs_icon("symmetry-vertical"),
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          ns("violinWidth"),
          bslib$tooltip(
            shiny$tags$span(
              "Violin Width ",
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              )
            ),
            "Width of the violin plots (0-1)"
          ),
          value = 0.9, min = 0.1, max = 1.5, step = 0.1
        )
      ),
      shiny$column(
        6,
        # Only show outlier checkbox for pure violin (not violin_points)
        shiny$conditionalPanel(
          condition = paste0(
            "input['", ns("plotType"), "'] == 'violin'"
          ),
          shiny$checkboxInput(
            ns("showViolinOutliers"),
            bslib$tooltip(
              shiny$tags$span(
                "Show Outliers ",
                bsicons$bs_icon(
                  "info-circle",
                  class = "text-muted"
                )
              ),
              paste0(
                "Show outliers detected by the configured algorithm as 'X' ",
                "marks (requires outlier detection enabled in Processing)"
              )
            ),
            value = FALSE
          )
        )
      )
    ),
    shiny$selectInput(
      ns("violinScale"),
      bslib$tooltip(
        shiny$tags$span(
          "Scale ",
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
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
    ),
    shiny$checkboxInput(
      ns("violinTrim"),
      bslib$tooltip(
        shiny$tags$span(
          "Trim Tails ",
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          )
        ),
        "Trim violin tails to data range"
      ),
      value = TRUE
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
                "info-circle",
                class = "text-muted"
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
                "info-circle",
                class = "text-muted"
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
