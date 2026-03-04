box::use(
  bsicons,
  bslib,
  colourpicker,
  rhino,
  shiny,
)

box::use(
  app/logic/data_utils,
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
#' - Dynamic color picker rendering based on color group levels
#' - Custom color map reactive
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @return List with color_map reactive
#' @export
tab_server <- function(input, output, session, input_data,
                       data_version) {
  ns <- session$ns

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

  # Unique color groups from interaction of color columns
  color_groups <- shiny$reactive({
    data <- input_data()
    cols <- color_cols()
    if (is.null(data) || nrow(data) == 0 ||
        length(cols) == 0) {
      return(character(0))
    }
    interaction_factor <- data_utils$create_interaction(
      data, cols
    )
    groups <- sort(as.character(unique(interaction_factor)))
    rhino$log$info(
      "Plotting style: {length(groups)} color group(s)"
    )
    groups
  })

  # Render dynamic color pickers
  output$colorPickers <- shiny$renderUI({
    groups <- color_groups()

    if (length(groups) == 0) {
      return(shiny$tags$p(
        class = "text-muted small fst-italic",
        paste(
          "Select X-Axis columns to customize",
          "group colors."
        )
      ))
    }

    # Get existing colors (isolate to avoid re-render loop)
    existing <- shiny$isolate(collect_colors(
      input, groups
    ))
    defaults <- data_utils$default_palette(length(groups))

    # Responsive grid: up to 3 columns
    num_cols <- min(3, length(groups))
    col_width <- 12 %/% num_cols

    pickers <- lapply(seq_along(groups), function(i) {
      group <- groups[i]
      input_id <- color_input_id(group)
      current <- if (
        !is.null(existing) &&
        group %in% names(existing)
      ) {
        existing[[group]]
      } else {
        defaults[i]
      }

      shiny$column(
        width = col_width,
        colourpicker$colourInput(
          inputId = ns(input_id),
          label = group,
          value = current,
          showColour = "both",
          allowTransparent = FALSE,
          closeOnClick = TRUE
        )
      )
    })

    shiny$fluidRow(pickers)
  })

  # Custom color map reactive
  color_map <- shiny$reactive({
    groups <- color_groups()
    if (length(groups) == 0) return(NULL)
    collect_colors(input, groups)
  })

  list(
    color_map = color_map,
    color_groups = color_groups
  )
}

# ---- Internal helpers ----

# Sanitize group name to a valid Shiny input ID
color_input_id <- function(group) {
  paste0("color_", gsub("[^[:alnum:]]", "_", group))
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

# ---- Accordion panel helpers ----

points_panel <- function(ns) {
  bslib$accordion_panel(
    title = "Points",
    value = "points",
    icon = bsicons$bs_icon("circle-fill"),
    shiny$fluidRow(
      shiny$column(
        4,
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
        4,
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
      ),
      shiny$column(
        4,
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
        )
      )
    ),
    shiny$fluidRow(
      shiny$column(
        6,
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
      shiny$column(
        6,
        shiny$selectizeInput(
          ns("pointColor"),
          bslib$tooltip(
            shiny$tags$span(
              "Color by ",
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              )
            ),
            "Column(s) to determine point colors"
          ),
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "X-Axis default"
          )
        )
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
          selected = c("showMedian", "showSD")
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
    title = "Custom Colors",
    value = "colors",
    icon = bsicons$bs_icon("palette"),
    shiny$uiOutput(ns("colorPickers"))
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
