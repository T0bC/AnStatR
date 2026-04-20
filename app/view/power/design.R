box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/power/validate,
  app/logic/shared/column_utils,
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "diagram-3",
    tooltip_text = "Study Design",
    value = "design_tab",
    shiny$h6(class = "text-muted mb-3", "Study Design"),
    # Mode selector (manual vs import)
    shiny$uiOutput(ns("mode_selector_ui")),
    shiny$tags$hr(),
    # Dynamic content based on mode
    shiny$uiOutput(ns("design_content_ui"))
  )
}

#' @export
tab_server <- function(input, output, session, input_data = NULL) {

  ns <- session$ns

  # Track last shown warnings to avoid repeated notifications
  last_warnings <- shiny$reactiveVal(character(0))

  # --- Mode selector UI ---
  output$mode_selector_ui <- shiny$renderUI({
    has_data <- !is.null(input_data) && !is.null(input_data())

    if (has_data) {
      shiny$radioButtons(
        inputId = ns("input_mode"),
        label = NULL,
        choices = list(
          "Manual Entry" = "manual",
          "Import from Data" = "import"
        ),
        selected = input$input_mode %||% "manual",
        inline = TRUE
      )
    } else {
      shiny$tags$div(
        class = "small text-muted",
        bsicons$bs_icon("info-circle", class = "me-1"),
        "Load data in the 'Load Data' tab to enable data import mode."
      )
    }
  })

  # --- Current mode reactive ---
  current_mode <- shiny$reactive({
    has_data <- !is.null(input_data) && !is.null(input_data())
    if (!has_data) return("manual")
    input$input_mode %||% "manual"
  })

  # --- Design content UI (switches based on mode) ---
  output$design_content_ui <- shiny$renderUI({
    mode <- current_mode()

    if (mode == "import") {
      render_import_mode_ui(ns, input_data)
    } else {
      render_manual_mode_ui(ns, input)
    }
  })

  # --- Import mode: detected factor structure display ---
  output$detected_structure_ui <- shiny$renderUI({
    mode <- current_mode()
    if (mode != "import") return(NULL)

    data <- if (!is.null(input_data)) input_data() else NULL
    if (is.null(data)) return(NULL)

    grouping_cols <- input$grouping_cols
    if (is.null(grouping_cols) || length(grouping_cols) == 0) {
      return(shiny$tags$div(
        class = "small text-muted",
        "Select grouping columns above to see the detected factor structure."
      ))
    }

    # Build factor structure from selected columns
    factors <- lapply(grouping_cols, function(col) {
      levels <- unique(as.character(data[[col]]))
      levels <- levels[!is.na(levels)]
      list(name = col, levels = sort(levels))
    })

    n_ways <- length(factors)
    n_groups <- prod(sapply(factors, function(f) length(f$levels)))

    # Render read-only factor structure
    factor_displays <- lapply(factors, function(f) {
      shiny$tags$div(
        class = "mb-2 p-2 border rounded bg-light",
        shiny$tags$strong(class = "small", f$name),
        shiny$tags$div(
          class = "small text-muted",
          paste0(length(f$levels), " levels: ", paste(f$levels, collapse = ", "))
        )
      )
    })

    shiny$tagList(
      shiny$tags$div(
        class = "alert alert-info py-2 small mb-2",
        bsicons$bs_icon("diagram-3", class = "me-1"),
        paste0(
          "Detected ", n_ways, "-way design with ",
          n_groups, " group", if (n_groups != 1) "s" else ""
        )
      ),
      shiny$tags$div(
        shiny$tags$strong(class = "small text-muted", "Factor Structure (read-only):"),
        factor_displays
      )
    )
  })

  # --- Design structure reactive (handles both modes) ---
  design_structure <- shiny$reactive({
    mode <- current_mode()

    if (mode == "import") {
      # Import mode: build from selected columns
      data <- if (!is.null(input_data)) input_data() else NULL
      if (is.null(data)) {
        return(list(
          n_ways = 1,
          factors = list(list(name = "Group", levels = c("A", "B"))),
          measure_name = "measure",
          n_groups = 2
        ))
      }

      grouping_cols <- input$grouping_cols
      measure_col <- input$measure_col

      if (is.null(grouping_cols) || length(grouping_cols) == 0) {
        return(list(
          n_ways = 1,
          factors = list(list(name = "Group", levels = c("A", "B"))),
          measure_name = measure_col %||% "measure",
          n_groups = 2
        ))
      }

      # Build factor structure from data
      factors <- lapply(grouping_cols, function(col) {
        levels <- unique(as.character(data[[col]]))
        levels <- levels[!is.na(levels)]
        list(name = col, levels = sort(levels))
      })

      n_groups <- prod(sapply(factors, function(f) length(f$levels)))

      list(
        n_ways = length(factors),
        factors = factors,
        measure_name = measure_col %||% "measure",
        n_groups = n_groups
      )
    } else {
      # Manual mode: use text inputs
      n_factors <- as.integer(input$design_type %||% "1")

      raw_factors <- lapply(seq_len(n_factors), function(i) {
        factor_name <- input[[paste0("factor_", i)]]
        levels_raw <- input[[paste0("levels_", i)]]

        if (is.null(factor_name) || is.null(levels_raw)) {
          return(NULL)
        }

        levels <- trimws(strsplit(levels_raw, ",")[[1]])
        levels <- levels[nchar(levels) > 0]

        list(
          name = trimws(factor_name),
          levels = levels
        )
      })

      raw_factors <- Filter(Negate(is.null), raw_factors)

      # Sanitize factor structure
      sanitized <- validate$sanitize_factor_structure(raw_factors)
      factors <- sanitized$factors
      warnings <- sanitized$warnings

      # Show notifications for new warnings (avoid repeating)
      prev_warnings <- last_warnings()
      new_warnings <- setdiff(warnings, prev_warnings)
      if (length(new_warnings) > 0) {
        shiny$showNotification(
          shiny$tags$div(
            shiny$tags$strong("Input sanitized:"),
            shiny$tags$ul(
              lapply(new_warnings, function(w) shiny$tags$li(w))
            )
          ),
          type = "warning",
          duration = 5
        )
        last_warnings(warnings)
      }

      measure_name <- validate$sanitize_name(input$measure_name %||% "measure")

      list(
        n_ways = n_factors,
        factors = factors,
        measure_name = measure_name,
        n_groups = prod(sapply(factors, function(f) length(f$levels)))
      )
    }
  })

  list(
    design = design_structure,
    mode = current_mode,
    import_trigger = shiny$reactive(input$import_from_data)
  )
}

# --- Helper: Render manual mode UI ---
render_manual_mode_ui <- function(ns, input) {
  n_factors <- as.integer(input$design_type %||% "1")

  factor_uis <- lapply(seq_len(n_factors), function(i) {
    factor_id <- paste0("factor_", i)
    levels_id <- paste0("levels_", i)

    default_name <- switch(
      as.character(i),
      "1" = "Material",
      "2" = "Treatment",
      "3" = "Condition"
    )

    default_levels <- switch(
      as.character(i),
      "1" = "MatA, MatB",
      "2" = "TreatX, TreatY",
      "3" = "Cond1, Cond2"
    )

    shiny$tags$div(
      class = "mb-3 p-2 border rounded",
      shiny$tags$strong(
        class = "text-muted small",
        paste0("Factor ", i)
      ),
      shiny$textInput(
        inputId = ns(factor_id),
        label = shiny$tags$span(
          "Factor Name ",
          bslib$tooltip(
            bsicons$bs_icon("info-circle", class = "text-muted"),
            paste(
              "Use letters, numbers, and underscores.",
              "Spaces and special characters will be converted automatically."
            )
          )
        ),
        value = default_name,
        placeholder = "e.g., Material"
      ),
      shiny$textInput(
        inputId = ns(levels_id),
        label = shiny$tags$span(
          "Levels ",
          bslib$tooltip(
            bsicons$bs_icon("info-circle", class = "text-muted"),
            paste(
              "Comma-separated level names (e.g., Mat_A, Mat_B).",
              "Use letters, numbers, underscores.",
              "Spaces/special chars will be converted."
            )
          )
        ),
        value = default_levels,
        placeholder = "Level_1, Level_2, Level_3"
      )
    )
  })

  shiny$tagList(
    # Design type selector
    shiny$selectInput(
      inputId = ns("design_type"),
      label = "Factorial Design:",
      choices = list(
        "1-way (single factor)" = "1",
        "2-way (two factors)" = "2",
        "3-way (three factors)" = "3"
      ),
      selected = input$design_type %||% "1"
    ),
    shiny$tags$hr(),
    # Factor inputs
    shiny$tagList(factor_uis),
    shiny$tags$hr(),
    # Measurement column name
    shiny$textInput(
      inputId = ns("measure_name"),
      label = "Measurement Name:",
      value = input$measure_name %||% "measure",
      placeholder = "e.g., Strength, Hardness"
    )
  )
}

# --- Helper: Render import mode UI ---
render_import_mode_ui <- function(ns, input_data) {
  data <- if (!is.null(input_data)) input_data() else NULL

  if (is.null(data)) {
    return(shiny$tags$div(
      class = "alert alert-warning py-2 small",
      bsicons$bs_icon("exclamation-triangle", class = "me-1"),
      "No data loaded. Please load data first."
    ))
  }

  # Get column choices
  all_cols <- names(data)
  descriptive_cols <- column_utils$get_descriptive_cols(data)
  measurement_cols <- column_utils$get_measurement_cols(data)

  # If no clear separation, use all columns
  if (length(descriptive_cols) == 0) {
    descriptive_cols <- all_cols
  }
  if (length(measurement_cols) == 0) {
    measurement_cols <- all_cols
  }

  shiny$tagList(
    # Grouping columns selector
    shiny$selectInput(
      inputId = ns("grouping_cols"),
      label = shiny$tags$span(
        "Grouping Columns ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste(
            "Select 1-3 columns that define your experimental groups.",
            "The number of columns determines the factorial design type."
          )
        )
      ),
      choices = descriptive_cols,
      selected = NULL,
      multiple = TRUE
    ),
    shiny$tags$div(
      class = "small text-muted mb-3",
      "Select 1-3 columns (determines 1/2/3-way design)"
    ),
    # Measurement column selector
    shiny$selectInput(
      inputId = ns("measure_col"),
      label = shiny$tags$span(
        "Measurement Column ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          "Select the outcome variable for power analysis."
        )
      ),
      choices = measurement_cols,
      selected = if (length(measurement_cols) > 0) measurement_cols[1] else NULL
    ),
    shiny$tags$hr(),
    # Detected structure display
    shiny$uiOutput(ns("detected_structure_ui"))
  )
}
