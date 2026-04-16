box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/power/validate,
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "diagram-3",
    tooltip_text = "Study Design",
    value = "design_tab",
    shiny$h6(class = "text-muted mb-3", "Study Design"),
    # Design type selector
    shiny$selectInput(
      inputId = ns("design_type"),
      label = "Factorial Design:",
      choices = list(
        "1-way (single factor)" = "1",
        "2-way (two factors)" = "2",
        "3-way (three factors)" = "3"
      ),
      selected = "1"
    ),
    shiny$tags$hr(),
    # Dynamic factor inputs
    shiny$uiOutput(ns("factor_inputs")),
    shiny$tags$hr(),
    # Measurement column name
    shiny$textInput(
      inputId = ns("measure_name"),
      label = "Measurement Name:",
      value = "measure",
      placeholder = "e.g., Strength, Hardness"
    ),
    shiny$tags$hr(),
    # Import from loaded data button
    shiny$uiOutput(ns("import_button_ui"))
  )
}

#' @export
tab_server <- function(input, output, session, input_data = NULL) {
  ns <- session$ns

  # --- Dynamic factor inputs based on design type ---
  output$factor_inputs <- shiny$renderUI({
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

      # Unique default levels per factor to avoid collisions in multi-way designs
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

    shiny$tagList(factor_uis)
  })

  # --- Import from loaded data button ---
  output$import_button_ui <- shiny$renderUI({
    has_data <- !is.null(input_data) && !is.null(input_data())

    if (has_data) {
      shiny$actionButton(
        inputId = ns("import_from_data"),
        label = "Import from Loaded Data",
        class = "btn-outline-secondary btn-sm w-100",
        icon = bsicons$bs_icon("download")
      )
    } else {
      shiny$tags$div(
        class = "small text-muted",
        bsicons$bs_icon("info-circle", class = "me-1"),
        "Load data to enable import of factor structure."
      )
    }
  })

  # Track last shown warnings to avoid repeated notifications
  last_warnings <- shiny$reactiveVal(character(0))

  # --- Return reactive with current design structure ---
  design_structure <- shiny$reactive({
    n_factors <- as.integer(input$design_type %||% "1")

    # Parse raw factors
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

    # Sanitize measure name
    measure_name <- validate$sanitize_name(input$measure_name %||% "measure")

    list(
      n_ways = n_factors,
      factors = factors,
      measure_name = measure_name,
      n_groups = prod(sapply(factors, function(f) length(f$levels)))
    )
  })

  list(
    design = design_structure,
    import_trigger = shiny$reactive(input$import_from_data)
  )
}
