box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
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

      shiny$tags$div(
        class = "mb-3 p-2 border rounded",
        shiny$tags$strong(
          class = "text-muted small",
          paste0("Factor ", i)
        ),
        shiny$textInput(
          inputId = ns(factor_id),
          label = "Factor Name:",
          value = default_name,
          placeholder = "e.g., Material"
        ),
        shiny$textInput(
          inputId = ns(levels_id),
          label = shiny$tags$span(
            "Levels ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Enter comma-separated level names, e.g.: Mat_A, Mat_B, Mat_C"
            )
          ),
          value = "Level_1, Level_2",
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

  # --- Return reactive with current design structure ---
  design_structure <- shiny$reactive({
    n_factors <- as.integer(input$design_type %||% "1")

    factors <- lapply(seq_len(n_factors), function(i) {
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

    factors <- Filter(Negate(is.null), factors)

    list(
      n_ways = n_factors,
      factors = factors,
      measure_name = input$measure_name %||% "measure",
      n_groups = prod(sapply(factors, function(f) length(f$levels)))
    )
  })

  list(
    design = design_structure,
    import_trigger = shiny$reactive(input$import_from_data)
  )
}
