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
    icon = "sliders",
    tooltip_text = "Settings",
    value = "options_tab",
    shiny$h6(class = "text-muted mb-3", "Analysis Settings"),
    # Dynamic content based on mode
    shiny$uiOutput(ns("options_content_ui"))
  )
}

#' @export
tab_server <- function(input, output, session,
                       mode_reactive = NULL,
                       effect_params_reactive = NULL) {
  ns <- session$ns

  # --- Current mode (manual vs import) ---
  current_mode <- shiny$reactive({
    if (!is.null(mode_reactive)) mode_reactive() else "manual"
  })

  # --- Get observed N from effect params (import mode) ---
  observed_n <- shiny$reactive({
    mode <- current_mode()
    if (mode != "import") return(NULL)

    effect <- if (!is.null(effect_params_reactive)) effect_params_reactive() else NULL
    if (is.null(effect) || is.null(effect$n_per_group)) return(NULL)

    # Return minimum N per group (for unbalanced designs)
    min(effect$n_per_group, na.rm = TRUE)
  })

  # --- Options content UI ---
  output$options_content_ui <- shiny$renderUI({
    mode <- current_mode()
    obs_n <- observed_n()

    render_options_ui(ns, input, mode, obs_n)
  })

  # Return reactive with current options
  shiny$reactive({
    mode <- current_mode()
    obs_n <- observed_n()

    # In import mode, use observed N when solving for power/mde
    n_per_group <- if (mode == "import" && !is.null(obs_n)) {
      solve_for <- input$solve_for %||% "sample_size"
      if (solve_for != "sample_size") obs_n else input$n_per_group %||% 20
    } else {
      input$n_per_group %||% 20
    }

    list(
      solve_for = input$solve_for %||% "sample_size",
      alpha = input$alpha %||% 0.05,
      power_target = input$power_target %||% 0.80,
      n_per_group = n_per_group,
      approach = input$approach %||% "parametric",
      n_sim = input$n_sim %||% 1000
    )
  })
}

# --- Helper: Render options UI ---
render_options_ui <- function(ns, input, mode, observed_n) {
  # Build mode-specific info banner
  mode_info <- if (mode == "import" && !is.null(observed_n)) {
    shiny$tags$div(
      class = "alert alert-info py-2 small mb-3",
      bsicons$bs_icon("database", class = "me-1"),
      paste0(
        "Using imported data. Observed N per group: ~", round(observed_n),
        ". Sample size recommendations apply to future studies."
      )
    )
  } else {
    NULL
  }

  # Build solve_for description based on mode
  solve_for_help <- if (mode == "import") {
    shiny$tags$div(
      class = "small text-muted mb-2",
      shiny$tags$ul(
        class = "ps-3 mb-0",
        shiny$tags$li(
          shiny$tags$strong("Sample Size:"),
          " Recommendation for future studies"
        ),
        shiny$tags$li(
          shiny$tags$strong("Power:"),
          " Post-hoc power of current study"
        ),
        shiny$tags$li(
          shiny$tags$strong("MDE:"),
          " Smallest effect detectable with current N"
        )
      )
    )
  } else {
    NULL
  }

  shiny$tagList(
    mode_info,
    # Solve for selector
    shiny$radioButtons(
      inputId = ns("solve_for"),
      label = "Solve for:",
      choices = list(
        "Sample Size" = "sample_size",
        "Power" = "power",
        "Minimum Detectable Effect" = "mde"
      ),
      selected = input$solve_for %||% "sample_size"
    ),
    solve_for_help,
    shiny$tags$hr(),
    # Alpha level
    shiny$numericInput(
      inputId = ns("alpha"),
      label = shiny$tags$span(
        "Significance Level (\u03b1) ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          "Type I error rate. Common values: 0.05, 0.01"
        )
      ),
      value = input$alpha %||% 0.05,
      min = 0.001,
      max = 0.5,
      step = 0.01
    ),
    # Power target (conditional on solve_for)
    shiny$conditionalPanel(
      condition = "input.solve_for != 'power'",
      ns = ns,
      shiny$numericInput(
        inputId = ns("power_target"),
        label = shiny$tags$span(
          "Target Power (1-\u03b2) ",
          bslib$tooltip(
            bsicons$bs_icon("info-circle", class = "text-muted"),
            "Probability of detecting a true effect. Common: 0.80, 0.90"
          )
        ),
        value = input$power_target %||% 0.80,
        min = 0.5,
        max = 0.99,
        step = 0.05
      )
    ),
    # N per group (conditional - show observed N in import mode)
    if (mode == "import" && !is.null(observed_n)) {
      shiny$conditionalPanel(
        condition = "input.solve_for != 'sample_size'",
        ns = ns,
        shiny$tags$div(
          class = "mb-3",
          shiny$tags$label(class = "form-label", "Sample Size per Group"),
          shiny$tags$div(
            class = "form-control bg-light",
            paste0(round(observed_n), " (from data)")
          ),
          shiny$tags$small(
            class = "text-muted",
            "Using observed sample size from imported data."
          )
        )
      )
    } else {
      shiny$conditionalPanel(
        condition = "input.solve_for != 'sample_size'",
        ns = ns,
        shiny$numericInput(
          inputId = ns("n_per_group"),
          label = shiny$tags$span(
            "Sample Size per Group ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Number of observations in each group/cell."
            )
          ),
          value = input$n_per_group %||% 20,
          min = 2,
          step = 1
        )
      )
    },
    shiny$tags$hr(),
    # Statistical approach
    shiny$radioButtons(
      inputId = ns("approach"),
      label = "Statistical Approach:",
      choices = list(
        "Parametric (ANOVA)" = "parametric",
        "Robust (Trimmed Means)" = "robust",
        "Non-Parametric (Kruskal-Wallis)" = "nonparametric"
      ),
      selected = input$approach %||% "parametric"
    ),
    # Simulation settings (conditional)
    shiny$conditionalPanel(
      condition = "input.approach != 'parametric'",
      ns = ns,
      shiny$tags$div(
        class = "mt-2 p-2 border rounded bg-light",
        shiny$tags$small(
          class = "text-muted d-block mb-2",
          bsicons$bs_icon("cpu", class = "me-1"),
          "Simulation-based power estimation"
        ),
        shiny$numericInput(
          inputId = ns("n_sim"),
          label = "Simulation Iterations:",
          value = input$n_sim %||% 1000,
          min = 100,
          max = 10000,
          step = 100
        ),
        shiny$tags$small(
          class = "text-muted",
          "More iterations = more accurate but slower."
        )
      )
    )
  )
}
