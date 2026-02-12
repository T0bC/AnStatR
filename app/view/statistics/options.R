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
    icon = "gear",
    tooltip_text = "Options",
    value = "options_tab",
    shiny$h6(class = "text-muted mb-3", "Options"),
    # Trim value display (read-only, synced from Plotting)
    shiny$uiOutput(ns("trim_value_display")),
    shiny$tags$hr(),
    # Output options
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$checkboxInput(
          inputId = ns("show_additional_output"),
          label = shiny$tags$span(
            "Additional Output ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              paste(
                "Show additional output like",
                "Linear Contrasts and",
                "Cliff's Delta tables."
              )
            )
          ),
          value = FALSE
        )
      ),
      shiny$column(
        6,
        shiny$checkboxInput(
          inputId = ns("use_scientific_notation"),
          label = shiny$tags$span(
            "Scientific Notation ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              "Display results in scientific notation."
            )
          ),
          value = FALSE
        )
      )
    ),
    shiny$tags$hr(),
    # Statistical approach selection
    shiny$radioButtons(
      inputId = ns("test_approach"),
      label = "Statistical Approach:",
      choices = list(
        "Robust Tests" = "robust",
        "Parametric Tests" = "parametric"
      ),
      selected = "robust"
    ),
    # Expandable info about approaches
    bslib$accordion(
      id = ns("test_approach_accordion"),
      open = FALSE,
      bslib$accordion_panel(
        title = "When to use which approach?",
        value = "test_approach_info",
        shiny$uiOutput(ns("approach_details"))
      )
    ),
    shiny$tags$hr(),
    # Filter significant p-values
    shiny$checkboxInput(
      inputId = ns("filter_p_values"),
      label = shiny$tags$span(
        "Show only significant p-values ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Filter results for p-values < 0.07.",
            "Always check effect sizes when",
            "interpreting p-values!"
          )
        )
      ),
      value = FALSE
    ),
    # Valid comparisons (conditionally shown for multi-way)
    shiny$uiOutput(ns("valid_comparisons_ui"))
  )
}

#' @param input Shiny input object from the parent module
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param plotting_x_axis Reactive returning X-axis columns from Plotting
#' @param plotting_trim_percent Reactive returning trim % from Plotting
#' @export
tab_server <- function(input, output, session,
                       plotting_x_axis, plotting_trim_percent) {
  # --- Trim value display (read-only from Plotting) ---
  output$trim_value_display <- shiny$renderUI({
    tr <- if (!is.null(plotting_trim_percent)) {
      plotting_trim_percent()
    } else {
      0
    }
    tr_val <- tr %||% 0

    shiny$tags$div(
      class = "d-flex align-items-center gap-2 mb-2",
      bsicons$bs_icon("scissors", class = "text-muted"),
      shiny$tags$span(
        class = "small",
        paste0(
          "Trim: ", tr_val, "%",
          " (from Plotting tab)"
        )
      )
    )
  })

  # --- Approach details accordion content ---
  output$approach_details <- shiny$renderUI({
    approach <- input$test_approach %||% "robust"
    if (approach == "robust") {
      shiny$tags$div(
        class = "small",
        shiny$tags$p(
          shiny$tags$strong("Robust Tests"),
          " use trimmed means and are less",
          " sensitive to outliers and",
          " non-normal distributions."
        ),
        shiny$tags$p(
          "Recommended for most real-world data."
        )
      )
    } else {
      shiny$tags$div(
        class = "small",
        shiny$tags$p(
          shiny$tags$strong("Parametric Tests"),
          " assume normal distribution and",
          " equal variances."
        ),
        shiny$tags$p(
          "Use only when assumptions are met."
        )
      )
    }
  })

  # --- Valid comparisons UI (multi-way designs) ---
  output$valid_comparisons_ui <- shiny$renderUI({
    x_axis <- if (!is.null(plotting_x_axis)) {
      plotting_x_axis()
    } else {
      NULL
    }
    if (is.null(x_axis) || length(x_axis) < 2) {
      return(NULL)
    }

    shiny$tags$div(
      class = "mt-2",
      shiny$tags$hr(),
      shiny$checkboxInput(
        inputId = session$ns("filter_valid_comparisons"),
        label = shiny$tags$span(
          "Only valid comparisons ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "For multi-way designs, keep only",
              "comparisons where groups differ",
              "by exactly one factor level.",
              "P-value adjustment is applied",
              "after filtering."
            )
          )
        ),
        value = TRUE
      )
    )
  })
}
