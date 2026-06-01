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
    ),
    shiny$tags$hr(),
    # Statistical approach selection
    shiny$radioButtons(
      inputId = ns("test_approach"),
      label = "Statistical Approach:",
      choices = list(
        "Robust Tests" = "robust",
        "Parametric Tests" = "parametric",
        "Non-Parametric Tests" = "nonparametric"
      ),
      selected = "robust"
    ),
    # Non-parametric post-hoc method (shown only for nonparametric)
    shiny$uiOutput(ns("np_posthoc_method_ui")),
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
    # Repeated measures controls
    shiny$checkboxInput(
      inputId = ns("is_repeated_measures"),
      label = shiny$tags$span(
        "Repeated Measures ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Enable for within-subject / repeated",
            "measures designs. Requires an ID column",
            "identifying subjects and a within-subject",
            "factor on the X-axis."
          )
        )
      ),
      value = FALSE
    ),
    shiny$uiOutput(ns("rm_options_ui")),
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
#' @param input_data Reactive returning the current data frame
#' @export
tab_server <- function(input, output, session,
                       plotting_x_axis, plotting_trim_percent,
                       input_data = NULL) {
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

  # --- Repeated measures options (conditional) ---
  output$rm_options_ui <- shiny$renderUI({
    if (!isTRUE(input$is_repeated_measures)) return(NULL)

    # Derive descriptive column choices from data
    data <- if (!is.null(input_data)) input_data() else NULL
    desc_choices <- if (!is.null(data)) {
      all_cols <- names(data)
      # descriptive = uppercase-only columns
      desc <- all_cols[grepl("^[A-Z0-9_]+$", all_cols)]
      if (length(desc) == 0) all_cols else desc
    } else {
      character(0)
    }

    # Within-subject factor choices from x-axis
    x_choices <- if (!is.null(plotting_x_axis)) {
      plotting_x_axis()
    } else {
      character(0)
    }

    shiny$tags$div(
      class = "mt-2 mb-2 ps-3 border-start border-primary border-2",
      shiny$selectInput(
        inputId = session$ns("rm_id_col"),
        label = shiny$tags$span(
          "ID Column (Subject) ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Select the column that uniquely",
              "identifies each subject/specimen.",
              "Must be a descriptive column in",
              "your dataset."
            )
          )
        ),
        choices = desc_choices,
        selected = NULL
      ),
      shiny$selectInput(
        inputId = session$ns("rm_within_col"),
        label = shiny$tags$span(
          "Within-Subject Factor ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Select which X-axis factor is the",
              "within-subject (repeated) factor.",
              "Remaining X-axis factors are treated",
              "as between-subject."
            )
          )
        ),
        choices = x_choices,
        selected = if (length(x_choices) > 0) {
          x_choices[length(x_choices)]
        } else {
          NULL
        }
      ),
      shiny$tags$small(
        class = "text-muted d-block mt-1",
        paste(
          "Each subject must appear exactly once",
          "per level of the within-subject factor."
        )
      )
    )
  })

  # --- Non-parametric post-hoc method radio (conditional) ---
  output$np_posthoc_method_ui <- shiny$renderUI({
    approach <- input$test_approach %||% "robust"
    if (approach != "nonparametric") return(NULL)

    shiny$tags$div(
      class = "mt-2 mb-2",
      shiny$radioButtons(
        inputId = session$ns("np_posthoc_method"),
        label = shiny$tags$span(
          "1-Way Post-Hoc Method ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Dunn's test uses rank sums from the",
              "Kruskal-Wallis test (standard post-hoc).",
              "Pairwise Wilcoxon performs independent",
              "rank-sum tests per pair (more conservative).",
              "For 2/3-way designs, ART contrasts are",
              "always used regardless of this setting."
            )
          )
        ),
        choices = list(
          "Dunn's Test" = "dunn",
          "Pairwise Wilcoxon" = "wilcox"
        ),
        selected = "dunn",
        inline = TRUE
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
    } else if (approach == "parametric") {
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
    } else {
      shiny$tags$div(
        class = "small",
        shiny$tags$p(
          shiny$tags$strong("Non-Parametric Tests"),
          " make no assumptions about the",
          " underlying distribution."
        ),
        shiny$tags$p(
          "Suitable for ordinal data or when",
          " parametric assumptions are violated."
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
