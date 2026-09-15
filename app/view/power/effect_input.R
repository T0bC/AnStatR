box::use(
  bsicons,
  bslib,
  shiny,
  stats[sd, shapiro.test],
)

box::use(
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "rulers",
    tooltip_text = "Effect Size",
    value = "effect_tab",
    shiny$h6(class = "text-muted mb-3", "Effect Size Input"),
    # Dynamic content based on mode (manual vs import)
    shiny$uiOutput(ns("effect_tab_content_ui"))
  )
}

#' @export
tab_server <- function(input, output, session,
                       design_reactive = NULL,
                       mode_reactive = NULL,
                       input_data = NULL) {
  ns <- session$ns

  # --- Current mode (manual vs import) ---
  current_mode <- shiny$reactive({
    if (!is.null(mode_reactive)) mode_reactive() else "manual"
  })

  # --- Detected distribution from data ---
  detected_distribution <- shiny$reactive({
    mode <- current_mode()
    if (mode != "import") {
      return(NULL)
    }

    data <- if (!is.null(input_data)) input_data() else NULL
    design <- if (!is.null(design_reactive)) design_reactive() else NULL

    if (is.null(data) || is.null(design)) {
      return("normal")
    }

    measure_col <- design$measure_name
    if (is.null(measure_col) || !measure_col %in% names(data)) {
      return("normal")
    }

    values <- data[[measure_col]]
    values <- values[!is.na(values) & is.finite(values)]

    if (length(values) < 3) {
      return("normal")
    }

    # Auto-detect distribution using Shapiro-Wilk test
    detect_distribution(values)
  })

  # --- Computed group statistics from data ---
  computed_stats <- shiny$reactive({
    mode <- current_mode()
    if (mode != "import") {
      return(NULL)
    }

    data <- if (!is.null(input_data)) input_data() else NULL
    design <- if (!is.null(design_reactive)) design_reactive() else NULL

    if (is.null(data) || is.null(design)) {
      return(NULL)
    }

    measure_col <- design$measure_name
    grouping_cols <- sapply(design$factors, function(f) f$name)

    if (is.null(measure_col) || !measure_col %in% names(data)) {
      return(NULL)
    }
    if (length(grouping_cols) == 0) {
      return(NULL)
    }

    # Compute statistics per group
    compute_group_statistics(data, grouping_cols, measure_col)
  })

  # --- Main content UI (switches based on mode) ---
  output$effect_tab_content_ui <- shiny$renderUI({
    mode <- current_mode()

    if (mode == "import") {
      render_import_mode_effect_ui(ns, input, detected_distribution(), computed_stats())
    } else {
      render_manual_mode_effect_ui(ns)
    }
  })

  # --- Manual mode UI outputs (existing logic) ---
  output$effect_type_ui <- shiny$renderUI({
    distribution <- input$distribution %||% "normal"

    if (distribution != "normal") {
      shiny$tags$div(
        shiny$tags$div(
          class = "alert alert-info py-2 small",
          bsicons$bs_icon("info-circle", class = "me-1"),
          paste0(
            "Standardized effect sizes (Cohen's f) are only valid for normal distributions. ",
            "Using raw distribution parameters for ", distribution, " data."
          )
        ),
        shiny$tags$strong(class = "small text-muted", "Input Method: Raw Parameters")
      )
    } else {
      shiny$radioButtons(
        inputId = ns("effect_type"),
        label = "Input Method:",
        choices = list(
          "Standardized (Cohen's f)" = "standardized",
          "Raw (Group Parameters)" = "raw"
        ),
        selected = input$effect_type %||% "standardized"
      )
    }
  })

  effective_effect_type <- shiny$reactive({
    distribution <- input$distribution %||% "normal"
    if (distribution != "normal") {
      "raw"
    } else {
      input$effect_type %||% "standardized"
    }
  })

  output$input_mode_ui <- shiny$renderUI({
    effect_type <- effective_effect_type()

    if (effect_type == "raw") {
      shiny$tagList(
        shiny$radioButtons(
          inputId = ns("input_mode"),
          label = shiny$tags$span(
            "Input Statistics ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Choose how to specify group parameters based on available data."
            )
          ),
          choices = list(
            "Mean + SD" = "mean_sd",
            "Median + IQR" = "median_iqr"
          ),
          selected = input$input_mode %||% "mean_sd",
          inline = TRUE
        ),
        shiny$tags$hr()
      )
    } else {
      NULL
    }
  })

  output$effect_inputs_ui <- shiny$renderUI({
    effect_type <- effective_effect_type()

    if (effect_type == "standardized") {
      shiny$uiOutput(ns("standardized_inputs"))
    } else {
      shiny$uiOutput(ns("raw_inputs"))
    }
  })

  output$standardized_inputs <- shiny$renderUI({
    design <- if (!is.null(design_reactive)) design_reactive() else NULL
    n_ways <- design$n_ways %||% 1

    if (n_ways == 1) {
      shiny$tags$div(
        shiny$numericInput(
          inputId = ns("cohens_f"),
          label = shiny$tags$span(
            "Cohen's f ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              paste(
                "Effect size for ANOVA.",
                "Small: 0.10, Medium: 0.25, Large: 0.40"
              )
            )
          ),
          value = 0.25,
          min = 0.01,
          max = 2,
          step = 0.05
        ),
        shiny$tags$div(
          class = "small text-muted",
          "Benchmarks: Small=0.10, Medium=0.25, Large=0.40"
        )
      )
    } else {
      factors <- design$factors
      terms <- generate_effect_terms(factors)

      term_inputs <- lapply(seq_along(terms), function(i) {
        term <- terms[i]
        shiny$numericInput(
          inputId = ns(paste0("effect_", i)),
          label = paste0("f for ", term, ":"),
          value = 0.25,
          min = 0.01,
          max = 2,
          step = 0.05
        )
      })

      shiny$tags$div(
        shiny$tags$p(
          class = "small text-muted mb-2",
          "Enter effect size (Cohen's f) for each term:"
        ),
        shiny$tagList(term_inputs)
      )
    }
  })

  output$raw_inputs <- shiny$renderUI({
    design <- if (!is.null(design_reactive)) design_reactive() else NULL
    input_mode <- input$input_mode %||% "mean_sd"
    distribution <- input$distribution %||% "normal"

    if (is.null(design) || is.null(design$factors) ||
      length(design$factors) == 0) {
      return(shiny$tags$div(
        class = "text-muted small",
        "Define factors and levels in the Design tab first."
      ))
    }

    groups <- generate_group_combinations(design$factors)

    if (input_mode == "mean_sd") {
      loc_label <- "Mean:"
      spread_label <- if (distribution == "exponential") NULL else "SD:"
      loc_default <- function(i) i
      spread_default <- 1
      loc_min <- if (distribution %in% c("lognormal", "exponential")) 0.01 else NA_real_
    } else {
      loc_label <- "Median:"
      spread_label <- "IQR:"
      loc_default <- function(i) i
      spread_default <- 1
      loc_min <- if (distribution %in% c("lognormal", "exponential")) 0.01 else NA_real_
    }

    group_inputs <- lapply(seq_along(groups), function(i) {
      group_name <- groups[i]

      loc_input <- shiny$numericInput(
        inputId = ns(paste0("loc_", i)),
        label = loc_label,
        value = loc_default(i),
        min = loc_min,
        step = 0.1
      )

      if (!is.null(spread_label)) {
        spread_input <- shiny$numericInput(
          inputId = ns(paste0("spread_", i)),
          label = spread_label,
          value = spread_default,
          min = 0.01,
          step = 0.1
        )
        row_content <- shiny$fluidRow(
          shiny$column(6, loc_input),
          shiny$column(6, spread_input)
        )
      } else {
        row_content <- loc_input
      }

      shiny$tags$div(
        class = "mb-2 p-2 border rounded",
        shiny$tags$strong(class = "small", group_name),
        row_content
      )
    })

    desc_text <- if (input_mode == "mean_sd") {
      if (distribution == "exponential") {
        "Enter expected mean for each group (SD = mean for exponential):"
      } else {
        "Enter expected mean and SD for each group:"
      }
    } else {
      "Enter expected median and IQR for each group:"
    }

    shiny$tags$div(
      shiny$tags$p(class = "small text-muted mb-2", desc_text),
      shiny$tagList(group_inputs)
    )
  })

  # --- Return reactive with current effect parameters ---
  effect_params <- shiny$reactive({
    mode <- current_mode()

    if (mode == "import") {
      # Import mode: use computed statistics from data
      stats <- computed_stats()
      distribution <- input$distribution_override %||% detected_distribution() %||% "normal"

      if (is.null(stats)) {
        return(list(
          effect_type = "raw",
          input_mode = "mean_sd",
          group_means = c(1, 2),
          group_sd = c(1, 1),
          distribution = distribution
        ))
      }

      list(
        effect_type = "raw",
        input_mode = "mean_sd",
        group_means = stats$means,
        group_sd = stats$sds,
        distribution = distribution,
        computed_f = stats$cohens_f,
        n_per_group = stats$n_per_group
      )
    } else {
      # Manual mode: existing logic
      distribution <- input$distribution %||% "normal"
      effect_type <- if (distribution != "normal") {
        "raw"
      } else {
        input$effect_type %||% "standardized"
      }
      input_mode <- input$input_mode %||% "mean_sd"
      design <- if (!is.null(design_reactive)) design_reactive() else NULL

      if (effect_type == "standardized") {
        n_ways <- design$n_ways %||% 1

        if (n_ways == 1) {
          list(
            effect_type = "standardized",
            effect_size = input$cohens_f %||% 0.25,
            distribution = distribution,
            input_mode = "mean_sd"
          )
        } else {
          factors <- design$factors
          terms <- generate_effect_terms(factors)
          effects <- sapply(seq_along(terms), function(i) {
            input[[paste0("effect_", i)]] %||% 0.25
          })
          names(effects) <- terms

          list(
            effect_type = "standardized",
            effect_size = max(effects),
            effect_terms = effects,
            distribution = distribution,
            input_mode = "mean_sd"
          )
        }
      } else {
        if (is.null(design) || is.null(design$factors) ||
          length(design$factors) == 0) {
          return(list(
            effect_type = "raw",
            input_mode = input_mode,
            group_means = c(1, 2),
            group_sd = c(1, 1),
            distribution = distribution
          ))
        }

        groups <- generate_group_combinations(design$factors)
        n_groups <- length(groups)

        if (input_mode == "mean_sd") {
          means <- sapply(seq_len(n_groups), function(i) {
            input[[paste0("loc_", i)]] %||% i
          })
          sds <- if (distribution == "exponential") {
            means
          } else {
            sapply(seq_len(n_groups), function(i) {
              input[[paste0("spread_", i)]] %||% 1
            })
          }
          names(means) <- groups

          list(
            effect_type = "raw",
            input_mode = "mean_sd",
            group_means = means,
            group_sd = sds,
            distribution = distribution
          )
        } else {
          medians <- sapply(seq_len(n_groups), function(i) {
            input[[paste0("loc_", i)]] %||% i
          })
          iqrs <- sapply(seq_len(n_groups), function(i) {
            input[[paste0("spread_", i)]] %||% 1
          })
          names(medians) <- groups

          list(
            effect_type = "raw",
            input_mode = "median_iqr",
            group_medians = medians,
            group_iqr = iqrs,
            distribution = distribution
          )
        }
      }
    }
  })

  effect_params
}

# --- Helper: Render manual mode effect UI ---
render_manual_mode_effect_ui <- function(ns) {
  shiny$tagList(
    # Distribution selector
    shiny$selectInput(
      inputId = ns("distribution"),
      label = shiny$tags$span(
        "Distribution Shape ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste(
            "Assumed distribution for power simulation.",
            "Non-normal distributions use Monte Carlo simulation."
          )
        )
      ),
      choices = list(
        "Normal" = "normal",
        "Log-normal" = "lognormal",
        "Exponential" = "exponential"
      ),
      selected = "normal"
    ),
    shiny$tags$hr(),
    # Effect type toggle
    shiny$uiOutput(ns("effect_type_ui")),
    shiny$tags$hr(),
    # Input mode toggle
    shiny$uiOutput(ns("input_mode_ui")),
    # Effect inputs
    shiny$uiOutput(ns("effect_inputs_ui"))
  )
}

# --- Helper: Render import mode effect UI ---
render_import_mode_effect_ui <- function(ns, input, detected_dist, stats) {
  detected_dist <- detected_dist %||% "normal"

  # Build distribution info message
  dist_label <- switch(detected_dist,
    "normal" = "Normal (Gaussian)",
    "lognormal" = "Log-normal (right-skewed)",
    "exponential" = "Exponential",
    "Normal"
  )

  # Build stats display
  stats_display <- if (!is.null(stats)) {
    # Create a table of group statistics
    group_rows <- lapply(seq_along(stats$means), function(i) {
      group_name <- names(stats$means)[i]
      shiny$tags$tr(
        shiny$tags$td(class = "small", group_name),
        shiny$tags$td(class = "small text-end", stats$n_per_group[i]),
        shiny$tags$td(class = "small text-end", round(stats$means[i], 3)),
        shiny$tags$td(class = "small text-end", round(stats$sds[i], 3))
      )
    })

    shiny$tagList(
      shiny$tags$div(
        class = "alert alert-success py-2 small mb-2",
        bsicons$bs_icon("check-circle", class = "me-1"),
        paste0("Computed Cohen's f = ", round(stats$cohens_f, 3))
      ),
      shiny$tags$div(
        class = "table-responsive",
        shiny$tags$table(
          class = "table table-sm table-striped mb-0",
          shiny$tags$thead(
            shiny$tags$tr(
              shiny$tags$th(class = "small", "Group"),
              shiny$tags$th(class = "small text-end", "N"),
              shiny$tags$th(class = "small text-end", "Mean"),
              shiny$tags$th(class = "small text-end", "SD")
            )
          ),
          shiny$tags$tbody(group_rows)
        )
      )
    )
  } else {
    shiny$tags$div(
      class = "small text-muted",
      "Select grouping and measurement columns in the Study Design tab."
    )
  }

  shiny$tagList(
    # Auto-detected distribution with override
    shiny$tags$div(
      class = "alert alert-info py-2 small mb-2",
      bsicons$bs_icon("graph-up", class = "me-1"),
      paste0("Auto-detected distribution: ", dist_label)
    ),
    shiny$selectInput(
      inputId = ns("distribution_override"),
      label = shiny$tags$span(
        "Distribution Shape ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          "Override the auto-detected distribution if needed."
        )
      ),
      choices = list(
        "Normal" = "normal",
        "Log-normal" = "lognormal",
        "Exponential" = "exponential"
      ),
      selected = input$distribution_override %||% detected_dist
    ),
    shiny$tags$hr(),
    # Computed statistics display
    shiny$tags$strong(class = "small text-muted", "Computed Group Statistics:"),
    shiny$tags$div(class = "mt-2", stats_display)
  )
}

# --- Helper: Detect distribution from data ---
detect_distribution <- function(values) {
  if (length(values) < 3) {
    return("normal")
  }

  # Check for all positive values (required for lognormal/exponential)
  all_positive <- all(values > 0)

  # Shapiro-Wilk test for normality (use sample if too large)
  test_values <- if (length(values) > 5000) {
    sample(values, 5000)
  } else {
    values
  }

  sw_result <- tryCatch(
    shapiro.test(test_values),
    error = function(e) list(p.value = 1)
  )

  # If p > 0.05, assume normal

  if (sw_result$p.value > 0.05) {
    return("normal")
  }

  # Check skewness for non-normal data
  if (all_positive) {
    # Check if log-transform improves normality
    log_values <- log(test_values)
    sw_log <- tryCatch(
      shapiro.test(log_values),
      error = function(e) list(p.value = 0)
    )

    if (sw_log$p.value > sw_result$p.value) {
      return("lognormal")
    }
  }

  # Default to normal if uncertain
  "normal"
}

# --- Helper: Compute group statistics from data ---
compute_group_statistics <- function(data, grouping_cols, measure_col) {
  if (!measure_col %in% names(data)) {
    return(NULL)
  }
  if (!all(grouping_cols %in% names(data))) {
    return(NULL)
  }

  # Create group identifier
  if (length(grouping_cols) == 1) {
    data$`.group` <- as.character(data[[grouping_cols]])
  } else {
    data$`.group` <- apply(
      data[, grouping_cols, drop = FALSE], 1,
      paste,
      collapse = ":::"
    )
  }

  # Compute statistics per group
  groups <- unique(data$`.group`)
  groups <- groups[!is.na(groups)]

  means <- sapply(groups, function(g) {
    vals <- data[[measure_col]][data$`.group` == g]
    vals <- vals[!is.na(vals) & is.finite(vals)]
    if (length(vals) == 0) NA_real_ else mean(vals)
  })

  sds <- sapply(groups, function(g) {
    vals <- data[[measure_col]][data$`.group` == g]
    vals <- vals[!is.na(vals) & is.finite(vals)]
    if (length(vals) < 2) NA_real_ else sd(vals)
  })

  ns <- sapply(groups, function(g) {
    vals <- data[[measure_col]][data$`.group` == g]
    sum(!is.na(vals) & is.finite(vals))
  })

  # Compute Cohen's f
  grand_mean <- mean(means, na.rm = TRUE)
  pooled_var <- sum((ns - 1) * sds ^ 2, na.rm = TRUE) / sum(ns - 1, na.rm = TRUE)
  pooled_sd <- sqrt(pooled_var)

  between_var <- sum(ns * (means - grand_mean) ^ 2, na.rm = TRUE) / sum(ns, na.rm = TRUE)
  cohens_f <- sqrt(between_var) / pooled_sd

  names(means) <- groups
  names(sds) <- groups
  names(ns) <- groups

  list(
    means = means,
    sds = sds,
    n_per_group = ns,
    cohens_f = if (is.finite(cohens_f)) cohens_f else 0.25,
    pooled_sd = pooled_sd
  )
}

# --- Helper: generate effect terms for factorial designs ---
generate_effect_terms <- function(factors) {
  if (is.null(factors) || length(factors) == 0) {
    return(character(0))
  }

  factor_names <- sapply(factors, function(f) f$name)
  n <- length(factor_names)

  terms <- factor_names

  if (n >= 2) {
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        terms <- c(terms, paste(factor_names[i], factor_names[j], sep = " x "))
      }
    }
  }

  if (n == 3) {
    terms <- c(terms, paste(factor_names, collapse = " x "))
  }

  terms
}

# Internal separator for multi-way group names (must match dummy_data.R)
GROUP_SEP <- ":::"

# --- Helper: generate all group combinations ---
generate_group_combinations <- function(factors) {
  if (is.null(factors) || length(factors) == 0) {
    return(character(0))
  }

  if (length(factors) == 1) {
    return(factors[[1]]$levels)
  }

  level_lists <- lapply(factors, function(f) f$levels)
  grid <- expand.grid(level_lists, stringsAsFactors = FALSE)
  apply(grid, 1, paste, collapse = GROUP_SEP)
}
