box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
)

#' Build the processing sidebar tab UI
#' @param ns Namespace function from the parent module
#' @return A sidebar tab created via sidebar_tabs$create_tab()
#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "sliders",
    tooltip_text = "Data Processing",
    value = "processing_tab",
    shiny$h6(class = "text-muted mb-3", "Data Processing"),
    # --- 1. Trimming ---
    shiny$tags$label(
      class = "small fw-semibold",
      "1. Trimming"
    ),
    shiny$sliderInput(
      inputId = ns("trim_slider"),
      label = shiny$tags$span(
        "Trim % ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Percentage trimmed from each end for",
            "robust statistics (WRS2). Applied after",
            "outlier removal."
          )
        )
      ),
      min = 0, max = 50, value = 0, step = 1
    ),
    shiny$tags$hr(),
    # --- 2. Outlier Detection ---
    shiny$tags$label(
      class = "small fw-semibold",
      "2. Outlier Detection"
    ),
    shiny$checkboxInput(
      inputId = ns("enableOutlierDetection"),
      label = shiny$tags$span(
        "Enable ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Univariate outlier detection - removes",
            "outliers per measurement column",
            "independently."
          )
        )
      ),
      value = FALSE
    ),
    # Outlier options (shown when enabled)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("enableOutlierDetection"),
        "'] == true"
      ),
      outlier_method_radio(ns),
      # Factor slider (factor-based methods)
      shiny$conditionalPanel(
        condition = paste0(
          "['IQR','zscore','modified_zscore',",
          "'adjusted_boxplot','bootstrap']",
          ".includes(input['",
          ns("detectOutlier"), "'])"
        ),
        shiny$sliderInput(
          inputId = ns("standardFactor"),
          label = "Factor:",
          value = 1.5, min = 0.5, max = 10,
          step = 0.1
        )
      ),
      # Probability slider (probability-based methods)
      shiny$conditionalPanel(
        condition = paste0(
          "['kde','isolation_forest','lof']",
          ".includes(input['",
          ns("detectOutlier"), "'])"
        ),
        shiny$sliderInput(
          inputId = ns("probabilityFactor"),
          label = "Threshold:",
          value = 0.05, min = 0.05, max = 1,
          step = 0.05
        )
      ),
      # Bootstrap samples
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("detectOutlier"),
          "'] == 'bootstrap'"
        ),
        shiny$numericInput(
          inputId = ns("bootstrapSamples"),
          label = "Samples:",
          value = 1000, min = 100, max = 10000,
          step = 100
        )
      )
    )
  )
}

#' Stub server for the processing tab (logic added later)
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param data_version Reactive returning the data version counter
#' @export
tab_server <- function(input, output, session, data_version) {
  # Reset inputs on new data
  shiny$observeEvent(data_version(), {
    shiny$updateSliderInput(
      session, "trim_slider", value = 0
    )
    shiny$updateCheckboxInput(
      session, "enableOutlierDetection", value = FALSE
    )
  }, ignoreInit = TRUE)
}

# --- Helper: outlier method radio buttons with tooltips ---
outlier_method_radio <- function(ns) {
  methods <- list(
    list(
      value = "IQR",
      label = "IQR",
      tip = paste(
        "Tukey's method. Best for symmetric data.",
        "Factor: 1.5-3.0"
      )
    ),
    list(
      value = "zscore",
      label = "Z-Score",
      tip = paste(
        "Mean/SD method. Best for normal data.",
        "Factor: 2.0-3.0"
      )
    ),
    list(
      value = "modified_zscore",
      label = "Modified Z-Score",
      tip = paste(
        "Median/MAD method. Robust for skewed data.",
        "Factor: 3.5-4.5"
      )
    ),
    list(
      value = "adjusted_boxplot",
      label = "Adjusted Boxplot",
      tip = paste(
        "Skewness-adjusted IQR.",
        "Factor: 1.5-3.0"
      )
    ),
    list(
      value = "kde",
      label = "KDE",
      tip = paste(
        "Kernel Density. Best for multimodal data.",
        "Threshold: 0.05-0.2"
      )
    ),
    list(
      value = "isolation_forest",
      label = "Isolation Forest",
      tip = paste(
        "Tree-based. Best for large datasets.",
        "Threshold: 0.05-0.2"
      )
    ),
    list(
      value = "lof",
      label = "LOF",
      tip = paste(
        "Local Outlier Factor. Best for density",
        "clusters. Threshold: 0.05-0.2"
      )
    ),
    list(
      value = "bootstrap",
      label = "Bootstrap",
      tip = paste(
        "Resampling method. Best for small samples.",
        "Samples: 1000-10000"
      )
    )
  )

  radio_items <- lapply(methods, function(m) {
    checked <- if (m$value == "IQR") "checked" else NULL
    shiny$tags$div(
      class = "radio",
      shiny$tags$label(
        shiny$tags$input(
          type = "radio",
          name = ns("detectOutlier"),
          value = m$value,
          checked = checked
        ),
        shiny$tags$span(
          paste0(m$label, " "),
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle",
              class = "text-muted small"
            ),
            m$tip
          )
        )
      )
    )
  })

  shiny$tags$div(
    class = paste(
      "form-group shiny-input-radiogroup",
      "shiny-input-container"
    ),
    id = ns("detectOutlier"),
    shiny$tags$label(
      class = "control-label small",
      `for` = ns("detectOutlier"),
      "Method:"
    ),
    shiny$tags$div(
      class = "shiny-options-group",
      radio_items
    )
  )
}
