box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
)

# =============================================================================
# UI — all inputs are static so they exist immediately
# =============================================================================

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "palette",
    tooltip_text = "Plot Settings",
    value = "plotting_tab",
    # Hidden input that server sets to "pca" or "lda"
    # to toggle which control panel is visible
    shiny$tags$div(
      style = "display:none;",
      shiny$textInput(
        inputId = ns("plot_mode"),
        label = NULL,
        value = "none"
      )
    ),
    # LDA/MDA/QDA controls panel
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("plot_mode"), "'] === 'lda'"
      ),
      build_lda_controls_ui(ns)
    ),
    # PCA controls panel
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("plot_mode"), "'] === 'pca'"
      ),
      build_pca_controls_ui(ns)
    ),
    # Cluster controls panel
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("plot_mode"), "'] === 'cluster'"
      ),
      build_cluster_controls_ui(ns)
    ),
    # Label column selector (shared across all types)
    shiny$uiOutput(ns("label_selector")),
    shiny$tags$hr(),
    # Plot dimensions for export
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          inputId = ns("width"),
          label = shiny$tags$span(
            "Width (cm) ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Set the width of the plot in cm",
                "for export. A value of 16 cm",
                "correlates with the page width",
                "in typical Word documents."
              )
            )
          ),
          value = 16,
          min = 1,
          max = 50
        )
      ),
      shiny$column(
        6,
        shiny$numericInput(
          inputId = ns("height"),
          label = shiny$tags$span(
            "Height (cm) ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Set the height of the plot in cm",
                "for export. In combination with",
                "a width of 16 cm, a good value",
                "could be 10 cm."
              )
            )
          ),
          value = 10,
          min = 1,
          max = 50
        )
      )
    )
  )
}


# =============================================================================
# Server
# =============================================================================

#' @export
tab_server <- function(input, output, session,
                       bundle_reactive,
                       unknown_data_reactive) {
  ns <- session$ns

  # Toggle which control panel is visible and update
  # dimension choices when the bundle changes
  shiny$observeEvent(bundle_reactive(), {
    bundle <- bundle_reactive()
    if (is.null(bundle)) {
      # Hide both panels
      shiny$updateTextInput(
        session, "plot_mode", value = "none"
      )
      return()
    }

    analysis_type <- bundle$analysis_type

    if (analysis_type == "pca") {
      shiny$updateTextInput(
        session, "plot_mode", value = "pca"
      )
      update_pca_choices(session, bundle)
    } else if (
      analysis_type %in% c("lda", "mda", "qda")
    ) {
      shiny$updateTextInput(
        session, "plot_mode", value = "lda"
      )
      update_lda_choices(session, bundle)
    } else if (analysis_type == "cluster") {
      shiny$updateTextInput(
        session, "plot_mode", value = "cluster"
      )
      update_cluster_choices(session, bundle)
    }
  }, ignoreNULL = TRUE)

  # Label column selector (renderUI is fine here —
  # not on the critical path for plot rendering)
  output$label_selector <- shiny$renderUI({
    unknown <- unknown_data_reactive()
    if (is.null(unknown)) return(NULL)

    cols <- names(unknown)
    char_cols <- cols[vapply(
      unknown, function(x) {
        is.character(x) || is.factor(x)
      },
      logical(1)
    )]

    if (length(char_cols) == 0) return(NULL)

    shiny$selectInput(
      inputId = ns("label_col"),
      label = shiny$tags$span(
        "Label column ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select a column to use as labels",
            "for the unknown samples in the plot."
          )
        )
      ),
      choices = c("(auto)" = "", char_cols),
      selected = ""
    )
  })
}


# =============================================================================
# Static UI builders (called at UI creation time)
# =============================================================================

#' Build the LDA/MDA/QDA controls panel (static)
build_lda_controls_ui <- function(ns) {
  shiny$tagList(
    shiny$uiOutput(ns("lda_title")),
    # LD dimension selection
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$selectizeInput(
          inputId = ns("ldDimX"),
          label = shiny$tags$span(
            "Dim.X ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the discriminant dimension",
                "for the x-axis."
              )
            )
          ),
          choices = c("LD1", "LD2"),
          selected = "LD1"
        )
      ),
      shiny$column(
        6,
        shiny$selectizeInput(
          inputId = ns("ldDimY"),
          label = shiny$tags$span(
            "Dim.Y ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the discriminant dimension",
                "for the y-axis."
              )
            )
          ),
          choices = c("LD1", "LD2"),
          selected = "LD2"
        )
      )
    ),
    # Assumption diagnostics overlay toggle
    shiny$checkboxInput(
      inputId = ns("show_diagnostics"),
      label = shiny$tags$span(
        "Show Assumption Diagnostics ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Overlay per-group (solid) and pooled",
            "within-group (dashed) covariance",
            "ellipses on the LD Scores plot.",
            "If both match, the equal-covariance",
            "assumption holds."
          )
        )
      ),
      value = FALSE
    ),
    # Decision boundaries overlay toggle
    shiny$checkboxInput(
      inputId = ns("show_boundaries"),
      label = shiny$tags$span(
        "Show Decision Boundaries ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Overlay classification decision",
            "regions and boundary lines on the",
            "plot. Shaded areas show which group",
            "the model predicts for each region."
          )
        )
      ),
      value = TRUE
    ),
    shiny$tags$hr()
  )
}

#' Build the PCA controls panel (static)
build_pca_controls_ui <- function(ns) {
  shiny$tagList(
    shiny$h6(
      class = "text-muted mb-2",
      "PCA Plotting Controls"
    ),
    # Biplot layer toggle
    shiny$radioButtons(
      inputId = ns("biplot_layer"),
      label = shiny$tags$span(
        "Biplot Layer ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select which layers to display:",
            "individual scores, variable loadings,",
            "or both combined."
          )
        )
      ),
      choices = c(
        "Individuals" = "individuals",
        "Variables (Loadings)" = "variables",
        "Combined" = "combined"
      ),
      selected = "individuals",
      inline = TRUE
    ),
    shiny$tags$hr(),
    # Group column selector
    shiny$selectizeInput(
      inputId = ns("group_col"),
      label = shiny$tags$span(
        "Group training data ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select metadata column(s) to group",
            "the training data in the overlay",
            "plot. Multiple columns are combined."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select grouping column(s)..."
      )
    ),
    # Convex hull toggle
    shiny$checkboxInput(
      inputId = ns("show_convex_hull"),
      label = shiny$tags$span(
        "Use Convex Hull ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Show convex hull instead of 95%",
            "confidence ellipse around groups."
          )
        )
      ),
      value = FALSE
    ),
    shiny$tags$hr(),
    # Point controls
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$selectInput(
          inputId = ns("point_alpha"),
          label = "Point Alpha",
          choices = c(
            "Contrib." = "Contribution",
            "0.25" = 0.25,
            "0.5" = 0.5,
            "0.75" = 0.75,
            "1.0" = 1.0
          ),
          selected = "Contribution"
        )
      ),
      shiny$column(
        6,
        shiny$selectInput(
          inputId = ns("point_size"),
          label = "Point Size",
          choices = c(
            "Contrib." = "Contribution",
            "1" = 1, "2" = 2, "3" = 3,
            "4" = 4, "5" = 5, "6" = 6
          ),
          selected = "Contribution"
        )
      )
    ),
    shiny$tags$hr(),
    # Dimension selection
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$selectizeInput(
          inputId = ns("dim_x"),
          label = "X Axis",
          choices = c("Dim.1", "Dim.2", "Dim.3"),
          selected = "Dim.1"
        )
      ),
      shiny$column(
        6,
        shiny$selectizeInput(
          inputId = ns("dim_y"),
          label = "Y Axis",
          choices = c("Dim.1", "Dim.2", "Dim.3"),
          selected = "Dim.2"
        )
      )
    ),
    shiny$tags$hr()
  )
}

#' Build the Cluster controls panel (static)
build_cluster_controls_ui <- function(ns) {
  shiny$tagList(
    shiny$h6(
      class = "text-muted mb-2",
      "Cluster Plotting Controls"
    ),
    # Dimension selection (raw measurement columns)
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$selectizeInput(
          inputId = ns("clusterDimX"),
          label = shiny$tags$span(
            "X Axis ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              paste(
                "Select the measurement column",
                "for the x-axis."
              )
            )
          ),
          choices = NULL
        )
      ),
      shiny$column(
        6,
        shiny$selectizeInput(
          inputId = ns("clusterDimY"),
          label = shiny$tags$span(
            "Y Axis ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              paste(
                "Select the measurement column",
                "for the y-axis."
              )
            )
          ),
          choices = NULL
        )
      )
    ),
    shiny$tags$hr()
  )
}


# =============================================================================
# Server helpers for updating choices
# =============================================================================

#' Update LDA/MDA/QDA dimension choices
update_lda_choices <- function(session, bundle) {
  analysis_type <- bundle$analysis_type

  if (analysis_type %in% c("lda", "mda")) {
    dims <- get_available_dims(bundle)
    n_ld <- length(dims)

    shiny$updateSelectizeInput(
      session, "ldDimX",
      choices = dims,
      selected = dims[1]
    )
    shiny$updateSelectizeInput(
      session, "ldDimY",
      choices = dims,
      selected = if (n_ld >= 2) {
        dims[2]
      } else {
        dims[1]
      }
    )
  } else if (analysis_type == "qda") {
    # QDA: offer LD axes + original variables
    ld_names <- if (!is.null(bundle$lda_scores)) {
      colnames(bundle$lda_scores)
    } else {
      character(0)
    }
    orig_names <- bundle$numeric_cols

    choices <- list()
    if (length(ld_names) > 0) {
      choices[["LD Axes (LDA projection)"]] <-
        ld_names
    }
    if (length(orig_names) > 0) {
      choices[["Original Variables"]] <- orig_names
    }

    n_ld <- length(ld_names)
    default_x <- if (n_ld >= 1) {
      ld_names[1]
    } else {
      orig_names[1]
    }
    default_y <- if (n_ld >= 2) {
      ld_names[2]
    } else if (length(orig_names) >= 2) {
      orig_names[2]
    } else {
      default_x
    }

    shiny$updateSelectizeInput(
      session, "ldDimX",
      choices = choices,
      selected = default_x
    )
    shiny$updateSelectizeInput(
      session, "ldDimY",
      choices = choices,
      selected = default_y
    )
  }

  # Update title dynamically
  title_label <- switch(
    analysis_type,
    qda = "QDA Plotting Controls",
    mda = "MDA Plotting Controls",
    "LDA Plotting Controls"
  )
  session$output$lda_title <- shiny$renderUI({
    shiny$h6(class = "text-muted mb-2", title_label)
  })
}

#' Update PCA dimension and group choices
update_pca_choices <- function(session, bundle) {
  dims <- get_available_dims(bundle)
  shiny$updateSelectizeInput(
    session, "dim_x",
    choices = dims,
    selected = dims[1]
  )
  shiny$updateSelectizeInput(
    session, "dim_y",
    choices = dims,
    selected = dims[min(2, length(dims))]
  )

  # Update group column choices
  meta_cols <- bundle$meta_cols
  if (
    !is.null(meta_cols) &&
    length(meta_cols) > 0
  ) {
    available <- intersect(
      meta_cols, names(bundle$used_data)
    )
    shiny$updateSelectizeInput(
      session, "group_col",
      choices = available
    )
  }
}

#' Update Cluster dimension choices
update_cluster_choices <- function(session, bundle) {
  dims <- bundle$numeric_cols
  shiny$updateSelectizeInput(
    session, "clusterDimX",
    choices = dims,
    selected = dims[1]
  )
  shiny$updateSelectizeInput(
    session, "clusterDimY",
    choices = dims,
    selected = dims[min(2, length(dims))]
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Get available dimension names for plotting
get_available_dims <- function(bundle) {
  analysis_type <- bundle$analysis_type

  if (analysis_type == "pca") {
    model <- bundle$model
    n_pc <- ncol(model$rotation)
    paste0("Dim.", seq_len(n_pc))
  } else if (analysis_type %in% c("lda", "mda")) {
    model <- bundle$model
    if (analysis_type == "lda") {
      n_ld <- length(model$svd)
    } else {
      used <- bundle$used_data
      numeric_cols <- bundle$numeric_cols
      scores <- tryCatch(
        stats::predict(
          model, used[, numeric_cols, drop = FALSE],
          type = "variates"
        ),
        error = function(e) NULL
      )
      n_ld <- if (!is.null(scores)) {
        ncol(scores)
      } else {
        2
      }
    }
    paste0("LD", seq_len(max(n_ld, 1)))
  } else if (analysis_type == "qda") {
    if (!is.null(bundle$lda_scores)) {
      colnames(bundle$lda_scores)
    } else {
      c("LD1", "LD2")
    }
  } else {
    c("Dim1", "Dim2")
  }
}
