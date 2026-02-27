box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "upload",
    tooltip_text = "Upload",
    value = "upload_tab",
    shiny$h6(class = "text-muted mb-3", "Upload"),
    # RDS bundle upload
    shiny$fileInput(
      inputId = ns("bundle_file"),
      label = shiny$tags$span(
        shiny$tags$strong("Model bundle (.rds) "),
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Upload a .rds bundle exported from the",
            "PCA, LDA, QDA, or MDA tab. This file",
            "contains the trained model and all",
            "preprocessing parameters."
          )
        )
      ),
      accept = ".rds",
      placeholder = "Select .rds file..."
    ),
    shiny$uiOutput(ns("bundle_summary")),
    shiny$tags$hr(),
    # Unknown data upload
    shiny$fileInput(
      inputId = ns("unknown_file"),
      label = shiny$tags$span(
        shiny$tags$strong("Unknown data "),
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Upload a CSV or Excel file containing",
            "the unknown samples to classify or",
            "project. Must have the same measurement",
            "columns as the training data."
          )
        )
      ),
      accept = c(".csv", ".xlsx"),
      placeholder = "Select CSV or Excel..."
    ),
    shiny$uiOutput(ns("unknown_summary")),
    shiny$tags$hr(),
    # Plot settings
    shiny$uiOutput(ns("dim_selectors")),
    # Biplot layer toggle (PCA only)
    shiny$uiOutput(ns("layer_selector")),
    # Group column selector (for PCA training data)
    shiny$uiOutput(ns("group_selector")),
    # Convex hull / ellipse toggle (PCA only)
    shiny$uiOutput(ns("hull_toggle")),
    # Point aesthetics (PCA only)
    shiny$uiOutput(ns("point_controls")),
    # Label column selector (for unknowns)
    shiny$uiOutput(ns("label_selector"))
  )
}

#' @export
tab_server <- function(input, output, session,
                       bundle_reactive,
                       unknown_data_reactive,
                       validation_reactive) {
  ns <- session$ns

  # Bundle summary card
  output$bundle_summary <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) return(NULL)

    analysis_label <- toupper(bundle$analysis_type)
    n_train <- nrow(bundle$used_data)
    n_vars <- length(bundle$numeric_cols)
    src <- bundle$data_source %||% "raw"
    created <- if (!is.null(bundle$created)) {
      format(bundle$created, "%Y-%m-%d %H:%M")
    } else {
      "unknown"
    }
    version <- bundle$app_version %||% "?"

    shiny$tags$div(
      class = "alert alert-success py-2 px-2 small mb-2",
      bsicons$bs_icon(
        "check-circle-fill", class = "me-1"
      ),
      shiny$tags$strong(analysis_label),
      " bundle loaded",
      shiny$tags$br(),
      shiny$tags$span(
        class = "text-muted",
        paste0(
          n_train, " training obs, ",
          n_vars, " variables"
        ),
        shiny$tags$br(),
        paste0(
          "Source: ", src,
          " \u2022 v", version,
          " \u2022 ", created
        )
      )
    )
  })

  # Unknown data summary
  output$unknown_summary <- shiny$renderUI({
    unknown <- unknown_data_reactive()
    if (is.null(unknown)) return(NULL)

    val <- validation_reactive()

    n_rows <- nrow(unknown)
    n_cols <- ncol(unknown)

    # Status badge
    badge <- if (is.null(val)) {
      shiny$tags$span(
        class = "badge bg-secondary",
        "Not validated"
      )
    } else if (val$valid && length(val$warnings) == 0) {
      shiny$tags$span(
        class = "badge bg-success",
        "Ready"
      )
    } else if (val$valid) {
      shiny$tags$span(
        class = "badge bg-warning text-dark",
        paste0(length(val$warnings), " warning(s)")
      )
    } else {
      shiny$tags$span(
        class = "badge bg-danger",
        paste0(length(val$errors), " error(s)")
      )
    }

    content <- shiny$tagList(
      shiny$tags$div(
        class = "alert alert-info py-2 px-2 small mb-2",
        bsicons$bs_icon(
          "file-earmark-spreadsheet", class = "me-1"
        ),
        paste0(n_rows, " rows, ", n_cols, " columns "),
        badge
      )
    )

    # Show errors or warnings
    if (!is.null(val)) {
      if (length(val$errors) > 0) {
        content <- shiny$tagList(
          content,
          shiny$tags$div(
            class = paste(
              "alert alert-danger py-1",
              "px-2 small mb-1"
            ),
            shiny$tags$ul(
              class = "mb-0 ps-3",
              lapply(val$errors, function(e) {
                shiny$tags$li(e)
              })
            )
          )
        )
      }
      if (length(val$warnings) > 0) {
        content <- shiny$tagList(
          content,
          shiny$tags$div(
            class = paste(
              "alert alert-warning py-1",
              "px-2 small mb-1"
            ),
            shiny$tags$ul(
              class = "mb-0 ps-3",
              lapply(val$warnings, function(w) {
                shiny$tags$li(w)
              })
            )
          )
        )
      }
    }

    content
  })

  # Dimension selectors (dynamic based on bundle)
  output$dim_selectors <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) return(NULL)

    # Determine available dimensions
    dims <- get_available_dims(bundle)
    if (length(dims) < 2) return(NULL)

    shiny$tagList(
      shiny$selectInput(
        inputId = ns("dim_x"),
        label = "X Axis",
        choices = dims,
        selected = dims[1]
      ),
      shiny$selectInput(
        inputId = ns("dim_y"),
        label = "Y Axis",
        choices = dims,
        selected = dims[min(2, length(dims))]
      )
    )
  })

  # Biplot layer selector (PCA only)
  output$layer_selector <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) return(NULL)
    if (bundle$analysis_type != "pca") return(NULL)

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
    )
  })

  # Group column selector (PCA training data)
  output$group_selector <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) return(NULL)
    if (bundle$analysis_type != "pca") return(NULL)

    meta_cols <- bundle$meta_cols
    if (is.null(meta_cols) || length(meta_cols) == 0) {
      return(NULL)
    }

    # Filter to columns actually present in used_data
    available <- intersect(
      meta_cols, names(bundle$used_data)
    )
    if (length(available) == 0) return(NULL)

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
      choices = available,
      multiple = TRUE,
      options = list(
        placeholder = "Select grouping column(s)..."
      )
    )
  })

  # Convex hull toggle (PCA only)
  output$hull_toggle <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) return(NULL)
    if (bundle$analysis_type != "pca") return(NULL)

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
    )
  })

  # Point controls (PCA only)
  output$point_controls <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) return(NULL)
    if (bundle$analysis_type != "pca") return(NULL)

    shiny$tagList(
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
      )
    )
  })

  # Label column selector
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
# Internal helpers (not exported)
# =============================================================================

#' Get available dimension names for plotting
get_available_dims <- function(bundle) {
  analysis_type <- bundle$analysis_type

  if (analysis_type == "pca") {
    # Dim.X format to match create_biplot convention
    model <- bundle$model
    n_pc <- ncol(model$rotation)
    paste0("Dim.", seq_len(n_pc))
  } else if (analysis_type %in% c("lda", "mda")) {
    # LD dimensions from model
    model <- bundle$model
    if (analysis_type == "lda") {
      n_ld <- length(model$svd)
    } else {
      # MDA: use dimension from training scores
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
    # Use companion LDA dimensions
    if (!is.null(bundle$lda_scores)) {
      colnames(bundle$lda_scores)
    } else {
      c("LD1", "LD2")
    }
  } else {
    c("Dim1", "Dim2")
  }
}
