box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/shared/data_utils,
  app/view/components/sidebar_tabs,
)

# =============================================================================
# Shared "Filter Data" sidebar tab, used by Plotting, PCA, LDA and Cluster.
#
# This is a sidebar tab *fragment*, not a moduleServer: it receives the
# parent's input/output/session and writes into the parent namespace,
# matching every other sidebar submodule in the app. Input ids are
# prefixed (see `id_prefix`) so a data column can never collide with a
# parent input such as metaData or data_source.
# =============================================================================

#' Build the filter sidebar tab UI
#' @param ns Namespace function from the parent module
#' @return A sidebar tab created via sidebar_tabs$create_tab()
#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "funnel",
    tooltip_text = "Filter Data",
    value = "filter_tab",
    shiny$h6(class = "text-muted mb-3", "Filter Data"),
    shiny$selectizeInput(
      inputId = ns("hideCols"),
      label = shiny$tags$span(
        "Hide columns ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Hide selected descriptive columns from",
            "filtering but keep them for tooltips."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Optional...")
    ),
    shiny$tags$hr(),
    shiny$uiOutput(ns("filter_row_count")),
    shiny$uiOutput(ns("checkboxes"))
  )
}

#' Server logic for the filter sidebar tab
#'
#' Manages:
#' - hideCols choices (derived from the candidate column pool)
#' - Filter columns reactive (candidates minus hideCols)
#' - Dynamic checkbox rendering with two-column layout
#' - Filtered data reactive with NA handling
#' - Filter state persistence across data recalculations
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @param candidate_cols Reactive returning the filterable column pool.
#'   Defaults to the parent's `metaData` selection, which is what the
#'   Plotting module wants; the analysis modules pass every descriptive
#'   column instead, so a column can be filtered on without also being
#'   selected as a label column.
#' @param enabled Reactive returning FALSE to disable filtering (used
#'   when a module is running on PCA/LDA scores rather than raw data,
#'   where metadata-level filtering is not meaningful)
#' @param log_prefix Character, prefix for this module's log lines
#' @param id_prefix Character, prefix for generated checkbox input ids
#' @return List with filtered_data, filter_cols, filter_signature and
#'   filter_state reactives
#' @export
tab_server <- function(input, output, session, input_data,
                       data_version,
                       candidate_cols = NULL,
                       enabled = NULL,
                       log_prefix = "Filter",
                       id_prefix = "flt_") {
  ns <- session$ns
  saved_filter_state <- shiny$reactiveVal(list())

  if (is.null(candidate_cols)) {
    candidate_cols <- shiny$reactive(input$metaData)
  }
  if (is.null(enabled)) {
    enabled <- shiny$reactive(TRUE)
  }

  col_input_id <- function(col) paste0(id_prefix, col)

  # Smart retention on new data: keep hideCols that still exist
  shiny$observeEvent(data_version(),
    {
      cur_hide <- shiny$isolate(input$hideCols)
      cur_candidates <- shiny$isolate(candidate_cols())
      if (!is.null(cur_hide) && !is.null(cur_candidates)) {
        retained <- intersect(cur_hide, cur_candidates)
      } else {
        retained <- character(0)
      }
      shiny$updateSelectizeInput(
        session, "hideCols",
        selected = retained
      )
      # Clear saved filter state — checkbox values may differ
      saved_filter_state(list())
      rhino$log$info("{log_prefix}: reset for new data")
    },
    ignoreInit = TRUE
  )

  # Update hideCols choices from the candidate pool
  shiny$observe({
    candidates <- candidate_cols()
    if (is.null(candidates)) candidates <- character(0)
    shiny$updateSelectizeInput(
      session, "hideCols",
      choices = candidates,
      selected = input$hideCols[
        input$hideCols %in% candidates
      ]
    )
  })

  # Filter columns = candidate pool minus hideCols
  filter_cols <- shiny$reactive({
    if (!isTRUE(enabled())) {
      return(character(0))
    }
    candidates <- candidate_cols()
    hidden <- input$hideCols
    if (is.null(candidates)) {
      return(character(0))
    }
    candidates[!candidates %in% hidden]
  })

  # Save filter state before data changes (for persistence)
  shiny$observeEvent(input_data(),
    {
      cols <- shiny$isolate(filter_cols())
      if (length(cols) > 0) {
        state <- lapply(cols, function(col) input[[col_input_id(col)]])
        names(state) <- cols
        saved_filter_state(state)
      }
    },
    priority = 100,
    ignoreInit = TRUE
  )

  # Render dynamic filter checkboxes
  output$checkboxes <- shiny$renderUI({
    if (!isTRUE(enabled())) {
      return(shiny$tags$p(
        class = "text-muted fst-italic small",
        paste(
          "Filtering is available for raw data only.",
          "Switch the data source back to Raw Data to",
          "filter by descriptive columns."
        )
      ))
    }

    data <- input_data()
    shiny$req(data)

    cols <- filter_cols()
    if (length(cols) == 0) {
      return(shiny$tags$p(
        class = "text-muted fst-italic small",
        paste(
          "Select descriptive columns or unhide",
          "some to see filtering options."
        )
      ))
    }

    saved_state <- shiny$isolate(saved_filter_state())

    get_selected <- function(col, choices) {
      if (!is.null(saved_state[[col]])) {
        valid <- intersect(saved_state[[col]], choices)
        if (length(valid) > 0) {
          return(valid)
        }
      }
      choices
    }

    make_checkbox <- function(col) {
      ch <- data_utils$get_filter_choices(data[[col]])
      sel <- get_selected(col, ch)
      shiny$checkboxGroupInput(
        ns(col_input_id(col)),
        label = shiny$tags$div(
          class = "d-flex justify-content-between align-items-center",
          shiny$tags$span(col),
          shiny$actionLink(
            inputId = ns(paste0("toggle_all_", col)),
            label = "All / None",
            class = "small ms-2"
          )
        ),
        choices = ch, selected = sel
      )
    }

    if (length(cols) > 1) {
      half <- ceiling(length(cols) / 2)
      cols1 <- cols[seq_len(half)]
      cols2 <- cols[-seq_len(half)]

      shiny$fluidRow(
        shiny$column(6, lapply(cols1, make_checkbox)),
        shiny$column(6, lapply(cols2, make_checkbox))
      )
    } else {
      make_checkbox(cols)
    }
  })

  # Row counter, so an over-narrow filter is visible before the user
  # presses Compute and gets an unexplained failure.
  output$filter_row_count <- shiny$renderUI({
    data <- input_data()
    if (is.null(data) || !isTRUE(enabled())) {
      return(NULL)
    }
    kept <- nrow(filtered_data())
    total <- nrow(data)
    shiny$tags$p(
      class = if (kept < total) {
        "small fw-semibold mb-2"
      } else {
        "small text-muted mb-2"
      },
      paste0(kept, " / ", total, " rows selected")
    )
  })

  # Register "All / None" toggle observers whenever the set of filter cols changes
  active_observers <- list()
  shiny$observe({
    cols <- filter_cols()
    lapply(active_observers, function(obs) obs$destroy())
    active_observers <<- lapply(cols, function(col) {
      local({
        local_col <- col
        shiny$observeEvent(
          input[[paste0("toggle_all_", local_col)]],
          {
            data <- shiny$isolate(input_data())
            if (is.null(data)) {
              return()
            }
            ch <- data_utils$get_filter_choices(data[[local_col]])
            cur <- input[[col_input_id(local_col)]]
            new_sel <- if (length(cur) == length(ch)) character(0) else ch
            shiny$updateCheckboxGroupInput(
              session, col_input_id(local_col),
              choices = ch, selected = new_sel
            )
          },
          ignoreInit = TRUE
        )
      })
    })
  })

  # Current selection per filter column. Reads only the inputs, never
  # the data, so consumers can react to it cheaply.
  filter_state <- shiny$reactive({
    cols <- filter_cols()
    if (length(cols) == 0) {
      return(list())
    }
    state <- lapply(cols, function(col) input[[col_input_id(col)]])
    names(state) <- cols
    state
  })

  # Cheap signature of the current selection, used by the analysis
  # modules to detect that displayed results were computed under a
  # different row subset. Never touches a data column — uploads may be
  # several hundred MB.
  filter_signature <- shiny$reactive({
    data_utils$filter_signature(filter_state())
  })

  # Filtered data reactive
  filtered_data <- shiny$reactive({
    data <- input_data()
    shiny$req(data)

    cols <- filter_cols()
    if (length(cols) == 0) {
      return(data)
    }

    result <- data_utils$filter_data(data, filter_state())
    rhino$log$info(
      "{log_prefix}: {nrow(result)}/{nrow(data)} rows retained"
    )
    result
  })

  # Return filtered data for downstream use
  list(
    filtered_data = filtered_data,
    filter_cols = filter_cols,
    filter_signature = filter_signature,
    filter_state = filter_state
  )
}
