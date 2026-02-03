# Grouping UI rendering
# This file defines a function that renders the grouping column selection UI
#
# @param output Shiny output object
# @param output_id Character string for the output ID
# @param loaded_data Reactive containing the loaded data
# @param input Shiny input object
# @param session Shiny session object
# @return NULL (side effects: creates output; input$grouping_columns used by median_params)

#' @export
render_grouping_ui <- function(output, output_id, loaded_data, input, session) {
    ns <- session$ns
    
    # Reactive to get descriptive columns (strict pattern: uppercase + underscores only)
    descriptive_cols <- shiny::reactive({
        shiny::req(loaded_data())
        get_descriptive_cols(loaded_data())
    })
    
    # Render grouping column selection UI
    output[[output_id]] <- shiny::renderUI({
        shiny::req(loaded_data())
        
        cols <- descriptive_cols()
        
        if (length(cols) == 0) {
            return(shiny::tags$div(
                class = "alert alert-warning",
                "No descriptive columns found. Check column naming conventions."
            ))
        }
        
        shiny::tagList(
            shiny::tags$p(
                class = "text-muted small",
                "Select the columns that define your sample structure. ",
                "This determines how data is grouped for filtering and median calculation."
            ),
            shiny::selectizeInput(
                inputId = ns("grouping_columns"),
                label = NULL,
                choices = cols,
                selected = NULL,
                multiple = TRUE,
                options = list(
                    placeholder = "Select grouping columns..."
                )
            ),
            # Dynamic help text based on selection
            shiny::uiOutput(ns("grouping_info"))
        )
    })
    
    # Render info about current grouping selection
    output$grouping_info <- shiny::renderUI({
        data <- loaded_data()
        shiny::req(data)
        
        group_cols <- input$grouping_columns
        
        if (is.null(group_cols) || length(group_cols) == 0) {
            return(shiny::tags$p(
                class = "text-muted small fst-italic",
                "No grouping selected. Filtering will apply to entire dataset."
            ))
        }
        
        # Calculate number of unique groups
        n_groups <- nrow(unique(data[, group_cols, drop = FALSE]))
        n_rows <- nrow(data)
        avg_per_group <- round(n_rows / n_groups, 1)
        
        shiny::tags$div(
            class = "alert alert-info py-1 px-2 small",
            shiny::tags$strong(n_groups), " unique groups identified",
            shiny::tags$br(),
            shiny::tags$span(
                class = "text-muted",
                paste0("(~", avg_per_group, " rows per group on average)")
            )
        )
    })
    
}
