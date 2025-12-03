# Filtering message rendering logic
# This file defines a function that displays a message about the current filtering state
#
# @param output Shiny output object
# @param output_id Character string for the output ID (e.g., "filteringMessage2")
# @param filtered_data ReactiveVal containing the filtered data
# @return NULL (side effects: creates output$filteringMessage2)

render_filtering_message <- function(output, output_id, filtered_data) {
    output[[output_id]] <- shiny::renderUI({
        shiny::req(filtered_data())

        data <- filtered_data()

        # Create a message about the filtered data
        shiny::tagList(
            shiny::tags$hr(),
            shiny::tags$p(
                shiny::tags$strong("Filtered data summary:"),
                sprintf(" %d rows and %d columns", nrow(data), ncol(data))
            )
        )
    })
}
