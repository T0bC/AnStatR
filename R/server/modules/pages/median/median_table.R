# Median table rendering logic
# This file defines a function that calculates and renders the median results table
#
# @param output Shiny output object
# @param output_id Character string for the output ID (e.g., "medianTable")
# @param filtered_data ReactiveVal containing the filtered data
# @param median_results ReactiveVal to store the calculated median results
# @return NULL (side effects: creates output$medianTable and updates median_results)

render_median_table <- function(output, output_id, filtered_data, median_results) {
    output[[output_id]] <- DT::renderDataTable({
        shiny::req(filtered_data())

        data <- filtered_data()

        # Calculate medians for numeric columns
        numeric_cols <- sapply(data, is.numeric)

        if (sum(numeric_cols) == 0) {
            # No numeric columns found
            results <- data.frame(
                Message = "No numeric columns found in the filtered data."
            )
        } else {
            # Calculate medians
            medians <- sapply(data[, numeric_cols, drop = FALSE], median, na.rm = TRUE)

            results <- data.frame(
                Column = names(medians),
                Median = as.numeric(medians),
                stringsAsFactors = FALSE
            )
        }

        # Store results
        median_results(results)

        # Create DataTable with options
        DT::datatable(
            results,
            options = list(
                pageLength = 10,
                lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
                scrollX = TRUE,
                dom = "Blfrtip"
            ),
            rownames = FALSE
        )
    })
}
