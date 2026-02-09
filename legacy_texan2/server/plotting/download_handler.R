#' Download Handler Component
#'
#' Handles Excel export of filtered data.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @name download_handler
NULL


#' Setup Download Data Handler
#'
#' Registers download handler for filtered data export (Excel format).
#'
#' @param output Shiny output object from parent module
#' @param input Shiny input object from parent module
#'   - input$xAxis: Selected X-axis columns (used in filename)
#' @param filtered_data Reactive returning filtered data frame
#' @return NULL (side effects only - registers download handler)
#' @export
setup_download_handler <- function(output, input, filtered_data) {
    output$downloadData <- shiny::downloadHandler(
        filename = function() {
            # Create descriptive filename with date and selected X-axis columns
            x_cols <- input$xAxis
            x_suffix <- if (!is.null(x_cols) && length(x_cols) > 0) {
                paste0("_", paste(x_cols, collapse = "-"))
            } else {
                ""
            }
            paste0("filtered_data_", Sys.Date(), x_suffix, ".xlsx")
        },
        content = function(file) {
            data <- filtered_data()
            if (is.null(data) || nrow(data) == 0) {
                # Create empty workbook with message if no data
                wb <- openxlsx::createWorkbook()
                openxlsx::addWorksheet(wb, "No Data")
                openxlsx::writeData(wb, "No Data", "No filtered data available.")
                openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
                return()
            }
            # Write data to Excel
            openxlsx::write.xlsx(data, file, rowNames = FALSE)
        }
    )
}
