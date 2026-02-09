# Help modal display logic
# This file defines a function that shows a help modal when the help button is clicked
#
# @param input Shiny input object
# @param session Shiny session object
# @return NULL (side effects: displays modal dialog)

#' @export
handle_help_button <- function(input, session) {
    shiny::observeEvent(input$helpButton, {
        shiny::showModal(
            shiny::modalDialog(
                title = "Calculating the Median of Multiple Measurements",
                easyClose = TRUE,
                size = "l",
                bslib::accordion(
                    multiple = TRUE,
                    open = "filtering",
                    bslib::accordion_panel(
                        title = "Filtering Values",
                        value = "filtering",
                        icon = bsicons::bs_icon("funnel"),
                        shiny::includeMarkdown("docs/median_calculation/MEDIAN_help_filter.md")
                    ),
                    bslib::accordion_panel(
                        title = "Median Calculation",
                        value = "median",
                        icon = bsicons::bs_icon("calculator"),
                        shiny::includeMarkdown("docs/median_calculation/MEDIAN_help.md")
                    ),
                    bslib::accordion_panel(
                        title = "Example Data",
                        value = "example",
                        icon = bsicons::bs_icon("table"),
                        shiny::tags$img(
                            src = base64enc::dataURI(
                                file = "www/images/MEDIAN_help_DF_new.PNG",
                                mime = "image/png"
                            ),
                            alt = "Example for a data frame, with multiple groups and subgrouping columns."
                        )
                    )
                )
            )
        )
    })
}
