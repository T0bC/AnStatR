# Help modal display logic
# This file defines a function that shows a help modal when the help button is clicked
#
# @param input Shiny input object
# @param session Shiny session object
# @return NULL (side effects: displays modal dialog)

handle_help_button <- function(input, session) {
    shiny::observeEvent(input$helpButton, {
        shiny::showModal(
            shiny::modalDialog(
                title = "Calculating the Median of Multiple Measurements",
                easyClose = TRUE,
                size = "l",
                shinyBS::bsCollapse(
                    multiple = TRUE,
                    shinyBS::bsCollapsePanel(
                        "Filtering Values",
                        shiny::includeMarkdown("docs/median_calculation/MEDIAN_help_filter.md")
                    ),
                    shinyBS::bsCollapsePanel(
                        "Median Calculation",
                        shiny::includeMarkdown("docs/median_calculation/MEDIAN_help.md")
                    ),
                    shinyBS::bsCollapsePanel(
                        "Example Data",
                        style = "success",
                        shiny::tags$img(
                            src = base64enc::dataURI(
                                file = "www/images/MEDIAN_help_DF_new.PNG",
                                mime = "image/png"
                            ),
                            alt = "Example for a data frame, with multiple groups and subgrouping columns."
                        )
                    ),
                    open = "Filtering Values"
                )
            )
        )
    })
}
