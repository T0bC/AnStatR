#' KMO Results Display Component
#'
#' Renders KMO measure results with color-coded interpretation in collapsible panels.

#' Render KMO results in collapsible accordion panels
#'
#' @param kmo_result List with overall and individual KMO values
#' @return Shiny tags object with formatted KMO display
render_kmo_results <- function(kmo_result) {
    overall_kmo <- kmo_result$overall
    individual_kmo <- kmo_result$individual
    
    # Info banner if rows were removed due to NA
    na_info <- NULL
    if (!is.null(kmo_result$rows_removed) && kmo_result$rows_removed > 0) {
        na_info <- shiny::tags$div(
            class = "alert alert-info d-flex align-items-center mb-3",
            role = "alert",
            bsicons::bs_icon("info-circle-fill", class = "me-2"),
            sprintf(
                "%d of %d rows were excluded due to missing values in selected columns.",
                kmo_result$rows_removed,
                kmo_result$original_rows
            )
        )
    }
    
    shiny::tagList(
        na_info,
        bslib::accordion(
        id = "kmo_accordion",
        open = "overall_kmo",
        bslib::accordion_panel(
            title = shiny::tags$span(
                bsicons::bs_icon("speedometer2", class = "me-2"),
                "Overall KMO"
            ),
            value = "overall_kmo",
            shiny::tags$div(
                class = "d-flex align-items-center gap-3",
                shiny::tags$div(
                    class = paste("badge fs-5", kmo_badge_class(overall_kmo)),
                    sprintf("%.3f", overall_kmo)
                ),
                shiny::tags$span(
                    class = "text-muted",
                    kmo_interpretation(overall_kmo)
                )
            )
        ),
        bslib::accordion_panel(
            title = shiny::tags$span(
                bsicons::bs_icon("list-ul", class = "me-2"),
                "Individual Variable KMO"
            ),
            value = "individual_kmo",
            render_kmo_table(individual_kmo)
        )
    ))
}

#' Get badge class based on KMO value
#'
#' @param kmo Numeric KMO value
#' @return Character CSS class for badge
kmo_badge_class <- function(kmo) {
    if (kmo >= 0.8) "bg-success"
    else if (kmo >= 0.6) "bg-warning text-dark"
    else "bg-danger"
}

#' Get interpretation text for KMO value
#'
#' @param kmo Numeric KMO value
#' @return Character interpretation
kmo_interpretation <- function(kmo) {
    if (kmo >= 0.9) "Marvelous"
    else if (kmo >= 0.8) "Meritorious"
    else if (kmo >= 0.7) "Middling"
    else if (kmo >= 0.6) "Mediocre"
    else if (kmo >= 0.5) "Miserable"
    else "Unacceptable"
}

#' Render individual KMO values as a table
#'
#' @param individual_kmo Named numeric vector of individual KMO values
#' @return Shiny tags object with table
render_kmo_table <- function(individual_kmo) {
    sorted_kmo <- sort(individual_kmo, decreasing = TRUE)
    
    rows <- lapply(names(sorted_kmo), function(var) {
        val <- sorted_kmo[[var]]
        shiny::tags$tr(
            shiny::tags$td(var),
            shiny::tags$td(
                class = "text-end",
                shiny::tags$span(
                    class = paste("badge", kmo_badge_class(val)),
                    sprintf("%.3f", val)
                )
            )
        )
    })
    
    shiny::tags$table(
        class = "table table-sm table-striped",
        shiny::tags$thead(
            shiny::tags$tr(
                shiny::tags$th("Variable"),
                shiny::tags$th(class = "text-end", "KMO")
            )
        ),
        shiny::tags$tbody(rows)
    )
}
