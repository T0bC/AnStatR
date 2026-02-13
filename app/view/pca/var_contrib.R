box::use(
  ggiraph,
  shiny,
)

box::use(
  app/logic/pca/var_contrib[create_var_contrib_plot],
)

#' Render variable contribution bar chart output
#'
#' Wires up the ggiraph output for the variable contribution
#' bar chart. Reads dimX from sidebar to select which dimension
#' to display. Uses debounced params to avoid flicker.
#' Called by the parent pca module using dependency injection.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param pca_result Reactive returning the PCA computation
#'   result wrapper (with $success and $result)
#' @export
render_output <- function(input, output, session,
                          pca_result) {
  ns <- session$ns

  # Debounced params for dimension + title inputs
  cached_params <- shiny$reactiveVal(NULL)

  make_fingerprint <- function(params) {
    paste(params$dim, params$show_title, sep = "|")
  }

  shiny$observe({
    new_params <- list(
      dim = input$dimX,
      show_title = input$title
    )

    current <- cached_params()
    new_fp <- make_fingerprint(new_params)
    old_fp <- if (!is.null(current)) {
      make_fingerprint(current)
    } else {
      ""
    }
    if (new_fp != old_fp) {
      cached_params(new_params)
    }
  }) |> shiny$debounce(400)

  vc_params <- shiny$reactive({ cached_params() })

  output$var_contrib <- ggiraph$renderGirafe({
    pca_res <- pca_result()
    if (is.null(pca_res)) return(NULL)
    if (!pca_res$success) return(NULL)

    params <- vc_params()
    if (is.null(params)) return(NULL)

    dim <- params$dim
    if (is.null(dim)) dim <- "Dim.1"
    show_title <- isTRUE(params$show_title)

    plot_res <- create_var_contrib_plot(
      pca_result = pca_res$result,
      dim = dim,
      show_title = show_title
    )

    if (!plot_res$success) return(NULL)

    ggiraph$girafe(
      ggobj = plot_res$result,
      width_svg = 7,
      height_svg = 5,
      options = list(
        ggiraph$opts_hover(
          css = paste0(
            "fill-opacity:0.8;",
            "stroke:black;stroke-width:2px;"
          )
        ),
        ggiraph$opts_tooltip(
          css = paste0(
            "background-color:white;padding:8px;",
            "border-radius:4px;",
            "border:1px solid #ccc;",
            "font-family:sans-serif;"
          ),
          use_fill = FALSE
        ),
        ggiraph$opts_selection(type = "none")
      )
    )
  })
}
