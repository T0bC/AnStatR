box::use(
  bslib,
)

#' Application version string
#' @export
app_version <- "2.0.0"

#' Available theme definitions
#'
#' Named list mapping display names to bslib theme objects.
#' @export
available_themes <- list(
  "Default (Light)" = bslib$bs_theme(preset = "bootstrap"),
  "Flatly (Light)" = bslib$bs_theme(preset = "flatly"),
  "Cosmo (Light)" = bslib$bs_theme(preset = "cosmo"),
  "Lumen (Light)" = bslib$bs_theme(preset = "lumen"),
  "Darkly (Dark)" = bslib$bs_theme(preset = "darkly"),
  "Cyborg (Dark)" = bslib$bs_theme(preset = "cyborg"),
  "Slate (Dark)" = bslib$bs_theme(preset = "slate"),
  "Solar (Dark)" = bslib$bs_theme(preset = "solar")
)

#' Default theme name
#' @export
default_theme_name <- "Cosmo (Light)"

#' Get the default theme object
#' @return A bslib theme object
#' @export
get_default_theme <- function() {
  available_themes[[default_theme_name]]
}

#' Get theme names
#' @return Character vector of available theme display names
#' @export
get_theme_names <- function() {
  names(available_themes)
}

#' Get a theme by name
#' @param name Character, display name of the theme
#' @return A bslib theme object, or the default if name is invalid
#' @export
get_theme <- function(name) {
  if (is.null(name) || !name %in% names(available_themes)) {
    return(available_themes[[default_theme_name]])
  }
  available_themes[[name]]
}
