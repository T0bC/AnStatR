box::use(
  bslib,
)

#' Path to the changelog file
changelog_path <- system.file("../CHANGELOG.md", package = "app")
if (changelog_path == "") {
  changelog_path <- file.path(getwd(), "CHANGELOG.md")
}

#' Read and cache changelog content
#' @return Character vector of changelog lines
get_changelog_content <- function() {
  if (file.exists(changelog_path)) {
    readLines(changelog_path, warn = FALSE)
  } else {
    character(0)
  }
}

#' Parse version info from changelog
#' Extracts version and date from first ## [x.x.x] - YYYY-MM-DD line
#' @return List with version and date elements
#' @export
get_version_info <- function() {
  lines <- get_changelog_content()
  version_pattern <- "^## \\[([0-9]+\\.[0-9]+\\.[0-9]+)\\] - ([0-9]{4}-[0-9]{2}-[0-9]{2})"

  for (line in lines) {
    match <- regmatches(line, regexec(version_pattern, line))[[1]]
    if (length(match) == 3) {
      return(list(
        version = match[2],
        date = match[3]
      ))
    }
  }
  list(version = "unknown", date = "unknown")
}

#' Get formatted version string with date
#' @return Character string like "2.0.0 (2024-03-04)"
#' @export
get_version_string <- function() {
  info <- get_version_info()
  paste0(info$version, " (", info$date, ")")
}

#' Get changelog as markdown text
#' @return Character string of full changelog content
#' @export
get_changelog_markdown <- function() {
  paste(get_changelog_content(), collapse = "\n")
}

#' Application version string (for backward compatibility)
#' @export
app_version <- get_version_info()$version

#' Get the default theme object
#' @return A bslib theme object
#' @export
get_default_theme <- function() {
  bslib$bs_theme(preset = "cosmo")
}
