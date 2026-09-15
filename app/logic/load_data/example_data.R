box::use(
  rhino,
  tools[file_ext],
)

box::use(
  app/logic/load_data/load_data,
)

#' Directory containing bundled example datasets.
#' box::file() resolves relative to this module's own location (app/logic/load_data/),
#' so we navigate up two levels to app/ then into static/example_data/.
example_dir <- box::file("../../static/example_data")

#' Pretty-print a filename as a display name
#' @param filename Character, e.g. "iris.xlsx"
#' @return Character, e.g. "Iris"
#' @keywords internal
display_name <- function(filename) {
  name <- sub("\\.[^.]+$", "", filename)
  paste0(toupper(substring(name, 1, 1)), substring(name, 2))
}

#' List available example datasets
#' @return Named character vector (display name -> filename), or empty vector
#' @export
list_examples <- function() {
  if (!dir.exists(example_dir)) {
    rhino$log$warn("Example data directory not found: '{example_dir}'")
    return(character(0))
  }

  files <- list.files(
    example_dir,
    pattern = "\\.(csv|xlsx)$",
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    return(character(0))
  }

  names(files) <- vapply(files, display_name, character(1))
  files
}

#' Resolve the full path of an example dataset
#' @param filename Character, the filename (e.g. "iris.xlsx")
#' @return Character, full path to the file
#' @export
example_path <- function(filename) {
  file.path(example_dir, filename)
}

#' Load an example dataset
#' @param filename Character, the filename (e.g. "iris.xlsx")
#' @return List with `success`, `data`, `error` — same structure as
#'   load_data$read_data_file()
#' @export
load_example <- function(filename) {
  path <- example_path(filename)

  if (!file.exists(path)) {
    rhino$log$error("Example file not found: '{path}'")
    return(list(
      success = FALSE,
      data = NULL,
      error = list(message = paste0("Example file not found: ", filename))
    ))
  }

  ext <- tolower(file_ext(filename))
  rhino$log$info("Loading example dataset: '{filename}'")

  load_data$read_data_file(
    path = path,
    ext = ext,
    header = TRUE,
    delimiter = ",",
    quote_char = '"'
  )
}
