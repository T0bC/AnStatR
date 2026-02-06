box::use(
  openxlsx,
  utils[read.csv],
  tools[file_ext],
)

#' Validate that a filename has a supported extension
#' @param filename Character, the original filename (not the temp path)
#' @return List with `valid` (logical) and `ext` (character, lowercased extension)
#' @export
validate_file_extension <- function(filename) {
  ext <- tolower(file_ext(filename))
  list(
    valid = ext %in% c("csv", "xlsx"),
    ext = ext
  )
}

#' Normalize the CSV quote character from UI input
#' @param quote_char Character from the radio button input
#' @return Character, a valid quote argument for read.csv
#' @export
normalize_quote_char <- function(quote_char) {
  if (is.null(quote_char) ||
      !is.character(quote_char) ||
      length(quote_char) != 1 ||
      quote_char == "" ||
      quote_char == "None") {
    return("")
  }
  quote_char
}

#' Read a data file (CSV or XLSX) and return a data.frame or error message
#' @param path Character, path to the file on disk
#' @param ext Character, file extension ("csv" or "xlsx")
#' @param header Logical, does the CSV have a header row?
#' @param delimiter Character, CSV field separator
#' @param quote_char Character, CSV quote character (already normalized)
#' @return List with `success` (logical), `data` (data.frame or NULL),
#'   and `error` (character or NULL)
#' @export
read_data_file <- function(path, ext, header = TRUE, delimiter = ",",
                           quote_char = '"') {
  tryCatch(
    {
      data <- suppressWarnings(
        if (ext == "csv") {
          read.csv(
            file = path,
            header = header,
            sep = delimiter,
            quote = quote_char,
            stringsAsFactors = FALSE
          )
        } else {
          openxlsx$read.xlsx(
            xlsxFile = path,
            sheet = 1
          )
        }
      )
      list(success = TRUE, data = data, error = NULL)
    },
    error = function(e) {
      list(success = FALSE, data = NULL, error = conditionMessage(e))
    }
  )
}

#' Validate that a loaded data.frame is usable
#' @param data The object returned from reading a file
#' @return List with `valid` (logical) and `message` (character or NULL)
#' @export
validate_data <- function(data) {
  if (!is.data.frame(data)) {
    return(list(valid = FALSE, message = "The uploaded file did not produce a data frame."))
  }
  if (nrow(data) == 0) {
    return(list(valid = FALSE, message = "The uploaded file appears to be empty."))
  }
  list(valid = TRUE, message = NULL)
}
