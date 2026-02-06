box::use(
  openxlsx,
  rhino,
  utils[read.csv],
  tools[file_ext],
)

box::use(
  app/logic/error_handling,
)

#' Validate that a filename has a supported extension
#' @param filename Character, the original filename (not the temp path)
#' @return List with `valid` (logical) and `ext` (character, lowercased extension)
#' @export
validate_file_extension <- function(filename) {
  ext <- tolower(file_ext(filename))
  valid <- ext %in% c("csv", "xlsx")
  if (!valid) {
    rhino$log$warn("Unsupported file extension: '{ext}' from '{filename}'")
  }
  list(valid = valid, ext = ext)
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

#' Read a data file (CSV or XLSX) and return a data.frame or error
#' @param path Character, path to the file on disk
#' @param ext Character, file extension ("csv" or "xlsx")
#' @param header Logical, does the CSV have a header row?
#' @param delimiter Character, CSV field separator
#' @param quote_char Character, CSV quote character (already normalized)
#' @return List with `success` (logical), `data` (data.frame or NULL),
#'   and `error` (structured error object or NULL)
#' @export
read_data_file <- function(path, ext, header = TRUE, delimiter = ",",
                           quote_char = '"') {
  result <- error_handling$safe_execute(
    expr = suppressWarnings(
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
    ),
    operation_name = "Data Import",
    context = list(file = path, format = ext),
    error_parser = error_handling$default_error_parser
  )

  if (!result$success) {
    return(list(success = FALSE, data = NULL, error = result$error))
  }

  rhino$log$info(
    "File read successfully: {ext} ({nrow(result$result)} rows, "
  )
  list(success = TRUE, data = result$result, error = NULL)
}

#' Validate that a loaded data.frame is usable
#' @param data The object returned from reading a file
#' @return List with `valid` (logical) and `message` (character or NULL)
#' @export
validate_data <- function(data) {
  if (!is.data.frame(data)) {
    rhino$log$warn("Validation failed: not a data.frame")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "The uploaded file did not produce a data frame.",
        operation_name = "Data Validation"
      )
    ))
  }
  if (nrow(data) == 0) {
    rhino$log$warn("Validation failed: data.frame has 0 rows")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "The uploaded file appears to be empty.",
        operation_name = "Data Validation"
      )
    ))
  }
  rhino$log$info("Data validation passed: {nrow(data)} rows, {ncol(data)} cols")
  list(valid = TRUE, error = NULL)
}
