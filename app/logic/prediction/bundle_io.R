box::use(
  rhino,
  tools[file_ext],
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Bundle I/O for the prediction module
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Required top-level fields in a prediction bundle
REQUIRED_FIELDS <- c(
  "analysis_type", "model", "raw_data", "used_data",
  "numeric_cols", "app_version", "created"
)

#' Valid analysis types
VALID_TYPES <- c("pca", "lda", "mda", "qda")

#' Load and validate an RDS bundle
#'
#' Reads an .rds file, checks that it contains the
#' expected bundle structure, and returns the bundle
#' or a structured error.
#'
#' @param path Character, path to the .rds file
#' @return List with $success, $result (bundle) or $error
#' @export
load_bundle <- function(path) {
  error_handling$safe_execute(
    expr = {
      if (!file.exists(path)) {
        stop("File does not exist: ", path)
      }

      ext <- tolower(file_ext(path))
      if (ext != "rds") {
        stop(paste0(
          "Expected an .rds file, got '.",
          ext, "'"
        ))
      }

      bundle <- readRDS(path)

      validation <- validate_bundle(bundle)
      if (!validation$valid) {
        stop(validation$message)
      }

      rhino$log$info(
        "Prediction: loaded {toupper(bundle$analysis_type)}",
        " bundle (v{bundle$app_version},",
        " {length(bundle$numeric_cols)} vars,",
        " {nrow(bundle$used_data)} training obs)"
      )

      bundle
    },
    operation_name = "Load Bundle",
    error_parser = bundle_error_parser
  )
}

#' Validate bundle structure
#'
#' Checks that a loaded object has all required fields
#' and a valid analysis_type.
#'
#' @param bundle The object read from the .rds file
#' @return List with $valid (logical) and $message
#'   (character or NULL)
#' @export
validate_bundle <- function(bundle) {
  if (!is.list(bundle)) {
    return(list(
      valid = FALSE,
      message = paste(
        "The uploaded file does not contain a valid",
        "prediction bundle. Expected a named list."
      )
    ))
  }

  missing <- setdiff(REQUIRED_FIELDS, names(bundle))
  if (length(missing) > 0) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Bundle is missing required fields: ",
        paste(missing, collapse = ", "),
        ". This file may not be a AnStatR prediction",
        " bundle."
      )
    ))
  }

  if (!bundle$analysis_type %in% VALID_TYPES) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Unknown analysis type: '",
        bundle$analysis_type,
        "'. Expected one of: ",
        paste(VALID_TYPES, collapse = ", ")
      )
    ))
  }

  if (is.null(bundle$model)) {
    return(list(
      valid = FALSE,
      message = paste(
        "Bundle does not contain a fitted model.",
        "The model may have been exported with",
        "cross-validation (CV) mode, which does",
        "not store the model object."
      )
    ))
  }

  list(valid = TRUE, message = NULL)
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

bundle_error_parser <- function(error_msg,
                                operation_name =
                                  "Load Bundle") {
  if (grepl("does not exist", error_msg,
            ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": The selected file was not found."
    )
  } else if (grepl("rds", error_msg,
                    ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Please upload a valid .rds bundle file."
    )
  } else if (grepl("missing required", error_msg,
                    ignore.case = TRUE)) {
    paste0(operation_name, ": ", error_msg)
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
