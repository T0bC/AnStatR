box::use(
  app/logic/shared/data_utils,
)

# =============================================================================
# Training filter specifications
#
# A filter spec records which row subset a model was fitted on, so the
# same subset can be reproduced on unknown data in the Prediction tab.
# Without it, a model fitted on "M1, buccal facet only" would silently be
# applied to a test set containing every tooth and facet.
#
# Not every filtered column can be reapplied. Metadata columns play three
# roles at training time:
#
#   Structural  (TOOTH, FACET, JAW)   shared vocabulary  -> reapply
#   Identity    (SPECIMEN_ID, SITE)   new specimens      -> do not reapply
#   Outcome     (LIVELIHOOD, DIET)    absent from test   -> do not reapply
#
# Reapplying an identity filter to a test set yields zero rows, so the
# split is recorded explicitly rather than inferred at prediction time.
#
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Current filter spec schema version
spec_version <- 1L

# A column is treated as identity-like (not reapplicable) when it has
# more distinct values than this fraction of the rows -- the signature of
# a per-specimen identifier rather than a protocol vocabulary.
identity_distinct_fraction <- 0.5

# Above this many distinct values a column is not offered for reapplying
# even in a small dataset, where the fraction rule alone is unreliable.
max_reapply_levels <- 20L

#' Determine which filtered columns actually constrain the data
#'
#' A column whose every level is selected is not a constraint and must
#' not enter a spec, or every bundle would carry meaningless entries.
#'
#' @param filter_state Named list of selected values per column
#' @param data Data frame the filter was applied to
#' @return Character vector of constrained column names
#' @export
constrained_cols <- function(filter_state, data) {
  if (length(filter_state) == 0 || is.null(data)) {
    return(character(0))
  }

  cols <- intersect(names(filter_state), names(data))
  keep <- vapply(cols, function(col) {
    selected <- filter_state[[col]]
    if (is.null(selected) || length(selected) == 0) {
      # Empty selection is a no-op in filter_data(), not a constraint
      return(FALSE)
    }
    available <- data_utils$get_filter_choices(data[[col]])
    length(setdiff(available, as.character(selected))) > 0
  }, logical(1))

  cols[keep]
}

#' Suggest which constrained columns should be reapplied to test data
#'
#' Heuristic only -- it sets the default tick in the save dialog and the
#' user decides. Marks a column reapplicable when it looks structural
#' (a small, recurring vocabulary) rather than identity-like.
#'
#' @param filter_state Named list of selected values per column
#' @param data Data frame the filter was applied to
#' @param exclude_cols Character vector of columns never suggested (e.g.
#'   the LDA grouping column, which is the outcome being predicted)
#' @return Character vector of suggested column names
#' @export
suggest_reapply_cols <- function(filter_state, data,
                                 exclude_cols = character(0)) {
  cols <- setdiff(constrained_cols(filter_state, data), exclude_cols)
  if (length(cols) == 0) {
    return(character(0))
  }

  n_rows <- nrow(data)
  keep <- vapply(cols, function(col) {
    values <- as.character(data[[col]])
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(FALSE)
    }

    n_distinct <- length(unique(values))
    if (n_distinct > max_reapply_levels) {
      return(FALSE)
    }
    if (n_rows > 0 &&
          n_distinct > identity_distinct_fraction * n_rows) {
      return(FALSE)
    }
    # Levels must recur, otherwise the column identifies rows
    stats::median(as.numeric(table(values))) >= 2
  }, logical(1))

  cols[keep]
}

#' Summarise a filter selection against the data it was applied to
#'
#' Must be given the **unfiltered** data: whether a column is
#' constrained can only be judged against the levels that were
#' available before filtering. Returns small character vectors so the
#' caller can record the outcome without retaining a second copy of a
#' potentially very large data frame.
#'
#' @param filter_state Named list of selected values per column
#' @param data Data frame **before** the filter was applied
#' @param exclude_cols Character vector never suggested for reapplying
#' @return List with $constrained and $suggested character vectors and
#'   $level_counts, the number of distinct levels each constrained
#'   column had before filtering (shown in the save dialog so a
#'   4-level TOOTH is distinguishable from a 480-level SPECIMEN_ID)
#' @export
analyze_filter <- function(filter_state, data,
                           exclude_cols = character(0)) {
  constrained <- constrained_cols(filter_state, data)

  counts <- vapply(constrained, function(col) {
    length(unique(as.character(data[[col]])))
  }, integer(1))
  names(counts) <- constrained

  list(
    constrained = constrained,
    suggested = suggest_reapply_cols(filter_state, data, exclude_cols),
    level_counts = counts
  )
}

#' Build a training filter spec
#'
#' @param filter_state Named list of selected values per column
#' @param constrained Character vector of genuinely constrained columns,
#'   from `analyze_filter()` against the unfiltered data
#' @param reapply_cols Character vector of columns to enforce on test
#'   data; any remaining constrained columns are recorded as
#'   training-only provenance
#' @param n_rows_before Integer, row count before filtering
#' @param n_rows_after Integer, row count after filtering
#' @return A filter spec list, or NULL when nothing is constrained
#' @export
build_filter_spec <- function(filter_state, constrained, reapply_cols,
                              n_rows_before = NULL,
                              n_rows_after = NULL) {
  cols <- intersect(constrained, names(filter_state))
  if (length(cols) == 0) {
    return(NULL)
  }

  reapply_cols <- intersect(reapply_cols, cols)
  training_cols <- setdiff(cols, reapply_cols)

  as_selection <- function(col) as.character(filter_state[[col]])

  reapply <- lapply(reapply_cols, as_selection)
  names(reapply) <- reapply_cols
  training_only <- lapply(training_cols, as_selection)
  names(training_only) <- training_cols

  list(
    version = spec_version,
    reapply = reapply,
    training_only = training_only,
    n_rows_before = n_rows_before,
    n_rows_after = n_rows_after
  )
}

#' Describe a filter spec in one human-readable line
#'
#' @param spec A filter spec, or NULL
#' @return A character string, or NULL when there is nothing to reapply
#' @export
describe_filter_spec <- function(spec) {
  if (is.null(spec) || length(spec$reapply) == 0) {
    return(NULL)
  }

  parts <- vapply(names(spec$reapply), function(col) {
    paste0(col, " = ", paste(spec$reapply[[col]], collapse = ", "))
  }, character(1))

  paste(parts, collapse = "; ")
}

#' Check unknown data against a training filter spec
#'
#' Returns blocking errors rather than warnings: a missing structural
#' column means the comparison is invalid, not merely less informative.
#'
#' @param spec A filter spec, or NULL
#' @param test_data Data frame of unknown observations
#' @return List with $errors (character), $warnings (character) and
#'   $n_matching (integer, rows surviving the filter)
#' @export
check_filter_spec <- function(spec, test_data) {
  empty <- list(
    errors = character(0),
    warnings = character(0),
    n_matching = if (is.null(test_data)) 0L else nrow(test_data)
  )
  if (is.null(spec) || length(spec$reapply) == 0 ||
        is.null(test_data)) {
    return(empty)
  }

  errors <- character(0)

  for (col in names(spec$reapply)) {
    required <- as.character(spec$reapply[[col]])

    if (!col %in% names(test_data)) {
      errors <- c(errors, paste0(
        "The model was trained on ", col, " = ",
        paste(required, collapse = ", "),
        ", but the unknown data has no '", col, "' column. ",
        "Add it so the same subset can be selected."
      ))
      next
    }

    available <- data_utils$get_filter_choices(test_data[[col]])
    if (length(intersect(required, available)) == 0) {
      errors <- c(errors, paste0(
        "No rows match the model's training filter on '", col,
        "'. Required: ", paste(required, collapse = ", "),
        ". Found: ", paste(available, collapse = ", "),
        ". Check for spelling or upper/lower case differences."
      ))
    }
  }

  if (length(errors) > 0) {
    return(list(errors = errors, warnings = character(0), n_matching = 0L))
  }

  n_matching <- nrow(apply_filter_spec(spec, test_data))
  if (n_matching == 0) {
    errors <- c(errors, paste0(
      "No rows match the combined training filter (",
      describe_filter_spec(spec),
      "). Each column matches on its own, but no row satisfies",
      " all of them together."
    ))
  }

  list(
    errors = errors,
    warnings = character(0),
    n_matching = n_matching
  )
}

#' Apply a training filter spec to unknown data
#'
#' Delegates to `data_utils$filter_data()` so the matching semantics
#' cannot drift from the filter UI that produced the spec.
#'
#' @param spec A filter spec, or NULL
#' @param test_data Data frame of unknown observations
#' @return The filtered data frame (unchanged when there is no spec)
#' @export
apply_filter_spec <- function(spec, test_data) {
  if (is.null(spec) || length(spec$reapply) == 0 ||
        is.null(test_data)) {
    return(test_data)
  }

  present <- intersect(names(spec$reapply), names(test_data))
  if (length(present) == 0) {
    return(test_data)
  }

  filters <- spec$reapply[present]
  data_utils$filter_data(test_data, filters)
}
