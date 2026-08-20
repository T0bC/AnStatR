box::use(
  bsicons,
  shiny,
)

# =============================================================================
# Shared helpers for the opt-in cross-validation controls in the PCA and LDA
# sidebars: a coarse up-front runtime estimate, and parsing of the user's
# keepX candidate grid.
#
# The estimate exists so the user can give informed consent before starting a
# slow job. It is deliberately vague — an honestly rough range is more useful
# than a precise-looking number that turns out wrong on their hardware.
# =============================================================================

# Rough per-fit cost constants, in seconds per million cell-operations.
# Calibrated to be pessimistic rather than optimistic: over-promising speed
# is worse for the user than warning them about a job that finishes early.
CV_COST_CONSTANTS <- list(
  spca = 4.0e-7,
  splsda = 9.0e-7,
  perf = 5.0e-7
)


#' Estimate the runtime of a cross-validated tuning run
#'
#' Work scales roughly with the number of model fits
#' (folds x repeats x grid size x components) times the cost of one
#' fit (samples x variables). Accurate to an order of magnitude,
#' which is all that is needed to set expectations.
#'
#' @param n_samples Integer, rows in the analysis-ready data
#' @param n_vars Integer, measurement columns
#' @param folds Integer, CV folds
#' @param repeats Integer, CV repeats
#' @param n_grid Integer, number of candidate keepX values (1 for
#'   perf(), which does not search a grid)
#' @param ncomp Integer, number of components
#' @param method Character, one of "spca", "splsda", "perf"
#' @return List with $seconds, $label and $tier
#'   ("fast", "moderate" or "slow"), or NULL when inputs are
#'   unusable (the caller should then render nothing)
#' @export
estimate_cv_runtime <- function(n_samples = NULL, n_vars = NULL,
                                folds = NULL, repeats = NULL,
                                n_grid = 1, ncomp = 2,
                                method = "spca") {
  # Everything defaults to NULL: these are fed straight from Shiny
  # inputs, which are NULL before the UI has initialised. A missing
  # estimate must degrade to "render nothing", never to an error
  # inside renderUI.
  vals <- list(
    n_samples = n_samples, n_vars = n_vars, folds = folds,
    repeats = repeats, n_grid = n_grid, ncomp = ncomp
  )
  ok <- vapply(vals, function(v) {
    !is.null(v) && length(v) == 1 && is.finite(suppressWarnings(
      as.numeric(v)
    )) && as.numeric(v) > 0
  }, logical(1))
  if (!all(ok)) return(NULL)

  vals <- lapply(vals, as.numeric)
  constant <- CV_COST_CONSTANTS[[method]]
  if (is.null(constant)) constant <- CV_COST_CONSTANTS$spca

  n_fits <- vals$folds * vals$repeats * vals$n_grid * vals$ncomp
  cell_ops <- vals$n_samples * vals$n_vars
  seconds <- constant * n_fits * cell_ops

  # Nothing meaningful ever completes faster than the round trip.
  seconds <- max(seconds, 1)

  list(
    seconds = seconds,
    label = humanise_seconds(seconds),
    tier = if (seconds < 20) {
      "fast"
    } else if (seconds < 120) {
      "moderate"
    } else {
      "slow"
    }
  )
}


#' Render an estimate as a rounded, deliberately coarse phrase
#'
#' @param seconds Numeric
#' @return Character
humanise_seconds <- function(seconds) {
  if (seconds < 10) {
    "a few seconds"
  } else if (seconds < 60) {
    paste0("around ", 10 * round(seconds / 10), " seconds")
  } else if (seconds < 300) {
    minutes <- round(seconds / 60)
    paste0(
      "roughly ", minutes, "-", minutes + 1, " minutes"
    )
  } else if (seconds < 1800) {
    paste0("several minutes (about ", round(seconds / 60), ")")
  } else {
    "a long time — well over half an hour"
  }
}


#' Render the runtime estimate beneath a tuning button
#'
#' @param est List from estimate_cv_runtime(), or NULL
#' @return Shiny tag, or NULL when no estimate is available
#' @export
render_runtime_estimate <- function(est) {
  if (is.null(est)) return(NULL)

  if (identical(est$tier, "slow")) {
    return(shiny$tags$div(
      class = "alert alert-warning py-1 px-2 small mt-1 mb-0",
      bsicons$bs_icon("hourglass-split", class = "me-1"),
      shiny$tags$strong("Estimated runtime: "),
      est$label,
      shiny$tags$span(
        class = "d-block",
        paste(
          "The app is unresponsive while this runs. Consider",
          "fewer repeats or a shorter keepX grid first."
        )
      )
    ))
  }

  shiny$tags$small(
    class = "text-muted d-block mt-1",
    bsicons$bs_icon("hourglass", class = "me-1"),
    paste0("Estimated runtime: ", est$label, "."),
    if (identical(est$tier, "moderate")) {
      " The app is unresponsive until it finishes."
    }
  )
}


#' Check cross-validation settings against mixOmics' guidance
#'
#' The mixOmics documentation advises at least 5-6 samples per
#' fold, and 50-100 repeats for a final reported result. Our
#' defaults are deliberately lower so the buttons stay usable, so
#' this states plainly when a result should be treated as
#' provisional rather than quietly presenting it as final.
#'
#' @param n_samples Integer, rows in the analysis-ready data
#' @param folds Integer, CV folds
#' @param repeats Integer, CV repeats
#' @return Character vector of advisory messages (empty when the
#'   settings follow the guidance)
#' @export
check_cv_settings <- function(n_samples = NULL, folds = NULL,
                              repeats = NULL) {
  msgs <- character(0)

  usable <- function(v) {
    !is.null(v) && length(v) == 1 &&
      is.finite(suppressWarnings(as.numeric(v))) &&
      as.numeric(v) > 0
  }

  if (usable(n_samples) && usable(folds)) {
    per_fold <- as.numeric(n_samples) / as.numeric(folds)
    if (per_fold < 3) {
      msgs <- c(msgs, paste0(
        "Only about ", floor(per_fold), " sample(s) per fold. ",
        "mixOmics needs at least 3 and this run will likely ",
        "fail; reduce the fold count."
      ))
    } else if (per_fold < 5) {
      msgs <- c(msgs, paste0(
        "Only about ", floor(per_fold), " samples per fold. ",
        "mixOmics advises 5-6 as a minimum, so the error ",
        "estimate will be noisy — consider fewer folds."
      ))
    }
  }

  if (usable(repeats) && as.numeric(repeats) < 50) {
    msgs <- c(msgs, paste0(
      "mixOmics advises 50-100 repeats for a final reported ",
      "result; ", repeats, " gives a quicker, provisional ",
      "answer. Raise it before quoting these numbers in a ",
      "publication."
    ))
  }

  msgs
}


#' Render CV setting advisories beneath a tuning control
#'
#' @param msgs Character vector from check_cv_settings()
#' @return Shiny tag, or NULL when there is nothing to say
#' @export
render_cv_advice <- function(msgs) {
  if (length(msgs) == 0) return(NULL)
  shiny$tags$small(
    class = "text-muted d-block mt-1",
    bsicons$bs_icon("info-circle", class = "me-1"),
    paste(msgs, collapse = " ")
  )
}


#' Parse a user-entered keepX candidate grid
#'
#' Accepts comma- or space-separated integers. Values above the
#' available variable count are capped rather than rejected, so a
#' grid carried over from a wider dataset still works.
#'
#' @param text Character, the raw input value
#' @param n_vars Integer, number of available measurement columns
#' @return List with $values (integer vector, or NULL when nothing
#'   usable was entered) and $message (character, or NULL) —
#'   $values NULL means "fall back to the default grid"
#' @export
parse_keepx_grid <- function(text, n_vars) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(values = NULL, message = NULL))
  }

  parts <- unlist(strsplit(text, "[,[:space:]]+"))
  parts <- parts[nzchar(parts)]
  nums <- suppressWarnings(as.numeric(parts))

  if (length(nums) == 0 || all(is.na(nums))) {
    return(list(
      values = NULL,
      message = paste(
        "Could not read the keepX grid; using the default",
        "values instead."
      )
    ))
  }

  dropped <- sum(is.na(nums) | nums < 1)
  nums <- nums[!is.na(nums) & nums >= 1]
  if (length(nums) == 0) {
    return(list(
      values = NULL,
      message = paste(
        "The keepX grid contained no usable values; using the",
        "default values instead."
      )
    ))
  }

  capped <- any(nums > n_vars)
  values <- sort(unique(pmin(as.integer(nums), as.integer(n_vars))))

  notes <- c(
    if (dropped > 0) {
      paste0(dropped, " unreadable value(s) ignored.")
    },
    if (capped) {
      paste0("Values above ", n_vars, " capped to ", n_vars, ".")
    }
  )

  list(
    values = values,
    message = if (length(notes) > 0) {
      paste(notes, collapse = " ")
    }
  )
}
