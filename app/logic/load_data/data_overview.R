# =============================================================================
# Data overview and missingness analysis (pure logic, no Shiny)
#
# Feeds the Load tab's Overview panel, quality-flag banner, and the three
# missingness views. All functions take a data frame and return plain
# lists / data frames so they can be unit tested without a running app.
# =============================================================================

box::use(
  stats[complete.cases],
)

box::use(
  app/logic/shared/column_utils,
)

#' Safe percentage helper (returns 0 when the denominator is 0)
#' @param part Numeric numerator
#' @param whole Numeric denominator
#' @return Numeric percentage rounded to one decimal
pct_of <- function(part, whole) {
  if (is.null(whole) || length(whole) == 0 || whole == 0) {
    return(0)
  }
  round(100 * part / whole, 1)
}

#' Compute headline statistics for the Overview panel
#'
#' Column classification is delegated to column_utils so the app-wide
#' naming convention stays in one place. Note that descriptive +
#' measurement does not necessarily equal the total column count:
#' uppercase-with-digits names (e.g. LOT_STEP1) are ambiguous and are
#' reported separately.
#'
#' @param data A data frame
#' @return List of overview statistics
#' @export
compute_overview_stats <- function(data) {
  if (is.null(data) || !is.data.frame(data)) {
    data <- data.frame()
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  classification <- column_utils$validate_column_naming(data)

  n_cells <- n_rows * n_cols
  n_missing_cells <- if (n_cells == 0) 0 else sum(is.na(data))

  n_complete_rows <- if (n_rows == 0 || n_cols == 0) {
    n_rows
  } else {
    sum(complete.cases(data))
  }

  list(
    n_rows = n_rows,
    n_cols = n_cols,
    n_descriptive = length(classification$descriptive_cols),
    n_measurement = length(classification$measurement_cols),
    n_ambiguous = length(classification$ambiguous_cols),
    n_cells = n_cells,
    n_missing_cells = n_missing_cells,
    pct_missing_cells = pct_of(n_missing_cells, n_cells),
    n_complete_rows = n_complete_rows,
    pct_complete_rows = pct_of(n_complete_rows, n_rows)
  )
}

#' Detect structural data-quality problems
#'
#' Flags issues that the summary table cannot surface: column names that
#' collide when case is ignored, columns with no usable variation, and
#' columns that are mostly or entirely empty.
#'
#' @param data A data frame
#' @param high_missing_threshold Proportion at or above which a column is
#'   flagged as mostly missing
#' @return List with `has_issues` and `flags` (list of
#'   `list(type, label, columns)`)
#' @export
detect_quality_flags <- function(data, high_missing_threshold = 0.5) {
  flags <- list()

  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(list(has_issues = FALSE, flags = flags))
  }

  col_names <- names(data)
  n_rows <- nrow(data)

  # --- Column names that differ only by case ---
  lower_names <- tolower(col_names)
  dup_keys <- unique(lower_names[duplicated(lower_names)])
  if (length(dup_keys) > 0) {
    dup_cols <- col_names[lower_names %in% dup_keys]
    flags[[length(flags) + 1]] <- list(
      type = "duplicate_names",
      label = paste(
        "Column names that differ only by capitalisation.",
        "Downstream selections may pick the wrong one."
      ),
      columns = dup_cols
    )
  }

  n_missing <- vapply(data, function(x) sum(is.na(x)), numeric(1))

  # --- Entirely empty columns ---
  empty_cols <- col_names[n_rows > 0 & n_missing == n_rows]
  if (length(empty_cols) > 0) {
    flags[[length(flags) + 1]] <- list(
      type = "empty_cols",
      label = "Columns that are entirely empty.",
      columns = empty_cols
    )
  }

  n_unique <- vapply(
    data,
    function(x) length(unique(x[!is.na(x)])),
    numeric(1)
  )

  # --- Mostly missing columns ---
  high_missing <- character(0)
  if (n_rows > 0) {
    prop_missing <- n_missing / n_rows
    high_missing <- col_names[
      prop_missing >= high_missing_threshold & !(col_names %in% empty_cols)
    ]
    if (length(high_missing) > 0) {
      flags[[length(flags) + 1]] <- list(
        type = "high_missing",
        label = paste0(
          "Columns missing at least ",
          round(100 * high_missing_threshold),
          "% of their values."
        ),
        columns = high_missing
      )
    }
  }

  # --- Constant columns (no usable variation) ---
  # Mostly-missing columns are already reported above; repeating them
  # here would just pad the banner without telling the user anything new.
  already_flagged <- c(empty_cols, high_missing)
  constant_cols <- col_names[
    n_unique <= 1 & !(col_names %in% already_flagged)
  ]
  if (length(constant_cols) > 0) {
    flags[[length(flags) + 1]] <- list(
      type = "constant_cols",
      label = paste(
        "Columns with a single value throughout.",
        "They cannot be used for grouping or comparison."
      ),
      columns = constant_cols
    )
  }

  list(has_issues = length(flags) > 0, flags = flags)
}

#' Names of columns containing at least one NA
#' @param data A data frame
#' @return Character vector of column names
#' @export
na_bearing_cols <- function(data) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(character(0))
  }
  names(data)[vapply(data, function(x) anyNA(x), logical(1))]
}

#' Missing-value counts per column, restricted to affected columns
#'
#' Columns with no missing values are deliberately excluded: plotting a
#' bar for every column is what makes the default missingness plot
#' unreadable on wide data sets.
#'
#' @param data A data frame
#' @return List with `data` (data frame of column, n_missing, pct_missing,
#'   sorted descending), `n_complete_cols` and `n_total_cols`
#' @export
missing_by_column <- function(data) {
  empty_result <- list(
    data = data.frame(
      column = character(0),
      n_missing = numeric(0),
      pct_missing = numeric(0),
      stringsAsFactors = FALSE
    ),
    n_complete_cols = 0,
    n_total_cols = 0
  )

  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(empty_result)
  }

  n_rows <- nrow(data)
  n_missing <- vapply(data, function(x) sum(is.na(x)), numeric(1))
  affected <- n_missing > 0

  result <- data.frame(
    column = names(data)[affected],
    n_missing = unname(n_missing[affected]),
    pct_missing = pct_of(unname(n_missing[affected]), n_rows),
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$n_missing, result$column), , drop = FALSE]
  rownames(result) <- NULL

  list(
    data = result,
    n_complete_cols = sum(!affected),
    n_total_cols = ncol(data)
  )
}

#' Missingness signature for every row, over NA-bearing columns only
#'
#' @param data A data frame
#' @param cols Character vector of columns to consider
#' @return Character vector of "0101"-style signatures, one per row
row_signatures <- function(data, cols) {
  if (length(cols) == 0 || nrow(data) == 0) {
    return(rep("", nrow(data)))
  }
  flags <- vapply(
    cols,
    function(col) as.integer(is.na(data[[col]])),
    integer(nrow(data))
  )
  flags <- matrix(flags, nrow = nrow(data))
  apply(flags, 1, paste, collapse = "")
}

#' Most frequent co-occurring missingness patterns
#'
#' Reports whichever patterns exist rather than assuming missingness is
#' structured. When every pattern occurs only once the `all_singleton`
#' flag is set so the UI can say so instead of showing a misleading
#' top-N chart.
#'
#' @param data A data frame
#' @param top_n Maximum number of patterns to return
#' @return List with `data` (pattern_id, label, columns, n_rows, pct_rows),
#'   `n_patterns`, `n_hidden`, `n_na_cols` and `all_singleton`
#' @export
missing_patterns <- function(data, top_n = 8) {
  empty_result <- list(
    data = data.frame(
      pattern_id = character(0),
      label = character(0),
      n_cols_missing = numeric(0),
      n_rows = numeric(0),
      pct_rows = numeric(0),
      stringsAsFactors = FALSE
    ),
    n_patterns = 0,
    n_hidden = 0,
    n_na_cols = 0,
    all_singleton = FALSE
  )

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(empty_result)
  }

  cols <- na_bearing_cols(data)
  if (length(cols) == 0) {
    return(empty_result)
  }

  n_rows <- nrow(data)
  signatures <- row_signatures(data, cols)
  counts <- table(signatures)
  counts <- sort(counts, decreasing = TRUE)

  labels <- vapply(
    names(counts),
    function(sig) {
      missing_cols <- cols[strsplit(sig, "")[[1]] == "1"]
      if (length(missing_cols) == 0) {
        "Complete rows"
      } else {
        paste(missing_cols, collapse = " + ")
      }
    },
    character(1)
  )

  n_cols_missing <- vapply(
    names(counts),
    function(sig) sum(strsplit(sig, "")[[1]] == "1"),
    numeric(1)
  )

  result <- data.frame(
    pattern_id = names(counts),
    label = unname(labels),
    n_cols_missing = unname(n_cols_missing),
    n_rows = as.numeric(counts),
    pct_rows = pct_of(as.numeric(counts), n_rows),
    stringsAsFactors = FALSE
  )

  n_patterns <- nrow(result)
  kept <- result[seq_len(min(top_n, n_patterns)), , drop = FALSE]
  rownames(kept) <- NULL

  list(
    data = kept,
    n_patterns = n_patterns,
    n_hidden = max(0, n_patterns - nrow(kept)),
    n_na_cols = length(cols),
    all_singleton = all(result$n_rows <= 1)
  )
}

#' Long-format data for the row-by-column missingness raster
#'
#' Rows are ordered by their missingness signature so that rows sharing a
#' pattern sit together and any block structure becomes visible. When the
#' data set exceeds `max_rows` a systematic sample is taken *after*
#' ordering, which preserves the visual structure while keeping the plot
#' cheap to render.
#'
#' @param data A data frame
#' @param max_rows Maximum number of rows to render
#' @return List with `data` (row_id, column, is_missing), `n_rows_shown`,
#'   `n_rows_total`, `downsampled` and `n_na_cols`
#' @export
missing_raster_data <- function(data, max_rows = 2000) {
  empty_result <- list(
    data = data.frame(
      row_id = numeric(0),
      column = character(0),
      is_missing = logical(0),
      stringsAsFactors = FALSE
    ),
    n_rows_shown = 0,
    n_rows_total = 0,
    downsampled = FALSE,
    n_na_cols = 0
  )

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(empty_result)
  }

  cols <- na_bearing_cols(data)
  if (length(cols) == 0) {
    return(empty_result)
  }

  n_rows_total <- nrow(data)
  signatures <- row_signatures(data, cols)

  # Order by signature so identical patterns form contiguous blocks.
  row_order <- order(signatures, seq_len(n_rows_total))

  downsampled <- n_rows_total > max_rows
  if (downsampled) {
    keep <- unique(round(seq(1, n_rows_total, length.out = max_rows)))
    row_order <- row_order[keep]
  }

  subset_data <- data[row_order, cols, drop = FALSE]
  n_rows_shown <- nrow(subset_data)

  # Keep the ranked column order so the worst offenders sit together.
  col_levels <- missing_by_column(data)$data$column
  col_levels <- col_levels[col_levels %in% cols]

  long <- data.frame(
    row_id = rep(seq_len(n_rows_shown), times = length(cols)),
    column = factor(
      rep(cols, each = n_rows_shown),
      levels = col_levels
    ),
    is_missing = as.vector(
      vapply(
        subset_data,
        function(x) is.na(x),
        logical(n_rows_shown)
      )
    ),
    stringsAsFactors = FALSE
  )

  list(
    data = long,
    n_rows_shown = n_rows_shown,
    n_rows_total = n_rows_total,
    downsampled = downsampled,
    n_na_cols = length(cols)
  )
}
