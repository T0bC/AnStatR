box::use(
  base64enc,
  ggplot2,
  htmltools,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# HTML report generation for statistics results.
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Convert a data frame to an HTML table string
#'
#' @param df Data frame to convert
#' @return Character string with HTML table markup
#' @export
df_to_html_table <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return("<p><em>No data available</em></p>")
  }

  header <- paste0(
    "<tr>",
    paste0(
      "<th>",
      htmltools$htmlEscape(names(df)),
      "</th>",
      collapse = ""
    ),
    "</tr>"
  )

  rows <- apply(df, 1, function(row) {
    cells <- vapply(row, function(x) {
      val <- if (is.numeric(x)) {
        format(round(x, 4), nsmall = 4)
      } else {
        as.character(x)
      }
      paste0(
        "<td>",
        htmltools$htmlEscape(val),
        "</td>"
      )
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })

  paste0(
    "<table>\n<thead>\n", header,
    "\n</thead>\n<tbody>\n",
    paste(rows, collapse = "\n"),
    "\n</tbody>\n</table>"
  )
}


#' Render a ggplot object to a base64-encoded PNG data URI
#'
#' @param plot_object A ggplot object
#' @param width_cm Width in cm (default 16)
#' @param height_cm Height in cm (default 10)
#' @param dpi Resolution (default 150)
#' @return Character string with data URI, or NULL on failure
render_plot_base64 <- function(plot_object,
                               width_cm = 16,
                               height_cm = 10,
                               dpi = 150) {
  if (is.null(plot_object)) return(NULL)

  plot_file <- tempfile(fileext = ".png")
  on.exit(unlink(plot_file), add = TRUE)

  tryCatch(
    {
      ggplot2$ggsave(
        filename = plot_file,
        plot = plot_object,
        width = width_cm / 2.54,
        height = height_cm / 2.54,
        dpi = dpi,
        bg = "white"
      )
      encoded <- base64enc$base64encode(plot_file)
      paste0("data:image/png;base64,", encoded)
    },
    error = function(e) NULL
  )
}


#' Build the omnibus section HTML
#'
#' @param omnibus_result Data frame or app_error or NULL
#' @param x_axis Character vector of grouping columns
#' @param approach Character, test approach name
#' @return Character string with HTML
build_omnibus_html <- function(omnibus_result,
                               x_axis,
                               approach,
                               is_rm = FALSE) {
  if (is.null(omnibus_result)) {
    return("")
  }

  if (error_handling$is_app_error(omnibus_result)) {
    return(paste0(
      '<div class="warning">',
      htmltools$htmlEscape(
        omnibus_result$user_message %||%
          "Omnibus test produced an error."
      ),
      "</div>\n"
    ))
  }

  if (is.data.frame(omnibus_result) &&
      nrow(omnibus_result) > 0) {
    n_ways <- length(x_axis)
    rm_prefix <- if (isTRUE(is_rm)) "RM " else ""
    header_label <- if (approach == "robust") {
      paste0(
        rm_prefix, "Robust ", n_ways, "-Way ANOVA",
        " \u2014 Trimmed Means (t", n_ways, "way)"
      )
    } else if (approach == "nonparametric") {
      paste0(
        rm_prefix, "Non-Parametric ", n_ways,
        "-Way ANOVA"
      )
    } else {
      paste0(rm_prefix, "Classical ", n_ways, "-Way ANOVA")
    }

    return(paste0(
      "<h2>",
      htmltools$htmlEscape(header_label),
      "</h2>\n",
      df_to_html_table(omnibus_result),
      "\n"
    ))
  }

  ""
}


#' Build HTML for mixed RM post-hoc results (Paired + Unpaired)
#'
#' Splits the results by Type column and renders separate tables
#' for paired (within-subject) and unpaired (between-subject) comparisons.
#'
#' @param posthoc_result Data frame with Type column
#' @return Character string with HTML
build_rm_mixed_posthoc_html <- function(posthoc_result) {
  html_parts <- list()
  html_parts[[1]] <- "<h2>Pairwise Comparisons (Repeated Measures)</h2>\n"

  # Split by Type
  paired_df <- posthoc_result[posthoc_result$Type == "Paired", , drop = FALSE]
  unpaired_df <- posthoc_result[posthoc_result$Type == "Unpaired", , drop = FALSE]

  # Helper to clean column names and remove empty columns

  clean_df <- function(df, prefix_pattern, label) {
    if (nrow(df) == 0) return(NULL)

    # Remove Type column
    df$Type <- NULL

    # Keep only columns with data (non-NA)
    cols_with_data <- vapply(df, function(x) !all(is.na(x)), logical(1))
    df <- df[, cols_with_data, drop = FALSE]

    # Clean column names - remove prefixes
    names(df) <- gsub(prefix_pattern, "", names(df))
    names(df) <- gsub("^\\.", "", names(df))

    list(df = df, label = label)
  }

  # Process paired comparisons
  if (nrow(paired_df) > 0) {
    # Detect paired test type
    if (any(grepl("^Paired\\.t\\.", names(paired_df)))) {
      paired_clean <- clean_df(
        paired_df, "^Paired\\.(t\\.|d\\.?)", "Paired t-Test + Cohen's d"
      )
    } else if (any(grepl("^RM\\.Lincon\\.", names(paired_df)))) {
      paired_clean <- clean_df(
        paired_df, "^RM\\.Lincon\\.", "Paired Trimmed Means (Yuen)"
      )
    } else if (any(grepl("^Paired\\.Wilcox\\.", names(paired_df)))) {
      paired_clean <- clean_df(
        paired_df, "^Paired\\.Wilcox\\.", "Paired Wilcoxon Signed-Rank"
      )
    } else {
      paired_clean <- list(df = paired_df, label = "Paired Comparisons")
      paired_clean$df$Type <- NULL
    }

    if (!is.null(paired_clean) && nrow(paired_clean$df) > 0) {
      html_parts[[length(html_parts) + 1]] <- paste0(
        "<h3>Within-Subject (Paired): ",
        htmltools$htmlEscape(paired_clean$label),
        "</h3>\n",
        df_to_html_table(paired_clean$df),
        "\n"
      )
    }
  }

  # Process unpaired comparisons
  if (nrow(unpaired_df) > 0) {
    # Detect unpaired test type
    if (any(grepl("^Tukey\\.", names(unpaired_df)))) {
      unpaired_clean <- clean_df(
        unpaired_df, "^(Tukey\\.|Cohen\\.?)", "Independent t-Test + Cohen's d"
      )
    } else if (any(grepl("^Lincon\\.", names(unpaired_df)))) {
      unpaired_clean <- clean_df(
        unpaired_df, "^Lincon\\.", "Independent Trimmed Means (Yuen)"
      )
    } else if (any(grepl("^Wilcox\\.", names(unpaired_df)))) {
      unpaired_clean <- clean_df(
        unpaired_df, "^Wilcox\\.", "Independent Wilcoxon Rank-Sum"
      )
    } else {
      unpaired_clean <- list(df = unpaired_df, label = "Unpaired Comparisons")
      unpaired_clean$df$Type <- NULL
    }

    if (!is.null(unpaired_clean) && nrow(unpaired_clean$df) > 0) {
      html_parts[[length(html_parts) + 1]] <- paste0(
        "<h3>Between-Subject (Unpaired): ",
        htmltools$htmlEscape(unpaired_clean$label),
        "</h3>\n",
        df_to_html_table(unpaired_clean$df),
        "\n"
      )
    }
  }

  paste0(html_parts, collapse = "")
}


#' Build the post-hoc section HTML
#'
#' Detects column prefixes (Lincon/Cliff vs Tukey/Cohen)
#' and splits into two side-by-side tables.
#'
#' @param posthoc_result Data frame or app_error or NULL
#' @return Character string with HTML
build_posthoc_html <- function(posthoc_result) {
  if (is.null(posthoc_result)) return("")

  if (error_handling$is_app_error(posthoc_result)) {
    return(paste0(
      '<h2>Pairwise Comparisons</h2>\n',
      '<div class="warning">',
      htmltools$htmlEscape(
        posthoc_result$user_message %||%
          "Post-hoc test produced an error."
      ),
      "</div>\n"
    ))
  }

  if (!is.data.frame(posthoc_result) ||
      nrow(posthoc_result) == 0) {
    return("")
  }

  # Check for mixed RM format (has Type column with Paired/Unpaired)
  has_type_col <- "Type" %in% names(posthoc_result)
  if (has_type_col) {
    return(build_rm_mixed_posthoc_html(posthoc_result))
  }

  # Detect prefix set
  has_lincon <- any(grepl("^Lincon\\.", names(posthoc_result)))
  has_rm_lincon <- any(grepl("^RM\\.Lincon\\.", names(posthoc_result)))
  has_tukey <- any(grepl("^Tukey\\.", names(posthoc_result)))
  has_paired_t <- any(grepl("^Paired\\.t\\.", names(posthoc_result)))
  has_paired_d <- any(grepl("^Paired\\.d", names(posthoc_result)))
  has_paired_wilcox <- any(grepl(
    "^Paired\\.Wilcox\\.", names(posthoc_result)
  ))
  has_dunn <- any(grepl("^Dunn\\.", names(posthoc_result)))
  has_wilcox <- any(grepl("^Wilcox\\.", names(posthoc_result)))
  has_art <- any(grepl("^ART\\.", names(posthoc_result)))

  if (has_paired_t && has_paired_d) {
    left_prefix <- "Paired.t"
    left_label <- "Paired t-Test"
    right_prefix <- "Paired.d"
    right_label <- "Paired Cohen's d"
  } else if (has_rm_lincon) {
    left_prefix <- "RM.Lincon"
    left_label <- "RM Lincon (Trimmed Means)"
    right_prefix <- NULL
    right_label <- NULL
  } else if (has_paired_wilcox) {
    left_prefix <- "Paired.Wilcox"
    left_label <- "Paired Wilcoxon"
    right_prefix <- NULL
    right_label <- NULL
  } else if (has_lincon) {
    left_prefix <- "Lincon"
    left_label <- "Lincon"
    right_prefix <- "Cliff"
    right_label <- "Cliff's Delta"
  } else if (has_tukey) {
    left_prefix <- "Tukey"
    left_label <- "Tukey HSD"
    right_prefix <- "Cohen"
    right_label <- "Cohen's d"
  } else if (has_dunn) {
    left_prefix <- "Dunn"
    left_label <- "Dunn's Test"
    right_prefix <- "Cliff"
    right_label <- "Cliff's Delta"
  } else if (has_wilcox) {
    left_prefix <- "Wilcox"
    left_label <- "Pairwise Wilcoxon"
    right_prefix <- "Cliff"
    right_label <- "Cliff's Delta"
  } else if (has_art) {
    left_prefix <- "ART"
    left_label <- "ART Contrasts"
    right_prefix <- "ART.d"
    right_label <- "ART Cohen's d"
  } else {
    # Unknown prefix — render the whole table as-is
    return(paste0(
      "<h2>Pairwise Comparisons</h2>\n",
      df_to_html_table(posthoc_result),
      "\n"
    ))
  }

  # Handle single-table RM formats (no right panel)
  if (is.null(right_prefix)) {
    left_cols <- grep(
      paste0("^", left_prefix, "\\."),
      names(posthoc_result), value = TRUE
    )
    left_df <- posthoc_result[
      , c("Interaction", left_cols), drop = FALSE
    ]
    names(left_df) <- gsub(
      paste0("^", left_prefix, "\\."), "",
      names(left_df)
    )
    return(paste0(
      "<h2>Pairwise Comparisons</h2>\n",
      "<h3>", htmltools$htmlEscape(left_label), "</h3>\n",
      df_to_html_table(left_df),
      "\n"
    ))
  }

  # For ART, split by explicit column names
  if (has_art) {
    art_left_names <- c(
      "ART.estimate", "ART.SE", "ART.df",
      "ART.t.ratio", "ART.p.value", "ART.p.adjusted"
    )
    art_right_names <- c(
      "ART.d", "ART.d.ci.lower", "ART.d.ci.upper"
    )
    left_cols <- intersect(art_left_names, names(posthoc_result))
    right_cols <- intersect(
      art_right_names, names(posthoc_result)
    )
  } else if (has_paired_t && has_paired_d) {
    paired_t_names <- c(
      "Paired.t.statistic",
      "Paired.t.p.value", "Paired.t.p.adjusted"
    )
    paired_d_names <- c(
      "Paired.d", "Paired.d.ci.lower",
      "Paired.d.ci.upper"
    )
    left_cols <- intersect(
      paired_t_names, names(posthoc_result)
    )
    right_cols <- intersect(
      paired_d_names, names(posthoc_result)
    )
  } else {
    left_cols <- grep(
      paste0("^", left_prefix, "\\."),
      names(posthoc_result), value = TRUE
    )
    right_cols <- grep(
      paste0("^", right_prefix, "\\."),
      names(posthoc_result), value = TRUE
    )
  }

  # Left table: Interaction + left columns, strip prefix
  left_df <- posthoc_result[
    , c("Interaction", left_cols), drop = FALSE
  ]
  names(left_df) <- gsub(
    paste0("^", left_prefix, "\\."), "", names(left_df)
  )

  # Right table: Interaction + right columns, strip prefix
  right_df <- posthoc_result[
    , c("Interaction", right_cols), drop = FALSE
  ]
  if (has_art) {
    names(right_df) <- gsub("^ART\\.d\\.", "", names(right_df))
    names(right_df) <- gsub("^ART\\.d$", "d", names(right_df))
  } else if (has_paired_t && has_paired_d) {
    names(right_df) <- gsub(
      "^Paired\\.d\\.", "", names(right_df)
    )
    names(right_df) <- gsub(
      "^Paired\\.d$", "d", names(right_df)
    )
  } else {
    names(right_df) <- gsub(
      paste0("^", right_prefix, "\\."), "", names(right_df)
    )
  }

  paste0(
    "<h2>Pairwise Comparisons</h2>\n",
    '<div class="two-tables">\n',
    '<div class="table-panel">\n',
    "<h3>", htmltools$htmlEscape(left_label), "</h3>\n",
    df_to_html_table(left_df),
    "\n</div>\n",
    '<div class="table-panel">\n',
    "<h3>", htmltools$htmlEscape(right_label), "</h3>\n",
    df_to_html_table(right_df),
    "\n</div>\n",
    "</div>\n"
  )
}


#' Generate a standalone HTML report for a single measurement
#'
#' @param measure Character, measurement column name
#' @param plot_object ggplot object (or NULL)
#' @param omnibus_result Data frame, app_error, or NULL
#' @param posthoc_result Data frame, app_error, or NULL
#' @param params List with test_approach, use_bootstrap,
#'   p_val_cor_method, etc.
#' @param x_axis Character vector of grouping columns
#' @param timestamp POSIXct timestamp
#' @return Character string with full HTML document
#' @export
generate_html_report <- function(measure,
                                 plot_object,
                                 omnibus_result,
                                 posthoc_result,
                                 params,
                                 x_axis,
                                 timestamp) {
  # --- Meta info ---
  approach_label <- switch(
    params$test_approach %||% "unknown",
    robust = "Robust Tests",
    parametric = "Parametric Tests",
    nonparametric = "Non-Parametric Tests",
    params$test_approach
  )

  # RM meta info
  rm_html <- ""
  if (isTRUE(params$is_repeated_measures)) {
    rm_html <- paste0(
      "<strong>Design Type:</strong> Repeated Measures",
      "<br>\n",
      "<strong>ID Column:</strong> ",
      htmltools$htmlEscape(
        params$rm_id_col %||% "(not set)"
      ), "<br>\n",
      "<strong>Within-Subject Factor:</strong> ",
      htmltools$htmlEscape(
        params$rm_within_col %||% "(not set)"
      ), "<br>\n"
    )
  }

  meta_html <- paste0(
    '<div class="meta-info">\n',
    "<strong>Approach:</strong> ",
    htmltools$htmlEscape(approach_label), "<br>\n",
    "<strong>Design:</strong> ",
    length(x_axis), "-way",
    " (", htmltools$htmlEscape(
      paste(x_axis, collapse = ", ")
    ), ")<br>\n",
    rm_html,
    "<strong>Bootstrap:</strong> ",
    ifelse(isTRUE(params$use_bootstrap), "Yes", "No"),
    "<br>\n",
    "<strong>P-value Adjustment:</strong> ",
    htmltools$htmlEscape(
      params$p_val_cor_method %||% "none"
    ), "\n",
    "</div>\n"
  )

  # --- Plot ---
  plot_html <- ""
  plot_uri <- render_plot_base64(plot_object)
  if (!is.null(plot_uri)) {
    plot_html <- paste0(
      "<h2>Plot</h2>\n",
      '<div class="plot-container">\n',
      '<img src="', plot_uri,
      '" alt="Statistical Plot">\n',
      "</div>\n"
    )
  }

  # --- Omnibus ---
  omnibus_html <- build_omnibus_html(
    omnibus_result, x_axis,
    params$test_approach %||% "unknown",
    is_rm = isTRUE(params$is_repeated_measures)
  )

  # --- Post-hoc ---
  posthoc_html <- build_posthoc_html(posthoc_result)

  # --- Assemble full document ---
  paste0(
    '<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" ',
    'content="width=device-width, initial-scale=1.0">
  <title>Statistics Report: ',
    htmltools$htmlEscape(measure),
    '</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont,
        "Segoe UI", Roboto, "Helvetica Neue", Arial,
        sans-serif;
      line-height: 1.6;
      max-width: 1000px;
      margin: 0 auto;
      padding: 20px;
      color: #333;
    }
    h1 {
      color: #2c3e50;
      border-bottom: 2px solid #3498db;
      padding-bottom: 10px;
    }
    h2 { color: #34495e; margin-top: 30px; }
    h3 { color: #7f8c8d; margin-top: 10px; }
    table {
      border-collapse: collapse;
      width: 100%;
      margin: 15px 0;
      font-size: 0.9em;
    }
    th, td {
      border: 1px solid #ddd;
      padding: 8px 10px;
      text-align: left;
    }
    th {
      background-color: #3498db;
      color: white;
    }
    tr:nth-child(even) { background-color: #f9f9f9; }
    tr:hover { background-color: #f5f5f5; }
    .meta-info {
      background-color: #ecf0f1;
      padding: 15px;
      border-radius: 4px;
      margin-bottom: 20px;
    }
    .plot-container {
      margin: 20px 0;
      text-align: center;
    }
    .plot-container img {
      max-width: 100%;
      height: auto;
      border: 1px solid #ddd;
      border-radius: 4px;
    }
    .two-tables {
      display: flex;
      gap: 20px;
    }
    .table-panel { flex: 1; min-width: 0; }
    .warning {
      background-color: #fff3cd;
      border: 1px solid #ffc107;
      padding: 10px;
      border-radius: 4px;
      margin: 10px 0;
    }
    .footer {
      margin-top: 40px;
      padding-top: 20px;
      border-top: 1px solid #ddd;
      color: #666;
      font-size: 0.9em;
    }
    @media print {
      body { max-width: 100%; }
      .two-tables { break-inside: avoid; }
    }
  </style>
</head>
<body>
  <h1>Statistical Analysis: ',
    htmltools$htmlEscape(measure), '</h1>
  ', meta_html, '
  ', plot_html, '
  ', omnibus_html, '
  ', posthoc_html, '
  <div class="footer">
    <p>Generated by TexAn 2.0 on ',
    format(timestamp, "%Y-%m-%d %H:%M:%S"),
    '</p>
  </div>
</body>
</html>'
  )
}
