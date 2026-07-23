###############################################
# Configuration Chart Generator for ThSQCA
###############################################

#' Parse solution expression into individual terms
#'
#' Splits a solution expression (ORed terms) into individual prime implicants.
#'
#' @param expr Character. Solution expression (e.g., "X3 + X1*X2").
#'
#' @return Character vector of terms (e.g., c("X3", "X1*X2")), or NULL if
#'   no valid expression.
#'
#' @keywords internal
parse_solution_terms <- function(expr) {
  if (is.na(expr) || expr == "No solution" || expr == "" || is.null(expr)) {
    return(NULL)
  }
  # Split by + (OR operator), allowing spaces
  terms <- strsplit(expr, "\\s*\\+\\s*")[[1]]
  # Trim whitespace
  terms <- trimws(terms)
  # Remove empty strings
  terms <- terms[terms != ""]
  if (length(terms) == 0) return(NULL)
  return(terms)
}


#' Determine condition status in a term
#'
#' Checks whether a condition is present, absent (negated), or don't care
#' in a given term.
#'
#' @param term Character. Single term (e.g., "X1*X2", "~X3").
#' @param condition Character. Condition name (e.g., "X1").
#'
#' @return Character. One of "present", "absent", or "dontcare".
#'
#' @details Uses word boundary matching to avoid false positives when
#'   condition names are substrings of each other (e.g., X1 vs X10).
#'
#' @keywords internal
get_condition_status <- function(term, condition) {
  # Remove spaces from term for consistent matching
  term <- gsub("\\s+", "", term)
  
  # Escape special regex characters in condition name
  cond_escaped <- gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\.\\|\\?\\+\\\\])", 
                       "\\\\\\1", condition)
  
  # Negated condition check (~X1 or similar)
  negated_pattern <- paste0("~", cond_escaped, "(?![A-Za-z0-9_]|$)")
  # Also check for ~X1 at end of string
  negated_pattern_end <- paste0("~", cond_escaped, "$")
  if (grepl(negated_pattern, term, perl = TRUE) || 
      grepl(negated_pattern_end, term, perl = TRUE)) {
    return("absent")
  }
  
  # Positive condition check (X1 but not ~X1)
  # Pattern: X1 not preceded by ~ or alphanumeric, not followed by alphanumeric
  positive_pattern <- paste0("(?<![~A-Za-z0-9_])", cond_escaped, "(?![A-Za-z0-9_])")
  # Also check for condition at start of string
  positive_pattern_start <- paste0("^", cond_escaped, "(?![A-Za-z0-9_]|$)")
  positive_pattern_end <- paste0("(?<![~A-Za-z0-9_])", cond_escaped, "$")
  
  if (grepl(positive_pattern, term, perl = TRUE) ||
      grepl(positive_pattern_start, term, perl = TRUE) ||
      grepl(positive_pattern_end, term, perl = TRUE)) {
    return("present")
  }
  
  return("dontcare")
}


#' Symbol sets for configuration charts
#' @keywords internal
SYMBOL_SETS <- list(
  unicode = list(
    present = "\u25CF",     # ● (BLACK CIRCLE)
    absent  = "\u2297",     # ⊗ (CIRCLED TIMES)
    note_en = "\u25CF = presence, \u2297 = absence, blank = don't care",
    note_ja = "\u25CF = \u5b58\u5728, \u2297 = \u4e0d\u5728, \u7a7a\u6b04 = \u7121\u95a2\u4fc2"
  ),

  ascii = list(
    present = "O",
    absent  = "X",
    note_en = "O = presence, X = absence, blank = don't care",
    note_ja = "O = \u5b58\u5728, X = \u4e0d\u5728, \u7a7a\u6b04 = \u7121\u95a2\u4fc2"
  ),
  latex = list(
    present = "$\\bullet$",
    absent  = "$\\otimes$",
    note_en = "$\\bullet$ = presence, $\\otimes$ = absence, blank = don't care",
    note_ja = "$\\bullet$ = \u5b58\u5728, $\\otimes$ = \u4e0d\u5728, \u7a7a\u6b04 = \u7121\u95a2\u4fc2"
  )
)


#' Generate solution note for multiple solutions
#'
#' Creates a note explaining that multiple equivalent solutions exist
#' and that the displayed configuration is based on M1.
#'
#' @param n_sol Integer. Number of solutions.
#' @param epi_list Character vector. Essential prime implicants (NULL to omit).
#' @param style Character. \code{"simple"} or \code{"detailed"}.
#' @param language Character. \code{"en"} or \code{"ja"}.
#' @param format Character. \code{"markdown"} or \code{"latex"}.
#'
#' @return Character string of the note, or empty string if n_sol <= 1.
#'
#' @examples
#' # Simple note
#' generate_solution_note(2, style = "simple")
#' 
#' # Detailed note with EPIs
#' generate_solution_note(3, epi_list = c("A*B", "C"), style = "detailed")
#' 
#' # Japanese
#' generate_solution_note(2, style = "simple", language = "ja")
#'
#' @export
generate_solution_note <- function(n_sol,
                                    epi_list = NULL,
                                    style = c("simple", "detailed"),
                                    language = c("en", "ja"),
                                    format = c("markdown", "latex")) {
  
  # Return empty string for single solution
  if (is.null(n_sol) || n_sol <= 1) {
    return("")
  }

  
  style <- match.arg(style)
  language <- match.arg(language)
  format <- match.arg(format)
  
  labels <- get_config_labels(language)
  
  # Build note text
  if (language == "ja") {
    # Japanese version
    n_sol_text <- gsub("\\{n\\}", as.character(n_sol), labels$n_equiv_solutions)
    base_note <- paste0(n_sol_text, labels$table_based_on_m1)
    
    if (style == "detailed" && !is.null(epi_list) && length(epi_list) > 0) {
      # Format EPIs with appropriate connector
      if (format == "latex") {
        epi_str <- paste(epi_list, collapse = ", ")
        epi_str <- gsub("\\*", "$\\\\cdot$", epi_str)
      } else {
        epi_str <- paste(epi_list, collapse = ", ")
        epi_str <- gsub("\\*", "\u00B7", epi_str)  # middle dot
      }
      
      if (length(epi_list) == 1) {
        epi_text <- gsub("\\{epi\\}", epi_str, labels$all_share_epi_single)
      } else {
        epi_text <- gsub("\\{epi\\}", epi_str, labels$all_share_epi)
      }
      base_note <- paste0(base_note, epi_text)
    } else if (style == "detailed" && (is.null(epi_list) || length(epi_list) == 0)) {
      base_note <- paste0(base_note, labels$no_epi)
    }
    
  } else {
    # English version
    if (style == "detailed") {
      n_sol_text <- gsub("\\{n\\}", as.character(n_sol), labels$n_equiv_solutions_range)
    } else {
      n_sol_text <- gsub("\\{n\\}", as.character(n_sol), labels$n_equiv_solutions)
    }
    base_note <- paste0(n_sol_text, " ", labels$table_based_on_m1)
    
    if (style == "detailed" && !is.null(epi_list) && length(epi_list) > 0) {
      # Format EPIs with appropriate connector
      if (format == "latex") {
        epi_str <- paste(epi_list, collapse = " and ")
        epi_str <- gsub("\\*", "$\\\\cdot$", epi_str)
      } else {
        epi_str <- paste(epi_list, collapse = " and ")
        epi_str <- gsub("\\*", "\u00B7", epi_str)  # middle dot
      }
      
      if (length(epi_list) == 1) {
        epi_text <- gsub("\\{epi\\}", epi_str, labels$all_share_epi_single)
      } else {
        epi_text <- gsub("\\{epi\\}", epi_str, labels$all_share_epi)
      }
      base_note <- paste0(base_note, " ", epi_text)
    } else if (style == "detailed" && (is.null(epi_list) || length(epi_list) == 0)) {
      base_note <- paste0(base_note, " ", labels$no_epi)
    }
  }
  
  # Format output
  if (format == "latex") {
    note <- paste0("\\footnotesize{\\textit{", labels$note_prefix, "}: ", base_note, "}")
  } else {
    note <- paste0("*", labels$note_prefix, ": ", base_note, "*")
  }
  
  return(note)
}


#' Identify Essential Prime Implicants from multiple solutions
#'
#' Finds terms that appear in ALL solutions (EPIs) versus terms that
#' appear in only some solutions (SPIs).
#'
#' @param solutions List of solution vectors. Each element is a character
#'   vector of terms for one solution.
#'
#' @return List with:
#'   \itemize{
#'     \item \code{epi} — Essential prime implicants (in all solutions)
#'     \item \code{spi} — Selective prime implicants (in some solutions)
#'     \item \code{n_solutions} — Number of solutions
#'   }
#'
#' @examples
#' solutions <- list(
#'   c("A*B", "C", "D"),
#'   c("A*B", "C", "E"),
#'   c("A*B", "C", "F")
#' )
#' result <- identify_epi(solutions)
#' # result$epi = c("A*B", "C")
#' # result$spi = c("D", "E", "F")
#'
#' @export
identify_epi <- function(solutions) {
  
  if (is.null(solutions) || length(solutions) == 0) {
    return(list(epi = character(0), spi = character(0), n_solutions = 0L))
  }
  
  n_solutions <- length(solutions)
  
  if (n_solutions == 1) {
    # Single solution: all terms are "essential" by definition
    return(list(
      epi = solutions[[1]],
      spi = character(0),
      n_solutions = 1L
    ))
  }
  
  # Find intersection of all solutions (EPIs)
  epi <- Reduce(intersect, solutions)
  
  # Find union of all solutions
  all_terms <- Reduce(union, solutions)
  
  # SPIs are terms not in all solutions
spi <- setdiff(all_terms, epi)
  
  list(
    epi = epi,
    spi = spi,
    n_solutions = n_solutions
  )
}


#' Parse a single path/term into conditions
#'
#' @param path Character. A single path like "A*B*~C"
#' @return List with 'present' and 'absent' condition names
#' @keywords internal
parse_path_conditions <- function(path) {
  # Split by * (AND operator)
  terms <- unlist(strsplit(path, "\\*"))
  terms <- trimws(terms)
  
  # Separate present and absent conditions
  absent_terms <- grep("^~", terms, value = TRUE)
  present_terms <- setdiff(terms, absent_terms)
  
  # Remove ~ prefix from absent terms
  absent_conds <- gsub("^~", "", absent_terms)
  
  list(
    present = present_terms,
    absent  = absent_conds
  )
}


#' Extract all unique conditions from paths
#'
#' @param paths Character vector of paths
#' @return Character vector of unique condition names (without ~)
#' @keywords internal
extract_conditions_from_paths <- function(paths) {
  all_conds <- c()
  for (path in paths) {
    parsed <- parse_path_conditions(path)
    all_conds <- c(all_conds, parsed$present, parsed$absent)
  }
  unique(all_conds)
}


#' Build condition-path matrix for configuration chart
#'
#' @param paths Character vector of paths
#' @param conditions Character vector of condition names (optional)
#' @param symbols List with 'present' and 'absent' symbols
#' @return Matrix with conditions as rows, paths as columns
#' @keywords internal
build_config_matrix <- function(paths, conditions = NULL, symbols) {
  
  # Auto-detect conditions if not provided
  if (is.null(conditions)) {
    conditions <- extract_conditions_from_paths(paths)
  }
  
  n_paths <- length(paths)
  n_conds <- length(conditions)
  
  mat <- matrix("", nrow = n_conds, ncol = n_paths)
  rownames(mat) <- conditions
  colnames(mat) <- paste0("M", seq_len(n_paths))
  
  for (j in seq_along(paths)) {
    parsed <- parse_path_conditions(paths[j])
    for (cond in conditions) {
      if (cond %in% parsed$present) {
        mat[cond, j] <- symbols$present
      } else if (cond %in% parsed$absent) {
        mat[cond, j] <- symbols$absent
      }
      # else: leave blank (don't care)
    }
  }
  
  mat
}


#' Convert configuration matrix to Markdown table
#'
#' @param mat Matrix with rownames and colnames
#' @param row_header Character. Header for the row names column
#' @param center_align Logical. Whether to center-align columns
#' @return Character string of Markdown table
#' @keywords internal
config_matrix_to_md <- function(mat, row_header = "Condition", center_align = TRUE) {
  n_cols <- ncol(mat)
  
  # Header row
  header <- paste0("| ", row_header, " | ", 
                   paste(colnames(mat), collapse = " | "), " |")
  
  # Separator row
  if (center_align) {
    sep <- paste0("|", paste(rep(":--:", n_cols + 1), collapse = "|"), "|")
  } else {
    sep <- paste0("|", paste(rep("---", n_cols + 1), collapse = "|"), "|")
  }
  
  # Data rows
  rows <- sapply(seq_len(nrow(mat)), function(i) {
    paste0("| ", rownames(mat)[i], " | ", 
           paste(mat[i, ], collapse = " | "), " |")
  })
  
  paste(c(header, sep, rows), collapse = "\n")
}


#' Generate solution-term level chart (Fiss-style)
#'
#' Creates a configuration chart where each column represents a single
#' prime implicant (configuration), following Fiss (2011) notation.
#'
#' @param sum_df Data frame. Summary data frame from sweep results with
#'   expression column and threshold column(s).
#' @param conditions Character vector. Condition names for row ordering.
#' @param symbols List. Symbol set (present, absent) for the chart.
#' @param language Character. Language for labels ("en" or "ja").
#'
#' @return Character string containing Markdown-formatted table.
#'
#' @keywords internal
generate_term_level_chart <- function(sum_df, conditions, symbols, language = "en") {
  
  labels <- get_config_labels(language)
  
  # Step 1: Extract all terms from all thresholds
  terms_list <- lapply(seq_len(nrow(sum_df)), function(i) {
    expr <- sum_df$expression[i]
    
    # Identify threshold column (otSweep: thrY, ctSweepS: threshold, etc.)
    thr_col <- intersect(c("thrY", "threshold", "thrX"), names(sum_df))[1]
    if (is.na(thr_col) || is.null(thr_col)) {
      thr <- paste0("row", i)
    } else {
      thr <- sum_df[[thr_col]][i]
    }
    
    terms <- parse_solution_terms(expr)
    if (is.null(terms)) return(NULL)
    
    data.frame(
      thr = thr,
      term_num = seq_along(terms),
      term_expr = terms,
      stringsAsFactors = FALSE
    )
  })
  
  terms_df <- do.call(rbind, terms_list)
  
  # No solutions found
  if (is.null(terms_df) || nrow(terms_df) == 0) {
    return(paste0("*", labels$no_solution, "*\n"))
  }
  
  # Step 2: Generate column names (Fiss-style format)
  col_names <- paste0("thrY = ", terms_df$thr, " (C", terms_df$term_num, ")")
  
  # Step 3: Initialize matrix (blank = don't care)
  chart_matrix <- matrix(
    "",
    nrow = length(conditions),
    ncol = nrow(terms_df),
    dimnames = list(conditions, col_names)
  )
  
  # Step 4: Fill in cells
  for (j in seq_len(nrow(terms_df))) {
    term <- terms_df$term_expr[j]
    
    for (cond in conditions) {
      status <- get_condition_status(term, cond)
      
      chart_matrix[cond, j] <- switch(
        status,
        "present"  = symbols$present,
        "absent"   = symbols$absent,
        "dontcare" = ""
      )
    }
  }
  
  # Step 5: Convert to markdown
  table_str <- config_matrix_to_md(chart_matrix, labels$condition)
  
  # Add legend
  legend_note <- if (language == "ja") symbols$note_ja else symbols$note_en
  paste0(table_str, "\n\n*", legend_note, "*\n")
}


#' Generate threshold-level summary chart
#'
#' Creates a configuration chart where each column represents one threshold,
#' showing all conditions that appear in any configuration at that threshold.
#'
#' @param sum_df Data frame. Summary data frame from sweep results.
#' @param conditions Character vector. Condition names for row ordering.
#' @param symbols List. Symbol set (present, absent) for the chart.
#' @param language Character. Language for labels ("en" or "ja").
#'
#' @return Character string containing Markdown-formatted table.
#'
#' @keywords internal
generate_threshold_level_chart <- function(sum_df, conditions, symbols, language = "en") {
  
  labels <- get_config_labels(language)
  
  # Identify threshold column
  thr_col <- intersect(c("thrY", "threshold", "thrX"), names(sum_df))[1]
  if (is.na(thr_col) || is.null(thr_col)) {
    thr_col <- "row"
    sum_df[[thr_col]] <- seq_len(nrow(sum_df))
  }
  
  # Get unique thresholds
  thresholds <- unique(sum_df[[thr_col]])
  
  # Build column names
  col_names <- paste0("thrY=", thresholds)
  
  # Initialize matrix
  chart_matrix <- matrix(
    "",
    nrow = length(conditions),
    ncol = length(thresholds),
    dimnames = list(conditions, col_names)
  )
  
  # Fill in cells (aggregate across all terms at each threshold)
  for (j in seq_along(thresholds)) {
    thr <- thresholds[j]
    
    # Get all expressions at this threshold
    exprs <- sum_df$expression[sum_df[[thr_col]] == thr]
    
    # Parse all terms
    all_terms <- character(0)
    for (expr in exprs) {
      terms <- parse_solution_terms(expr)
      if (!is.null(terms)) {
        all_terms <- c(all_terms, terms)
      }
    }
    
    if (length(all_terms) == 0) next
    
    # Check each condition across all terms
    for (cond in conditions) {
      has_present <- FALSE
      has_absent <- FALSE
      
      for (term in all_terms) {
        status <- get_condition_status(term, cond)
        if (status == "present") has_present <- TRUE
        if (status == "absent") has_absent <- TRUE
      }
      
      # If both present and absent appear, show presence (it appears in some config)
      if (has_present) {
        chart_matrix[cond, j] <- symbols$present
      } else if (has_absent) {
        chart_matrix[cond, j] <- symbols$absent
      }
      # else: leave blank (don't care across all configs)
    }
  }
  
  # Convert to markdown
  table_str <- config_matrix_to_md(chart_matrix, labels$condition)
  
  # Add legend
  legend_note <- if (language == "ja") symbols$note_ja else symbols$note_en
  paste0(table_str, "\n\n*", legend_note, "*\n")
}


#' Generate cross-threshold configuration chart from sweep results
#'
#' Creates a configuration chart from threshold sweep results. Supports
#' two levels of aggregation: solution-term level (Fiss-style, default) and
#' threshold-level summary.
#'
#' @param result A result object from any Sweep function (otSweep, ctSweepS,
#'   ctSweepM, or dtSweep).
#' @param conditions Character vector. Condition names for row ordering.
#'   If NULL, automatically extracted from expressions.
#' @param symbol_set Character. One of \code{"unicode"}, \code{"ascii"}, 
#'   or \code{"latex"}. Default is \code{"unicode"}.
#' @param chart_level Character. Chart aggregation level:
#'   \code{"term"} (default) produces solution-term level charts following Fiss (2011)
#'   notation, where each column represents one prime implicant.
#'   \code{"summary"} produces threshold-level summaries where
#'   each column represents one threshold, aggregating all configurations.
#' @param language Character. \code{"en"} for English, \code{"ja"} for Japanese.
#'
#' @return Character string containing Markdown-formatted table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(sample_data)
#' result <- otSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:8,
#'   thrX = c(X1 = 7, X2 = 7, X3 = 7)
#' )
#' 
#' # Solution-term level, Fiss-style (default)
#' chart <- generate_cross_threshold_chart(result, c("X1", "X2", "X3"))
#' cat(chart)
#' 
#' # Threshold-level summary
#' chart <- generate_cross_threshold_chart(result, c("X1", "X2", "X3"),
#'                                          chart_level = "summary")
#' cat(chart)
#' }
generate_cross_threshold_chart <- function(result,
                                            conditions = NULL,
                                            symbol_set = c("unicode", "ascii", "latex"),
                                            chart_level = c("term", "summary"),
                                            language = c("en", "ja")) {
  
  symbol_set <- match.arg(symbol_set)
  chart_level <- match.arg(chart_level)
  language <- match.arg(language)
  symbols <- SYMBOL_SETS[[symbol_set]]
  
  # Get summary data frame
  if (is.data.frame(result)) {
    sum_df <- result
  } else if (is.list(result) && "summary" %in% names(result)) {
    sum_df <- result$summary
  } else {
    stop("'result' must be a data frame or a list with 'summary' element.")
  }
  
  # Auto-detect conditions from expressions if not provided
  if (is.null(conditions)) {
    all_terms <- character(0)
    for (expr in sum_df$expression) {
      terms <- parse_solution_terms(expr)
      if (!is.null(terms)) {
        all_terms <- c(all_terms, terms)
      }
    }
    conditions <- extract_conditions_from_paths(all_terms)
  }
  
  if (length(conditions) == 0) {
    return("*No conditions found.*\n")
  }
  
  # Dispatch to appropriate chart generator
  if (chart_level == "summary") {
    generate_threshold_level_chart(sum_df, conditions, symbols, language)
  } else {
    generate_term_level_chart(sum_df, conditions, symbols, language)
  }
}


#' Generate Configuration Chart from QCA Solution
#'
#' Creates a Markdown-formatted configuration chart (Fiss-style table)
#' from QCA minimization results. Supports single solution with multiple
#' paths, and multiple solutions (displayed as separate tables).
#'
#' @param sol A solution object returned by \code{QCA::minimize()}, or
#'   a list containing solution information.
#' @param symbol_set Character. One of \code{"unicode"}, \code{"ascii"}, 
#'   or \code{"latex"}. Default is \code{"unicode"}.
#' @param include_metrics Logical. Whether to include consistency/coverage
#'   metrics in the table. Default is TRUE.
#' @param language Character. \code{"en"} for English, \code{"ja"} for Japanese.
#'   Default is \code{"en"}.
#' @param condition_order Character vector. Optional ordering of conditions
#'   in the table rows. If NULL, conditions are ordered as they appear in paths.
#'
#' @return Character string containing Markdown-formatted table(s).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After running QCA::minimize()
#' library(QCA)
#' tt <- truthTable(data, outcome = "Y", conditions = c("A", "B", "C"))
#' sol <- minimize(tt, include = "?", details = TRUE)
#' 
#' # Generate configuration chart
#' chart <- generate_config_chart(sol)
#' cat(chart)
#'
#' # For LaTeX/PDF output (e.g., rticles)
#' chart <- generate_config_chart(sol, symbol_set = "latex")
#'
#' # ASCII for maximum compatibility
#' chart <- generate_config_chart(sol, symbol_set = "ascii")
#'
#' # Japanese labels
#' chart <- generate_config_chart(sol, language = "ja")
#' }
generate_config_chart <- function(sol,
                                   symbol_set = c("unicode", "ascii", "latex"),
                                   include_metrics = TRUE,
                                   language = c("en", "ja"),
                                   condition_order = NULL) {
  
  symbol_set <- match.arg(symbol_set)
  language <- match.arg(language)
  symbols <- SYMBOL_SETS[[symbol_set]]
  
  # Get labels based on language
  labels <- get_config_labels(language)
  
  # Get symbol note
  note <- if (language == "ja") symbols$note_ja else symbols$note_en
  
  # Extract solutions from QCA object
  sol_list <- extract_solution_list(sol)
  n_solutions <- length(sol_list)
  
  if (n_solutions == 0) {
    return(paste0("*", labels$no_solution, "*\n"))
  }
  
  # Single solution vs multiple solutions
  if (n_solutions == 1) {
    # Single solution: all paths in one table
    paths <- extract_paths_from_solution(sol_list[[1]])
    chart <- build_single_chart(
      paths = paths,
      sol = sol,
      symbols = symbols,
      labels = labels,
      include_metrics = include_metrics,
      condition_order = condition_order
    )
  } else {
    # Multiple solutions: separate tables with warning
    warning_msg <- paste0(
      "**", labels$note, ":** ", n_solutions, " ", labels$equiv_solutions, " ",
      labels$separate_tables, "\n\n"
    )
    
    charts <- lapply(seq_along(sol_list), function(i) {
      paths <- extract_paths_from_solution(sol_list[[i]])
      header <- paste0("### ", labels$solution, " M", i, "\n\n")
      table <- build_single_chart(
        paths = paths,
        sol = sol,
        symbols = symbols,
        labels = labels,
        include_metrics = include_metrics,
        condition_order = condition_order,
        solution_index = i
      )
      paste0(header, table)
    })
    
    chart <- paste0(warning_msg, paste(charts, collapse = "\n\n---\n\n"))
  }
  
  # Add symbol legend
  legend <- paste0("\n\n*", note, "*\n")
  
  paste0(chart, legend)
}


#' Get labels for configuration chart based on language
#' @keywords internal
get_config_labels <- function(language) {
  if (language == "ja") {
    list(
      condition = "\u6761\u4ef6",
      consistency = "\u4e00\u8cab\u6027",
      raw_coverage = "\u751f\u30ab\u30d0\u30ec\u30c3\u30b8",
      unique_coverage = "\u56fa\u6709\u30ab\u30d0\u30ec\u30c3\u30b8",
      solution_consistency = "\u89e3\u4e00\u8cab\u6027",
      solution_coverage = "\u89e3\u30ab\u30d0\u30ec\u30c3\u30b8",
      solution = "\u89e3",
      note = "\u6ce8\u610f",
      equiv_solutions = "\u500b\u306e\u7b49\u4fa1\u306a\u89e3\u304c\u5b58\u5728\u3057\u307e\u3059\u3002",
      separate_tables = "\u4ee5\u4e0b\u306b\u5225\u3005\u306e\u8868\u3092\u793a\u3057\u307e\u3059\u3002",
      no_solution = "\u89e3\u304c\u898b\u3064\u304b\u308a\u307e\u305b\u3093\u3067\u3057\u305f",
      # Solution note labels (Japanese)
      note_prefix = "\u6ce8",
      n_equiv_solutions = "\u8ad6\u7406\u7684\u306b\u7b49\u4fa1\u306a{n}\u3064\u306e\u89e3\u304c\u5f97\u3089\u308c\u305f\u3002",
      table_based_on_m1 = "\u672c\u8868\u306fM1\u306b\u57fa\u3065\u304f\u69cb\u6210\u3092\u793a\u3059\u3002",
      all_share_epi = "\u5168\u89e3\u306b\u5171\u901a\u3059\u308bEssential Prime Implicants: {epi}",
      all_share_epi_single = "\u5168\u89e3\u306b\u5171\u901a\u3059\u308bEssential Prime Implicant: {epi}",
      no_epi = "\u5168\u3066\u306e\u9805\u304cSelective Prime Implicants\u3067\u3042\u308b\u3002",
      # Fiss core/peripheral labels (Japanese)
      fiss_core       = "\u30b3\u30a2\u6761\u4ef6",
      fiss_peripheral = "\u5468\u8fba\u6761\u4ef6",
      fiss_parsim     = "\u7c21\u6f54\u89e3",
      fiss_interm     = "\u4e2d\u9593\u89e3",
      fiss_note       = paste0(
        "\u30b3\u30a2\u6761\u4ef6\uff1a\u7c21\u6f54\u89e3\u304b\u3064\u4e2d\u9593\u89e3\u306b\u5171\u901a\u3059\u308b\u6761\u4ef6\u3002",
        "\u5468\u8fba\u6761\u4ef6\uff1a\u4e2d\u9593\u89e3\u306e\u307f\u306b\u73fe\u308c\u308b\u6761\u4ef6\u3002",
        "Fiss (2011) \u306b\u57fa\u3065\u304f\u3002"
      )
    )
  } else {
    list(
      condition = "Condition",
      consistency = "Consistency",
      raw_coverage = "Raw Cov.",
      unique_coverage = "Uniq. Cov.",
      solution_consistency = "Solution Consistency",
      solution_coverage = "Solution Coverage",
      solution = "Solution",
      note = "Note",
      equiv_solutions = "equivalent solutions exist.",
      separate_tables = "Tables are shown separately below.",
      no_solution = "No solution found",
      # Solution note labels (English)
      note_prefix = "Note",
      n_equiv_solutions = "{n} logically equivalent solutions were identified.",
      n_equiv_solutions_range = "{n} logically equivalent solutions were identified (M1-M{n}).",
      table_based_on_m1 = "This table presents configurations based on M1.",
      all_share_epi = "All solutions share the essential prime implicants: {epi}.",
      all_share_epi_single = "All solutions share the essential prime implicant: {epi}.",
      no_epi = "Solutions differ in all prime implicants (no essential prime implicants).",
      # Fiss core/peripheral labels (English)
      fiss_core       = "Core condition",
      fiss_peripheral = "Peripheral condition",
      fiss_parsim     = "Parsimonious solution",
      fiss_interm     = "Intermediate solution",
      fiss_note       = paste0(
        "Core conditions appear in both the parsimonious and intermediate solutions. ",
        "Peripheral conditions appear in the intermediate solution only. ",
        "Based on Fiss (2011)."
      )
    )
  }
}


#' Extract solution list from QCA object
#' @note When dir.exp is specified, the true Intermediate solution is stored in
#'   sol$i.sol, not sol$solution (which contains the Parsimonious solution).
#' @keywords internal
extract_solution_list <- function(sol) {
  if (is.null(sol)) return(list())
  
  # Method 1: Through i.sol structure (true Intermediate when dir.exp specified)
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    # Collect solutions from all i.sol entries
    solutions <- list()
    for (isol_name in names(sol$i.sol)) {
      isol_sols <- sol$i.sol[[isol_name]]$solution
      if (!is.null(isol_sols) && length(isol_sols) > 0) {
        for (s in isol_sols) {
          solutions <- c(solutions, list(s))
        }
      }
    }
    if (length(solutions) > 0) return(solutions)
  }
  
  # Method 2: Direct $solution (Parsimonious or when dir.exp not specified)
  if (!is.null(sol$solution) && length(sol$solution) > 0) {
    return(sol$solution)
  }
  
  # Method 3: If passed as simple character vector of paths
  if (is.character(sol)) {
    return(list(sol))
  }
  
  # Method 4: If passed as list of character vectors
  if (is.list(sol) && all(sapply(sol, is.character))) {
    return(sol)
  }
  
  list()
}


#' Extract paths from a single solution
#' @keywords internal
extract_paths_from_solution <- function(solution) {
  if (is.null(solution)) return(character(0))
  
  # If it's a character vector with multiple elements, those are paths
  if (is.character(solution) && length(solution) > 1) {
    return(solution)
  }
  
  # If it's a single string, check for " + " separator
  if (is.character(solution) && length(solution) == 1) {
    if (grepl(" \\+ ", solution)) {
      return(trimws(unlist(strsplit(solution, " \\+ "))))
    }
    return(solution)
  }
  
  character(0)
}


#' Build configuration chart for a single solution
#' @keywords internal
build_single_chart <- function(paths, sol, symbols, labels, 
                                include_metrics, condition_order = NULL,
                                solution_index = 1) {
  
  if (length(paths) == 0) {
    return(paste0("*", labels$no_solution, "*"))
  }
  
  # Determine condition order
  if (is.null(condition_order)) {
    conditions <- extract_conditions_from_paths(paths)
  } else {
    conditions <- condition_order
  }
  
  # Build matrix
  mat <- build_config_matrix(paths, conditions, symbols)
  
  # Convert to markdown
  table_str <- config_matrix_to_md(mat, labels$condition)
  
  # Add metrics if requested and available
  if (include_metrics && !is.null(sol)) {
    # Try to get per-path metrics
    path_metrics <- extract_path_metrics_for_chart(sol, solution_index)
    
    if (!is.null(path_metrics) && nrow(path_metrics) == length(paths)) {
      table_str <- add_metrics_rows(table_str, path_metrics, labels)
    }
    
    # Add solution-level metrics
    sol_metrics <- extract_solution_metrics_for_chart(sol, solution_index)
    
    if (!is.null(sol_metrics)) {
      table_str <- paste0(
        table_str, "\n\n",
        "**", labels$solution_consistency, "**: ", 
        format(round(sol_metrics$inclS, 3), nsmall = 3), "  \n",
        "**", labels$solution_coverage, "**: ", 
        format(round(sol_metrics$covS, 3), nsmall = 3)
      )
    }
  }
  
  table_str
}


#' Extract per-path metrics for configuration chart
#' @keywords internal
extract_path_metrics_for_chart <- function(sol, solution_index = 1) {
  if (is.null(sol)) return(NULL)

  # Mirror the displayed-paths source (i.sol first for intermediate), as in
  # extract_solution_metrics_for_chart.

  # Path A (intermediate): per-term table from the intermediate solution, BEFORE
  # sol$IC$incl.cov (which is the parsimonious per-term table).
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    ic <- try(sol$i.sol$C1P1$IC$incl.cov, silent = TRUE)
    if ((inherits(ic, "try-error") || is.null(ic)) && length(sol$i.sol) >= solution_index) {
      ic <- try(sol$i.sol[[solution_index]]$IC$incl.cov, silent = TRUE)
    }
    if (!inherits(ic, "try-error") && !is.null(ic)) return(ic)
  }
  # Path B: single solution without dir.exp.
  if (!is.null(sol$IC$incl.cov)) {
    return(sol$IC$incl.cov)
  }
  # Path C: multiple solutions - the displayed solution's per-term table.
  if (!is.null(sol$IC$individual)) {
    indiv <- sol$IC$individual
    if (length(indiv) >= solution_index) {
      if (!is.null(indiv[[solution_index]]$incl.cov)) {
        return(indiv[[solution_index]]$incl.cov)
      }
    }
  }

  NULL
}


#' Extract solution-level metrics for configuration chart
#' @keywords internal
extract_solution_metrics_for_chart <- function(sol, solution_index = 1) {
  if (is.null(sol)) return(NULL)
  mk <- function(ic) if (!is.null(ic)) list(inclS = ic$inclS[1], covS = ic$covS[1]) else NULL

  # The displayed paths come from get_solution_list(), which uses sol$i.sol first
  # (intermediate, when dir.exp was given) and only then sol$solution. The fit must
  # follow the same source, so:
  #
  # Path A (intermediate): read the intermediate solution's own fit. Checked BEFORE
  # sol$IC, whose sol.incl.cov / overall describe the PARSIMONIOUS solution.
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    ic <- try(sol$i.sol$C1P1$IC$sol.incl.cov, silent = TRUE)
    if ((inherits(ic, "try-error") || is.null(ic)) && length(sol$i.sol) >= solution_index) {
      ic <- try(sol$i.sol[[solution_index]]$IC$sol.incl.cov, silent = TRUE)
    }
    if (!inherits(ic, "try-error") && !is.null(ic)) return(mk(ic))
  }
  # Path B (single parsimonious/complex solution).
  if (!is.null(sol$IC$sol.incl.cov)) return(mk(sol$IC$sol.incl.cov))
  # Path C (multiple solutions): the displayed solution's OWN fit, not the overall
  # (disjunction-of-all) aggregate.
  if (!is.null(sol$IC$individual) && length(sol$IC$individual) >= solution_index) {
    ic <- try(sol$IC$individual[[solution_index]]$sol.incl.cov, silent = TRUE)
    if (!inherits(ic, "try-error") && !is.null(ic)) return(mk(ic))
  }
  # Path D (last-resort fallback): overall aggregate.
  if (!is.null(sol$IC$overall$sol.incl.cov)) return(mk(sol$IC$overall$sol.incl.cov))

  NULL
}


#' Add metrics rows to markdown table
#' @keywords internal
add_metrics_rows <- function(table_str, metrics, labels) {
  
  n_paths <- nrow(metrics)
  
  # Add consistency row
  if ("inclS" %in% names(metrics)) {
    vals <- format(round(metrics$inclS, 3), nsmall = 3)
    row <- paste0("| **", labels$consistency, "** | ", 
                  paste(vals, collapse = " | "), " |")
    table_str <- paste0(table_str, "\n", row)
  }
  
  # Add raw coverage row
  if ("covS" %in% names(metrics)) {
    vals <- format(round(metrics$covS, 3), nsmall = 3)
    row <- paste0("| **", labels$raw_coverage, "** | ", 
                  paste(vals, collapse = " | "), " |")
    table_str <- paste0(table_str, "\n", row)
  }
  
  # Add unique coverage row
  if ("covU" %in% names(metrics)) {
    vals <- format(round(metrics$covU, 3), nsmall = 3)
    row <- paste0("| **", labels$unique_coverage, "** | ", 
                  paste(vals, collapse = " | "), " |")
    table_str <- paste0(table_str, "\n", row)
  }
  
  table_str
}


#' Generate configuration chart from paths (simple interface)
#'
#' A simpler interface for generating configuration charts when you have
#' paths directly (without a full QCA solution object).
#'
#' @param paths Character vector. Paths in QCA notation (e.g., "A*B*~C").
#' @param symbol_set Character. One of \code{"unicode"}, \code{"ascii"}, 
#'   or \code{"latex"}.
#' @param language Character. \code{"en"} for English, \code{"ja"} for Japanese.
#' @param condition_order Character vector. Optional ordering of conditions.
#' @param n_sol Integer. Number of equivalent solutions. If > 1, a note is added
#'   explaining that multiple solutions exist and M1 is shown. Default is 1.
#' @param solution_note Logical. Whether to add solution note when n_sol > 1.
#'   Default is TRUE.
#' @param solution_note_style Character. \code{"simple"} or \code{"detailed"}.
#'   Default is \code{"simple"}.
#' @param epi_list Character vector. Essential prime implicants for detailed notes.
#'   Only used when \code{solution_note_style = "detailed"}.
#'
#' @return Character string containing Markdown-formatted table.
#'
#' @export
#'
#' @examples
#' # Simple usage with paths
#' paths <- c("A*B", "A*C*~D", "B*E")
#' chart <- config_chart_from_paths(paths)
#' cat(chart)
#'
#' # With ASCII symbols
#' chart <- config_chart_from_paths(paths, symbol_set = "ascii")
#' cat(chart)
#'
#' # With multiple solution note
#' chart <- config_chart_from_paths(paths, n_sol = 2)
#' cat(chart)
#'
#' # With detailed note including EPIs
#' chart <- config_chart_from_paths(
#'   paths, n_sol = 2,
#'   solution_note_style = "detailed",
#'   epi_list = c("A*B")
#' )
#' cat(chart)
config_chart_from_paths <- function(paths,
                                     symbol_set = c("unicode", "ascii", "latex"),
                                     language = c("en", "ja"),
                                     condition_order = NULL,
                                     n_sol = 1L,
                                     solution_note = TRUE,
                                     solution_note_style = c("simple", "detailed"),
                                     epi_list = NULL) {
  
  symbol_set <- match.arg(symbol_set)
  language <- match.arg(language)
  solution_note_style <- match.arg(solution_note_style)
  symbols <- SYMBOL_SETS[[symbol_set]]
  labels <- get_config_labels(language)
  
  # Get symbol note
  legend_note <- if (language == "ja") symbols$note_ja else symbols$note_en
  
  # Determine format for solution note
  format_type <- if (symbol_set == "latex") "latex" else "markdown"
  
  # Determine conditions
  if (is.null(condition_order)) {
    conditions <- extract_conditions_from_paths(paths)
  } else {
    conditions <- condition_order
  }
  
  # Build matrix and table
  mat <- build_config_matrix(paths, conditions, symbols)
  table_str <- config_matrix_to_md(mat, labels$condition)
  
  # Add legend
  result <- paste0(table_str, "\n\n*", legend_note, "*")
  
  # Add solution note if multiple solutions exist
  if (solution_note && !is.null(n_sol) && n_sol > 1) {
    sol_note <- generate_solution_note(
      n_sol = n_sol,
      epi_list = epi_list,
      style = solution_note_style,
      language = language,
      format = format_type
    )
    result <- paste0(result, "\n\n", sol_note)
  }
  
  paste0(result, "\n")
}


#' Generate configuration chart for multiple solutions (simple interface)
#'
#' Generates separate configuration charts for multiple solutions.
#'
#' @param solutions List of character vectors. Each element is a vector of 
#'   paths for one solution.
#' @param symbol_set Character. One of \code{"unicode"}, \code{"ascii"}, 
#'   or \code{"latex"}.
#' @param language Character. \code{"en"} for English, \code{"ja"} for Japanese.
#' @param condition_order Character vector. Optional ordering of conditions.
#' @param show_epi Logical. Whether to identify and display Essential Prime
#'   Implicants (EPIs) in the note. Default is FALSE.
#'
#' @return Character string containing Markdown-formatted tables.
#'
#' @export
#'
#' @examples
#' # Multiple solutions
#' solutions <- list(
#'   c("A*B", "C"),
#'   c("A*B", "D"),
#'   c("A*C")
#' )
#' chart <- config_chart_multi_solutions(solutions)
#' cat(chart)
#'
#' # With EPI identification
#' chart <- config_chart_multi_solutions(solutions, show_epi = TRUE)
#' cat(chart)
config_chart_multi_solutions <- function(solutions,
                                          symbol_set = c("unicode", "ascii", "latex"),
                                          language = c("en", "ja"),
                                          condition_order = NULL,
                                          show_epi = FALSE) {
  
  symbol_set <- match.arg(symbol_set)
  language <- match.arg(language)
  symbols <- SYMBOL_SETS[[symbol_set]]
  labels <- get_config_labels(language)
  
  # Get symbol note
  legend_note <- if (language == "ja") symbols$note_ja else symbols$note_en
  
  n_solutions <- length(solutions)
  
  # Identify EPIs if requested
  epi_info <- NULL
  if (show_epi && n_solutions > 1) {
    epi_info <- identify_epi(solutions)
  }
  
  # Build note message
  if (show_epi && !is.null(epi_info) && length(epi_info$epi) > 0) {
    # Format EPIs
    epi_str <- paste(epi_info$epi, collapse = ", ")
    epi_str <- gsub("\\*", "\u00B7", epi_str)  # middle dot for markdown
    
    if (language == "ja") {
      note_msg <- paste0(
        "**", labels$note, ":** ", n_solutions, " ", labels$equiv_solutions, " ",
        labels$separate_tables, "\n\n",
        "**Essential Prime Implicants (EPI):** ", epi_str, "\n\n"
      )
    } else {
      note_msg <- paste0(
        "**", labels$note, ":** ", n_solutions, " ", labels$equiv_solutions, " ",
        labels$separate_tables, "\n\n",
        "**Essential Prime Implicants (EPI):** ", epi_str, "\n\n"
      )
    }
  } else if (show_epi && !is.null(epi_info) && length(epi_info$epi) == 0) {
    # No EPIs - all terms are SPIs
    if (language == "ja") {
      note_msg <- paste0(
        "**", labels$note, ":** ", n_solutions, " ", labels$equiv_solutions, " ",
        labels$separate_tables, "\n\n",
        "*", labels$no_epi, "*\n\n"
      )
    } else {
      note_msg <- paste0(
        "**", labels$note, ":** ", n_solutions, " ", labels$equiv_solutions, " ",
        labels$separate_tables, "\n\n",
        "*", labels$no_epi, "*\n\n"
      )
    }
  } else {
    # Simple note
    note_msg <- paste0(
      "**", labels$note, ":** ", n_solutions, " ", labels$equiv_solutions, " ",
      labels$separate_tables, "\n\n"
    )
  }
  
  # Generate chart for each solution
  charts <- lapply(seq_along(solutions), function(i) {
    paths <- solutions[[i]]
    
    # Determine conditions
    if (is.null(condition_order)) {
      conditions <- extract_conditions_from_paths(paths)
    } else {
      conditions <- condition_order
    }
    
    header <- paste0("### ", labels$solution, " M", i, "\n\n")
    mat <- build_config_matrix(paths, conditions, symbols)
    table_str <- config_matrix_to_md(mat, labels$condition)
    paste0(header, table_str)
  })
  
  # Combine
  paste0(note_msg, 
         paste(charts, collapse = "\n\n---\n\n"),
         "\n\n*", legend_note, "*\n")
}
