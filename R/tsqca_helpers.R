###############################################
# Helper functions for ThSQCA
###############################################

#' Sanitize a truth table before Boolean minimization
#'
#' Guards against two behaviors of \code{QCA::truthTable()} (observed with
#' QCA 3.25 / admisc 0.40) that can affect programmatic, repeated calls such
#' as threshold sweeps. For sparse truth tables the \code{incl}/\code{PRI}
#' columns may be returned as \code{character} rather than \code{numeric},
#' and logical-remainder rows (observed \code{n = 0}) may carry the string
#' \code{"-"}. Passing such a truth table to \code{QCA::minimize()} can, for
#' some truth table structures, cause the minimization to hang or return
#' misleading fit values. This helper coerces \code{incl}/\code{PRI} to
#' numeric and sets the remainder rows to 0.
#'
#' The \code{OUT} column is deliberately left untouched: \code{minimize()}
#' uses \code{OUT} (with the symbolic \code{"?"} for remainders) to identify
#' positive, negative, and remainder rows, so setting \code{incl}/\code{PRI}
#' to 0 on remainder rows does not alter remainder handling.
#'
#' @param tt A truth table object returned by \code{QCA::truthTable()}.
#'
#' @return The same object with numeric \code{incl}/\code{PRI} columns and
#'   no \code{NA} in those columns. Returns the input unchanged if it is
#'   \code{NULL} or lacks a \code{$tt} data frame.
#' @keywords internal
sanitize_truthtable <- function(tt) {
  if (is.null(tt) || is.null(tt$tt)) return(tt)
  if (!is.null(tt$tt$incl) && is.character(tt$tt$incl)) {
    tt$tt$incl <- suppressWarnings(as.numeric(tt$tt$incl))
  }
  if (!is.null(tt$tt$PRI) && is.character(tt$tt$PRI)) {
    tt$tt$PRI <- suppressWarnings(as.numeric(tt$tt$PRI))
  }
  if (!is.null(tt$tt$incl)) tt$tt$incl[is.na(tt$tt$incl)] <- 0
  if (!is.null(tt$tt$PRI))  tt$tt$PRI[is.na(tt$tt$PRI)]   <- 0
  tt
}

#' Escape special characters for Markdown
#'
#' Escapes asterisks and other special characters that have special
#' meaning in Markdown syntax.
#'
#' @param text Character. Text to escape.
#'
#' @return Character. Text with special characters escaped.
#' @keywords internal
escape_md <- function(text) {
  if (is.null(text) || length(text) == 0) return(text)
  # Escape * to \* for Markdown

  gsub("\\*", "\\\\*", text)
}


#' Extract all metrics from QCA solution object
#'
#' Safely extracts solution-level and term-level metrics from
#' various QCA solution object structures. Handles cases where
#' multiple solutions exist and sol.incl.cov may be NULL.
#'
#' @param IC IC object from QCA solution.
#' @param sol_obj Full solution object (optional, for fallback).
#'
#' @return List with elements:
#'   \itemize{
#'     \item \code{sol_inclS} — solution consistency (or NA)
#'     \item \code{sol_PRI} — solution PRI (or NA)
#'     \item \code{sol_covS} — solution coverage (or NA)
#'     \item \code{term_df} — data frame of per-term metrics (or NULL)
#'   }
#' @keywords internal
extract_all_metrics <- function(IC, sol_obj = NULL) {
  
  result <- list(
    sol_inclS = NA_real_,
    sol_PRI   = NA_real_,
    sol_covS  = NA_real_,
    term_df   = NULL
  )
  
  # Try to get from IC directly
  if (!is.null(IC)) {
    
    # Method 1: sol.incl.cov (single solution case)
    if ("sol.incl.cov" %in% names(IC)) {
      sol <- IC$sol.incl.cov
      if (!is.null(sol$inclS)) result$sol_inclS <- sol$inclS[1]
      if (!is.null(sol$PRI)) result$sol_PRI <- sol$PRI[1]
      if (!is.null(sol$covS)) result$sol_covS <- sol$covS[1]
    }
    
    # Method 2: overall (multiple solutions case)
    if (is.na(result$sol_inclS) && "overall" %in% names(IC)) {
      overall <- IC$overall
      if (is.list(overall)) {
        if ("sol.incl.cov" %in% names(overall)) {
          sol <- overall$sol.incl.cov
          if (!is.null(sol$inclS)) result$sol_inclS <- sol$inclS[1]
          if (!is.null(sol$PRI)) result$sol_PRI <- sol$PRI[1]
          if (!is.null(sol$covS)) result$sol_covS <- sol$covS[1]
        }
        if (is.na(result$sol_inclS) && !is.null(overall$inclS)) {
          result$sol_inclS <- overall$inclS[1]
        }
        if (is.na(result$sol_PRI) && !is.null(overall$PRI)) {
          result$sol_PRI <- overall$PRI[1]
        }
        if (is.na(result$sol_covS) && !is.null(overall$covS)) {
          result$sol_covS <- overall$covS[1]
        }
      }
    }
    
    # Method 3: individual (multiple solutions, first solution metrics)
    if ("individual" %in% names(IC)) {
      indiv <- IC$individual
      if (is.list(indiv) && length(indiv) > 0) {
        first_indiv <- indiv[[1]]
        if (is.list(first_indiv)) {
          # Get sol_inclS if not already found
          if (is.na(result$sol_inclS) && "sol.incl.cov" %in% names(first_indiv)) {
            sol <- first_indiv$sol.incl.cov
            if (!is.null(sol$inclS)) result$sol_inclS <- sol$inclS[1]
            if (!is.null(sol$PRI)) result$sol_PRI <- sol$PRI[1]
            if (!is.null(sol$covS)) result$sol_covS <- sol$covS[1]
          }
          # Always try to get term_df from individual
          if (is.null(result$term_df) && "incl.cov" %in% names(first_indiv)) {
            result$term_df <- first_indiv$incl.cov
          }
        }
      }
    }
    
    # Get per-term metrics (incl.cov)
    if (is.null(result$term_df) && "incl.cov" %in% names(IC)) {
      result$term_df <- IC$incl.cov
    }
    
    # If we got values, return
    if (!is.na(result$sol_inclS)) return(result)
  }
  
  # Fallback: try to get from sol_obj
  if (!is.null(sol_obj)) {
    
    # Method 4: sol_obj$IC
    if (!is.null(sol_obj$IC)) {
      IC <- sol_obj$IC
      
      # Try to get term_df from sol_obj$IC$incl.cov first
      if (is.null(result$term_df) && "incl.cov" %in% names(IC)) {
        result$term_df <- IC$incl.cov
      }
      
      if ("overall" %in% names(IC)) {
        overall <- IC$overall
        if (is.list(overall)) {
          if ("sol.incl.cov" %in% names(overall)) {
            sol <- overall$sol.incl.cov
            if (!is.null(sol$inclS)) result$sol_inclS <- sol$inclS[1]
            if (!is.null(sol$PRI)) result$sol_PRI <- sol$PRI[1]
            if (!is.null(sol$covS)) result$sol_covS <- sol$covS[1]
          }
          if (is.na(result$sol_inclS) && !is.null(overall$inclS)) {
            result$sol_inclS <- overall$inclS[1]
          }
          if (is.na(result$sol_PRI) && !is.null(overall$PRI)) {
            result$sol_PRI <- overall$PRI[1]
          }
          if (is.na(result$sol_covS) && !is.null(overall$covS)) {
            result$sol_covS <- overall$covS[1]
          }
        }
      }
      
      # Method 4b: individual (always check for term_df)
      if ("individual" %in% names(IC)) {
        indiv <- IC$individual
        if (is.list(indiv) && length(indiv) > 0) {
          first_indiv <- indiv[[1]]
          if (is.list(first_indiv)) {
            if (is.na(result$sol_inclS) && "sol.incl.cov" %in% names(first_indiv)) {
              sol <- first_indiv$sol.incl.cov
              if (!is.null(sol$inclS)) result$sol_inclS <- sol$inclS[1]
              if (!is.null(sol$PRI)) result$sol_PRI <- sol$PRI[1]
              if (!is.null(sol$covS)) result$sol_covS <- sol$covS[1]
            }
            if (is.null(result$term_df) && "incl.cov" %in% names(first_indiv)) {
              result$term_df <- first_indiv$incl.cov
            }
          }
        }
      }
    }
    
    # Method 5: i.sol (named list: C1P1, C1P2, etc.)
    # Check for term_df even if sol_inclS is already found
    if (!is.null(sol_obj$i.sol) && length(sol_obj$i.sol) > 0) {
      first_isol <- sol_obj$i.sol[[1]]
      if (!is.null(first_isol$IC)) {
        IC_isol <- first_isol$IC
        if (is.na(result$sol_inclS) && "sol.incl.cov" %in% names(IC_isol)) {
          sol <- IC_isol$sol.incl.cov
          if (!is.null(sol$inclS)) result$sol_inclS <- sol$inclS[1]
          if (!is.null(sol$PRI)) result$sol_PRI <- sol$PRI[1]
          if (!is.null(sol$covS)) result$sol_covS <- sol$covS[1]
        }
        if (is.null(result$term_df) && "incl.cov" %in% names(IC_isol)) {
          result$term_df <- IC_isol$incl.cov
        }
        # Also check individual within i.sol
        if (is.null(result$term_df) && "individual" %in% names(IC_isol)) {
          indiv_isol <- IC_isol$individual
          if (is.list(indiv_isol) && length(indiv_isol) > 0) {
            first_indiv_isol <- indiv_isol[[1]]
            if (is.list(first_indiv_isol) && "incl.cov" %in% names(first_indiv_isol)) {
              result$term_df <- first_indiv_isol$incl.cov
            }
          }
        }
      }
    }
  }
  
  return(result)
}


#' Convert data frame to Markdown table
#'
#' @param df Data frame to convert.
#' @param digits Number of decimal places for numeric columns.
#'
#' @return Character string of Markdown table.
#' @keywords internal
df_to_md_table <- function(df, digits = 3) {
  if (is.null(df) || nrow(df) == 0) return("(No data)\n")
  
  # Round numeric columns
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]] <- round(df[[col]], digits)
    }
  }
  
  # Convert all to character for consistent output
  df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  
  # Escape special Markdown characters (especially * in QCA expressions)
  df <- as.data.frame(lapply(df, escape_md), stringsAsFactors = FALSE)
  
  # Build header
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  separator <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  
  # Build rows
  rows <- apply(df, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })
  
  paste(c(header, separator, rows), collapse = "\n")
}


#' Split solution expression into terms
#'
#' @param solution Character. Solution expression (e.g., "X1*X2 + X3").
#'
#' @return Character vector of terms.
#' @keywords internal
split_solution_terms <- function(solution) {
  if (is.null(solution) || is.na(solution) || solution == "" || 
      solution == "No solution" || solution == "No essential prime implicants") {
    return(character(0))
  }
  trimws(unlist(strsplit(solution, " \\+ ")))
}


#' Get all unique terms from multiple solutions
#'
#' @param sol_list List of solution character vectors from minimize().
#'
#' @return List with:
#'   \itemize{
#'     \item \code{all_terms} — all terms (with duplicates)
#'     \item \code{unique_terms} — unique terms
#'     \item \code{term_counts} — table of term frequencies
#'   }
#' @keywords internal
get_all_terms <- function(sol_list) {
  if (is.null(sol_list) || length(sol_list) == 0) {
    return(list(
      all_terms    = character(0),
      unique_terms = character(0),
      term_counts  = table(character(0))
    ))
  }
  
  # Extract terms from each solution
  all_terms <- unlist(lapply(sol_list, function(sol) {
    split_solution_terms(paste(sol, collapse = " + "))
  }))
  
  list(
    all_terms    = all_terms,
    unique_terms = unique(all_terms),
    term_counts  = table(all_terms)
  )
}


###############################################
# Solution formatting functions
###############################################

#' Format a single QCA term
#'
#' Inserts \code{*} between variables in a term where it may have been omitted.
#'
#' @param term Character. A single term (e.g., "KSPRVT" or "~KPR*PRD").
#' @param var_names Character vector. Variable names used in the analysis.
#' @param use_tilde Logical. If TRUE, negation is represented as \code{~VAR}.
#'   If FALSE, negation is represented as lowercase (e.g., \code{var}).
#'
#' @return Character. The formatted term with \code{*} between all variables.
#' @export
#'
#' @examples
#' var_names <- c("KSP", "KPR", "PRD", "RVT", "RCM")
#' format_qca_term("KSPRVTRCM", var_names)
#' # Returns: "KSP*RVT*RCM"
#' 
#' format_qca_term("~KPRPRD", var_names)
#' # Returns: "~KPR*PRD"
format_qca_term <- function(term, var_names, use_tilde = TRUE) {
  
  if (is.null(term) || is.na(term) || term == "") {
    return("")
  }
  
  # If already contains *, assume it's formatted
  if (grepl("\\*", term)) {
    return(term)
  }
  
  # Sort variable names by length (descending) to avoid partial matches
  var_names_sorted <- var_names[order(nchar(var_names), decreasing = TRUE)]
  
  # Build pattern for matching
  if (use_tilde) {
    # Match ~VAR or VAR
    patterns <- paste0("~?", var_names_sorted)
  } else {
    # Match VAR (uppercase) or var (lowercase for negation)
    patterns <- c(var_names_sorted, tolower(var_names_sorted))
  }
  
  pattern <- paste(patterns, collapse = "|")
  
  # Extract all matches
  matches <- regmatches(term, gregexpr(pattern, term, ignore.case = FALSE))[[1]]
  
  if (length(matches) == 0) {
    return(term)
  }
  
  # Join with *
  paste(matches, collapse = "*")
}


#' Format a QCA solution expression
#'
#' Formats a complete solution expression (multiple terms joined by +).
#'
#' @param solution Character. A solution expression (e.g., "KSPRVT + ~KPRPRD").
#' @param var_names Character vector. Variable names used in the analysis.
#' @param use_tilde Logical. If TRUE, negation is represented as \code{~VAR}.
#'
#' @return Character. The formatted solution expression.
#' @export
#'
#' @examples
#' var_names <- c("KSP", "KPR", "PRD", "RVT", "RCM")
#' format_qca_solution("KSPRVT + ~KPRPRD + RCM", var_names)
#' # Returns: "KSP*RVT + ~KPR*PRD + RCM"
format_qca_solution <- function(solution, var_names, use_tilde = TRUE) {
  
  if (is.null(solution) || is.na(solution) || solution == "") {
    return("")
  }
  
  if (solution == "No solution" || solution == "No essential prime implicants") {
    return(solution)
  }
  
  # Split by " + "
  terms <- trimws(unlist(strsplit(solution, " \\+ ")))
  
  # Format each term
  formatted_terms <- sapply(terms, format_qca_term, 
                            var_names = var_names, 
                            use_tilde = use_tilde,
                            USE.NAMES = FALSE)
  
  # Rejoin
  paste(formatted_terms, collapse = " + ")
}


#' Format multiple QCA solutions
#'
#' Formats a vector of solution expressions.
#'
#' @param solutions Character vector. Solution expressions from \code{minimize()}.
#' @param var_names Character vector. Variable names used in the analysis.
#' @param use_tilde Logical. If TRUE, negation is represented as \code{~VAR}.
#'
#' @return Character vector. Formatted solution expressions.
#' @export
#'
#' @examples
#' var_names <- c("KSP", "KPR", "PRD", "RVT", "RCM")
#' solutions <- c("KSPRVT + RCM", "~KPRPRD")
#' format_qca_solutions(solutions, var_names)
format_qca_solutions <- function(solutions, var_names, use_tilde = TRUE) {
  sapply(solutions, format_qca_solution,
         var_names = var_names,
         use_tilde = use_tilde,
         USE.NAMES = FALSE)
}


#' Extract and format terms from solutions
#'
#' Extracts individual terms from solution expressions and returns
#' formatted unique terms.
#'
#' @param solutions Character vector. Solution expressions.
#' @param var_names Character vector. Variable names used in the analysis.
#' @param use_tilde Logical. If TRUE, negation is represented as \code{~VAR}.
#'
#' @return List with:
#'   \itemize{
#'     \item \code{all_terms} — all terms (with duplicates)
#'     \item \code{unique_terms} — unique terms
#'     \item \code{n_total} — total term count
#'     \item \code{n_unique} — unique term count
#'   }
#' @export
#'
#' @examples
#' var_names <- c("X1", "X2", "X3")
#' solutions <- c("X1*X2 + X3", "X1*X2 + X1*X3")
#' extract_terms(solutions, var_names)
extract_terms <- function(solutions, var_names, use_tilde = TRUE) {
  
  if (is.null(solutions) || length(solutions) == 0) {
    return(list(
      all_terms    = character(0),
      unique_terms = character(0),
      n_total      = 0L,
      n_unique     = 0L
    ))
  }
  
  # Format solutions first
  formatted <- format_qca_solutions(solutions, var_names, use_tilde)
  
  # Split into terms
  all_terms <- unlist(lapply(formatted, function(sol) {
    if (sol == "" || sol == "No solution" || sol == "No essential prime implicants") {
      return(character(0))
    }
    trimws(unlist(strsplit(sol, " \\+ ")))
  }))
  
  list(
    all_terms    = all_terms,
    unique_terms = unique(all_terms),
    n_total      = length(all_terms),
    n_unique     = length(unique(all_terms))
  )
}
