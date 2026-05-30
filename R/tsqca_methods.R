###############################################
# S3 Methods for ThSQCA Result Objects
# v0.4.0: print() and summary() methods
###############################################

# ============================================
# PRINT METHODS
# ============================================

#' Print method for ThSQCA results
#'
#' Displays a concise overview of ThSQCA analysis results.
#'
#' @param x A ThSQCA result object returned by one of the sweep functions.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' \donttest{
#' data(sample_data)
#' result <- otSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:8,
#'   thrX = c(X1 = 7, X2 = 7, X3 = 7)
#' )
#' print(result)
#' }
#'
#' @export
print.tsqca_result <- function(x, ...) {
  
  # Determine analysis type from class
  analysis_type <- class(x)[1]
  type_label <- switch(analysis_type,
    "otSweep_result" = "OTS-QCA (Outcome Threshold Sweep)",
    "dtSweep_result" = "DTS-QCA (Dual Threshold Sweep)",
    "ctSweepS_result" = "CTS-QCA (Condition Threshold Sweep - Single)",
    "ctSweepM_result" = "MCTS-QCA (Multi-Condition Threshold Sweep)",
    "ThSQCA Analysis"
  )
  
  cat(type_label, "Result\n")
  cat(strrep("=", nchar(type_label) + 7), "\n\n")
  
  # Extract params
  p <- x$params
  
  # Outcome info
  if (!is.null(p$outcome)) {
    outcome_str <- p$outcome
    if (isTRUE(p$negate_outcome)) {
      outcome_str <- paste0(outcome_str, " (negated)")
    }
    cat("Outcome:", outcome_str, "\n")
  }
  
  # Conditions info
  if (!is.null(p$conditions)) {
    cat("Conditions:", paste(p$conditions, collapse = ", "), "\n")
  }
  
  # Threshold info (varies by analysis type)
  if (analysis_type == "otSweep_result") {
    if (!is.null(p$sweep_range)) {
      cat("Y thresholds swept:", paste(p$sweep_range, collapse = ", "), "\n")
    }
    if (!is.null(p$thrX)) {
      thrX_str <- paste(names(p$thrX), p$thrX, sep = "=", collapse = ", ")
      cat("Fixed X thresholds:", thrX_str, "\n")
    }
  } else if (analysis_type == "dtSweep_result") {
    if (!is.null(p$sweep_range_Y)) {
      cat("Y thresholds swept:", paste(p$sweep_range_Y, collapse = ", "), "\n")
    }
    if (!is.null(p$sweep_list_X)) {
      cat("X thresholds swept:", length(p$sweep_list_X), "condition(s)\n")
    }
  } else if (analysis_type == "ctSweepS_result") {
    if (!is.null(p$sweep_var)) {
      cat("Swept condition:", p$sweep_var, "\n")
    }
    if (!is.null(p$sweep_range)) {
      cat("Thresholds swept:", paste(p$sweep_range, collapse = ", "), "\n")
    }
    if (!is.null(p$thrY)) {
      cat("Fixed Y threshold:", p$thrY, "\n")
    }
  } else if (analysis_type == "ctSweepM_result") {
    if (!is.null(p$sweep_list)) {
      cat("Conditions swept:", length(p$sweep_list), "\n")
    }
    if (!is.null(p$thrY)) {
      cat("Fixed Y threshold:", p$thrY, "\n")
    }
  }
  
  cat("\n")
  
  # Summary statistics
  df <- x$summary
  n_total <- nrow(df)
  n_valid <- sum(df$expression != "No solution", na.rm = TRUE)
  n_multi <- sum(df$n_solutions > 1, na.rm = TRUE)
  
  cat("Threshold settings analyzed:", n_total, "\n")
  cat("  - Valid solutions:", n_valid, "\n")
  cat("  - No solution:", n_total - n_valid, "\n")
  
  if (n_multi > 0) {
    cat("  - Multiple solutions:", n_multi, "\n")
  }
  
  cat("\n")
  cat("Use summary() for detailed results table\n")
  cat("Use generate_report() for full markdown report\n")
  
  invisible(x)
}

#' @rdname print.tsqca_result
#' @export
print.otSweep_result <- function(x, ...) {
  NextMethod()
}

#' @rdname print.tsqca_result
#' @export
print.dtSweep_result <- function(x, ...) {
  NextMethod()
}

#' @rdname print.tsqca_result
#' @export
print.ctSweepS_result <- function(x, ...) {
  NextMethod()
}

#' @rdname print.tsqca_result
#' @export
print.ctSweepM_result <- function(x, ...) {
  NextMethod()
}


# ============================================
# SUMMARY METHODS
# ============================================

#' Summary method for ThSQCA results
#'
#' Displays detailed results table with solution formulas and fit measures.
#'
#' @param object A ThSQCA result object returned by one of the sweep functions.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{object}.
#'
#' @examples
#' \donttest{
#' data(sample_data)
#' result <- otSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:8,
#'   thrX = c(X1 = 7, X2 = 7, X3 = 7)
#' )
#' summary(result)
#' }
#'
#' @export
summary.tsqca_result <- function(object, ...) {
  
  # Determine analysis type from class
  analysis_type <- class(object)[1]
  type_label <- switch(analysis_type,
    "otSweep_result" = "OTS-QCA",
    "dtSweep_result" = "DTS-QCA",
    "ctSweepS_result" = "CTS-QCA",
    "ctSweepM_result" = "MCTS-QCA",
    "ThSQCA"
  )
  
  cat(type_label, "Summary\n")
  cat(strrep("=", nchar(type_label) + 8), "\n\n")
  
  # Analysis Parameters
  cat("Analysis Parameters:\n")
  p <- object$params
  
  if (!is.null(p$outcome)) {
    outcome_str <- p$outcome
    if (isTRUE(p$negate_outcome)) {
      outcome_str <- paste0(outcome_str, " (negated)")
    }
    cat("  Outcome:", outcome_str, "\n")
  }
  if (!is.null(p$conditions)) {
    cat("  Conditions:", paste(p$conditions, collapse = ", "), "\n")
  }
  if (!is.null(p$incl.cut)) {
    cat("  Consistency cutoff:", p$incl.cut, "\n")
  }
  if (!is.null(p$n.cut)) {
    cat("  Frequency cutoff:", p$n.cut, "\n")
  }
  
  cat("\n")
  
  # Results Table
  cat("Results by Threshold:\n\n")
  
  df <- object$summary
  
  # Select columns to display based on what's available
  display_cols <- intersect(
    c("thrY", "threshold", "combo_id", "thrX", 
      "expression", "inclS", "covS", "n_solutions"),
    names(df)
  )
  
  df_display <- df[, display_cols, drop = FALSE]
  
  # Round numeric columns
  for (col in names(df_display)) {
    if (is.numeric(df_display[[col]]) && col %in% c("inclS", "covS")) {
      df_display[[col]] <- round(df_display[[col]], 3)
    }
  }
  
  # Print as formatted table
  print(df_display, row.names = FALSE)
  
  cat("\n")
  
  # Notes
  n_multi <- sum(df$n_solutions > 1, na.rm = TRUE)
  if (n_multi > 0) {
    cat("Note:", n_multi, "threshold setting(s) had multiple solutions.\n")
    cat("      Use generate_report() for full details.\n")
  }
  
  invisible(object)
}

#' @rdname summary.tsqca_result
#' @export
summary.otSweep_result <- function(object, ...) {
  NextMethod()
}

#' @rdname summary.tsqca_result
#' @export
summary.dtSweep_result <- function(object, ...) {
  NextMethod()
}

#' @rdname summary.tsqca_result
#' @export
summary.ctSweepS_result <- function(object, ...) {
  NextMethod()
}

#' @rdname summary.tsqca_result
#' @export
summary.ctSweepM_result <- function(object, ...) {
  NextMethod()
}
