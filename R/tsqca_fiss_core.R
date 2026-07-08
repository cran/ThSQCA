###############################################
# Fiss Core/Peripheral Classification for ThSQCA
#
# Implements Fiss (2011) core/peripheral distinction:
#   Core condition    : appears in BOTH parsimonious AND intermediate solution
#   Peripheral condition: appears in intermediate solution ONLY
#
# Reference:
#   Fiss, P. C. (2011). Building better causal theories: A fuzzy set approach
#   to typologies in organization research. Academy of Management Journal,
#   54(2), 393-420.
###############################################


# ============================================================
# Symbol sets (4-symbol: core present/absent, peripheral present/absent)
# ============================================================

#' Fiss-style symbol sets (4 symbols)
#' @keywords internal
SYMBOL_SETS_FISS <- list(

  unicode = list(
    core_present   = "\u25CF",   # ● BLACK CIRCLE          (large, filled)
    core_absent    = "\u2297",   # ⊗ CIRCLED TIMES         (large, X)
    periph_present = "\u2299",   # ⊙ CIRCLED DOT OPERATOR  (small, dot)
    periph_absent  = "\u2298",   # ⊘ CIRCLED DIVISION SLASH(small, slash)
    note_en = paste0(
      "\u25CF = core presence, \u2297 = core absence, ",
      "\u2299 = peripheral presence, \u2298 = peripheral absence, ",
      "blank = don't care"
    ),
    note_ja = paste0(
      "\u25CF = \u30b3\u30a2\u5b58\u5728, \u2297 = \u30b3\u30a2\u4e0d\u5728, ",
      "\u2299 = \u5468\u8fba\u5b58\u5728, \u2298 = \u5468\u8fba\u4e0d\u5728, ",
      "\u7a7a\u6b04 = \u7121\u95a2\u4fc2"
    )
  ),

  latex = list(
    core_present   = "$\\bullet$",
    core_absent    = "$\\otimes$",
    periph_present = "$\\odot$",
    periph_absent  = "$\\oslash$",
    note_en = paste0(
      "$\\bullet$ = core presence, $\\otimes$ = core absence, ",
      "$\\odot$ = peripheral presence, $\\oslash$ = peripheral absence, ",
      "blank = don't care"
    ),
    note_ja = paste0(
      "$\\bullet$ = \u30b3\u30a2\u5b58\u5728, $\\otimes$ = \u30b3\u30a2\u4e0d\u5728, ",
      "$\\odot$ = \u5468\u8fba\u5b58\u5728, $\\oslash$ = \u5468\u8fba\u4e0d\u5728, ",
      "\u7a7a\u6b04 = \u7121\u95a2\u4fc2"
    )
  ),

  ascii = list(
    core_present   = "O",    # large / core
    core_absent    = "X",
    periph_present = "o",    # small / peripheral
    periph_absent  = "x",
    note_en = paste0(
      "O = core presence, X = core absence, ",
      "o = peripheral presence, x = peripheral absence, ",
      "blank = don't care"
    ),
    note_ja = paste0(
      "O = \u30b3\u30a2\u5b58\u5728, X = \u30b3\u30a2\u4e0d\u5728, ",
      "o = \u5468\u8fba\u5b58\u5728, x = \u5468\u8fba\u4e0d\u5728, ",
      "\u7a7a\u6b04 = \u7121\u95a2\u4fc2"
    )
  )
)


# ============================================================
# Internal helpers
# ============================================================

#' Extract all (condition, status) pairs present in a set of solution terms
#'
#' @param terms Character vector of solution terms (e.g. c("X1*X2", "~X3"))
#' @param conditions Character vector of all condition names
#'
#' @return Named list: condition name -> character vector of statuses
#'   ("present" and/or "absent") found in any term
#'
#' @keywords internal
extract_cond_status_map <- function(terms, conditions) {
  map <- setNames(vector("list", length(conditions)), conditions)
  for (cond in conditions) {
    statuses <- character(0)
    for (t in terms) {
      s <- get_condition_status(t, cond)
      if (s != "dontcare") statuses <- c(statuses, s)
    }
    map[[cond]] <- unique(statuses)
  }
  map
}


#' Classify each (condition, status) pair in an intermediate term as
#' core or peripheral
#'
#' A condition is **core** when it appears with the same presence/absence
#' status in at least one term of the parsimonious solution.
#' A condition is **peripheral** when it appears in the intermediate term
#' but NOT (with the same status) in any parsimonious term.
#'
#' @param interm_term  Character. Single intermediate-solution term.
#' @param parsim_map   Named list returned by \code{extract_cond_status_map()}
#'   for the parsimonious solution.
#' @param conditions   Character vector of all condition names.
#'
#' @return Data frame with columns:
#'   \code{condition}, \code{status} ("present"/"absent"/"dontcare"),
#'   \code{type} ("core"/"peripheral"/"dontcare").
#'
#' @keywords internal
classify_term_conditions <- function(interm_term, parsim_map, conditions) {

  n <- length(conditions)
  result <- data.frame(
    condition = conditions,
    status    = character(n),
    type      = character(n),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n)) {
    cond     <- conditions[i]
    i_status <- get_condition_status(interm_term, cond)

    if (i_status == "dontcare") {
      result$status[i] <- "dontcare"
      result$type[i]   <- "dontcare"
      next
    }

    result$status[i] <- i_status

    # Core if parsimonious solution also contains this condition with same status
    p_statuses <- parsim_map[[cond]]
    result$type[i] <- if (!is.null(p_statuses) && i_status %in% p_statuses) {
      "core"
    } else {
      "peripheral"
    }
  }

  result
}


#' Run parsimonious QCA minimization on a stored truth table
#'
#' @param truth_table  Truth table object (from QCA::truthTable())
#' @param conditions   Character vector of condition names
#'
#' @return QCA solution object, or NULL on error / no solution
#'
#' @keywords internal
run_parsimonious <- function(truth_table, conditions) {
  truth_table <- sanitize_truthtable(truth_table)  # QCA 3.25 guard
  sol <- try(
    QCA::minimize(
      truth_table,
      include    = "?",
      dir.exp    = NULL,    # parsimonious: no directional expectations
      details    = TRUE,
      show.cases = FALSE
    ),
    silent = TRUE
  )
  if (inherits(sol, "try-error")) return(NULL)
  sol
}


#' Extract solution terms from a QCA solution object (intermediate or parsim)
#'
#' @param sol  QCA solution object
#'
#' @return Character vector of terms (prime implicants), or character(0)
#'
#' @keywords internal
extract_sol_terms <- function(sol) {
  if (is.null(sol)) return(character(0))

  # Intermediate solution stored in i.sol
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    terms <- character(0)
    for (entry in sol$i.sol) {
      if (!is.null(entry$solution) && length(entry$solution) > 0) {
        for (s in entry$solution) {
          parsed <- parse_solution_terms(paste(s, collapse = " + "))
          terms  <- c(terms, parsed)
        }
      }
    }
    if (length(terms) > 0) return(unique(terms))
  }

  # Parsimonious / complex solution in $solution
  if (!is.null(sol$solution) && length(sol$solution) > 0) {
    terms <- character(0)
    for (s in sol$solution) {
      parsed <- parse_solution_terms(paste(s, collapse = " + "))
      terms  <- c(terms, parsed)
    }
    return(unique(terms))
  }

  character(0)
}


# ============================================================
# Public API
# ============================================================

#' Compute Fiss Core/Peripheral Classification for Sweep Results
#'
#' Takes an existing threshold-sweep result object (produced by
#' \code{\link{otSweep}}, \code{\link{ctSweepS}}, \code{\link{ctSweepM}},
#' or \code{\link{dtSweep}}) and augments it with Fiss (2011)
#' core/peripheral classification.
#'
#' The classification requires that:
#' \itemize{
#'   \item The sweep was run with \code{include = "?"} (to allow parsimonious
#'         computation)
#'   \item \code{return_details = TRUE} was used (truth tables must be stored)
#'   \item \code{dir.exp} was specified (i.e., the sweep produced intermediate
#'         solutions — core/peripheral is only meaningful when comparing
#'         parsimonious vs intermediate)
#' }
#'
#' For each threshold in the result, this function:
#' \enumerate{
#'   \item Retrieves the intermediate solution already stored in
#'         \code{result$details}.
#'   \item Re-runs \code{QCA::minimize()} on the same truth table with
#'         \code{dir.exp = NULL} to obtain the parsimonious solution.
#'   \item Compares the two solutions: conditions appearing in both are
#'         \strong{core}; conditions appearing only in the intermediate
#'         solution are \strong{peripheral}.
#' }
#'
#' @param result  A sweep result object with \code{$details} and
#'   \code{$settings} slots (e.g., from \code{otSweep(..., return_details = TRUE)}).
#' @param conditions  Character vector. Condition names (used for consistent
#'   row ordering in charts). If \code{NULL}, extracted automatically.
#'
#' @return The original \code{result} object with an additional
#'   \code{$fiss_core} slot: a named list keyed by threshold (character),
#'   each entry containing:
#'   \itemize{
#'     \item \code{parsim_expression} — parsimonious solution expression
#'     \item \code{interm_expression} — intermediate solution expression
#'     \item \code{classification}    — data frame with columns
#'       \code{term_idx}, \code{term_expr}, \code{condition},
#'       \code{status}, \code{type}
#'   }
#'
#' @references
#' Fiss, P. C. (2011). Building better causal theories: A fuzzy set approach
#' to typologies in organization research. \emph{Academy of Management Journal},
#' 54(2), 393-420.
#'
#' @seealso \code{\link{generate_fiss_chart}}
#'
#' @examples
#' \dontrun{
#' library(ThSQCA)
#' data(sample_data)
#'
#' # Step 1: Run intermediate sweep (dir.exp required)
#' res <- otSweep(
#'   dat        = sample_data,
#'   outcome    = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:8,
#'   thrX       = c(X1 = 7, X2 = 7, X3 = 7),
#'   include    = "?",
#'   dir.exp    = c(1, 1, 1),
#'   return_details = TRUE
#' )
#'
#' # Step 2: Augment with Fiss core/peripheral classification
#' res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))
#'
#' # Step 3: Generate Fiss-style chart
#' cat(generate_fiss_chart(res_fiss, symbol_set = "unicode"))
#' }
#'
#' @export
compute_fiss_core <- function(result, conditions = NULL) {

  # --- Guard: details must be present ---
  if (is.null(result$details) || length(result$details) == 0) {
    stop(
      "No details found in result. ",
      "Re-run the sweep with return_details = TRUE."
    )
  }

  # --- Guard: sweep must have used include = "?" ---
  stored_include <- result$params$include
  if (!is.null(stored_include) && stored_include != "?") {
    stop(
      "Fiss core/peripheral classification requires include = \"?\". ",
      "Re-run the sweep with include = \"?\" and dir.exp specified."
    )
  }

  # --- Guard: dir.exp must have been specified ---
  stored_direxp <- result$params$dir.exp
  if (is.null(stored_direxp)) {
    stop(
      "Fiss core/peripheral classification requires dir.exp to be specified ",
      "(intermediate solution). Re-run the sweep with dir.exp = c(1, 1, ...)."
    )
  }

  # --- Resolve conditions ---
  if (is.null(conditions)) {
    conditions <- result$params$conditions
  }
  if (is.null(conditions)) {
    stop("Could not determine condition names. Please supply conditions argument.")
  }

  # --- Process each threshold ---
  fiss_list <- list()

  for (thr_key in names(result$details)) {

    detail <- result$details[[thr_key]]
    tt     <- detail$truth_table
    interm_sol <- detail$solution

    # Skip if no truth table or no intermediate solution
    if (is.null(tt) || is.null(interm_sol)) {
      fiss_list[[thr_key]] <- list(
        parsim_expression = NA_character_,
        interm_expression = NA_character_,
        classification    = NULL
      )
      next
    }

    # Extract intermediate solution terms
    interm_terms <- extract_sol_terms(interm_sol)

    # Build intermediate expression string
    interm_expr <- if (length(interm_terms) > 0) {
      paste(interm_terms, collapse = " + ")
    } else {
      "No solution"
    }

    # Compute parsimonious solution on same truth table
    parsim_sol   <- run_parsimonious(tt, conditions)
    parsim_terms <- extract_sol_terms(parsim_sol)
    parsim_expr  <- if (length(parsim_terms) > 0) {
      paste(parsim_terms, collapse = " + ")
    } else {
      "No solution"
    }

    # Build parsimonious condition-status map
    parsim_map <- extract_cond_status_map(parsim_terms, conditions)

    # Classify each intermediate term
    if (length(interm_terms) == 0) {
      fiss_list[[thr_key]] <- list(
        parsim_expression = parsim_expr,
        interm_expression = interm_expr,
        classification    = NULL
      )
      next
    }

    classif_rows <- lapply(seq_along(interm_terms), function(j) {
      row_df <- classify_term_conditions(
        interm_term = interm_terms[j],
        parsim_map  = parsim_map,
        conditions  = conditions
      )
      row_df$term_idx  <- j
      row_df$term_expr <- interm_terms[j]
      row_df[, c("term_idx", "term_expr", "condition", "status", "type")]
    })

    classif_df <- do.call(rbind, classif_rows)
    rownames(classif_df) <- NULL

    fiss_list[[thr_key]] <- list(
      parsim_expression = parsim_expr,
      interm_expression = interm_expr,
      classification    = classif_df
    )
  }

  # Attach to result
  result$fiss_core <- fiss_list
  result
}


# ============================================================
# Chart generation
# ============================================================

#' Build a Fiss-style configuration matrix (4-symbol)
#'
#' @param interm_terms    Character vector of intermediate-solution terms
#' @param classification  Data frame from \code{compute_fiss_core()} for one
#'   threshold (the \code{$classification} element)
#' @param conditions      Character vector of condition names (row order)
#' @param symbols         Fiss symbol set (one element of \code{SYMBOL_SETS_FISS})
#' @param thr_label       Character. Label prefix for column headers (e.g. "thrY=7")
#'
#' @return Character matrix with conditions as rows, terms as columns
#'
#' @keywords internal
build_fiss_matrix <- function(interm_terms, classification,
                               conditions, symbols, thr_label) {
  n_terms <- length(interm_terms)
  n_conds <- length(conditions)

  col_names <- paste0(thr_label, " (M", seq_len(n_terms), ")")

  mat <- matrix(
    "",
    nrow = n_conds,
    ncol = n_terms,
    dimnames = list(conditions, col_names)
  )

  for (j in seq_len(n_terms)) {
    rows_j <- classification[classification$term_idx == j, , drop = FALSE]

    for (cond in conditions) {
      row_cond <- rows_j[rows_j$condition == cond, , drop = FALSE]

      if (nrow(row_cond) == 0) next

      status <- row_cond$status[1]
      type   <- row_cond$type[1]

      mat[cond, j] <- if (status == "dontcare" || type == "dontcare") {
        ""
      } else if (type == "core" && status == "present") {
        symbols$core_present
      } else if (type == "core" && status == "absent") {
        symbols$core_absent
      } else if (type == "peripheral" && status == "present") {
        symbols$periph_present
      } else if (type == "peripheral" && status == "absent") {
        symbols$periph_absent
      } else {
        ""
      }
    }
  }

  mat
}


#' Generate Fiss-Style Configuration Chart from Sweep Results
#'
#' Produces a Markdown-formatted configuration chart following Fiss (2011),
#' using four symbols to distinguish core conditions (present in both
#' parsimonious and intermediate solutions) from peripheral conditions
#' (present in intermediate solution only).
#'
#' Call \code{\link{compute_fiss_core}} first to augment the sweep result.
#'
#' @param result     Sweep result augmented by \code{\link{compute_fiss_core}}.
#' @param conditions Character vector. Condition names (row order).
#'   If \code{NULL}, extracted from stored settings.
#' @param symbol_set Character. One of \code{"unicode"} (default),
#'   \code{"ascii"}, or \code{"latex"}.
#' @param language   Character. \code{"en"} (default) or \code{"ja"}.
#'
#' @return Character string: Markdown-formatted Fiss configuration chart.
#'
#' @references
#' Fiss, P. C. (2011). Building better causal theories: A fuzzy set approach
#' to typologies in organization research. \emph{Academy of Management Journal},
#' 54(2), 393-420.
#'
#' @seealso \code{\link{compute_fiss_core}}
#'
#' @examples
#' \dontrun{
#' data(sample_data)
#' res <- otSweep(
#'   dat = sample_data, outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:8,
#'   thrX = c(X1 = 7, X2 = 7, X3 = 7),
#'   include = "?", dir.exp = c(1, 1, 1),
#'   return_details = TRUE
#' )
#' res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))
#' cat(generate_fiss_chart(res_fiss))
#' cat(generate_fiss_chart(res_fiss, symbol_set = "latex"))
#' cat(generate_fiss_chart(res_fiss, language = "ja"))
#' }
#'
#' @export
generate_fiss_chart <- function(result,
                                 conditions = NULL,
                                 symbol_set = c("unicode", "ascii", "latex"),
                                 language   = c("en", "ja")) {

  symbol_set <- match.arg(symbol_set)
  language   <- match.arg(language)
  symbols    <- SYMBOL_SETS_FISS[[symbol_set]]

  # --- Guard: fiss_core must be computed ---
  if (is.null(result$fiss_core)) {
    stop(
      "No fiss_core data found. ",
      "Run compute_fiss_core(result) first."
    )
  }

  # --- Resolve conditions ---
  if (is.null(conditions)) {
    conditions <- result$params$conditions
  }
  if (is.null(conditions)) {
    stop("Could not determine condition names. Please supply conditions argument.")
  }

  # --- Determine threshold label column ---
  sum_df  <- result$summary
  thr_col <- intersect(c("thrY", "threshold", "thrX"), names(sum_df))[1]

  # --- Build one matrix per threshold ---
  all_matrices <- list()

  for (thr_key in names(result$fiss_core)) {
    fc <- result$fiss_core[[thr_key]]

    if (is.null(fc$classification)) next

    interm_terms <- unique(fc$classification$term_expr)
    if (length(interm_terms) == 0) next

    thr_label <- paste0("thrY=", thr_key)

    mat <- build_fiss_matrix(
      interm_terms   = interm_terms,
      classification = fc$classification,
      conditions     = conditions,
      symbols        = symbols,
      thr_label      = thr_label
    )

    all_matrices[[thr_key]] <- mat
  }

  if (length(all_matrices) == 0) {
    return("*No solution found across all thresholds.*\n")
  }

  # --- Combine all matrices into one wide matrix ---
  combined <- do.call(cbind, all_matrices)

  # --- Render as Markdown table ---
  table_str <- config_matrix_to_md(combined, "Condition")

  # --- Legend ---
  legend <- if (language == "ja") symbols$note_ja else symbols$note_en

  paste0(table_str, "\n\n*", legend, "*\n")
}


#' Print Fiss core/peripheral summary for a single threshold
#'
#' Displays which conditions are core and which are peripheral
#' at a given threshold, in a human-readable format.
#'
#' @param result   Sweep result augmented by \code{\link{compute_fiss_core}}.
#' @param thr_key  Character or numeric. Threshold key (e.g., "7" or 7).
#' @param language Character. \code{"en"} or \code{"ja"}.
#'
#' @return Invisibly returns the classification data frame for \code{thr_key}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res_fiss <- compute_fiss_core(res)
#' print_fiss_summary(res_fiss, thr_key = "7")
#' }
print_fiss_summary <- function(result, thr_key, language = c("en", "ja")) {

  language <- match.arg(language)

  if (is.null(result$fiss_core)) {
    stop("No fiss_core data. Run compute_fiss_core() first.")
  }

  thr_key <- as.character(thr_key)

  if (!thr_key %in% names(result$fiss_core)) {
    available <- paste(names(result$fiss_core), collapse = ", ")
    stop("thr_key '", thr_key, "' not found. Available: ", available)
  }

  fc <- result$fiss_core[[thr_key]]

  if (language == "ja") {
    cat("=== Fiss \u30b3\u30a2/\u5468\u8fba\u5206\u985e (thrY =", thr_key, ") ===\n")
    cat("\u3010\u7c21\u6f54\u89e3\u3011", fc$parsim_expression, "\n")
    cat("\u3010\u4e2d\u9593\u89e3\u3011", fc$interm_expression, "\n\n")
  } else {
    cat("=== Fiss Core/Peripheral Classification (thrY =", thr_key, ") ===\n")
    cat("Parsimonious :", fc$parsim_expression, "\n")
    cat("Intermediate :", fc$interm_expression, "\n\n")
  }

  if (is.null(fc$classification)) {
    cat(if (language == "ja") "\u89e3\u306a\u3057\n" else "No solution\n")
    return(invisible(NULL))
  }

  classif <- fc$classification
  classif_active <- classif[classif$status != "dontcare", , drop = FALSE]

  if (nrow(classif_active) == 0) {
    cat(if (language == "ja") "\u6761\u4ef6\u306a\u3057\n" else "No conditions\n")
    return(invisible(classif))
  }

  # Summary by term
  terms <- unique(classif_active$term_expr)
  for (j in seq_along(terms)) {
    term <- terms[j]
    rows_j <- classif_active[classif_active$term_expr == term, , drop = FALSE]

    core_pres  <- rows_j$condition[rows_j$type == "core"       & rows_j$status == "present"]
    core_abs   <- rows_j$condition[rows_j$type == "core"       & rows_j$status == "absent"]
    periph_pres <- rows_j$condition[rows_j$type == "peripheral" & rows_j$status == "present"]
    periph_abs  <- rows_j$condition[rows_j$type == "peripheral" & rows_j$status == "absent"]

    cat(if (language == "ja") paste0("[\u9805 M", j, "] ") else paste0("[Term M", j, "] "),
        term, "\n")

    if (length(core_pres) > 0)
      cat(if (language == "ja") "  \u30b3\u30a2\u5b58\u5728  : " else "  Core present    : ",
          paste(core_pres, collapse = ", "), "\n")
    if (length(core_abs) > 0)
      cat(if (language == "ja") "  \u30b3\u30a2\u4e0d\u5728  : " else "  Core absent     : ",
          paste(core_abs, collapse = ", "), "\n")
    if (length(periph_pres) > 0)
      cat(if (language == "ja") "  \u5468\u8fba\u5b58\u5728  : " else "  Periph. present : ",
          paste(periph_pres, collapse = ", "), "\n")
    if (length(periph_abs) > 0)
      cat(if (language == "ja") "  \u5468\u8fba\u4e0d\u5728  : " else "  Periph. absent  : ",
          paste(periph_abs, collapse = ", "), "\n")
    cat("\n")
  }

  invisible(classif)
}
