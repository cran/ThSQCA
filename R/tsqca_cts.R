###############################################
# CTS–QCA (single X) and MCTS–QCA (multiple X)
# v0.3.0: QCA-compatible argument names + negated outcome support
###############################################

#' CTS–QCA: Single-condition threshold sweep
#'
#' Performs a threshold sweep for one focal condition X. For each threshold
#' in \code{sweep_range}, the outcome Y and all X variables are binarized
#' using user-specified thresholds, and a crisp-set QCA is executed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param outcome Character. Outcome variable name. Supports negation with
#'   tilde prefix (e.g., \code{"~Y"}) following QCA package conventions.
#' @param conditions Character vector. Names of condition variables.
#' @param sweep_var Character. Name of the condition to be swept.
#'   Must be one of \code{conditions}.
#' @param sweep_range Numeric vector. Candidate thresholds for \code{sweep_var}.
#' @param thrY Numeric. Threshold for Y (fixed).
#' @param thrX_default Numeric. Default threshold for non-swept X variables.
#'   Variables listed in \code{pre_calibrated} do not require this threshold.
#' @param pre_calibrated Character vector or \code{NULL}. Names of condition
#'   variables that have been pre-calibrated (e.g., via \code{QCA::calibrate()})
#'   and should be passed through to \code{QCA::truthTable()} without
#'   binarization. These variables must contain values in the \code{[0, 1]}
#'   range. Variables not listed here will be binarized using \code{thrX_default}
#'   as usual. Default is \code{NULL} (all variables binarized).
#'   It is recommended to sweep variables on their original (raw) scale rather
#'   than as pre-calibrated fuzzy values, because raw-scale thresholds are
#'   easier to interpret substantively.
#' @param dir.exp Directional expectations for \code{minimize}.
#'   If \code{NULL} (default), no directional expectations are applied.
#'   To compute the \strong{intermediate solution}, specify a numeric vector
#'   (1, 0, or -1 for each condition). Example: \code{dir.exp = c(1, 1, 1)}
#'   for three conditions all expected to contribute positively.
#' @param include Inclusion rule for \code{minimize}. 
#'   \code{""} (default, QCA compatible) computes the \strong{complex solution}
#'   without logical remainders.
#'   Use \code{"?"} to include logical remainders for \strong{parsimonious}
#'   (with \code{dir.exp = NULL}) or \strong{intermediate} solutions
#'   (with \code{dir.exp} specified).
#' @param incl.cut Consistency cutoff for \code{\link[QCA]{truthTable}}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param extract_mode Character. How to handle multiple solutions:
#'   \code{"first"} (default), \code{"all"}, or \code{"essential"}.
#'   See \code{\link{qca_extract}} for details.
#' @param return_details Logical. If \code{TRUE} (default), returns both
#'   summary and detailed objects for use with \code{generate_report()}.
#' @param Yvar Deprecated. Use \code{outcome} instead.
#' @param Xvars Deprecated. Use \code{conditions} instead.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{threshold} — swept threshold for \code{sweep_var}
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
#'   \item (additional columns depending on \code{extract_mode})
#' }
#'
#' If \code{return_details = TRUE}, a list with:
#' \itemize{
#'   \item \code{summary} — the data frame above
#'   \item \code{details} — per-threshold list of
#'     \code{threshold}, \code{thrX_vec}, \code{truth_table}, \code{solution}
#' }
#'
#' Note that \code{return_details} changes the \emph{type} of the returned
#' object, not just its contents: with \code{TRUE} the summary table is at
#' \code{result$summary}, whereas with \code{FALSE} the summary table
#' \emph{is} the returned object and \code{result$summary} is \code{NULL}.
#' Code intended to work under both settings should branch on
#' \code{inherits(result, "data.frame")} (or simply always pass
#' \code{return_details = TRUE}) rather than assuming \code{result$summary}
#' exists.
#'
#' @importFrom QCA truthTable minimize
#' @export
#' @examples
#' # Load sample data
#' data(sample_data)
#' 
#' # === Three Types of QCA Solutions ===
#' 
#' # 1. Complex Solution (default, QCA compatible)
#' result_comp <- ctSweepS(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_var = "X3",
#'   sweep_range = 7,
#'   thrY = 7,
#'   thrX_default = 7
#'   # include = "" (default), dir.exp = NULL (default)
#' )
#' head(result_comp$summary)
#' 
#' # 2. Parsimonious Solution (include = "?")
#' result_pars <- ctSweepS(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_var = "X3",
#'   sweep_range = 7,
#'   thrY = 7,
#'   thrX_default = 7,
#'   include = "?"  # Include logical remainders
#' )
#' head(result_pars$summary)
#' 
#' # 3. Intermediate Solution (include = "?" + dir.exp)
#' result_int <- ctSweepS(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_var = "X3",
#'   sweep_range = 7,
#'   thrY = 7,
#'   thrX_default = 7,
#'   include = "?",
#'   dir.exp = c(1, 1, 1)  # All conditions expected positive
#' )
#' head(result_int$summary)
#' 
#' # === Threshold Sweep Example ===
#' 
#' # Run single condition threshold sweep on X3 (complex solutions by default)
#' result <- ctSweepS(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_var = "X3",
#'   sweep_range = 6:8,
#'   thrY = 7,
#'   thrX_default = 7
#' )
#' head(result$summary)
#' 
#' # Run with negated outcome (~Y)
#' result_neg <- ctSweepS(
#'   dat = sample_data,
#'   outcome = "~Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_var = "X3",
#'   sweep_range = 6:8,
#'   thrY = 7,
#'   thrX_default = 7
#' )
#' head(result_neg$summary)
ctSweepS <- function(dat, 
                     outcome = NULL, conditions = NULL,
                     sweep_var, sweep_range,
                     thrY, thrX_default = 7,
                     pre_calibrated = NULL,
                     dir.exp = NULL, include = "",
                     incl.cut = 0.8, n.cut = 1, pri.cut = 0,
                     extract_mode = c("first", "all", "essential"),
                     return_details = TRUE,
                     Yvar = NULL, Xvars = NULL) {
  
  # === Backward compatibility for deprecated arguments ===
  if (!is.null(Yvar) && is.null(outcome)) {
    outcome <- Yvar
    warning("Argument 'Yvar' is deprecated. Use 'outcome' instead.",
            call. = FALSE)
  }
  if (!is.null(Xvars) && is.null(conditions)) {
    conditions <- Xvars
    warning("Argument 'Xvars' is deprecated. Use 'conditions' instead.",
            call. = FALSE)
  }
  
  # === Validate required arguments ===
  if (is.null(outcome)) {
    stop("Argument 'outcome' is required.")
  }
  if (is.null(conditions)) {
    stop("Argument 'conditions' is required.")
  }
  
  # === Handle negated outcome ===
  negate_outcome <- grepl("^~", outcome)
  outcome_clean <- sub("^~", "", outcome)
  
  # Validate outcome variable exists
  if (!outcome_clean %in% names(dat)) {
    stop("Variable '", outcome_clean, "' not found in data.")
  }
  
  # Validate condition variables exist
  missing_conds <- setdiff(conditions, names(dat))
  if (length(missing_conds) > 0) {
    stop("Condition variable(s) not found in data: ", 
         paste(missing_conds, collapse = ", "))
  }
  
  extract_mode <- match.arg(extract_mode)
  
  if (!sweep_var %in% conditions) {
    stop("sweep_var must be one of conditions.")
  }
  
  # Initialize output data frame based on extract_mode
  df_out <- data.frame(
    threshold   = numeric(0),
    expression  = character(0),
    inclS       = numeric(0),
    covS        = numeric(0),
    n_solutions = integer(0),
    stringsAsFactors = FALSE
  )
  
  # Add columns based on extract_mode
  if (extract_mode == "essential") {
    df_out$selective_terms <- character(0)
    df_out$unique_terms     <- character(0)
  }
  
  details_list <- list()
  
  # Track thresholds with multiple solutions (for warning in "first" mode)
  multi_sol_thresholds <- c()
  
  # Handle dir.exp: scalar -> expand to vector; NULL is passed through
  # NULL -> parsimonious solution; c(1,1,...) -> intermediate solution
  local_dir.exp <- dir.exp
  if (!is.null(local_dir.exp) && length(local_dir.exp) == 1) {
    local_dir.exp <- rep(local_dir.exp[1], length(conditions))
    names(local_dir.exp) <- conditions
  } else if (!is.null(local_dir.exp) && is.null(names(local_dir.exp))) {
    names(local_dir.exp) <- conditions
  }

  # Validate pre_calibrated (once, before loop)
  validate_pre_calibrated(pre_calibrated, conditions, dat)

  for (thr in sweep_range) {
    
    # vector of thresholds for all X
    thrX_vec <- rep(thrX_default, length(conditions))
    names(thrX_vec) <- conditions
    thrX_vec[sweep_var] <- thr
    
    # Prepare data: binarize or pass through based on pre_calibrated
    dat_bin <- prepare_dat_bin(
      dat, outcome_clean, conditions,
      thrY, thrX_vec,
      pre_calibrated = pre_calibrated
    )
    
    # Determine outcome string for truthTable (with ~ if negated)
    outcome_tt <- if (negate_outcome) "~Y" else "Y"
    
    # Truth table (wrapped in try to handle errors)
    tt <- try(
      QCA::truthTable(
        dat_bin,
        outcome    = outcome_tt,
        conditions = conditions,
        show.cases = FALSE,
        incl.cut1  = incl.cut,
        n.cut      = n.cut,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(tt, "try-error")) {
      new_row <- data.frame(
        threshold   = thr,
        expression  = "No solution",
        inclS       = NA_real_,
        covS        = NA_real_,
        n_solutions = 0L,
        stringsAsFactors = FALSE
      )
      
      if (extract_mode == "essential") {
        new_row$selective_terms <- NA_character_
        new_row$unique_terms     <- NA_character_
      }
      
      df_out <- rbind(df_out, new_row)
      
      if (return_details) {
        details_list[[as.character(thr)]] <- list(
          threshold   = thr,
          thrX_vec    = thrX_vec,
          truth_table = NULL,
          solution    = NULL,
          dat_bin     = NULL
        )
      }
      next
    }
    
    # Minimize (wrapped in try to handle errors)
    tt <- sanitize_truthtable(tt)  # QCA 3.25 guard: char incl/PRI + '-' remainders
    sol <- try(
      QCA::minimize(
        tt,
        include    = include,
        dir.exp    = local_dir.exp,
        details    = TRUE,
        show.cases = FALSE,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(sol, "try-error")) {
      new_row <- data.frame(
        threshold   = thr,
        expression  = "No solution",
        inclS       = NA_real_,
        covS        = NA_real_,
        n_solutions = 0L,
        stringsAsFactors = FALSE
      )
      
      if (extract_mode == "essential") {
        new_row$selective_terms <- NA_character_
        new_row$unique_terms     <- NA_character_
      }
      
      df_out <- rbind(df_out, new_row)
      
      if (return_details) {
        details_list[[as.character(thr)]] <- list(
          threshold   = thr,
          thrX_vec    = thrX_vec,
          truth_table = tt,
          solution    = NULL,
          dat_bin     = dat_bin
        )
      }
      next
    }
    
    sol_info <- qca_extract(sol, extract_mode = extract_mode)
    
    # Track multiple solutions
    if (sol_info$n_solutions > 1) {
      multi_sol_thresholds <- c(multi_sol_thresholds, thr)
    }
    
    # Build result row based on extract_mode
    new_row <- data.frame(
      threshold   = thr,
      expression  = sol_info$expression,
      inclS       = sol_info$inclS,
      covS        = sol_info$covS,
      n_solutions = sol_info$n_solutions,
      stringsAsFactors = FALSE
    )
    
    if (extract_mode == "essential") {
      new_row$selective_terms <- sol_info$selective_terms
      new_row$unique_terms     <- sol_info$unique_terms
    }
    
    df_out <- rbind(df_out, new_row)
    
    if (return_details) {
      details_list[[as.character(thr)]] <- list(
        threshold   = thr,
        thrX_vec    = thrX_vec,
        truth_table = tt,
        solution    = sol,
        dat_bin     = dat_bin
      )
    }
  }
  
  df_out <- df_out[order(df_out$threshold), ]
  
  # Issue warning for multiple solutions
  if (length(multi_sol_thresholds) > 0) {
    warning(
      "Multiple intermediate solutions exist for threshold = ",
      paste(multi_sol_thresholds, collapse = ", "),
      " (n_solutions > 1). ",
      "Only the first solution (M1) and its fit metrics are shown. ",
      "Use generate_report() for full analysis.",
      call. = FALSE
    )
  }
  
  if (return_details) {
    result <- list(
      summary = df_out, 
      details = details_list,
      params = list(
        outcome = outcome,
        conditions = conditions,
        negate_outcome = negate_outcome,
        pre_calibrated = pre_calibrated,
        sweep_var = sweep_var,
        sweep_range = sweep_range,
        thrY = thrY,
        thrX_default = thrX_default,
        incl.cut = incl.cut,
        n.cut = n.cut,
        pri.cut = pri.cut,
        include = include,
        dir.exp = dir.exp  # Store original value for reproducibility
      )
    )
    class(result) <- c("ctSweepS_result", "tsqca_result", "list")
    return(result)
  }
  
  df_out
}


###############################################
# MCTS–QCA (multiple X)
###############################################

#' MCTS–QCA: Multi-condition threshold sweep
#'
#' Performs a grid search over thresholds of multiple X variables.
#' For each combination of thresholds in \code{sweep_list}, the outcome Y
#' and all X variables are binarized, and a crisp-set QCA is executed.
#'
#' @param dat Data frame containing the outcome and condition variables.
#' @param outcome Character. Outcome variable name. Supports negation with
#'   tilde prefix (e.g., \code{"~Y"}) following QCA package conventions.
#' @param conditions Character vector. Names of condition variables.
#' @param sweep_list Named list. Each element is a numeric vector of
#'   candidate thresholds for the corresponding X. Names must match
#'   \code{conditions}. Variables listed in \code{pre_calibrated} do not
#'   need a \code{sweep_list} entry. Every condition must receive a threshold
#'   from one of \code{sweep_list}, \code{pre_calibrated}, or
#'   \code{thrX_default}; a condition covered by none of these raises an
#'   error. To hold a condition at a fixed value while sweeping others, give
#'   it a length-one entry (e.g. \code{list(SPV = 6:8, PRD = 7)}).
#' @param thrY Numeric. Threshold for Y (fixed).
#' @param thrX_default Numeric or \code{NULL}. Threshold used to binarize any
#'   condition that appears in neither \code{sweep_list} nor
#'   \code{pre_calibrated}, mirroring the argument of the same name in
#'   \code{\link{ctSweepS}}. Default is \code{NULL}, in which case such a
#'   condition is an error rather than being silently dropped.
#' @param pre_calibrated Character vector or \code{NULL}. Names of condition
#'   variables that have been pre-calibrated (e.g., via \code{QCA::calibrate()})
#'   and should be passed through to \code{QCA::truthTable()} without
#'   binarization. These variables must contain values in the \code{[0, 1]}
#'   range. Variables not listed here will be binarized using \code{sweep_list}
#'   thresholds as usual. Default is \code{NULL} (all variables binarized).
#'   It is recommended to sweep variables on their original (raw) scale rather
#'   than as pre-calibrated fuzzy values, because raw-scale thresholds are
#'   easier to interpret substantively.
#' @param dir.exp Directional expectations for \code{minimize}.
#'   If \code{NULL} (default), no directional expectations are applied.
#'   To compute the \strong{intermediate solution}, specify a numeric vector
#'   (1, 0, or -1 for each condition). Example: \code{dir.exp = c(1, 1, 1)}
#'   for three conditions all expected to contribute positively.
#' @param include Inclusion rule for \code{minimize}. 
#'   \code{""} (default, QCA compatible) computes the \strong{complex solution}
#'   without logical remainders.
#'   Use \code{"?"} to include logical remainders for \strong{parsimonious}
#'   (with \code{dir.exp = NULL}) or \strong{intermediate} solutions
#'   (with \code{dir.exp} specified).
#' @param incl.cut Consistency cutoff for \code{truthTable}.
#' @param n.cut Frequency cutoff for \code{truthTable}.
#' @param pri.cut PRI cutoff for \code{minimize}.
#' @param extract_mode Character. How to handle multiple solutions:
#'   \code{"first"} (default), \code{"all"}, or \code{"essential"}.
#'   See \code{\link{qca_extract}} for details.
#' @param return_details Logical. If \code{TRUE} (default), returns both
#'   summary and detailed objects for use with \code{generate_report()}.
#' @param Yvar Deprecated. Use \code{outcome} instead.
#' @param Xvars Deprecated. Use \code{conditions} instead.
#'
#' @return
#' If \code{return_details = FALSE}, a data frame with columns:
#' \itemize{
#'   \item \code{combo_id} — index of the threshold combination
#'   \item \code{threshold} — character string summarizing thresholds,
#'     e.g. \code{"X1=6, X2=7, X3=7"}
#'   \item \code{expression} — minimized solution expression
#'   \item \code{inclS} — solution consistency
#'   \item \code{covS} — solution coverage
#'   \item (additional columns depending on \code{extract_mode})
#' }
#'
#' If \code{return_details = TRUE}, a list with:
#' \itemize{
#'   \item \code{summary} — the data frame above
#'   \item \code{details} — per-combination list of
#'     \code{combo_id}, \code{thrX_vec}, \code{truth_table}, \code{solution}
#' }
#'
#' Note that \code{return_details} changes the \emph{type} of the returned
#' object, not just its contents: with \code{TRUE} the summary table is at
#' \code{result$summary}, whereas with \code{FALSE} the summary table
#' \emph{is} the returned object and \code{result$summary} is \code{NULL}.
#' Code intended to work under both settings should branch on
#' \code{inherits(result, "data.frame")} (or simply always pass
#' \code{return_details = TRUE}) rather than assuming \code{result$summary}
#' exists.
#'
#' @importFrom QCA truthTable minimize
#' @export
#' @examples
#' # Load sample data
#' data(sample_data)
#' 
#' # === Three Types of QCA Solutions ===
#' 
#' # Quick demonstration with 2 conditions
#' sweep_list <- list(X1 = 7, X2 = 7)
#' 
#' # 1. Complex Solution (default, QCA compatible)
#' result_comp <- ctSweepM(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2"),
#'   sweep_list = sweep_list,
#'   thrY = 7
#'   # include = "" (default), dir.exp = NULL (default)
#' )
#' head(result_comp$summary)
#' 
#' # 2. Parsimonious Solution (include = "?")
#' result_pars <- ctSweepM(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2"),
#'   sweep_list = sweep_list,
#'   thrY = 7,
#'   include = "?"  # Include logical remainders
#' )
#' head(result_pars$summary)
#' 
#' # 3. Intermediate Solution (include = "?" + dir.exp)
#' result_int <- ctSweepM(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2"),
#'   sweep_list = sweep_list,
#'   thrY = 7,
#'   include = "?",
#'   dir.exp = c(1, 1)  # Positive expectations
#' )
#' head(result_int$summary)
#' 
#' # === Threshold Sweep Example ===
#' 
#' # Using 2 conditions and 2 threshold levels
#' sweep_list <- list(
#'   X1 = 6:7,
#'   X2 = 6:7
#' )
#' 
#' # Run multiple condition threshold sweep (complex solutions by default)
#' result_quick <- ctSweepM(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2"),
#'   sweep_list = sweep_list,
#'   thrY = 7
#' )
#' head(result_quick$summary)
#' 
#' # Run with negated outcome (~Y)
#' result_neg <- ctSweepM(
#'   dat = sample_data,
#'   outcome = "~Y",
#'   conditions = c("X1", "X2"),
#'   sweep_list = sweep_list,
#'   thrY = 7
#' )
#' head(result_neg$summary)
#' 
#' \donttest{
#' # Full multi-condition analysis (27 combinations)
#' sweep_list_full <- list(
#'   X1 = 6:8,
#'   X2 = 6:8,
#'   X3 = 6:8
#' )
#' 
#' result_full <- ctSweepM(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_list = sweep_list_full,
#'   thrY = 7
#' )
#' head(result_full$summary)
#' }
ctSweepM <- function(dat, 
                     outcome = NULL, conditions = NULL,
                     sweep_list, thrY,
                     thrX_default = NULL,
                     pre_calibrated = NULL,
                     dir.exp = NULL, include = "",
                     incl.cut = 0.8, n.cut = 1, pri.cut = 0,
                     extract_mode = c("first", "all", "essential"),
                     return_details = TRUE,
                     Yvar = NULL, Xvars = NULL) {
  
  # === Backward compatibility for deprecated arguments ===
  if (!is.null(Yvar) && is.null(outcome)) {
    outcome <- Yvar
    warning("Argument 'Yvar' is deprecated. Use 'outcome' instead.",
            call. = FALSE)
  }
  if (!is.null(Xvars) && is.null(conditions)) {
    conditions <- Xvars
    warning("Argument 'Xvars' is deprecated. Use 'conditions' instead.",
            call. = FALSE)
  }
  
  # === Validate required arguments ===
  if (is.null(outcome)) {
    stop("Argument 'outcome' is required.")
  }
  if (is.null(conditions)) {
    stop("Argument 'conditions' is required.")
  }
  
  # === Handle negated outcome ===
  negate_outcome <- grepl("^~", outcome)
  outcome_clean <- sub("^~", "", outcome)
  
  # Validate outcome variable exists
  if (!outcome_clean %in% names(dat)) {
    stop("Variable '", outcome_clean, "' not found in data.")
  }
  
  # Validate condition variables exist
  missing_conds <- setdiff(conditions, names(dat))
  if (length(missing_conds) > 0) {
    stop("Condition variable(s) not found in data: ", 
         paste(missing_conds, collapse = ", "))
  }
  
  extract_mode <- match.arg(extract_mode)
  
  # === Validate coverage of all conditions (Issue: silent "No solution") ===
  # Every condition must get a binarization threshold from somewhere: either
  # sweep_list (swept or held at a fixed value), pre_calibrated (already fuzzy),
  # or thrX_default. A condition covered by none of these previously had no
  # column created in dat_bin at all, which made truthTable() fail and produced
  # "No solution" in every cell with no error or warning.
  uncovered <- setdiff(conditions, c(names(sweep_list), pre_calibrated))
  if (length(uncovered) > 0 && is.null(thrX_default)) {
    stop("Condition(s) not covered by 'sweep_list', 'pre_calibrated', or ",
         "'thrX_default': ", paste(uncovered, collapse = ", "), ".\n",
         "Every condition needs a threshold. Either add them to 'sweep_list' ",
         "as fixed values (e.g. sweep_list = list(",
         paste(names(sweep_list)[1], " = ", 
               if (length(sweep_list) > 0) paste0(min(sweep_list[[1]]), ":", max(sweep_list[[1]])) else "6:8",
               sep = ""),
         ", ", paste(paste0(uncovered, " = 7"), collapse = ", "), ")), ",
         "or supply 'thrX_default' to binarize them all at one value.",
         call. = FALSE)
  }
  
  combo_mat <- expand.grid(
    sweep_list,
    KEEP.OUT.ATTRS  = FALSE,
    stringsAsFactors = FALSE
  )
  
  n_combos <- nrow(combo_mat)
  
  # Initialize output data frame based on extract_mode
  df_out <- data.frame(
    combo_id    = seq_len(n_combos),
    threshold   = NA_character_,
    expression  = NA_character_,
    inclS       = NA_real_,
    covS        = NA_real_,
    n_solutions = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  # Add columns based on extract_mode
  if (extract_mode == "essential") {
    df_out$selective_terms <- NA_character_
    df_out$unique_terms     <- NA_character_
  }
  
  details_list <- list()
  
  # Track combinations with multiple solutions (for warning in "first" mode)
  multi_sol_combos <- c()
  
  # Handle dir.exp: scalar -> expand to vector; NULL is passed through
  # NULL -> parsimonious solution; c(1,1,...) -> intermediate solution
  local_dir.exp <- dir.exp
  if (!is.null(local_dir.exp) && length(local_dir.exp) == 1) {
    local_dir.exp <- rep(local_dir.exp[1], length(conditions))
    names(local_dir.exp) <- conditions
  } else if (!is.null(local_dir.exp) && is.null(names(local_dir.exp))) {
    names(local_dir.exp) <- conditions
  }

  # Validate pre_calibrated (once, before loop)
  validate_pre_calibrated(pre_calibrated, conditions, dat)

  # Warn if pre_calibrated variable is also a sweep target in sweep_list
  if (!is.null(pre_calibrated)) {
    swept_vars <- names(sweep_list)[
      sapply(sweep_list, function(x) length(x) > 1)
    ]
    conflict <- intersect(pre_calibrated, swept_vars)
    if (length(conflict) > 0) {
      warning("Variable(s) ", paste(conflict, collapse = ", "),
              " are both pre_calibrated and in sweep_list. ",
              "Pre-calibrated values will be used; sweep thresholds ignored. ",
              "It is recommended to sweep variables on their original (raw) scale, ",
              "not as pre-calibrated fuzzy values, because threshold values on a ",
              "raw scale (e.g., Likert 1-10) are easier to interpret substantively. ",
              "See the package vignette section 'Choosing Sweep Variables' for details.",
              call. = FALSE)
    }
  }

  for (i in seq_len(n_combos)) {
    
    thrX_vec <- as.numeric(combo_mat[i, ])
    names(thrX_vec) <- names(combo_mat)
    
    thrX_label <- paste(names(thrX_vec), thrX_vec,
                        sep = "=", collapse = ", ")
    
    # Prepare data: binarize or pass through based on pre_calibrated
    # Note: prepare_dat_bin uses conditions (all vars); thrX_vec covers sweep_list vars only.
    # For conditions not in sweep_list, they are handled via pre_calibrated or thrX_vec lookup.
    dat_bin <- data.frame(Y = qca_bin(dat[[outcome_clean]], thrY))
    for (x in conditions) {
      if (!is.null(pre_calibrated) && x %in% pre_calibrated) {
        dat_bin[[x]] <- dat[[x]]
      } else if (x %in% names(thrX_vec)) {
        dat_bin[[x]] <- qca_bin(dat[[x]], thrX_vec[x])
      } else {
        # Not swept and not pre-calibrated: fall back to thrX_default.
        # Guaranteed non-NULL here by the validation above.
        dat_bin[[x]] <- qca_bin(dat[[x]], thrX_default)
      }
    }
    
    # Determine outcome string for truthTable (with ~ if negated)
    outcome_tt <- if (negate_outcome) "~Y" else "Y"
    
    tt <- try(
      QCA::truthTable(
        dat_bin,
        outcome    = outcome_tt,
        conditions = conditions,
        show.cases = FALSE,
        incl.cut1  = incl.cut,
        n.cut      = n.cut,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(tt, "try-error")) {
      df_out$threshold[i]   <- thrX_label
      df_out$expression[i]  <- "No solution"
      df_out$inclS[i]       <- NA_real_
      df_out$covS[i]        <- NA_real_
      df_out$n_solutions[i] <- 0L
      
      if (extract_mode == "essential") {
        df_out$selective_terms[i] <- NA_character_
        df_out$unique_terms[i]     <- NA_character_
      }
      
      if (return_details) {
        details_list[[i]] <- list(
          combo_id    = i,
          thrX_vec    = thrX_vec,
          truth_table = NULL,
          solution    = NULL,
          dat_bin     = NULL
        )
      }
      next
    }
    
    tt <- sanitize_truthtable(tt)  # QCA 3.25 guard: char incl/PRI + '-' remainders
    sol <- try(
      QCA::minimize(
        tt,
        include    = include,
        dir.exp    = local_dir.exp,
        details    = TRUE,
        show.cases = FALSE,
        pri.cut    = pri.cut
      ),
      silent = TRUE
    )
    
    if (inherits(sol, "try-error")) {
      df_out$threshold[i]   <- thrX_label
      df_out$expression[i]  <- "No solution"
      df_out$inclS[i]       <- NA_real_
      df_out$covS[i]        <- NA_real_
      df_out$n_solutions[i] <- 0L
      
      if (extract_mode == "essential") {
        df_out$selective_terms[i] <- NA_character_
        df_out$unique_terms[i]     <- NA_character_
      }
      
      if (return_details) {
        details_list[[i]] <- list(
          combo_id    = i,
          thrX_vec    = thrX_vec,
          truth_table = tt,
          solution    = NULL,
          dat_bin     = dat_bin
        )
      }
      next
    }
    
    sol_info <- qca_extract(sol, extract_mode = extract_mode)
    
    # Track multiple solutions
    if (sol_info$n_solutions > 1) {
      multi_sol_combos <- c(multi_sol_combos, i)
    }
    
    df_out$threshold[i]   <- thrX_label
    df_out$expression[i]  <- sol_info$expression
    df_out$inclS[i]       <- sol_info$inclS
    df_out$covS[i]        <- sol_info$covS
    df_out$n_solutions[i] <- sol_info$n_solutions
    
    if (extract_mode == "essential") {
      df_out$selective_terms[i] <- sol_info$selective_terms
      df_out$unique_terms[i]     <- sol_info$unique_terms
    }
    
    if (return_details) {
      details_list[[i]] <- list(
        combo_id    = i,
        thrX_vec    = thrX_vec,
        truth_table = tt,
        solution    = sol,
        dat_bin     = dat_bin
      )
    }
  }
  
  # Issue warning for multiple solutions
  if (length(multi_sol_combos) > 0) {
    n_multi <- length(multi_sol_combos)
    warning(
      "Multiple intermediate solutions exist for ", n_multi, " combination(s) ",
      "(n_solutions > 1). ",
      "Only the first solution (M1) and its fit metrics are shown. ",
      "Use generate_report() for full analysis.",
      call. = FALSE
    )
  }
  
  if (return_details) {
    result <- list(
      summary = df_out, 
      details = details_list,
      params = list(
        outcome = outcome,
        conditions = conditions,
        negate_outcome = negate_outcome,
        pre_calibrated = pre_calibrated,
        sweep_list = sweep_list,
        thrY = thrY,
        incl.cut = incl.cut,
        n.cut = n.cut,
        pri.cut = pri.cut,
        include = include,
        dir.exp = dir.exp  # Store original value for reproducibility
      )
    )
    class(result) <- c("ctSweepM_result", "tsqca_result", "list")
    return(result)
  }
  
  df_out
}
