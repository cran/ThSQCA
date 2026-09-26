###############################################
# Fiss Core/Peripheral Classification for ThSQCA
#
# Implements Fiss (2011) core/peripheral distinction, configuration by
# configuration as in Fiss's solution tables:
#   Core condition      : belongs to a parsimonious term contained in the
#                         intermediate term
#   Peripheral condition: appears in the intermediate term only
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

#' Condition statuses of a single term
#'
#' @param term Character. A single product term (e.g. \code{"A*~B"}).
#' @param conditions Character vector of all condition names.
#' @return Named character vector: \code{"present"}, \code{"absent"} or
#'   \code{"dontcare"} for each condition.
#' @keywords internal
term_status <- function(term, conditions) {
  vapply(conditions, function(cond) get_condition_status(term, cond),
         character(1), USE.NAMES = TRUE)
}


#' Core conditions of one intermediate term relative to one parsimonious model
#'
#' Implements the configuration-level reading of Fiss (2011): the core
#' conditions of an intermediate configuration are the conditions of the
#' parsimonious term(s) contained in it. A parsimonious term is contained
#' (nested) in the intermediate term when every condition it specifies has the
#' same status (present or absent) in the intermediate term. When several
#' parsimonious terms are contained in it, their conditions are pooled, as in
#' solution 2 of Fiss (2011, Table 4).
#'
#' @param interm_term Character. A single intermediate-solution term.
#' @param parsim_model Character vector. The terms of one parsimonious
#'   minimal solution.
#' @param conditions Character vector of all condition names.
#' @return List with \code{core} (character, condition names classified as
#'   core) and \code{nested} (logical, whether any parsimonious term is
#'   contained in the intermediate term).
#' @keywords internal
nested_core_conditions <- function(interm_term, parsim_model, conditions) {
  t_st <- term_status(interm_term, conditions)
  core <- character(0)
  nested <- FALSE
  for (p in parsim_model) {
    p_st <- term_status(p, conditions)
    specified <- p_st != "dontcare"
    if (!any(specified)) next
    if (all(p_st[specified] == t_st[specified])) {
      nested <- TRUE
      core <- union(core, conditions[specified])
    }
  }
  list(core = core, nested = nested)
}


#' Classify each condition of an intermediate term as core or peripheral
#'
#' The term is compared with each source parsimonious solution by
#' \code{nested_core_conditions()}. When there are several source solutions
#' (tied parsimonious solutions from which QCA derived the same intermediate
#' solution), a condition is core only if it is core relative to every one of
#' them. A condition whose polarity differs across the source solutions can
#' therefore never be core (the safeguard introduced in version 2.0.5).
#'
#' @param interm_term Character. A single intermediate-solution term.
#' @param source_models List of character vectors, one per source
#'   parsimonious solution.
#' @param conditions Character vector of all condition names.
#' @return Data frame with columns \code{condition}, \code{status}
#'   (\code{"present"}, \code{"absent"}, \code{"dontcare"}) and \code{type}
#'   (\code{"core"}, \code{"peripheral"}, \code{"dontcare"}). Attribute
#'   \code{"not_nested"}: integer indices of the source solutions that have no
#'   term contained in \code{interm_term}.
#' @keywords internal
classify_term_fiss <- function(interm_term, source_models, conditions) {
  t_st <- term_status(interm_term, conditions)
  per_source <- lapply(source_models, nested_core_conditions,
                       interm_term = interm_term, conditions = conditions)
  core <- if (length(per_source) > 0) {
    Reduce(intersect, lapply(per_source, `[[`, "core"))
  } else {
    character(0)
  }
  type <- ifelse(t_st == "dontcare", "dontcare",
                 ifelse(conditions %in% core, "core", "peripheral"))
  out <- data.frame(condition = conditions, status = unname(t_st),
                    type = unname(type), stringsAsFactors = FALSE)
  attr(out, "not_nested") <- which(!vapply(per_source, `[[`, logical(1), "nested"))
  out
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
  sol <- quiet_try(
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
  models <- extract_sol_terms_by_model(sol)
  if (length(models) == 0) return(character(0))
  unique(unlist(models, use.names = FALSE))
}


#' Extract solution terms separately for each minimal solution
#'
#' Same sources as \code{extract_sol_terms()}, but the terms of each minimal
#' solution (M1, M2, ...) are kept apart instead of being pooled. This is what
#' the core/peripheral classification needs: pooling first makes a condition
#' look as if it occurred with both polarities in "the" parsimonious solution,
#' when in fact one minimal solution had it present and another had it absent.
#'
#' @param sol QCA solution object.
#'
#' @return List of character vectors, one per minimal solution. Empty list when
#'   no solution is available.
#'
#' @keywords internal
extract_sol_terms_by_model <- function(sol) {
  if (is.null(sol)) return(list())

  # Deduplicate models by term-set content. When the solution spans several
  # prime implicant charts, each chart's $solution may report the same model,
  # so a plain chart-by-chart loop counts one model several times. The
  # core/peripheral classification itself is agreement-based and so is
  # insensitive to duplicates, but the reported parsim_n_solutions (and the
  # warning derived from it) would otherwise be inflated.
  dedup_models <- function(models) {
    if (length(models) <= 1L) return(models)
    keys <- vapply(models, function(x) paste(sort(as.character(x)), collapse = " | "),
                   character(1))
    models[!duplicated(keys)]
  }

  # Intermediate solution stored in i.sol
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    models <- list()
    for (entry in sol$i.sol) {
      if (!is.null(entry$solution) && length(entry$solution) > 0) {
        for (s in entry$solution) {
          parsed <- parse_solution_terms(paste(s, collapse = " + "))
          if (length(parsed) > 0) models[[length(models) + 1L]] <- unique(parsed)
        }
      }
    }
    if (length(models) > 0) return(dedup_models(models))
  }

  # Parsimonious / complex solution in $solution
  if (!is.null(sol$solution) && length(sol$solution) > 0) {
    models <- list()
    for (s in sol$solution) {
      parsed <- parse_solution_terms(paste(s, collapse = " + "))
      if (length(parsed) > 0) models[[length(models) + 1L]] <- unique(parsed)
    }
    return(dedup_models(models))
  }

  list()
}


#' Order-independent key for a model (set of terms)
#'
#' @param terms Character vector of solution terms.
#' @return Single character string.
#' @keywords internal
model_key <- function(terms) {
  paste(sort(unique(as.character(terms))), collapse = " | ")
}


#' Trace the reported intermediate model back to its parsimonious source(s)
#'
#' With \code{include = "?"} and \code{dir.exp}, \code{QCA::minimize()} stores
#' one entry in \code{sol$i.sol} for each pair of a complex solution
#' (\code{C1}, \code{C2}, ...) and a parsimonious minimal solution (\code{P1},
#' \code{P2}, ...), named \code{C1P1}, \code{C1P2}, and so on. Each entry holds
#' that parsimonious model in \code{$p.sol} and the intermediate model(s)
#' obtained from it in \code{$solution}.
#'
#' This helper picks the intermediate model that ThSQCA reports as M1 (the
#' first model of \code{i.sol$C1P1}, else of the first entry, exactly as in
#' \code{qca_extract()}), and returns every \code{i.sol} entry whose
#' \code{$solution} contains that same model, together with those entries'
#' parsimonious models. The core/peripheral comparison is then made against
#' these source models only, not against tied parsimonious solutions from
#' which the reported intermediate model was not obtained.
#'
#' \code{print(sol)} groups entries whose \code{$solution} lists are
#' identical (\code{"From C1P1, C1P2:"}). This helper instead keeps every
#' entry that lists M1 among its models, including entries that also list
#' other tied intermediate models. The set of sources is therefore the same as
#' or larger than the printed grouping, which can only make the classification
#' more conservative.
#'
#' @param sol A \code{QCA::minimize()} result with \code{dir.exp} specified.
#'
#' @return \code{NULL} when \code{sol$i.sol} is missing, empty, or lacks
#'   \code{$p.sol} for a source entry (callers then fall back to recomputing
#'   the parsimonious solution). Otherwise a list with
#'   \code{interm_terms} (character), \code{sources} (character, entry names),
#'   \code{parsim_models} (list of character vectors, deduplicated), and
#'   \code{parsim_labels} (character, the \code{P} part of the first entry
#'   name for each model, e.g. \code{"P2"}).
#'
#' @keywords internal
trace_intermediate_sources <- function(sol) {
  isol <- sol$i.sol
  if (is.null(isol) || length(isol) == 0) return(NULL)
  if (is.null(names(isol))) names(isol) <- paste0("E", seq_along(isol))

  # The model ThSQCA reports as M1 (mirrors qca_extract()).
  first_list <- isol[["C1P1"]]$solution
  if (is.null(first_list) || length(first_list) == 0) first_list <- isol[[1]]$solution
  if (is.null(first_list) || length(first_list) == 0) return(NULL)
  m1 <- parse_solution_terms(paste(first_list[[1]], collapse = " + "))
  if (length(m1) == 0) return(NULL)
  m1_key <- model_key(m1)

  sources <- character(0)
  parsim_models <- list()
  parsim_labels <- character(0)
  for (nm in names(isol)) {
    entry_models <- isol[[nm]]$solution
    if (is.null(entry_models) || length(entry_models) == 0) next
    keys <- vapply(entry_models, function(s) {
      model_key(parse_solution_terms(paste(s, collapse = " + ")))
    }, character(1))
    if (!m1_key %in% keys) next

    p <- isol[[nm]]$p.sol
    if (is.null(p) || length(p) == 0) return(NULL)
    p_terms <- parse_solution_terms(paste(p, collapse = " + "))
    if (length(p_terms) == 0) return(NULL)
    sources <- c(sources, nm)
    parsim_models[[length(parsim_models) + 1L]] <- unique(p_terms)
    parsim_labels <- c(parsim_labels,
                       if (grepl("^C[0-9]+P[0-9]+$", nm)) sub("^C[0-9]+", "", nm) else nm)
  }
  if (length(sources) == 0) return(NULL)

  keep <- !duplicated(vapply(parsim_models, model_key, character(1)))
  list(
    interm_terms  = unique(m1),
    sources       = sources,
    parsim_models = parsim_models[keep],
    parsim_labels = parsim_labels[keep]
  )
}


#' Format one or more parsimonious models as a display expression
#'
#' @param models List of character vectors.
#' @param labels Optional character labels, one per model (e.g. \code{"P1"}).
#'   Defaults to \code{"P1"}, \code{"P2"}, ...
#' @return \code{"A + B"} for one model, \code{"P1: A + B; P2: C + B"} for
#'   several, \code{"No solution"} for none.
#' @keywords internal
format_models_expr <- function(models, labels = NULL) {
  if (length(models) == 0) return("No solution")
  exprs <- vapply(models, paste, character(1), collapse = " + ")
  if (length(exprs) == 1L) return(exprs)
  if (is.null(labels) || length(labels) != length(exprs)) {
    labels <- paste0("P", seq_along(exprs))
  }
  paste0(labels, ": ", exprs, collapse = "; ")
}


# ============================================================
# Public API
# ============================================================

#' Compute Fiss Core/Peripheral Classification for Sweep Results
#'
#' Takes a threshold-sweep result produced by \code{\link{otSweep}} or
#' \code{\link{ctSweepS}} and augments it with the Fiss (2011) core/peripheral
#' classification of the intermediate solution. Results of
#' \code{\link{ctSweepM}} and \code{\link{dtSweep}} are not supported yet and
#' give an error.
#'
#' The classification requires that:
#' \itemize{
#'   \item The sweep was run with \code{include = "?"} (to allow parsimonious
#'         computation)
#'   \item \code{return_details = TRUE} was used (truth tables must be stored)
#'   \item \code{dir.exp} was specified (i.e., the sweep produced intermediate
#'         solutions; core/peripheral is only meaningful when comparing
#'         parsimonious and intermediate solutions)
#' }
#'
#' For each threshold in the result, this function:
#' \enumerate{
#'   \item Retrieves the intermediate solution already stored in
#'         \code{result$details}, and takes the model reported in the sweep
#'         summary (M1).
#'   \item Identifies the parsimonious solution(s) from which
#'         \code{QCA::minimize()} derived that model. With \code{dir.exp},
#'         QCA stores one entry in \code{sol$i.sol} for each pair of a
#'         complex solution and a parsimonious minimal solution
#'         (\code{C1P1}, \code{C1P2}, ...; see \code{print(sol)}), holding the
#'         parsimonious model in \code{$p.sol} and the intermediate model(s)
#'         obtained from it in \code{$solution}. Every entry that lists M1 is
#'         treated as a source.
#'   \item Classifies each term (configuration) of M1: the conditions of the
#'         source parsimonious term(s) contained in that term are
#'         \strong{core}; its other conditions are \strong{peripheral}.
#' }
#'
#' @section Relation to Fiss (2011):
#' Fiss (2011) defines core conditions as those that are part of both the
#' parsimonious and the intermediate solution, and peripheral conditions as
#' those that are eliminated in the parsimonious solution and therefore
#' appear only in the intermediate solution. His solution tables apply this
#' configuration by configuration: solutions are grouped by their core
#' conditions, and the same condition can be core in one configuration and
#' peripheral in another. This function follows that practice. A parsimonious term is contained in an intermediate
#' term when every condition it specifies has the same status (present or
#' absent) in the intermediate term; the core conditions of the intermediate
#' term are the conditions of all parsimonious terms contained in it (one of
#' the configurations in Fiss's high-performance table contains two
#' parsimonious terms and has the core conditions of both).
#'
#' Example: with the parsimonious solution \code{~A*E + A*B}, the intermediate
#' term \code{~A*~B*C*E} contains \code{~A*E}, so \code{~A} and \code{E} are
#' core and \code{~B} and \code{C} are peripheral, even though \code{B}
#' occurs in the other parsimonious term. Versions up to 2.0.7 compared each
#' condition with the whole parsimonious solution and reported \code{~B} as
#' core here, which does not match how Fiss's tables are built.
#'
#' Fiss (2011) does not discuss two situations, which this function handles
#' conservatively and reports with a warning:
#' \itemize{
#'   \item \strong{Tied parsimonious solutions.} If QCA derived M1 from a
#'     single parsimonious solution, that solution alone decides. If QCA
#'     derived the same M1 from several tied parsimonious solutions (the
#'     directional expectations do not single one out), a condition is core
#'     only if it is core relative to every one of them. For example, if
#'     M1 = \code{SUP + TRU*PRC} is derived both from \code{TRU + SUP} and
#'     from \code{PRC + SUP}, then \code{TRU} is core relative to the first
#'     and \code{PRC} relative to the second, so both are reported as
#'     peripheral. Fiss grounds coreness in the strength of the evidence; a
#'     condition whose status depends on which tied solution is chosen is not
#'     treated as strongly supported. To report core/peripheral status
#'     relative to one particular parsimonious solution, state that choice
#'     explicitly.
#'   \item \strong{No contained parsimonious term.} An intermediate term can
#'     be covered by the parsimonious solution without containing any single
#'     parsimonious term (for example \code{B*C*D*E} with the parsimonious
#'     solution \code{~A*E + A*B}). Its conditions are then all classified as
#'     peripheral.
#' }
#'
#' Only M1 is classified. When the intermediate solution itself has several
#' minimal models (see \code{n_solutions} in the sweep summary and
#' \code{interm_n_solutions} below), the other models are not included in the
#' classification or the chart. If the derivation cannot be read from the
#' stored solution (no \code{$i.sol} or \code{$p.sol}), M1 is compared with
#' every tied parsimonious solution.
#'
#' @param result  A result of \code{otSweep()} or \code{ctSweepS()} run with
#'   \code{include = "?"}, \code{dir.exp} and \code{return_details = TRUE}.
#' @param conditions  Character vector. Condition names (used for consistent
#'   row ordering in charts). If \code{NULL}, extracted automatically.
#'
#' @return The original \code{result} object with an additional
#'   \code{$fiss_core} slot: a named list keyed by threshold (character),
#'   each entry containing:
#'   \itemize{
#'     \item \code{parsim_expression} — the parsimonious solution(s) M1 was
#'       compared with; several are shown with QCA's labels, e.g.
#'       \code{"P1: ...; P2: ..."}
#'     \item \code{interm_expression} — the intermediate solution classified
#'       (M1)
#'     \item \code{parsim_n_solutions} — number of tied parsimonious
#'       solutions on the truth table
#'     \item \code{interm_n_solutions} — number of intermediate minimal
#'       solutions
#'     \item \code{parsim_sources} — names of the \code{i.sol} entries M1 was
#'       derived from (e.g. \code{"C1P1"}), or \code{NA} when unavailable
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

  # --- Guard: supported sweep types ---
  # ctSweepM() and dtSweep() store their details without threshold names, so
  # the per-threshold loop below would silently produce nothing.
  if (inherits(result, c("ctSweepM_result", "dtSweep_result")) ||
      (!is.null(result$details) && length(result$details) > 0 &&
       is.null(names(result$details)))) {
    stop(
      "compute_fiss_core() currently supports results of otSweep() and ",
      "ctSweepS() only. Results of ctSweepM() and dtSweep() are not supported ",
      "yet; run otSweep() or ctSweepS() at the threshold setting of interest ",
      "instead.",
      call. = FALSE
    )
  }

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
        parsim_expression   = NA_character_,
        interm_expression   = NA_character_,
        parsim_n_solutions  = NA_integer_,
        interm_n_solutions  = NA_integer_,
        parsim_sources      = NA_character_,
        classification      = NULL
      )
      next
    }

    # All tied parsimonious solutions on the same truth table (used for the
    # reported count, and as the comparison set when the derivation below
    # cannot be traced).
    parsim_sol        <- run_parsimonious(tt, conditions)
    all_parsim_models <- extract_sol_terms_by_model(parsim_sol)
    n_parsim_sol      <- length(all_parsim_models)

    # Intermediate models (distinct), for the reported count.
    interm_models <- extract_sol_terms_by_model(interm_sol)
    n_interm_sol  <- length(interm_models)

    # Trace the reported intermediate model (M1) to the parsimonious
    # solution(s) QCA derived it from (sol$i.sol[[...]]$p.sol).
    traced <- trace_intermediate_sources(interm_sol)

    if (!is.null(traced)) {
      interm_terms   <- traced$interm_terms
      source_models  <- traced$parsim_models
      source_labels  <- traced$parsim_labels
      parsim_sources <- traced$sources
    } else {
      # Fallback (derivation not available): previous behavior, i.e. compare
      # against every tied parsimonious solution.
      interm_terms   <- if (n_interm_sol > 0) interm_models[[1]] else character(0)
      source_models  <- all_parsim_models
      source_labels  <- NULL
      parsim_sources <- NA_character_
    }

    interm_expr <- if (length(interm_terms) > 0) {
      paste(interm_terms, collapse = " + ")
    } else {
      "No solution"
    }
    parsim_expr <- format_models_expr(source_models, source_labels)

    n_source <- length(source_models)
    if (n_source > 1L) {
      warning(
        "For ", thr_key, ", the intermediate solution is derived from ",
        n_source, " tied parsimonious solutions",
        if (!all(is.na(parsim_sources))) paste0(" (", paste(parsim_sources, collapse = ", "), ")"),
        ". A condition is classified as core only where all of them agree, ",
        "so some conditions may be reported as peripheral.",
        call. = FALSE
      )
    }

    # Classify each intermediate term
    if (length(interm_terms) == 0) {
      fiss_list[[thr_key]] <- list(
        parsim_expression   = parsim_expr,
        interm_expression   = interm_expr,
        parsim_n_solutions  = n_parsim_sol,
        interm_n_solutions  = n_interm_sol,
        parsim_sources      = parsim_sources,
        classification      = NULL
      )
      next
    }

    # Configuration-level classification (Fiss, 2011): each term is compared
    # with the parsimonious term(s) contained in it, for every source solution.
    term_classes <- lapply(interm_terms, classify_term_fiss,
                           source_models = source_models,
                           conditions    = conditions)
    not_nested_terms <- interm_terms[vapply(term_classes, function(x) {
      length(attr(x, "not_nested")) > 0
    }, logical(1))]
    classif_rows <- lapply(seq_along(interm_terms), function(j) {
      row_df <- term_classes[[j]]
      row_df$term_idx  <- j
      row_df$term_expr <- interm_terms[j]
      row_df[, c("term_idx", "term_expr", "condition", "status", "type")]
    })

    if (length(not_nested_terms) > 0) {
      warning(
        "For ", thr_key, ", the intermediate term(s) ",
        paste(not_nested_terms, collapse = ", "),
        " contain no term of ",
        if (n_source > 1L) "at least one source parsimonious solution" else "the parsimonious solution",
        ", so their conditions are classified as peripheral.",
        call. = FALSE
      )
    }

    classif_df <- do.call(rbind, classif_rows)
    rownames(classif_df) <- NULL

    fiss_list[[thr_key]] <- list(
      parsim_expression   = parsim_expr,
      interm_expression   = interm_expr,
      parsim_n_solutions  = n_parsim_sol,
      interm_n_solutions  = n_interm_sol,
      parsim_sources      = parsim_sources,
      classification      = classif_df
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

  # Terms are labeled T1, T2, ... rather than M1, M2, ...: M is reserved for
  # whole alternative minimal solutions elsewhere in the package.
  col_names <- paste0(thr_label, " (T", seq_len(n_terms), ")")

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


#' Label for one threshold setting in Fiss charts and summaries
#'
#' @param result Sweep result augmented by \code{compute_fiss_core()}.
#' @param thr_key Character. Threshold key (a name of \code{result$fiss_core}).
#' @param sep Character placed between the variable name and the value.
#' @return \code{"X3=6"} for a \code{ctSweepS()} result sweeping \code{X3},
#'   otherwise \code{"thrY=6"}.
#' @keywords internal
fiss_threshold_label <- function(result, thr_key, sep = "=") {
  sweep_var <- result$params$sweep_var
  if (inherits(result, "ctSweepS_result") && length(sweep_var) == 1L) {
    paste0(sweep_var, sep, thr_key)
  } else {
    paste0("thrY", sep, thr_key)
  }
}


#' Generate Fiss-Style Configuration Chart from Sweep Results
#'
#' Produces a Markdown-formatted configuration chart following Fiss (2011),
#' using four symbols to distinguish core conditions (conditions of the
#' parsimonious term contained in each configuration) from peripheral
#' conditions (present in the intermediate solution only).
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

    thr_label <- fiss_threshold_label(result, thr_key)

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
print_fiss_summary <- function(result, thr_key = NULL, language = c("en", "ja")) {

  language <- match.arg(language)

  if (is.null(result$fiss_core)) {
    stop("No fiss_core data. Run compute_fiss_core() first.")
  }

  # thr_key omitted: summarize every threshold level rather than erroring out
  # with R's generic "argument is missing" message. Callers previously had to
  # inspect names(result$fiss_core) themselves to discover valid keys.
  if (is.null(thr_key)) {
    keys <- names(result$fiss_core)
    if (length(keys) == 0) {
      stop("No threshold levels found in result$fiss_core.", call. = FALSE)
    }
    out <- list()
    for (k in keys) {
      out[[k]] <- print_fiss_summary(result, thr_key = k, language = language)
    }
    return(invisible(out))
  }

  thr_key <- as.character(thr_key)

  if (!thr_key %in% names(result$fiss_core)) {
    available <- paste(names(result$fiss_core), collapse = ", ")
    stop("thr_key '", thr_key, "' not found. Available: ", available)
  }

  fc <- result$fiss_core[[thr_key]]

  if (language == "ja") {
    cat("=== Fiss \u30b3\u30a2/\u5468\u8fba\u5206\u985e (", fiss_threshold_label(result, thr_key, " = "), ") ===\n", sep = "")
    cat("\u3010\u7c21\u6f54\u89e3\u3011", fc$parsim_expression, "\n")
    cat("\u3010\u4e2d\u9593\u89e3\u3011", fc$interm_expression, "\n\n")
  } else {
    cat("=== Fiss Core/Peripheral Classification (", fiss_threshold_label(result, thr_key, " = "), ") ===\n", sep = "")
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

    # Term numbering uses "T" (not "M"): in print(sol) and elsewhere in this
    # package, M1/M2 denote whole SOLUTIONS (alternative minimal solutions),
    # so reusing M for product terms within one solution is ambiguous.
    cat(if (language == "ja") paste0("[\u9805 T", j, "] ") else paste0("[Term T", j, "] "),
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
