# Helpers for the second audit stage (test-audit2-*.R).
#
# Stage 1 (helper-audit.R) checks that reports agree with the stored QCA
# objects. Stage 2 removes that dependency: fit measures are recomputed BY HAND
# from the raw data, binarization is re-implemented independently, and the
# results are compared under transformations that must not change them
# (row order, column order, affine rescaling of data and thresholds).

# ---------------------------------------------------------------------------
# Hand computation of the displayed solution's fit
# ---------------------------------------------------------------------------

# "X1*~X2 + X3" -> list(c("X1", "~X2"), "X3"); parentheses are dropped.
.hand_terms <- function(expr) {
  expr <- gsub("[()]", "", expr)
  terms <- trimws(strsplit(expr, "\\s\\+\\s")[[1]])
  terms <- terms[nzchar(terms)]
  lapply(terms, function(t) trimws(strsplit(t, "\\*")[[1]]))
}

# Membership of every case in a solution expression (min within a term, max
# across terms, 1 - x for "~"). `bin` is a data frame holding the conditions.
.hand_membership <- function(expr, bin) {
  terms <- .hand_terms(expr)
  per_term <- vapply(terms, function(lits) {
    v <- vapply(lits, function(l) {
      if (startsWith(l, "~")) 1 - bin[[substring(l, 2)]] else bin[[l]]
    }, numeric(nrow(bin)))
    if (is.null(dim(v))) v <- matrix(v, ncol = length(lits))
    apply(v, 1, min)
  }, numeric(nrow(bin)))
  if (is.null(dim(per_term))) per_term <- matrix(per_term, ncol = length(terms))
  apply(per_term, 1, max)
}

# c(inclS, covS) of `expr` for outcome column `y` (a numeric vector).
.hand_fit <- function(expr, bin, y) {
  x <- .hand_membership(expr, bin)
  num <- sum(pmin(x, y))
  c(inclS = if (sum(x) > 0) num / sum(x) else NA_real_,
    covS  = if (sum(y) > 0) num / sum(y) else NA_real_)
}

# ---------------------------------------------------------------------------
# Independent binarization
# ---------------------------------------------------------------------------

# Re-implements the data preparation from the documented rule (x >= threshold
# is 1); pre_calibrated conditions pass through unchanged.
.indep_binarize <- function(dat, outcome_clean, conditions, thrY, thrX_vec,
                            pre_calibrated = NULL, thrX_default = NULL) {
  out <- data.frame(Y = as.numeric(dat[[outcome_clean]] >= thrY))
  for (x in conditions) {
    out[[x]] <- if (!is.null(pre_calibrated) && x %in% pre_calibrated) {
      dat[[x]]
    } else if (x %in% names(thrX_vec)) {
      as.numeric(dat[[x]] >= thrX_vec[[x]])
    } else if (!is.null(thrX_default)) {
      as.numeric(dat[[x]] >= thrX_default)
    } else {
      stop("no threshold known for condition '", x, "'")
    }
  }
  out
}

# Thresholds of one detail entry; NULL when they cannot be determined.
.detail_thresholds <- function(det, res) {
  thrY <- det$thrY
  if (is.null(thrY)) thrY <- res$params$thrY
  thrX <- det$thrX_vec
  if (is.null(thrY) || is.null(thrX)) return(NULL)
  list(thrY = thrY, thrX = thrX, thrX_default = res$params$thrX_default)
}

# ---------------------------------------------------------------------------
# Checks (return character vectors of problems)
# ---------------------------------------------------------------------------

# det$dat_bin must equal the independent binarization.
.audit_check_binarization <- function(res, dat, conditions) {
  p <- character(0)
  outcome_clean <- sub("^~", "", res$params$outcome)
  pre <- res$params$pre_calibrated
  for (i in seq_along(res$details)) {
    det <- res$details[[i]]
    if (is.null(det$dat_bin)) next
    thr <- .detail_thresholds(det, res)
    if (is.null(thr)) next
    mine <- tryCatch(.indep_binarize(dat, outcome_clean, conditions, thr$thrY, thr$thrX, pre, thr$thrX_default),
                     error = function(e) e)
    if (inherits(mine, "error")) {
      p <- c(p, sprintf("detail %d: cannot re-binarize (%s)", i, conditionMessage(mine)))
      next
    }
    theirs <- det$dat_bin
    same <- all(c("Y", conditions) %in% names(theirs)) &&
      isTRUE(all.equal(as.numeric(theirs$Y), mine$Y)) &&
      all(vapply(conditions, function(x) isTRUE(all.equal(as.numeric(theirs[[x]]), mine[[x]])),
                 logical(1)))
    if (!same) p <- c(p, sprintf("detail %d: dat_bin differs from independent binarization", i))
  }
  p
}

# Summary fit (extract_mode = "first") vs hand computation from raw data.
.audit_check_hand_fit <- function(res, dat, conditions, tol = 1.1e-3) {
  p <- character(0)
  sm <- res$summary
  if (is.null(sm) || nrow(sm) != length(res$details)) return(p)
  outcome_clean <- sub("^~", "", res$params$outcome)
  negate <- isTRUE(res$params$negate_outcome)
  pre <- res$params$pre_calibrated
  for (i in seq_len(nrow(sm))) {
    expr <- sm$expression[i]
    if (is.na(expr) || expr == "No solution") next
    thr <- .detail_thresholds(res$details[[i]], res)
    if (is.null(thr)) next
    bin <- tryCatch(.indep_binarize(dat, outcome_clean, conditions, thr$thrY, thr$thrX, pre, thr$thrX_default),
                    error = function(e) e)
    if (inherits(bin, "error")) {
      p <- c(p, sprintf("row %d: cannot re-binarize (%s)", i, conditionMessage(bin)))
      next
    }
    y <- if (negate) 1 - bin$Y else bin$Y
    hand <- tryCatch(.hand_fit(expr, bin, y), error = function(e) c(inclS = NA, covS = NA))
    got <- c(inclS = sm$inclS[i], covS = sm$covS[i])
    if (anyNA(hand)) {
      p <- c(p, sprintf("row %d: could not hand-compute fit of '%s'", i, expr))
    } else if (anyNA(got) || !isTRUE(all.equal(unname(got), unname(hand), tolerance = tol))) {
      p <- c(p, sprintf("row %d '%s': reported inclS/covS %s, hand-computed %s",
                        i, expr, paste(round(got, 3), collapse = "/"),
                        paste(round(hand, 3), collapse = "/")))
    }
  }
  p
}

# ---------------------------------------------------------------------------
# Signatures for metamorphic comparisons
# ---------------------------------------------------------------------------

# Order-free canonical form of an expression: literals sorted within terms,
# terms sorted.
.canon_expr <- function(expr) {
  if (is.na(expr) || expr == "No solution") return(expr)
  # extract_mode = "all" labels models ("M1: ..."), even for a unique solution.
  expr <- sub("^M[0-9]+:\\s*", "", expr)
  terms <- vapply(.hand_terms(expr), function(l) paste(sort(l), collapse = "*"), character(1))
  paste(sort(terms), collapse = " + ")
}

# What must be invariant: number of solutions always; expression and fit only
# where the solution is unique (with ties, which model is "M1" may depend on
# the order of rows or conditions).
.result_signature <- function(res) {
  sm <- res$summary
  unique_row <- sm$n_solutions <= 1L
  data.frame(
    n_solutions = sm$n_solutions,
    expression = ifelse(unique_row, unname(vapply(sm$expression, .canon_expr, character(1))), NA_character_),
    inclS = ifelse(unique_row, round(sm$inclS, 6), NA_real_),
    covS  = ifelse(unique_row, round(sm$covS, 6), NA_real_),
    stringsAsFactors = FALSE, row.names = NULL)
}

# Parse the cells of one condition row in a Markdown chart table.
.chart_row_cells <- function(chart_lines, cond) {
  ln <- chart_lines[startsWith(chart_lines, paste0("| ", cond, " |"))]
  if (length(ln) != 1L) return(NULL)
  # Every table line ends with "|", so strsplit() yields exactly the cells
  # (a blank last cell is kept as whitespace, the empty tail is dropped).
  parts <- strsplit(ln, "\\|")[[1]]
  trimws(parts[-(1:2)])
}

# ---------------------------------------------------------------------------
# Chart check: the configuration chart of a unique solution must place a
# symbol exactly where the solution's terms have a (negated) condition.
# ---------------------------------------------------------------------------
.audit_check_chart <- function(res, conditions) {
  p <- character(0)
  for (i in seq_along(res$details)) {
    sol <- res$details[[i]]$solution
    if (is.null(sol)) next
    lst <- collect_unique_i_sol(sol)
    if (length(lst) != 1L) next
    terms <- as.character(lst[[1]])
    chart <- tryCatch(
      generate_config_chart(sol, symbol_set = "ascii", include_metrics = FALSE,
                            condition_order = conditions),
      error = function(e) e)
    if (inherits(chart, "error")) {
      p <- c(p, sprintf("detail %d: generate_config_chart failed (%s)", i, conditionMessage(chart)))
      next
    }
    lines <- strsplit(chart, "\n")[[1]]
    for (cond in conditions) {
      cells <- .chart_row_cells(lines, cond)
      if (is.null(cells)) {
        p <- c(p, sprintf("detail %d: no chart row for '%s'", i, cond))
        next
      }
      expected <- vapply(terms, function(t) {
        lits <- trimws(strsplit(t, "\\*")[[1]])
        if (cond %in% lits) "O" else if (paste0("~", cond) %in% lits) "X" else ""
      }, character(1))
      if (!identical(unname(cells), unname(expected))) {
        p <- c(p, sprintf("detail %d, condition '%s': chart cells [%s], expected [%s] for '%s'",
                          i, cond, paste(cells, collapse = "|"),
                          paste(expected, collapse = "|"), paste(terms, collapse = " + ")))
      }
    }
  }
  p
}
