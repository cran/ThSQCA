###############################################
# Core utilities for ThSQCA
###############################################

#' Binary calibration helper for ThSQCA
#'
#' Converts a numeric vector into a crisp set (0/1) based on a threshold.
#'
#' @param x Numeric vector.
#' @param thr Numeric scalar. Cases with \code{x >= thr} are coded as 1,
#'   others as 0.
#'
#' @return Integer vector of 0/1 with the same length as \code{x}.
#' @keywords internal
qca_bin <- function(x, thr) {
  ifelse(x >= thr, 1L, 0L)
}

#' Prepare analysis data frame for QCA::truthTable()
#'
#' Constructs the data frame to be passed to \code{QCA::truthTable()}.
#' For pre-calibrated variables, the original values are passed through
#' without binarization. For all other variables, \code{qca_bin()} is applied.
#'
#' @param dat Original data frame.
#' @param outcome_clean Character. Outcome variable name (without \code{~}).
#' @param conditions Character vector. Condition variable names.
#' @param thrY Numeric. Threshold for outcome binarization.
#' @param thrX_vec Named numeric vector. Thresholds for conditions.
#' @param pre_calibrated Character vector or NULL. Names of pre-calibrated
#'   variables to pass through without binarization.
#'
#' @return Data frame with column \code{Y} (binarized outcome) and condition
#'   columns (binarized or passed through).
#' @keywords internal
prepare_dat_bin <- function(dat, outcome_clean, conditions,
                            thrY, thrX_vec,
                            pre_calibrated = NULL) {

  # Outcome: always binarize (threshold sweeping is the core purpose of TS-QCA)
  dat_bin <- data.frame(Y = qca_bin(dat[[outcome_clean]], thrY))

  # Conditions: binarize or pass through
  for (x in conditions) {
    if (!is.null(pre_calibrated) && x %in% pre_calibrated) {
      # Pass through: use original values (fuzzy membership or binary 0/1)
      dat_bin[[x]] <- dat[[x]]
    } else {
      # Binarize: apply threshold
      dat_bin[[x]] <- qca_bin(dat[[x]], thrX_vec[x])
    }
  }

  dat_bin
}

#' Validate the pre_calibrated parameter
#'
#' Checks that all names in \code{pre_calibrated} exist in \code{conditions}
#' and that the corresponding values in \code{dat} are within the \code{[0, 1]}
#' range required for fuzzy membership scores.
#'
#' @param pre_calibrated Character vector or NULL.
#' @param conditions Character vector. Valid condition variable names.
#' @param dat Data frame containing the variables.
#'
#' @return Invisible NULL. Raises errors or warnings as needed.
#' @keywords internal
validate_pre_calibrated <- function(pre_calibrated, conditions, dat) {
  if (is.null(pre_calibrated)) return(invisible(NULL))

  # All names must exist in conditions
  invalid <- setdiff(pre_calibrated, conditions)
  if (length(invalid) > 0) {
    stop("pre_calibrated variable(s) not found in conditions: ",
         paste(invalid, collapse = ", "),
         call. = FALSE)
  }

  # Values must be in [0, 1]
  for (v in pre_calibrated) {
    vals <- dat[[v]]
    if (any(is.na(vals))) {
      warning("pre_calibrated variable '", v, "' contains NA values.",
              call. = FALSE)
    }
    rng <- range(vals, na.rm = TRUE)
    if (rng[1] < 0 || rng[2] > 1) {
      stop("pre_calibrated variable '", v,
           "' has values outside [0, 1] range: [",
           round(rng[1], 4), ", ", round(rng[2], 4), "]. ",
           "Apply QCA::calibrate() before passing to the sweep function.",
           call. = FALSE)
    }
  }

  invisible(NULL)
}

#' Collect and deduplicate the solution models attached to a QCA::minimize()
#' result
#'
#' When \code{dir.exp} is specified and the minimization produces more than
#' one prime implicant chart (QCA's own \code{C1}, \code{C2}, ... indexing,
#' visible as \code{"From C1P1, C2P1:"} in \code{print()}), \code{sol$i.sol}
#' contains one list entry per chart/path combination (e.g. \code{C1P1},
#' \code{C2P1}). Each entry's own \code{$solution} field is computed
#' independently by \code{QCA::minimize()} (via its internal
#' \code{getSolution()} call) and is NOT guaranteed to hold only that chart's
#' own model: depending on the data, it may already enumerate the full,
#' cross-chart set of tied minimal models. Concatenating every chart's
#' \code{$solution} list (the previous approach used here and in
#' \code{generate_report()}) therefore risks double- or N-fold counting the
#' same model, inflating the reported number of solutions.
#'
#' This helper performs the same enumeration, then deduplicates by comparing
#' each model's term set (order-independent), so structurally identical
#' models are counted once regardless of which chart(s) produced them. This
#' mirrors the manual "remove duplicate Boolean expressions" step that is
#' standard practice when reporting multiple minimal solutions by hand.
#'
#' @param sol A solution object returned by \code{QCA::minimize()}.
#' @return A list of unique solution term-vectors, or \code{NULL} if none
#'   were found.
#' @note When dir.exp is specified, the true Intermediate solution is stored in
#'   sol$i.sol, not sol$solution (which contains the Parsimonious solution).
#' @keywords internal
collect_unique_i_sol <- function(sol) {
  if (is.null(sol)) return(NULL)

  raw <- list()

  # Priority 1: i.sol structure (contains true Intermediate solution when dir.exp specified)
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    for (model_name in names(sol$i.sol)) {
      model_sols <- sol$i.sol[[model_name]]$solution
      if (!is.null(model_sols) && length(model_sols) > 0) {
        for (s in model_sols) {
          raw <- c(raw, list(s))
        }
      }
    }
  }

  # Fallback: sol$solution (for Parsimonious or when dir.exp not specified)
  if (length(raw) == 0) {
    sol_list <- try(sol$solution, silent = TRUE)
    if (!inherits(sol_list, "try-error") && !is.null(sol_list) && length(sol_list) > 0) {
      raw <- sol_list
    }
  }

  if (length(raw) == 0) return(NULL)

  # Deduplicate by term-set content: same terms regardless of order/source
  # chart are the same model.
  keys <- vapply(raw, function(x) paste(sort(as.character(x)), collapse = " | "), character(1))
  raw[!duplicated(keys)]
}

#' Get the number of intermediate solutions
#'
#' @param sol A solution object returned by \code{QCA::minimize()}.
#' @return Integer. Number of intermediate solutions, or 0 if none.
#' @note When dir.exp is specified, the true Intermediate solution is stored in
#'   sol$i.sol, not sol$solution (which contains the Parsimonious solution).
#'   See \code{\link{collect_unique_i_sol}} for why deduplication is required
#'   when multiple prime implicant charts are present.
#' @keywords internal
get_n_solutions <- function(sol) {
  if (is.null(sol)) return(0L)

  uniq <- collect_unique_i_sol(sol)
  if (is.null(uniq)) return(0L)
  length(uniq)
}

#' Extract solution information from a QCA minimization result
#'
#' Internal helper to obtain the solution expression, consistency
#' (\code{inclS}) and coverage (\code{covS}) from an object returned by
#' \code{QCA::minimize()}.
#'
#' @param sol A solution object returned by \code{QCA::minimize()}.
#' @param extract_mode Character. How to handle multiple intermediate solutions:
#'   \itemize{
#'     \item \code{"first"} - return only the first solution (M1). Default.
#'     \item \code{"all"} - return all solutions concatenated.
#'     \item \code{"essential"} - return essential prime implicants (terms 
#'       common to all solutions), plus selective prime implicants and 
#'       solution count.
#'   }
#'
#' @return A list with elements depending on \code{extract_mode}.
#'
#'   For \code{"first"}: \code{expression}, \code{inclS}, \code{covS}.
#'
#'   For \code{"all"}: adds \code{n_solutions}.
#'
#'   For \code{"essential"}: adds \code{selective_terms}, \code{unique_terms},
#'   \code{n_solutions}.
#'
#'   If extraction fails, returns \code{"No solution"} and \code{NA_real_}
#'   for numeric values.
#' @keywords internal
qca_extract <- function(sol, extract_mode = c("first", "all", "essential")) {
  
  extract_mode <- match.arg(extract_mode)
  
  # Base null response
  null_response <- function(mode) {
    base <- list(
      expression   = "No solution",
      inclS        = NA_real_,
      covS         = NA_real_,
      n_solutions  = 0L
    )
    if (mode == "essential") {
      base$selective_terms <- NA_character_
      base$unique_terms     <- NA_character_
    }
    base
  }
  
  if (is.null(sol)) {
    return(null_response(extract_mode))
  }
  
  # === Priority 1: i.sol structure (true Intermediate solution when dir.exp specified) ===
  sol_list <- NULL
  # Track whether the displayed expression comes from the i.sol (dir.exp intermediate)
  # structure or from sol$solution (parsimonious/complex). The per-solution fit lives
  # in a different place for each, so this flag guards the fit lookup below.
  used_isol <- FALSE

  # Try i.sol first (contains true Intermediate solution when dir.exp specified)
  if (!is.null(sol$i.sol) && length(sol$i.sol) > 0) {
    sol_list <- try(sol$i.sol$C1P1$solution, silent = TRUE)
    if (inherits(sol_list, "try-error") || is.null(sol_list) || length(sol_list) == 0) {
      # Try first i.sol entry
      sol_list <- try(sol$i.sol[[1]]$solution, silent = TRUE)
      if (inherits(sol_list, "try-error")) sol_list <- NULL
    }
    if (!is.null(sol_list) && length(sol_list) > 0) used_isol <- TRUE
  }
  
  # Fallback: sol$solution (for Parsimonious or when dir.exp not specified)
  if (is.null(sol_list) || length(sol_list) == 0) {
    if (!is.null(sol$solution) && length(sol$solution) > 0) {
      sol_list <- sol$solution
      used_isol <- FALSE
    }
  }
  
  if (is.null(sol_list) || length(sol_list) == 0) {
    return(null_response(extract_mode))
  }
  
  # === Fit measures: source them from the SAME solution the expression shows ===
  #
  # The displayed expression (sol_list) is either the intermediate solution
  # (sol$i.sol, when dir.exp was given; used_isol == TRUE) or the
  # parsimonious/complex solution (sol$solution; used_isol == FALSE). The fit for
  # each lives in a different place, so we branch on used_isol and never let the
  # intermediate branch read sol$IC (which describes the parsimonious solution).
  inclS <- NA_real_
  covS <- NA_real_

  if (used_isol) {
    # --- Intermediate solution (dir.exp): fit comes from the intermediate itself.
    # sol$IC (sol.incl.cov / individual / overall) all describe the PARSIMONIOUS
    # solution, so reading them here attaches the parsimonious fit to the displayed
    # intermediate formula. The intermediate fit is under sol$i.sol$C1P1$IC, and no
    # sol$IC fallback is used.
    #
    # That intermediate IC has the SAME two shapes as sol$IC: flat
    # ($sol.incl.cov) when the intermediate solution is unique, and split into
    # $individual[[k]] / $overall when the intermediate solution itself has
    # several minimal solutions. Both shapes must be handled, otherwise the
    # multiple-intermediate-solution case yields no fit at all.
    isol_ic <- try(sol$i.sol$C1P1$IC, silent = TRUE)
    if (inherits(isol_ic, "try-error") || is.null(isol_ic)) {
      # Rare shape where C1P1 is absent: first i.sol entry.
      isol_ic <- try(sol$i.sol[[1]]$IC, silent = TRUE)
    }
    if (!inherits(isol_ic, "try-error") && !is.null(isol_ic)) {
      # Path A1: unique intermediate solution.
      ic_flat <- try(isol_ic$sol.incl.cov, silent = TRUE)
      if (!inherits(ic_flat, "try-error") && !is.null(ic_flat)) {
        if (!is.null(ic_flat$inclS)) inclS <- ic_flat$inclS[1]
        if (!is.null(ic_flat$covS))  covS  <- ic_flat$covS[1]
      }
      # Path A2: several intermediate solutions, extract_mode = "first". The
      # displayed expression is the first of them, so use its own fit.
      if (identical(extract_mode, "first") && (is.na(inclS) || is.na(covS)) &&
          !is.null(isol_ic$individual) && length(isol_ic$individual) >= 1L) {
        ic_ind <- try(isol_ic$individual[[1L]]$sol.incl.cov, silent = TRUE)
        if (!inherits(ic_ind, "try-error") && !is.null(ic_ind)) {
          if (is.na(inclS) && !is.null(ic_ind$inclS)) inclS <- ic_ind$inclS[1]
          if (is.na(covS)  && !is.null(ic_ind$covS))  covS  <- ic_ind$covS[1]
        }
      }
      # Path A3: aggregate across the intermediate solutions. Correct for
      # "all"/"essential" (which summarize across solutions) and a last resort.
      if (is.na(inclS) || is.na(covS)) {
        ic_ov <- try(isol_ic$overall$sol.incl.cov, silent = TRUE)
        if (!inherits(ic_ov, "try-error") && !is.null(ic_ov)) {
          if (is.na(inclS) && !is.null(ic_ov$inclS)) inclS <- ic_ov$inclS[1]
          if (is.na(covS)  && !is.null(ic_ov$covS))  covS  <- ic_ov$covS[1]
        }
      }
    }
  } else {
    # --- Parsimonious / complex solution: 2.0.3 behavior (includes the Bug A fix).

    # Path 1: sol$IC$sol.incl.cov (single solution).
    if (is.na(inclS)) {
      incl_val <- try(sol$IC$sol.incl.cov$inclS, silent = TRUE)
      if (!inherits(incl_val, "try-error") && !is.null(incl_val)) inclS <- incl_val
    }
    if (is.na(covS)) {
      cov_val <- try(sol$IC$sol.incl.cov$covS, silent = TRUE)
      if (!inherits(cov_val, "try-error") && !is.null(cov_val)) covS <- cov_val
    }

    # Path 1b: multiple solutions, extract_mode = "first". The displayed expression
    # is the first solution (M1), so its fit is sol$IC$individual[[1]]$sol.incl.cov,
    # NOT the overall aggregate (the disjunction of ALL solutions, which is >= any
    # single solution's coverage on fuzzy data). Crisp data: individual == overall.
    if (identical(extract_mode, "first") &&
        !is.null(sol$IC$individual) && length(sol$IC$individual) >= 1L) {
      ic_first <- try(sol$IC$individual[[1L]]$sol.incl.cov, silent = TRUE)
      if (!inherits(ic_first, "try-error") && !is.null(ic_first)) {
        if (is.na(inclS) && !is.null(ic_first$inclS)) inclS <- ic_first$inclS
        if (is.na(covS)  && !is.null(ic_first$covS))  covS  <- ic_first$covS
      }
    }

    # Path 2: sol$IC$overall. Appropriate for extract_mode = "all"/"essential",
    # which summarize across all solutions, and as a fallback.
    if (is.na(inclS)) {
      incl_val <- try(sol$IC$overall$sol.incl.cov$inclS, silent = TRUE)
      if (!inherits(incl_val, "try-error") && !is.null(incl_val)) inclS <- incl_val
    }
    if (is.na(covS)) {
      cov_val <- try(sol$IC$overall$sol.incl.cov$covS, silent = TRUE)
      if (!inherits(cov_val, "try-error") && !is.null(cov_val)) covS <- cov_val
    }
  }
  
  # Get total solution count (always use get_n_solutions for consistency)
  n_solutions <- get_n_solutions(sol)

  # A solution was found, but no fit measures could be located for it. This
  # should not happen for the shapes QCA currently produces; if a future version
  # stores them elsewhere, tell the user why the cell is empty rather than
  # returning a silent NA that looks like a computation failure.
  if (is.na(inclS) || is.na(covS)) {
    warning(
      "A solution was found, but its consistency/coverage could not be read ",
      "from the QCA result, so inclS/covS are reported as NA",
      if (used_isol) " (intermediate solution)" else "",
      ifelse(is.na(n_solutions) || n_solutions <= 1, "",
             paste0(" (", n_solutions, " minimal solutions)")),
      ". The solution expression itself is unaffected. Please report this ",
      "together with your QCA package version at ",
      "https://github.com/im-research-yt/ThSQCA/issues",
      call. = FALSE
    )
  }


  # Mode-specific processing
  if (extract_mode == "first") {
    expression <- paste(sol_list[[1]], collapse = " + ")
    return(list(
      expression  = expression,
      inclS       = inclS,
      covS        = covS,
      n_solutions = n_solutions
    ))
  }

  # For the modes that summarize ACROSS solutions ("all", "essential"), the
  # model list must be the same enumeration that n_solutions counts, otherwise
  # the reported count and the reported terms describe different things.
  #
  # sol_list above is deliberately chart-1-only (sol$i.sol$C1P1$solution),
  # which is right for "first" because C1P1's first entry is the displayed M1.
  # It is NOT a safe basis for cross-solution summaries: when the intermediate
  # solution spans several prime implicant charts, whether C1P1$solution
  # happens to enumerate every chart's model or only its own is an
  # implementation detail of QCA::minimize()'s internal getSolution() call, not
  # a documented guarantee. collect_unique_i_sol() enumerates every chart and
  # deduplicates by term-set content, and is what get_n_solutions() counts, so
  # using it here keeps the count and the terms consistent by construction.
  models <- collect_unique_i_sol(sol)
  if (is.null(models) || length(models) == 0) {
    models <- sol_list
  }

  if (extract_mode == "all") {
    all_exprs <- sapply(seq_along(models), function(i) {
      paste0("M", i, ": ", paste(models[[i]], collapse = " + "))
    })
    expression <- paste(all_exprs, collapse = "; ")
    return(list(
      expression  = expression,
      inclS       = inclS,
      covS        = covS,
      n_solutions = n_solutions
    ))
  }
  
  if (extract_mode == "essential") {
    # Split each solution into terms
    sol_terms <- lapply(models, function(x) {
      unlist(strsplit(paste(x, collapse = " + "), " \\+ "))
    })
    
    # Essential prime implicants: intersection of all solutions
    essential_terms <- Reduce(intersect, sol_terms)
    
    # All terms: union of all solutions
    all_terms <- Reduce(union, sol_terms)
    
    # Selective prime implicants: in some but not all solutions
    selective_terms <- setdiff(all_terms, essential_terms)
    
    # Unique terms: only in specific solution
    if (n_solutions > 1) {
      unique_terms_list <- lapply(seq_along(sol_terms), function(i) {
        other_terms <- unique(unlist(sol_terms[-i]))
        setdiff(sol_terms[[i]], other_terms)
      })
      unique_terms_formatted <- sapply(seq_along(unique_terms_list), function(i) {
        if (length(unique_terms_list[[i]]) > 0) {
          paste0("M", i, ":", paste(unique_terms_list[[i]], collapse = "+"))
        } else {
          NULL
        }
      })
      unique_terms_str <- paste(unique_terms_formatted[!sapply(unique_terms_formatted, is.null)],
                                collapse = "; ")
      if (unique_terms_str == "") unique_terms_str <- NA_character_
    } else {
      unique_terms_str <- NA_character_
    }
    
    # Essential expression
    expression <- if (length(essential_terms) > 0) {
      paste(essential_terms, collapse = " + ")
    } else {
      "No essential prime implicants"
    }
    
    # Selective expression
    selective_str <- if (length(selective_terms) > 0) {
      paste(selective_terms, collapse = " + ")
    } else {
      NA_character_
    }
    
    return(list(
      expression       = expression,
      inclS            = inclS,
      covS             = covS,
      selective_terms  = selective_str,
      unique_terms     = unique_terms_str,
      n_solutions      = n_solutions
    ))
  }
}
