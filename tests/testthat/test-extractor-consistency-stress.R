# Cross-extractor consistency stress test (v2.0.4).
#
# Scaled-down, fully deterministic version of the differential fuzzer used to
# find the fit-source bugs. The invariant, for every data set / solution-type /
# solution-count / calibration shape reached:
#
#   hand-computed fit of the DISPLAYED first solution
#     == qca_extract(sol, "first")
#     == extract_all_metrics(<matching IC>, sol)
#     == extract_solution_metrics_for_chart(sol, 1)
#
# The displayed solution is sol$i.sol$C1P1$solution[[1]] when dir.exp was given,
# else sol$solution[[1]]. Any regression that reattaches an overall/parsimonious
# fit to the displayed formula fails this test.

.stress_fit_of_terms <- function(terms, qd, y) {
  if (length(terms) == 0) return(c(NA_real_, NA_real_))
  ev <- function(t) {
    l <- strsplit(t, "\\*")[[1]]
    v <- sapply(l, function(p) {
      if (startsWith(p, "~")) 1 - qd[[substring(p, 2)]] else qd[[p]]
    })
    apply(as.matrix(v), 1, min)
  }
  X <- apply(as.matrix(sapply(terms, ev)), 1, max)
  c(sum(pmin(X, y)) / sum(X), sum(pmin(X, y)) / sum(y))
}

.stress_one <- function(qd, conds, include, dir_exp) {
  tt <- tryCatch(
    QCA::truthTable(qd, outcome = "Y", conditions = conds,
                    incl.cut = 0.80, n.cut = 1, complete = FALSE),
    error = function(e) NULL)
  if (is.null(tt)) return(NULL)
  sol <- tryCatch(
    if (is.null(dir_exp)) QCA::minimize(tt, include = include, details = TRUE)
    else QCA::minimize(tt, include = include, dir.exp = dir_exp, details = TRUE),
    error = function(e) NULL)
  if (is.null(sol)) return(NULL)

  is_inter <- !is.null(sol$i.sol) && length(sol$i.sol) > 0 &&
    !is.null(tryCatch(sol$i.sol$C1P1$solution, error = function(e) NULL))
  first_terms <- if (is_inter) {
    tryCatch(sol$i.sol$C1P1$solution[[1]], error = function(e) NULL)
  } else {
    tryCatch(sol$solution[[1]], error = function(e) NULL)
  }
  if (is.null(first_terms) || length(first_terms) == 0) return(NULL)

  oracle <- .stress_fit_of_terms(first_terms, qd, qd$Y)
  if (is.na(oracle[2])) return(NULL)

  ic_for_report <- if (is_inter) sol$i.sol$C1P1$IC else sol$IC
  list(
    oracle_covS = oracle[2],
    qe   = tryCatch(ThSQCA:::qca_extract(sol, "first")$covS, error = function(e) NA_real_),
    am   = tryCatch(ThSQCA:::extract_all_metrics(ic_for_report, sol)$sol_covS,
                    error = function(e) NA_real_),
    ch   = tryCatch(ThSQCA:::extract_solution_metrics_for_chart(sol, 1)$covS,
                    error = function(e) NA_real_)
  )
}

test_that("all three solution-level extractors match the displayed solution's fit", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D")
  dir_exp <- stats::setNames(rep(1, 4), conds)

  n_checked <- 0L
  for (crisp in c(TRUE, FALSE)) {
    for (seed in 1:4) {
      set.seed(seed * 13 + 4 * 97 + if (crisp) 0 else 4000)
      n <- 45
      if (crisp) {
        qd <- as.data.frame(lapply(conds, function(z) rbinom(n, 1, 0.5)))
        names(qd) <- conds
        qd$Y <- rbinom(n, 1, plogis(0.9 * qd$A + 0.8 * qd$B - 0.5))
      } else {
        qd <- as.data.frame(lapply(conds, function(z) round(plogis(rnorm(n, 0, 1.4)), 3)))
        names(qd) <- conds
        qd$Y <- round(plogis(
          qnorm(pmax(pmin(qd$A, .999), .001)) +
          0.9 * qnorm(pmax(pmin(qd$B, .999), .001)) + rnorm(n, 0, 1.2)), 3)
      }
      for (spec in list(list(inc = "", dx = NULL),
                        list(inc = "?", dx = NULL),
                        list(inc = "?", dx = dir_exp))) {
        r <- .stress_one(qd, conds, spec$inc, spec$dx)
        if (is.null(r)) next
        n_checked <- n_checked + 1L
        if (!is.na(r$qe)) expect_equal(r$qe, r$oracle_covS, tolerance = 2e-3)
        if (!is.na(r$am)) expect_equal(r$am, r$oracle_covS, tolerance = 2e-3)
        if (!is.na(r$ch)) expect_equal(r$ch, r$oracle_covS, tolerance = 2e-3)
      }
    }
  }
  # The loop must actually have exercised cells, otherwise the test is vacuous.
  expect_gt(n_checked, 5L)
})
