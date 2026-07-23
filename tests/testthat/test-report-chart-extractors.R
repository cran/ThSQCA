# Regression tests for the report and chart fit extractors (v2.0.4).
# Fixtures live in helper-fixtures.R.
#
# extract_all_metrics() (report path) and extract_solution_metrics_for_chart()
# navigate the same polymorphic minimize() output as qca_extract() and had the
# same bug family independently: they returned sol$IC$overall (multiple
# solutions) or the parsimonious fit (intermediate) instead of the displayed
# solution's own fit. These tests pin the corrected behavior and assert the
# three solution-level extractors agree with each other.

test_that("report/chart extractors report individual[[1]], not overall (fuzzy multiple solutions)", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D")
  qd <- .multisol_fuzzy_data()
  tt <- QCA::truthTable(qd, outcome = "Y", conditions = conds,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "", details = TRUE)
  skip_if(length(sol$solution) <= 1L)

  indiv1  <- sol$IC$individual[[1]]$sol.incl.cov$covS
  overall <- sol$IC$overall$sol.incl.cov$covS
  expect_gt(abs(overall - indiv1), 1e-3)

  am <- ThSQCA:::extract_all_metrics(sol$IC, sol)
  ch <- ThSQCA:::extract_solution_metrics_for_chart(sol, 1)
  expect_equal(am$sol_covS, indiv1, tolerance = 1e-6)
  expect_equal(ch$covS,     indiv1, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(am$sol_covS, overall, tolerance = 1e-6)))
  expect_false(isTRUE(all.equal(ch$covS,     overall, tolerance = 1e-6)))
})

test_that("report/chart extractors report the intermediate fit, not the parsimonious fit", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D", "E")
  qd <- .intermediate_fuzzy_data()
  tt <- QCA::truthTable(qd, outcome = "Y", conditions = conds,
                        incl.cut = 0.75, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "?",
                       dir.exp = stats::setNames(rep(1, 5), conds),
                       details = TRUE)

  isol   <- sol$i.sol$C1P1$IC$sol.incl.cov$covS[1]
  parsim <- sol$IC$sol.incl.cov$covS[1]
  expect_gt(abs(isol - parsim), 1e-3)

  # Unit-level check: given the intermediate IC, the extractor returns the
  # intermediate fit. (Whether generate_report() actually hands it the
  # intermediate IC at each call site is verified end-to-end in
  # test-generate-report-e2e.R, which runs the full report pipeline.)
  am <- ThSQCA:::extract_all_metrics(sol$i.sol$C1P1$IC, sol)
  ch <- ThSQCA:::extract_solution_metrics_for_chart(sol, 1)
  expect_equal(am$sol_covS, isol, tolerance = 1e-6)
  expect_equal(ch$covS,     isol, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(ch$covS, parsim, tolerance = 1e-6)))

  # Per-term (path) chart table must describe the intermediate solution: one row
  # per intermediate product term, not the parsimonious term table.
  pm <- ThSQCA:::extract_path_metrics_for_chart(sol, 1)
  expect_false(is.null(pm))
  expect_equal(nrow(pm), length(sol$i.sol$C1P1$solution[[1]]))
})

test_that("qca_extract, extract_all_metrics and the chart extractor agree (fuzzy multiple solutions)", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D")
  qd <- .multisol_fuzzy_data()
  tt <- QCA::truthTable(qd, outcome = "Y", conditions = conds,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "", details = TRUE)
  skip_if(length(sol$solution) <= 1L)

  v_qe <- ThSQCA:::qca_extract(sol, "first")$covS
  v_am <- ThSQCA:::extract_all_metrics(sol$IC, sol)$sol_covS
  v_ch <- ThSQCA:::extract_solution_metrics_for_chart(sol, 1)$covS
  expect_equal(v_qe, v_am, tolerance = 1e-6)
  expect_equal(v_qe, v_ch, tolerance = 1e-6)
})
