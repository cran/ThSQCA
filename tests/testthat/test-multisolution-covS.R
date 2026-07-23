# Regression tests for the multiple-minimal-solution fit-measure fix (v2.0.3).
# Fixtures (.multisol_fuzzy_data, .hand_covS) live in helper-fixtures.R.
#
# With multiple minimal solutions, extract_mode = "first" displays M1; its fit
# must be sol$IC$individual[[1]]$sol.incl.cov, not sol$IC$overall (the
# disjunction of all solutions, which overinflates coverage on fuzzy data).

test_that("first-solution covS matches individual[[1]], not overall (fuzzy, multiple solutions)", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D")
  qd <- .multisol_fuzzy_data()

  tt <- QCA::truthTable(qd, outcome = "Y", conditions = conds,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "", details = TRUE)

  expect_gt(length(sol$solution), 1L)
  indiv1_covS  <- sol$IC$individual[[1]]$sol.incl.cov$covS
  overall_covS <- sol$IC$overall$sol.incl.cov$covS
  expect_gt(abs(overall_covS - indiv1_covS), 1e-3)

  info <- ThSQCA:::qca_extract(sol, extract_mode = "first")
  expect_equal(info$covS, indiv1_covS, tolerance = 1e-6)
  hand <- .hand_covS(sol$solution[[1]], qd[conds], qd$Y)
  expect_equal(info$covS, hand, tolerance = 1e-3)
  expect_false(isTRUE(all.equal(info$covS, overall_covS, tolerance = 1e-6)))
})

test_that("single-solution cell is unaffected by the fix", {
  skip_if_not_installed("QCA")
  data("sample_data", package = "ThSQCA")
  res <- otSweep(dat = sample_data, outcome = "Y",
                 conditions = c("X1", "X2", "X3"),
                 sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
                 return_details = FALSE)
  ok <- !is.na(res$covS)
  expect_true(all(res$covS[ok] >= 0 & res$covS[ok] <= 1))
})

test_that("all/essential modes still report the overall aggregate", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D")
  qd <- .multisol_fuzzy_data()
  tt <- QCA::truthTable(qd, outcome = "Y", conditions = conds,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "", details = TRUE)
  skip_if(length(sol$solution) <= 1L)

  overall_covS <- sol$IC$overall$sol.incl.cov$covS
  info_all <- ThSQCA:::qca_extract(sol, extract_mode = "all")
  expect_equal(info_all$covS, overall_covS, tolerance = 1e-6)
})
