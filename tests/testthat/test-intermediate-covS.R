# Regression tests for the intermediate-solution fit-measure fix (v2.0.4).
# Fixture (.intermediate_fuzzy_data) lives in helper-fixtures.R.
#
# With dir.exp, the displayed solution is the intermediate (sol$i.sol$C1P1);
# its fit must come from sol$i.sol$C1P1$IC$sol.incl.cov, not sol$IC (which
# describes the parsimonious solution).

test_that("intermediate (used_isol) fit comes from i.sol C1P1, not sol$IC", {
  sol <- list(
    solution = list(c("A", "B"), c("A", "C")),
    i.sol = list(C1P1 = list(
      solution = list(c("A", "B", "D")),
      IC = list(sol.incl.cov = data.frame(inclS = 0.811, covS = 0.172))
    )),
    IC = list(
      sol.incl.cov = NULL,
      individual = list(
        list(sol.incl.cov = data.frame(inclS = 0.90, covS = 0.30)),
        list(sol.incl.cov = data.frame(inclS = 0.90, covS = 0.30))
      ),
      overall = list(sol.incl.cov = data.frame(inclS = 0.455, covS = 0.177))
    )
  )
  for (mode in c("first", "all", "essential")) {
    qe <- ThSQCA:::qca_extract(sol, mode)
    expect_equal(qe$inclS, 0.811, tolerance = 1e-6)
    expect_equal(qe$covS,  0.172, tolerance = 1e-6)
    expect_false(isTRUE(all.equal(qe$covS, 0.177)))
  }
})

test_that("intermediate fit matches i.sol on real fuzzy data (single parsimonious solution)", {
  skip_if_not_installed("QCA")
  conds <- c("A", "B", "C", "D", "E")
  qd <- .intermediate_fuzzy_data()
  tt <- QCA::truthTable(qd, outcome = "Y", conditions = conds,
                        incl.cut = 0.75, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "?",
                       dir.exp = stats::setNames(rep(1, 5), conds),
                       details = TRUE)

  isol_covS   <- sol$i.sol$C1P1$IC$sol.incl.cov$covS[1]
  parsim_covS <- sol$IC$sol.incl.cov$covS[1]
  expect_true(!is.null(isol_covS) && !is.na(isol_covS))
  expect_gt(abs(isol_covS - parsim_covS), 1e-3)

  info <- ThSQCA:::qca_extract(sol, extract_mode = "first")
  expect_equal(info$inclS, sol$i.sol$C1P1$IC$sol.incl.cov$inclS[1], tolerance = 1e-6)
  expect_equal(info$covS,  isol_covS, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(info$covS, parsim_covS, tolerance = 1e-6)))
})
