# Regression tests for the core/peripheral polarity safeguard (v2.0.5),
# ported to the configuration-level classification of v2.0.8.
#
# When the parsimonious solution has several tied minimal solutions, the terms
# used to be pooled across all of them. A condition present in M1 and absent in
# M2 therefore held both statuses, so an intermediate term matched whichever
# polarity it used and the condition was classified "core" either way. Since
# v2.0.5 a condition is core only where every (source) minimal solution agrees.
#
# v2.0.8 changed how core status is judged against ONE parsimonious solution:
# instead of "appears anywhere in the parsimonious solution", a condition is
# core when it belongs to a parsimonious term contained in the intermediate term
# (Fiss 2011, Tables 4 and 5). The scenarios below are unchanged; where the
# expected label changed because of that definition, the comment says so.

test_that("extract_sol_terms_by_model keeps minimal solutions apart", {
  sol <- list(solution = list(c("A*C", "B"), c("A*~C", "B")))
  models <- extract_sol_terms_by_model(sol)
  expect_equal(length(models), 2L)
  expect_true(all(c("A*C", "B") %in% models[[1]]))
  expect_true(all(c("A*~C", "B") %in% models[[2]]))
  # The pooled helper still returns the union, for the displayed expression.
  expect_true(all(c("A*C", "A*~C", "B") %in% extract_sol_terms(sol)))
})

test_that("a condition with conflicting polarity across minimal solutions is not core", {
  conds <- c("A", "B", "C")
  models <- list(c("A*C", "B"), c("A*~C", "B"))   # C present in M1, absent in M2

  for (term in c("A*C*B", "A*~C*B")) {
    cl <- classify_term_fiss(term, models, conds)
    expect_equal(cl$type[cl$condition == "C"], "peripheral")
    # B is a parsimonious term of both solutions and is contained in the term.
    expect_equal(cl$type[cl$condition == "B"], "core")
    # Changed in v2.0.8: relative to the solution whose A-term has the other
    # polarity of C, only B is contained in the intermediate term, so A is not
    # core there either (it was core under the v2.0.7 whole-solution rule).
    expect_equal(cl$type[cl$condition == "A"], "peripheral")
  }
})

test_that("with a single minimal solution only that solution decides", {
  conds <- c("A", "B", "C")
  single <- list(c("A*C", "B"))
  cl <- classify_term_fiss("A*B*C", single, conds)
  # Both parsimonious terms are contained in A*B*C, so all three are core.
  expect_equal(cl$type, c("core", "core", "core"))
  cl2 <- classify_term_fiss("A*~B*C", single, conds)
  expect_equal(cl2$type[cl2$condition == "B"], "peripheral")
  expect_equal(cl2$type[cl2$condition %in% c("A", "C")], c("core", "core"))
})

test_that("a condition absent from one minimal solution is not core", {
  conds <- c("A", "B", "C")
  # C is present in M1 and does not occur at all in M2.
  models <- list(c("A*C"), c("A*B"))
  cl <- classify_term_fiss("A*C", models, conds)
  expect_equal(cl$type[cl$condition == "C"], "peripheral")
  # Changed in v2.0.8: no term of M2 is contained in A*C, so nothing is core
  # relative to M2, and the term is flagged.
  expect_equal(cl$type[cl$condition == "A"], "peripheral")
  expect_equal(attr(cl, "not_nested"), 2L)
})

test_that("compute_fiss_core records the number of tied parsimonious solutions", {
  skip_if_not_installed("QCA")
  data("sample_data", package = "ThSQCA")
  res <- otSweep(dat = sample_data, outcome = "Y",
                 conditions = c("X1", "X2", "X3"),
                 sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
                 include = "?", dir.exp = c(X1 = 1, X2 = 1, X3 = 1),
                 return_details = TRUE)
  f <- suppressWarnings(compute_fiss_core(res))
  entries <- Filter(function(e) !is.null(e$classification), f$fiss_core)
  skip_if(length(entries) == 0)
  for (e in entries) {
    expect_true(!is.null(e$parsim_n_solutions))
    expect_true(e$parsim_n_solutions >= 1)
  }
})
