# Regression tests for the core/peripheral polarity-pooling fix (v2.0.5).
#
# When the parsimonious solution has several tied minimal solutions, the terms
# used to used to be pooled across all of them before the condition-status map
# was built. A condition present in M1 and absent in M2 therefore ended up
# holding both statuses, so an intermediate term matched whichever polarity it
# used and the condition was classified "core" either way: a structural bias
# toward core. A status now counts only where every minimal solution agrees.

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

  pooled <- extract_cond_status_map(unique(unlist(models)), conds)
  expect_setequal(pooled[["C"]], c("present", "absent"))   # the old, biased map

  map <- build_parsim_status_map(models, conds)
  expect_equal(length(map[["C"]]), 0L)   # no status survives the disagreement
  # Conditions the solutions agree on are unaffected.
  expect_true("present" %in% map[["A"]])
  expect_true("present" %in% map[["B"]])

  # Consequently an intermediate term using either polarity of C is peripheral.
  for (term in c("A*C*B", "A*~C*B")) {
    cl <- classify_term_conditions(term, map, conds)
    expect_equal(cl$type[cl$condition == "C"], "peripheral")
    expect_equal(cl$type[cl$condition == "A"], "core")
  }
})

test_that("with a single minimal solution the map is unchanged", {
  conds <- c("A", "B", "C")
  single <- list(c("A*C", "B"))
  old <- extract_cond_status_map(unlist(single), conds)
  new <- build_parsim_status_map(single, conds)
  expect_equal(lapply(old, sort), lapply(new, sort))
})

test_that("a condition absent from one minimal solution is not core", {
  conds <- c("A", "B", "C")
  # C is present in M1 and does not occur at all in M2.
  models <- list(c("A*C"), c("A*B"))
  map <- build_parsim_status_map(models, conds)
  expect_equal(length(map[["C"]]), 0L)
  expect_true("present" %in% map[["A"]])
  cl <- classify_term_conditions("A*C", map, conds)
  expect_equal(cl$type[cl$condition == "C"], "peripheral")
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
