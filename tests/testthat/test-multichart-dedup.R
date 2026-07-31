# Regression tests for the multi-prime-implicant-chart double-counting fix
# (collect_unique_i_sol(), added after v2.0.5).
#
# Bug: when include = "?" and dir.exp are specified and QCA::minimize()
# splits the intermediate solution across multiple prime implicant charts
# (print()'s "From C1P1, C2P1:"), sol$i.sol has one list entry per chart.
# Each entry's own $solution field is computed independently by
# QCA::minimize() and is not guaranteed to be chart-exclusive: on this
# fixture it already contains the full, resolved list of both M1 and M2 in
# BOTH chart entries. Naively concatenating every chart's $solution
# (the pre-fix behavior of get_n_solutions() and of two sol_list-building
# blocks in generate_report()) therefore double-counts: 2 charts x 2
# solutions each = 4, when only 2 unique models actually exist. This
# propagated a wrong n_solutions = 4 into res$summary and into the
# generated Markdown report's Summary Table.
#
# Fixture: .multichart_brand_data() (helper-fixtures.R), a real n = 336
# brand survey dataset. At thrY = 9, DLV = 8 (SPV = PRD = COM = 7 fixed,
# incl.cut = 0.85), this specific combination reliably produces the
# "C1P1, C2P1" two-chart structure (verified interactively; a hand-crafted
# small synthetic dataset only ever reproduced the different "C1P1, C1P2"
# single-chart/multiple-parsimonious-path case, so a real fixture is used).

test_that("get_n_solutions() does not double-count models across multiple prime implicant charts", {
  skip_if_not_installed("QCA")
  dat <- .multichart_brand_data()

  res <- ThSQCA::dtSweep(
    dat = dat, outcome = "INT", conditions = c("SPV", "PRD", "DLV", "COM"),
    sweep_range_Y = 6:9,
    sweep_list_X  = list(DLV = 6:9, SPV = 7, PRD = 7, COM = 7),
    incl.cut = 0.85, n.cut = 1,
    include = "?", dir.exp = c(1, 1, 1, "-"),
    extract_mode = "all", return_details = TRUE
  )

  idx <- which(res$summary$n_solutions > 1)
  skip_if(length(idx) == 0, "fixture no longer produces a multi-chart cell")

  sol <- res$details[[idx[1]]]$solution

  # Ground truth: print(sol) shows exactly two models, M1 and M2, spanning
  # two prime implicant charts.
  expect_true(!is.null(sol$i.sol))
  expect_gte(length(sol$i.sol), 2L)

  expect_equal(ThSQCA:::get_n_solutions(sol), 2L)
  expect_equal(res$summary$n_solutions[idx[1]], 2L)
})

test_that("collect_unique_i_sol() deduplicates by term-set content, not by chart identity", {
  skip_if_not_installed("QCA")
  dat <- .multichart_brand_data()

  res <- ThSQCA::dtSweep(
    dat = dat, outcome = "INT", conditions = c("SPV", "PRD", "DLV", "COM"),
    sweep_range_Y = 6:9,
    sweep_list_X  = list(DLV = 6:9, SPV = 7, PRD = 7, COM = 7),
    incl.cut = 0.85, n.cut = 1,
    include = "?", dir.exp = c(1, 1, 1, "-"),
    extract_mode = "all", return_details = TRUE
  )
  idx <- which(res$summary$n_solutions > 1)
  skip_if(length(idx) == 0, "fixture no longer produces a multi-chart cell")
  sol <- res$details[[idx[1]]]$solution

  uniq <- ThSQCA:::collect_unique_i_sol(sol)
  expect_length(uniq, 2L)

  # Each unique model must match one of the two chart-specific c.sol vectors
  # (the values print() actually displays as M1 and M2), independent of
  # which chart name it came from.
  chart_names <- names(sol$i.sol)
  csols <- lapply(chart_names, function(m) sol$i.sol[[m]]$c.sol)
  key <- function(x) paste(sort(as.character(x)), collapse = " | ")
  uniq_keys <- vapply(uniq, key, character(1))
  csol_keys <- vapply(csols, key, character(1))
  expect_setequal(uniq_keys, unique(csol_keys))
})

test_that("identify_epi() on the deduplicated list matches print()'s EPI/SPI split", {
  skip_if_not_installed("QCA")
  dat <- .multichart_brand_data()

  res <- ThSQCA::dtSweep(
    dat = dat, outcome = "INT", conditions = c("SPV", "PRD", "DLV", "COM"),
    sweep_range_Y = 6:9,
    sweep_list_X  = list(DLV = 6:9, SPV = 7, PRD = 7, COM = 7),
    incl.cut = 0.85, n.cut = 1,
    include = "?", dir.exp = c(1, 1, 1, "-"),
    extract_mode = "all", return_details = TRUE
  )
  idx <- which(res$summary$n_solutions > 1)
  skip_if(length(idx) == 0, "fixture no longer produces a multi-chart cell")
  sol <- res$details[[idx[1]]]$solution

  uniq <- ThSQCA:::collect_unique_i_sol(sol)
  epi_info <- identify_epi(uniq)

  expect_equal(epi_info$n_solutions, 2L)
  # Ground truth from print(sol): the two essential (non-parenthesized)
  # terms shared by both M1 and M2, and the two chart-specific optional terms.
  expect_setequal(epi_info$epi, c("~PRD*DLV*COM", "PRD*DLV*~COM"))
  expect_setequal(epi_info$spi, c("SPV*~PRD*DLV", "SPV*DLV*~COM"))
})

test_that("single-chart single-solution cells are unaffected by the dedup fix", {
  skip_if_not_installed("QCA")
  dat <- .multichart_brand_data()

  res <- ThSQCA::dtSweep(
    dat = dat, outcome = "INT", conditions = c("SPV", "PRD", "DLV", "COM"),
    sweep_range_Y = 6:9,
    sweep_list_X  = list(DLV = 6:9, SPV = 7, PRD = 7, COM = 7),
    incl.cut = 0.85, n.cut = 1,
    include = "?", dir.exp = c(1, 1, 1, "-"),
    extract_mode = "all", return_details = TRUE
  )

  single_idx <- which(res$summary$n_solutions == 1)
  expect_gt(length(single_idx), 0L)
  for (i in single_idx) {
    sol <- res$details[[i]]$solution
    expect_equal(ThSQCA:::get_n_solutions(sol), 1L)
    uniq <- ThSQCA:::collect_unique_i_sol(sol)
    expect_length(uniq, 1L)
  }

  no_sol_idx <- which(res$summary$n_solutions == 0)
  if (length(no_sol_idx) > 0) {
    for (i in no_sol_idx) {
      expect_null(res$details[[i]]$solution)
    }
  }
})

# The same duplication pattern existed in two further places, found in a
# follow-up audit of every sol$i.sol traversal in the package.

test_that("extract_solution_list() deduplicates, so config charts are not emitted twice", {
  skip_if_not_installed("QCA")
  dat <- .multichart_brand_data()

  res <- ThSQCA::dtSweep(
    dat = dat, outcome = "INT", conditions = c("SPV", "PRD", "DLV", "COM"),
    sweep_range_Y = 6:9,
    sweep_list_X  = list(DLV = 6:9, SPV = 7, PRD = 7, COM = 7),
    incl.cut = 0.85, n.cut = 1,
    include = "?", dir.exp = c(1, 1, 1, "-"),
    extract_mode = "all", return_details = TRUE
  )
  idx <- which(res$summary$n_solutions > 1)
  skip_if(length(idx) == 0, "fixture no longer produces a multi-chart cell")
  sol <- res$details[[idx[1]]]$solution

  sl <- ThSQCA:::extract_solution_list(sol)
  expect_length(sl, 2L)
  expect_equal(length(sl), ThSQCA:::get_n_solutions(sol))

  # generate_config_chart() emits one table per solution; with duplicates it
  # previously emitted the same table more than once and announced an inflated
  # number of equivalent solutions.
  chart <- ThSQCA:::generate_config_chart(sol)
  n_headings <- length(gregexpr("### Solution", chart, fixed = TRUE)[[1]])
  expect_equal(n_headings, 2L)
  expect_true(grepl("2 equivalent solutions", chart, fixed = TRUE))
  expect_false(grepl("4 equivalent solutions", chart, fixed = TRUE))
})

test_that("extract_sol_terms_by_model() deduplicates, so parsim_n_solutions is not inflated", {
  skip_if_not_installed("QCA")
  dat <- .multichart_brand_data()

  res <- ThSQCA::dtSweep(
    dat = dat, outcome = "INT", conditions = c("SPV", "PRD", "DLV", "COM"),
    sweep_range_Y = 6:9,
    sweep_list_X  = list(DLV = 6:9, SPV = 7, PRD = 7, COM = 7),
    incl.cut = 0.85, n.cut = 1,
    include = "?", dir.exp = c(1, 1, 1, "-"),
    extract_mode = "all", return_details = TRUE
  )
  idx <- which(res$summary$n_solutions > 1)
  skip_if(length(idx) == 0, "fixture no longer produces a multi-chart cell")
  sol <- res$details[[idx[1]]]$solution

  models <- ThSQCA:::extract_sol_terms_by_model(sol)
  expect_length(models, 2L)

  # No two returned models may share the same term set.
  keys <- vapply(models, function(x) paste(sort(x), collapse = " | "), character(1))
  expect_equal(length(unique(keys)), length(keys))
})
