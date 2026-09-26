# Regression tests for the v2.0.8 fix to compute_fiss_core().
#
# With include = "?" and dir.exp, QCA::minimize() records in sol$i.sol which
# parsimonious minimal solution ($p.sol) each intermediate model was derived
# from ("From C1P1, C1P2:" in print()). Up to v2.0.7, compute_fiss_core()
# ignored this and
#   (a) pooled the terms of all intermediate models, so the Fiss chart could
#       contain terms that are not in the reported solution (M1), and
#   (b) required agreement with every tied parsimonious solution, including
#       ones M1 was not derived from, so core conditions could be demoted.
# The classification now covers M1 only and compares it with its own source
# parsimonious solution(s). When M1 is derived from several tied parsimonious
# solutions, agreement among those is still required (v2.0.5 polarity fix).

# --- unit tests (no QCA needed) --------------------------------------------

mock_sol <- function() {
  list(
    solution = list(c("A*B", "C*D"), c("C*D", "B*~C*~D")),
    i.sol = list(
      C1P1 = list(p.sol = c("A*B", "C*D"),
                  solution = list(c("A*B", "B*C*D"))),
      C1P2 = list(p.sol = c("C*D", "B*~C*~D"),
                  solution = list(c("B*C*D", "A*B*~C*~D")))
    )
  )
}

test_that("trace_intermediate_sources() follows only the branch that produced M1", {
  tr <- trace_intermediate_sources(mock_sol())
  expect_equal(tr$interm_terms, c("A*B", "B*C*D"))
  expect_equal(tr$sources, "C1P1")
  expect_equal(length(tr$parsim_models), 1L)
  expect_setequal(tr$parsim_models[[1]], c("A*B", "C*D"))
})

test_that("trace_intermediate_sources() keeps every branch that yields the same M1", {
  sol <- list(i.sol = list(
    C1P1 = list(p.sol = c("TRU", "SUP"), solution = list(c("SUP", "TRU*PRC"))),
    C1P2 = list(p.sol = c("PRC", "SUP"), solution = list(c("SUP", "TRU*PRC")))
  ))
  tr <- trace_intermediate_sources(sol)
  expect_equal(tr$sources, c("C1P1", "C1P2"))
  expect_equal(length(tr$parsim_models), 2L)
})

test_that("trace_intermediate_sources() returns NULL when the derivation is unavailable", {
  expect_null(trace_intermediate_sources(list(solution = list(c("A", "B")))))
  sol <- mock_sol(); sol$i.sol$C1P1$p.sol <- NULL
  expect_null(trace_intermediate_sources(sol))
})

test_that("format_models_expr() labels several models and leaves one unlabeled", {
  expect_equal(format_models_expr(list(c("A", "B"))), "A + B")
  expect_equal(format_models_expr(list(c("A", "B"), c("C", "B"))),
               "P1: A + B; P2: C + B")
  expect_equal(format_models_expr(list(c("A", "B"), c("C", "B")), c("P2", "P3")),
               "P2: A + B; P3: C + B")
  expect_equal(format_models_expr(list()), "No solution")
})

test_that("trace_intermediate_sources() keeps QCA's P labels", {
  sol <- list(i.sol = list(
    C1P1 = list(p.sol = c("TRU", "SUP"), solution = list(c("SUP", "TRU*PRC"))),
    C1P2 = list(p.sol = c("PRC", "SUP"), solution = list(c("SUP", "TRU*PRC"))),
    C2P2 = list(p.sol = c("PRC", "SUP"), solution = list(c("SUP", "TRU*PRC")))
  ))
  tr <- trace_intermediate_sources(sol)
  expect_equal(tr$sources, c("C1P1", "C1P2", "C2P2"))
  expect_equal(length(tr$parsim_models), 2L)   # C2P2 repeats P2's model
  expect_equal(tr$parsim_labels, c("P1", "P2"))
})

# --- end-to-end tests (QCA) -------------------------------------------------

test_that("intermediate models derived from different parsimonious solutions are not mixed", {
  skip_if_not_installed("QCA")
  # Two tied parsimonious solutions, each yielding a DIFFERENT intermediate
  # model: C1P1 (A*B + C*D) -> A*B + B*C*D; C1P2 (C*D + B*~C*~D) -> B*C*D + A*B*~C*~D.
  obs <- data.frame(A = c(0, 0, 0, 0, 1, 1), B = c(0, 1, 1, 1, 0, 1),
                    C = c(0, 0, 1, 1, 0, 0), D = c(0, 1, 0, 1, 0, 0),
                    Y = c(0, 0, 0, 1, 0, 1))
  res <- suppressWarnings(otSweep(
    dat = obs, outcome = "Y", conditions = LETTERS[1:4], sweep_range = 1,
    thrX = c(A = 1, B = 1, C = 1, D = 1), include = "?",
    dir.exp = c(1, 1, 1, 1), incl.cut = 1, n.cut = 1, return_details = TRUE))

  # M1 has a single source, so no tie warning is raised.
  f  <- expect_silent(compute_fiss_core(res, conditions = LETTERS[1:4]))
  fc <- f$fiss_core[["1"]]

  # Classification covers exactly the reported M1 (no pooled term from M2).
  expect_equal(fc$interm_expression, res$summary$expression[1])
  expect_setequal(unique(fc$classification$term_expr), c("A*B", "B*C*D"))
  expect_false("A*B*~C*~D" %in% fc$classification$term_expr)

  # Compared with its own source only; A*B is a parsimonious term, so A is core.
  expect_equal(fc$parsim_sources, "C1P1")
  expect_equal(fc$parsim_expression, "A*B + C*D")
  cl <- fc$classification
  expect_equal(cl$type[cl$term_expr == "A*B" & cl$condition == "A"], "core")
  expect_equal(cl$type[cl$term_expr == "A*B" & cl$condition == "B"], "core")
  expect_equal(fc$parsim_n_solutions, 2L)
  expect_equal(fc$interm_n_solutions, 2L)
})

test_that("an intermediate solution derived from several tied parsimonious solutions stays conservative", {
  skip_if_not_installed("QCA")
  # Truth table: 000 (13/30 Y), 001 (6/7), 011, 101, 110, 111 all Y; 010 and 100
  # are remainders. Parsimonious: TRU + SUP | PRC + SUP. Intermediate with
  # dir.exp = c(1, 1, 1): SUP + TRU*PRC, derived identically from both.
  cfg <- function(t, p, s, n, y) data.frame(TRU = t, PRC = p, SUP = s,
                                           RNW = c(rep(1, y), rep(0, n - y)))
  dat <- rbind(cfg(0, 0, 0, 30, 13), cfg(0, 0, 1, 7, 6), cfg(0, 1, 1, 3, 3),
               cfg(1, 0, 1, 3, 3), cfg(1, 1, 0, 4, 4), cfg(1, 1, 1, 13, 13))
  res <- suppressWarnings(otSweep(
    dat = dat, outcome = "RNW", conditions = c("TRU", "PRC", "SUP"),
    sweep_range = 1, thrX = c(TRU = 1, PRC = 1, SUP = 1), include = "?",
    dir.exp = c(1, 1, 1), return_details = TRUE))

  expect_warning(f <- compute_fiss_core(res), "derived from 2 tied parsimonious")
  fc <- f$fiss_core[["1"]]
  expect_equal(fc$parsim_sources, c("C1P1", "C1P2"))
  expect_equal(fc$parsim_expression, "P1: TRU + SUP; P2: PRC + SUP")
  cl <- fc$classification
  expect_equal(cl$type[cl$term_expr == "SUP" & cl$condition == "SUP"], "core")
  # TRU is core relative to C1P1 but not C1P2, and PRC the reverse.
  expect_equal(cl$type[cl$term_expr == "TRU*PRC" & cl$condition == "TRU"], "peripheral")
  expect_equal(cl$type[cl$term_expr == "TRU*PRC" & cl$condition == "PRC"], "peripheral")
})

test_that("thresholds without a solution have the same fields as the others", {
  skip_if_not_installed("QCA")
  data("sample_data", package = "ThSQCA")
  res <- suppressWarnings(otSweep(dat = sample_data, outcome = "Y",
                 conditions = c("X1", "X2", "X3"),
                 sweep_range = 7:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
                 include = "?", dir.exp = c(1, 1, 1), return_details = TRUE))
  f <- compute_fiss_core(res)
  expect_setequal(names(f$fiss_core[["8"]]), names(f$fiss_core[["7"]]))
  expect_null(f$fiss_core[["8"]]$classification)
})

test_that("sample_data: every condition of X3 + X1*X2 is core, as in 2.0.7", {
  skip_if_not_installed("QCA")
  data("sample_data", package = "ThSQCA")
  res <- otSweep(dat = sample_data, outcome = "Y",
                 conditions = c("X1", "X2", "X3"),
                 sweep_range = 6:7, thrX = c(X1 = 7, X2 = 7, X3 = 7),
                 include = "?", dir.exp = c(1, 1, 1), return_details = TRUE)
  f <- expect_silent(compute_fiss_core(res))
  for (k in c("6", "7")) {
    fc <- f$fiss_core[[k]]
    expect_equal(fc$parsim_expression, "X3 + X1*X2")
    expect_equal(fc$interm_expression, "X3 + X1*X2")
    expect_true(all(fc$classification$type[fc$classification$status != "dontcare"] == "core"))
  }
})

# --- configuration-level rule (v2.0.8): Fiss (2011) Tables 4 and 5 ----------
#
# Fiss (2011, working-paper version) marks core conditions with large symbols
# and groups solutions by
# their core. The parsimonious terms below are the minimal sets of large
# symbols in his tables; the intermediate configurations are his columns.
# Classifying each column against them must reproduce every large/small symbol.

fiss_conds <- c("Size", "Form", "Cent", "Comp", "Diff", "Cost", "Change", "Unc")

check_fiss_table <- function(configs, parsim, expected_core) {
  for (k in names(configs)) {
    cl <- classify_term_fiss(configs[[k]], list(parsim), fiss_conds)
    got <- cl$condition[cl$type == "core"]
    expect_setequal(got, expected_core[[k]])
    expect_length(attr(cl, "not_nested"), 0L)
  }
}

test_that("Fiss (2011) Table 4 is reproduced", {
  configs <- list(
    "1a" = "~Size*Form*Cent*Diff*Cost*~Change*~Unc",
    "1b" = "~Size*Form*Cent*Comp*Diff*Cost*~Unc",
    "2"  = "~Size*~Form*Cent*~Comp*Diff*Cost*~Change*~Unc",
    "3a" = "~Size*~Form*~Cent*Comp*Diff*~Cost*Change*~Unc",
    "3b" = "~Size*~Form*~Cent*~Comp*Diff*~Cost*~Change*~Unc",
    "4"  = "Size*Form*~Cent*Comp*Diff*~Change*~Unc")
  parsim <- c("Cost", "~Form*Diff", "Size*~Unc")
  expected <- list("1a" = "Cost", "1b" = "Cost", "2" = c("Form", "Diff", "Cost"),
                   "3a" = c("Form", "Diff"), "3b" = c("Form", "Diff"),
                   "4" = c("Size", "Unc"))
  check_fiss_table(configs, parsim, expected)
  # Differentiation is core in 2, 3a, 3b but peripheral in 1a, 1b, 4, as in
  # Fiss's table (the v2.0.7 whole-solution rule made it core everywhere).
  cl4 <- classify_term_fiss(configs[["4"]], list(parsim), fiss_conds)
  expect_equal(cl4$type[cl4$condition == "Diff"], "peripheral")
})

test_that("Fiss (2011) Table 5 is reproduced", {
  configs <- list(
    "1a" = "Size*Form*~Cent*Comp*Diff*Cost*~Change*~Unc",
    "1b" = "~Size*Form*Cent*Comp*Diff*Cost*~Change*~Unc",
    "2"  = "~Size*~Form*Cent*~Comp*Diff*Cost*~Change*Unc")
  parsim <- c("Comp*Cost*~Change", "~Form*Diff*Unc")
  expected <- list("1a" = c("Comp", "Cost", "Change"),
                   "1b" = c("Comp", "Cost", "Change"),
                   "2"  = c("Form", "Diff", "Unc"))
  check_fiss_table(configs, parsim, expected)
})

test_that("a condition in another parsimonious term is not core (no ties)", {
  skip_if_not_installed("QCA")
  # Parsimonious ~A*E + A*B (unique). The term ~A*~B*C*E contains ~A*E only,
  # so ~B is peripheral even though B occurs in the other parsimonious term.
  # The term B*C*D*E contains neither parsimonious term.
  d <- data.frame(A = c(6, 1, 5, 1, 7, 9, 6, 3, 1, 3, 3),
                  B = c(9, 8, 4, 9, 6, 4, 8, 3, 8, 5, 8),
                  C = c(6, 9, 6, 9, 6, 2, 3, 1, 3, 9, 8),
                  D = c(8, 2, 4, 6, 9, 2, 7, 7, 8, 7, 2),
                  E = c(5, 1, 5, 8, 8, 5, 4, 4, 1, 4, 1),
                  Y = c(9, 7, 7, 8, 8, 2, 7, 4, 3, 3, 4))
  res <- suppressWarnings(ctSweepS(d, "Y", LETTERS[1:5], sweep_var = "A",
                                   sweep_range = 6, thrY = 5, thrX_default = 5,
                                   include = "?", dir.exp = c(1, 0, 1, 1, 0),
                                   return_details = TRUE))
  expect_warning(f <- compute_fiss_core(res), "B\\*C\\*D\\*E contain no term")
  fc <- f$fiss_core[["6"]]
  expect_equal(fc$parsim_expression, "~A*E + A*B")
  cl <- fc$classification
  type_of <- function(t, c) cl$type[cl$term_expr == t & cl$condition == c]
  expect_equal(type_of("~A*~B*C*E", "A"), "core")
  expect_equal(type_of("~A*~B*C*E", "E"), "core")
  expect_equal(type_of("~A*~B*C*E", "B"), "peripheral")
  expect_equal(type_of("A*B*D*~E", "E"), "peripheral")
  expect_true(all(cl$type[cl$term_expr == "B*C*D*E" & cl$status != "dontcare"] == "peripheral"))
})

test_that("ctSweepM() and dtSweep() results give a clear error", {
  skip_if_not_installed("QCA")
  data("sample_data", package = "ThSQCA")
  r_m <- suppressWarnings(ctSweepM(sample_data, "Y", c("X1", "X2", "X3"),
                                   sweep_list = list(X1 = 6:7), thrY = 7,
                                   thrX_default = 7, include = "?",
                                   dir.exp = c(1, 1, 1)))
  expect_error(compute_fiss_core(r_m), "otSweep\\(\\) and ctSweepS\\(\\) only")
  r_d <- suppressWarnings(dtSweep(sample_data, "Y", c("X1", "X2", "X3"),
                                  sweep_list_X = list(X1 = 7, X2 = 7, X3 = 7),
                                  sweep_range_Y = 7, include = "?",
                                  dir.exp = c(1, 1, 1)))
  expect_error(compute_fiss_core(r_d), "otSweep\\(\\) and ctSweepS\\(\\) only")
})

test_that("condition names with dots, underscores and shared prefixes are parsed exactly", {
  conds <- c("A", "B", "A.B", "C_1", "X1", "X10")
  st <- function(term) unname(term_status(term, conds))
  expect_equal(st("A*B"),      c("present", "present", "dontcare", "dontcare", "dontcare", "dontcare"))
  expect_equal(st("A.B*~C_1"), c("dontcare", "dontcare", "present", "absent", "dontcare", "dontcare"))
  expect_equal(st("~A.B*B"),   c("dontcare", "present", "absent", "dontcare", "dontcare", "dontcare"))
  expect_equal(st("X1*~X10"),  c("dontcare", "dontcare", "dontcare", "dontcare", "present", "absent"))
  # A parsimonious term "A" is not contained in "A.B*C_1".
  cl <- classify_term_fiss("A.B*C_1", list(c("A", "A.B")), conds)
  expect_equal(cl$type[cl$condition == "A.B"], "core")
  expect_equal(cl$type[cl$condition == "A"], "dontcare")
  expect_equal(cl$type[cl$condition == "C_1"], "peripheral")
})
