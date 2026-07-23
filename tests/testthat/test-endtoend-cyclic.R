# End-to-end sweep-function regression tests built on a CONSTRUCTED data set
# with a guaranteed structure (v2.0.4).
#
# The data realize the classic cyclic prime-implicant chart on three conditions:
# positive configurations {000, 001, 010, 101, 110, 111}, negative {011, 100}.
# The coverage matrix is cyclic, so Boolean minimization has EXACTLY two
# equal-cost minimum covers,
#   M1 = ~A*~B + A*C + B*~C      M2 = ~A*~C + ~B*C + A*B,
# by construction (not by luck of a random seed). Conditions carry fuzzy
# memberships (jittered corners), so M1's and M2's memberships differ on the
# positive cases and the per-solution covS differs from the overall (union)
# aggregate. This is the exact pipeline shape of the original bug report
# (fuzzy conditions, outcome binarized by the sweep threshold), which makes
# these tests end-to-end: they check the numbers the user actually sees in the
# sweep table, not just the internal extractor.

.cyclic_ms_data <- function() {
  structure(list(
    A = c(0.216, 0.22, 0.116, 0.183, 0.143, 0.185, 0.146, 0.16, 0.215,
          0.915, 0.842, 0.904, 0.771, 0.803, 0.915, 0.923, 0.912, 0.872,
          0.076, 0.19, 0.178, 0.861, 0.906, 0.8),
    B = c(0.203, 0.173, 0.153, 0.22, 0.111, 0.144, 0.792, 0.928, 0.921,
          0.188, 0.2, 0.132, 0.868, 0.831, 0.84, 0.925, 0.869, 0.823,
          0.797, 0.812, 0.852, 0.113, 0.203, 0.181),
    C = c(0.188, 0.092, 0.175, 0.92, 0.927, 0.789, 0.083, 0.152, 0.132,
          0.88, 0.771, 0.903, 0.076, 0.226, 0.139, 0.825, 0.834, 0.896,
          0.878, 0.927, 0.892, 0.108, 0.077, 0.092),
    Y = c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
          2, 2, 2, 2, 2, 2)),
    row.names = c(NA, -24L), class = "data.frame")
}

# Hand computation of solution coverage against fuzzy condition columns and a
# crisp outcome vector, for a solution given as a "+"-joined expression string.
.covS_of_expression <- function(expr, qd, y) {
  terms <- trimws(unlist(strsplit(gsub("M[0-9]+:|->.*$", "", expr), "[+;]")))
  terms <- terms[nzchar(terms)]
  terms <- unique(terms[vapply(terms, function(t) {
    all(sub("^~", "", strsplit(t, "\\*")[[1]]) %in% names(qd))
  }, logical(1))])
  if (length(terms) == 0) return(NA_real_)
  ev <- function(t) {
    l <- strsplit(t, "\\*")[[1]]
    v <- sapply(l, function(p) {
      if (startsWith(p, "~")) 1 - qd[[substring(p, 2)]] else qd[[p]]
    })
    apply(as.matrix(v), 1, min)
  }
  X <- apply(as.matrix(sapply(terms, ev)), 1, max)
  sum(pmin(X, y)) / sum(y)
}

test_that("cyclic construction yields the guaranteed two minimal solutions", {
  skip_if_not_installed("QCA")
  d <- .cyclic_ms_data()
  dd <- d[c("A", "B", "C")]
  dd$Y <- as.integer(d$Y >= 5)
  tt <- QCA::truthTable(dd, outcome = "Y", conditions = c("A", "B", "C"),
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "", details = TRUE)
  # By construction (cyclic PI chart): exactly two minimal solutions.
  expect_equal(length(sol$solution), 2L)
  # And their fits genuinely differ from the overall aggregate.
  ind1 <- sol$IC$individual[[1]]$sol.incl.cov$covS
  ov   <- sol$IC$overall$sol.incl.cov$covS
  expect_gt(abs(ov - ind1), 2e-3)
})

test_that("otSweep end-to-end: displayed covS equals the displayed formula's covS", {
  skip_if_not_installed("QCA")
  d <- .cyclic_ms_data()
  emptyX <- stats::setNames(numeric(0), character(0))
  y <- as.integer(d$Y >= 5)

  r_first <- otSweep(dat = d, outcome = "Y", conditions = c("A", "B", "C"),
                     sweep_range = 5, thrX = emptyX,
                     pre_calibrated = c("A", "B", "C"),
                     include = "", incl.cut = 0.80, n.cut = 1,
                     extract_mode = "first", return_details = FALSE)
  hand_first <- .covS_of_expression(r_first$expression[1], d[c("A", "B", "C")], y)
  expect_equal(r_first$covS[1], hand_first, tolerance = 2e-3)

  r_all <- otSweep(dat = d, outcome = "Y", conditions = c("A", "B", "C"),
                   sweep_range = 5, thrX = emptyX,
                   pre_calibrated = c("A", "B", "C"),
                   include = "", incl.cut = 0.80, n.cut = 1,
                   extract_mode = "all", return_details = FALSE)
  # "all" reports the aggregate of all listed solutions: the covS must equal the
  # coverage of the union of all displayed terms.
  hand_all <- .covS_of_expression(r_all$expression[1], d[c("A", "B", "C")], y)
  expect_equal(r_all$covS[1], hand_all, tolerance = 2e-3)
  # And first != all on this construction (per-solution vs union).
  expect_gt(abs(r_all$covS[1] - r_first$covS[1]), 2e-3)
})

test_that("ctSweepS end-to-end shares the corrected extraction", {
  skip_if_not_installed("QCA")
  d <- .cyclic_ms_data()
  y <- as.integer(d$Y >= 5)
  r <- ctSweepS(dat = d, outcome = "Y", conditions = c("A", "B", "C"),
                sweep_var = "A", sweep_range = c(0.5), thrY = 5,
                pre_calibrated = c("B", "C"),
                include = "", incl.cut = 0.80, n.cut = 1,
                extract_mode = "first", return_details = FALSE)
  # Oracle must use ctSweepS's own calibration: A binarized at the swept 0.5.
  qd <- d[c("A", "B", "C")]
  qd$A <- as.numeric(qd$A >= 0.5)
  hand <- .covS_of_expression(r$expression[1], qd, y)
  expect_equal(r$covS[1], hand, tolerance = 2e-3)
})

test_that("multiple INTERMEDIATE solutions report a fit, not NA (regression)", {
  skip_if_not_installed("QCA")
  # Same cyclic construction, but with dir.exp: the intermediate solution itself
  # has several minimal solutions, so its IC is split into $individual/$overall
  # rather than a flat $sol.incl.cov. Reading only the flat slot returned NA.
  d <- .cyclic_ms_data()
  c3 <- c("A", "B", "C")
  dd <- d[c3]; dd$Y <- as.integer(d$Y >= 5)
  tt <- QCA::truthTable(dd, outcome = "Y", conditions = c3,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "?", dir.exp = c(A = 1, B = 1, C = 1),
                       details = TRUE)

  isol <- sol$i.sol$C1P1
  skip_if(length(isol$solution) < 2)          # precondition: several intermediates
  expect_null(isol$IC$sol.incl.cov)           # precondition: split IC shape
  ind1 <- isol$IC$individual[[1]]$sol.incl.cov
  ov   <- isol$IC$overall$sol.incl.cov

  info <- ThSQCA:::qca_extract(sol, "first")
  expect_false(is.na(info$covS))              # the regression: this was NA
  expect_false(is.na(info$inclS))
  expect_equal(info$covS,  ind1$covS[1],  tolerance = 1e-6)
  expect_equal(info$inclS, ind1$inclS[1], tolerance = 1e-6)

  # "all" summarizes across the intermediate solutions, so it uses the aggregate.
  info_all <- ThSQCA:::qca_extract(sol, "all")
  expect_equal(info_all$covS, ov$covS[1], tolerance = 1e-6)

  # Chart extractors must also return the first intermediate solution's fit and
  # its per-term table (not the parsimonious ones).
  cm <- ThSQCA:::extract_solution_metrics_for_chart(sol, 1)
  expect_equal(cm$covS, ind1$covS[1], tolerance = 1e-6)
  pm <- ThSQCA:::extract_path_metrics_for_chart(sol, 1)
  expect_false(is.null(pm))
  expect_equal(nrow(pm), length(isol$solution[[1]]))

  # And end to end through the sweep table.
  emptyX <- stats::setNames(numeric(0), character(0))
  r <- otSweep(dat = d, outcome = "Y", conditions = c3, sweep_range = 5,
               thrX = emptyX, pre_calibrated = c3,
               dir.exp = c(A = 1, B = 1, C = 1), include = "?",
               incl.cut = 0.80, n.cut = 1, return_details = FALSE)
  expect_false(is.na(r$covS[1]))
  expect_equal(r$covS[1], ind1$covS[1], tolerance = 1e-6)
})

test_that("a solution with unreadable fit warns instead of returning a silent NA", {
  # Mock a result whose solution exists but whose IC carries no fit measures.
  sol <- list(solution = list(c("A", "B")), IC = list())
  expect_warning(info <- ThSQCA:::qca_extract(sol, "first"),
                 "could not be read")
  expect_true(is.na(info$covS))
  expect_equal(info$expression, "A + B")   # the expression is still reported
})
