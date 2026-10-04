# Regression test: format = "simple" must print numeric inclS / covS under each
# threshold heading, also when otSweep() was run WITHOUT dir.exp (no
# intermediate solution, so sol$i.sol is empty and the fit is in sol$IC).
# Before the fix, write_simple_report() read sol$i.sol$C1P1$IC directly and
# printed "*inclS = N/A, covS = N/A*" in that case.

.read_simple_report <- function(res) {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  suppressWarnings(generate_report(res, output_file = tmp, format = "simple",
                                   include_chart = FALSE))
  readLines(tmp)
}

test_that("simple report shows numeric fit when dir.exp is not given", {
  skip_if_not_installed("QCA")
  d <- .intermediate_construct_data()
  c3 <- c("A", "B", "C")
  emptyX <- stats::setNames(numeric(0), character(0))

  res <- otSweep(dat = d, outcome = "Y", conditions = c3, sweep_range = 5,
                 thrX = emptyX, pre_calibrated = c3,
                 incl.cut = 0.80, n.cut = 1, return_details = TRUE)
  txt <- .read_simple_report(res)

  fit_lines <- grep("^\\*inclS = ", txt, value = TRUE)
  expect_true(length(fit_lines) >= 1)
  expect_false(any(grepl("N/A", fit_lines)))
  expect_true(all(grepl("^\\*inclS = [0-9.]+, covS = [0-9.]+\\*$", fit_lines)))
})

test_that("simple report still shows the intermediate fit when dir.exp is given", {
  skip_if_not_installed("QCA")
  d <- .intermediate_construct_data()
  c3 <- c("A", "B", "C")
  emptyX <- stats::setNames(numeric(0), character(0))

  dd <- d; dd$Y <- as.integer(d$Y >= 5)
  tt <- QCA::truthTable(dd, outcome = "Y", conditions = c3,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "?", dir.exp = c(A = 1, B = 1, C = 1),
                       details = TRUE)
  isol_inclS <- round(sol$i.sol$C1P1$IC$sol.incl.cov$inclS[1], 3)
  isol_covS  <- round(sol$i.sol$C1P1$IC$sol.incl.cov$covS[1], 3)

  res <- otSweep(dat = d, outcome = "Y", conditions = c3, sweep_range = 5,
                 thrX = emptyX, pre_calibrated = c3,
                 dir.exp = c(A = 1, B = 1, C = 1), include = "?",
                 incl.cut = 0.80, n.cut = 1, return_details = TRUE)
  txt <- .read_simple_report(res)

  fit_lines <- grep("^\\*inclS = ", txt, value = TRUE)
  expect_true(length(fit_lines) >= 1)
  expect_equal(fit_lines[1],
               paste0("*inclS = ", isol_inclS, ", covS = ", isol_covS, "*"))
})
