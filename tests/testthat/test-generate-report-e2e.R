# End-to-end generate_report() regression test (v2.0.4).
# Fixture (.intermediate_construct_data) lives in helper-fixtures.R.
#
# This runs the ACTUAL generate_report() pipeline and inspects the emitted
# Markdown, rather than calling the internal extractors with hand-picked
# arguments. It exists because an earlier extractor-level test assumed
# generate_report() passed the intermediate IC to extract_all_metrics(), which
# was false for the "Solution Fit" and "Cross-Threshold Comparison" sections
# (they passed sol$IC, the parsimonious solution). Those sections therefore
# reported the parsimonious fit under the displayed intermediate formula. This
# test locks in the corrected, self-consistent report.

.read_report <- function(res, ...) {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  suppressWarnings(generate_report(res, output_file = tmp, format = "full",
                                   include_chart = FALSE, ...))
  readLines(tmp)
}

# Pull "0.911" style number out of a "| Label | 0.911 |" row that contains `key`.
.report_metric <- function(txt, key) {
  line <- grep(key, txt, value = TRUE)[1]
  if (is.na(line)) return(NA_real_)
  nums <- regmatches(line, gregexpr("[0-9]+\\.[0-9]+", line))[[1]]
  if (length(nums) == 0) return(NA_real_)
  as.numeric(nums[length(nums)])
}

test_that("generate_report Solution Fit reports the intermediate fit, not the parsimonious fit", {
  skip_if_not_installed("QCA")
  d <- .intermediate_construct_data()
  c3 <- c("A", "B", "C")
  emptyX <- stats::setNames(numeric(0), character(0))

  # Ground truth: the intermediate solution's own fit.
  dd <- d; dd$Y <- as.integer(d$Y >= 5)
  tt <- QCA::truthTable(dd, outcome = "Y", conditions = c3,
                        incl.cut = 0.80, n.cut = 1, complete = FALSE)
  sol <- QCA::minimize(tt, include = "?", dir.exp = c(A = 1, B = 1, C = 1),
                       details = TRUE)
  isol_inclS <- sol$i.sol$C1P1$IC$sol.incl.cov$inclS[1]
  isol_covS  <- sol$i.sol$C1P1$IC$sol.incl.cov$covS[1]
  interm_expr <- paste(sol$i.sol$C1P1$solution[[1]], collapse = "*")  # "A*B*C"

  # Run the report under BOTH extract modes. The residual bug reports reproduced
  # with extract_mode = "all"; generate_report reads the stored minimize object
  # (mode-independent), so both must be correct.
  for (mode in c("all", "first")) {
    res <- otSweep(dat = d, outcome = "Y", conditions = c3, sweep_range = 5,
                   thrX = emptyX, pre_calibrated = c3,
                   dir.exp = c(A = 1, B = 1, C = 1), include = "?",
                   incl.cut = 0.80, n.cut = 1, extract_mode = mode,
                   return_details = TRUE)
    txt <- .read_report(res)

    # "Solution Fit" section: Consistency / Coverage must be the intermediate fit.
    rep_inclS <- .report_metric(txt, "Consistency \\(inclS\\)")
    rep_covS  <- .report_metric(txt, "Coverage \\(covS\\)")
    expect_equal(rep_inclS, isol_inclS, tolerance = 1e-3)
    expect_equal(rep_covS,  isol_covS,  tolerance = 1e-3)

    # The Per-Term Metrics row must describe the intermediate term (A*B*C), not a
    # parsimonious term (A*B / A*C / B*C).
    perterm_line <- grep("\\|.*\\\\?\\*.*\\|.*[0-9]\\.[0-9]", txt, value = TRUE)
    expect_true(any(grepl("A.*B.*C", perterm_line)))

    # Cross-Threshold Comparison row must also carry the intermediate fit.
    cross_line <- grep("thrY=5|thrY = 5", txt, value = TRUE)
    cross_line <- cross_line[grepl("[0-9]\\.[0-9]", cross_line)]
    expect_true(length(cross_line) >= 1)
    cross_nums <- as.numeric(regmatches(cross_line[1],
                               gregexpr("[0-9]+\\.[0-9]+", cross_line[1]))[[1]])
    expect_true(any(abs(cross_nums - isol_covS) < 1e-3))
    # And must NOT still be reporting the parsimonious aggregate.
    parsim_covS <- tryCatch(sol$IC$overall$sol.incl.cov$covS[1],
                            error = function(e) NA_real_)
    if (!is.na(parsim_covS) && abs(parsim_covS - isol_covS) > 1e-3) {
      expect_false(any(abs(cross_nums - parsim_covS) < 1e-4))
    }
  }
})

test_that("generate_report Summary Table and Solution Fit agree (self-consistency)", {
  skip_if_not_installed("QCA")
  d <- .intermediate_construct_data()
  c3 <- c("A", "B", "C")
  emptyX <- stats::setNames(numeric(0), character(0))
  res <- otSweep(dat = d, outcome = "Y", conditions = c3, sweep_range = 5,
                 thrX = emptyX, pre_calibrated = c3,
                 dir.exp = c(A = 1, B = 1, C = 1), include = "?",
                 incl.cut = 0.80, n.cut = 1, extract_mode = "all",
                 return_details = TRUE)
  txt <- .read_report(res)

  fit_covS <- .report_metric(txt, "Coverage \\(covS\\)")
  summ_line <- grep("A.*B.*C", grep("\\| 5 \\||thrY", txt, value = TRUE), value = TRUE)
  if (length(summ_line) == 0) summ_line <- grep("A\\\\?\\*B\\\\?\\*C", txt, value = TRUE)
  expect_true(length(summ_line) >= 1)
  summ_nums <- as.numeric(regmatches(summ_line[1],
                            gregexpr("[0-9]+\\.[0-9]+", summ_line[1]))[[1]])
  expect_true(any(abs(summ_nums - fit_covS) < 1e-3))
})
