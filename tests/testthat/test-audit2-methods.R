# Audit stage 2, part 5: every exported function and S3 method that is not
# covered by the report audits: print / summary methods, Fiss functions,
# cross-threshold charts, solution notes, EPI identification and the term
# formatting utilities.

test_that("methods: print() and summary() run cleanly for every sweep function", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  bad <- character(0)
  for (sw in c("otSweep", "ctSweepS", "ctSweepM", "dtSweep")) for (spec in c("complex", "intermediate")) {
    res <- .audit_run(sw, spec)
    tag <- paste(sw, spec)
    if (.audit_is_error(res)) { bad <- c(bad, paste(tag, "sweep failed:", res$message)); next }
    out <- tryCatch(utils::capture.output(print(res)), error = function(e) e)
    if (inherits(out, "error")) bad <- c(bad, paste(tag, "print failed:", conditionMessage(out)))
    else if (length(.audit_anomalies(out))) bad <- c(bad, paste(tag, "print shows:", .audit_anomalies(out)[1]))
    out <- tryCatch(utils::capture.output(summary(res)), error = function(e) e)
    if (inherits(out, "error")) bad <- c(bad, paste(tag, "summary failed:", conditionMessage(out)))
    else if (length(.audit_anomalies(out))) bad <- c(bad, paste(tag, "summary shows:", .audit_anomalies(out)[1]))
  }
  expect_true(length(bad) == 0L, info = paste(c("", bad), collapse = "\n"))
})

test_that("methods: Fiss functions run cleanly and refuse unsupported input", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  conds <- c("X1", "X2", "X3")
  bad <- character(0)
  for (sw in c("otSweep", "ctSweepS")) {
    res <- .audit_run(sw, "intermediate")
    if (.audit_is_error(res)) { bad <- c(bad, paste(sw, "sweep failed:", res$message)); next }
    fc <- tryCatch(suppressWarnings(compute_fiss_core(res, conditions = conds)), error = function(e) e)
    if (inherits(fc, "error")) { bad <- c(bad, paste(sw, "compute_fiss_core failed:", conditionMessage(fc))); next }
    out <- tryCatch(utils::capture.output(print_fiss_summary(fc)), error = function(e) e)
    if (inherits(out, "error")) bad <- c(bad, paste(sw, "print_fiss_summary failed:", conditionMessage(out)))
    else if (length(.audit_anomalies(out))) bad <- c(bad, paste(sw, "print_fiss_summary shows:", .audit_anomalies(out)[1]))
    for (ss in c("unicode", "ascii", "latex")) for (lg in c("en", "ja")) {
      ch <- tryCatch(generate_fiss_chart(fc, conditions = conds, symbol_set = ss, language = lg),
                     error = function(e) e)
      tag <- paste(sw, "fiss chart", ss, lg)
      if (inherits(ch, "error")) { bad <- c(bad, paste(tag, "failed:", conditionMessage(ch))); next }
      txt <- paste(ch, collapse = "\n")
      if (!nzchar(txt)) bad <- c(bad, paste(tag, "is empty"))
      else if (length(.audit_anomalies(strsplit(txt, "\n")[[1]]))) {
        bad <- c(bad, paste(tag, "shows:", .audit_anomalies(strsplit(txt, "\n")[[1]])[1]))
      }
    }
  }
  expect_true(length(bad) == 0L, info = paste(c("", bad), collapse = "\n"))

  # Unsupported input must fail with a clear error, not silently.
  expect_error(compute_fiss_core(.audit_run("ctSweepM", "intermediate")), "ctSweepM|dtSweep|supports")
  expect_error(compute_fiss_core(.audit_run("dtSweep", "intermediate")), "ctSweepM|dtSweep|supports")
  expect_error(compute_fiss_core(.audit_run("otSweep", "complex")), "include")
  expect_error(compute_fiss_core(.audit_run("otSweep", "parsimonious")), "dir.exp")
})

test_that("methods: cross-threshold and multi-solution charts run cleanly", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  bad <- character(0)
  for (spec in c("complex", "intermediate")) {
    res <- .audit_run("otSweep", spec)
    for (lv in c("term", "summary")) for (ss in c("unicode", "ascii", "latex")) for (lg in c("en", "ja")) {
      ch <- tryCatch(generate_cross_threshold_chart(res, chart_level = lv, symbol_set = ss, language = lg),
                     error = function(e) e)
      tag <- paste("cross-threshold", spec, lv, ss, lg)
      if (inherits(ch, "error")) { bad <- c(bad, paste(tag, "failed:", conditionMessage(ch))); next }
      lines <- strsplit(paste(ch, collapse = "\n"), "\n")[[1]]
      if (length(.audit_anomalies(lines))) bad <- c(bad, paste(tag, "shows:", .audit_anomalies(lines)[1]))
    }
  }
  sols <- list(c("X1*X2", "X3"), c("X1*X2", "~X3"))
  for (ss in c("unicode", "ascii", "latex")) for (lg in c("en", "ja")) {
    ch <- tryCatch(config_chart_multi_solutions(sols, symbol_set = ss, language = lg, show_epi = TRUE),
                   error = function(e) e)
    if (inherits(ch, "error")) bad <- c(bad, paste("multi-solution chart", ss, lg, "failed:", conditionMessage(ch)))
  }
  expect_true(length(bad) == 0L, info = paste(c("", bad), collapse = "\n"))
})

test_that("methods: solution notes and EPI identification behave", {
  expect_equal(generate_solution_note(1), "")
  for (style in c("simple", "detailed")) for (lg in c("en", "ja")) {
    nt <- generate_solution_note(2, epi_list = c("A*B"), style = style, language = lg)
    expect_true(is.character(nt) && length(nt) == 1L && nzchar(nt), info = paste(style, lg))
  }
  sols <- list(c("A*B", "C"), c("A*B", "D"))
  ep <- identify_epi(sols)
  expect_equal(ep$n_solutions, 2L)
  expect_true("A*B" %in% ep$epi)
  expect_false(any(ep$epi %in% ep$spi))
  expect_true(all(ep$epi %in% sols[[1]]) && all(ep$epi %in% sols[[2]]))
  expect_equal(identify_epi(list())$n_solutions, 0L)
})

test_that("methods: term formatting utilities are exact and never drop text", {
  vn <- c("KSP", "KPR", "PRD", "RVT", "RCM")
  expect_equal(format_qca_term("KSPRVTRCM", vn), "KSP*RVT*RCM")
  expect_equal(format_qca_term("~KPRPRD", vn), "~KPR*PRD")
  expect_equal(format_qca_term("KSP*RVT", vn), "KSP*RVT")           # already formatted
  expect_equal(format_qca_solution("KSPRVT + ~KPRPRD + RCM", vn), "KSP*RVT + ~KPR*PRD + RCM")
  # names that contain one another: the longest name must win
  vn2 <- c("X1", "X10", "X2")
  expect_equal(format_qca_term("X10X2", vn2), "X10*X2")
  expect_equal(format_qca_term("~X1X10", vn2), "~X1*X10")
  # text that cannot be split into known names must not be silently dropped
  out <- format_qca_term("KSPZZZ", vn)
  expect_true(grepl("ZZZ", out, fixed = TRUE), info = paste("result:", out))
  # names containing regular-expression characters are matched literally
  expect_equal(format_qca_term("A.BC", c("A.B", "C")), "A.B*C")
  expect_equal(format_qca_term("AxBC", c("A.B", "C")), "AxBC")
  expect_equal(format_qca_term("A+BC", c("A+B", "C")), "A+B*C")
  et <- extract_terms(c("X1*X2 + X3", "X1*X2 + X1*X3"), c("X1", "X2", "X3"))
  expect_equal(et$n_total, 4L)
  expect_equal(et$n_unique, 3L)
})
