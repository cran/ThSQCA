# Systematic audit 1: option matrix.
#
# Every sweep function x solution type x extract_mode x outcome polarity is run
# on sample_data, and each result is rendered as a "full" report, a "simple"
# report with charts, and a "simple" report without charts. The reports are
# checked by .audit_check_report() / .audit_check_summary() (helper-audit.R).
#
# A failure lists every offending configuration, so one run shows the full
# extent of a problem. Failures are informative, not necessarily bugs in the
# test: read the message, then check the named configuration by hand.

.audit_matrix <- function(sweeper) {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  grid <- expand.grid(
    spec = c("complex", "parsimonious", "intermediate"),
    extract_mode = c("first", "all", "essential"),
    outcome = c("Y", "~Y"),
    stringsAsFactors = FALSE)

  problems <- character(0)
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    tag <- sprintf("[%s | %s | %s | %s]", sweeper, g$spec, g$extract_mode, g$outcome)
    res <- .audit_run(sweeper, g$spec, g$extract_mode, g$outcome, dat)
    if (.audit_is_error(res)) {
      problems <- c(problems, paste(tag, "sweep failed:", res$message))
      next
    }
    sp <- .audit_check_summary(res, g$extract_mode)
    if (length(sp)) problems <- c(problems, paste(tag, sp))

    variants <- list(
      list(format = "full",   include_chart = TRUE,  dat = dat),
      list(format = "simple", include_chart = TRUE),
      list(format = "simple", include_chart = FALSE))
    for (v in variants) {
      rep <- do.call(.audit_report, c(list(res = res), v))
      pr <- .audit_check_report(rep, res, v$format)
      if (length(pr)) {
        problems <- c(problems, paste0(tag, "[", v$format,
                                       if (!v$include_chart) ", no chart" else "",
                                       "] ", pr))
      }
    }
  }
  expect_true(length(problems) == 0L,
              info = paste(c("", problems), collapse = "\n"))
}

test_that("audit matrix: otSweep reports are well-formed", .audit_matrix("otSweep"))
test_that("audit matrix: ctSweepS reports are well-formed", .audit_matrix("ctSweepS"))
test_that("audit matrix: ctSweepM reports are well-formed", .audit_matrix("ctSweepM"))
test_that("audit matrix: dtSweep reports are well-formed", .audit_matrix("dtSweep"))

# ---------------------------------------------------------------------------
# Presentation options that change code paths but not the numbers.
# ---------------------------------------------------------------------------

test_that("audit options: symbol sets, chart levels, note styles and languages", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  res <- .audit_run("otSweep", "intermediate")
  expect_false(.audit_is_error(res))

  grid <- expand.grid(
    format = c("full", "simple"),
    chart_symbol_set = c("unicode", "ascii", "latex"),
    chart_level = c("term", "summary"),
    solution_note_style = c("simple", "detailed"),
    solution_note_lang = c("en", "ja"),
    stringsAsFactors = FALSE)

  problems <- character(0)
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    rep <- .audit_report(res, format = g$format, include_chart = TRUE,
                         chart_symbol_set = g$chart_symbol_set,
                         chart_level = g$chart_level,
                         solution_note_style = g$solution_note_style,
                         solution_note_lang = g$solution_note_lang)
    pr <- .audit_check_report(rep, res, g$format)
    if (length(pr)) {
      problems <- c(problems, paste0("[", paste(unlist(g), collapse = " | "), "] ", pr))
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("audit options: include_raw_output and solution_note toggles", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  res <- .audit_run("otSweep", "complex")
  expect_false(.audit_is_error(res))
  problems <- character(0)
  for (raw in c(TRUE, FALSE)) for (note in c(TRUE, FALSE)) for (fmt in c("full", "simple")) {
    rep <- .audit_report(res, format = fmt, include_raw_output = raw,
                         solution_note = note)
    pr <- .audit_check_report(rep, res, fmt)
    if (length(pr)) {
      problems <- c(problems, paste0("[", fmt, " raw=", raw, " note=", note, "] ", pr))
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

# ---------------------------------------------------------------------------
# Fiss core/peripheral report path (otSweep and ctSweepS only).
# ---------------------------------------------------------------------------

test_that("audit fiss: reports with include_fiss_core = TRUE are well-formed", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  problems <- character(0)
  for (sweeper in c("otSweep", "ctSweepS")) {
    res <- .audit_run(sweeper, "intermediate")
    if (.audit_is_error(res)) {
      problems <- c(problems, paste0("[", sweeper, "] sweep failed: ", res$message))
      next
    }
    fc <- tryCatch(suppressWarnings(compute_fiss_core(res, conditions = c("X1", "X2", "X3"))),
                   error = function(e) e)
    if (inherits(fc, "error")) {
      problems <- c(problems, paste0("[", sweeper, "] compute_fiss_core failed: ",
                                     conditionMessage(fc)))
      next
    }
    for (fmt in c("full", "simple")) {
      rep <- .audit_report(fc, format = fmt, include_fiss_core = TRUE)
      pr <- .audit_check_report(rep, fc, fmt)
      if (length(pr)) {
        problems <- c(problems, paste0("[", sweeper, " fiss ", fmt, "] ", pr))
      }
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})
