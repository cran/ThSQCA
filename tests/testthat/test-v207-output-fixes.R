# Output-cosmetics regression tests (v2.0.7).
#
# 1. Sweeps must not print stray console output for "No solution" cells.
# 2. Reports must show the user's outcome name, not the internal column "Y".
# 3. print()/summary() headers use the ThS-QCA labels (OTS / CTS / DTS).

test_that("quiet_try() returns a try-error and prints nothing", {
  noisy <- function() {
    cat("\n")
    message("stray message")
    stop("boom")
  }
  out <- capture.output(res <- quiet_try(noisy()))
  expect_length(out, 0L)
  expect_s3_class(res, "try-error")

  # A successful call passes its value through unchanged.
  out2 <- capture.output(val <- quiet_try(42))
  expect_length(out2, 0L)
  expect_identical(val, 42)

  # The output diversion is always released.
  expect_identical(sink.number(), 0L)
})

test_that("quiet_try() passes warnings on, and user handlers can print them", {
  # 1. The warning is still signalled (e.g. QCA's "values of 0.5" warning).
  expect_warning(quiet_try({ warning("w1"); 1 }), "w1")

  # 2. A handler that writes to the console (as a logging script would) is
  #    called after the diversion is closed, so its output is visible, while
  #    the wrapped call's own output stays hidden.
  out <- capture.output(
    withCallingHandlers(
      quiet_try({ cat("hidden\n"); warning("w2"); 1 }),
      warning = function(w) {
        cat("[W]", conditionMessage(w), "\n")
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_true(any(grepl("[W] w2", out, fixed = TRUE)))
  expect_false(any(grepl("hidden", out)))
  expect_identical(sink.number(), 0L)
})

test_that("quiet_try() drops only QCA's per-cell 0.5 warning", {
  expect_no_warning(
    quiet_try({ warning("Fuzzy causal conditions should not have values of 0.5 in the data."); 1 })
  )
  expect_warning(quiet_try({ warning("something else"); 1 }), "something else")
  expect_warning(
    quiet_try({ warning("Fuzzy causal conditions should not have values of 0.5 in the data."); 1 },
              drop_pattern = NULL),
    "0.5"
  )
})

test_that("a pre_calibrated variable with exact 0.5 warns once, naming the variable", {
  skip_if_not_installed("QCA")
  set.seed(3)
  d <- data.frame(X1 = sample(0:10, 60, TRUE),
                  X2 = sample(0:10, 60, TRUE),
                  Y  = sample(0:10, 60, TRUE))
  d$F1 <- pmin(1, pmax(0, (d$X1 - 2.5) / 6))
  d$F1[1:5] <- 0.5
  n_msgs <- 0L
  msgs <- character()
  res <- withCallingHandlers(
    otSweep(dat = d, outcome = "Y", conditions = c("F1", "X2"),
            sweep_range = 5:8, thrX = c(X2 = 7), pre_calibrated = "F1"),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  half <- grep("0\\.5", msgs, value = TRUE)
  expect_length(half, 1L)
  expect_match(half, "'F1'")
  expect_match(half, "5 case\\(s\\)")
  expect_false(any(grepl("should not have values of 0.5", msgs)))
})

test_that("otSweep prints no stray output for a No-solution cell", {
  skip_if_not_installed("QCA")
  set.seed(1)
  d <- data.frame(X1 = sample(0:10, 30, TRUE),
                  X2 = sample(0:10, 30, TRUE),
                  X3 = sample(0:10, 30, TRUE),
                  Y  = rep(0, 30))
  expect_output(
    res <- suppressWarnings(
      otSweep(dat = d, outcome = "Y", conditions = c("X1", "X2", "X3"),
              sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
              return_details = FALSE)
    ),
    NA
  )
  expect_equal(nrow(res), 3L)
})

.renamed_fixture <- function() {
  d <- .intermediate_construct_data()
  names(d)[names(d) == "Y"] <- "LOY"
  d
}

.report_lines <- function(res, format) {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  suppressWarnings(generate_report(res, output_file = tmp, format = format,
                                   include_chart = FALSE))
  readLines(tmp)
}

test_that("reports show the outcome name instead of the internal 'Y'", {
  skip_if_not_installed("QCA")
  d <- .renamed_fixture()
  c3 <- c("A", "B", "C")
  res <- otSweep(dat = d, outcome = "LOY", conditions = c3, sweep_range = 5,
                 thrX = stats::setNames(numeric(0), character(0)),
                 pre_calibrated = c3, dir.exp = c(A = 1, B = 1, C = 1),
                 include = "?", incl.cut = 0.80, n.cut = 1,
                 return_details = TRUE)

  for (fmt in c("full", "simple")) {
    txt <- .report_lines(res, fmt)
    expect_false(any(grepl("->\\s*Y\\s*$", txt)), info = fmt)
    expect_true(any(grepl("->\\s*LOY", txt)), info = fmt)
    expect_true(any(grepl("outcome = \"LOY\"", txt, fixed = TRUE)), info = fmt)
    expect_true(any(grepl("conditions = c(\"A\", \"B\", \"C\")", txt, fixed = TRUE)),
                info = fmt)
  }
})

test_that("relabel_outcome_lines() only touches the trailing arrow", {
  x <- c("M1:    A*B -> Y ", "        M1  0.981  0.981  0.403 ", "M1: A -> ~Y",
         "Yes -> Yes")
  expect_equal(relabel_outcome_lines(x, "LOY")[1], "M1:    A*B -> LOY ")
  expect_equal(relabel_outcome_lines(x, "~LOY")[3], "M1: A -> ~LOY")
  expect_equal(relabel_outcome_lines(x, "LOY")[c(2, 4)], x[c(2, 4)])
  expect_identical(relabel_outcome_lines(x, "Y"), x)
})

test_that("print() and summary() headers use ThS-QCA labels", {
  skip_if_not_installed("QCA")
  data(sample_data)
  res <- otSweep(dat = sample_data, outcome = "Y",
                 conditions = c("X1", "X2", "X3"), sweep_range = 6:8,
                 thrX = c(X1 = 7, X2 = 7, X3 = 7))
  expect_output(print(res), "OTS \\(Outcome Threshold Sweep\\)")
  expect_output(summary(res), "OTS Summary")

  cts <- ctSweepS(dat = sample_data, outcome = "Y",
                  conditions = c("X1", "X2", "X3"), sweep_var = "X3",
                  sweep_range = 6:8, thrY = 7, thrX_default = 7)
  expect_output(summary(cts), "CTS \\(single\\) Summary")

  out <- capture.output(print(res), summary(res))
  expect_false(any(grepl("OTS-QCA|CTS-QCA|MCTS-QCA|DTS-QCA", out)))
})

test_that("verification_snippet() reproduces the solution type that was used", {
  base <- list(outcome = "LOY", conditions = c("QUA", "SER"), incl.cut = 0.85)

  cx <- verification_snippet(c(base, list(include = "", dir.exp = NULL)))
  expect_true("sol <- minimize(tt)" %in% cx)

  ps <- verification_snippet(c(base, list(include = "?", dir.exp = NULL)))
  expect_true("sol <- minimize(tt, include = \"?\")" %in% ps)

  im <- verification_snippet(c(base, list(include = "?", dir.exp = c(QUA = 1, SER = 1))))
  expect_true("sol <- minimize(tt, include = \"?\", dir.exp = c(1, 1))" %in% im)

  expect_true(any(grepl("incl.cut = 0.85", cx, fixed = TRUE)))
  expect_false(any(grepl("\\.\\.\\.", im)))
})

test_that("the Necessity table in a full report has no leading spaces in condition names", {
  skip_if_not_installed("QCA")
  data(sample_data)
  res <- suppressWarnings(
    otSweep(dat = sample_data, outcome = "Y", conditions = c("X1", "X2", "X3"),
            sweep_range = 7, thrX = c(X1 = 7, X2 = 7, X3 = 7))
  )
  txt <- .report_lines(res, "full")
  i <- grep("^#### Necessity Analysis", txt)
  skip_if(length(i) == 0L, "no necessity table produced")
  rows <- txt[i[1] + 1:14]
  rows <- rows[grepl("^\\|", rows) & grepl("X[123]", rows)]
  expect_gt(length(rows), 0L)
  expect_false(any(grepl("^\\|\\s{2,}", rows)))
})
