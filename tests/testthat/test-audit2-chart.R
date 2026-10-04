# Audit stage 2, part 3: configuration charts.
#
# A chart that puts a symbol in the wrong cell misleads the reader without any
# error message, so charts are checked cell by cell against the solution.

test_that("chart: condition status is exact for tricky variable names", {
  cases <- list(
    # name-prefix pairs (X1 vs X10)
    c("X1*~X10", "X1", "present"), c("X1*~X10", "X10", "absent"),
    c("X10", "X1", "dontcare"), c("X1", "X10", "dontcare"),
    c("~X1", "X1", "absent"), c("X1*X2", "X3", "dontcare"),
    # dots and underscores are name characters
    c("A.B*C", "A", "dontcare"), c("A.B*C", "A.B", "present"),
    c("~A.B", "A.B", "absent"), c("A*B", "A.B", "dontcare"),
    c("brand_x*~brand_y", "brand_x", "present"),
    c("brand_x*~brand_y", "brand_y", "absent"),
    c("brand_x", "brand", "dontcare"),
    # case matters
    c("a*~b", "a", "present"), c("a*~b", "b", "absent"), c("~a", "A", "dontcare"),
    c("A*~B*C", "C", "present"), c("~A*B", "A", "absent"))
  bad <- character(0)
  for (cs in cases) {
    got <- get_condition_status(cs[1], cs[2])
    if (!identical(got, cs[3])) {
      bad <- c(bad, sprintf("term '%s', condition '%s': got %s, expected %s", cs[1], cs[2], got, cs[3]))
    }
  }
  expect_true(length(bad) == 0L, info = paste(c("", bad), collapse = "\n"))
})

test_that("chart: condition status is exact for Japanese variable names", {
  hinshitsu <- "\u54c1\u8cea"                    # quality
  kakaku    <- "\u4fa1\u683c"                    # price
  kanri     <- "\u7ba1\u7406"                    # management
  manzoku   <- "\u6e80\u8db3"                    # satisfaction
  do        <- "\u5ea6"                          # degree
  cases <- list(
    c(paste0(hinshitsu, "*~", kakaku), hinshitsu, "present"),
    c(paste0(hinshitsu, "*~", kakaku), kakaku, "absent"),
    c(paste0(hinshitsu, "*", kakaku), manzoku, "dontcare"),
    # one name contained in another: must not be confused
    c(paste0(hinshitsu, kanri), hinshitsu, "dontcare"),
    c(paste0("~", hinshitsu, kanri), hinshitsu, "dontcare"),
    c(paste0(manzoku, do), manzoku, "dontcare"),
    c(paste0(hinshitsu, kanri), paste0(hinshitsu, kanri), "present"))
  bad <- character(0)
  for (cs in cases) {
    got <- get_condition_status(cs[1], cs[2])
    if (!identical(got, cs[3])) {
      bad <- c(bad, sprintf("term '%s', condition '%s': got %s, expected %s", cs[1], cs[2], got, cs[3]))
    }
  }
  expect_true(length(bad) == 0L, info = paste(c("", bad), collapse = "\n"))
})

test_that("chart: build_config_matrix and config_chart_from_paths place symbols exactly", {
  sym <- SYMBOL_SETS$ascii
  paths <- c("X1*~X10", "X2", "~X1*X2*X10")
  conds <- c("X1", "X10", "X2")
  mat <- build_config_matrix(paths, conds, sym)
  expect_equal(unname(mat["X1", ]),  c("O", "", "X"))
  expect_equal(unname(mat["X10", ]), c("X", "", "O"))
  expect_equal(unname(mat["X2", ]),  c("", "O", "O"))
  expect_equal(colnames(mat), c("T1", "T2", "T3"))

  chart <- config_chart_from_paths(paths, symbol_set = "ascii", condition_order = conds)
  lines <- strsplit(chart, "\n")[[1]]
  expect_equal(.chart_row_cells(lines, "X1"),  c("O", "", "X"))
  expect_equal(.chart_row_cells(lines, "X10"), c("X", "", "O"))
  expect_equal(.chart_row_cells(lines, "X2"),  c("", "O", "O"))
})

test_that("chart: generated charts match the solutions of real sweeps", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  problems <- character(0)
  for (sw in c("otSweep", "ctSweepS", "ctSweepM", "dtSweep")) {
    for (spec in c("complex", "parsimonious", "intermediate")) for (oc in c("Y", "~Y")) {
      res <- .audit_run(sw, spec, "first", oc, dat)
      tag <- sprintf("[%s | %s | %s]", sw, spec, oc)
      if (.audit_is_error(res)) { problems <- c(problems, paste(tag, "sweep failed:", res$message)); next }
      pr <- .audit_check_chart(res, c("X1", "X2", "X3"))
      if (length(pr)) problems <- c(problems, paste(tag, pr))
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("chart: the three symbol sets and both languages produce well-formed charts", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  res <- .audit_run("otSweep", "intermediate")
  sol <- Filter(Negate(is.null), lapply(res$details, `[[`, "solution"))[[1]]
  bad <- character(0)
  for (ss in c("unicode", "ascii", "latex")) for (lg in c("en", "ja")) for (im in c(TRUE, FALSE)) {
    ch <- tryCatch(generate_config_chart(sol, symbol_set = ss, language = lg, include_metrics = im),
                   error = function(e) e)
    tag <- paste(ss, lg, im)
    if (inherits(ch, "error")) { bad <- c(bad, paste(tag, "error:", conditionMessage(ch))); next }
    if (length(ch) != 1L || !nzchar(ch)) bad <- c(bad, paste(tag, "empty chart"))
    an <- .audit_anomalies(strsplit(ch, "\n")[[1]])
    if (length(an)) bad <- c(bad, paste(tag, "placeholder text:", an[1]))
  }
  expect_true(length(bad) == 0L, info = paste(c("", bad), collapse = "\n"))
})
