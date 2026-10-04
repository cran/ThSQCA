# Audit stage 3: multi-dimensional sweeps, fuzzy (pre_calibrated) conditions,
# and the settings printed in the report overview.
#
# Size of the random parts: THSQCA_AUDIT_N (default 20 data sets per group).

.n_sets3 <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("THSQCA_AUDIT_N", "20")))
  if (is.na(n) || n < 1L) 20L else n
}

.audit_fuzzy_data <- function(seed, n = 60) {
  set.seed(seed)
  m <- matrix(round(stats::runif(n * 4), 3), ncol = 4,
              dimnames = list(NULL, paste0("X", 1:4)))
  m[m == 0.5] <- 0.501                      # QCA warns about memberships of 0.5
  w <- stats::runif(4)
  y <- as.numeric(m %*% w) / sum(w) * 10 + stats::rnorm(n, 0, 1.2)
  data.frame(Y = pmin(10, pmax(0, round(y))), m)
}

# Check one result: independent binarization, hand-computed fit, charts, and
# both report formats. Returns a character vector of problems.
.audit3_check <- function(res, dat, conds) {
  pr <- c(.audit_check_binarization(res, dat, conds),
          .audit_check_hand_fit(res, dat, conds),
          .audit_check_chart(res, conds),
          .audit_check_summary(res, "first"))
  for (fmt in c("full", "simple")) {
    rp <- .audit_check_report(.audit_report(res, format = fmt, dat = dat), res, fmt)
    if (length(rp)) pr <- c(pr, paste0("[", fmt, "] ", rp))
  }
  pr
}

# ---------------------------------------------------------------------------
# Report overview: the sweep design and thresholds must be stated correctly.
# ---------------------------------------------------------------------------

test_that("overview: every sweep function states its design and thresholds", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  ov <- function(res) {
    rep <- .audit_report(res, format = "full", include_chart = FALSE, dat = dat)
    expect_null(rep$error)
    rep$lines
  }

  ots <- ov(.audit_run("otSweep", "complex"))
  expect_true(any(grepl("^\\| Y Sweep Range \\| 6-8 \\|", ots)))

  cts <- ov(.audit_run("ctSweepS", "complex"))
  expect_true(any(grepl("^\\| Swept Condition \\| X3 \\|", cts)))
  expect_true(any(grepl("^\\| X Sweep Range \\(X3\\) \\| 6-8 \\|", cts)))
  expect_false(any(grepl("^\\| Y Sweep Range \\|", cts)),
               info = "ctSweepS sweeps a condition threshold, not the outcome threshold")
  expect_true(any(grepl("^\\| Y Threshold \\| 7 \\|", cts)))

  dts <- ov(.audit_run("dtSweep", "complex"))
  expect_true(any(grepl("^\\| X Sweep List \\| X1=6-7, X2=7, X3=7 \\|", dts)))
  expect_true(any(grepl("^\\| Y Sweep Range \\| 6-8 \\|", dts)))

  # ctSweepM with thrX_default: the default must be recorded and reported.
  res <- suppressWarnings(suppressMessages(
    ctSweepM(dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
             sweep_list = list(X1 = 6:7), thrY = 7, thrX_default = 6,
             return_details = TRUE)))
  expect_equal(res$params$thrX_default, 6)
  ctm <- ov(res)
  expect_true(any(grepl("^\\| X Sweep List \\| X1=6-7 \\|", ctm)))
  expect_true(any(grepl("^\\| Default X Threshold \\| 6 \\|", ctm)))
  expect_true(any(grepl("^thrX_default: 6$", ctm)))
})

test_that("multi sweeps with thrX_default are hand-checked against raw data", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  conds <- c("X1", "X2", "X3")
  problems <- character(0)
  for (oc in c("Y", "~Y")) for (d in c(5, 7)) {
    res <- suppressWarnings(suppressMessages(
      ctSweepM(dat = dat, outcome = oc, conditions = conds,
               sweep_list = list(X1 = 6:7), thrY = 7, thrX_default = d,
               return_details = TRUE)))
    pr <- .audit3_check(res, dat, conds)
    if (length(pr)) problems <- c(problems, paste0("[ctSweepM ", oc, " default ", d, "] ", pr))
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

# ---------------------------------------------------------------------------
# Random data, multi-dimensional sweeps
# ---------------------------------------------------------------------------

test_that("random: ctSweepS, ctSweepM and dtSweep are consistent end to end", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  conds <- paste0("X", 1:4)
  problems <- character(0)
  n_failed <- 0L; msgs <- character(0); n_total <- 0L
  for (seed in seq_len(.n_sets3())) {
    dat <- .audit_random_data(seed)
    for (sw in c("ctSweepS", "ctSweepM", "dtSweep")) {
      set.seed(3000 + seed)
      spec <- sample(c("complex", "parsimonious", "intermediate"), 1)
      oc <- sample(c("Y", "~Y"), 1)
      thr <- sample(4:6, 1); thrY <- sample(4:7, 1)
      ic <- sample(c(0.75, 0.8, 0.85), 1); nc <- sample(1:2, 1)
      dflt <- sample(4:7, 1); svar <- sample(conds, 1)
      sp <- .audit_spec(spec)
      args <- list(dat = dat, outcome = oc, conditions = conds, include = sp$include,
                   extract_mode = "first", incl.cut = ic, n.cut = nc, return_details = TRUE)
      if (spec == "intermediate") args$dir.exp <- rep(1, 4)
      extra <- switch(sw,
        ctSweepS = list(sweep_var = svar, sweep_range = thr:(thr + 2), thrY = thrY, thrX_default = dflt),
        ctSweepM = list(sweep_list = list(X1 = thr:(thr + 1), X2 = thr), thrY = thrY, thrX_default = dflt),
        dtSweep  = list(sweep_list_X = list(X1 = thr:(thr + 1), X2 = thr, X3 = thr, X4 = thr),
                        sweep_range_Y = 4:6))
      tag <- sprintf("[seed %d | %s | %s | %s | %s]", seed, sw, spec, oc,
                     if (sw == "ctSweepS") paste("var", svar) else paste("thr", thr))
      n_total <- n_total + 1L
      res <- tryCatch(suppressWarnings(suppressMessages(do.call(match.fun(sw), c(args, extra)))),
                      error = function(e) e)
      if (inherits(res, "error")) {
        n_failed <- n_failed + 1L; msgs <- c(msgs, paste(tag, conditionMessage(res))); next
      }
      pr <- .audit3_check(res, dat, conds)
      if (length(pr)) problems <- c(problems, paste(tag, pr))
    }
  }
  expect_true(n_failed <= ceiling(0.3 * n_total),
              info = paste(c("too many failed sweeps:", msgs), collapse = "\n"))
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

# ---------------------------------------------------------------------------
# Fuzzy (pre_calibrated) conditions in every sweep function
# ---------------------------------------------------------------------------

test_that("fuzzy: pre_calibrated conditions are consistent end to end", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  conds <- paste0("X", 1:4)
  problems <- character(0)
  n_failed <- 0L; msgs <- character(0); n_total <- 0L
  for (seed in seq_len(.n_sets3())) {
    dat <- .audit_fuzzy_data(seed)
    for (sw in c("otSweep", "ctSweepS", "ctSweepM", "dtSweep")) {
      set.seed(4000 + seed)
      k <- sample(1:3, 1)
      pre <- sort(sample(conds, k)); nonpre <- setdiff(conds, pre)
      spec <- sample(c("complex", "parsimonious", "intermediate"), 1)
      oc <- sample(c("Y", "~Y"), 1)
      thr <- sample(4:6, 1); thrY <- sample(4:7, 1)
      sp <- .audit_spec(spec)
      args <- list(dat = dat, outcome = oc, conditions = conds, pre_calibrated = pre,
                   include = sp$include, extract_mode = "first", return_details = TRUE)
      if (spec == "intermediate") args$dir.exp <- rep(1, 4)
      extra <- switch(sw,
        otSweep  = list(sweep_range = thrY:(thrY + 2),
                        thrX = stats::setNames(rep(thr, length(nonpre)), nonpre)),
        ctSweepS = list(sweep_var = nonpre[1], sweep_range = thr:(thr + 2), thrY = thrY,
                        thrX_default = 5),
        ctSweepM = list(sweep_list = stats::setNames(list(thr:(thr + 1)), nonpre[1]),
                        thrY = thrY, thrX_default = 5),
        dtSweep  = list(sweep_list_X = c(stats::setNames(list(thr:(thr + 1)), nonpre[1]),
                                         stats::setNames(rep(list(thr), length(nonpre) - 1L), nonpre[-1])),
                        sweep_range_Y = 4:6))
      tag <- sprintf("[seed %d | %s | %s | %s | pre %s]", seed, sw, spec, oc, paste(pre, collapse = ","))
      n_total <- n_total + 1L
      res <- tryCatch(suppressWarnings(suppressMessages(do.call(match.fun(sw), c(args, extra)))),
                      error = function(e) e)
      if (inherits(res, "error")) {
        n_failed <- n_failed + 1L; msgs <- c(msgs, paste(tag, conditionMessage(res))); next
      }
      pr <- .audit3_check(res, dat, conds)
      if (length(pr)) problems <- c(problems, paste(tag, pr))
    }
  }
  expect_true(n_failed <= ceiling(0.3 * n_total),
              info = paste(c("too many failed sweeps:", msgs), collapse = "\n"))
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("invariant: return_details = FALSE matches for every sweep function", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  for (sw in c("ctSweepS", "ctSweepM", "dtSweep")) {
    full <- .audit_run(sw, "complex")
    sp <- .audit_spec("complex")
    extra <- switch(sw,
      ctSweepS = list(sweep_var = "X3", sweep_range = 6:8, thrY = 7, thrX_default = 7),
      ctSweepM = list(sweep_list = list(X1 = 6:7, X2 = 7, X3 = 7), thrY = 7),
      dtSweep  = list(sweep_list_X = list(X1 = 6:7, X2 = 7, X3 = 7), sweep_range_Y = 6:8))
    lite <- suppressWarnings(suppressMessages(do.call(match.fun(sw), c(
      list(dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"), include = sp$include,
           return_details = FALSE), extra))))
    expect_true(isTRUE(all.equal(as.data.frame(lite), as.data.frame(full$summary),
                                 check.attributes = FALSE)), info = sw)
  }
})
