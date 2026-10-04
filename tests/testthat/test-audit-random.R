# Systematic audit 2: randomized data and rare solution shapes.
#
# Rare shapes (several minimal solutions, no solution at all, tied
# intermediate solutions) are where extraction and report code paths diverge.
# Data sets are generated from fixed seeds, so every failure is reproducible.
#
# Size: THSQCA_AUDIT_N (default 25 data sets). Set e.g. THSQCA_AUDIT_N=300
# for a long overnight run.

test_that("audit random: reports are well-formed on random data sets", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  n_sets <- suppressWarnings(as.integer(Sys.getenv("THSQCA_AUDIT_N", "25")))
  if (is.na(n_sets) || n_sets < 1L) n_sets <- 25L

  conds <- paste0("X", 1:4)
  problems <- character(0)
  n_sweep_failed <- 0L
  sweep_msgs <- character(0)

  for (seed in seq_len(n_sets)) {
    dat <- .audit_random_data(seed)
    set.seed(1000 + seed)
    spec <- sample(c("complex", "parsimonious", "intermediate"), 1)
    mode <- sample(c("first", "all", "essential"), 1)
    outcome <- sample(c("Y", "~Y"), 1)
    thr <- sample(4:7, 1)
    sp <- .audit_spec(spec)
    args <- list(dat = dat, outcome = outcome, conditions = conds,
                 sweep_range = thr:(thr + 2), thrX = stats::setNames(rep(thr, 4), conds),
                 include = sp$include, extract_mode = mode,
                 incl.cut = sample(c(0.75, 0.8, 0.85), 1),
                 n.cut = sample(1:2, 1), return_details = TRUE)
    if (spec == "intermediate") args$dir.exp <- rep(1, 4)

    tag <- sprintf("[seed %d | %s | %s | %s | thr %d]", seed, spec, mode, outcome, thr)
    res <- tryCatch(suppressWarnings(suppressMessages(do.call(otSweep, args))),
                    error = function(e) e)
    if (inherits(res, "error")) {
      n_sweep_failed <- n_sweep_failed + 1L
      sweep_msgs <- c(sweep_msgs, paste(tag, conditionMessage(res)))
      next
    }

    sm <- .audit_check_summary(res, mode)
    if (length(sm)) problems <- c(problems, paste(tag, sm))
    for (fmt in c("full", "simple")) {
      rep <- .audit_report(res, format = fmt, include_chart = TRUE)
      pr <- .audit_check_report(rep, res, fmt)
      if (length(pr)) problems <- c(problems, paste0(tag, "[", fmt, "] ", pr))
    }
  }

  # A sweep that stops with an error on random data can be legitimate (for
  # example, no case passes a threshold); too many of them means the generator
  # or the sweep function is at fault. Messages are shown for inspection.
  expect_true(n_sweep_failed <= ceiling(0.3 * n_sets),
              info = paste(c("too many failed sweeps:", sweep_msgs), collapse = "\n"))
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("audit random: several minimal solutions are listed consistently", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  conds <- paste0("X", 1:4)

  # Find data sets whose complex solution has several minimal solutions.
  found <- list()
  for (seed in 1:300) {
    dat <- .audit_random_data(seed)
    res <- tryCatch(
      suppressWarnings(suppressMessages(
        otSweep(dat = dat, outcome = "Y", conditions = conds, sweep_range = 5,
                thrX = stats::setNames(rep(5, 4), conds), incl.cut = 0.8,
                n.cut = 1, return_details = TRUE))),
      error = function(e) NULL)
    if (is.null(res)) next
    n_min <- length(collect_unique_i_sol(res$details[[1]]$solution))
    if (n_min >= 2L) found[[length(found) + 1L]] <- seed
    if (length(found) >= 3L) break
  }
  skip_if(length(found) == 0L, "no random data set with several minimal solutions in 300 seeds")

  problems <- character(0)
  for (seed in unlist(found)) {
    dat <- .audit_random_data(seed)
    for (mode in c("first", "all", "essential")) {
      res <- suppressWarnings(suppressMessages(
        otSweep(dat = dat, outcome = "Y", conditions = conds, sweep_range = 5,
                thrX = stats::setNames(rep(5, 4), conds), incl.cut = 0.8,
                n.cut = 1, extract_mode = mode, return_details = TRUE)))
      tag <- sprintf("[seed %d | %s]", seed, mode)
      sm <- .audit_check_summary(res, mode)
      if (length(sm)) problems <- c(problems, paste(tag, sm))
      for (fmt in c("full", "simple")) {
        rep <- .audit_report(res, format = fmt)
        pr <- .audit_check_report(rep, res, fmt)
        if (length(pr)) problems <- c(problems, paste0(tag, "[", fmt, "] ", pr))
      }
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("audit random: a sweep with no solution at every threshold still reports", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  res <- tryCatch(
    suppressWarnings(suppressMessages(
      otSweep(dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
              sweep_range = 9:10, thrX = c(X1 = 9, X2 = 9, X3 = 9),
              return_details = TRUE))),
    error = function(e) e)
  skip_if(inherits(res, "error"), "sweep itself stops on this input (acceptable)")

  problems <- character(0)
  for (fmt in c("full", "simple")) {
    rep <- .audit_report(res, format = fmt)
    pr <- .audit_check_report(rep, res, fmt)
    if (length(pr)) problems <- c(problems, paste0("[", fmt, "] ", pr))
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})
