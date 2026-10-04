# Audit stage 2, part 1: independent oracle.
#
# (a) The binarized data stored in each detail entry equals an independent
#     re-implementation of "x >= threshold -> 1" (with pre_calibrated
#     conditions passed through).
# (b) The reported inclS / covS of the displayed solution equals a hand
#     computation from the RAW data and the displayed expression:
#         inclS = sum(min(X, Y)) / sum(X),  covS = sum(min(X, Y)) / sum(Y)
#     with min within a term, max across terms and 1 - x for "~". This does not
#     touch QCA's own fit measures at all.
# (c) Range invariants: 0 <= covS <= 1, and inclS >= incl.cut (all observed
#     rows have n >= n.cut = 1, so every configuration in a solution passed the
#     consistency cut-off).

.oracle_sweepers <- c("otSweep", "ctSweepS", "ctSweepM", "dtSweep")

test_that("oracle: dat_bin equals an independent binarization", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  problems <- character(0)
  for (sw in .oracle_sweepers) for (spec in c("complex", "intermediate")) for (oc in c("Y", "~Y")) {
    res <- .audit_run(sw, spec, "first", oc, dat)
    tag <- sprintf("[%s | %s | %s]", sw, spec, oc)
    if (.audit_is_error(res)) { problems <- c(problems, paste(tag, "sweep failed:", res$message)); next }
    pr <- .audit_check_binarization(res, dat, c("X1", "X2", "X3"))
    if (length(pr)) problems <- c(problems, paste(tag, pr))
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("oracle: reported inclS/covS equal a hand computation from raw data", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  problems <- character(0)
  for (sw in .oracle_sweepers) for (spec in c("complex", "parsimonious", "intermediate")) for (oc in c("Y", "~Y")) {
    res <- .audit_run(sw, spec, "first", oc, dat)
    tag <- sprintf("[%s | %s | %s]", sw, spec, oc)
    if (.audit_is_error(res)) { problems <- c(problems, paste(tag, "sweep failed:", res$message)); next }
    pr <- .audit_check_hand_fit(res, dat, c("X1", "X2", "X3"))
    if (length(pr)) problems <- c(problems, paste(tag, pr))
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("oracle: fit measures stay inside their mathematical bounds", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  problems <- character(0)
  for (sw in .oracle_sweepers) for (spec in c("complex", "parsimonious", "intermediate")) for (oc in c("Y", "~Y")) {
    res <- .audit_run(sw, spec, "first", oc, dat)
    if (.audit_is_error(res)) next
    sm <- res$summary
    ok <- !is.na(sm$inclS)
    tag <- sprintf("[%s | %s | %s]", sw, spec, oc)
    if (any(sm$covS[ok] < -1e-9 | sm$covS[ok] > 1 + 1e-9)) problems <- c(problems, paste(tag, "covS outside [0, 1]"))
    if (any(sm$inclS[ok] > 1 + 1e-9)) problems <- c(problems, paste(tag, "inclS above 1"))
    if (any(sm$inclS[ok] < 0.8 - 1e-6)) problems <- c(problems, paste(tag, "inclS below incl.cut = 0.8"))
    if (any(is.na(sm$inclS[sm$expression != "No solution"]))) {
      problems <- c(problems, paste(tag, "a solution is reported without inclS"))
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("oracle: hand computation agrees on random data sets", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  n_sets <- suppressWarnings(as.integer(Sys.getenv("THSQCA_AUDIT_N", "25")))
  if (is.na(n_sets) || n_sets < 1L) n_sets <- 25L
  conds <- paste0("X", 1:4)
  problems <- character(0)
  for (seed in seq_len(n_sets)) {
    dat <- .audit_random_data(seed)
    set.seed(2000 + seed)
    spec <- sample(c("complex", "parsimonious", "intermediate"), 1)
    oc <- sample(c("Y", "~Y"), 1)
    thr <- sample(4:7, 1)
    sp <- .audit_spec(spec)
    args <- list(dat = dat, outcome = oc, conditions = conds,
                 sweep_range = thr:(thr + 2), thrX = stats::setNames(rep(thr, 4), conds),
                 include = sp$include, extract_mode = "first", return_details = TRUE)
    if (spec == "intermediate") args$dir.exp <- rep(1, 4)
    res <- tryCatch(suppressWarnings(suppressMessages(do.call(otSweep, args))),
                    error = function(e) NULL)
    if (is.null(res)) next
    tag <- sprintf("[seed %d | %s | %s | thr %d]", seed, spec, oc, thr)
    pr <- c(.audit_check_binarization(res, dat, conds), .audit_check_hand_fit(res, dat, conds))
    if (length(pr)) problems <- c(problems, paste(tag, pr))
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})

test_that("oracle: fuzzy pre_calibrated conditions use min / 1 - x semantics", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  conds <- c("A", "B", "C")
  dat <- .intermediate_construct_data()
  emptyX <- stats::setNames(numeric(0), character(0))
  problems <- character(0)
  for (oc in c("Y", "~Y")) for (spec in c("complex", "parsimonious")) {
    sp <- .audit_spec(spec)
    res <- tryCatch(
      suppressWarnings(suppressMessages(
        otSweep(dat = dat, outcome = oc, conditions = conds, sweep_range = c(3, 5),
                thrX = emptyX, pre_calibrated = conds, include = sp$include,
                incl.cut = 0.8, n.cut = 1, return_details = TRUE))),
      error = function(e) e)
    tag <- sprintf("[%s | %s]", oc, spec)
    if (inherits(res, "error")) { problems <- c(problems, paste(tag, "sweep failed:", conditionMessage(res))); next }
    pr <- c(.audit_check_binarization(res, dat, conds), .audit_check_hand_fit(res, dat, conds))
    if (length(pr)) problems <- c(problems, paste(tag, pr))
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})
