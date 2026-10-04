# Audit stage 2, part 2: metamorphic invariants.
#
# Transformations of the input that must not change the analysis:
#   * repeating the call (determinism)
#   * return_details = FALSE vs TRUE (same summary)
#   * shuffling the rows of the data
#   * reordering the columns of the data
#   * permuting the order of `conditions`
#   * an affine change of scale of the data together with the thresholds
#   * storing the columns as double instead of integer
#   * the extract_mode ("first" / "all" / "essential") for unique solutions
# Also: the input data are never modified, and the number of rows of each
# summary equals the number of threshold combinations requested.

.inv_run <- function(dat, spec = "complex", conditions = c("X1", "X2", "X3"),
                     sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
                     extract_mode = "first", return_details = TRUE, outcome = "Y") {
  sp <- .audit_spec(spec)
  args <- list(dat = dat, outcome = outcome, conditions = conditions,
               sweep_range = sweep_range, thrX = thrX[conditions],
               include = sp$include, extract_mode = extract_mode,
               return_details = return_details)
  if (!is.null(sp$dir.exp)) args$dir.exp <- rep(1, length(conditions))
  suppressWarnings(suppressMessages(do.call(otSweep, args)))
}

.inv_specs <- c("complex", "parsimonious", "intermediate")

test_that("invariant: repeating a call gives an identical result", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  for (spec in .inv_specs) {
    a <- .inv_run(dat, spec); b <- .inv_run(dat, spec)
    expect_identical(a$summary, b$summary, info = spec)
  }
})

test_that("invariant: return_details = FALSE returns the same summary", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  for (spec in .inv_specs) {
    full <- .inv_run(dat, spec, return_details = TRUE)
    lite <- .inv_run(dat, spec, return_details = FALSE)
    expect_true(is.data.frame(lite), info = spec)
    expect_true(isTRUE(all.equal(as.data.frame(lite), as.data.frame(full$summary),
                                 check.attributes = FALSE)), info = spec)
  }
})

test_that("invariant: row order, column order and storage type do not matter", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  set.seed(42)
  shuffled <- dat[sample(nrow(dat)), , drop = FALSE]
  reordered <- dat[, rev(names(dat)), drop = FALSE]
  as_double <- dat; as_double[] <- lapply(as_double, as.numeric)
  for (spec in .inv_specs) {
    base <- .result_signature(.inv_run(dat, spec))
    expect_equal(.result_signature(.inv_run(shuffled, spec)), base, info = paste(spec, "shuffled rows"))
    expect_equal(.result_signature(.inv_run(reordered, spec)), base, info = paste(spec, "reordered columns"))
    expect_equal(.result_signature(.inv_run(as_double, spec)), base, info = paste(spec, "double storage"))
  }
})

test_that("invariant: the order of `conditions` does not matter", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  for (spec in .inv_specs) {
    base <- .result_signature(.inv_run(dat, spec))
    for (perm in list(c("X3", "X1", "X2"), c("X2", "X3", "X1"))) {
      expect_equal(.result_signature(.inv_run(dat, spec, conditions = perm)), base,
                   info = paste(spec, paste(perm, collapse = ",")))
    }
  }
})

test_that("invariant: an affine change of scale of data and thresholds does not matter", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  a <- 10; b <- 100                       # x -> a * x + b, exact on integers
  scaled <- dat; scaled[] <- lapply(scaled, function(v) a * v + b)
  for (spec in .inv_specs) {
    base <- .result_signature(.inv_run(dat, spec))
    sc <- .inv_run(scaled, spec, sweep_range = a * (6:8) + b,
                   thrX = c(X1 = a * 7 + b, X2 = a * 7 + b, X3 = a * 7 + b))
    expect_equal(.result_signature(sc), base, info = spec)
  }
})

test_that("invariant: the input data are never modified", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  before <- dat
  for (sw in c("otSweep", "ctSweepS", "ctSweepM", "dtSweep")) {
    res <- .audit_run(sw, "intermediate", "first", "Y", dat)
    expect_identical(dat, before, info = paste(sw, "sweep"))
    if (!.audit_is_error(res)) {
      for (fmt in c("full", "simple")) {
        invisible(.audit_report(res, format = fmt, dat = dat))
        expect_identical(dat, before, info = paste(sw, fmt, "report"))
      }
    }
  }
})

test_that("invariant: extract_mode does not change unique solutions", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  dat <- .audit_sample()
  for (spec in .inv_specs) {
    s <- lapply(c("first", "all", "essential"),
                function(m) .inv_run(dat, spec, extract_mode = m)$summary)
    expect_equal(s[[1]]$n_solutions, s[[2]]$n_solutions, info = spec)
    expect_equal(s[[1]]$n_solutions, s[[3]]$n_solutions, info = spec)
    uniq <- s[[1]]$n_solutions == 1L
    if (any(uniq)) {
      for (k in 2:3) {
        expect_equal(unname(vapply(s[[k]]$expression[uniq], .canon_expr, character(1))),
                     unname(vapply(s[[1]]$expression[uniq], .canon_expr, character(1))),
                     info = paste(spec, "expression, mode", k))
        expect_equal(s[[k]]$inclS[uniq], s[[1]]$inclS[uniq], info = paste(spec, "inclS, mode", k))
        expect_equal(s[[k]]$covS[uniq],  s[[1]]$covS[uniq],  info = paste(spec, "covS, mode", k))
      }
    }
  }
})

test_that("invariant: one summary row per requested threshold combination", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  expect_equal(nrow(.audit_run("otSweep", "complex")$summary), 3L)
  expect_equal(nrow(.audit_run("ctSweepS", "complex")$summary), 3L)
  expect_equal(nrow(.audit_run("ctSweepM", "complex")$summary), 2L)   # X1 in 6:7
  expect_equal(nrow(.audit_run("dtSweep", "complex")$summary), 6L)    # 2 x 3
})
