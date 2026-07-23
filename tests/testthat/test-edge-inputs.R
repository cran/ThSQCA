# Degenerate-input robustness tests (v2.0.4).
#
# These pin the CURRENT (verified-safe) behavior on edge inputs: the sweep
# functions must not crash, and must return a well-formed data.frame with the
# expected number of rows. They intentionally do not assert specific solution
# strings (which may legitimately vary with QCA versions); they assert
# structural sanity only.

.edge_frame <- function(n, y, seed = 1) {
  set.seed(seed)
  d <- data.frame(X1 = sample(0:10, n, TRUE),
                  X2 = sample(0:10, n, TRUE),
                  X3 = sample(0:10, n, TRUE))
  d$Y <- y
  d
}

.expect_sweep_ok <- function(res, n_rows) {
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), n_rows)
  expect_true(all(c("expression", "inclS", "covS") %in% colnames(res)))
  ok <- !is.na(res$covS)
  expect_true(all(res$covS[ok] >= 0 & res$covS[ok] <= 1))
  expect_true(all(res$inclS[ok & !is.na(res$inclS)] >= 0 &
                  res$inclS[ok & !is.na(res$inclS)] <= 1))
}

test_that("otSweep survives all-positive and all-negative outcomes", {
  skip_if_not_installed("QCA")
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)

  d_hi <- .edge_frame(30, rep(10, 30))
  r_hi <- otSweep(dat = d_hi, outcome = "Y", conditions = c("X1", "X2", "X3"),
                  sweep_range = 6:8, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_hi, 3L)

  d_lo <- .edge_frame(30, rep(0, 30))
  r_lo <- otSweep(dat = d_lo, outcome = "Y", conditions = c("X1", "X2", "X3"),
                  sweep_range = 6:8, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_lo, 3L)
  # No positive case at any threshold: no cell may claim a solution with fit.
  expect_true(all(is.na(r_lo$covS) | grepl("No solution", r_lo$expression)))
})

test_that("otSweep survives constant conditions, tiny n, and duplicated rows", {
  skip_if_not_installed("QCA")
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)

  d_const <- .edge_frame(30, sample(0:10, 30, TRUE), seed = 2)
  d_const$X3 <- 5
  r_const <- otSweep(dat = d_const, outcome = "Y", conditions = c("X1", "X2", "X3"),
                     sweep_range = 6:8, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_const, 3L)

  d_one <- .edge_frame(1, 8, seed = 3)
  r_one <- otSweep(dat = d_one, outcome = "Y", conditions = c("X1", "X2", "X3"),
                   sweep_range = 6:8, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_one, 3L)

  d_dup <- .edge_frame(4, c(8, 8, 2, 2), seed = 4)
  d_dup <- d_dup[rep(1:4, each = 10), ]
  r_dup <- otSweep(dat = d_dup, outcome = "Y", conditions = c("X1", "X2", "X3"),
                   sweep_range = 6:8, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_dup, 3L)
})

test_that("otSweep survives out-of-range sweeps, huge n.cut, and NA in a condition", {
  skip_if_not_installed("QCA")
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)

  d <- .edge_frame(30, sample(0:10, 30, TRUE), seed = 5)
  r_oor <- otSweep(dat = d, outcome = "Y", conditions = c("X1", "X2", "X3"),
                   sweep_range = 50:52, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_oor, 3L)
  # Nothing can clear a threshold above the data maximum.
  expect_true(all(is.na(r_oor$covS) | grepl("No solution", r_oor$expression)))

  r_ncut <- otSweep(dat = d, outcome = "Y", conditions = c("X1", "X2", "X3"),
                    sweep_range = 6:8, thrX = thrX, n.cut = 999,
                    return_details = FALSE)
  .expect_sweep_ok(r_ncut, 3L)

  d_na <- d; d_na$X1[3] <- NA
  r_na <- otSweep(dat = d_na, outcome = "Y", conditions = c("X1", "X2", "X3"),
                  sweep_range = 6:8, thrX = thrX, return_details = FALSE)
  .expect_sweep_ok(r_na, 3L)
})

test_that("otSweep accepts a descending sweep_range", {
  skip_if_not_installed("QCA")
  d <- .edge_frame(30, sample(0:10, 30, TRUE), seed = 6)
  r <- otSweep(dat = d, outcome = "Y", conditions = c("X1", "X2", "X3"),
               sweep_range = 8:6, thrX = c(X1 = 7, X2 = 7, X3 = 7),
               return_details = FALSE)
  .expect_sweep_ok(r, 3L)
})
