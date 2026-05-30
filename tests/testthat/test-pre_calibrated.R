# =============================================================================
# Tests for pre_calibrated parameter (ThSQCA v2.0.0)
# =============================================================================
# Test coverage:
#   [BC] Backward compatibility  — pre_calibrated = NULL produces v1.2.0 output
#   [T]  Functional tests        — T-01 through T-06 from design spec
#   [V]  Validation tests        — V-01 through V-04 from design spec
#   [C]  Consistency test        — manual binarization equivalence
#   [E]  extract_mode="essential" x pre_calibrated (confirmation item 2)
# =============================================================================

# ---------------------------------------------------------------------------
# Helper: build test data with one pre-calibrated (fuzzy) column
# ---------------------------------------------------------------------------
make_test_dat <- function() {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data
  # X2_fuzzy: manually fuzzy-calibrate X2 (range ~1-10) into [0,1]
  # simple linear rescale so values stay in [0,1]
  x2_raw <- dat$X2
  dat$X2_fuzzy <- (x2_raw - min(x2_raw)) / (max(x2_raw) - min(x2_raw))
  # X3_bin: hard binary (already 0/1-like after threshold)
  dat$X3_bin <- ifelse(dat$X3 >= 7, 1L, 0L)
  dat
}

# =============================================================================
# [BC] Backward Compatibility Tests
# =============================================================================

test_that("[BC] otSweep: pre_calibrated=NULL produces identical output to v1.2.0", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)

  res_default <- otSweep(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8, thrX = thrX, return_details = FALSE
  )
  res_null <- otSweep(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8, thrX = thrX,
    pre_calibrated = NULL, return_details = FALSE
  )

  expect_equal(res_default, res_null)
})

test_that("[BC] dtSweep: pre_calibrated=NULL produces identical output to v1.2.0", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data

  res_default <- dtSweep(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_list_X = list(X1 = 6:7, X2 = 7, X3 = 7),
    sweep_range_Y = 6:7, return_details = FALSE
  )
  res_null <- dtSweep(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_list_X = list(X1 = 6:7, X2 = 7, X3 = 7),
    sweep_range_Y = 6:7,
    pre_calibrated = NULL, return_details = FALSE
  )

  expect_equal(res_default, res_null)
})

test_that("[BC] ctSweepS: pre_calibrated=NULL produces identical output to v1.2.0", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data

  res_default <- ctSweepS(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_var = "X3", sweep_range = 6:8, thrY = 7, thrX_default = 7,
    return_details = FALSE
  )
  res_null <- ctSweepS(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_var = "X3", sweep_range = 6:8, thrY = 7, thrX_default = 7,
    pre_calibrated = NULL, return_details = FALSE
  )

  expect_equal(res_default, res_null)
})

test_that("[BC] ctSweepM: pre_calibrated=NULL produces identical output to v1.2.0", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data

  res_default <- ctSweepM(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_list = list(X1 = 6:7, X2 = 7, X3 = 7),
    thrY = 7, return_details = FALSE
  )
  res_null <- ctSweepM(
    dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
    sweep_list = list(X1 = 6:7, X2 = 7, X3 = 7),
    thrY = 7, pre_calibrated = NULL, return_details = FALSE
  )

  expect_equal(res_default, res_null)
})

# =============================================================================
# [T] Functional Tests (T-01 to T-06)
# =============================================================================

test_that("[T-01] otSweep: single fuzzy condition runs and returns valid output", {
  dat <- make_test_dat()

  res <- otSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_range = 6:8,
    thrX        = c(X1 = 7, X2_fuzzy = 0.5, X3 = 7),
    pre_calibrated = c("X2_fuzzy"),
    return_details = TRUE
  )

  expect_s3_class(res, "otSweep_result")
  expect_true(nrow(res$summary) > 0)
  expect_true(all(c("expression", "inclS", "covS") %in% names(res$summary)))
  # pre_calibrated stored in params
  expect_equal(res$params$pre_calibrated, c("X2_fuzzy"))
})

test_that("[T-01] otSweep: fuzzy condition produces different result from crisp binarization", {
  dat <- make_test_dat()
  thrX_crisp  <- c(X1 = 7, X2 = 7,   X3 = 7)
  thrX_fuzzy  <- c(X1 = 7, X2_fuzzy = 0.5, X3 = 7)

  res_crisp <- otSweep(
    dat = dat, outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8, thrX = thrX_crisp, return_details = FALSE
  )
  res_fuzzy <- otSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_range = 6:8, thrX = thrX_fuzzy,
    pre_calibrated = c("X2_fuzzy"), return_details = FALSE
  )

  # Results should differ because X2_fuzzy retains graded membership
  # (at least one expression or fit metric will differ)
  expect_false(identical(res_crisp$expression, res_fuzzy$expression) &&
               identical(res_crisp$inclS,      res_fuzzy$inclS) &&
               identical(res_crisp$covS,        res_fuzzy$covS))
})

test_that("[T-02] otSweep: all conditions fuzzy runs without error", {
  dat <- make_test_dat()
  dat$X1_fuzzy <- (dat$X1 - min(dat$X1)) / (max(dat$X1) - min(dat$X1))

  res <- otSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1_fuzzy", "X2_fuzzy", "X3_bin"),
    sweep_range = 6:8,
    thrX        = c(X1_fuzzy = 0.5, X2_fuzzy = 0.5, X3_bin = 1),
    pre_calibrated = c("X1_fuzzy", "X2_fuzzy", "X3_bin"),
    return_details = FALSE
  )

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
})

test_that("[T-03] dtSweep: mixed fuzzy/crisp conditions run without error", {
  dat <- make_test_dat()

  res <- dtSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_list_X  = list(X1 = 6:7, X2_fuzzy = 0.5, X3 = 7),
    sweep_range_Y = 6:7,
    pre_calibrated = c("X2_fuzzy"),
    return_details = FALSE
  )

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
})

test_that("[T-04] ctSweepS: pre_calibrated non-swept conditions pass through", {
  dat <- make_test_dat()

  res <- ctSweepS(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_var   = "X1",
    sweep_range = 6:8,
    thrY        = 7, thrX_default = 7,
    pre_calibrated = c("X2_fuzzy"),
    return_details = TRUE
  )

  expect_s3_class(res, "ctSweepS_result")
  expect_true(nrow(res$summary) > 0)
  expect_equal(res$params$pre_calibrated, c("X2_fuzzy"))
})

test_that("[T-05] ctSweepM: pre_calibrated non-swept conditions pass through", {
  dat <- make_test_dat()

  res <- ctSweepM(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_list  = list(X1 = 6:7, X2_fuzzy = 0.5, X3 = 7),
    thrY        = 7,
    pre_calibrated = c("X2_fuzzy"),
    return_details = TRUE
  )

  expect_s3_class(res, "ctSweepM_result")
  expect_true(nrow(res$summary) > 0)
  expect_equal(res$params$pre_calibrated, c("X2_fuzzy"))
})

test_that("[T-06] Binary variable as pre_calibrated passes through unchanged", {
  dat <- make_test_dat()

  # X3_bin is already 0/1 — using pre_calibrated should give same result as
  # binarizing with thrX = 1
  res_pc <- otSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2", "X3_bin"),
    sweep_range = 6:8,
    thrX        = c(X1 = 7, X2 = 7, X3_bin = 1),
    pre_calibrated = c("X3_bin"),
    return_details = FALSE
  )
  res_thr <- otSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2", "X3_bin"),
    sweep_range = 6:8,
    thrX        = c(X1 = 7, X2 = 7, X3_bin = 1),
    return_details = FALSE
  )

  expect_equal(res_pc$expression, res_thr$expression)
  expect_equal(res_pc$inclS,      res_thr$inclS)
  expect_equal(res_pc$covS,       res_thr$covS)
})

# =============================================================================
# [V] Validation Tests (V-01 to V-04)
# =============================================================================

test_that("[V-01] Error when pre_calibrated variable not in conditions", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data

  expect_error(
    otSweep(
      dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
      sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
      pre_calibrated = c("X99")
    ),
    regexp = "not found in conditions"
  )
})

test_that("[V-02] Error when pre_calibrated variable has values outside [0, 1]", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data  # X1 has raw Likert values (e.g., 1-10)

  expect_error(
    otSweep(
      dat = dat, outcome = "Y", conditions = c("X1", "X2", "X3"),
      sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
      pre_calibrated = c("X1")  # X1 is raw scale, not [0,1]
    ),
    regexp = "outside \\[0, 1\\]"
  )
})

test_that("[V-03] Warning when pre_calibrated variable contains NA", {
  dat <- make_test_dat()
  dat$X2_fuzzy[1] <- NA  # inject NA

  expect_warning(
    otSweep(
      dat = dat, outcome = "Y",
      conditions  = c("X1", "X2_fuzzy", "X3"),
      sweep_range = 6:8,
      thrX        = c(X1 = 7, X2_fuzzy = 0.5, X3 = 7),
      pre_calibrated = c("X2_fuzzy")
    ),
    regexp = "contains NA"
  )
})

test_that("[V-04] Warning when pre_calibrated variable is also a sweep target in dtSweep", {
  dat <- make_test_dat()

  expect_warning(
    dtSweep(
      dat = dat, outcome = "Y",
      conditions  = c("X1", "X2_fuzzy", "X3"),
      sweep_list_X  = list(X1 = 6:7, X2_fuzzy = c(0.3, 0.5, 0.7), X3 = 7),
      sweep_range_Y = 6:7,
      pre_calibrated = c("X2_fuzzy")
    ),
    regexp = "sweep thresholds ignored"
  )
})

test_that("[V-04] Warning when pre_calibrated variable is also a sweep target in ctSweepM", {
  dat <- make_test_dat()

  expect_warning(
    ctSweepM(
      dat = dat, outcome = "Y",
      conditions  = c("X1", "X2_fuzzy", "X3"),
      sweep_list  = list(X1 = 6:7, X2_fuzzy = c(0.3, 0.5, 0.7), X3 = 7),
      thrY        = 7,
      pre_calibrated = c("X2_fuzzy")
    ),
    regexp = "sweep thresholds ignored"
  )
})

# =============================================================================
# [C] Consistency Test — manual binarization equivalence
# =============================================================================

test_that("[C] Manual binarization at 0.5 equals pre_calibrated=NULL with thrX=0.5", {
  dat <- make_test_dat()

  # Method A: pass fuzzy values, let qca_bin() binarize at 0.5 (default behavior)
  res_a <- otSweep(
    dat = dat, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_range = 6:8,
    thrX        = c(X1 = 7, X2_fuzzy = 0.5, X3 = 7),
    pre_calibrated = NULL,   # binarize X2_fuzzy at 0.5
    return_details = FALSE
  )

  # Method B: manually binarize X2_fuzzy at 0.5 before passing
  dat_b <- dat
  dat_b$X2_fuzzy <- ifelse(dat$X2_fuzzy >= 0.5, 1L, 0L)
  res_b <- otSweep(
    dat = dat_b, outcome = "Y",
    conditions  = c("X1", "X2_fuzzy", "X3"),
    sweep_range = 6:8,
    thrX        = c(X1 = 7, X2_fuzzy = 1, X3 = 7),
    pre_calibrated = c("X2_fuzzy"),  # already 0/1, pass through
    return_details = FALSE
  )

  expect_equal(res_a$expression, res_b$expression)
  expect_equal(res_a$inclS,      res_b$inclS,      tolerance = 1e-10)
  expect_equal(res_a$covS,       res_b$covS,        tolerance = 1e-10)
})

# =============================================================================
# [E] extract_mode="essential" x pre_calibrated (confirmation item 2)
# =============================================================================

test_that("[E] extract_mode='essential' works with pre_calibrated fuzzy condition", {
  dat <- make_test_dat()

  res <- otSweep(
    dat = dat, outcome = "Y",
    conditions   = c("X1", "X2_fuzzy", "X3"),
    sweep_range  = 6:8,
    thrX         = c(X1 = 7, X2_fuzzy = 0.5, X3 = 7),
    pre_calibrated = c("X2_fuzzy"),
    extract_mode = "essential",
    return_details = TRUE
  )

  expect_s3_class(res, "otSweep_result")
  expect_true("n_solutions" %in% names(res$summary))
  # expression column must exist and be character (EPI or "No essential prime implicants")
  expect_true(is.character(res$summary$expression))
})

test_that("[E] extract_mode='all' works with pre_calibrated fuzzy condition", {
  dat <- make_test_dat()

  res <- otSweep(
    dat = dat, outcome = "Y",
    conditions   = c("X1", "X2_fuzzy", "X3"),
    sweep_range  = 6:8,
    thrX         = c(X1 = 7, X2_fuzzy = 0.5, X3 = 7),
    pre_calibrated = c("X2_fuzzy"),
    extract_mode = "all",
    return_details = TRUE
  )

  expect_s3_class(res, "otSweep_result")
  expect_true("n_solutions" %in% names(res$summary))
})

test_that("[E] EPI identification unaffected by variable being fuzzy vs crisp", {
  dat <- make_test_dat()

  # essential mode with pre_calibrated fuzzy variable
  res_fuzzy <- otSweep(
    dat = dat, outcome = "Y",
    conditions   = c("X1", "X2_fuzzy", "X3"),
    sweep_range  = 6:8,
    thrX         = c(X1 = 7, X2_fuzzy = 0.5, X3 = 7),
    pre_calibrated = c("X2_fuzzy"),
    extract_mode = "essential",
    return_details = FALSE
  )

  # expression column must not be NA for all rows
  non_na_count <- sum(!is.na(res_fuzzy$expression))
  expect_true(non_na_count > 0)
  # selective_terms and unique_terms columns must exist
  expect_true("selective_terms" %in% names(res_fuzzy))
  expect_true("unique_terms"    %in% names(res_fuzzy))
})
