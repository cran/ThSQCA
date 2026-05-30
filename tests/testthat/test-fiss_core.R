test_that("compute_fiss_core returns augmented result with fiss_core slot", {
  data("sample_data", package = "ThSQCA")

  res <- otSweep(
    dat        = sample_data,
    outcome    = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX       = c(X1 = 7, X2 = 7, X3 = 7),
    include    = "?",
    dir.exp    = c(1, 1, 1),
    return_details = TRUE
  )

  res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))

  expect_true(!is.null(res_fiss$fiss_core))
  expect_true(is.list(res_fiss$fiss_core))
  # Keys should match sweep_range
  expect_true(all(c("6", "7", "8") %in% names(res_fiss$fiss_core)))
})

test_that("compute_fiss_core classification has correct structure", {
  data("sample_data", package = "ThSQCA")

  res <- otSweep(
    dat        = sample_data,
    outcome    = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 7,
    thrX       = c(X1 = 7, X2 = 7, X3 = 7),
    include    = "?",
    dir.exp    = c(1, 1, 1),
    return_details = TRUE
  )

  res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))
  fc <- res_fiss$fiss_core[["7"]]

  expect_true(!is.null(fc))
  expect_true("parsim_expression" %in% names(fc))
  expect_true("interm_expression" %in% names(fc))

  # If a solution exists, classification should be a data frame
  if (!is.null(fc$classification)) {
    expect_s3_class(fc$classification, "data.frame")
    expect_true(all(c("term_idx", "term_expr", "condition",
                      "status", "type") %in% names(fc$classification)))
    # type must be one of core/peripheral/dontcare
    expect_true(all(fc$classification$type %in%
                      c("core", "peripheral", "dontcare")))
    # status must be one of present/absent/dontcare
    expect_true(all(fc$classification$status %in%
                      c("present", "absent", "dontcare")))
  }
})

test_that("compute_fiss_core errors without return_details", {
  data("sample_data", package = "ThSQCA")

  res <- otSweep(
    dat        = sample_data,
    outcome    = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 7,
    thrX       = c(X1 = 7, X2 = 7, X3 = 7),
    include    = "?",
    dir.exp    = c(1, 1, 1),
    return_details = FALSE  # no details
  )

  expect_error(compute_fiss_core(res), "return_details")
})

test_that("compute_fiss_core errors without dir.exp", {
  data("sample_data", package = "ThSQCA")

  res <- otSweep(
    dat        = sample_data,
    outcome    = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 7,
    thrX       = c(X1 = 7, X2 = 7, X3 = 7),
    include    = "?",
    # dir.exp = NULL  (not specified)
    return_details = TRUE
  )

  expect_error(compute_fiss_core(res), "dir.exp")
})

test_that("generate_fiss_chart returns character string", {
  data("sample_data", package = "ThSQCA")

  res <- otSweep(
    dat        = sample_data,
    outcome    = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX       = c(X1 = 7, X2 = 7, X3 = 7),
    include    = "?",
    dir.exp    = c(1, 1, 1),
    return_details = TRUE
  )
  res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))

  chart_uni   <- generate_fiss_chart(res_fiss, symbol_set = "unicode")
  chart_latex <- generate_fiss_chart(res_fiss, symbol_set = "latex")
  chart_ascii <- generate_fiss_chart(res_fiss, symbol_set = "ascii")

  expect_type(chart_uni,   "character")
  expect_type(chart_latex, "character")
  expect_type(chart_ascii, "character")

  # Should contain table markup
  expect_true(grepl("\\|", chart_uni))
})

test_that("generate_fiss_chart errors without fiss_core", {
  data("sample_data", package = "ThSQCA")

  res <- otSweep(
    dat        = sample_data,
    outcome    = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 7,
    thrX       = c(X1 = 7, X2 = 7, X3 = 7),
    include    = "?",
    dir.exp    = c(1, 1, 1),
    return_details = TRUE
  )
  # No compute_fiss_core call
  expect_error(generate_fiss_chart(res), "fiss_core")
})

test_that("SYMBOL_SETS_FISS has correct structure", {
  for (fmt in c("unicode", "latex", "ascii")) {
    ss <- SYMBOL_SETS_FISS[[fmt]]
    expect_true(!is.null(ss$core_present))
    expect_true(!is.null(ss$core_absent))
    expect_true(!is.null(ss$periph_present))
    expect_true(!is.null(ss$periph_absent))
    expect_true(!is.null(ss$note_en))
    expect_true(!is.null(ss$note_ja))
  }
})
