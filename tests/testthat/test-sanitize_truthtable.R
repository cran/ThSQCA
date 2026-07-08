test_that("sanitize_truthtable coerces character incl/PRI to numeric", {
  # Mimic a QCA::truthTable() object whose incl/PRI came back as character
  # (observed with QCA 3.25 / admisc 0.40 on sparse truth tables), with
  # logical-remainder rows carrying the "-" string.
  fake_tt <- list(
    tt = data.frame(
      OUT  = c("1", "0", "?"),          # positive, negative, remainder
      n    = c(5, 3, 0),
      incl = c("0.900", "0.400", "-"),  # character, remainder = "-"
      PRI  = c("0.850", "0.300", "-"),
      stringsAsFactors = FALSE
    )
  )

  out <- sanitize_truthtable(fake_tt)

  expect_type(out$tt$incl, "double")
  expect_type(out$tt$PRI, "double")
  expect_false(anyNA(out$tt$incl))
  expect_false(anyNA(out$tt$PRI))

  # Observed rows keep their values; the remainder row ("-") becomes 0.
  expect_equal(out$tt$incl, c(0.900, 0.400, 0))
  expect_equal(out$tt$PRI, c(0.850, 0.300, 0))

  # OUT must be left untouched so minimize() can still identify remainders.
  expect_identical(out$tt$OUT, c("1", "0", "?"))
})

test_that("sanitize_truthtable is a no-op on already-numeric truth tables", {
  fake_tt <- list(
    tt = data.frame(
      OUT  = c("1", "0"),
      n    = c(5, 3),
      incl = c(0.9, 0.4),
      PRI  = c(0.85, 0.3),
      stringsAsFactors = FALSE
    )
  )
  out <- sanitize_truthtable(fake_tt)
  expect_equal(out$tt$incl, c(0.9, 0.4))
  expect_equal(out$tt$PRI, c(0.85, 0.3))
  expect_identical(out$tt$OUT, c("1", "0"))
})

test_that("sanitize_truthtable tolerates NULL and missing $tt", {
  expect_null(sanitize_truthtable(NULL))
  expect_identical(sanitize_truthtable(list(a = 1)), list(a = 1))
})
