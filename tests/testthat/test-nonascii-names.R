# Non-ASCII condition names (for example Japanese): QCA::minimize() can fail on
# them, which used to appear only as "No solution". The sweep functions now
# warn about such names up front (the check does not depend on QCA).

test_that("non-ASCII condition names trigger a warning in otSweep()", {
  skip_if_not_installed("QCA")
  d <- sample_data
  nm <- "\u8981\u56e0"          # two kanji, written as escapes
  names(d) <- c("Y", nm, "X2", "X3")
  thrX <- c(7, 7)
  names(thrX) <- c(nm, "X2")
  w <- character(0)
  withCallingHandlers(
    otSweep(d, outcome = "Y", conditions = c(nm, "X2"),
            sweep_range = 7, thrX = thrX),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("non-ASCII", w)))
})
