# Audit stage 2, part 4: variable names.
#
# Real data sets use names such as "brand.eval", "trust_1", "X1" next to "X10",
# lower-case names, and (for Japanese users) Japanese names, including names
# that start with another name. Each variant is run end to end: sweep, hand
# check of the fit, independent binarization, configuration charts, and both
# report formats.

.names_variants <- list(
  dots_underscores = list(y = "brand.eval", x = c("trust_1", "trust.2", "price")),
  prefix_pair      = list(y = "Y",          x = c("X1", "X10", "X2")),
  lowercase        = list(y = "y",          x = c("x1", "x2", "x3")),
  japanese         = list(y = "\u6e80\u8db3",
                          x = c("\u54c1\u8cea", "\u4fa1\u683c", "\u4fe1\u983c")),
  japanese_prefix  = list(y = "\u6e80\u8db3",
                          x = c("\u54c1\u8cea", "\u54c1\u8cea\u7ba1\u7406", "\u4fa1\u683c"))
)

.rename_sample <- function(v) {
  dat <- .audit_sample()
  names(dat)[match(c("Y", "X1", "X2", "X3"), names(dat))] <- c(v$y, v$x)
  dat
}

test_that("names: sweeps, charts and reports work for unusual variable names", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  problems <- character(0)
  for (nm in names(.names_variants)) {
    v <- .names_variants[[nm]]
    dat <- .rename_sample(v)
    thrX <- stats::setNames(rep(7, 3), v$x)
    for (spec in c("complex", "intermediate")) for (oc in c("Y", "~Y")) {
      outcome <- if (oc == "~Y") paste0("~", v$y) else v$y
      tag <- sprintf("[%s | %s | %s]", nm, spec, oc)
      sp <- .audit_spec(spec)
      args <- list(dat = dat, outcome = outcome, conditions = v$x, sweep_range = 6:8,
                   thrX = thrX, include = sp$include, return_details = TRUE)
      if (!is.null(sp$dir.exp)) args$dir.exp <- sp$dir.exp
      res <- tryCatch(suppressWarnings(suppressMessages(do.call(otSweep, args))),
                      error = function(e) e)
      if (inherits(res, "error")) {
        problems <- c(problems, paste(tag, "sweep failed:", conditionMessage(res)))
        next
      }
      pr <- c(.audit_check_binarization(res, dat, v$x),
              .audit_check_hand_fit(res, dat, v$x),
              .audit_check_chart(res, v$x))
      for (fmt in c("full", "simple")) {
        rep <- .audit_report(res, format = fmt, dat = dat)
        rp <- .audit_check_report(rep, res, fmt)
        if (length(rp)) pr <- c(pr, paste0("[", fmt, "] ", rp))
      }
      if (length(pr)) problems <- c(problems, paste(tag, pr))
    }
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n"))
})
