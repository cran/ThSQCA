# Regression test: the Necessity Analysis table of the full report must be
# computed for the SAME outcome as the solution. With a negated outcome ("~Y")
# it used to analyse "Y" (the binarized column) instead of "~Y".

.necessity_rows <- function(lines) {
  i <- grep("^#### Necessity Analysis", lines)
  if (length(i) == 0L) return(NULL)
  tab <- lines[(i[1] + 1L):length(lines)]
  tab <- tab[grepl("^\\|", tab)]
  tab <- tab[-(1:2)]                       # header and separator
  end <- which(!grepl("^\\|", c(tab, "x")))[1] - 1L
  tab <- tab[seq_len(end)]
  do.call(rbind, lapply(strsplit(tab, "\\|"), function(p) {
    p <- trimws(p[-1])
    data.frame(cond = p[1], inclN = suppressWarnings(as.numeric(p[2])),
               covN = suppressWarnings(as.numeric(p[4])), stringsAsFactors = FALSE)
  }))
}

test_that("necessity table follows the outcome polarity (Y and ~Y)", {
  skip_if_not_installed("QCA")
  skip_on_cran()
  for (outcome in c("Y", "~Y")) {
    res <- .audit_run("otSweep", "complex", outcome = outcome)
    expect_false(.audit_is_error(res))
    rep <- .audit_report(res, format = "full", include_chart = FALSE)
    expect_null(rep$error)

    for (k in seq_along(res$details)) {
      det <- res$details[[k]]
      if (is.null(det$dat_bin)) next
      expected <- QCA::pofind(det$dat_bin, outcome = outcome,
                              conditions = names(det$thrX_vec))$incl.cov
      blocks <- .audit_blocks(.audit_strip_fences(rep$lines))
      rows <- .necessity_rows(blocks[[k]]$body)
      skip_if(is.null(rows), "no necessity table in this block")
      for (r in seq_len(nrow(rows))) {
        ex <- expected[trimws(rownames(expected)) == rows$cond[r], , drop = FALSE]
        if (nrow(ex) != 1L || is.na(rows$inclN[r]) || is.nan(ex$inclN)) next
        expect_equal(rows$inclN[r], round(ex$inclN, 3), tolerance = 1.1e-3,
                     info = paste(outcome, "block", k, rows$cond[r]))
      }
    }
  }
})
