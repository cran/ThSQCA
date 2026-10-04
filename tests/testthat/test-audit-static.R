# Systematic audit 3: static checks on the package source.
#
# These look at the R/ files themselves (available when tests run from the
# source tree, e.g. devtools::test()); they are skipped from an installed
# package, where R/ does not exist.

.audit_source_files <- function() {
  dir <- testthat::test_path("..", "..", "R")
  if (!dir.exists(dir)) return(character(0))
  list.files(dir, pattern = "\\.R$", full.names = TRUE)
}

test_that("extract_all_metrics() is never fed sol$i.sol$C1P1$IC directly", {
  files <- .audit_source_files()
  skip_if(length(files) == 0L, "R/ directory not available")
  hits <- character(0)
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    bad <- grep("extract_all_metrics\\(\\s*sol\\$i\\.sol", src)
    if (length(bad)) hits <- c(hits, paste0(basename(f), ":", bad))
  }
  expect_equal(hits, character(0),
               info = "Use the ic_for_metrics fallback (i.sol first, then sol$IC).")
})

test_that("both report writers name unnamed details before looping", {
  files <- .audit_source_files()
  skip_if(length(files) == 0L, "R/ directory not available")
  src <- readLines(file.path(dirname(files[1]), "tsqca_report.R"), warn = FALSE)
  # ctSweepM() and dtSweep() return details without names. A loop over
  # names(details) then runs zero times and the report silently omits every
  # threshold combination. Each writer must call name_report_details() first.
  calls <- grep("details <- name_report_details\\(details\\)", src)
  loops <- grep("for \\(key in names\\(details\\)\\)", src)
  expect_equal(length(calls), 2L)
  expect_true(length(loops) == 0L || length(calls) >= 2L)
})
