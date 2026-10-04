# Helpers for the systematic audit tests (test-audit-*.R).
#
# Design idea: the audit tests do not re-derive QCA results. They (1) run many
# option combinations through the sweep functions and generate_report(), and
# (2) check the emitted Markdown for internal consistency: no placeholder
# strings where a number is expected, one block per threshold, the same fit in
# every place it is printed, and agreement with the stored QCA object.
# A "problem" is a character string; a clean run yields character(0).

# ---------------------------------------------------------------------------
# Data and sweep runners
# ---------------------------------------------------------------------------

.audit_sample <- function() {
  e <- new.env()
  utils::data("sample_data", package = "ThSQCA", envir = e)
  e$sample_data
}

# Solution specifications: include / dir.exp pairs.
.audit_spec <- function(spec) {
  switch(spec,
    complex       = list(include = "",  dir.exp = NULL),
    parsimonious  = list(include = "?", dir.exp = NULL),
    intermediate  = list(include = "?", dir.exp = c(1, 1, 1)),
    stop("unknown spec: ", spec))
}

# Run one sweep function on sample_data. Returns the result, or an object of
# class "audit_error" carrying the message when the call failed.
.audit_run <- function(sweeper, spec, extract_mode = "first", outcome = "Y",
                       dat = .audit_sample()) {
  sp <- .audit_spec(spec)
  common <- list(dat = dat, outcome = outcome,
                 conditions = c("X1", "X2", "X3"),
                 include = sp$include, extract_mode = extract_mode,
                 return_details = TRUE)
  if (!is.null(sp$dir.exp)) common$dir.exp <- sp$dir.exp

  extra <- switch(sweeper,
    otSweep  = list(sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7)),
    ctSweepS = list(sweep_var = "X3", sweep_range = 6:8,
                    thrY = 7, thrX_default = 7),
    ctSweepM = list(sweep_list = list(X1 = 6:7, X2 = 7, X3 = 7), thrY = 7),
    dtSweep  = list(sweep_list_X = list(X1 = 6:7, X2 = 7, X3 = 7),
                    sweep_range_Y = 6:8),
    stop("unknown sweeper: ", sweeper))

  tryCatch(
    suppressWarnings(suppressMessages(
      do.call(match.fun(sweeper), c(common, extra)))),
    error = function(e) structure(list(message = conditionMessage(e)),
                                  class = "audit_error"))
}

.audit_is_error <- function(x) inherits(x, "audit_error")

# Generate a report; returns list(lines = <character>, error = <NULL|message>).
.audit_report <- function(res, format, include_chart = TRUE, ...) {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    suppressWarnings(suppressMessages(
      generate_report(res, output_file = tmp, format = format,
                      include_chart = include_chart, ...)))
    list(lines = readLines(tmp, encoding = "UTF-8", warn = FALSE), error = NULL)
  }, error = function(e) list(lines = character(0), error = conditionMessage(e)))
}

# ---------------------------------------------------------------------------
# Markdown parsing helpers (pure string functions)
# ---------------------------------------------------------------------------

# Drop fenced code blocks (raw QCA output, code snippets) and the fence lines.
.audit_strip_fences <- function(lines) {
  is_fence <- grepl("^```", lines)
  inside <- cumsum(is_fence) %% 2 == 1
  lines[!inside & !is_fence]
}

# Split fence-free lines into blocks that start at each "### " heading and end
# before the next heading of level 1 to 3.
.audit_blocks <- function(lines) {
  starts <- grep("^### ", lines)
  if (length(starts) == 0) return(list())
  bounds <- grep("^#{1,3} ", lines)
  lapply(starts, function(s) {
    nxt <- bounds[bounds > s]
    e <- if (length(nxt)) nxt[1] - 1L else length(lines)
    list(heading = lines[s],
         body = if (e > s) lines[(s + 1L):e] else character(0))
  })
}

# Placeholder strings that must never appear outside code fences.
.audit_anomalies <- function(lines) {
  x <- .audit_strip_fences(lines)
  pat <- paste0("NULL|character\\(0\\)|numeric\\(0\\)|integer\\(0\\)|",
                "logical\\(0\\)|list\\(\\)|NaN|\\bInf\\b|Error in|<NA>")
  x[grepl(pat, x)]
}

.audit_num <- function(x) suppressWarnings(as.numeric(x))

# "*inclS = 0.906, covS = 0.853*" -> c(inclS, covS) or NULL.
.audit_parse_simple_fit <- function(body) {
  ln <- grep("^\\*inclS = ", body, value = TRUE)
  if (length(ln) != 1L) return(NULL)
  m <- regmatches(ln, regexec("^\\*inclS = ([0-9.]+), covS = ([0-9.]+)\\*$", ln))[[1]]
  if (length(m) != 3L) return(NULL)
  c(inclS = .audit_num(m[2]), covS = .audit_num(m[3]))
}

# "| Consistency (inclS) | 0.906 |" ... in the full report's Solution Fit table.
.audit_parse_full_fit <- function(body) {
  get1 <- function(label) {
    ln <- grep(paste0("^\\| ", label, " \\| "), body, value = TRUE)
    if (length(ln) != 1L) return(NA_real_)
    .audit_num(sub(paste0("^\\| ", label, " \\| ([0-9.]+) \\|$"), "\\1", ln))
  }
  c(inclS = get1("Consistency \\(inclS\\)"), covS = get1("Coverage \\(covS\\)"))
}

.audit_declared_nsol <- function(body) {
  ln <- grep("^\\*\\*Number of Solutions\\*\\*: ", body, value = TRUE)
  if (length(ln) == 0L) return(NA_integer_)
  as.integer(sub("^\\*\\*Number of Solutions\\*\\*: ([0-9]+).*$", "\\1", ln[1]))
}

.audit_count_models <- function(body) sum(grepl("^- M[0-9]+: ", body))

# ---------------------------------------------------------------------------
# Oracle: fit of the displayed solution, read straight from the stored object
# ---------------------------------------------------------------------------

# Returns c(inclS, covS) rounded to 3 digits, or NULL when the stored object
# has a shape this oracle does not cover (several minimal solutions).
.audit_oracle_fit <- function(sol) {
  if (is.null(sol)) return(NULL)
  has_isol <- !is.null(sol$i.sol) && length(sol$i.sol) > 0
  if (has_isol && is.null(sol$i.sol$C1P1$IC)) return(NULL)
  ic <- if (has_isol) sol$i.sol$C1P1$IC else sol$IC
  sc <- ic$sol.incl.cov
  if (is.null(sc) || is.null(sc$inclS) || is.null(sc$covS)) return(NULL)
  round(c(inclS = sc$inclS[1], covS = sc$covS[1]), 3)
}

.audit_has_solution <- function(det) {
  !is.null(det$solution) && length(collect_unique_i_sol(det$solution)) > 0
}

# ---------------------------------------------------------------------------
# The checks. Each returns a character vector of problems.
# ---------------------------------------------------------------------------

.audit_check_report <- function(rep, res, format) {
  p <- character(0)
  if (!is.null(rep$error)) return(paste0("generate_report() failed: ", rep$error))
  lines <- rep$lines

  bad <- .audit_anomalies(lines)
  if (length(bad)) {
    p <- c(p, paste0("placeholder text in report: ",
                     paste(utils::head(bad, 3), collapse = " // ")))
  }

  details <- res$details
  n_det <- length(details)
  has_sol <- vapply(details, .audit_has_solution, logical(1))
  blocks <- .audit_blocks(.audit_strip_fences(lines))

  if (format == "simple") {
    expected <- sum(has_sol)
    if (length(blocks) != expected) {
      p <- c(p, sprintf("simple report has %d solution blocks, expected %d",
                        length(blocks), expected))
    }
    aligned <- length(blocks) == expected
    sol_idx <- which(has_sol)
    for (k in seq_along(blocks)) {
      fit <- .audit_parse_simple_fit(blocks[[k]]$body)
      if (is.null(fit)) {
        p <- c(p, sprintf("block '%s': missing or non-numeric '*inclS = ..., covS = ...*' line",
                          blocks[[k]]$heading))
        next
      }
      if (aligned) {
        orc <- .audit_oracle_fit(details[[sol_idx[k]]]$solution)
        if (!is.null(orc) && !isTRUE(all.equal(unname(fit), unname(orc), tolerance = 1.1e-3))) {
          p <- c(p, sprintf("block '%s': fit %s differs from stored QCA object %s",
                            blocks[[k]]$heading,
                            paste(fit, collapse = "/"), paste(orc, collapse = "/")))
        }
      }
    }
  } else {
    if (n_det <= 27L) {
      if (length(blocks) != n_det) {
        p <- c(p, sprintf("full report has %d detail blocks, expected %d",
                          length(blocks), n_det))
      }
      aligned <- length(blocks) == n_det
      for (k in seq_along(blocks)) {
        if (!has_sol[k] && aligned) next
        fit <- .audit_parse_full_fit(blocks[[k]]$body)
        if (aligned && has_sol[k]) {
          if (any(is.na(fit))) {
            p <- c(p, sprintf("block '%s': Solution Fit table missing or non-numeric",
                              blocks[[k]]$heading))
          } else {
            orc <- .audit_oracle_fit(details[[k]]$solution)
            if (!is.null(orc) && !isTRUE(all.equal(unname(fit), unname(orc), tolerance = 1.1e-3))) {
              p <- c(p, sprintf("block '%s': fit %s differs from stored QCA object %s",
                                blocks[[k]]$heading,
                                paste(fit, collapse = "/"), paste(orc, collapse = "/")))
            }
          }
        }
      }
    }
  }

  # Declared number of solutions must equal the listed models (both formats).
  for (b in blocks) {
    decl <- .audit_declared_nsol(b$body)
    listed <- .audit_count_models(b$body)
    if (!is.na(decl) && listed > 0L && decl != listed) {
      p <- c(p, sprintf("block '%s': declares %d solutions but lists %d",
                        b$heading, decl, listed))
    }
  }
  p
}

# Summary table vs stored objects (extract_mode = "first" only).
.audit_check_summary <- function(res, extract_mode) {
  p <- character(0)
  if (extract_mode != "first") return(p)
  sm <- res$summary
  details <- res$details
  if (is.null(sm) || nrow(sm) != length(details)) return(p)
  for (i in seq_along(details)) {
    sol <- details[[i]]$solution
    if (!.audit_has_solution(details[[i]])) {
      if (!is.na(sm$inclS[i])) {
        p <- c(p, sprintf("summary row %d has inclS but no solution is stored", i))
      }
      next
    }
    orc <- .audit_oracle_fit(sol)
    if (is.null(orc)) next
    got <- round(c(sm$inclS[i], sm$covS[i]), 3)
    if (anyNA(got) || !isTRUE(all.equal(unname(got), unname(orc), tolerance = 1.1e-3))) {
      p <- c(p, sprintf("summary row %d: fit %s differs from stored QCA object %s",
                        i, paste(got, collapse = "/"), paste(orc, collapse = "/")))
    }
  }
  p
}

# Random crisp-threshold test data (fixed seed, reproducible): outcome Y and
# four conditions X1..X4 on a 0-10 scale.
.audit_random_data <- function(seed, n = 60) {
  set.seed(seed)
  X <- matrix(round(stats::runif(n * 4, 0, 10)), ncol = 4,
              dimnames = list(NULL, paste0("X", 1:4)))
  w <- stats::runif(4, 0, 1)
  y <- as.numeric(X %*% w) / sum(w) + stats::rnorm(n, 0, 1.2)
  y <- pmin(10, pmax(0, round(y)))
  data.frame(Y = y, X, check.names = FALSE)
}
