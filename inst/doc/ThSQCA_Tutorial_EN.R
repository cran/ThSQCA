## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width = 100)

## ----data-----------------------------------------------------------------------------------------
make_demo_data <- function(n = 400, seed = 2026) {
  set.seed(seed)
  clip <- function(x) pmin(10, pmax(0, round(x)))
  QUA <- clip(rnorm(n, 6, 2))   # quality evaluation
  SER <- clip(rnorm(n, 6, 2))   # service evaluation
  ENV <- clip(rnorm(n, 6, 2))   # store environment evaluation
  # Loyalty is high when quality and service are both high, or (a little
  # less strongly) when quality and environment are both high.
  core <- pmax(pmin(QUA, SER), pmin(QUA, ENV) - 1)
  LOY  <- clip(core + rnorm(n, 0, 0.7))
  data.frame(LOY, QUA, SER, ENV)
}
demo <- make_demo_data()
head(demo)

## ----quick-ots, results="hide"--------------------------------------------------------------------
library(ThSQCA)

res <- otSweep(
  dat         = demo,
  outcome     = "LOY",
  conditions  = c("QUA", "SER", "ENV"),
  sweep_range = 5:8,                          # outcome thresholds to try
  thrX        = c(QUA = 7, SER = 7, ENV = 7)  # fixed condition thresholds
)

## ----quick-summary--------------------------------------------------------------------------------
summary(res)

## ----binary-example, eval=FALSE-------------------------------------------------------------------
# # X1 is binary (0/1); X2 and X3 are 0-10 scores.
# res_mixed <- ctSweepM(
#   dat        = dat,
#   outcome    = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_list = list(X1 = 1,      # binary: fixed threshold 1, not swept
#                     X2 = 6:8,    # scores: swept
#                     X3 = 6:8),
#   thrY       = 7
# )

## ----binary-check, eval=FALSE---------------------------------------------------------------------
# sapply(dat[, c("X1", "X2", "X3")], function(x) all(x %in% c(0, 1)))

## ----pre-cal, eval=FALSE--------------------------------------------------------------------------
# d <- demo
# d$ENV_fz <- pmin(1, pmax(0, (d$ENV - 2.5) / 6))   # example calibration (0 to 1)
# 
# res_mix <- otSweep(
#   dat            = d,
#   outcome        = "LOY",
#   conditions     = c("QUA", "SER", "ENV_fz"),
#   sweep_range    = 5:8,
#   thrX           = c(QUA = 7, SER = 7),           # no entry for ENV_fz
#   pre_calibrated = "ENV_fz"
# )

## ----sweep-membership, eval=FALSE-----------------------------------------------------------------
# fz <- function(x) pmin(1, pmax(0, (x - 2.5) / 6))   # example calibration (0 to 1)
# d_fz <- data.frame(LOY_fz = fz(demo$LOY), QUA_fz = fz(demo$QUA),
#                    SER_fz = fz(demo$SER), ENV_fz = fz(demo$ENV))
# 
# res_fz <- otSweep(
#   dat         = d_fz,
#   outcome     = "LOY_fz",
#   conditions  = c("QUA_fz", "SER_fz", "ENV_fz"),
#   sweep_range = c(0.4, 0.6, 0.8),                    # outcome membership criteria
#   thrX        = c(QUA_fz = 0.6, SER_fz = 0.6, ENV_fz = 0.6)
# )

## ----ots, results="hide"--------------------------------------------------------------------------
res_ots <- otSweep(
  dat         = demo,
  outcome     = "LOY",
  conditions  = c("QUA", "SER", "ENV"),
  sweep_range = 5:8,
  thrX        = c(QUA = 7, SER = 7, ENV = 7),
  incl.cut    = 0.8,   # consistency cutoff for the truth table
  n.cut       = 1      # minimum number of cases per configuration
)

## ----ots-summary----------------------------------------------------------------------------------
summary(res_ots)

## ----cts-s, results="hide"------------------------------------------------------------------------
res_cts <- ctSweepS(
  dat          = demo,
  outcome      = "LOY",
  conditions   = c("QUA", "SER", "ENV"),
  sweep_var    = "SER",   # the condition whose threshold is swept
  sweep_range  = 5:9,     # candidate thresholds for SER
  thrY         = 7,       # fixed outcome threshold (LOY >= 7)
  thrX_default = 7        # fixed threshold for the other conditions
)

## ----cts-s-summary--------------------------------------------------------------------------------
summary(res_cts)

## ----cts-s-sparse---------------------------------------------------------------------------------
sum(demo$QUA >= 7 & demo$SER >= 9)

## ----cts-m, results="hide"------------------------------------------------------------------------
res_mcts <- ctSweepM(
  dat        = demo,
  outcome    = "LOY",
  conditions = c("QUA", "SER", "ENV"),
  sweep_list = list(QUA = 6:8, SER = 6:8, ENV = 6:8),  # candidates per condition
  thrY       = 7                                        # fixed outcome threshold
)

## ----cts-m-summary--------------------------------------------------------------------------------
summary(res_mcts)

## ----dts, results="hide"--------------------------------------------------------------------------
res_dts <- dtSweep(
  dat           = demo,
  outcome       = "LOY",
  conditions    = c("QUA", "SER", "ENV"),
  sweep_list_X  = list(QUA = 6:7, SER = 6:7, ENV = 6:7),  # condition candidates
  sweep_range_Y = 6:8                                     # outcome candidates
)

## ----dts-summary----------------------------------------------------------------------------------
summary(res_dts)

## ----data2----------------------------------------------------------------------------------------
make_demo_data2 <- function(n = 60, seed = 89, rho = 0.85) {
  set.seed(seed)
  clip <- function(x) pmin(10, pmax(0, round(x)))
  L   <- rnorm(n)                                   # common factor
  mk  <- function() clip(6 + 2 * (rho * L + sqrt(1 - rho^2) * rnorm(n)))
  TRU <- mk(); PRC <- mk(); SUP <- mk()
  # Renewal is high when trust is high and either price or support is high.
  RNW <- clip(pmin(TRU, pmax(PRC, SUP)) + rnorm(n, 0, 0.7))
  data.frame(RNW, TRU, PRC, SUP)
}
demo2 <- make_demo_data2()
head(demo2)

## ----tt2------------------------------------------------------------------------------------------
library(QCA)
bin2 <- data.frame(
  RNW = as.integer(demo2$RNW >= 5),
  TRU = as.integer(demo2$TRU >= 7),
  PRC = as.integer(demo2$PRC >= 7),
  SUP = as.integer(demo2$SUP >= 7)
)
tt2 <- truthTable(bin2, outcome = "RNW", conditions = c("TRU", "PRC", "SUP"),
                  incl.cut = 0.8, n.cut = 1, show.cases = FALSE)
tt2

## ----types, warning=FALSE-------------------------------------------------------------------------
thr2  <- c(TRU = 7, PRC = 7, SUP = 7)
cond2 <- c("TRU", "PRC", "SUP")
run2  <- function(...) {
  otSweep(dat = demo2, outcome = "RNW", conditions = cond2, sweep_range = 5:8,
          thrX = thr2, incl.cut = 0.8, n.cut = 1, ...)
}

res_cx <- run2()                                     # complex
res_ps <- run2(include = "?")                        # parsimonious
res_im <- run2(include = "?", dir.exp = c(1, 1, 1))  # intermediate

data.frame(
  thrY         = 5:8,
  complex      = res_cx$summary$expression,
  parsimonious = res_ps$summary$expression,
  intermediate = res_im$summary$expression
)

## ----multi-all------------------------------------------------------------------------------------
res_all <- run2(include = "?", extract_mode = "all")
summary(res_all)

## ----multi-essential, warning=FALSE---------------------------------------------------------------
res_ess <- run2(include = "?", extract_mode = "essential")
res_ess$summary[, c("thrY", "expression", "selective_terms", "unique_terms", "n_solutions")]

## ----multi-report, eval=FALSE---------------------------------------------------------------------
# generate_report(res_all, "ots_multiple_report.md", dat = demo2, format = "full")

## ----str-result-----------------------------------------------------------------------------------
names(res_ots)

## ----negated, eval=FALSE--------------------------------------------------------------------------
# # Presence: cases with LOY >= threshold
# res_pos <- otSweep(dat = demo, outcome = "LOY",  conditions = c("QUA", "SER", "ENV"),
#                    sweep_range = 4:6, thrX = c(QUA = 7, SER = 7, ENV = 7))
# 
# # Absence: cases with LOY < threshold
# res_neg <- otSweep(dat = demo, outcome = "~LOY", conditions = c("QUA", "SER", "ENV"),
#                    sweep_range = 4:6, thrX = c(QUA = 7, SER = 7, ENV = 7))

## ----negated-params, eval=FALSE-------------------------------------------------------------------
# res_neg$params$negate_outcome   # TRUE
# res_neg$params$outcome          # "~LOY"

## ----versions-------------------------------------------------------------------------------------
c(R = R.version.string,
  QCA = as.character(packageVersion("QCA")),
  ThSQCA = as.character(packageVersion("ThSQCA")))

## ----report, eval=FALSE---------------------------------------------------------------------------
# generate_report(res_ots, "ots_report_simple.md", dat = demo, format = "simple")
# generate_report(res_ots, "ots_report_full.md",   dat = demo, format = "full")

## ----report-options, eval=FALSE-------------------------------------------------------------------
# generate_report(res_ots, "r.md", dat = demo, include_chart = FALSE)         # no charts
# generate_report(res_ots, "r.md", dat = demo, chart_symbol_set = "latex")    # LaTeX symbols
# generate_report(res_ots, "r.md", dat = demo, include_raw_output = FALSE)    # omit QCA output

## ----chart----------------------------------------------------------------------------------------
paths <- c("A*B*~C", "A*D", "B*E")
cat(config_chart_from_paths(paths))

## ----chart-ascii----------------------------------------------------------------------------------
cat(config_chart_from_paths(paths, symbol_set = "ascii"))

## ----chart-multi----------------------------------------------------------------------------------
solutions <- list(c("A*B", "C*D"), c("A*B", "C*E"))
cat(config_chart_multi_solutions(solutions))

## ----verify, results="hide"-----------------------------------------------------------------------
bin <- function(x, t) as.integer(x >= t)
d7 <- data.frame(
  LOY = bin(demo$LOY, 7), QUA = bin(demo$QUA, 7),
  SER = bin(demo$SER, 7), ENV = bin(demo$ENV, 7)
)
tt  <- truthTable(d7, outcome = "LOY", conditions = c("QUA", "SER", "ENV"),
                  incl.cut = 0.8, n.cut = 1, show.cases = FALSE)
sol <- minimize(tt)

## ----verify-print---------------------------------------------------------------------------------
sol

## ----fiss, eval=FALSE-----------------------------------------------------------------------------
# res_i <- otSweep(
#   dat         = demo,
#   outcome     = "LOY",
#   conditions  = c("QUA", "SER", "ENV"),
#   sweep_range = 6:8,
#   thrX        = c(QUA = 7, SER = 7, ENV = 7),
#   include     = "?",
#   dir.exp     = c(1, 1, 1)
# )
# 
# # For every threshold, compare each term of the stored intermediate solution
# # with the parsimonious term(s) contained in it.
# res_fiss <- compute_fiss_core(res_i, conditions = c("QUA", "SER", "ENV"))
# 
# print_fiss_summary(res_fiss, thr_key = "7")           # one threshold
# cat(generate_fiss_chart(res_fiss, symbol_set = "unicode"))
# cat(generate_fiss_chart(res_fiss, symbol_set = "latex"))
# 
# generate_report(res_fiss, "fiss_report.md", dat = demo, format = "full",
#                 include_fiss_core = TRUE)

## ----session-info---------------------------------------------------------------------------------
sessionInfo()

