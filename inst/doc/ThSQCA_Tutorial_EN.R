## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ThSQCA)
data(sample_data)
thrX <- c(X1 = 7, X2 = 7, X3 = 7)

# 1. Complex Solution (default, most conservative)
result_comp <- otSweep(
  dat = sample_data,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 7,
  thrX = thrX
  # include = "" (default), dir.exp = NULL (default)
)
cat("Complex Solution:", result_comp$summary$expression, "\n")

# 2. Parsimonious Solution (uses all logical remainders)
result_pars <- otSweep(
  dat = sample_data,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 7,
  thrX = thrX,
  include = "?"  # Include logical remainders
)
cat("Parsimonious Solution:", result_pars$summary$expression, "\n")

# 3. Intermediate Solution (theory-guided remainders)
result_int <- otSweep(
  dat = sample_data,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 7,
  thrX = thrX,
  include = "?",
  dir.exp = c(1, 1, 1)  # All conditions expected to contribute positively
)
cat("Intermediate Solution:", result_int$summary$expression, "\n")

## ----eval=FALSE---------------------------------------------------------------
# # v1.0.0 (intermediate solution by default due to bug)
# result <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX)
# 
# # v1.1.0: To get the same result as v1.0.0, explicitly specify:
# result <- otSweep(
#   dat = sample_data,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 7,
#   thrX = thrX,
#   include = "?",
#   dir.exp = c(1, 1, 1)  # Explicit intermediate solution
# )

## -----------------------------------------------------------------------------
library(ThSQCA)
data("sample_data")
dat <- sample_data
str(dat)

## -----------------------------------------------------------------------------
outcome  <- "Y"
conditions <- c("X1", "X2", "X3")

## ----eval=FALSE---------------------------------------------------------------
# # CORRECT: Specify threshold explicitly for each variable
# sweep_list <- list(
#   X1 = 1,      # Binary variable: use threshold 1
#   X2 = 6:8,    # Continuous: sweep thresholds
#   X3 = 6:8     # Continuous: sweep thresholds
# )
# 
# # Using intermediate solution (most common in publications)
# res_mixed <- ctSweepM(
#   dat            = dat,
#   outcome        = "Y",
#   conditions     = c("X1", "X2", "X3"),
#   sweep_list     = sweep_list,
#   thrY           = 7,
#   include        = "?",           # Include logical remainders
#   dir.exp        = c(1, 1, 1)     # Directional expectations (intermediate)
# )

## ----eval=FALSE---------------------------------------------------------------
# # WRONG: Using sweep range for binary variables
# sweep_list <- list(
#   X1 = 6:8,    # All values become 0 (since 0 < 6 and 1 < 6)
#   X2 = 6:8,
#   X3 = 6:8
# )

## ----eval=FALSE---------------------------------------------------------------
# # Check variable ranges
# summary(dat[, c("X1", "X2", "X3")])
# 
# # Identify binary variables (only 0 and 1)
# sapply(dat[, c("X1", "X2", "X3")], function(x) {
#   unique_vals <- sort(unique(x))
#   if (length(unique_vals) == 2 && all(unique_vals == c(0, 1))) {
#     "Binary (use threshold = 1)"
#   } else {
#     paste("Continuous (range:", min(x), "-", max(x), ")")
#   }
# })

## ----error=TRUE---------------------------------------------------------------
try({
sweep_var   <- "X3"   # Condition (X) whose threshold is swept
sweep_range <- 6:9    # Candidate threshold values to evaluate

thrY         <- 7     # Outcome (Y) threshold (fixed)
thrX_default <- 7     # Threshold for other X conditions (fixed)

# Default: Complex solution (include = "", dir.exp = NULL)
res_cts <- ctSweepS(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_var      = sweep_var,
  sweep_range    = sweep_range,
  thrY           = thrY,
  thrX_default   = thrX_default,
  return_details = TRUE
)

summary(res_cts)
})

## ----error=TRUE---------------------------------------------------------------
try({
# Intermediate solution: specify include = "?" and dir.exp
res_cts_int <- ctSweepS(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_var      = sweep_var,
  sweep_range    = sweep_range,
  thrY           = thrY,
  thrX_default   = thrX_default,
  include        = "?",           # Include logical remainders
  dir.exp        = c(1, 1, 1),    # Directional expectations
  return_details = TRUE
)

summary(res_cts_int)
})

## ----error=TRUE---------------------------------------------------------------
try({
# Create a sweep list specifying thresholds for each condition
sweep_list <- list(
  X1 = 6:7,
  X2 = 6:7,
  X3 = 6:7
)

# Default: Complex solution
res_mcts <- ctSweepM(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_list     = sweep_list,
  thrY           = 7,
  return_details = TRUE
)

summary(res_mcts)
})

## ----error=TRUE---------------------------------------------------------------
try({
# Intermediate solution: specify include = "?" and dir.exp
res_mcts_int <- ctSweepM(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_list     = sweep_list,
  thrY           = 7,
  include        = "?",
  dir.exp        = c(1, 1, 1),
  return_details = TRUE
)

summary(res_mcts_int)
})

## -----------------------------------------------------------------------------
# Default: Complex solution
res_ots <- otSweep(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_range    = 6:8,
  thrX           = c(X1 = 7, X2 = 7, X3 = 7),
  return_details = TRUE
)

summary(res_ots)

## -----------------------------------------------------------------------------
# Intermediate solution: specify include = "?" and dir.exp
res_ots_int <- otSweep(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_range    = 6:8,
  thrX           = c(X1 = 7, X2 = 7, X3 = 7),
  include        = "?",
  dir.exp        = c(1, 1, 1),
  return_details = TRUE
)

summary(res_ots_int)

## -----------------------------------------------------------------------------
sweep_list_dts_X <- list(
  X1 = 6:7,
  X2 = 6:7,
  X3 = 6:7
)

sweep_range_dts_Y <- 6:7

# Default: Complex solution
res_dts <- dtSweep(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_list_X   = sweep_list_dts_X,
  sweep_range_Y  = sweep_range_dts_Y,
  return_details = TRUE
)

summary(res_dts)

## -----------------------------------------------------------------------------
# Intermediate solution: specify include = "?" and dir.exp
res_dts_int <- dtSweep(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_list_X   = sweep_list_dts_X,
  sweep_range_Y  = sweep_range_dts_Y,
  include        = "?",
  dir.exp        = c(1, 1, 1),
  return_details = TRUE
)

summary(res_dts_int)

## ----eval=FALSE---------------------------------------------------------------
# # Returns only the first solution (M1)
# # Backward compatible with v0.1.x
# result <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = c(X1 = 7, X2 = 7, X3 = 7),
#   extract_mode = "first"  # Default
# )

## ----eval=FALSE---------------------------------------------------------------
# # Returns all solutions concatenated
# # Useful for seeing all equivalent solutions
# result_all <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = c(X1 = 7, X2 = 7, X3 = 7),
#   extract_mode = "all"
# )
# 
# # Output includes n_solutions column
# head(result_all$summary)
# # expression column shows: "M1: A*B + C; M2: A*B + D; M3: ..."

## ----eval=FALSE---------------------------------------------------------------
# # Returns essential prime implicants (terms common to all solutions)
# # Best for identifying robust findings
# result_essential <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = c(X1 = 7, X2 = 7, X3 = 7),
#   extract_mode = "essential"
# )
# 
# # Output includes:
# # - expression: essential prime implicants
# # - selective_terms: terms in some but not all solutions
# # - unique_terms: solution-specific terms
# # - n_solutions: number of equivalent solutions

## ----eval=FALSE---------------------------------------------------------------
# # Generate full report
# generate_report(res_ots, "my_analysis_full.md", dat = dat, format = "full")

## ----eval=FALSE---------------------------------------------------------------
# # Generate simple report
# generate_report(res_ots, "my_analysis_simple.md", dat = dat, format = "simple")

## ----eval=FALSE---------------------------------------------------------------
# # Complete workflow
# result <- otSweep(
#   dat = mydata,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = c(X1 = 7, X2 = 7, X3 = 7)
# )
# 
# # For main text
# generate_report(result, "manuscript_results.md", dat = mydata, format = "simple")
# 
# # For supplementary materials
# generate_report(result, "supplementary_full.md", dat = mydata, format = "full")

## ----eval=FALSE---------------------------------------------------------------
# # View stored parameters
# result$params
# 
# # Includes:
# # - outcome, conditions: variable names
# # - thrX, thrY: threshold values
# # - incl.cut, n.cut, pri.cut: QCA parameters
# # - dir.exp, include: minimization settings

## ----eval=FALSE---------------------------------------------------------------
# # Test with a single threshold first
# result <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 7,  # Single value
#   thrX = c(X1 = 7, X2 = 7, X3 = 7)
# )

## ----eval=FALSE---------------------------------------------------------------
# # Expand to a small range
# result <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:7,  # Small range
#   thrX = c(X1 = 7, X2 = 7, X3 = 7)
# )

## ----eval=FALSE---------------------------------------------------------------
# # Run full analysis only after confirming it works
# result <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 5:9,  # Full range
#   thrX = c(X1 = 7, X2 = 7, X3 = 7)
# )

## ----eval=FALSE---------------------------------------------------------------
# # Manageable: 2 × 2 × 2 = 8 combinations
# sweep_list <- list(X1 = 6:7, X2 = 6:7, X3 = 6:7)
# 
# # Caution: 5 × 5 × 5 = 125 combinations
# sweep_list <- list(X1 = 5:9, X2 = 5:9, X3 = 5:9)
# 
# # Avoid: 5 × 5 × 5 × 5 × 5 = 3125 combinations!
# sweep_list <- list(X1 = 5:9, X2 = 5:9, X3 = 5:9, X4 = 5:9, X5 = 5:9)

## ----eval=FALSE---------------------------------------------------------------
# # Analyze conditions for Y >= threshold (standard)
# result_Y <- otSweep(
#   dat = dat,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = c(X1 = 7, X2 = 7, X3 = 7)
# )
# 
# # Analyze conditions for Y < threshold (negated)
# result_negY <- otSweep(
#   dat = dat,
#   outcome = "~Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = c(X1 = 7, X2 = 7, X3 = 7)
# )

## ----eval=FALSE---------------------------------------------------------------
# # ctSweepS with negated outcome
# result <- ctSweepS(
#   dat = dat,
#   outcome = "~Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_var = "X3",
#   sweep_range = 6:8,
#   thrY = 7,
#   thrX_default = 7
# )
# 
# # ctSweepM with negated outcome
# result <- ctSweepM(
#   dat = dat,
#   outcome = "~Y",
#   conditions = c("X1", "X2"),
#   sweep_list = list(X1 = 6:7, X2 = 6:7),
#   thrY = 7
# )
# 
# # dtSweep with negated outcome
# result <- dtSweep(
#   dat = dat,
#   outcome = "~Y",
#   conditions = c("X1", "X2"),
#   sweep_list_X = list(X1 = 6:7, X2 = 7),
#   sweep_range_Y = 6:8
# )

## ----eval=FALSE---------------------------------------------------------------
# result <- otSweep(dat = dat, outcome = "~Y", ...)
# 
# # Check if negated
# result$params$negate_outcome
# # [1] TRUE
# 
# result$params$outcome
# # [1] "~Y"

## ----eval=FALSE---------------------------------------------------------------
# library(ThSQCA)
# data(sample_data)
# 
# # Step 1: Run intermediate sweep (all three conditions above are met)
# res <- otSweep(
#   dat        = sample_data,
#   outcome    = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX       = c(X1 = 7, X2 = 7, X3 = 7),
#   include    = "?",           # required
#   dir.exp    = c(1, 1, 1),   # required
#   return_details = TRUE       # required
# )
# 
# # Step 2: Compute Fiss core/peripheral classification
# # For each threshold, compute_fiss_core() re-runs QCA::minimize()
# # with dir.exp = NULL to obtain the parsimonious solution,
# # then compares it to the stored intermediate solution.
# res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))
# 
# # Step 3: Inspect a specific threshold
# print_fiss_summary(res_fiss, thr_key = "7")
# 
# # Step 4: Generate Fiss-style four-symbol chart
# cat(generate_fiss_chart(res_fiss, symbol_set = "unicode"))
# 
# # For academic papers (LaTeX)
# cat(generate_fiss_chart(res_fiss, symbol_set = "latex"))
# 
# # Step 5: Include in report
# generate_report(
#   res_fiss,
#   output_file      = "fiss_report.md",
#   format           = "full",
#   include_fiss_core = TRUE,          # activates four-symbol charts
#   chart_symbol_set  = "unicode"
# )

## ----eval=FALSE---------------------------------------------------------------
# # Generate report with configuration charts (default)
# generate_report(result, "my_report.md", dat = dat, format = "full")
# 
# # Disable charts if needed
# generate_report(result, "my_report.md", dat = dat, include_chart = FALSE)
# 
# # Use LaTeX symbols for academic papers
# generate_report(result, "my_report.md", dat = dat, chart_symbol_set = "latex")
# 
# # Include QCA package raw output for verification (default is TRUE)
# generate_report(result, "my_report.md", dat = dat, include_raw_output = TRUE)
# 
# # Disable raw output if not needed
# generate_report(result, "my_report.md", dat = dat, include_raw_output = FALSE)

## -----------------------------------------------------------------------------
# From path strings
paths <- c("A*B*~C", "A*D", "B*E")
chart <- config_chart_from_paths(paths)
cat(chart)

## -----------------------------------------------------------------------------
# ASCII (maximum compatibility)
cat(config_chart_from_paths(paths, symbol_set = "ascii"))

## ----eval=FALSE---------------------------------------------------------------
# # LaTeX (for PDF/academic papers)
# cat(config_chart_from_paths(paths, symbol_set = "latex"))
# # Output: $\bullet$ for presence, $\otimes$ for absence

## -----------------------------------------------------------------------------
solutions <- list(
  c("A*B", "C*D"),
  c("A*B", "C*E")
)
chart <- config_chart_multi_solutions(solutions)
cat(chart)

## ----eval=FALSE---------------------------------------------------------------
# library(QCA)
# library(ThSQCA)
# 
# # Step 1: Run ThSQCA analysis
# data(sample_data)
# thrX <- c(X1 = 7, X2 = 7, X3 = 7)
# 
# result <- otSweep(
#   dat = sample_data,
#   outcome = "Y",
#   conditions = c("X1", "X2", "X3"),
#   sweep_range = 6:8,
#   thrX = thrX,
#   include = "?",
#   dir.exp = c(1, 1, 1),
#   return_details = TRUE
# )
# 
# # Step 2: Generate report with QCA output for verification
# generate_report(result, "my_analysis.md", dat = sample_data,
#                 include_raw_output = TRUE)  # Default is TRUE
# 
# # Step 3: Verify key results directly with QCA package
# # For each important threshold, run QCA::minimize() directly
# dat_bin <- sample_data
# dat_bin$Y_bin <- ifelse(sample_data$Y >= 7, 1, 0)
# dat_bin$X1_bin <- ifelse(sample_data$X1 >= 7, 1, 0)
# dat_bin$X2_bin <- ifelse(sample_data$X2 >= 7, 1, 0)
# dat_bin$X3_bin <- ifelse(sample_data$X3 >= 7, 1, 0)
# 
# tt <- truthTable(dat_bin, outcome = "Y_bin",
#                  conditions = c("X1_bin", "X2_bin", "X3_bin"),
#                  incl.cut = 0.8)
# sol <- minimize(tt, include = "?", dir.exp = c(1, 1, 1))
# print(sol)  # Compare with ThSQCA report

## -----------------------------------------------------------------------------
sessionInfo()

