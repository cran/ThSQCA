## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ThSQCA)
library(QCA)

## -----------------------------------------------------------------------------
# Adjust the file name as needed
library(ThSQCA)
data("sample_data")
dat <- sample_data

# Outcome and conditions
outcome  <- "Y"
conditions <- c("X1", "X2", "X3")

# Quick inspection
str(dat)
summary(dat)

## -----------------------------------------------------------------------------
thrY_base <- 7
thrX_base <- 7

# Fixed X thresholds (for OTS)
thrX_vec <- c(
  X1 = thrX_base,
  X2 = thrX_base,
  X3 = thrX_base
)
thrX_vec

## -----------------------------------------------------------------------------
# Example: Three solution types at single threshold
thrX <- c(X1 = 7, X2 = 7, X3 = 7)

# Complex (default)
res_comp <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX)
cat("Complex:", res_comp$summary$expression, "\n")

# Parsimonious
res_pars <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX,
                    include = "?")
cat("Parsimonious:", res_pars$summary$expression, "\n")

# Intermediate
res_int <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX,
                   include = "?", dir.exp = c(1, 1, 1))
cat("Intermediate:", res_int$summary$expression, "\n")

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
  sweep_var      = "X3",
  sweep_range    = 6:9,
  thrY           = 7,
  thrX_default   = 7,
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
  sweep_var      = "X3",
  sweep_range    = 6:9,
  thrY           = 7,
  thrX_default   = 7,
  include        = "?",
  dir.exp        = c(1, 1, 1),
  return_details = TRUE
)

summary(res_cts_int)
})

## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_cts$summary, file = "ThSQCA_CTS_results.csv", row.names = FALSE)

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

## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_mcts$summary, file = "ThSQCA_CTS_multi_results.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
sweep_range_ots <- 6:8

# Default: Complex solution
res_ots <- otSweep(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_range    = sweep_range_ots,
  thrX           = thrX_vec,
  return_details = TRUE
)

summary(res_ots)

## -----------------------------------------------------------------------------
# Intermediate solution: specify include = "?" and dir.exp
res_ots_int <- otSweep(
  dat            = dat,
  outcome        = "Y",
  conditions     = c("X1", "X2", "X3"),
  sweep_range    = sweep_range_ots,
  thrX           = thrX_vec,
  include        = "?",
  dir.exp        = c(1, 1, 1),
  return_details = TRUE
)

summary(res_ots_int)

## ----eval=FALSE---------------------------------------------------------------
# write.csv(res_ots$summary, file = "ThSQCA_OTS_results.csv", row.names = FALSE)

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
  dir.exp        = c(1, 1, 1),
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
# write.csv(res_dts$summary, file = "ThSQCA_DTS_results.csv", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# res_all <- otSweep(
#   dat            = dat,
#   outcome        = "Y",
#   conditions     = c("X1", "X2", "X3"),
#   sweep_range    = 6:8,
#   thrX           = thrX_vec,
#   include        = "?",           # Include logical remainders
#   dir.exp        = c(1, 1, 1),    # Intermediate solution
#   extract_mode   = "all",
#   return_details = TRUE
# )
# 
# # View results with n_solutions column
# head(res_all$summary)

## ----eval=FALSE---------------------------------------------------------------
# res_essential <- otSweep(
#   dat            = dat,
#   outcome        = "Y",
#   conditions     = c("X1", "X2", "X3"),
#   sweep_range    = 6:8,
#   thrX           = thrX_vec,
#   include        = "?",           # Include logical remainders
#   dir.exp        = c(1, 1, 1),    # Intermediate solution
#   extract_mode   = "essential",
#   return_details = TRUE
# )
# 
# # View results with essential prime implicants, selective terms, and unique terms
# head(res_essential$summary)

## ----eval=FALSE---------------------------------------------------------------
# generate_report(res_ots, "ThSQCA_OTS_report_full.md", dat = dat, format = "full")

## ----eval=FALSE---------------------------------------------------------------
# generate_report(res_ots, "ThSQCA_OTS_report_simple.md", dat = dat, format = "simple")

## ----eval=FALSE---------------------------------------------------------------
# # Standard: conditions for Y >= threshold (intermediate solution)
# res_Y <- otSweep(
#   dat            = dat,
#   outcome        = "Y",
#   conditions     = c("X1", "X2", "X3"),
#   sweep_range    = 6:8,
#   thrX           = thrX_vec,
#   include        = "?",           # Include logical remainders
#   dir.exp        = c(1, 1, 1)     # Intermediate solution
# )
# 
# # Negated: conditions for Y < threshold (intermediate solution)
# res_negY <- otSweep(
#   dat            = dat,
#   outcome        = "~Y",
#   conditions     = c("X1", "X2", "X3"),
#   sweep_range    = 6:8,
#   thrX           = thrX_vec,
#   include        = "?",           # Include logical remainders
#   dir.exp        = c(1, 1, 1)     # Intermediate solution
# )
# 
# # Compare results
# res_Y$summary
# res_negY$summary
# 
# # Check negation flag
# res_negY$params$negate_outcome
# # [1] TRUE

## ----eval=FALSE---------------------------------------------------------------
# # View stored parameters
# res_ots$params
# 
# # Example output:
# # $outcome
# # [1] "Y"
# # $conditions
# # [1] "X1" "X2" "X3"
# # $thrX
# # X1 X2 X3
# #  7  7  7
# # $incl.cut
# # [1] 0.8
# # $n.cut
# # [1] 1
# # $pri.cut
# # [1] 0

## -----------------------------------------------------------------------------
# From path strings
paths <- c("A*B*~C", "A*D")
chart <- config_chart_from_paths(paths)
cat(chart)

## ----eval=FALSE---------------------------------------------------------------
# # Charts are included by default
# generate_report(result, "report.md", dat = dat, format = "full")
# 
# # Use LaTeX symbols for academic papers
# generate_report(result, "report.md", dat = dat, chart_symbol_set = "latex")

## ----eval=FALSE---------------------------------------------------------------
# # Requires: include = "?", dir.exp specified, return_details = TRUE
# # res_ots_int was created in Section 6 above.
# 
# res_fiss <- compute_fiss_core(
#   res_ots_int,
#   conditions = c("X1", "X2", "X3")
# )
# 
# # Inspect available thresholds
# names(res_fiss$fiss_core)

## ----eval=FALSE---------------------------------------------------------------
# # English
# print_fiss_summary(res_fiss, thr_key = "7")
# 
# # Japanese
# print_fiss_summary(res_fiss, thr_key = "7", language = "ja")

## ----eval=FALSE---------------------------------------------------------------
# # Unicode (Markdown / screen)
# cat(generate_fiss_chart(res_fiss, symbol_set = "unicode"))
# 
# # LaTeX (for PDF/journal submission)
# cat(generate_fiss_chart(res_fiss, symbol_set = "latex"))
# 
# # ASCII (maximum compatibility)
# cat(generate_fiss_chart(res_fiss, symbol_set = "ascii"))

## ----eval=FALSE---------------------------------------------------------------
# # Full report with Fiss four-symbol charts
# generate_report(
#   res_fiss,
#   output_file       = "ThSQCA_Fiss_report.md",
#   format            = "full",
#   dat               = dat,
#   include_fiss_core = TRUE,       # activates four-symbol charts
#   chart_symbol_set  = "unicode",
#   include_raw_output = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# # Access raw classification data frame for a given threshold
# classif_7 <- res_fiss$fiss_core[["7"]]$classification
# classif_7
# 
# # Filter core conditions only
# classif_7[classif_7$type == "core" & classif_7$status != "dontcare", ]
# 
# # Filter peripheral conditions only
# classif_7[classif_7$type == "peripheral" & classif_7$status != "dontcare", ]
# 
# # Parsimonious and intermediate expressions
# res_fiss$fiss_core[["7"]]$parsim_expression
# res_fiss$fiss_core[["7"]]$interm_expression

## -----------------------------------------------------------------------------
sessionInfo()

