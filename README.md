# ThSQCA

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17899390.svg)](https://doi.org/10.5281/zenodo.17899390)

ThSQCA is an R package implementing **Threshold-Sweep QCA (TS-QCA)**,  
a framework for systematically varying the thresholds used to binarize  
the outcome and conditions in crisp-set QCA.

After calibration, QCA results may change depending on how thresholds are set.  
TS-QCA evaluates this sensitivity by automatically:

- binarizing the data using many threshold candidates  
- generating truth tables  
- applying `QCA::minimize()`  
- extracting the solution, consistency, and coverage  

Implemented sweep types:

- **CTS-QCA (ctSweepS)**: Sweep the threshold of one X  
- **MCTS-QCA (ctSweepM)**: Sweep thresholds of multiple X conditions  
- **OTS-QCA (otSweep)**: Sweep the threshold of Y only  
- **DTS-QCA (dtSweep)**: Sweep X and Y thresholds simultaneously (2D sweep)

> **Scope:** ThSQCA focuses on **sufficiency analysis**. Necessity analysis is planned for future versions.

---

## Features

### Bug Fix for Intermediate Solutions (v1.2.0)

**Important**: v1.2.0 fixes a critical bug where intermediate solutions were incorrectly extracted when using `dir.exp`. If you used intermediate solutions in previous versions, please re-run your analyses.

### Fit-measure fixes for fuzzy multiple-solution and intermediate cells (v2.0.3–v2.0.4)

**Important**: v2.0.3 and v2.0.4 correct the consistency and coverage (`inclS`/`covS`) reported next to a solution in two situations: when a cell has multiple minimal solutions (which occurs on fuzzy data), and when a cell reports an intermediate solution (`dir.exp`). In earlier versions the displayed formula could be paired with the aggregate fit of all minimal solutions, or with the parsimonious fit, rather than with the fit of the formula actually shown. The affected measure is mainly `covS` (`inclS` differs only marginally). Crisp-set results, single-solution cells, and the solution formulas themselves are unaffected, and these fixes do not change any calculation, only which fit value is read. If you have reported ThSQCA's `covS` for fuzzy multiple-solution or intermediate cells, please re-check those values against the NEWS entries.

### Verification Recommendation for Publications

**For academic publications**, we strongly recommend verifying ThSQCA results directly with the QCA package:

```r
library(QCA)

# Run QCA directly to verify ThSQCA results
tt <- truthTable(dat, outcome = "Y", 
                 conditions = c("X1", "X2", "X3"),
                 incl.cut = 0.8)
sol <- minimize(tt, include = "?", dir.exp = c(1, 1, 1))
print(sol)  # Compare with ThSQCA report output
```

ThSQCA reports include a "QCA Package Output" section for this purpose. Always ensure the solution expressions match before publishing.

### Solution Type Display and Verification (New in v1.2.0)

Reports now explicitly show the **Solution Type** (Complex/Parsimonious/Intermediate) and include optional **QCA package output** for verification:

```r
# Reports now show Solution Type in Analysis Overview
generate_report(result, "report.md", dat = mydata)

# Disable QCA raw output if not needed
generate_report(result, "report.md", dat = mydata, include_raw_output = FALSE)
```

### Three Types of QCA Solutions (New in v1.1.0)

As of v1.1.0, ThSQCA uses the **same defaults** as `QCA::minimize()`:

| Solution Type | `include` | `dir.exp` | Logical Remainders | Description |
|--------------|-----------|-----------|-------------------|-------------|
| **Complex** (default) | `""` | `NULL` | Not used | Most conservative |
| **Parsimonious** | `"?"` | `NULL` | All used | Most simplified |
| **Intermediate** | `"?"` | `c(1,1,...)` | Theory-guided | Most common in publications |

```r
data(sample_data)
thrX <- c(X1 = 7, X2 = 7, X3 = 7)

# 1. Complex Solution (default)
result_comp <- otSweep(dat = sample_data, outcome = "Y", 
                       conditions = c("X1", "X2", "X3"),
                       sweep_range = 7, thrX = thrX)

# 2. Parsimonious Solution
result_pars <- otSweep(dat = sample_data, outcome = "Y",
                       conditions = c("X1", "X2", "X3"),
                       sweep_range = 7, thrX = thrX,
                       include = "?")

# 3. Intermediate Solution (most common in publications)
result_int <- otSweep(dat = sample_data, outcome = "Y",
                      conditions = c("X1", "X2", "X3"),
                      sweep_range = 7, thrX = thrX,
                      include = "?", dir.exp = c(1, 1, 1))
```

> **Migration from v1.0.0**: The default has changed from intermediate to complex solution. To reproduce v1.0.0 behavior, explicitly specify `include = "?"` and `dir.exp = c(1, 1, 1)`.

### Multiple Solution Detection

QCA minimization can produce multiple equivalent intermediate solutions. ThSQCA detects and reports these cases, allowing researchers to identify robust essential prime implicants versus solution-specific selective prime implicants.

Use the `extract_mode` parameter to control output:

| Mode | Description | Use Case |
|------|-------------|----------|
| `"first"` | Returns only the first solution (M1) | Default, backward compatible |
| `"all"` | Returns all solutions concatenated | See all equivalent solutions |
| `"essential"` | Returns essential prime implicants common to all solutions | Identify robust findings |

```r
# Detect and show all solutions
result <- otSweep(
  dat = mydata,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 6:8,
  thrX = c(X1 = 7, X2 = 7, X3 = 7),
  extract_mode = "all"  # Show all solutions
)

# Extract essential prime implicants only
result_essential <- otSweep(
  dat = mydata,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 6:8,
  thrX = c(X1 = 7, X2 = 7, X3 = 7),
  extract_mode = "essential"  # Show essential prime implicants
)
```

### Automatic Report Generation

The `generate_report()` function creates comprehensive markdown reports:

```r
# Run analysis with return_details = TRUE (the default)
result <- otSweep(
  dat = mydata,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 6:8,
  thrX = c(X1 = 7, X2 = 7, X3 = 7)
)

# Generate full report
generate_report(result, "my_analysis.md", dat = mydata, format = "full")

# Generate simple report (for journal manuscripts)
generate_report(result, "my_analysis_simple.md", dat = mydata, format = "simple")
```

Reports include:
- Analysis settings (for reproducibility)
- Solution formulas with essential/selective prime implicants
- Fit measures (consistency, coverage, PRI)
- Cross-threshold comparison tables

### Configuration Charts

Configuration charts are automatically included in reports. Generate Fiss-style configuration charts (Table 5 format):

```r
# Reports include configuration charts by default
generate_report(result, "my_report.md", dat = mydata, format = "full")

# Disable charts if needed
generate_report(result, "my_report.md", dat = mydata, include_chart = FALSE)

# Use LaTeX symbols for academic papers
generate_report(result, "my_report.md", dat = mydata, chart_symbol_set = "latex")
```

Standalone chart functions are also available:

```r
# From path strings
paths <- c("A*B*~C", "A*D")
chart <- config_chart_from_paths(paths)
cat(chart)
```

Output:
```
| Condition | M1 | M2 |
|:--:|:--:|:--:|
| A | ● | ● |
| B | ● |   |
| C | ⊗ |   |
| D |   | ● |
```

Three symbol sets available: `"unicode"` (● / ⊗), `"ascii"` (O / X), `"latex"` ($\bullet$ / $\otimes$)

### S3 Methods

All sweep functions return objects supporting S3 class methods:

- `print()`: Display analysis overview with solution statistics
- `summary()`: Display full results table with parameters

```r
result <- otSweep(...)
print(result)    # Quick overview
summary(result)  # Full details
```

### Negated Outcome Support

Analyze conditions sufficient for the **absence** of an outcome:

```r
# Analyze when Y is low (below threshold)
result <- otSweep(
  dat = mydata,
  outcome = "~Y",  # Tilde prefix for negation
  conditions = c("X1", "X2", "X3"),
  sweep_range = 6:9,
  thrX = c(X1 = 7, X2 = 7, X3 = 7)
)
```

### Terminology Note

ThSQCA uses precise Boolean algebra terminology:

| Term | Meaning |
|------|---------|
| **Essential Prime Implicants (EPI)** | Terms in ALL equivalent solutions (M1, M2...) |
| **Selective Prime Implicants (SPI)** | Terms in SOME solutions only |

> **Note**: This is distinct from Fiss (2011) "Core Conditions" which compares parsimonious vs. intermediate solutions. See `docs/TSQCA_Terminology_Guide_EN.md` for details.

---

## Installation

```r
install.packages("devtools")
devtools::install_github("im-research-yt/ThSQCA")
```

## Relationship with QCA Package

ThSQCA is built on top of the [QCA package](https://cran.r-project.org/package=QCA) (Duşa, 2019). All function arguments follow QCA conventions:

- **`incl.cut`, `n.cut`, `pri.cut`** → See `QCA::truthTable()`
  - Consistency, frequency, and PRI cutoffs for truth table construction
- **`include`, `dir.exp`** → See `QCA::minimize()`
  - Inclusion rule and directional expectations for logical minimization

### Why This Matters

- **Familiar Workflow**: If you know QCA, you already know ThSQCA's parameters
- **Detailed Documentation**: For in-depth parameter explanations, refer to QCA package documentation
- **Seamless Integration**: ThSQCA extends QCA without replacing it

### Quick Reference

```r
# Check QCA parameter documentation
?QCA::truthTable  # For incl.cut, n.cut, pri.cut
?QCA::minimize    # For include, dir.exp

# ThSQCA uses these same parameters
result <- dtSweep(
  dat = sample_data,
  outcome = "Y",
  conditions = c("X1", "X2"),
  sweep_list_X = list(X1 = 6:7, X2 = 6:7),
  sweep_range_Y = 6:7,
  incl.cut = 0.8,   # QCA parameter
  n.cut = 1,        # QCA parameter
  pri.cut = 0       # QCA parameter
)
```

## Basic Setup

```r
library(QCA)
library(ThSQCA)

dat <- read.csv("sample_data.csv", fileEncoding = "UTF-8")

outcome  <- "Y"
conditions <- c("X1", "X2", "X3")

str(dat)
```

## Working with Mixed Data Types

### Handling Binary Variables (0/1)

When your dataset contains **both continuous and binary (0/1) variables**, special attention is needed:

- **Do NOT use threshold sweep for binary variables** — they are already binarized
- **Specify threshold = 1** for binary variables to preserve their original values
- **Explicitly define thresholds** for each variable in `sweep_list`

#### Why Threshold = 1 for Binary Variables?

The `qca_bin()` function uses `x >= thr` for binarization:
- If `x = 0`: `0 >= 1` → FALSE → **0** (preserved)
- If `x = 1`: `1 >= 1` → TRUE → **1** (preserved)

#### Example: Mixed Data

```r
# Suppose X1 is binary (0/1), while X2 and X3 are continuous (0-10)
sweep_list <- list(
  X1 = 1,      # Binary variable: use threshold 1 to preserve values
  X2 = 6:8,    # Continuous: sweep thresholds
  X3 = 6:8     # Continuous: sweep thresholds
)

res_mcts <- ctSweepM(
  dat = dat,
  outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_list = sweep_list,
  thrY = 7
)
```

This explores 1 × 3 × 3 = 9 threshold combinations, treating X1 as a fixed binary condition.

#### Common Mistake

```r
# WRONG: Using sweep range for binary variables
sweep_list <- list(
  X1 = 6:8,    # All values become 0 (since 0 < 6 and 1 < 6)
  X2 = 6:8,
  X3 = 6:8
)
```

**Best Practice**: Always specify thresholds explicitly for each variable based on its data type.

## Usage Examples

### 1. CTS-QCA: single-condition X sweep (ctSweepS)

```r
sweep_var <- "X3"      # Condition (X) whose threshold will be varied
sweep_range <- 6:9     # Candidate threshold values

thrY <- 7              # Fixed threshold for Y
thrX_default <- 7      # Fixed thresholds for other X's

res_cts <- ctSweepS(
  dat            = dat,
  outcome        = outcome,
  conditions     = conditions,
  sweep_var      = sweep_var,      # X to sweep
  sweep_range    = sweep_range,    # Threshold candidates for X
  thrY           = thrY,           # Fixed Y threshold
  thrX_default   = thrX_default    # Fixed thresholds for other X's
)

summary(res_cts)
```

### 2. MCTS-QCA: multi-condition X sweep (ctSweepM)

```r
# Threshold candidates for each X
sweep_list <- list(
  X1 = 6:7,
  X2 = 6:7,
  X3 = 6:7
)

res_mcts <- ctSweepM(
  dat            = dat,
  outcome        = outcome,
  conditions     = conditions,
  sweep_list     = sweep_list,     # Threshold candidates for each X
  thrY           = 7               # Fixed Y threshold
)

summary(res_mcts)
```

### 3. OTS-QCA: outcome Y sweep (otSweep)

```r
thrX <- c(X1 = 7, X2 = 7, X3 = 7)  # Fixed thresholds for X
sweep_range_Y <- 6:8               # Candidate thresholds for Y

res_ots <- otSweep(
  dat            = dat,
  outcome        = outcome,
  conditions     = conditions,
  sweep_range    = sweep_range_Y,  # Y threshold candidates
  thrX           = thrX            # Fixed X thresholds
)

summary(res_ots)

# Generate report for detailed analysis
generate_report(res_ots, "ots_report.md", dat = dat, format = "full")
```

### 4. DTS-QCA: 2D sweep of X and Y (dtSweep)

```r
# X-side threshold candidates (multiple conditions)
sweep_list_X <- list(
  X1 = 6:7,
  X2 = 6:7,
  X3 = 6:7
)

# Y-side threshold candidates
sweep_range_Y <- 6:7

res_dts <- dtSweep(
  dat            = dat,
  outcome        = outcome,
  conditions     = conditions,
  sweep_list_X   = sweep_list_X,   # X threshold candidates
  sweep_range_Y  = sweep_range_Y   # Y threshold candidates
)

summary(res_dts)
```

## Sample Data

```r
data(sample_data)
str(sample_data)
```

## References

### Core QCA Methodology

- Ragin, C. C. (2008). *Redesigning Social Inquiry: Fuzzy Sets and Beyond*. University of Chicago Press. [DOI: 10.7208/chicago/9780226702797.001.0001](https://doi.org/10.7208/chicago/9780226702797.001.0001)
- Schneider, C. Q., & Wagemann, C. (2012). *Set-Theoretic Methods for the Social Sciences: A Guide to Qualitative Comparative Analysis*. Cambridge University Press. [DOI: 10.1017/CBO9781139004244](https://doi.org/10.1017/CBO9781139004244)

### QCA R Package

- Duşa, A. (2019). *QCA with R: A Comprehensive Resource*. Springer. [DOI: 10.1007/978-3-319-75668-4](https://doi.org/10.1007/978-3-319-75668-4)
- Duşa, A. (2018). Consistency Cubes: A Fast, Efficient Method for Exact Boolean Minimization. *The R Journal*, 10(2), 357–370. [DOI: 10.32614/RJ-2018-080](https://doi.org/10.32614/RJ-2018-080)
- Duşa, A. (2024). *QCA: Qualitative Comparative Analysis*. R package version 3.25. https://CRAN.R-project.org/package=QCA

### Robustness and Threshold Sensitivity

- Oana, I.-E., & Schneider, C. Q. (2024). A Robustness Test Protocol for Applied QCA: Theory and R Software Application. *Sociological Methods & Research*, 53(1), 57–88. [DOI: 10.1177/00491241211036158](https://doi.org/10.1177/00491241211036158)
- Oana, I.-E., & Schneider, C. Q. (2018). SetMethods: An Add-on R Package for Advanced QCA. *The R Journal*, 10(1), 507–533. [DOI: 10.32614/RJ-2018-031](https://doi.org/10.32614/RJ-2018-031)
- Skaaning, S.-E. (2011). Assessing the Robustness of Crisp-Set and Fuzzy-Set QCA Results. *Sociological Methods & Research*, 40(2), 391–408. [DOI: 10.1177/0049124111404818](https://doi.org/10.1177/0049124111404818)
- Thiem, A., Spöhel, R., & Duşa, A. (2016). Enhancing Sensitivity Diagnostics for Qualitative Comparative Analysis: A Combinatorial Approach. *Political Analysis*, 24(1), 104–120. [DOI: 10.1093/pan/mpv028](https://doi.org/10.1093/pan/mpv028)

## Acknowledgements

This package implements methods developed in research supported by JSPS KAKENHI Grant Number JP20K01998.

## License

MIT License.
