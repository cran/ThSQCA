# ThSQCA 2.0.7

## Documentation

* `citation("ThSQCA")` now also lists the accompanying preprint (Toyoda, 2026,
  SocArXiv, <doi:10.31235/osf.io/yb8xs_v1>), which is also added to the README
  and to the reference list of the tutorial vignette.
* Help page titles now use the ThS-QCA labels ("OTS", "CTS (single)",
  "CTS (multiple)", "DTS") instead of "OTS-QCA", "CTS-QCA", "MCTS-QCA" and
  "DTS-QCA", and the package overview page describes results in terms of
  sufficiency structures and threshold transitions rather than causal
  structures.
* `ThSQCA_Tutorial_EN` was reorganized and extended: a quick start, data
  preparation (binary variables, `pre_calibrated`, sweeping membership scores),
  a section on choosing between complex, parsimonious and intermediate
  solutions and one on multiple minimal solutions (both with a second simulated
  data set that has logical remainders), guidance on reporting, and answers to
  common questions. Version-history headings were removed.
* Unified terminology with the companion methodology paper: the framework is
  now called Threshold-Sweep QCA (ThS-QCA), and the sweeps are referred to as
  CTS (`ctSweepS()` for one condition, `ctSweepM()` for several conditions),
  OTS (`otSweep()`), and DTS (`dtSweep()`). The former labels "TS-QCA" and
  "MCTS-QCA" were removed from the README and the vignettes. No function names,
  arguments, or results changed.
* README: added links to the two browser-based helpers (Sweep Builder and QCA
  Quickstart) and to the tutorial vignettes, added `install.packages("ThSQCA")`
  as the primary installation route, and made the "Basic Setup" example use the
  bundled `sample_data` so that it runs as written. Fixed two bullet lists that
  were rendered as running text on CRAN.
* Vignettes: example output files in `ThSQCA_Reproducible_EN` are now named
  `ThSQCA_*` instead of `TSQCA_*`; `TSQCA_MCTS_results.csv` became
  `ThSQCA_CTS_multi_results.csv`. Added a pointer to Sweep Builder in
  `ThSQCA_Tutorial_EN`.

## Bug fixes and output changes

* Sweeps no longer print one stray blank line for every "No solution" cell.
  The calls to `QCA::truthTable()` and `QCA::minimize()` inside the sweep
  loops now go through an internal helper, `quiet_try()`, which behaves like
  `try(..., silent = TRUE)` but also discards console output written by the
  QCA call itself. Warnings are passed on unchanged (they are collected while
  the output is diverted and signalled again afterwards). Results are unchanged.
* A `pre_calibrated` variable that contains memberships of exactly 0.5 now
  produces one warning per sweep, naming the variable and the number of cases.
  Previously QCA's own warning ("Fuzzy causal conditions should not have values
  of 0.5 in the data") was repeated for every cell of the sweep (twice per cell)
  without saying which variable was affected; that per-cell warning is no longer
  repeated.
* Printed and summarised results now use the ThS-QCA labels (OTS, DTS,
  CTS (single), CTS (multiple)) instead of "OTS-QCA", "DTS-QCA", "CTS-QCA" and
  "MCTS-QCA". The S3 class names (`otSweep_result`, `tsqca_result`, and so on)
  are unchanged, so existing code that dispatches on them keeps working.
* The Necessity Analysis table in full reports no longer shows a leading space
  in some condition names (for example `" ~TRU"`).
* The verification code at the end of a report now reproduces the solution
  type that was actually used (`minimize(tt)`, `minimize(tt, include = "?")` or
  `minimize(tt, include = "?", dir.exp = c(...))` with your values) instead of
  a placeholder.
* `generate_report()` now shows the outcome name you supplied (for example
  `LOY`, or `~LOY` for a negated outcome) in solution formulas, in the
  captured QCA console output, and in the "Solutions Overview" headings, where
  it previously showed the internal column name `Y`. The verification snippet at
  the end of each report now names your outcome and conditions and uses your
  `incl.cut`. Internally the outcome column is still called `Y`.
* The documentation no longer recommends sweeping variables only on their raw
  scale. Membership scores can be swept like any other numeric variable (leave
  them out of `pre_calibrated` and give thresholds on the 0 to 1 scale); the
  tutorial has a new subsection on this. The warning for a variable that is
  both in `pre_calibrated` and in a sweep list now says how to sweep it
  (remove it from `pre_calibrated`) and no longer points to a vignette section
  that does not exist. The rule itself is unchanged: a `pre_calibrated`
  variable is used as it is and its sweep thresholds are ignored.
* The warning issued when a sweep finds more than one equivalent solution now
  reads "Multiple equivalent solutions exist ..." (it used to say "intermediate
  solutions" even for complex and parsimonious runs, where it also applies).

# ThSQCA 2.0.6

## Bug fixes

* **`get_n_solutions()` no longer double-counts models when the intermediate
  solution spans multiple prime implicant charts.** When `dir.exp` produces
  more than one prime implicant chart (QCA's own chart indexing, visible as
  `"From C1P1, C2P1:"` in `print()`, as opposed to multiple parsimonious
  paths within a single chart, `"From C1P1, C1P2:"`), `sol$i.sol` holds one
  list entry per chart/path combination. Each entry's own `$solution` field
  is computed independently by `QCA::minimize()` and is not guaranteed to be
  chart-exclusive: depending on the data, it may already enumerate the full,
  cross-chart set of tied minimal models. Summing `length($solution)` across
  every chart name, as `get_n_solutions()` previously did, could therefore
  double- (or *N*-fold) count identical models. This was visible as an
  inflated `n_solutions` in `res$summary` and in the Summary Table of
  `generate_report()`'s Markdown output; the displayed solution formula
  itself was already correct. A new internal helper,
  `collect_unique_i_sol()`, performs the same enumeration and then
  deduplicates by comparing each model's term set (order-independent), so
  structurally identical models are counted once regardless of which
  chart(s) produced them. `generate_report()`'s two `sol_list`-construction
  blocks (used for the "Full Solutions" listing and as input to
  `identify_epi()`) were updated to use the same helper; the `identify_epi()`
  EPI/SPI content itself was unaffected by the duplication (set operations
  are insensitive to duplicate entries), only the displayed solution count
  was wrong. Cells with a single prime implicant chart, which are the common
  case, are unaffected.
* The standalone `identify_epi()` function itself was audited and confirmed
  correct: it is a pure function over its `solutions` argument and does not
  read `sol$solution` internally. All three internal call sites already
  built their input via the `sol$i.sol` traversal above rather than a naive
  `sol$solution` read; the double-counting above was the only defect found
  in that path. Passing `sol$solution` (top level) directly to
  `identify_epi()`, as opposed to a `sol_list` built via `sol$i.sol`, is
  still not meaningful when `dir.exp` is specified and should be avoided:
  that slot holds the parsimonious solution's terms in that case, not the
  intermediate solution's.

* **`qca_extract(extract_mode = "all" | "essential")` now summarizes across the
  same set of solutions that `n_solutions` counts.** These two modes derived
  their term sets from `sol$i.sol$C1P1$solution`, that is, from the first prime
  implicant chart only. That slot is the right basis for
  `extract_mode = "first"` (its first entry is the displayed M1), but whether it
  enumerates every chart's model or only its own is an implementation detail of
  `QCA::minimize()`'s internal `getSolution()` call rather than a documented
  guarantee. Both modes now build their model list with
  `collect_unique_i_sol()`, the same enumeration `get_n_solutions()` counts, so
  the reported EPI/SPI terms and the reported solution count can no longer
  describe different sets. On data where the first chart already enumerated
  every model, which includes all cases checked here, the output is unchanged.

* **The same duplication defect was found and fixed in two further places.** A
  follow-up audit of every `sol$i.sol` traversal in the package turned up two
  more sites built on the same chart-by-chart concatenation:
  `extract_solution_list()` (used by `generate_config_chart()`), where the
  inflated count made the chart announce too many equivalent solutions and emit
  one identical table per duplicate; and `extract_sol_terms_by_model()` (used by
  `compute_fiss_core()`), where it inflated the reported
  `parsim_n_solutions` and the tie warning derived from it. The
  core/peripheral classification itself was not affected, because it counts a
  status only where every minimal solution agrees, which is insensitive to
  duplicates. Both now deduplicate by term-set content. `extract_all_metrics()`
  reads the first chart's `IC` deliberately, to match the displayed M1, and was
  left as it is.

## API consistency and usability

* **`ctSweepM()` gained a `thrX_default` argument and no longer fails
  silently.** A condition present in `conditions` but absent from both
  `sweep_list` and `pre_calibrated` previously had no column created at all,
  which made `QCA::truthTable()` fail and returned `"No solution"` for every
  cell with no error or warning. Such a condition is now either binarized at
  `thrX_default` (mirroring `ctSweepS()`) or, if `thrX_default` is not
  supplied, reported as an error naming the uncovered conditions and showing
  both ways to fix the call. Calls that already listed every condition in
  `sweep_list` are unaffected and produce identical results.
* **`print_fiss_summary()` no longer requires `thr_key`.** Omitting it raised
  R's generic "argument is missing" error, and discovering the valid keys meant
  inspecting `names(result$fiss_core)` by hand. `thr_key` now defaults to
  `NULL`, which summarizes every threshold level in turn. Passing an explicit
  `thr_key` behaves as before, including the existing error that lists the
  available keys.
* **Product terms in configuration charts are now labeled `T1`, `T2`, ...
  instead of `M1`, `M2`, ...** `M1`/`M2` denote whole alternative minimal
  *solutions* throughout the package (as in `print(sol)`), so reusing `M` for
  the product terms *within* one solution made the two levels
  indistinguishable. This affects the column headers produced by
  `build_config_matrix()` (used by `config_chart_multi_solutions()` and
  related chart functions), the term columns of the cross-threshold Fiss
  chart, and the `[Term ...]` headings printed by `print_fiss_summary()`.
  Charts now read, for example, "Solution M1" with term columns `T1`, `T2`.
  This is a change to displayed output only; no computed value is affected.
* The `@return` documentation of all four sweep functions (`otSweep()`,
  `ctSweepS()`, `ctSweepM()`, `dtSweep()`) now states explicitly that
  `return_details` changes the *type* of the returned object, not only its
  contents: with `TRUE` the summary table is at `result$summary`, while with
  `FALSE` the summary table *is* the returned object and `result$summary` is
  `NULL`. Code meant to work under both settings should branch on
  `inherits(result, "data.frame")` or always pass `return_details = TRUE`.
  The behavior itself is unchanged, since altering the return type would break
  existing scripts.

# ThSQCA 2.0.5

*Release date: 2026-07-23*

## Bug fixes

* **Fit measures are reported again when the intermediate solution itself has
  several minimal solutions.** This is a regression introduced in 2.0.4. When
  `dir.exp` is used and the intermediate solution has several tied minimal
  solutions, `QCA::minimize()` splits the intermediate fit into
  `$i.sol$C1P1$IC$individual[[k]]` and `$overall` rather than storing a flat
  `$sol.incl.cov`, exactly as it does for the parsimonious solution. Version
  2.0.4 read only the flat slot, so those cells reported `inclS` and `covS` as
  `NA`. The intermediate path now follows the same cascade as the parsimonious
  path: the displayed solution's own fit for `extract_mode = "first"`, and the
  aggregate for `"all"` and `"essential"`. Cells that reported a fit in 2.0.4
  are unchanged.
* **`compute_fiss_core()` no longer biases conditions toward "core" when the
  parsimonious solution has several tied minimal solutions.** The terms of all
  minimal solutions were pooled before the condition-status map was built, so a
  condition present in one minimal solution and absent in another ended up
  holding both statuses. An intermediate term then matched whichever polarity it
  used, and the condition was classified core either way. A status now counts
  only where every minimal parsimonious solution agrees, following the same
  principle as essential prime implicants: what cannot be asserted regardless of
  which minimal solution is selected is not treated as core. Cells with a single
  minimal parsimonious solution, which are the common case, are unaffected.
* `compute_fiss_core()` now records `parsim_n_solutions` for each threshold and
  warns when the parsimonious solution has tied minimal solutions, so the
  ambiguity behind a classification is visible rather than hidden.
* The internal chart helpers `extract_solution_metrics_for_chart()` and
  `extract_path_metrics_for_chart()` had the same gap. There it produced not
  `NA` but a silent fall-through to the parsimonious values, so configuration
  charts could show the parsimonious fit and the parsimonious per-term table
  for a displayed intermediate solution with several minimal solutions.

## Other changes

* If a solution is found but no fit measures can be located for it, the sweep
  functions now emit a warning explaining that `inclS` and `covS` are `NA` for
  that reason, instead of returning an unexplained `NA`.
* Added regression tests for the multiple-intermediate-solution case, covering
  `qca_extract()`, both chart helpers, `generate_report()`, and the sweep
  functions end to end.

---

# ThSQCA 2.0.4

*Release date: 2026-07-22*

## Bug fixes

* **Fit measures for the intermediate solution now come from the intermediate
  solution itself.** When `dir.exp` is specified, the sweep functions display
  the intermediate solution (`sol$i.sol$C1P1$solution`), but `qca_extract()`
  read `inclS`/`covS` from `sol$IC`, which describes the *parsimonious*
  solution. The displayed intermediate formula was therefore paired with the
  parsimonious fit: for a single parsimonious solution via
  `sol$IC$sol.incl.cov`, and for several (tied) parsimonious solutions via
  `sol$IC$overall` (the disjunction of the parsimonious solutions). The
  extractor now branches on the displayed solution and reads
  `sol$i.sol$C1P1$IC$sol.incl.cov` for intermediate solutions, so the fit
  matches the displayed expression and `QCA::minimize()`'s own `print()`. This
  affects the intermediate-solution output of `otSweep()`, `ctSweepS()`,
  `ctSweepM()`, and `dtSweep()` in every `extract_mode`, whether the cell has
  one or several minimal solutions.
* The parsimonious/complex fix from 2.0.3 (first-solution fit read from
  `sol$IC$individual[[1]]` rather than `sol$IC$overall`) is retained. Complex
  and parsimonious solutions, crisp-set analyses, single-solution cells, and
  no-solution cells are unchanged.
* **The report and chart fit extractors received the same correction.** The
  same fit-source confusion existed independently in `extract_all_metrics()`
  (used by `generate_report()`) and in the internal chart helpers
  `extract_solution_metrics_for_chart()` and `extract_path_metrics_for_chart()`:
  with multiple minimal solutions they reported the `overall` aggregate rather
  than the displayed solution's own fit, and for intermediate solutions they
  could report the parsimonious fit. They now prefer the intermediate solution
  (when `dir.exp` is used) and then the displayed solution's
  `individual[[k]]` fit, falling back to `overall` only when the per-solution
  fit is unavailable. `generate_report()` and the configuration charts now show
  fit measures consistent with the sweep tables.
* **`generate_report()` now reports the intermediate fit in every section.**
  Beyond the extractor fix above, the report's "Solution Fit" and
  "Cross-Threshold Comparison" sections passed `sol$IC` (the parsimonious
  solution) to the extractor even when the displayed solution was the
  intermediate one, so those sections showed the parsimonious `inclS`/`covS`
  (and a parsimonious per-term row) under an intermediate formula, contradicting
  the report's own Summary Table and its embedded QCA verification output. Both
  call sites now select `sol$i.sol$C1P1$IC` for intermediate solutions, matching
  the Summary Table. Non-intermediate reports are byte-for-byte unchanged.

---

# ThSQCA 2.0.3

*Release date: 2026-07-22*

## Bug fixes

* **Fit measures now match the displayed solution when multiple minimal
  solutions exist (fuzzy data).** When `QCA::minimize()` returns more than one
  minimal solution, it stores the fit of each solution in
  `sol$IC$individual[[k]]$sol.incl.cov` and the fit of the disjunction of all
  solutions in `sol$IC$overall$sol.incl.cov`. With `extract_mode = "first"`
  (the default) the sweep functions display the first solution (M1), but the
  internal extractor `qca_extract()` reported the `overall` aggregate for
  `inclS`/`covS`, so the printed formula (M1) and the printed fit did not
  correspond. The extractor now reads `sol$IC$individual[[1]]$sol.incl.cov`
  for the first solution. This affects `otSweep()`, `ctSweepS()`,
  `ctSweepM()`, and `dtSweep()`, which share `qca_extract()`.
  * Impact is confined to cells with multiple minimal solutions on fuzzy data,
    where `overall` (the union coverage) is greater than any single solution's
    coverage; the previously reported `covS` was biased upward. `covS` is the
    materially affected measure; `inclS` differs only marginally.
  * Crisp-set (csQCA) results are unchanged: for crisp data the per-solution
    and overall fit coincide, so single-solution and no-solution cells, and all
    crisp analyses, return identical values to previous versions.
  * `extract_mode = "all"` and `extract_mode = "essential"` still report the
    `overall` aggregate, which is appropriate because those modes summarize
    across all solutions rather than displaying one.

---

# ThSQCA 2.0.2

*Release date: 2026-07-15*

## Documentation and metadata

* Corrected an author attribution in `DESCRIPTION` and the package-level help.
  The robustness protocol at <doi:10.1177/00491241211036158> is by Oana and
  Schneider (2024), not "Rubinson et al. (2019)". The vignettes and README
  already cited this work correctly; this aligns the package metadata with them.
* `inst/CITATION` now derives the package version dynamically via
  `paste("R package version", meta$Version)` instead of hard-coding it.

---

# ThSQCA 2.0.1

*Release date: 2026-07-xx*

## Bug fixes

* **Guard against `QCA::truthTable()` type issues before minimization.**
  With QCA 3.25 / admisc 0.40, `truthTable()` can, for sparse truth tables,
  return the `incl`/`PRI` columns as `character` and represent
  logical-remainder rows (observed `n = 0`) with the string `"-"`. Passing
  such a truth table to `QCA::minimize()` can, for some truth table
  structures, cause the minimization to hang or return misleading fit
  values. All sweep functions (`otSweep()`, `ctSweepS()`, `ctSweepM()`,
  `dtSweep()`) and the Fiss parsimonious step now coerce these columns to
  numeric and set remainder rows to 0 via an internal `sanitize_truthtable()`
  helper before calling `minimize()`. The `OUT` column is left untouched, so
  remainder handling is unaffected.

## Notes

* This fix concerns robustness on sparse (typically crisp) truth tables that
  can arise across a sweep. It does not change results that already ran to
  completion: the guarded conditions either halt a run or leave completed
  output unchanged, so previously obtained, completed sweep results are not
  silently affected. Re-running is only necessary if a prior sweep hung or
  failed to return a solution on data that produced sparse truth tables.

---

# ThSQCA 2.0.0

*Release date: 2026-05-XX*

## Package Renamed

The package has been renamed from `TSQCA` to `ThSQCA` (Threshold-Sweep QCA).

### Background

Reviewers noted that `TSQCA` risks being misread as Time-Series QCA, since
`ts` is the base R class for time-series objects. The new name `ThSQCA`
unambiguously reflects the "Threshold-Sweep" methodology described in
Toyoda (2026b, *Quality & Quantity*).

### Migration

Users of `TSQCA` should update their code as follows:

```r
# Old
install.packages("TSQCA")
library(TSQCA)

# New
install.packages("ThSQCA")
library(ThSQCA)
```

All function names (`otSweep`, `ctSweepS`, `ctSweepM`, `dtSweep`, etc.)
and internal logic remain unchanged. `TSQCA` will remain on CRAN for
backward compatibility but will not receive further updates.

### Reference

Toyoda, Y. (2026b). Threshold-Sweep QCA (ThS-QCA): Systematic exploration of
threshold dependency in Qualitative Comparative Analysis.
*Quality & Quantity* (forthcoming).

---

# ThSQCA 1.3.2

*Release date: 2026-03-14*

## New Features

### Fiss (2011) Core/Peripheral Condition Classification

Three new functions implement the core/peripheral distinction introduced by
Fiss (2011, *Academy of Management Journal*), which distinguishes between
conditions that are central to a causal configuration and those that merely
supplement it:

- **`compute_fiss_core(result, conditions)`** — Augments any sweep result
  object with core/peripheral classification. For each threshold, it
  automatically re-runs `QCA::minimize()` with `dir.exp = NULL` to obtain
  the parsimonious solution, then compares it to the already-stored
  intermediate solution. Conditions appearing in both solutions are
  classified as **core**; conditions appearing only in the intermediate
  solution are classified as **peripheral**.

- **`generate_fiss_chart(result, conditions, symbol_set, language)`** —
  Generates a Markdown-formatted cross-threshold configuration chart using
  four distinct symbols:

  | Symbol (unicode) | Symbol (latex) | Symbol (ascii) | Meaning |
  |-----------------|----------------|----------------|---------|
  | ● | `$\bullet$` | O | Core condition present |
  | ⊗ | `$\otimes$` | X | Core condition absent |
  | ⊙ | `$\odot$` | o | Peripheral condition present |
  | ⊘ | `$\oslash$` | x | Peripheral condition absent |

- **`print_fiss_summary(result, thr_key, language)`** — Prints a
  human-readable summary of the core/peripheral classification for a
  specific threshold value.

**Usage:**
```r
# Step 1: Run intermediate sweep (include = "?" + dir.exp required)
res <- otSweep(
  dat = sample_data, outcome = "Y",
  conditions = c("X1", "X2", "X3"),
  sweep_range = 6:8, thrX = c(X1 = 7, X2 = 7, X3 = 7),
  include = "?", dir.exp = c(1, 1, 1),
  return_details = TRUE
)

# Step 2: Compute Fiss classification
res_fiss <- compute_fiss_core(res, conditions = c("X1", "X2", "X3"))

# Step 3: Generate Fiss-style four-symbol chart
cat(generate_fiss_chart(res_fiss, symbol_set = "unicode"))
cat(generate_fiss_chart(res_fiss, symbol_set = "latex"))   # for papers

# Step 4: Inspect a specific threshold
print_fiss_summary(res_fiss, thr_key = "7")
```

**Prerequisites:** `compute_fiss_core()` requires
`return_details = TRUE`, `include = "?"`, and `dir.exp` to be specified
in the original sweep. If any of these are missing, an informative error
is raised.

### Fiss Integration in `generate_report()`

`generate_report()` gains a new `include_fiss_core` argument (default
`FALSE`). When set to `TRUE` on a result augmented by `compute_fiss_core()`,
all configuration charts in the report use the four-symbol Fiss notation
and the Notes section includes the Fiss (2011) reference and symbol legend.

```r
res_fiss <- compute_fiss_core(res)
generate_report(res_fiss, "report.md",
                include_fiss_core = TRUE,
                chart_symbol_set  = "unicode")
```

### Updated Label Dictionary (`get_config_labels`)

Both English and Japanese label dictionaries in `get_config_labels()` now
include Fiss-specific entries:
`fiss_core`, `fiss_peripheral`, `fiss_parsim`, `fiss_interm`, `fiss_note`.

## Internal Changes

- Added `SYMBOL_SETS_FISS` constant (analogous to `SYMBOL_SETS`) with
  four-symbol sets for unicode, latex, and ascii output formats.
- Added internal helpers: `extract_cond_status_map()`,
  `classify_term_conditions()`, `run_parsimonious()`,
  `extract_sol_terms()`, `build_fiss_matrix()`.
- `write_full_report()` and `write_simple_report()` accept a new
  `use_fiss` logical parameter propagated from `generate_report()`.

## Documentation

- Vignette `TSQCA_Tutorial_EN.Rmd` gains a new section **"Fiss (2011)
  Core/Peripheral Classification (New in v1.3.2)"** with full workflow
  and interpretation guidance.
- Vignette `TSQCA_Reproducible_EN.Rmd` gains a new Section 12 with
  complete reproducible Fiss workflow code.
- New Rd documentation files for `compute_fiss_core`,
  `generate_fiss_chart`, `print_fiss_summary`, and `SYMBOL_SETS_FISS`.

## Reference

Fiss, P. C. (2011). Building better causal theories: A fuzzy set approach
to typologies in organization research. *Academy of Management Journal*,
54(2), 393–420. <doi:10.5465/amj.2011.60263120>

# ThSQCA 1.3.1

*Release date: 2026-02-18*

## Documentation

- Removed non-reproducible code example from the `pre_calibrated` vignette
  section. The example referenced variables not included in the bundled
  `sample_data`. The section now provides a prose description of the feature;
  a worked example will be added when a suitable public dataset is available.
- Corrected `@param` documentation for `thrX`, `thrX_default`, `sweep_list`,
  and `sweep_list_X`: pre-calibrated variables do not require a threshold
  entry (the previous documentation incorrectly stated otherwise).

## Note

This is a documentation-only patch. No changes to code logic or behavior.

# ThSQCA 1.3.0

*Release date: 2026-02-18*

## New Features

### Pre-Calibrated Variable Pass-Through (PCVP)

All four sweep functions (`otSweep`, `dtSweep`, `ctSweepS`, `ctSweepM`) now
support a `pre_calibrated` argument. Variables listed in `pre_calibrated` are
passed through to `QCA::truthTable()` without binarization, enabling mixed
crisp/fuzzy analyses where some conditions are pre-calibrated via
`QCA::calibrate()` while others are binarized by threshold sweep.

**Usage:**
```r
# AGE is pre-calibrated as a fuzzy set; other conditions are binarized
result <- otSweep(
  dat            = dat,
  outcome        = "INT",
  conditions     = c("CHT", "PRC", "UNQ", "AGE", "GEN"),
  sweep_range    = 6:9,
  thrX           = c(CHT = 7, PRC = 7, UNQ = 7, AGE = 0.5, GEN = 1),
  pre_calibrated = c("AGE"),
  include        = "?",
  dir.exp        = c(1, 1, 1, "-", "-"),
  incl.cut       = 0.80,
  n.cut          = 2,
  pri.cut        = 0.50
)
```

**Validation:** The function raises an error if a pre-calibrated variable is
not found in `conditions`, or if its values fall outside the `[0, 1]` range.
A warning is issued if a pre-calibrated variable is also listed as a sweep
target (in `sweep_list_X` / `sweep_list`), since threshold sweeping has no
effect on fixed fuzzy values.

**Backward compatibility:** When `pre_calibrated = NULL` (the default), all
functions produce exactly the same output as v1.2.0.

## Internal Changes

* New internal helper `prepare_dat_bin()` in `tsqca_core.R` centralizes
  data preparation logic, replacing the inline binarization code that was
  duplicated across all four sweep functions.
* New internal helper `validate_pre_calibrated()` in `tsqca_core.R` performs
  input validation for the `pre_calibrated` parameter.
* `pre_calibrated` is now stored in the `params` object returned by all sweep
  functions (when `return_details = TRUE`).
* `generate_report()` now displays pre-calibrated conditions in the Analysis
  Overview section.

---

# ThSQCA 1.2.0


*Release date: 2026-01-19*

## Bug Fixes

### CRITICAL: Fixed Intermediate Solution Extraction

**Problem:** When `dir.exp` is specified for intermediate solutions, the QCA package stores:
- `sol$solution` — Contains the **Parsimonious** solution
- `sol$i.sol$C1P1$solution` — Contains the true **Intermediate** solution

Previous versions of TSQCA incorrectly prioritized `sol$solution`, causing Parsimonious solutions to be extracted and displayed when Intermediate solutions were expected.

**Fix:** All solution extraction functions now correctly prioritize `sol$i.sol` when available:

- `get_n_solutions()` — Now checks `i.sol` first
- `qca_extract()` — Now checks `i.sol` first  
- `extract_solution_list()` — Now checks `i.sol` first
- `write_full_report()` — Fixed 3 locations
- `write_simple_report()` — Fixed 1 location

**Impact:** Users who specified `dir.exp` for intermediate solutions may have received incorrect results in:
- Report generation (`generate_report()`)
- Configuration charts
- Solution expression extraction

**Verification:** The `print(sol)` output was always correct because the QCA package's print method handles this correctly. Only programmatic extraction was affected.

## New Features

### Solution Type Display

Reports now explicitly display the solution type in the Analysis Overview section:

| Include | dir.exp | **Solution Type** |
|---------|---------|-------------------|
| `""` | any | Complex (Conservative) |
| `"?"` | `NULL` | Parsimonious |
| `"?"` | specified | **Intermediate** |

### QCA Package Output for Verification

Added optional raw QCA output section to reports for verification purposes:

```r
generate_report(result, "report.md", include_raw_output = TRUE)  # default
generate_report(result, "report.md", include_raw_output = FALSE) # disable
```

When enabled, each threshold's detailed results include:

```
#### QCA Package Output (for verification)

```
DEV*URB*LIT*STB + DEV*LIT*~IND*STB -> SURV
```

```

This allows researchers to verify that TSQCA's extraction matches the QCA package's native output.

## Migration Guide

If you used intermediate solutions (with `dir.exp`) in previous versions, we recommend re-running analyses to ensure correct results. Compare your new results with `print(minimize(...))` output for verification.

---

# ThSQCA 1.1.0

*Release date: 2026-01-17*

## Bug Fixes

### CRITICAL: Fixed `dir.exp = NULL` Behavior

**Problem:** In v1.0.0, when `dir.exp = NULL` (the default), the package incorrectly converted it to `c(1, 1, ...)`, which forced intermediate solution calculation regardless of user intent.

**Fix:** `dir.exp = NULL` is now correctly passed to `QCA::minimize()` without modification.

## Breaking Changes

### Default Arguments Now Match QCA Package

To ensure consistency with the QCA package, default argument values have been changed:

| Argument | v1.0.0 Default | v1.1.0 Default | Effect |
|----------|---------------|---------------|--------|
| `include` | `"?"` | `""` | Complex solution (no logical remainders) |
| `dir.exp` | `NULL` → `c(1,1,...)` (bug) | `NULL` | No directional expectations |

**Result:** TSQCA now produces **complex solutions** by default, matching `QCA::minimize()` default behavior.

### Solution Type Summary

| Solution Type | How to Compute |
|--------------|----------------|
| **Complex** (default) | `include = ""`, `dir.exp = NULL` |
| **Parsimonious** | `include = "?"`, `dir.exp = NULL` |
| **Intermediate** | `include = "?"`, `dir.exp = c(1, 1, ...)` |

### Migration Guide

```r
# v1.0.0 (incorrect: intermediate solution by default due to bug)
result <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX)

# v1.1.0: Complex solution (new default, QCA compatible)
result_comp <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX)

# v1.1.0: Parsimonious solution (include = "?")
result_pars <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX,
                       include = "?")

# v1.1.0: Intermediate solution (include = "?" + dir.exp)
result_int <- otSweep(dat, "Y", c("X1", "X2", "X3"), sweep_range = 7, thrX = thrX,
                      include = "?",
                      dir.exp = c(1, 1, 1))
```

## Documentation Improvements

### Enhanced Examples for All Sweep Functions

All four sweep functions (`otSweep`, `dtSweep`, `ctSweepS`, `ctSweepM`) now include examples demonstrating:

1. **Complex Solution** — default (QCA compatible)
2. **Parsimonious Solution** — using `include = "?"`
3. **Intermediate Solution** — using `include = "?"` with `dir.exp`

### Updated Parameter Documentation

The `@param dir.exp` and `@param include` documentation now clearly explains:
- Default behavior produces complex solutions (QCA compatible)
- `include = "?"` enables logical remainders for parsimonious/intermediate
- `dir.exp` specifies directional expectations for intermediate solutions

---

# ThSQCA 1.0.0

*Release date: 2026-01-06*

## Changes

### Default Chart Level Changed to "term" (Fiss-style)

The default value for `chart_level` parameter has been changed from `"summary"` to `"term"`.

**Rationale:**
The solution-term level format (Fiss, 2011 notation) is the standard for academic publications, where each column represents one prime implicant (configuration). The previous default (`"summary"`) aggregated all configurations at each threshold into a single column, which obscured the distinction between different sufficient paths.

**Column header format updated:**
- Old format: `thrY=6_M1`
- New format: `thrY = 6 (M1)` (consistent with the paper format)

**Affected functions:**
- `generate_report()` — default `chart_level` is now `"term"`
- `generate_cross_threshold_chart()` — default `chart_level` is now `"term"`

**Migration:**
If you prefer the previous behavior (threshold-level summary), explicitly specify `chart_level = "summary"`:

```r
generate_report(result, "report.md", chart_level = "summary")
generate_cross_threshold_chart(result, conditions, chart_level = "summary")
```

---

# ThSQCA 0.5.3

*Release date: 2026-01-03*

## New Features

### Solution-Term Level Configuration Charts (Fiss-style)

Added support for solution-term level configuration charts following Fiss (2011) notation. This feature allows generating charts where each column represents a single prime implicant (configuration), which is the standard format for academic publications.

**New parameter for `generate_report()`:**

* `chart_level` — Character. Either `"summary"` (default) or `"term"`.
  - `"summary"`: Threshold-level summaries where each column represents one threshold, showing all conditions that appear in any configuration at that threshold.
  - `"term"`: Solution-term level (Fiss-style) where each column represents one prime implicant (sufficient configuration). Recommended for academic publications.

**New functions:**

* `generate_cross_threshold_chart()` — Generate configuration charts from sweep results with `chart_level` option.
* `parse_solution_terms()` — Internal function to parse solution expressions into individual terms.
* `get_condition_status()` — Internal function to determine condition presence/absence in a term.
* `generate_term_level_chart()` — Internal function for term-level chart generation.
* `generate_threshold_level_chart()` — Internal function for threshold-level chart generation.

**Example:**

```r
# Threshold-level summary (default)
generate_report(result, "report.md", chart_level = "summary")

# Solution-term level (Fiss-style, recommended for publications)
generate_report(result, "report.md", chart_level = "term")
```

When the solution is `X3 + X1*X2`, the term-level chart will show two separate columns (`thrY=7_M1` for `X3` and `thrY=7_M2` for `X1*X2`), while the summary-level chart shows one column (`thrY=7`) with all three conditions marked.

---

# ThSQCA 0.5.2

*Release date: 2026-01-03*

## Documentation Fixes

### Vignette Examples Corrected

Fixed non-working code examples in all vignettes (Tutorial and Reproducible, both EN/JA):

* Updated argument names from deprecated `Yvar`/`Xvars` to `outcome`/`conditions`
* Fixed `ctSweepM()` examples to use `sweep_list` parameter instead of old `sweep_vars`/`sweep_range`
* Added required `dat` parameter to all `generate_report()` examples
* Changed output from `head(res$summary)` to `summary(res)` for consistency with S3 methods
* Reduced sweep ranges (e.g., 6:9 → 6:8, 6:8 → 6:7) for faster example execution

### README Updates

* Updated all code examples to use new argument names (`outcome`, `conditions`)
* Added `dat` parameter to `generate_report()` examples
* Consistent sweep ranges across all examples

### Test Scripts Added

* `test_quick.R` — Minimal verification script (6 tests)
* `test_tutorial_code.R` — Comprehensive verification script (11 tests)

---

# ThSQCA 0.5.1

*Release date: 2026-01-01*

## New Features

### Multiple Solutions Note in Configuration Charts

When multiple logically equivalent solutions (M1, M2, M3...) exist, configuration charts now automatically include a note explaining that M1 is displayed.

**New parameters for `generate_report()`:**

* `solution_note` — Logical. If TRUE (default), adds note when multiple solutions exist
* `solution_note_style` — `"simple"` (default) or `"detailed"` (includes EPIs)
* `solution_note_lang` — `"en"` (default) or `"ja"` for Japanese

**New parameters for `config_chart_from_paths()`:**

* `n_sol` — Number of equivalent solutions (triggers note if > 1)
* `solution_note` — Logical. Whether to add solution note
* `solution_note_style` — `"simple"` or `"detailed"`
* `epi_list` — Character vector of EPIs for detailed notes

**New exported functions:**

* `generate_solution_note()` — Generate solution note text
* `identify_epi()` — Identify Essential Prime Implicants from multiple solutions

**Example output (simple):**

```
*Note: 2 logically equivalent solutions were identified. This table presents configurations based on M1.*
```

**Example output (detailed with EPIs):**

```
*Note: 3 logically equivalent solutions were identified (M1-M3). This table presents configurations based on M1. All solutions share the essential prime implicants: A·B and C.*
```

**Example (Japanese):**

```
*注: 論理的に等価な2つの解が得られた。本表はM1に基づく構成を示す。*
```

---

# ThSQCA 0.5.0

*Release date: 2025-12-31*

## New Features

### Configuration Chart Integration in Reports

Configuration charts are now automatically included in reports generated by `generate_report()`.

**New parameters for `generate_report()`:**

* `include_chart` — Logical. If TRUE (default), includes Fiss-style configuration charts
* `chart_symbol_set` — Symbol set: `"unicode"` (default), `"ascii"`, or `"latex"`

**Example:**

```r
# Generate report with configuration charts (default)
generate_report(result, "my_report.md", format = "full")

# Generate report without charts
generate_report(result, "my_report.md", include_chart = FALSE)

# Generate report with LaTeX symbols (for PDF/academic papers)
generate_report(result, "my_report.md", chart_symbol_set = "latex")
```

### Standalone Configuration Chart Functions

* `generate_config_chart()` — Generate chart from QCA solution object
* `config_chart_from_paths()` — Generate chart from path strings (e.g., "A*B*~C")
* `config_chart_multi_solutions()` — Generate separate charts for multiple solutions

**Features:**

* Three symbol sets: `"unicode"` (● / ⊗), `"ascii"` (O / X), `"latex"` ($\bullet$ / $\otimes$)
* Automatic condition extraction from solution paths
* Bilingual labels (English / Japanese)
* Markdown table output

## Breaking Changes (from 0.4.x)

### Terminology Correction

Fixed incorrect use of "Core Conditions" terminology:

| Old (incorrect) | New (correct) | Meaning |
|-----------------|---------------|---------|
| `extract_mode = "core"` | `extract_mode = "essential"` | Mode for extracting shared terms |
| `core_terms` | `essential_terms` | Terms in ALL solutions |
| `peripheral_terms` | `selective_terms` | Terms in SOME solutions |

**Migration:** Change `extract_mode = "core"` to `extract_mode = "essential"`.

## Documentation

* Added `docs/TSQCA_Terminology_Guide_EN.md` — English terminology guide
* Added `docs/TSQCA_Terminology_Guide_JA.md` — Japanese terminology guide
* Added `README_JP.md` — Japanese README

---

# ThSQCA 0.4.2

## Terminology Correction (Breaking Change)

### Corrected QCA Terminology for Multiple Solutions

Fixed incorrect use of "Core Conditions" terminology. The terms that appear in ALL equivalent solutions (M1, M2, M3...) are now correctly called **Essential Prime Implicants** (EPI), following standard Boolean minimization terminology.

**Changed terms:**

| Old (incorrect) | New (correct) | Meaning |
|-----------------|---------------|---------|
| `extract_mode = "core"` | `extract_mode = "essential"` | Mode for extracting shared terms |
| `core_terms` | `essential_terms` | Terms in ALL solutions |
| `peripheral_terms` | `selective_terms` | Terms in SOME solutions |
| "Core Conditions" | "Essential Prime Implicants (EPI)" | Report labels |
| "Peripheral Terms" | "Selective Prime Implicants (SPI)" | Report labels |

**Why this matters:**

The term "Core Conditions" in QCA literature (Fiss, 2011) refers to conditions appearing in **both** parsimonious AND intermediate solutions—a comparison between solution *types*. This is distinct from terms shared across multiple equivalent solutions of the *same* type, which are properly called "Essential Prime Implicants" in Boolean algebra terminology.

**Migration:**

If you used `extract_mode = "core"` in previous versions, change to `extract_mode = "essential"`. The output structure is identical; only the names have changed for methodological accuracy.

## New Features

### Configuration Chart Generator

Added functions for generating Fiss-style configuration charts (Table 5 format) commonly used in QCA publications.

**New functions:**

* `generate_config_chart()` — Generate configuration chart from QCA solution object
* `config_chart_from_paths()` — Generate chart from path strings (e.g., "A*B*~C")
* `config_chart_multi_solutions()` — Generate separate charts for multiple solutions

**Features:**

* Three symbol sets: `"unicode"` (● / ⊗), `"ascii"` (O / X), `"latex"` ($\bullet$ / $\otimes$)
* Automatic condition extraction from solution paths
* Optional metrics rows (Consistency, Coverage, Unique Coverage)
* Bilingual labels (English / Japanese)
* Markdown table output for easy integration with reports

**Example:**

```r
# From QCA solution object
chart <- generate_config_chart(sol, symbol_set = "unicode")
cat(chart)

# From path strings
paths <- c("A*B*~C", "A*D")
chart <- config_chart_from_paths(paths)
cat(chart)
```

---

# ThSQCA 0.4.1

## Bug Fixes

### Report Generation Improvements
* Fixed empty "Detailed Results" and "Cross-Threshold Comparison" sections for large threshold sweeps
* Added threshold limit (27 combinations) for detailed output in reports
  - When threshold combinations exceed 27, detailed per-threshold results are omitted with explanatory message
  - Users are directed to access details programmatically via `result$details`
* Affected functions: `generate_report()` for dtSweep and ctSweepM results

---

# ThSQCA 0.4.0

## New Features

### S3 Methods
* Added S3 class system for all sweep function results
  - Class hierarchy: `otSweep_result`, `dtSweep_result`, `ctSweepS_result`, `ctSweepM_result` inherit from `tsqca_result`
* Added `print()` methods for all result types
  - Displays analysis overview: outcome, conditions, thresholds swept
  - Shows summary statistics: valid solutions, no solution, multiple solutions
* Added `summary()` methods for all result types
  - Displays analysis parameters and full results table
  - Notes when multiple solutions exist

## Changes

### Backward Compatibility
* All existing workflows continue to work unchanged
* Direct access to `$summary`, `$details`, and `$params` components still works
* When `return_details = FALSE`, returns plain data.frame without S3 class

---

# ThSQCA 0.3.0

## New Features

### QCA-Compatible Argument Names
* Renamed `Yvar` to `outcome` and `Xvars` to `conditions` in all sweep functions
  - Follows QCA package naming conventions for consistency
  - Old argument names (`Yvar`, `Xvars`) are still supported with deprecation warnings

### Negated Outcome Support
* Added support for negated outcomes using tilde prefix (e.g., `outcome = "~Y"`)
  - Analyzes conditions sufficient for the absence of the outcome (Y < threshold)
  - Follows QCA package's `truthTable()` convention for negation
  - Works with all sweep functions: `otSweep()`, `dtSweep()`, `ctSweepS()`, `ctSweepM()`

### Enhanced Report Generation
* `generate_report()` now displays "(negated)" indicator when analyzing negated outcomes
* Supports both old and new parameter names for backward compatibility

## Changes

### Argument Names (Backward Compatible)
* `Yvar` → `outcome` (recommended)
* `Xvars` → `conditions` (recommended)
* Using old argument names will trigger a deprecation warning but will continue to work

### Parameter Storage
* `$params` now includes:
  - `outcome`: New argument name (also stores `~Y` notation if negated)
  - `conditions`: New argument name
  - `negate_outcome`: Boolean indicating if outcome was negated

## Migration Guide

```r
# Old syntax (still works, but shows deprecation warning)
result <- otSweep(dat, Yvar = "Y", Xvars = c("X1", "X2"), ...)

# New syntax (recommended)
result <- otSweep(dat, outcome = "Y", conditions = c("X1", "X2"), ...)

# Negated outcome (new feature)
result <- otSweep(dat, outcome = "~Y", conditions = c("X1", "X2"), ...)
```

---

# ThSQCA 0.2.0

## New Features

### Multiple Solution Handling
* Added `extract_mode` parameter to all sweep functions (`otSweep()`, `dtSweep()`, `ctSweepS()`, `ctSweepM()`) with three options:
  - `"first"` (default): Returns only the first solution (M1), maintaining backward compatibility
  - `"all"`: Returns all intermediate solutions concatenated (e.g., "M1: A*B; M2: A*C")
  - `"essential"`: Returns essential prime implicants common to all solutions, plus peripheral and unique terms

* Added `get_n_solutions()` helper function to count the number of intermediate solutions

### Report Generation
* Added `generate_report()` function for automatic markdown report generation with two formats:
  - `"full"`: Comprehensive report including all analysis details, solution formulas, and fit measures
  - `"simple"`: Condensed format designed for journal manuscript supplementary materials

### Reproducibility
* All sweep functions now return analysis parameters in `$params` for full reproducibility
* Parameters include: variable names, thresholds, QCA settings (`incl.cut`, `n.cut`, `pri.cut`, `dir.exp`, `include`)

## Changes

### Default Value Updates
* Changed `return_details` default from `FALSE` to `TRUE` for better integration with `generate_report()`
* Changed `n.cut` default from `2` to `1` to align with QCA package conventions
* Changed `pri.cut` default from `0.5` to `0` to align with QCA package conventions

### Output Structure
* When `return_details = TRUE`, results are now accessed via `$summary` (e.g., `result$summary$expression`)
* Added `n_solutions` column when using `extract_mode = "all"` or `"essential"`
* Added `selective_terms` and `unique_terms` columns when using `extract_mode = "essential"`

## Documentation

* Updated README with new features section and usage examples
* Added new vignette sections:
  - "Handling Multiple Solutions" explaining essential vs. selective prime implicants
  - "Generating Reports" with workflow examples
  - "Best Practices" including computational complexity guidance
* Updated all code examples to reflect new default values and output structure

---
 
# ThSQCA 0.1.2

* Initial release for paper submission
* Implemented four threshold sweep methods:
  - `ctSweepS()`: Single-condition X sweep (CTS-QCA)
  - `ctSweepM()`: Multi-condition X sweep (MCTS-QCA)
  - `otSweep()`: Outcome Y sweep (OTS-QCA)
  - `dtSweep()`: Two-dimensional X and Y sweep (DTS-QCA)
* Core QCA functions: `qca_bin()`, `qca_extract()`
* Sample dataset included

---

# ThSQCA 0.1.1

* Bug fixes and documentation improvements

---

# ThSQCA 0.1.0

* Initial development version
