###############################################
# Report generation for ThSQCA
###############################################

#' Generate Markdown Report for QCA Analysis
#'
#' Creates a markdown report from QCA analysis results.
#' Supports two formats: "full" (comprehensive) and "simple" (for manuscripts).
#'
#' @param result A result object from any Sweep function with
#'   \code{return_details = TRUE}.
#' @param output_file Character. Path to output markdown file.
#' @param format Character. Report format: \code{"full"} or \code{"simple"}.
#' @param title Character. Report title.
#' @param dat Optional data frame. Original data for descriptive statistics.
#' @param desc_vars Optional character vector. Variables for descriptive statistics.
#'   If NULL and dat is provided, uses Yvar and Xvars from params.
#' @param include_chart Logical. If TRUE (default), includes configuration charts
#'   (Fiss-style tables) in the report for each threshold.
#' @param chart_symbol_set Character. Symbol set for configuration charts:
#'   \code{"unicode"} (default), \code{"ascii"}, or \code{"latex"}.
#' @param chart_level Character. Chart aggregation level:
#'   \code{"term"} (default) produces solution-term level charts following Fiss (2011)
#'   notation, where each column represents one prime implicant (sufficient
#'   configuration). This format is recommended for academic publications.
#'   \code{"summary"} produces threshold-level summaries where each
#'   column represents one threshold, aggregating all configurations.
#' @param solution_note Logical. If TRUE (default), adds a note when multiple
#'   equivalent solutions exist explaining that M1 is shown.
#' @param solution_note_style Character. Style of solution note:
#'   \code{"simple"} (default) or \code{"detailed"} (includes EPIs).
#' @param solution_note_lang Character. Language for solution notes:
#'   \code{"en"} (default) or \code{"ja"}.
#' @param include_fiss_core Logical. If TRUE and \code{result$fiss_core} exists
#'   (i.e., \code{\link{compute_fiss_core}} has been run), the configuration
#'   charts use full Fiss (2011) four-symbol notation distinguishing core
#'   (present in both parsimonious and intermediate solutions) from peripheral
#'   conditions (intermediate only). If FALSE (default) or if \code{fiss_core}
#'   data is absent, the standard two-symbol chart is used.
#' @param include_raw_output Logical. If TRUE (default), includes the raw QCA
#'   package output (print(sol)) for each threshold for verification purposes.
#'
#' @return Invisibly returns the path to the generated report.
#' @export
#'
#' @examples
#' \dontrun{
#' data(sample_data)
#' thrX <- c(X1 = 7, X2 = 7, X3 = 7)
#' 
#' result <- otSweep(
#'   dat = sample_data,
#'   outcome = "Y",
#'   conditions = c("X1", "X2", "X3"),
#'   sweep_range = 6:8,
#'   thrX = thrX,
#'   return_details = TRUE
#' )
#' 
#' # With descriptive statistics and configuration charts
#' generate_report(result, "my_report.md", format = "full", 
#'                 dat = sample_data, include_chart = TRUE)
#' 
#' # Without configuration charts
#' generate_report(result, "my_report.md", format = "simple",
#'                 include_chart = FALSE)
#' 
#' # With Fiss-style term-level charts (default, recommended for publications)
#' generate_report(result, "my_report.md", format = "full")
#' 
#' # With threshold-level summary charts
#' generate_report(result, "my_report.md", format = "full",
#'                 chart_level = "summary")
#' 
#' # With Fiss core/peripheral chart (requires compute_fiss_core)
#' res_fiss <- compute_fiss_core(result, conditions = c("X1", "X2", "X3"))
#' generate_report(res_fiss, "my_report.md", format = "full",
#'                 include_fiss_core = TRUE)
#' }
generate_report <- function(result,
                            output_file = "qca_report.md",
                            format = c("full", "simple"),
                            title = "QCA Analysis Report",
                            dat = NULL,
                            desc_vars = NULL,
                            include_chart = TRUE,
                            chart_symbol_set = c("unicode", "ascii", "latex"),
                            chart_level = c("term", "summary"),
                            solution_note = TRUE,
                            solution_note_style = c("simple", "detailed"),
                            solution_note_lang = c("en", "ja"),
                            include_fiss_core = FALSE,
                            include_raw_output = TRUE) {
  
  format <- match.arg(format)
  chart_symbol_set <- match.arg(chart_symbol_set)
  chart_level <- match.arg(chart_level)
  solution_note_style <- match.arg(solution_note_style)
  solution_note_lang <- match.arg(solution_note_lang)
  
  # Resolve fiss_core availability
  use_fiss <- isTRUE(include_fiss_core) && !is.null(result$fiss_core)
  
  # Validate input
  if (!is.list(result)) {
    stop("'result' must be a list object from a Sweep function.")
  }
  
  if (!"details" %in% names(result)) {
    stop("'result' must contain 'details'. Use return_details = TRUE in Sweep functions.")
  }
  
  # Open file connection
  con <- file(output_file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  # Write header
  writeLines(paste0("# ", title, "\n"), con)
  writeLines("*(Auto-generated by ThSQCA package)*\n", con)
  writeLines(paste0("**Generated**: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n"), con)
  writeLines("---\n", con)
  
  # Dispatch to appropriate format
  if (format == "full") {
    write_full_report(result, con, dat, desc_vars, include_chart, chart_symbol_set,
                      chart_level, solution_note, solution_note_style, solution_note_lang,
                      include_raw_output, use_fiss)
  } else {
    write_simple_report(result, con, include_chart, chart_symbol_set,
                        chart_level, solution_note, solution_note_style, solution_note_lang,
                        include_raw_output, use_fiss)
  }
  
  message("Report generated: ", output_file)
  invisible(output_file)
}


#' Write full report content
#' @keywords internal
write_full_report <- function(result, con, dat = NULL, desc_vars = NULL,
                              include_chart = TRUE, chart_symbol_set = "unicode",
                              chart_level = "term",
                              solution_note = TRUE, solution_note_style = "simple",
                              solution_note_lang = "en",
                              include_raw_output = TRUE,
                              use_fiss = FALSE) {
  
  summary_df <- result$summary
  details <- result$details
  params <- result$params
  
  # ============================================
  # 0. Analysis Overview
  # ============================================
  writeLines("## 0. Analysis Overview\n", con)
  writeLines("| Item | Value |", con)
  writeLines("|------|-------|", con)
  writeLines(paste0("| Analysis Date | ", format(Sys.time(), "%Y-%m-%d %H:%M"), " |"), con)
  
  if (!is.null(dat)) {
    writeLines(paste0("| Total N | ", nrow(dat), " |"), con)
  }
  
  if (!is.null(params)) {
    # Support both old (Yvar/Xvars) and new (outcome/conditions) parameter names
    outcome_var <- params$outcome
    if (is.null(outcome_var)) outcome_var <- params$Yvar
    conditions_var <- params$conditions
    if (is.null(conditions_var)) conditions_var <- params$Xvars
    
    if (!is.null(outcome_var)) {
      outcome_display <- outcome_var
      if (isTRUE(params$negate_outcome)) {
        outcome_display <- paste0(outcome_var, " (negated)")
      }
      writeLines(paste0("| Outcome Variable | ", outcome_display, " |"), con)
    }
    if (!is.null(conditions_var)) {
      writeLines(paste0("| Condition Variables | ", paste(conditions_var, collapse = ", "), " |"), con)
    }
    if (!is.null(params$pre_calibrated)) {
      pc_str <- paste(params$pre_calibrated, collapse = ", ")
      writeLines(paste0("| Pre-Calibrated Conditions | ", pc_str, " (passed through, no binarization) |"), con)
    }
    if (!is.null(params$thrX)) {
      thrX_str <- paste(names(params$thrX), params$thrX, sep = "=", collapse = ", ")
      writeLines(paste0("| X Thresholds | ", thrX_str, " |"), con)
    }
    if (!is.null(params$sweep_range)) {
      writeLines(paste0("| Y Sweep Range | ", min(params$sweep_range), "-", max(params$sweep_range), " |"), con)
    }
    if (!is.null(params$thrY)) {
      writeLines(paste0("| Y Threshold | ", params$thrY, " |"), con)
    }
    if (!is.null(params$incl.cut)) {
      writeLines(paste0("| Consistency Cutoff | ", params$incl.cut, " |"), con)
    }
    if (!is.null(params$n.cut)) {
      writeLines(paste0("| Frequency Cutoff (n.cut) | ", params$n.cut, " |"), con)
    }
    if (!is.null(params$include)) {
      writeLines(paste0("| Include | ", params$include, " |"), con)
    }
    if (!is.null(params$dir.exp)) {
      dir_str <- ifelse(all(params$dir.exp == 1), "positive (all)", 
                        paste(params$dir.exp, collapse = ", "))
      writeLines(paste0("| Directional Expectations | ", dir_str, " |"), con)
    }
    # Solution Type (determined from include and dir.exp)
    solution_type <- if (is.null(params$include) || params$include == "") {
      "Complex (Conservative)"
    } else if (!is.null(params$dir.exp)) {
      "Intermediate"
    } else {
      "Parsimonious"
    }
    writeLines(paste0("| **Solution Type** | **", solution_type, "** |"), con)
  }
  writeLines("\n---\n", con)
  
  # ============================================
  # 1. Descriptive Statistics (if dat provided)
  # ============================================
  if (!is.null(dat)) {
    writeLines("## 1. Descriptive Statistics\n", con)
    
    # Determine variables (support both old and new parameter names)
    if (is.null(desc_vars) && !is.null(params)) {
      outcome_var <- params$outcome
      if (is.null(outcome_var)) outcome_var <- params$Yvar
      conditions_var <- params$conditions
      if (is.null(conditions_var)) conditions_var <- params$Xvars
      
      # For negated outcome, use the cleaned variable name
      if (!is.null(outcome_var)) {
        outcome_clean <- sub("^~", "", outcome_var)
        desc_vars <- c(outcome_clean, conditions_var)
      } else {
        desc_vars <- conditions_var
      }
    }
    
    if (!is.null(desc_vars)) {
      desc_df <- data.frame(
        Variable = character(0),
        n = integer(0),
        Mean = numeric(0),
        SD = numeric(0),
        Min = numeric(0),
        Max = numeric(0),
        Skew = numeric(0),
        Kurtosis = numeric(0),
        stringsAsFactors = FALSE
      )
      
      for (var in desc_vars) {
        if (var %in% names(dat)) {
          x <- dat[[var]]
          x <- x[!is.na(x)]
          n <- length(x)
          m <- mean(x)
          s <- sd(x)
          
          # Skewness
          skew <- if (n > 2 && s > 0) {
            sum((x - m)^3) / (n * s^3)
          } else {
            NA
          }
          
          # Kurtosis (excess)
          kurt <- if (n > 3 && s > 0) {
            sum((x - m)^4) / (n * s^4) - 3
          } else {
            NA
          }
          
          desc_df <- rbind(desc_df, data.frame(
            Variable = var,
            n = n,
            Mean = round(m, 3),
            SD = round(s, 3),
            Min = round(min(x), 3),
            Max = round(max(x), 3),
            Skew = round(skew, 3),
            Kurtosis = round(kurt, 3),
            stringsAsFactors = FALSE
          ))
        }
      }
      
      if (nrow(desc_df) > 0) {
        writeLines(df_to_md_table(desc_df), con)
      }
    }
    writeLines("\n---\n", con)
  }
  
  # ============================================
  # 2. Summary Table
  # ============================================
  section_num <- if (!is.null(dat)) 2 else 1
  writeLines(paste0("## ", section_num, ". Summary Table\n"), con)
  writeLines(df_to_md_table(summary_df), con)
  writeLines("\n---\n", con)
  
  # ============================================
  # 3. Detailed Results per Threshold
  # ============================================
  section_num <- section_num + 1
  writeLines(paste0("## ", section_num, ". Detailed Results\n"), con)
  
  # Limit detailed output to avoid extremely long reports
  n_combinations <- length(details)
  MAX_DETAILS <- 27
  
  if (n_combinations > MAX_DETAILS) {
    writeLines(paste0("Due to the large number of threshold combinations (", 
                      n_combinations, "), detailed per-threshold results ",
                      "(necessity analysis, truth tables, per-term metrics) ",
                      "are omitted from this report.\n"), con)
    writeLines("", con)
    writeLines("To access details for specific threshold combinations, use:\n", con)
    writeLines("```r", con)
    writeLines("# List all threshold combinations", con)
    writeLines("names(result$details)", con)
    writeLines("", con)
    writeLines("# Access a specific combination (e.g., first one)", con)
    writeLines("key <- names(result$details)[1]", con)
    writeLines("det <- result$details[[key]]", con)
    writeLines("", con)
    writeLines("# Available components:", con)
    writeLines("det$truth_table$tt   # Truth table", con)
    writeLines("det$solution         # QCA solution object", con)
    writeLines("det$dat_bin          # Binarized data (for necessity analysis with QCA::pofind)", con)
    writeLines("det$thrX_vec         # X thresholds used", con)
    writeLines("det$thrY             # Y threshold used", con)
    writeLines("```\n", con)
  } else {
  
  for (key in names(details)) {
    det <- details[[key]]
    
    # Determine threshold label
    if (!is.null(det$thrY)) {
      writeLines(paste0("### thrY = ", det$thrY, "\n"), con)
    } else if (!is.null(det$threshold)) {
      writeLines(paste0("### Threshold = ", det$threshold, "\n"), con)
    } else if (!is.null(det$combo_id)) {
      writeLines(paste0("### Combination ", det$combo_id, "\n"), con)
    } else {
      writeLines(paste0("### ", key, "\n"), con)
    }
    
    # X thresholds
    if (!is.null(det$thrX_vec)) {
      thrX_str <- paste(names(det$thrX_vec), det$thrX_vec, sep = "=", collapse = ", ")
      writeLines(paste0("**X Thresholds**: ", thrX_str, "\n"), con)
    }
    
    # ---- Necessity Analysis ----
    dat_bin <- det$dat_bin
    if (!is.null(dat_bin) && !is.null(det$thrX_vec)) {
      Xvars <- names(det$thrX_vec)
      nec <- try(QCA::pofind(dat_bin, outcome = "Y", conditions = Xvars), silent = TRUE)
      if (!inherits(nec, "try-error") && !is.null(nec$incl.cov)) {
        writeLines("#### Necessity Analysis\n", con)
        nec_df <- nec$incl.cov
        nec_df <- cbind(Condition = rownames(nec_df), nec_df)
        rownames(nec_df) <- NULL
        writeLines(df_to_md_table(nec_df), con)
        writeLines("\n", con)
      }
    }
    
    # ---- Truth Table ----
    tt <- det$truth_table
    if (!is.null(tt) && !is.null(tt$tt)) {
      writeLines("#### Truth Table (observed configurations)\n", con)
      tt_df <- tt$tt
      tt_observed <- tt_df[tt_df$n > 0, , drop = FALSE]
      if (nrow(tt_observed) > 0) {
        tt_cols <- intersect(c(names(det$thrX_vec), "OUT", "n", "incl", "PRI"), names(tt_observed))
        tt_subset <- tt_observed[, tt_cols, drop = FALSE]
        tt_subset <- cbind(Row = rownames(tt_subset), tt_subset)
        rownames(tt_subset) <- NULL
        writeLines(df_to_md_table(tt_subset), con)
      } else {
        writeLines("*(No observed configurations)*", con)
      }
      writeLines("\n", con)
    }
    
    # ---- Solution ----
    sol <- det$solution
    if (is.null(sol)) {
      writeLines("#### Solution\n", con)
      writeLines("**No solution**\n", con)
    } else {
      n_sol <- get_n_solutions(sol)
      writeLines("#### Solution\n", con)
      writeLines(paste0("**Number of Solutions**: ", n_sol, "\n"), con)
      
      # Get solution list (i.sol first for true Intermediate when dir.exp
      # specified). Deduplicated across prime implicant charts; see
      # collect_unique_i_sol() for why naive concatenation double-counts.
      sol_list <- collect_unique_i_sol(sol)

      if (!is.null(sol_list) && length(sol_list) > 0) {
        writeLines("**Full Solutions**:\n", con)
        for (i in seq_along(sol_list)) {
          expr <- paste(sol_list[[i]], collapse = " + ")
          writeLines(paste0("- M", i, ": ", escape_md(expr), " -> Y\n"), con)
        }
        writeLines("\n", con)
        
        # Essential/Selective Prime Implicants (if multiple solutions)
        if (length(sol_list) > 1) {
          sol_terms <- lapply(sol_list, function(x) {
            if (is.character(x)) x else unlist(strsplit(paste(x, collapse = " + "), " \\+ "))
          })
          essential_terms <- Reduce(intersect, sol_terms)
          all_terms <- Reduce(union, sol_terms)
          selective_terms <- setdiff(all_terms, essential_terms)
          
          if (length(essential_terms) > 0) {
            writeLines(paste0("**Essential Prime Implicants**: ", 
                              escape_md(paste(essential_terms, collapse = " + ")), "\n"), con)
          } else {
            writeLines("**Essential Prime Implicants**: (none - solutions are disjoint)\n", con)
          }
          
          if (length(selective_terms) > 0) {
            writeLines(paste0("**Selective Prime Implicants**: ", 
                              escape_md(paste(selective_terms, collapse = " + ")), "\n"), con)
          }
          
          # Unique Terms
          unique_terms_list <- lapply(seq_along(sol_terms), function(i) {
            other_terms <- unique(unlist(sol_terms[-i]))
            setdiff(sol_terms[[i]], other_terms)
          })
          unique_terms_formatted <- sapply(seq_along(unique_terms_list), function(i) {
            if (length(unique_terms_list[[i]]) > 0) {
              paste0("M", i, ": ", escape_md(paste(unique_terms_list[[i]], collapse = " + ")))
            } else {
              NULL
            }
          })
          unique_terms_filtered <- unique_terms_formatted[!sapply(unique_terms_formatted, is.null)]
          if (length(unique_terms_filtered) > 0) {
            writeLines(paste0("**Unique Terms**: ", 
                              paste(unique_terms_filtered, collapse = "; "), "\n"), con)
          }
          writeLines("\n", con)
        }
      }
      
      # ---- Solution Fit ----
      writeLines("#### Solution Fit\n", con)
      # Report the fit of the DISPLAYED solution. When dir.exp yields an
      # intermediate solution, its fit lives in sol$i.sol$C1P1$IC; sol$IC
      # describes the parsimonious solution and must not be used here. This
      # mirrors the intermediate-aware call further below.
      ic_for_metrics <- if (!is.null(sol$i.sol) && length(sol$i.sol) > 0 &&
                            !is.null(sol$i.sol$C1P1$IC)) {
        sol$i.sol$C1P1$IC
      } else {
        sol$IC
      }
      metrics <- extract_all_metrics(ic_for_metrics, sol)
      writeLines("| Metric | Value |", con)
      writeLines("|--------|-------|", con)
      writeLines(paste0("| Consistency (inclS) | ", 
                        ifelse(is.na(metrics$sol_inclS), "N/A", round(metrics$sol_inclS, 3)), " |"), con)
      writeLines(paste0("| PRI | ", 
                        ifelse(is.na(metrics$sol_PRI), "N/A", round(metrics$sol_PRI, 3)), " |"), con)
      writeLines(paste0("| Coverage (covS) | ", 
                        ifelse(is.na(metrics$sol_covS), "N/A", round(metrics$sol_covS, 3)), " |"), con)
      writeLines("\n", con)
      
      # ---- Per-Term Metrics ----
      if (!is.null(metrics$term_df)) {
        writeLines("#### Per-Term Metrics\n", con)
        term_df <- metrics$term_df
        if ("cases" %in% names(term_df)) {
          term_df <- term_df[, !names(term_df) %in% "cases", drop = FALSE]
        }
        term_df <- cbind(Term = rownames(term_df), term_df)
        rownames(term_df) <- NULL
        writeLines(df_to_md_table(term_df), con)
        writeLines("\n", con)
      }
      
      # ---- Configuration Chart ----
      if (include_chart && !is.null(sol_list) && length(sol_list) > 0) {
        writeLines("#### Configuration Chart\n", con)
        
        # Fiss 4-symbol chart (per-threshold)
        thr_key <- as.character(key)
        if (use_fiss && !is.null(result$fiss_core[[thr_key]])) {
          fc <- result$fiss_core[[thr_key]]
          if (!is.null(fc$classification)) {
            writeLines(paste0(
              "*Fiss (2011) notation: core conditions (appear in both parsimonious ",
              "and intermediate solutions) are shown with large symbols; ",
              "peripheral conditions (intermediate only) with small symbols.*\n"
            ), con)
            writeLines(paste0(
              "- Parsimonious: `", fc$parsim_expression, "`\n"
            ), con)
            writeLines(paste0(
              "- Intermediate: `", fc$interm_expression, "`\n\n"
            ), con)
            
            conditions <- result$params$conditions
            interm_terms <- unique(fc$classification$term_expr)
            symbols_fiss <- SYMBOL_SETS_FISS[[chart_symbol_set]]
            thr_label <- paste0("thrY=", thr_key)
            
            mat <- build_fiss_matrix(
              interm_terms   = interm_terms,
              classification = fc$classification,
              conditions     = conditions,
              symbols        = symbols_fiss,
              thr_label      = thr_label
            )
            table_str <- config_matrix_to_md(mat, "Condition")
            legend <- if (solution_note_lang == "ja") symbols_fiss$note_ja else symbols_fiss$note_en
            writeLines(paste0(table_str, "\n\n*", legend, "*\n"), con)
          } else {
            writeLines("*No solution at this threshold.*\n", con)
          }
        } else {
          # Standard 2-symbol chart
          paths <- sol_list[[1]]
          if (!is.character(paths)) paths <- as.character(paths)
          
          if (length(paths) > 0) {
            epi_list <- NULL
            if (solution_note_style == "detailed" && length(sol_list) > 1) {
              epi_info <- identify_epi(sol_list)
              epi_list <- epi_info$epi
            }
            chart <- config_chart_from_paths(
              paths,
              symbol_set = chart_symbol_set,
              language = solution_note_lang,
              n_sol = length(sol_list),
              solution_note = solution_note,
              solution_note_style = solution_note_style,
              epi_list = epi_list
            )
            writeLines(chart, con)
            writeLines("\n", con)
          }
        }
      }
      
      # ---- QCA Package Output (for verification) ----
      if (include_raw_output) {
        writeLines("#### QCA Package Output (for verification)\n", con)
        writeLines("```", con)
        raw_output <- capture.output(print(sol))
        writeLines(raw_output, con)
        writeLines("```\n", con)
      }
    }
    
    # ---- Settings for Reproducibility ----
    writeLines("#### Settings (for reproducibility)\n", con)
    writeLines("```", con)
    if (!is.null(det$thrX_vec)) {
      writeLines(paste0("thrX: ", paste(det$thrX_vec, collapse = ", ")), con)
    }
    if (!is.null(det$thrY)) {
      writeLines(paste0("thrY: ", det$thrY), con)
    }
    if (!is.null(params$incl.cut)) {
      writeLines(paste0("incl.cut: ", params$incl.cut), con)
    }
    if (!is.null(params$dir.exp)) {
      writeLines(paste0("dir.exp: ", paste(params$dir.exp, collapse = ", ")), con)
    }
    writeLines("```\n", con)
    
    writeLines("---\n", con)
  }
  
  }  # End of if (n_combinations <= MAX_DETAILS)
  
  # ============================================
  # 4. Cross-Threshold Comparison
  # ============================================
  section_num <- section_num + 1
  writeLines(paste0("## ", section_num, ". Cross-Threshold Comparison\n"), con)
  
  if (n_combinations > MAX_DETAILS) {
    writeLines("Cross-threshold comparison table is designed for single-dimension sweeps ", con)
    writeLines("(otSweep, ctSweepS) with a smaller number of thresholds.\n", con)
    writeLines("", con)
    writeLines("For multi-dimensional sweeps with many combinations, ", con)
    writeLines("please refer to the Summary Table above to compare results across threshold settings.\n", con)
  } else {
  
  # Build comparison table
  comp_df <- data.frame(
    Threshold = character(0),
    inclS = numeric(0),
    PRI = numeric(0),
    covS = numeric(0),
    n_solutions = integer(0),
    N_Essential = integer(0),
    stringsAsFactors = FALSE
  )
  
  for (key in names(details)) {
    det <- details[[key]]
    sol <- det$solution
    
    # Threshold label
    thr_label <- if (!is.null(det$thrY)) {
      paste0("thrY=", det$thrY)
    } else if (!is.null(det$threshold)) {
      as.character(det$threshold)
    } else {
      key
    }
    
    if (is.null(sol)) {
      comp_df <- rbind(comp_df, data.frame(
        Threshold = thr_label,
        inclS = NA,
        PRI = NA,
        covS = NA,
        n_solutions = 0,
        N_Essential = 0,
        stringsAsFactors = FALSE
      ))
    } else {
      # Report the fit of the DISPLAYED solution (intermediate when dir.exp is
      # used); sol$IC is the parsimonious solution and would misreport it.
      ic_for_metrics <- if (!is.null(sol$i.sol) && length(sol$i.sol) > 0 &&
                            !is.null(sol$i.sol$C1P1$IC)) {
        sol$i.sol$C1P1$IC
      } else {
        sol$IC
      }
      metrics <- extract_all_metrics(ic_for_metrics, sol)
      n_sol <- get_n_solutions(sol)
      
      # Count essential prime implicants (i.sol first for true Intermediate).
      # Deduplicated across prime implicant charts; see collect_unique_i_sol().
      n_essential <- 0
      sol_list <- collect_unique_i_sol(sol)
      if (!is.null(sol_list) && length(sol_list) > 1) {
        sol_terms <- lapply(sol_list, function(x) {
          if (is.character(x)) x else unlist(strsplit(paste(x, collapse = " + "), " \\+ "))
        })
        essential_terms <- Reduce(intersect, sol_terms)
        n_essential <- length(essential_terms)
      } else if (!is.null(sol_list) && length(sol_list) == 1) {
        n_essential <- length(sol_list[[1]])
      }
      
      comp_df <- rbind(comp_df, data.frame(
        Threshold = thr_label,
        inclS = round(metrics$sol_inclS, 3),
        PRI = round(metrics$sol_PRI, 3),
        covS = round(metrics$sol_covS, 3),
        n_solutions = n_sol,
        N_Essential = n_essential,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  writeLines(df_to_md_table(comp_df), con)
  
  }  # End of if (n_combinations <= MAX_DETAILS) for Section 4
  
  writeLines("\n---\n", con)
  
  # ============================================
  # 5. Cross-Threshold Configuration Chart
  # ============================================
  if (include_chart && n_combinations <= MAX_DETAILS) {
    section_num <- section_num + 1
    writeLines(paste0("## ", section_num, ". Cross-Threshold Configuration Chart\n"), con)
    
    # Get conditions from params
    conditions <- params$conditions
    if (is.null(conditions)) conditions <- params$Xvars
    
    if (!is.null(conditions) && length(conditions) > 0) {
      
      if (use_fiss) {
        # --- Fiss 4-symbol cross-threshold chart ---
        writeLines(paste0(
          "*Fiss (2011) four-symbol notation. ",
          "Large symbols (\u25CF/\u2297) = core conditions (parsimonious + intermediate). ",
          "Small symbols (\u2299/\u2298) = peripheral conditions (intermediate only). ",
          "Blank = don't care.*\n\n"
        ), con)
        chart <- generate_fiss_chart(
          result,
          conditions = conditions,
          symbol_set = chart_symbol_set,
          language   = solution_note_lang
        )
        writeLines(chart, con)
      } else {
        # --- Standard 2-symbol cross-threshold chart ---
        if (chart_level == "term") {
          writeLines("*Configuration chart at solution-term level (Fiss, 2011 notation).*\n", con)
          writeLines("*Each column represents one prime implicant (configuration).*\n\n", con)
        } else {
          writeLines("*Configuration chart at threshold-level summary.*\n", con)
          writeLines("*Each column aggregates all conditions that appear in any configuration at that threshold.*\n\n", con)
        }
        
        symbols <- SYMBOL_SETS[[chart_symbol_set]]
        chart <- if (chart_level == "term") {
          generate_term_level_chart(summary_df, conditions, symbols, solution_note_lang)
        } else {
          generate_threshold_level_chart(summary_df, conditions, symbols, solution_note_lang)
        }
        writeLines(chart, con)
      }
    } else {
      writeLines("*(Could not generate configuration chart - conditions not found)*\n", con)
    }
    
    writeLines("\n---\n", con)
  }
  
  # ============================================
  # 6. Notes
  # ============================================
  section_num <- section_num + 1
  writeLines(paste0("## ", section_num, ". Notes\n"), con)
  writeLines("- **Essential Prime Implicants (EPI)**: Terms that appear in ALL equivalent solutions (M1, M2, M3...).", con)
  writeLines("- **Selective Prime Implicants (SPI)**: Terms that appear in some but not all solutions.", con)
  writeLines("- **Unique Terms**: Terms that appear only in one specific solution.", con)
  writeLines("- **inclS**: Solution consistency (sufficiency).", con)
  writeLines("- **covS**: Solution coverage.", con)
  writeLines("- **PRI**: Proportional Reduction in Inconsistency.", con)
  writeLines("- **covU**: Unique coverage (coverage by this term alone).", con)
  writeLines("- **inclN**: Necessity consistency (>= 0.9 typically indicates necessary condition).", con)
  writeLines("- **RoN**: Relevance of Necessity.", con)
  writeLines("- **covN**: Necessity coverage.", con)
  if (use_fiss) {
    writeLines("", con)
    writeLines("**Fiss (2011) Core/Peripheral Classification:**", con)
    writeLines("- **Core condition** (\u25CF/\u2297): Appears in BOTH the parsimonious and intermediate solutions.", con)
    writeLines("- **Peripheral condition** (\u2299/\u2298): Appears in the intermediate solution ONLY.", con)
    writeLines("- Reference: Fiss, P. C. (2011). Building better causal theories: A fuzzy set approach to typologies in organization research. *Academy of Management Journal*, 54(2), 393-420.", con)
  }
  
  writeLines("\n---\n", con)
  
  # ============================================
  # 7. Verification Recommendation
  # ============================================
  section_num <- section_num + 1
  writeLines(paste0("## ", section_num, ". Verification Recommendation\n"), con)
  writeLines("**For academic publications**, always verify ThSQCA results directly with the QCA package:\n", con)
  writeLines("```r", con)
  writeLines("library(QCA)", con)
  writeLines("tt <- truthTable(dat, outcome = \"Y\", conditions = c(...), incl.cut = 0.8)", con)
  writeLines("sol <- minimize(tt, include = \"?\", dir.exp = c(1, 1, ...))", con)
  writeLines("print(sol)  # Compare with ThSQCA output above", con)
  writeLines("```\n", con)
  writeLines("Ensure that solution expressions, consistency, and coverage values match before publishing.", con)
  writeLines("\n", con)
  writeLines("*Report generated by ThSQCA package (https://github.com/im-research-yt/ThSQCA)*", con)
}


#' Write simple report content
#' @keywords internal
write_simple_report <- function(result, con, include_chart = TRUE, 
                                chart_symbol_set = "unicode",
                                chart_level = "term",
                                solution_note = TRUE, solution_note_style = "simple",
                                solution_note_lang = "en",
                                include_raw_output = TRUE,
                                use_fiss = FALSE) {
  
  summary_df <- result$summary
  details <- result$details
  
  # 1. Summary Table
  writeLines("## Summary\n", con)
  writeLines(df_to_md_table(summary_df), con)
  writeLines("\n", con)
  writeLines("---\n", con)
  
  # 2. Solutions Overview
  writeLines("## Solutions Overview\n", con)
  
  for (key in names(details)) {
    det <- details[[key]]
    sol <- det$solution
    
    if (is.null(sol)) next
    
    # Determine threshold label
    if (!is.null(det$thrY)) {
      label <- paste0("Y >= ", det$thrY)
    } else if (!is.null(det$threshold)) {
      label <- paste0("threshold = ", det$threshold)
    } else {
      label <- key
    }
    
    n_sol <- get_n_solutions(sol)
    
    # Get solution list (i.sol first for true Intermediate when dir.exp
    # specified). Deduplicated across prime implicant charts; see
    # collect_unique_i_sol() for why naive concatenation double-counts.
    sol_list <- collect_unique_i_sol(sol)
    
    if (!is.null(sol_list) && length(sol_list) > 0) {
      writeLines(paste0("### ", label, "\n"), con)
      
      # Show solution formula
      if (length(sol_list) == 1) {
        expr <- paste(sol_list[[1]], collapse = " + ")
        writeLines(paste0("**Solution**: ", escape_md(expr), " -> Y\n"), con)
      } else {
        writeLines(paste0("**Number of Solutions**: ", length(sol_list), "\n"), con)
        
        # Essential prime implicants
        sol_terms <- lapply(sol_list, function(x) {
          if (is.character(x)) x else unlist(strsplit(paste(x, collapse = " + "), " \\+ "))
        })
        essential_terms <- Reduce(intersect, sol_terms)
        
        if (length(essential_terms) > 0) {
          writeLines(paste0("**Essential (EPI)**: ", escape_md(paste(essential_terms, collapse = " + ")), "\n"), con)
        }
        
        # List all solutions briefly
        for (i in seq_along(sol_list)) {
          expr <- paste(sol_list[[i]], collapse = " + ")
          writeLines(paste0("- M", i, ": ", escape_md(expr), "\n"), con)
        }
      }
      
      # Metrics (brief)
      metrics <- extract_all_metrics(sol$i.sol$C1P1$IC, sol)
      writeLines(paste0("*inclS = ", 
                        ifelse(is.na(metrics$sol_inclS), "N/A", round(metrics$sol_inclS, 3)),
                        ", covS = ",
                        ifelse(is.na(metrics$sol_covS), "N/A", round(metrics$sol_covS, 3)),
                        "*\n"), con)
      
      # ---- Configuration Chart ----
      if (include_chart) {
        writeLines("\n**Configuration Chart:**\n", con)
        
        thr_key <- as.character(key)
        if (use_fiss && !is.null(result$fiss_core[[thr_key]])) {
          fc <- result$fiss_core[[thr_key]]
          if (!is.null(fc$classification)) {
            writeLines(paste0(
              "*Fiss (2011) notation. ",
              "Parsimonious: `", fc$parsim_expression, "` / ",
              "Intermediate: `", fc$interm_expression, "`*\n\n"
            ), con)
            conditions <- result$params$conditions
            interm_terms <- unique(fc$classification$term_expr)
            symbols_fiss <- SYMBOL_SETS_FISS[[chart_symbol_set]]
            thr_label <- paste0("thrY=", thr_key)
            mat <- build_fiss_matrix(
              interm_terms   = interm_terms,
              classification = fc$classification,
              conditions     = conditions,
              symbols        = symbols_fiss,
              thr_label      = thr_label
            )
            table_str <- config_matrix_to_md(mat, "Condition")
            legend <- if (solution_note_lang == "ja") symbols_fiss$note_ja else symbols_fiss$note_en
            writeLines(paste0(table_str, "\n\n*", legend, "*\n"), con)
          }
        } else {
          paths <- sol_list[[1]]
          if (!is.character(paths)) paths <- as.character(paths)
          if (length(paths) > 0) {
            epi_list <- NULL
            if (solution_note_style == "detailed" && length(sol_list) > 1) {
              epi_info <- identify_epi(sol_list)
              epi_list <- epi_info$epi
            }
            chart <- config_chart_from_paths(
              paths,
              symbol_set = chart_symbol_set,
              language = solution_note_lang,
              n_sol = length(sol_list),
              solution_note = solution_note,
              solution_note_style = solution_note_style,
              epi_list = epi_list
            )
            writeLines(chart, con)
          }
        }
      }
      
      # ---- QCA Package Output (for verification) ----
      if (include_raw_output) {
        writeLines("\n**QCA Package Output (for verification):**\n", con)
        writeLines("```", con)
        raw_output <- capture.output(print(sol))
        writeLines(raw_output, con)
        writeLines("```\n", con)
      }
      
      writeLines("\n", con)
    }
  }
  
  # ============================================
  # Verification Recommendation
  # ============================================
  writeLines("---\n", con)
  writeLines("## Verification Recommendation\n", con)
  writeLines("**For academic publications**, always verify ThSQCA results directly with the QCA package:\n", con)
  writeLines("```r", con)
  writeLines("library(QCA)", con)
  writeLines("tt <- truthTable(dat, outcome = \"Y\", conditions = c(...), incl.cut = 0.8)", con)
  writeLines("sol <- minimize(tt, include = \"?\", dir.exp = c(1, 1, ...))", con)
  writeLines("print(sol)  # Compare with ThSQCA output above", con)
  writeLines("```\n", con)
  writeLines("Ensure that solution expressions, consistency, and coverage values match before publishing.", con)
  writeLines("\n", con)
  writeLines("*Report generated by ThSQCA package (https://github.com/im-research-yt/ThSQCA)*", con)
}