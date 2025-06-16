# Magic Variables Refactoring Plan

## VARIABLE INVENTORY

Based on `MAGIC_VARIABLES_DOCUMENTATION.md`, here's the complete inventory with risk assessment:

| CONSTANT_NAME | Original_Value | Type | File:Line(s) | Risk_Level | Usage_Count |
|---------------|----------------|------|--------------|------------|-------------|
| WEAK_INSTRUMENT_F_THRESHOLD | 10 | numeric | R/analysis.R:81,124; R/visualization.R:182,196,328 | HIGH | 5 |
| ALPHA_LEVEL | 0.05 | numeric | R/data-generation.R:220; R/estimation.R:221,321; R/utils-df.R:27 | HIGH | 4 |
| Z_CRITICAL_95 | 1.96 | numeric | R/visualization.R:251,252 | MEDIUM | 2 |
| MIN_EXPONENT | -10 | numeric | R/data-generation.R:99 | MEDIUM | 1 |
| MAX_EXPONENT | 10 | numeric | R/data-generation.R:99 | MEDIUM | 1 |
| WEAK_ID_TOLERANCE | 1e-6 | numeric | R/estimation.R:77 | MEDIUM | 1 |
| INSTRUMENT_SD_THRESHOLD | 1e-10 | numeric | R/estimation.R:256 | MEDIUM | 1 |
| DEFAULT_BOOTSTRAP_REPS | 100 | numeric | R/estimation.R:58; R/utils.R:96,94,95 | HIGH | 4 |
| DEFAULT_PARALLEL_WORKER_OFFSET | 1 | numeric | R/simulation.R:32,115,191,280 | HIGH | 4 |
| DF_ADJUST_ASYMPTOTIC | "asymptotic" | character | R/utils.R:109; R/simulation-helpers.R:7,44; R/utils-df.R:9,27,41,69 | HIGH | 7 |
| DF_ADJUST_FINITE | "finite" | character | R/simulation-helpers.R:50; R/utils-df.R:10 | MEDIUM | 2 |
| DISPLAY_DIGITS | 4 | numeric | R/analysis.R:75,115,200,260,321 | HIGH | 5 |
| PLOT_BASE_FONT_SIZE | 14 | numeric | R/visualization.R:60,106,148,209,276 | HIGH | 5 |
| HISTOGRAM_BINS | 50 | numeric | R/visualization.R:195 | LOW | 1 |
| PLOT_DISPLAY_LIMIT | 20 | numeric | R/visualization.R:244 | LOW | 1 |
| MIN_BOOTSTRAP_EXAMPLES | 5 | numeric | R/visualization.R:239 | LOW | 1 |
| VERBOSE_ENV_VAR | "VERBOSE" | character | R/messager.R:16 | LOW | 1 |
| VERBOSE_DEFAULT | "TRUE" | character | R/messager.R:16 | LOW | 1 |
| VERBOSE_DISABLE | "FALSE" | character | R/messager.R:16 | LOW | 1 |
| STATA_DO_EXT | ".do" | character | R/utils-hetid.R:98 | LOW | 1 |
| STATA_LOG_EXT | ".log" | character | R/utils-hetid.R:120 | LOW | 1 |
| SUCCESS_EXIT_CODE | 0 | numeric | R/utils-hetid.R:128 | LOW | 1 |
| TWO_TAILED_MULTIPLIER | 2 | numeric | R/data-generation.R:208 | MEDIUM | 1 |
| POINT_ID_TAU | 0 | numeric | R/estimation.R:333 | MEDIUM | 1 |
| LOWER_BOUND_IDX | 1 | numeric | R/estimation.R:114 | MEDIUM | 1 |
| UPPER_BOUND_IDX | 2 | numeric | R/estimation.R:115 | MEDIUM | 1 |

### Complex Constants (Arrays/Lists)

| CONSTANT_NAME | Original_Value | Type | File:Line(s) | Risk_Level |
|---------------|----------------|------|--------------|------------|
| STATA_REQUIRED_PACKAGES | c("ranktest", "ivreg2", "ivreg2h") | character | R/utils-hetid.R:101,106,111 | MEDIUM |
| STATA_EXECUTABLES_MAC | c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata") | character | R/utils-hetid.R:32-34 | MEDIUM |
| STATA_EXECUTABLES_UNIX | c("stata", "stata-mp", "stata-se") | character | R/utils-hetid.R:56-58 | MEDIUM |
| PLOT_COLORS | c("OLS (Biased)" = "#d95f02", "2SLS (Lewbel)" = "#1b9e77") | character | R/visualization.R:62 | MEDIUM |
| PLOT_LABELS | c("ols_gamma1" = "OLS (Biased)", "tsls_gamma1" = "2SLS (Lewbel)") | character | R/visualization.R:31-32 | MEDIUM |
| DEFAULT_SIM_CONFIG | list(...) | list | R/utils.R:89-109 | HIGH |
| DEFAULT_TEST_PARAMS | list(...) | list | R/utils-hetid.R:143-147 | MEDIUM |

## REFACTORING PLAN

### Pre-Refactoring Verification
```bash
# Establish baseline
devtools::check()
# Expected: 0 errors, 0 warnings, 0 notes
# Save output for comparison
```

### Step 1: Create Constants File

Create `R/constants.R` with comprehensive documentation:

```r
#' Package Constants
#'
#' This file contains all magic numbers and string literals used throughout
#' the hetid package, centralized for maintainability and consistency.
#'
#' @name constants
#' @keywords internal
NULL

# Statistical Thresholds -------------------------------------------------------

#' Weak Instrument F-Statistic Threshold
#'
#' Standard threshold for weak instrument detection in econometrics literature.
#' Used for identifying weak instruments and plotting threshold lines.
#'
#' @format numeric scalar
#' @source Lewbel (2012) and econometrics literature
WEAK_INSTRUMENT_F_THRESHOLD <- 10

#' Default Significance Level (Alpha)
#'
#' Standard 5% significance level for statistical tests and hypothesis testing.
#'
#' @format numeric scalar
ALPHA_LEVEL <- 0.05

#' Critical Value for 95% Confidence Intervals
#'
#' Standard normal critical value for 95% confidence intervals.
#' Equivalent to qnorm(1 - 0.05 / 2).
#'
#' @format numeric scalar
Z_CRITICAL_95 <- 1.96

# Numerical Bounds and Tolerances ---------------------------------------------

#' Exponent Safety Bounds
#'
#' Prevents numerical overflow in exponential calculations.
#' Used in heteroskedasticity variance generation.
#'
#' @format numeric scalars
MIN_EXPONENT <- -10
MAX_EXPONENT <- 10

#' Weak Identification Tolerance
#'
#' Numerical tolerance for weak identification detection.
#' Prevents division by near-zero values.
#'
#' @format numeric scalar
WEAK_ID_TOLERANCE <- 1e-6

#' Instrument Standard Deviation Threshold
#'
#' Numerical tolerance for instrument validity checking.
#' Used to detect zero-variance Lewbel instruments.
#'
#' @format numeric scalar
INSTRUMENT_SD_THRESHOLD <- 1e-10

# Bootstrap and Simulation Parameters -----------------------------------------

#' Default Bootstrap Replications
#'
#' Standard number of bootstrap replications for inference.
#'
#' @format numeric scalar
DEFAULT_BOOTSTRAP_REPS <- 100

#' Parallel Processing Worker Offset
#'
#' Number of cores to reserve for system operations during parallel processing.
#'
#' @format numeric scalar
DEFAULT_PARALLEL_WORKER_OFFSET <- 1

# Degrees of Freedom Adjustment Methods ---------------------------------------

#' Degrees of Freedom Adjustment Methods
#'
#' Valid methods for statistical inference calculations.
#'
#' @format character scalars
DF_ADJUST_ASYMPTOTIC <- "asymptotic"
DF_ADJUST_FINITE <- "finite"

# Display and Formatting Constants --------------------------------------------

#' Display Precision
#'
#' Number of decimal places for statistical output tables.
#'
#' @format numeric scalar
DISPLAY_DIGITS <- 4

#' Plot Base Font Size
#'
#' Standard font size for all plots in the package.
#'
#' @format numeric scalar
PLOT_BASE_FONT_SIZE <- 14

#' Histogram Bins
#'
#' Default number of bins for histogram plots.
#'
#' @format numeric scalar
HISTOGRAM_BINS <- 50

#' Plot Display Limits
#'
#' Maximum number of examples to show in plots and tables.
#'
#' @format numeric scalars
PLOT_DISPLAY_LIMIT <- 20
MIN_BOOTSTRAP_EXAMPLES <- 5

# Environment Variables -------------------------------------------------------

#' Verbose Environment Variable Configuration
#'
#' Environment variable names and values for controlling package verbosity.
#'
#' @format character scalars
VERBOSE_ENV_VAR <- "VERBOSE"
VERBOSE_DEFAULT <- "TRUE"
VERBOSE_DISABLE <- "FALSE"

# File Extensions and System Integration --------------------------------------

#' Stata File Extensions
#'
#' Standard file extensions for Stata integration.
#'
#' @format character scalars
STATA_DO_EXT <- ".do"
STATA_LOG_EXT <- ".log"

#' System Exit Codes
#'
#' Standard exit codes for system call verification.
#'
#' @format numeric scalar
SUCCESS_EXIT_CODE <- 0

# Mathematical Constants ------------------------------------------------------

#' Statistical Calculation Constants
#'
#' Standard multipliers and indices for statistical calculations.
#'
#' @format numeric scalars
TWO_TAILED_MULTIPLIER <- 2
POINT_ID_TAU <- 0
LOWER_BOUND_IDX <- 1
UPPER_BOUND_IDX <- 2

# Complex Constants (Arrays and Lists) ----------------------------------------

#' Required Stata Packages
#'
#' Stata packages required for hetid functionality.
#'
#' @format character vector
STATA_REQUIRED_PACKAGES <- c("ranktest", "ivreg2", "ivreg2h")

#' Stata Executable Names by Platform
#'
#' Platform-specific executable names for Stata detection.
#'
#' @format character vectors
STATA_EXECUTABLES_MAC <- c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata")
STATA_EXECUTABLES_UNIX <- c("stata", "stata-mp", "stata-se")

#' Plot Color Scheme
#'
#' Consistent color mapping for estimation method plots.
#'
#' @format named character vector
PLOT_COLORS <- c("OLS (Biased)" = "#d95f02", "2SLS (Lewbel)" = "#1b9e77")

#' Plot Labels
#'
#' Consistent labeling for estimation methods in plots.
#'
#' @format named character vector
PLOT_LABELS <- c("ols_gamma1" = "OLS (Biased)", "tsls_gamma1" = "2SLS (Lewbel)")
```

### Step 2: Update NAMESPACE and Documentation
```bash
# Update documentation
devtools::document()
# Expected: Updated man/constants.Rd created
```

## HIGH RISK VARIABLES - IMMEDIATE REFACTORING

### Variable: WEAK_INSTRUMENT_F_THRESHOLD
**Risk:** HIGH
**Files affected:** R/analysis.R, R/visualization.R
**Usage count:** 5 locations

#### Step 1: Update R/analysis.R
```r
# Line 81: Replace `mean(results_clean$first_stage_F < 10) * 100`
# with `mean(results_clean$first_stage_F < WEAK_INSTRUMENT_F_THRESHOLD) * 100`

# Line 124: Replace `mean(results_clean$first_stage_F < 10) * 100`
# with `mean(results_clean$first_stage_F < WEAK_INSTRUMENT_F_THRESHOLD) * 100`
```

#### Step 2: Update R/visualization.R
```r
# Line 182: Replace `mean(results_clean$first_stage_F < 10, na.rm = TRUE) * 100`
# with `mean(results_clean$first_stage_F < WEAK_INSTRUMENT_F_THRESHOLD, na.rm = TRUE) * 100`

# Line 196: Replace `ggplot2::geom_vline(xintercept = 10, linetype = "dashed", color = "red")`
# with `ggplot2::geom_vline(xintercept = WEAK_INSTRUMENT_F_THRESHOLD, linetype = "dashed", color = "red")`

# Line 198: Replace `label = "F = 10"`
# with `label = paste("F =", WEAK_INSTRUMENT_F_THRESHOLD)`

# Line 328: Replace `mean(results_clean$first_stage_F < 10, na.rm = TRUE) * 100`
# with `mean(results_clean$first_stage_F < WEAK_INSTRUMENT_F_THRESHOLD, na.rm = TRUE) * 100`
```

#### Step 3: Verification
```bash
testthat::test_file("tests/testthat/test-analysis.R")
testthat::test_file("tests/testthat/test-visualization.R")
# Expected: All tests pass
# If fail: git checkout R/analysis.R R/visualization.R && investigate
```

### Variable: ALPHA_LEVEL
**Risk:** HIGH
**Files affected:** R/data-generation.R, R/estimation.R, R/utils-df.R
**Usage count:** 4 locations

#### Step 1: Update R/data-generation.R
```r
# Line 220: Replace `if (p_value < 0.05)`
# with `if (p_value < ALPHA_LEVEL)`
```

#### Step 2: Update R/estimation.R
```r
# Line 221: Replace `alpha = 0.05, df_adjust = df_adjust`
# with `alpha = ALPHA_LEVEL, df_adjust = df_adjust`

# Line 321: Replace `alpha = 0.05, df_adjust = df_adjust`
# with `alpha = ALPHA_LEVEL, df_adjust = df_adjust`
```

#### Step 3: Update R/utils-df.R
```r
# Line 27: Replace `get_critical_value <- function(n, k, alpha = 0.05, ...)`
# with `get_critical_value <- function(n, k, alpha = ALPHA_LEVEL, ...)`
```

#### Step 4: Verification
```bash
testthat::test_file("tests/testthat/test-data-generation.R")
testthat::test_file("tests/testthat/test-estimation.R")
testthat::test_file("tests/testthat/test-utils-df.R")
# Expected: All tests pass
# If fail: git checkout affected files && investigate
```

### Variable: DEFAULT_BOOTSTRAP_REPS
**Risk:** HIGH
**Files affected:** R/estimation.R, R/utils.R
**Usage count:** 4 locations

#### Step 1: Update R/estimation.R
```r
# Line 58: Replace `b_reps = 100`
# with `b_reps = DEFAULT_BOOTSTRAP_REPS`
```

#### Step 2: Update R/utils.R
```r
# Line 96: Replace `bootstrap_reps = 100`
# with `bootstrap_reps = DEFAULT_BOOTSTRAP_REPS`

# Line 94: Replace `n_reps_by_n = 100`
# with `n_reps_by_n = DEFAULT_BOOTSTRAP_REPS`

# Line 95: Replace `n_reps_by_delta = 100`
# with `n_reps_by_delta = DEFAULT_BOOTSTRAP_REPS`
```

#### Step 3: Verification
```bash
testthat::test_file("tests/testthat/test-estimation.R")
testthat::test_file("tests/testthat/test-utils.R")
# Expected: All tests pass
# If fail: git checkout R/estimation.R R/utils.R && investigate
```

### Variable: DEFAULT_PARALLEL_WORKER_OFFSET
**Risk:** HIGH
**Files affected:** R/simulation.R
**Usage count:** 4 locations

#### Step 1: Update R/simulation.R
```r
# Line 32: Replace `workers = future::availableCores() - 1`
# with `workers = future::availableCores() - DEFAULT_PARALLEL_WORKER_OFFSET`

# Line 115: Replace `workers = future::availableCores() - 1`
# with `workers = future::availableCores() - DEFAULT_PARALLEL_WORKER_OFFSET`

# Line 191: Replace `workers = future::availableCores() - 1`
# with `workers = future::availableCores() - DEFAULT_PARALLEL_WORKER_OFFSET`

# Line 280: Replace `workers = future::availableCores() - 1`
# with `workers = future::availableCores() - DEFAULT_PARALLEL_WORKER_OFFSET`
```

#### Step 2: Verification
```bash
testthat::test_file("tests/testthat/test-simulation.R")
# Expected: All tests pass
# If fail: git checkout R/simulation.R && investigate
```

### Variable: DF_ADJUST_ASYMPTOTIC
**Risk:** HIGH
**Files affected:** R/utils.R, R/simulation-helpers.R, R/utils-df.R
**Usage count:** 7 locations

#### Step 1: Update R/utils.R
```r
# Line 109: Replace `df_adjust = "asymptotic"`
# with `df_adjust = DF_ADJUST_ASYMPTOTIC`
```

#### Step 2: Update R/simulation-helpers.R
```r
# Line 7: Replace `df_adjust = "asymptotic"`
# with `df_adjust = DF_ADJUST_ASYMPTOTIC`

# Line 44: Replace `df_adjust = "asymptotic"`
# with `df_adjust = DF_ADJUST_ASYMPTOTIC`
```

#### Step 3: Update R/utils-df.R
```r
# Line 9: Replace `df_adjust = "asymptotic"`
# with `df_adjust = DF_ADJUST_ASYMPTOTIC`

# Line 27: Replace `get_critical_value <- function(n, k, alpha = ALPHA_LEVEL, df_adjust = "asymptotic")`
# with `get_critical_value <- function(n, k, alpha = ALPHA_LEVEL, df_adjust = DF_ADJUST_ASYMPTOTIC)`

# Line 41: Replace `extract_se_lm <- function(lm_fit, df_adjust = "asymptotic")`
# with `extract_se_lm <- function(lm_fit, df_adjust = DF_ADJUST_ASYMPTOTIC)`

# Line 69: Replace `extract_se_ivreg <- function(ivreg_fit, df_adjust = "asymptotic")`
# with `extract_se_ivreg <- function(ivreg_fit, df_adjust = DF_ADJUST_ASYMPTOTIC)`
```

#### Step 4: Verification
```bash
testthat::test_file("tests/testthat/test-utils.R")
testthat::test_file("tests/testthat/test-simulation-helpers.R")
testthat::test_file("tests/testthat/test-utils-df.R")
# Expected: All tests pass
# If fail: git checkout affected files && investigate
```

### Variable: DISPLAY_DIGITS
**Risk:** HIGH
**Files affected:** R/analysis.R
**Usage count:** 5 locations

#### Step 1: Update R/analysis.R
```r
# Line 75: Replace `print(knitr::kable(summary_table, digits = 4))`
# with `print(knitr::kable(summary_table, digits = DISPLAY_DIGITS))`

# Line 115: Replace `print(knitr::kable(bounds_summary, digits = 4))`
# with `print(knitr::kable(bounds_summary, digits = DISPLAY_DIGITS))`

# Line 200: Replace `~ round(., 4)`
# with `~ round(., DISPLAY_DIGITS)`

# Line 260: Replace `print(knitr::kable(n_summary, digits = 4))`
# with `print(knitr::kable(n_summary, digits = DISPLAY_DIGITS))`

# Line 321: Replace `print(knitr::kable(delta_summary, digits = 4))`
# with `print(knitr::kable(delta_summary, digits = DISPLAY_DIGITS))`
```

#### Step 2: Verification
```bash
testthat::test_file("tests/testthat/test-analysis.R")
# Expected: All tests pass
# If fail: git checkout R/analysis.R && investigate
```

### Variable: PLOT_BASE_FONT_SIZE
**Risk:** HIGH
**Files affected:** R/visualization.R
**Usage count:** 5 locations

#### Step 1: Update R/visualization.R
```r
# Line 60: Replace `base_size = 14`
# with `base_size = PLOT_BASE_FONT_SIZE`

# Line 106: Replace `base_size = 14`
# with `base_size = PLOT_BASE_FONT_SIZE`

# Line 148: Replace `base_size = 14`
# with `base_size = PLOT_BASE_FONT_SIZE`

# Line 209: Replace `base_size = 14`
# with `base_size = PLOT_BASE_FONT_SIZE`

# Line 276: Replace `base_size = 14`
# with `base_size = PLOT_BASE_FONT_SIZE`
```

#### Step 2: Verification
```bash
testthat::test_file("tests/testthat/test-visualization.R")
# Expected: All tests pass
# If fail: git checkout R/visualization.R && investigate
```

## MEDIUM RISK VARIABLES - BATCH REFACTORING

### Batch 1: Numerical Tolerances and Bounds
**Variables:** Z_CRITICAL_95, MIN_EXPONENT, MAX_EXPONENT, WEAK_ID_TOLERANCE, INSTRUMENT_SD_THRESHOLD, TWO_TAILED_MULTIPLIER, POINT_ID_TAU, LOWER_BOUND_IDX, UPPER_BOUND_IDX

#### Step 1: Update R/visualization.R
```r
# Lines 251-252: Replace confidence interval calculation
# OLD: x = bound_lower_tau_set - 1.96 * bound_se_lower
#      xend = bound_upper_tau_set + 1.96 * bound_se_upper
# NEW: x = bound_lower_tau_set - Z_CRITICAL_95 * bound_se_lower
#      xend = bound_upper_tau_set + Z_CRITICAL_95 * bound_se_upper
```

#### Step 2: Update R/data-generation.R
```r
# Line 99: Replace exponent bounds
# OLD: exponent <- pmin(pmax(exponent, -10), 10)
# NEW: exponent <- pmin(pmax(exponent, MIN_EXPONENT), MAX_EXPONENT)

# Line 208: Replace two-tailed test multiplier
# OLD: p_value <- 2 * (1 - stats::pnorm(abs(test_stat)))
# NEW: p_value <- TWO_TAILED_MULTIPLIER * (1 - stats::pnorm(abs(test_stat)))
```

#### Step 3: Update R/estimation.R
```r
# Line 77: Replace weak identification tolerance
# OLD: if (abs(cov_z_w2sq) < 1e-6)
# NEW: if (abs(cov_z_w2sq) < WEAK_ID_TOLERANCE)

# Line 256: Replace instrument SD threshold
# OLD: stats::sd(lewbel_iv, na.rm = TRUE) < 1e-10
# NEW: stats::sd(lewbel_iv, na.rm = TRUE) < INSTRUMENT_SD_THRESHOLD

# Line 333: Replace point identification tau
# OLD: calculate_lewbel_bounds(df, 0, ...)
# NEW: calculate_lewbel_bounds(df, POINT_ID_TAU, ...)

# Lines 114-115: Replace bootstrap result indices
# OLD: boot_result$t[, 1], boot_result$t[, 2]
# NEW: boot_result$t[, LOWER_BOUND_IDX], boot_result$t[, UPPER_BOUND_IDX]
```

#### Step 4: Verification
```bash
testthat::test_file("tests/testthat/test-visualization.R")
testthat::test_file("tests/testthat/test-data-generation.R")
testthat::test_file("tests/testthat/test-estimation.R")
# Expected: All tests pass
```

### Batch 2: String Constants and Configuration
**Variables:** DF_ADJUST_FINITE, STATA_REQUIRED_PACKAGES, STATA_EXECUTABLES_MAC, STATA_EXECUTABLES_UNIX, PLOT_COLORS, PLOT_LABELS

#### Step 1: Update R/simulation-helpers.R
```r
# Line 50: Replace finite df adjustment
# OLD: df_adjust = "finite"
# NEW: df_adjust = DF_ADJUST_FINITE
```

#### Step 2: Update R/utils-df.R
```r
# Line 10: Replace finite df adjustment check
# OLD: if (df_adjust == "finite")
# NEW: if (df_adjust == DF_ADJUST_FINITE)
```

#### Step 3: Update R/utils-hetid.R
```r
# Lines 101, 106, 111: Replace individual package checks
# OLD: capture which ranktest
#      capture which ivreg2
#      capture which ivreg2h
# NEW: Use loop with STATA_REQUIRED_PACKAGES

# Lines 32-34: Replace Mac executable list
# OLD: c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata")
# NEW: STATA_EXECUTABLES_MAC

# Lines 56-58: Replace Unix executable list
# OLD: c("stata", "stata-mp", "stata-se")
# NEW: STATA_EXECUTABLES_UNIX
```

#### Step 4: Update R/visualization.R
```r
# Line 62: Replace color mapping
# OLD: c("OLS (Biased)" = "#d95f02", "2SLS (Lewbel)" = "#1b9e77")
# NEW: PLOT_COLORS

# Lines 31-32: Replace plot labels
# OLD: "ols_gamma1", "tsls_gamma1", "OLS (Biased)", "2SLS (Lewbel)"
# NEW: Use PLOT_LABELS mapping
```

#### Step 5: Verification
```bash
testthat::test_file("tests/testthat/test-simulation-helpers.R")
testthat::test_file("tests/testthat/test-utils-df.R")
testthat::test_file("tests/testthat/test-utils-hetid.R")
testthat::test_file("tests/testthat/test-visualization.R")
# Expected: All tests pass
```

## LOW RISK VARIABLES - FINAL CLEANUP

### Batch 3: Display and System Constants
**Variables:** HISTOGRAM_BINS, PLOT_DISPLAY_LIMIT, MIN_BOOTSTRAP_EXAMPLES, VERBOSE_ENV_VAR, VERBOSE_DEFAULT, VERBOSE_DISABLE, STATA_DO_EXT, STATA_LOG_EXT, SUCCESS_EXIT_CODE

#### Step 1: Update R/visualization.R
```r
# Line 195: Replace histogram bins
# OLD: bins = 50
# NEW: bins = HISTOGRAM_BINS

# Line 244: Replace display limit
# OLD: slice_head(bootstrap_examples, n = 20)
# NEW: slice_head(bootstrap_examples, n = PLOT_DISPLAY_LIMIT)

# Line 239: Replace minimum examples threshold
# OLD: if (nrow(bootstrap_examples) < 5)
# NEW: if (nrow(bootstrap_examples) < MIN_BOOTSTRAP_EXAMPLES)
```

#### Step 2: Update R/messager.R
```r
# Line 16: Replace environment variable handling
# OLD: "VERBOSE", "TRUE", "FALSE"
# NEW: VERBOSE_ENV_VAR, VERBOSE_DEFAULT, VERBOSE_DISABLE
```

#### Step 3: Update R/utils-hetid.R
```r
# Line 98: Replace Stata do file extension
# OLD: tempfile(fileext = ".do")
# NEW: tempfile(fileext = STATA_DO_EXT)

# Line 120: Replace Stata log file extension
# OLD: tempfile(fileext = ".log")
# NEW: tempfile(fileext = STATA_LOG_EXT)

# Line 128: Replace success exit code
# OLD: result == 0
# NEW: result == SUCCESS_EXIT_CODE
```

#### Step 4: Verification
```bash
testthat::test_file("tests/testthat/test-visualization.R")
testthat::test_file("tests/testthat/test-messager.R")
testthat::test_file("tests/testthat/test-utils-hetid.R")
# Expected: All tests pass
```

## IMPLEMENTATION CHECKLIST

### Pre-Refactoring Baseline
```bash
[ ] Pre-refactoring: devtools::check() passes with 0 errors, 0 warnings, 0 notes
[ ] Save baseline output: devtools::check() > baseline_check.txt
[ ] Run full test suite: devtools::test() > baseline_tests.txt
[ ] Generate example outputs for comparison (if applicable)
```

### Phase 1: Constants File Creation
```bash
[ ] Create R/constants.R with all constant definitions
[ ] Add roxygen2 documentation for each constant group
[ ] Update documentation: devtools::document()
[ ] Verify constants.Rd created in man/ directory
[ ] Test constants accessibility: exists("WEAK_INSTRUMENT_F_THRESHOLD")
```

### Phase 2: HIGH Risk Variables (Execute in Order)
```bash
[ ] WEAK_INSTRUMENT_F_THRESHOLD refactoring
    [ ] Update R/analysis.R (lines 81, 124)
    [ ] Update R/visualization.R (lines 182, 196, 198, 328)
    [ ] Test: testthat::test_file("tests/testthat/test-analysis.R")
    [ ] Test: testthat::test_file("tests/testthat/test-visualization.R")
    [ ] Rollback ready: git checkout R/analysis.R R/visualization.R

[ ] ALPHA_LEVEL refactoring
    [ ] Update R/data-generation.R (line 220)
    [ ] Update R/estimation.R (lines 221, 321)
    [ ] Update R/utils-df.R (line 27)
    [ ] Test: testthat::test_file("tests/testthat/test-data-generation.R")
    [ ] Test: testthat::test_file("tests/testthat/test-estimation.R")
    [ ] Test: testthat::test_file("tests/testthat/test-utils-df.R")
    [ ] Rollback ready: git checkout R/data-generation.R R/estimation.R R/utils-df.R

[ ] DEFAULT_BOOTSTRAP_REPS refactoring
    [ ] Update R/estimation.R (line 58)
    [ ] Update R/utils.R (lines 94, 95, 96)
    [ ] Test: testthat::test_file("tests/testthat/test-estimation.R")
    [ ] Test: testthat::test_file("tests/testthat/test-utils.R")
    [ ] Rollback ready: git checkout R/estimation.R R/utils.R

[ ] DEFAULT_PARALLEL_WORKER_OFFSET refactoring
    [ ] Update R/simulation.R (lines 32, 115, 191, 280)
    [ ] Test: testthat::test_file("tests/testthat/test-simulation.R")
    [ ] Rollback ready: git checkout R/simulation.R

[ ] DF_ADJUST_ASYMPTOTIC refactoring
    [ ] Update R/utils.R (line 109)
    [ ] Update R/simulation-helpers.R (lines 7, 44)
    [ ] Update R/utils-df.R (lines 9, 27, 41, 69)
    [ ] Test: testthat::test_file("tests/testthat/test-utils.R")
    [ ] Test: testthat::test_file("tests/testthat/test-simulation-helpers.R")
    [ ] Test: testthat::test_file("tests/testthat/test-utils-df.R")
    [ ] Rollback ready: git checkout R/utils.R R/simulation-helpers.R R/utils-df.R

[ ] DISPLAY_DIGITS refactoring
    [ ] Update R/analysis.R (lines 75, 115, 200, 260, 321)
    [ ] Test: testthat::test_file("tests/testthat/test-analysis.R")
    [ ] Rollback ready: git checkout R/analysis.R

[ ] PLOT_BASE_FONT_SIZE refactoring
    [ ] Update R/visualization.R (lines 60, 106, 148, 209, 276)
    [ ] Test: testthat::test_file("tests/testthat/test-visualization.R")
    [ ] Rollback ready: git checkout R/visualization.R
```

### Phase 3: MEDIUM Risk Variables (Batch Processing)
```bash
[ ] Numerical tolerances batch
    [ ] Update R/visualization.R (Z_CRITICAL_95)
    [ ] Update R/data-generation.R (MIN/MAX_EXPONENT, TWO_TAILED_MULTIPLIER)
    [ ] Update R/estimation.R (WEAK_ID_TOLERANCE, INSTRUMENT_SD_THRESHOLD, etc.)
    [ ] Test affected files
    [ ] Rollback ready: git checkout affected files

[ ] String constants batch
    [ ] Update R/simulation-helpers.R (DF_ADJUST_FINITE)
    [ ] Update R/utils-df.R (DF_ADJUST_FINITE)
    [ ] Update R/utils-hetid.R (STATA constants)
    [ ] Update R/visualization.R (PLOT_COLORS, PLOT_LABELS)
    [ ] Test affected files
    [ ] Rollback ready: git checkout affected files
```

### Phase 4: LOW Risk Variables (Final Cleanup)
```bash
[ ] Display and system constants
    [ ] Update R/visualization.R (HISTOGRAM_BINS, display limits)
    [ ] Update R/messager.R (VERBOSE constants)
    [ ] Update R/utils-hetid.R (file extensions, exit codes)
    [ ] Test affected files
    [ ] Rollback ready: git checkout affected files
```

### Phase 5: Final Verification
```bash
[ ] Update NAMESPACE if needed: devtools::document()
[ ] Full package check: devtools::check()
    [ ] Compare with baseline: diff baseline_check.txt current_check.txt
    [ ] Expected: Identical results (0 errors, 0 warnings, 0 notes)
[ ] Full test suite: devtools::test()
    [ ] Compare with baseline: diff baseline_tests.txt current_tests.txt
    [ ] Expected: All tests pass, no new failures
[ ] Build package documentation: pkgdown::build_site()
    [ ] Expected: No errors, consistent output
[ ] Example verification (if applicable):
    [ ] Run key examples and compare outputs byte-for-byte
    [ ] Expected: Identical results to baseline
```

### Rollback Instructions

#### Individual File Rollback
```bash
# For any failing step, immediately rollback affected files:
git checkout R/filename.R
# Re-run tests to confirm rollback successful
testthat::test_file("tests/testthat/test-filename.R")
```

#### Complete Rollback
```bash
# If major issues arise, rollback all changes:
git checkout R/constants.R
git checkout R/*.R
devtools::document()  # Restore original documentation
devtools::check()     # Verify return to baseline
```

### Success Criteria

1. **Functional Equivalence:** All tests pass with identical results
2. **Package Check:** `devtools::check()` produces identical output to baseline
3. **Documentation:** All constants properly documented with roxygen2
4. **Maintainability:** All magic numbers replaced with named constants
5. **Consistency:** Related constants grouped logically
6. **Performance:** No performance degradation in key functions

### Post-Refactoring Benefits

1. **Maintainability:** Single location for all configuration values
2. **Consistency:** Guaranteed identical values across all usage sites
3. **Documentation:** Each constant documented with context and source
4. **Testability:** Constants can be easily modified for testing scenarios
5. **Discoverability:** All magic values visible in one location

## DELIVERABLE SUMMARY

This refactoring plan provides:

✅ **Complete variable inventory** with risk assessment
✅ **Copy-paste ready code changes** for each constant
✅ **Specific test commands** after each modification
✅ **Rollback instructions** for each step
✅ **Final verification checklist** ensuring 100% compatibility

**Execution Time Estimate:** 2-3 hours for careful implementation
**Risk Level:** LOW (with proper testing and rollback procedures)
**Behavioral Impact:** ZERO (all changes preserve exact functionality)
