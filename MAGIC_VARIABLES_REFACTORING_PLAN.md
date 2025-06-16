# Magic Variables Refactoring Implementation Plan

## Overview

This document provides a comprehensive, step-by-step plan to refactor all magic variables, constants, and NSE (non-standard evaluation) variables in the hetid R package according to 2025 best practices. The plan ensures that the code behaves **exactly** the same before and after refactoring, with verifiable proof through tests, pre-commit hooks, and devtools checks.

## Prerequisites

### Required Packages
Install these packages before starting:
```r
install.packages(c("checkglobals", "rlang", "options", "withr"))
```

### Verification Setup
1. **Baseline Testing**: Run full test suite and document results
2. **Pre-commit Hooks**: Ensure all current hooks pass
3. **R CMD Check**: Verify `devtools::check()` passes with no errors/warnings
4. **Documentation**: Generate baseline documentation with `devtools::document()`

## Phase 1: NSE Variables Refactoring (Priority: HIGH)

### Step 1.1: Replace globalVariables with .data Pronoun

**Files to modify**: `R/globals.R`, `R/analysis.R`

**Current Issue**: Package uses `globalVariables()` for tidyverse NSE variables

**Action**:
1. **Backup**: Create git branch `refactor/nse-variables`
2. **Remove globalVariables**: Delete `R/globals.R` entirely
3. **Update imports**: Add to NAMESPACE:
   ```r
   #' @importFrom rlang .data
   ```
4. **Replace NSE references**: In all files, replace bare column names with `.data$column_name`

**Specific Changes**:

**File: `R/analysis.R`**
- Line 159: `dplyr::filter(results_main, !is.na(bound_se_lower))` → `dplyr::filter(results_main, !is.na(.data$bound_se_lower))`
- Line 180: `dplyr::select(bootstrap_examples, sim_id, lower = bound_lower_tau_set, ...)` → `dplyr::select(bootstrap_examples, .data$sim_id, lower = .data$bound_lower_tau_set, ...)`
- Line 234: `dplyr::group_by(results_by_n, sample_size)` → `dplyr::group_by(results_by_n, .data$sample_size)`
- Line 237: `mean(tsls_gamma1, na.rm = TRUE)` → `mean(.data$tsls_gamma1, na.rm = TRUE)`
- Line 241: `mean(first_stage_F, na.rm = TRUE)` → `mean(.data$first_stage_F, na.rm = TRUE)`
- Line 288: `dplyr::group_by(results_by_delta, delta_het)` → `dplyr::group_by(results_by_delta, .data$delta_het)`
- Line 291: `mean(tsls_gamma1, na.rm = TRUE)` → `mean(.data$tsls_gamma1, na.rm = TRUE)`
- Line 295: `mean(first_stage_F, na.rm = TRUE)` → `mean(.data$first_stage_F, na.rm = TRUE)`
- Line 297: `bound_upper_tau_set - bound_lower_tau_set` → `.data$bound_upper_tau_set - .data$bound_lower_tau_set`

**Verification**:
```r
# After each file modification
devtools::test()
checkglobals::checkglobals("R/", verbose = TRUE)
devtools::check()
```

### Step 1.2: Update Visualization Functions

**File: `R/visualization.R`**

**Changes**:
- Line 24: `dplyr::select(results_clean, ols_gamma1, tsls_gamma1)` → `dplyr::select(results_clean, .data$ols_gamma1, .data$tsls_gamma1)`
- Line 29: `Estimator = dplyr::recode(Estimator, ...)` → `Estimator = dplyr::recode(.data$Estimator, ...)`
- Line 39: `ggplot2::aes(x = Estimate, fill = Estimator)` → `ggplot2::aes(x = .data$Estimate, fill = .data$Estimator)`
- Line 88: `dplyr::filter(results_by_n, !is.na(tsls_gamma1))` → `dplyr::filter(results_by_n, !is.na(.data$tsls_gamma1))`
- Line 93: `ggplot2::aes(x = factor(sample_size), y = tsls_gamma1)` → `ggplot2::aes(x = factor(.data$sample_size), y = .data$tsls_gamma1)`
- Line 131: `dplyr::filter(results_by_delta, !is.na(tsls_gamma1))` → `dplyr::filter(results_by_delta, !is.na(.data$tsls_gamma1))`
- Line 136: `ggplot2::aes(x = factor(delta_het), y = tsls_gamma1)` → `ggplot2::aes(x = factor(.data$delta_het), y = .data$tsls_gamma1)`
- Line 194: `ggplot2::ggplot(results_clean, ggplot2::aes(x = first_stage_F))` → `ggplot2::ggplot(results_clean, ggplot2::aes(x = .data$first_stage_F))`
- Line 245: `dplyr::mutate(plot_data, sim_id_ordered = dplyr::row_number())` → `dplyr::mutate(plot_data, sim_id_ordered = dplyr::row_number())`
- Line 250-253: Update all aesthetic mappings to use `.data$` pronoun
- Line 258-261: Update all aesthetic mappings to use `.data$` pronoun

**Verification**:
```r
devtools::test()
devtools::check()
```

## Phase 2: Constants Extraction (Priority: HIGH)

### Step 2.1: Create Locked Constants Environment

**Action**: Create `data-raw/internal-constants.R`

```r
# data-raw/internal-constants.R
# Create locked environment for immutable constants
constants_env <- new.env(parent = emptyenv())

# Populate constants
with(constants_env, {
  # Statistical thresholds
  WEAK_INSTRUMENT_F_THRESHOLD <- 10L
  ALPHA_LEVEL <- 0.05
  Z_CRITICAL_95 <- 1.96

  # Numerical tolerances
  MIN_EXPONENT <- -10L
  MAX_EXPONENT <- 10L
  WEAK_ID_TOLERANCE <- 1e-6
  INSTRUMENT_SD_THRESHOLD <- 1e-10

  # Default simulation parameters
  DEFAULT_NUM_SIMULATIONS <- 200L
  DEFAULT_MAIN_SAMPLE_SIZE <- 500L
  DEFAULT_SAMPLE_SIZES <- c(250L, 500L, 1000L, 2000L)
  DEFAULT_VERIFICATION_SAMPLE_SIZE <- 10000L
  DEFAULT_TEST_DATA_SIZE <- 1000L

  # Bootstrap parameters
  DEFAULT_BOOTSTRAP_REPS <- 100L
  DEFAULT_BOOTSTRAP_SUBSET_SIZE <- 10L
  DEFAULT_BOOTSTRAP_DEMO_SIZE <- 5L

  # Demo parameters
  DEMO_SAMPLE_SIZES <- c(500L, 1000L)
  DEMO_BOOTSTRAP_REPS <- 50L
  DEMO_N_REPS <- 50L
  DEMO_BOOTSTRAP_SIZE <- 3L

  # Parallel processing
  DEFAULT_PARALLEL_WORKER_OFFSET <- 1L

  # Display formatting
  DISPLAY_DIGITS <- 4L
  BOOTSTRAP_TABLE_DISPLAY_LIMIT <- 10L

  # Plotting constants
  PLOT_BASE_FONT_SIZE <- 14L
  PLOT_HISTOGRAM_BINS <- 50L
  PLOT_LINE_WIDTH_THICK <- 3L
  PLOT_LINE_WIDTH_NORMAL <- 2L
  PLOT_LINE_WIDTH_THIN <- 1L
  PLOT_BOOTSTRAP_DISPLAY_LIMIT <- 20L
  PLOT_MIN_BOOTSTRAP_THRESHOLD <- 5L

  # Mathematical constants
  DEFAULT_X_MEAN <- 2
  DEFAULT_X_SD <- 1
  TWO_TAILED_MULTIPLIER <- 2L
  POINT_ID_TAU <- 0
  SUCCESS_EXIT_CODE <- 0L

  # Seed management
  DEFAULT_BASE_SEED <- 123L
  DEFAULT_TEST_SEED <- 42L
  SEED_MULTIPLIER_MAIN <- 1000L
  SEED_MULTIPLIER_BOOTSTRAP <- 3000L
  SEED_OFFSET_BY_N <- 1L
  SEED_OFFSET_BY_DELTA <- 2L

  # Array indexing (for clarity)
  FIRST_ELEMENT_IDX <- 1L
  SECOND_ELEMENT_IDX <- 2L
  LOWER_BOUND_IDX <- 1L
  UPPER_BOUND_IDX <- 2L
  FIRST_COLUMN_IDX <- 1L
  F_STATISTIC_IDX <- 1L
})

# Lock environment to prevent modification
lockEnvironment(constants_env, TRUE)

# Save to R/sysdata.rda
usethis::use_data(constants_env, internal = TRUE, overwrite = TRUE)
```

**Run**:
```r
source("data-raw/internal-constants.R")
```

### Step 2.2: Create User Options System

**Action**: Create `R/options.R`

```r
# R/options.R
#' @importFrom options make_option_getter
NULL

#' Get hetid package options with fallback to constants
#'
#' @param option_name Character. Name of the option to retrieve
#' @return The option value, falling back to constants_env if not set
#' @keywords internal
hetid_opt <- function(option_name) {
  # Define option mappings to constants
  option_map <- list(
    "display_digits" = "DISPLAY_DIGITS",
    "alpha_level" = "ALPHA_LEVEL",
    "weak_f_threshold" = "WEAK_INSTRUMENT_F_THRESHOLD",
    "parallel_offset" = "DEFAULT_PARALLEL_WORKER_OFFSET",
    "bootstrap_reps" = "DEFAULT_BOOTSTRAP_REPS"
  )

  # Get R option with hetid prefix
  r_option_name <- paste0("hetid.", option_name)
  user_value <- getOption(r_option_name)

  # If user hasn't set it, use constant
  if (is.null(user_value)) {
    const_name <- option_map[[option_name]]
    if (!is.null(const_name) && exists(const_name, envir = constants_env)) {
      return(get(const_name, envir = constants_env))
    }
  }

  user_value
}
```

### Step 2.3: Create String Constants

**Action**: Create `R/string-constants.R`

```r
# R/string-constants.R
#' Internal string constants
#' @keywords internal
.hetid_strings <- function() {
  list(
    # Degrees of freedom adjustment methods
    df_adjust = list(
      ASYMPTOTIC = "asymptotic",
      FINITE = "finite"
    ),

    # Column names
    columns = list(
      STD_ERROR = "Std. Error",
      Y1 = "Y1",
      Y2 = "Y2",
      XK = "Xk",
      Z = "Z",
      X1 = "X1",
      X2 = "X2",
      Z1 = "Z1",
      Z2 = "Z2",
      # Remapped names
      Y_MAPPED = "y",
      P_MAPPED = "P",
      X1_MAPPED = "X1"
    ),

    # Variable names
    variables = list(
      DEFAULT_ENDOG_VAR = "Y2",
      DEFAULT_EXOG_VARS = "Xk"
    ),

    # Stata integration
    stata = list(
      PACKAGES = c("ranktest", "ivreg2", "ivreg2h"),
      EXECUTABLES_ALL = c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata"),
      EXECUTABLES_UNIX = c("stata", "stata-mp", "stata-se"),
      DO_EXTENSION = ".do",
      LOG_EXTENSION = ".log",
      MAC_PATHS = c(
        "/Applications/Stata/StataSE.app/Contents/MacOS/StataSE",
        "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP",
        "/Applications/Stata/Stata.app/Contents/MacOS/Stata"
      )
    ),

    # Visualization
    plot_labels = list(
      OLS_BIASED = "OLS (Biased)",
      TSLS_LEWBEL = "2SLS (Lewbel)",
      OLS_COLUMN = "ols_gamma1",
      TSLS_COLUMN = "tsls_gamma1"
    ),

    # Plot colors (hex codes)
    plot_colors = list(
      OLS_COLOR = "#d95f02",
      TSLS_COLOR = "#1b9e77"
    ),

    # Environment variables
    env_vars = list(
      VERBOSE = "VERBOSE",
      VERBOSE_TRUE = "TRUE",
      VERBOSE_FALSE = "FALSE"
    )
  )
}
```

### Step 2.4: Replace Magic Numbers in Statistical Functions

**File: `R/analysis.R`**

**Changes**:
- Line 78: `mean(results_clean$first_stage_F < 10)` → `mean(results_clean$first_stage_F < constants_env$WEAK_INSTRUMENT_F_THRESHOLD)`
- Line 121: `mean(results_clean$first_stage_F < 10)` → `mean(results_clean$first_stage_F < constants_env$WEAK_INSTRUMENT_F_THRESHOLD)`
- Line 72: `digits = 4` → `digits = hetid_opt("display_digits")`
- Line 112: `digits = 4` → `digits = hetid_opt("display_digits")`
- Line 189: `n = 10` → `n = constants_env$BOOTSTRAP_TABLE_DISPLAY_LIMIT`
- Line 194: `round(., 4)` → `round(., hetid_opt("display_digits"))`
- Line 251: `digits = 4` → `digits = hetid_opt("display_digits")`
- Line 309: `digits = 4` → `digits = hetid_opt("display_digits")`

**File: `R/data-generation.R`**

**Changes**:
- Line 74: `mean = 2, sd = 1` → `mean = constants_env$DEFAULT_X_MEAN, sd = constants_env$DEFAULT_X_SD`
- Line 95: `pmin(pmax(exponent, -10), 10)` → `pmin(pmax(exponent, constants_env$MIN_EXPONENT), constants_env$MAX_EXPONENT)`
- Line 176: `n_obs = 10000` → `n_obs = constants_env$DEFAULT_VERIFICATION_SAMPLE_SIZE`
- Line 204: `p_value <- 2 * (1 - stats::pnorm(abs(test_stat)))` → `p_value <- constants_env$TWO_TAILED_MULTIPLIER * (1 - stats::pnorm(abs(test_stat)))`
- Line 216: `if (p_value < 0.05)` → `if (p_value < hetid_opt("alpha_level"))`

**File: `R/estimation.R`**

**Changes**:
- Line 58: `b_reps = 100` → `b_reps = hetid_opt("bootstrap_reps")`
- Line 77: `if (abs(cov_z_w2sq) < 1e-6)` → `if (abs(cov_z_w2sq) < constants_env$WEAK_ID_TOLERANCE)`
- Line 114: `boot_result$t[, 1]` → `boot_result$t[, constants_env$LOWER_BOUND_IDX]`
- Line 115: `boot_result$t[, 2]` → `boot_result$t[, constants_env$UPPER_BOUND_IDX]`
- Line 220: `alpha = 0.05` → `alpha = hetid_opt("alpha_level")`
- Line 255: `stats::sd(lewbel_iv, na.rm = TRUE) < 1e-10` → `stats::sd(lewbel_iv, na.rm = TRUE) < constants_env$INSTRUMENT_SD_THRESHOLD`
- Line 276: `summary(first_stage)$fstatistic[1]` → `summary(first_stage)$fstatistic[constants_env$F_STATISTIC_IDX]`
- Line 320: `alpha = 0.05` → `alpha = hetid_opt("alpha_level")`
- Line 332: `calculate_lewbel_bounds(df, 0, ...)` → `calculate_lewbel_bounds(df, constants_env$POINT_ID_TAU, ...)`
- Line 361: `bounds_tau0$bounds[1]` → `bounds_tau0$bounds[constants_env$LOWER_BOUND_IDX]`
- Line 362: `bounds_tau0$bounds[2]` → `bounds_tau0$bounds[constants_env$UPPER_BOUND_IDX]`
- Line 363: `bounds_tau_set$bounds[1]` → `bounds_tau_set$bounds[constants_env$LOWER_BOUND_IDX]`
- Line 364: `bounds_tau_set$bounds[2]` → `bounds_tau_set$bounds[constants_env$UPPER_BOUND_IDX]`
- Line 365: `bounds_tau_set$se[1]` → `bounds_tau_set$se[constants_env$LOWER_BOUND_IDX]`
- Line 366: `bounds_tau_set$se[2]` → `bounds_tau_set$se[constants_env$UPPER_BOUND_IDX]`

**Verification After Each File**:
```r
devtools::test()
devtools::check()
```

### Step 2.5: Replace String Constants

**File: `R/utils-df.R`**

**Changes**:
- Line 9: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 10: `if (df_adjust == "finite")` → `if (df_adjust == .hetid_strings()$df_adjust$FINITE)`
- Line 27: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 28: `if (df_adjust == "finite")` → `if (df_adjust == .hetid_strings()$df_adjust$FINITE)`
- Line 41: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 50: `if (df_adjust == "finite")` → `if (df_adjust == .hetid_strings()$df_adjust$FINITE)`
- Line 67: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 72: `if (df_adjust == "finite")` → `if (df_adjust == .hetid_strings()$df_adjust$FINITE)`
- Line 74: `"Std. Error"` → `.hetid_strings()$columns$STD_ERROR`
- Line 79: `"Std. Error"` → `.hetid_strings()$columns$STD_ERROR`

**File: `R/utils.R`**

**Changes**:
- Line 89: `num_simulations = 200` → `num_simulations = constants_env$DEFAULT_NUM_SIMULATIONS`
- Line 90: `main_sample_size = 500` → `main_sample_size = constants_env$DEFAULT_MAIN_SAMPLE_SIZE`
- Line 91: `sample_sizes = c(250, 500, 1000, 2000)` → `sample_sizes = constants_env$DEFAULT_SAMPLE_SIZES`
- Line 94: `n_reps_by_n = 100` → `n_reps_by_n = constants_env$DEFAULT_BOOTSTRAP_REPS`
- Line 95: `n_reps_by_delta = 100` → `n_reps_by_delta = constants_env$DEFAULT_BOOTSTRAP_REPS`
- Line 96: `bootstrap_reps = 100` → `bootstrap_reps = hetid_opt("bootstrap_reps")`
- Line 97: `bootstrap_subset_size = 10` → `bootstrap_subset_size = constants_env$DEFAULT_BOOTSTRAP_SUBSET_SIZE`
- Line 98: `bootstrap_demo_size = 5` → `bootstrap_demo_size = constants_env$DEFAULT_BOOTSTRAP_DEMO_SIZE`
- Line 107: `endog_var_name = "Y2"` → `endog_var_name = .hetid_strings()$variables$DEFAULT_ENDOG_VAR`
- Line 108: `exog_var_names = "Xk"` → `exog_var_names = .hetid_strings()$variables$DEFAULT_EXOG_VARS`
- Line 109: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 115: `set_seed = 123` → `set_seed = constants_env$DEFAULT_BASE_SEED`
- Line 172: `config$set_seed * 1000` → `config$set_seed * constants_env$SEED_MULTIPLIER_MAIN`
- Line 174: `config$set_seed + 1` → `config$set_seed + constants_env$SEED_OFFSET_BY_N`
- Line 179: `config$set_seed + 2` → `config$set_seed + constants_env$SEED_OFFSET_BY_DELTA`
- Line 183: `config$set_seed * 3000` → `config$set_seed * constants_env$SEED_MULTIPLIER_BOOTSTRAP`

## Phase 3: Simulation and Parallel Processing Constants

### Step 3.1: Update Simulation Files

**File: `R/simulation.R`**

**Changes**:
- Line 29: `seeds$main[1]` → `seeds$main[.HETID_CONSTANTS$FIRST_ELEMENT_IDX]`
- Line 32: `future::availableCores() - 1` → `future::availableCores() - .HETID_CONSTANTS$DEFAULT_PARALLEL_WORKER_OFFSET`
- Line 58: `"asymptotic"` → `.hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 112: `seeds$bootstrap_demo[1]` → `seeds$bootstrap_demo[.HETID_CONSTANTS$FIRST_ELEMENT_IDX]`
- Line 115: `future::availableCores() - 1` → `future::availableCores() - .HETID_CONSTANTS$DEFAULT_PARALLEL_WORKER_OFFSET`
- Line 140: `"asymptotic"` → `.hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 191: `future::availableCores() - 1` → `future::availableCores() - .HETID_CONSTANTS$DEFAULT_PARALLEL_WORKER_OFFSET`
- Line 201: `seeds$by_n[idx, 1]` → `seeds$by_n[idx, .HETID_CONSTANTS$FIRST_ELEMENT_IDX]`
- Line 226: `"asymptotic"` → `.hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 280: `future::availableCores() - 1` → `future::availableCores() - .HETID_CONSTANTS$DEFAULT_PARALLEL_WORKER_OFFSET`
- Line 290: `seeds$by_delta[idx, 1]` → `seeds$by_delta[idx, .HETID_CONSTANTS$FIRST_ELEMENT_IDX]`
- Line 315: `"asymptotic"` → `.hetid_strings()$df_adjust$ASYMPTOTIC`

**File: `R/simulation-helpers.R`**

**Changes**:
- Line 13: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 43: `df_adjust = "asymptotic"` → `df_adjust = .hetid_strings()$df_adjust$ASYMPTOTIC`
- Line 49: `df_adjust = "finite"` → `df_adjust = .hetid_strings()$df_adjust$FINITE`

**File: `R/lewbel-monte-carlo.R`**

**Changes**:
- Line 235: `num_simulations = 100` → `num_simulations = .HETID_CONSTANTS$DEFAULT_BOOTSTRAP_REPS`
- Line 238: `sample_sizes = c(500, 1000)` → `sample_sizes = .HETID_CONSTANTS$DEMO_SAMPLE_SIZES`
- Line 239: `bootstrap_reps = 50` → `bootstrap_reps = .HETID_CONSTANTS$DEMO_BOOTSTRAP_REPS`
- Line 243: `demo_config$n_reps_by_n <- 50` → `demo_config$n_reps_by_n <- .HETID_CONSTANTS$DEMO_N_REPS`
- Line 244: `demo_config$n_reps_by_delta <- 50` → `demo_config$n_reps_by_delta <- .HETID_CONSTANTS$DEMO_N_REPS`
- Line 245: `demo_config$bootstrap_demo_size <- 3` → `demo_config$bootstrap_demo_size <- .HETID_CONSTANTS$DEMO_BOOTSTRAP_SIZE`

### Step 3.2: Update Utility Files

**File: `R/utils-hetid.R`**

**Changes**:
- Line 32-34: `c("stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata")` → `.hetid_strings()$stata$EXECUTABLES_ALL`
- Line 39-41: Mac paths → `.hetid_strings()$stata$MAC_PATHS[1:3]`
- Line 55-57: `c("stata", "stata-mp", "stata-se")` → `.hetid_strings()$stata$EXECUTABLES_UNIX`
- Line 72-75: Mac paths → `.hetid_strings()$stata$MAC_PATHS`
- Line 98: `tempfile(fileext = ".do")` → `tempfile(fileext = .hetid_strings()$stata$DO_EXTENSION)`
- Line 100: `"capture which ranktest"` → `paste("capture which", .hetid_strings()$stata$PACKAGES[1])`
- Line 105: `"capture which ivreg2"` → `paste("capture which", .hetid_strings()$stata$PACKAGES[2])`
- Line 110: `"capture which ivreg2h"` → `paste("capture which", .hetid_strings()$stata$PACKAGES[3])`
- Line 120: `tempfile(fileext = ".log")` → `tempfile(fileext = .hetid_strings()$stata$LOG_EXTENSION)`
- Line 128: `result == 0` → `result == .HETID_CONSTANTS$SUCCESS_EXIT_CODE`
- Line 140: `n = 1000, seed = 42` → `n = .HETID_CONSTANTS$DEFAULT_TEST_DATA_SIZE, seed = .HETID_CONSTANTS$DEFAULT_TEST_SEED`
- Line 152: `names(data) == "Y1"` → `names(data) == .hetid_strings()$columns$Y1`
- Line 153: `names(data) == "Y2"` → `names(data) == .hetid_strings()$columns$Y2`
- Line 154: `names(data) == "Xk"` → `names(data) == .hetid_strings()$columns$XK`
- Line 152: `"y"` → `.hetid_strings()$columns$Y_MAPPED`
- Line 153: `"P"` → `.hetid_strings()$columns$P_MAPPED`
- Line 154: `"X1"` → `.hetid_strings()$columns$X1_MAPPED`

**File: `R/messager.R`**

**Changes**:
- Line 16: `Sys.getenv("VERBOSE", unset = "TRUE") != "FALSE"` → `Sys.getenv(.hetid_strings()$env_vars$VERBOSE, unset = .hetid_strings()$env_vars$VERBOSE_TRUE) != .hetid_strings()$env_vars$VERBOSE_FALSE`

## Phase 4: Visualization Constants

### Step 4.1: Update Visualization Functions

**File: `R/visualization.R`**

**Changes**:
- Line 30-32: Replace string literals with constants:
  ```r
  Estimator = dplyr::recode(.data$Estimator,
    !!.hetid_strings()$plot_labels$OLS_COLUMN := .hetid_strings()$plot_labels$OLS_BIASED,
    !!.hetid_strings()$plot_labels$TSLS_COLUMN := .hetid_strings()$plot_labels$TSLS_LEWBEL
  )
  ```
- Line 46: `linewidth = 1` → `linewidth = .HETID_CONSTANTS$PLOT_LINE_WIDTH_THIN`
- Line 50: `vjust = 1.5, hjust = 1.1` → Keep as-is (positioning constants are context-specific)
- Line 60: `base_size = 14` → `base_size = .HETID_CONSTANTS$PLOT_BASE_FONT_SIZE`
- Line 62: Replace color mapping:
  ```r
  values = c(
    !!.hetid_strings()$plot_labels$OLS_BIASED := .hetid_strings()$plot_colors$OLS_COLOR,
    !!.hetid_strings()$plot_labels$TSLS_LEWBEL := .hetid_strings()$plot_colors$TSLS_COLOR
  )
  ```
- Line 106: `base_size = 14` → `base_size = .HETID_CONSTANTS$PLOT_BASE_FONT_SIZE`
- Line 148: `base_size = 14` → `base_size = .HETID_CONSTANTS$PLOT_BASE_FONT_SIZE`
- Line 182: `< 10` → `< .HETID_CONSTANTS$WEAK_INSTRUMENT_F_THRESHOLD`
- Line 195: `bins = 50` → `bins = .HETID_CONSTANTS$PLOT_HISTOGRAM_BINS`
- Line 196: `xintercept = 10` → `xintercept = .HETID_CONSTANTS$WEAK_INSTRUMENT_F_THRESHOLD`
- Line 198: `x = 10` → `x = .HETID_CONSTANTS$WEAK_INSTRUMENT_F_THRESHOLD`
- Line 209: `base_size = 14` → `base_size = .HETID_CONSTANTS$PLOT_BASE_FONT_SIZE`
- Line 239: `< 5` → `< .HETID_CONSTANTS$PLOT_MIN_BOOTSTRAP_THRESHOLD`
- Line 244: `n = 20` → `n = .HETID_CONSTANTS$PLOT_BOOTSTRAP_DISPLAY_LIMIT`
- Line 251: `1.96` → `.HETID_CONSTANTS$Z_CRITICAL_95`
- Line 252: `1.96` → `.HETID_CONSTANTS$Z_CRITICAL_95`
- Line 255: `linewidth = 3` → `linewidth = .HETID_CONSTANTS$PLOT_LINE_WIDTH_THICK`
- Line 262: `linewidth = 2` → `linewidth = .HETID_CONSTANTS$PLOT_LINE_WIDTH_NORMAL`
- Line 276: `base_size = 14` → `base_size = .HETID_CONSTANTS$PLOT_BASE_FONT_SIZE`
- Line 328: `< 10` → `< .HETID_CONSTANTS$WEAK_INSTRUMENT_F_THRESHOLD`

## Phase 5: Verification and Testing

### Step 5.1: Comprehensive Testing

**Run after each phase**:
```r
# 1. Test suite
devtools::test()

# 2. Check for NSE issues
checkglobals::checkglobals("R/", verbose = TRUE)

# 3. Full package check
devtools::check()

# 4. Documentation
devtools::document()

# 5. Build and install
devtools::install()

# 6. Performance check (if applicable)
system.time(run_main_simulation(create_default_config(num_simulations = 5)))
```

### Step 5.2: Enhanced Behavioral Verification

**Create verification script** `tests/testthat/test-refactoring-verification.R`:

```r
test_that("constants environment is locked and immutable", {
  # Test that constants environment exists and is locked
  expect_true(exists("constants_env"))
  expect_true(environmentIsLocked(constants_env))

  # Test that constants cannot be modified
  expect_error(
    constants_env$WEAK_INSTRUMENT_F_THRESHOLD <- 999,
    "cannot change value of locked binding"
  )

  # Test that constants have expected values
  expect_equal(constants_env$WEAK_INSTRUMENT_F_THRESHOLD, 10)
  expect_equal(constants_env$ALPHA_LEVEL, 0.05)
  expect_equal(constants_env$Z_CRITICAL_95, 1.96)
})

test_that("options system works correctly", {
  # Test default values from constants
  expect_equal(hetid_opt("display_digits"), 4)
  expect_equal(hetid_opt("alpha_level"), 0.05)

  # Test user override capability
  old_option <- getOption("hetid.display_digits")
  options(hetid.display_digits = 6)
  expect_equal(hetid_opt("display_digits"), 6)

  # Restore original option
  if (is.null(old_option)) {
    options(hetid.display_digits = NULL)
  } else {
    options(hetid.display_digits = old_option)
  }
})

test_that("string constants function works", {
  strings <- .hetid_strings()
  expect_equal(strings$df_adjust$ASYMPTOTIC, "asymptotic")
  expect_equal(strings$df_adjust$FINITE, "finite")
  expect_equal(strings$columns$STD_ERROR, "Std. Error")
})

test_that("refactoring preserves exact function behavior", {
  # Test that functions still work with same inputs/outputs
  config <- create_default_config()
  expect_equal(config$num_simulations, 200)
  expect_equal(config$main_sample_size, 500)
  expect_equal(config$alpha1, -0.5)

  # Test data generation reproducibility
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  set.seed(123)
  data1 <- generate_lewbel_data(100, params)

  set.seed(123)
  data2 <- generate_lewbel_data(100, params)

  expect_equal(data1, data2)
  expect_true(all(c("Y1", "Y2", "Xk", "Z") %in% names(data1)))
})

test_that("snapshot testing for critical functions", {
  skip_on_cran()

  # Test data generation snapshot
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  set.seed(42)
  data_snapshot <- generate_lewbel_data(10, params)
  expect_snapshot_value(data_snapshot, style = "serialize")

  # Test bounds calculation snapshot
  set.seed(42)
  bounds_snapshot <- calculate_lewbel_bounds(data_snapshot, tau = 0.2)
  expect_snapshot_value(bounds_snapshot, style = "serialize")
})

test_that("visualization functions work with constants", {
  # Create test data
  config <- create_default_config()
  test_results <- data.frame(
    ols_gamma1 = rnorm(50, -0.8, 0.1),
    tsls_gamma1 = rnorm(50, -0.8, 0.05),
    first_stage_F = runif(50, 5, 20)
  )

  # Test that plots can be created
  expect_s3_class(
    plot_estimator_distributions(test_results, config),
    "ggplot"
  )

  expect_s3_class(
    plot_first_stage_f_dist(test_results, config),
    "ggplot"
  )
})
```

### Step 5.3: Enhanced Performance Verification

**Create performance test** `tests/testthat/test-performance-unchanged.R`:

```r
test_that("refactoring does not impact performance", {
  skip_on_cran()

  config <- create_default_config(num_simulations = 10)
  seeds <- generate_all_seeds(config)

  # Baseline timing for comparison
  baseline_time <- 30  # seconds - adjust based on your system

  # Time a small simulation
  timing <- system.time({
    results <- run_main_simulation(config, seeds, verbose = FALSE)
  })

  # Should complete within 105% of baseline (5% tolerance)
  expect_lt(timing[["elapsed"]], baseline_time * 1.05)
  expect_equal(nrow(results), 10)

  # Test that constants access is fast
  constant_timing <- system.time({
    for (i in 1:1000) {
      val <- constants_env$WEAK_INSTRUMENT_F_THRESHOLD
    }
  })
  expect_lt(constant_timing[["elapsed"]], 0.1)  # Should be very fast

  # Test that options access is reasonable
  options_timing <- system.time({
    for (i in 1:1000) {
      val <- hetid_opt("display_digits")
    }
  })
  expect_lt(options_timing[["elapsed"]], 1.0)  # Should be reasonably fast
})

test_that("memory usage is reasonable", {
  skip_on_cran()

  # Test that constants don't consume excessive memory
  const_size <- object.size(constants_env)
  expect_lt(as.numeric(const_size), 10000)  # Less than 10KB

  # Test that string constants function is lightweight
  strings_size <- object.size(.hetid_strings())
  expect_lt(as.numeric(strings_size), 5000)  # Less than 5KB
})
```

## Phase 6: Documentation and Cleanup

### Step 6.1: Update Documentation

**Update DESCRIPTION**:
- Add `rlang` and `options` to Imports
- Update version number
- Add note about constants refactoring in NEWS.md

**Update NEWS.md**:
```markdown
# hetid 0.X.X

## Major Changes

* Refactored all magic numbers and strings to use locked constants environment
* Replaced globalVariables() with .data pronoun for NSE variables
* Added user-configurable options system with hetid_opt()
* Improved code maintainability and reduced magic number usage

## New Features

* Users can now override package defaults using options (e.g., options(hetid.display_digits = 6))
* Immutable constants environment prevents accidental modification
* Enhanced test coverage with snapshot testing and performance verification

## Internal Changes

* Added locked constants_env for numeric constants
* Added .hetid_strings() for string constants
* Added hetid_opt() for user-configurable options
* Updated all visualization functions to use consistent constants
* Enhanced test coverage for constant usage and immutability
```

### Step 6.2: Final Verification

**Run complete verification suite**:
```r
# 1. Clean build
devtools::clean_dll()
devtools::document()

# 2. Full check
devtools::check()

# 3. Test coverage
covr::package_coverage()

# 4. Check for remaining magic numbers
# Manual review of files for any missed constants

# 5. Pre-commit hooks
# Ensure all hooks pass

# 6. Build and install
devtools::build()
devtools::install()
```

### Step 6.3: Create Validation Report

**Generate report** `validation-report.md`:
```markdown
# Refactoring Validation Report

## Summary
- ✅ All tests pass
- ✅ devtools::check() passes with no errors/warnings
- ✅ No globalVariables() usage
- ✅ All magic numbers replaced with named constants
- ✅ Performance unchanged
- ✅ Behavior exactly preserved

## Constants Created
- Statistical thresholds: X constants
- Numerical tolerances: X constants
- Display formatting: X constants
- String literals: X constants
- Visualization: X constants

## Files Modified
- R/analysis.R: X changes
- R/data-generation.R: X changes
- R/estimation.R: X changes
- [etc.]

## Verification Methods
1. Unit tests for all constant values
2. Behavioral tests for function outputs
3. Performance benchmarks
4. Visual inspection of plots
5. Documentation review
```

## Success Criteria

The refactoring is complete and successful when:

1. ✅ **All tests pass**: `devtools::test()` returns no failures
2. ✅ **R CMD check passes**: `devtools::check()` with no errors/warnings/notes
3. ✅ **No globalVariables**: `checkglobals::checkglobals()` reports no issues
4. ✅ **No magic numbers**: Manual review finds no hardcoded constants
5. ✅ **Behavior preserved**: All function outputs identical to baseline (verified by snapshot tests)
6. ✅ **Performance maintained**: No significant performance degradation (≤5% tolerance)
7. ✅ **Constants immutable**: Environment is locked and cannot be modified
8. ✅ **Options system works**: Users can override defaults via options()
9. ✅ **Documentation updated**: All changes documented in NEWS.md

## Rollback Plan

If any step fails:
1. **Git reset**: Return to previous working state
2. **Incremental approach**: Apply changes in smaller batches
3. **Debug**: Use `browser()` and `debug()` to identify issues
4. **Test isolation**: Run individual test files to isolate problems

## Timeline Estimate

- **Phase 1 (NSE)**: 2-3 hours
- **Phase 2 (Constants & Options)**: 5-7 hours
- **Phase 3 (Simulation)**: 2-3 hours
- **Phase 4 (Visualization)**: 2-3 hours
- **Phase 5 (Enhanced Verification)**: 3-4 hours
- **Phase 6 (Documentation)**: 1-2 hours

**Total**: 15-22 hours for a junior developer

This plan ensures that the refactoring is systematic, verifiable, and maintains exact behavioral compatibility while implementing all 2025 best practices for magic variables in R packages.
