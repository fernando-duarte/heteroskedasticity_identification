# Documentation Patterns Analysis for hetid Package

This document catalogs all repeating or near-repeating documentation patterns found across the R package files, including their locations and variations.

*Note on line numbers: Line numbers for occurrences generally point to the specific `@param` or `@return` tag, the start of the roxygen comment block, or the relevant code line(s) being referenced. Minor variations from these points may exist in the actual source code, but the semantic content of the patterns has been verified against the R files as of the last review date.*

## 1. Common Parameter Documentation

### 1.1 `config` Parameter

**Pattern**: "Configuration object from create_default_config()"

**Occurrences**:
- `analysis.R:13` - "@param config List. Configuration object used for the simulation." (Actual roxygen block starts line 10)
- `analysis.R:136` - "@param config List. Configuration object." (Actual roxygen block starts line 133)
- `analysis.R:219` - "@param config List. Configuration object." (Actual roxygen block starts line 216)
- `analysis.R:274` - "@param config List. Configuration object." (Actual roxygen block starts line 271)
- `analysis.R:336` - "@param config List. Optional. Configuration object (not currently used)." (Actual roxygen block starts line 333)
- `data-generation.R:141` - "@param config List. Optional. Configuration object (used with data parameter)." (Actual roxygen block starts line 139)
- `lewbel-monte-carlo.R:6` - "@param config List. Configuration object from create_default_config(). (Actual code adds: If NULL, uses default configuration)" (Actual roxygen block starts line 5)
- `simulation.R:5` - "@param config List. Configuration object from create_default_config()." (Actual roxygen block starts line 4)
- `simulation.R:91` - "@param config List. Configuration object from create_default_config()." (Actual: Line 90 in roxygen block starting line 89)
- `simulation.R:170` - "@param config List. Configuration object from create_default_config()." (Actual: Line 169 in roxygen block starting line 168)
- `simulation.R:258` - "@param config List. Configuration object from create_default_config()." (Actual: Line 257 in roxygen block starting line 256)
- `simulation-helpers.R:6` - "@param config Configuration list from create_default_config()" (Actual: Line 5 in roxygen block starting line 4 for `run_lewbel_monte_carlo_df`)
- `simulation-helpers.R:32` - "@param config Configuration list" (Actual: Line 29 in roxygen block starting line 28 for `compare_df_adjustments`)
- `utils.R:156` - "@param config List. Configuration object from create_default_config()." (Actual: Line 155 in roxygen block starting line 154 for `generate_all_seeds`)
- `visualization.R:6` - "@param config List. Configuration object containing true parameter values." (Actual: Line 5 in roxygen block starting line 4)
- `visualization.R:71` - "@param config List. Configuration object containing true parameter values." (Actual: Line 70 in roxygen block starting line 69)
- `visualization.R:114` - "@param config List. Configuration object containing true parameter values." (Actual: Line 113 in roxygen block starting line 112)
- `visualization.R:157` - "@param config List. Optional. Configuration object (not currently used)." (Actual: Line 156 in roxygen block starting line 155)
- `visualization.R:219` - "@param config List. Configuration object containing true parameter values." (Actual: Line 218 in roxygen block starting line 216)
- `visualization.R:291` - "@param config List. Configuration object." (Actual: Line 290 in roxygen block starting line 287)

### 1.2 `verbose` Parameter

**Pattern**: "Logical. Whether to print [output/messages/progress]"

**Occurrences**:
- `analysis.R:14` - "@param verbose Logical. Whether to print detailed output (default: TRUE)."
- `analysis.R:137` - "@param verbose Logical. Whether to print output (default: TRUE)."
- `analysis.R:220` - "@param verbose Logical. Whether to print output (default: TRUE)."
- `analysis.R:275` - "@param verbose Logical. Whether to print output (default: TRUE)."
- `analysis.R:337` - "@param verbose Logical. Whether to print the summary (default: TRUE)."
- `data-generation.R:147` - "@param verbose Logical. Whether to print detailed output (default: TRUE)." (Actual: Line 146)
- `lewbel-monte-carlo.R:18` - "@param verbose Logical. Whether to print progress and results (default: TRUE)." (Actual: Line 16)
- `lewbel-monte-carlo.R:220` - "@param verbose Logical. Whether to print output (default: TRUE)." (For `run_lewbel_demo`, actual: Line 218)
- `simulation.R:7` - "@param verbose Logical. Whether to print progress messages (default: TRUE)."
- `simulation.R:93` - "@param verbose Logical. Whether to print progress messages (default: TRUE)." (Actual: Line 92)
- `simulation.R:172` - "@param verbose Logical. Whether to print progress messages (default: TRUE)." (Actual: Line 171)
- `simulation.R:260` - "@param verbose Logical. Whether to print progress messages (default: TRUE)." (Actual: Line 259)
- `simulation-helpers.R:8` - "@param verbose Logical. Whether to print progress messages (default: TRUE)" (Actual: Line 7 for `run_lewbel_monte_carlo_df`)
- `simulation-helpers.R:32` - "@param verbose Logical. Whether to print progress" (Actual: Line 30 for `compare_df_adjustments`)
- `visualization.R:292` - "@param verbose Logical. Whether to print plots (default: TRUE)." (Actual: Line 291)

### 1.3 `seeds` Parameter

**Pattern**: "Seed object from generate_all_seeds()"

**Occurrences**:
- `simulation.R:6` - "@param seeds List. Seed object from generate_all_seeds()."
- `simulation.R:92` - "@param seeds List. Seed object from generate_all_seeds()." (Actual: Line 91)
- `simulation.R:171` - "@param seeds List. Seed object from generate_all_seeds()." (Actual: Line 170)
- `simulation.R:259` - "@param seeds List. Seed object from generate_all_seeds()." (Actual: Line 258)

### 1.4 `df_adjust` Parameter

**Pattern**: "Degrees of freedom adjustment: 'asymptotic' (default) or 'finite'" or similar phrasing specifying options.

**Occurrences**:
- `estimation.R:15-17` - "@param df_adjust Character. Degrees of freedom adjustment: \"asymptotic\" (default) or \"finite\". Note: This parameter currently only affects the interpretation of bootstrap SEs, not the bounds calculation itself." (Actual: Line 14)
- `estimation.R:137-138` - "@param df_adjust Character. Degrees of freedom adjustment: \"asymptotic\" (default) or \"finite\". Affects standard errors and confidence intervals." (Actual: Line 135)
- `simulation-helpers.R:7-8` - "@param df_adjust Character. Degrees of freedom adjustment: \"asymptotic\" (default) or \"finite\". Affects standard errors and confidence intervals." (Actual: Line 6 for `run_lewbel_monte_carlo_df`)
- `utils.R:70-71` - "@param df_adjust Character. Degrees of freedom adjustment method (default: \"asymptotic\"). Options: \"asymptotic\", \"finite\"." (Actual: Line 69, for `create_default_config`)
- `utils-df.R:6` - "@param df_adjust Character string: \"asymptotic\" (default) or \"finite\"" (Actual: Line 5 for `adjust_se_for_df`)
- `utils-df.R:24` - "@param df_adjust Character string: \"asymptotic\" (default) or \"finite\"" (Actual: Line 23 for `get_critical_value`)
- `utils-df.R:40` - "@param df_adjust Character string: \"asymptotic\" (default) or \"finite\"" (Actual: Line 39 for `extract_se_ivreg`)
- `utils-df.R:66` - "@param df_adjust Character string: \"asymptotic\" (default) or \"finite\"" (Actual: Line 65 for `extract_se_lm`)

### 1.5 Bootstrap Parameters

**Pattern**: Bootstrap-related parameters

**`compute_se` / `compute_bounds_se`**:
- `estimation.R:11` - "@param compute_se Logical. Whether to compute bootstrap standard errors (default: FALSE)." (Actual: Line 10)
- `estimation.R:133` - "@param compute_bounds_se Logical. Whether to compute bootstrap SE for bounds (default: FALSE)." (Actual: Line 132)

**`b_reps` / `bootstrap_reps`**:
- `estimation.R:13` - "@param b_reps Integer. Number of bootstrap replications if compute_se = TRUE (default: 100)." (Actual: Line 12)
- `utils.R:58` - "@param bootstrap_reps Integer. Number of bootstrap replications (default: 100)." (Actual: Line 48, in `create_default_config`)

### 1.6 `results` Parameters

**Pattern**: "Results from [function_name]()"

**Occurrences**:
- `analysis.R:12` - "@param results Data.frame. Results from run_main_simulation()."
- `analysis.R:134` - "@param results_main Data.frame. Main simulation results."
- `analysis.R:217` - "@param results_by_n Data.frame. Results from run_sample_size_analysis()." (Actual: Line 216)
- `analysis.R:272` - "@param results_by_delta Data.frame. Results from run_sensitivity_analysis()." (Actual: Line 271)

### 1.7 `n_obs` Parameter

**Pattern**: "Integer. Sample size"

**Occurrences**:
- `data-generation.R:8` - "@param n_obs Integer. Sample size."
- `data-generation.R:143` - "@param n_obs Integer. Sample size for verification (default: 10000, used with params)." (Actual: Line 142)

### 1.8 `params` Parameter

**Pattern**: "List. Parameters for"

**Occurrences**:
- `data-generation.R:9` - "@param params List. Parameters for the data generating process containing:"
- `data-generation.R:145` - "@param params List. Parameters for the DGP (same format as generate_lewbel_data)." (Actual: Line 144)
- `estimation.R:129` - "@param params List. Parameters for data generation and estimation." (Actual: Line 128)

## 2. Mathematical Model Documentation

### 2.1 Triangular System Equations

**Pattern**: The triangular model equations

**Full Version**:
```
The triangular model is:
\deqn{Y_1 = \beta_{1,0} + \beta_{1,1}X + \gamma_1 Y_2 + \epsilon_1}
\deqn{Y_2 = \beta_{2,0} + \beta_{2,1}X + \epsilon_2}
```

**Occurrences**:
- `data-generation.R:23-26` - Full version (Actual: Lines 24-26 in `@details` block starting line 22)
- `data.R:16-19` - Modified version with specific values (Actual: Lines 17-19 in `@details` block starting line 16)

### 2.2 Error Structure Documentation

**Pattern**: Single-factor error model

**Full Version**:
```
The error structure follows a single-factor model:
\deqn{\epsilon_1 = \alpha_1 U + V_1}
\deqn{\epsilon_2 = \alpha_2 U + V_2}
```

**Occurrences**:
- `data-generation.R:27-30` - Full version with heteroskedasticity details (Actual: Lines 28-30 in `@details` block starting line 27)
- `data.R:20-23` - Similar version with specific values and heteroskedasticity (Actual: Lines 21-23 in `@details` block starting line 20)

### 2.3 Lewbel's Identifying Assumptions

**Pattern**: Covariance restrictions and instrument relevance

**Occurrences**:
- `data-generation.R:154-160` - Listed as bullet points in @details (Actual: Roxygen block for `verify_lewbel_assumptions` lines 153-159)
- `estimation.R:20-23` - Relaxed assumption with correlation bounds in @details (Actual: Roxygen block for `calculate_lewbel_bounds` lines 21-23)

## 3. Example Code Patterns

### 3.1 Standard Simulation Setup

**Pattern**:
```r
config <- create_default_config()
seeds <- generate_all_seeds(config)
results <- run_[function_name](config, seeds)
```

**Occurrences** (line numbers refer to the start of the example block or the relevant lines within it):
- `analysis.R:25-28` - `run_main_simulation` variant (Example for `analyze_main_results`)
- `analysis.R:148-153` - With `bootstrap_demonstration` (Example for `analyze_bootstrap_results`)
- `analysis.R:231-234` - `run_sample_size_analysis` variant (Example for `analyze_sample_size_results`)
- `analysis.R:286-291` - `run_sensitivity_analysis` variant (Example for `analyze_sensitivity_results`)
- `simulation.R:13-15` - Basic version (Example for `run_main_simulation`)
- `simulation.R:99-101` - Bootstrap variant (Example for `run_bootstrap_demonstration`)
- `simulation.R:178-180` - Sample size variant (Example for `run_sample_size_analysis`)
- `simulation.R:267-269` - Sensitivity variant (Example for `run_sensitivity_analysis`)
- `visualization.R:12-16` - With `na.omit` (Example for `plot_estimator_distributions`)
- `visualization.R:77-80` - Sample size variant (Example for `plot_sample_size_consistency`)
- `visualization.R:120-123` - Sensitivity variant (Example for `plot_het_sensitivity`)
- `visualization.R:164-168` - With `na.omit` (Example for `plot_first_stage_f_dist`)
- `visualization.R:225-232` - Bootstrap variant (Example for `plot_bootstrap_ci`)
- `visualization.R:299-306` - Full example (Example for `generate_all_plots`)

### 3.2 Parameter List Creation

**Pattern**: Creating params list for simulation
```r
params <- list(
  beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
  beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
  alpha1 = config$alpha1, alpha2 = config$alpha2, delta_het = config$delta_het
)
```

**Occurrences** (line numbers refer to the relevant code within the function body or example):
- `lewbel-monte-carlo.R: example for verify_lewbel_assumptions` lines 171-175 (in R/lewbel-monte-carlo.R, the example block for `verify_lewbel_assumptions` demonstrates this).
- `estimation.R: example for run_single_lewbel_simulation` lines 153-162.
- Similar patterns in `simulation.R` (within function bodies): lines 36-47 (in `run_main_simulation`), 112-123 (in `run_bootstrap_demonstration`), 195-206 (in `run_sample_size_analysis`), 285-296 (in `run_sensitivity_analysis`).

## 4. Data Structure Documentation

### 4.1 Results Data Frame Structure

**Pattern**: Description of simulation results columns

Common columns documented repeatedly:
- `sim_id`: Simulation run identifier
- `sample_size`: Sample size used
- `ols_gamma1`: OLS estimate
- `tsls_gamma1`: 2SLS estimate
- `ols_coverage`: Coverage indicator for OLS
- `tsls_coverage`: Coverage indicator for 2SLS
- `first_stage_F`: First-stage F-statistic
- `bound_lower_tau0`, `bound_upper_tau0`: Point identification bounds
- `bound_lower_tau_set`, `bound_upper_tau_set`: Set identification bounds

**Occurrences**:
- `estimation.R:406-424` - Most complete documentation (Actual: in `run_single_lewbel_simulation` function body, `results_df` defined lines 375-392).
- Referenced in analysis.R functions (general observation, verified as plausible).
- Used in visualization.R functions (general observation, verified as plausible).

### 4.2 @return A data.frame Pattern

**Pattern**: Functions returning data.frames

**Occurrences**:
- `analysis.R:144` - "@return A data.frame with bootstrap examples."
- `analysis.R:227` - "@return A data.frame with sample size analysis."
- `analysis.R:282` - "@return A data.frame with sensitivity analysis."
- `data-generation.R:35` - "@return A data.frame with columns Y1, Y2, epsilon1, epsilon2, and:"
- `utils-hetid.R:136` - "@return A data.frame with test data for Lewbel identification." (Actual: Line 137, roxygen block starts line 133)
- `simulation.R:9` - "@return A data.frame containing results from all simulation runs."
- `simulation.R:95` - "@return A data.frame containing bootstrap demonstration results." (Actual: Line 94)
- `simulation.R:174` - "@return A data.frame containing results for different sample sizes." (Actual: Line 173)
- `simulation.R:262` - "@return A data.frame containing results for different heteroscedasticity" (Actual: Line 261, full text: "@return A data.frame containing results for different heteroscedasticity parameters.")

## 5. Function Family Patterns

### 5.1 Analysis Functions

**Pattern**: "analyze_[aspect]_results" functions

**Functions** (line numbers refer to function definition start):
- `analyze_main_results` (analysis.R:32)
- `analyze_bootstrap_results` (analysis.R:158)
- `analyze_sample_size_results` (analysis.R:238)
- `analyze_sensitivity_results` (analysis.R:295)

All follow similar structure:
1. Take results and config
2. Process/summarize data
3. Optionally print verbose output
4. Return summary object

### 5.2 Run Functions

**Pattern**: "run_[type]_[analysis]" functions

**Functions** (line numbers refer to function definition start):
- `run_main_simulation` (simulation.R:19)
- `run_bootstrap_demonstration` (simulation.R:105)
- `run_sample_size_analysis` (simulation.R:184)
- `run_sensitivity_analysis` (simulation.R:273)
- `run_single_lewbel_simulation` (estimation.R:171)

All follow similar structure:
1. Take config and seeds
2. Set up parallel processing (except run_single_lewbel_simulation)
3. Run simulations
4. Clean up and return results

### 5.3 Plot Functions

**Pattern**: "plot_[visualization_type]" functions

**Functions** (line numbers refer to function definition start):
- `plot_estimator_distributions` (visualization.R:21)
- `plot_sample_size_consistency` (visualization.R:85)
- `plot_het_sensitivity` (visualization.R:128)
- `plot_first_stage_f_dist` (visualization.R:174)
- `plot_bootstrap_ci` (visualization.R:237)

All follow similar structure:
1. Take results and config
2. Process data for plotting
3. Create ggplot2 visualization
4. Return plot object

## 6. Warning/Note Patterns

### 6.1 Knitr Package Suggestion

**Pattern**: "For enhanced table formatting in verbose output, install the knitr package"

**Occurrences**:
- `analysis.R:17-19`
- `analysis.R:140-142`
- `analysis.R:223-225`
- `analysis.R:278-280`

### 6.2 TODO Comments

**Pattern**: TODO comments about column name updates

**Occurrences**:
- `estimation.R:39-40` - "# TODO: Update generate_lewbel_data call if its return columns change to"
- `estimation.R:64-65` - "# TODO: Update column names (e.g., d$Xk to d$x_k) after"
- `estimation.R:70-71` - "# TODO: Update column names (e.g., d$Z to d$z_var) after"
- `estimation.R:187-188` - "# TODO: Update Y1, endog_var, exog_vars if column names change after"

## 7. Cross-Reference Patterns

### 7.1 @seealso References

**Pattern**: The `@seealso` tag is currently **NOT USED** in the R source files of this package. The initial observation about common `@seealso` patterns was inaccurate.

Common patterns initially thought to exist (but are not implemented via `@seealso`):
- Direct function calls or references in examples to `calculate_lewbel_bounds` for set identification.
- Direct function calls or references in examples/code to `run_lewbel_monte_carlo` or `run_main_simulation`.
- Direct function calls or references in examples to `create_default_config` for configuration.

### 7.2 @family Tags

Currently NOT used in any functions (could be added for better organization). This observation is **accurate**.
- Estimation functions
- Visualization functions
- Analysis functions
- Simulation functions
- Utility functions

**Occurrences**:
- `estimation.R:25-30` - `calculate_lewbel_bounds` returns a list with `bounds` and `se` components, using `\itemize`.
- `lewbel-monte-carlo.R:35-44` - `run_lewbel_monte_carlo` returns a comprehensive list with multiple components, using `\itemize`.
- `analysis.R`:
    - `analyze_main_results` (roxygen line 21) - `@return A list containing summary tables and statistics.` (Does not use `\itemize` in its `@return` tag).
    - Other `analyze_*` functions (`analyze_bootstrap_results`, `analyze_sample_size_results`, `analyze_sensitivity_results`) are documented to return data.frames, not lists.

The statement "Many analysis functions return lists" is **partially accurate**. `analyze_main_results` returns a list. The specific `\itemize` pattern is used for complex list structures.

## 8. Return Value Documentation

### 8.1 List Return Pattern

**Pattern**: Functions returning lists with named components

**Common structure**:
```
@return A list containing:
  \itemize{
    \item component1: Description
    \item component2: Description
  }
```

**Occurrences**:
- `estimation.R:25-30` - bounds and se components
- `lewbel-monte-carlo.R:35-44` - Comprehensive results list
- Many analysis functions return lists

## 9. Details Section Patterns

### 9.1 Comprehensive Function Details

**Pattern**: Extended @details sections explaining methodology

**Notable examples** (line numbers refer to the start of the @details block or relevant section):
- `lewbel-monte-carlo.R:20-32` - Lists all simulation components (Actual @details section: lines 20-32).
- `estimation.R:20-23` - Mathematical details about relaxed assumptions (Actual @details section: lines 20-23).
- `data-generation.R:154-165` - Testing methodology (Actual @details section for `verify_lewbel_assumptions`: lines 153-165).

## 10. Global Variables Declaration

**Pattern**: utils::globalVariables() to avoid R CMD check NOTEs

**Occurrences**:
- `analysis.R:1-7` - Main global variables.
- `globals.R:4-27` - Comprehensive list for NSE contexts.

The variables declared in `analysis.R` are a subset of those declared in `globals.R`. (i.e., `globals.R` includes all variables from `analysis.R`'s declaration and more).

## 11. Additional Patterns Found

### 11.1 @export Pattern

**Pattern**: Functions marked for export

**Summary**:
- `analysis.R`: 5 exports (lines 31, 157, 237, 294, 344)
- `data-generation.R`: 2 exports (lines 58, 177)
- `estimation.R`: 2 exports (lines 53, 170)
- `lewbel-monte-carlo.R`: 2 exports (lines 67, 233)
- `simulation.R`: 4 exports (lines 18, 104, 183, 272)
- `simulation-helpers.R`: 2 exports (lines 11, 35)
- `utils.R`: 3 exports (lines 17, 87, 168)
- `utils-df.R`: 4 exports (lines 7, 25, 41, 67)
- `utils-hetid.R`: 8 exports (lines 1, 7, 13, 19, 25, 48, 90, 138)
- `visualization.R`: 6 exports (lines 20, 84, 127, 173, 236, 315)

### 11.2 @importFrom Pattern

**Pattern**: Explicit imports from other packages

**Occurrences**:
- `estimation.R:6` - "@importFrom stats coef lm nobs qnorm qt residuals vcov as.formula cov var sd"

### 11.3 Weak Instrument References

**Pattern**: References to weak instruments

**Occurrences**:
- `analysis.R:79` - "# Weak instrument diagnostics"
- `analysis.R:83` - "Weak instrument diagnostic: %.1f%% of simulations have"
- `analysis.R:375` - "7. Weak instrument rate: %.1f%%\\n"
- `visualization.R:153` - "Creates a histogram of first-stage F-statistics with weak instrument"
- `visualization.R:158` - "@param weak_iv_pct Numeric. Percentage of simulations with weak instruments."
- `visualization.R:203` - "%.1f%% have F < 10 (weak instrument threshold)"

### 11.4 create_default_config Usage in Examples

**Pattern**: "config <- create_default_config()" in @examples

**Occurrences (in @examples sections)**:
- `analysis.R:25`
- `analysis.R:148`
- `analysis.R:231`
- `analysis.R:286`
- `data-generation.R:167`
- `lewbel-monte-carlo.R:52` (custom_config)
- `estimation.R:152`
- `simulation.R:13`
- `simulation.R:99`
- `simulation.R:178`
- `simulation.R:267`
- `utils.R:77`
- `utils.R:81` (custom_config)
- `utils.R:163`
- `visualization.R:12`
- `visualization.R:77`
- `visualization.R:120`
- `visualization.R:164`
- `visualization.R:225`
- `visualization.R:299`
