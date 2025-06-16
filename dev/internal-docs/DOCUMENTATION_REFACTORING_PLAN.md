# Documentation Refactoring Plan for hetid Package

## OBJECTIVE
Eliminate all documentation duplicates identified in `documentation-patterns.md` by creating single sources of truth using roxygen2 features, while ensuring the final rendered documentation remains identical.

Note: Some documentation may have changed slightly since this plan was created.

## 1. PREPARATION STEPS

### 1.1 Create Backup
```bash
# Create backup of current state
cp -r R/ R_backup_$(date +%Y%m%d_%H%M%S)/
cp -r man/ man_backup_$(date +%Y%m%d_%H%M%S)/
cp -r vignettes/ vignettes_backup_$(date +%Y%m%d_%H%M%S)/ 2>/dev/null || true
```

### 1.2 Generate Baseline Documentation Snapshot
```bash
# Generate checksums for current .Rd files
find man -name "*.Rd" -type f -exec md5sum {} \; | sort > man_checksums_before.txt

# Capture current documentation output
R -e "devtools::document()" > doc_output_before.txt 2>&1
```

### 1.3 Create Template Directory Structure
```bash
# Create roxygen templates directory
mkdir -p inst/roxygen/templates
```

## 2. TEMPLATE CREATION

### 2.1 Parameter Templates

#### Template: param-config.R
```
File: inst/roxygen/templates/param-config.R
Content:
#' @param config List. Configuration object from create_default_config().
```

#### Template: param-config-optional.R
```
File: inst/roxygen/templates/param-config-optional.R
Content:
#' @param config List. Optional. Configuration object (not currently used).
```

#### Template: param-config-viz.R
```
File: inst/roxygen/templates/param-config-viz.R
Content:
#' @param config List. Configuration object containing true parameter values.
```

#### Template: param-verbose.R
```
File: inst/roxygen/templates/param-verbose.R
Content:
#' @param verbose Logical. Whether to print output (default: TRUE).
```

#### Template: param-verbose-progress.R
```
File: inst/roxygen/templates/param-verbose-progress.R
Content:
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
```

#### Template: param-seeds.R
```
File: inst/roxygen/templates/param-seeds.R
Content:
#' @param seeds List. Seed object from generate_all_seeds().
```

#### Template: param-df-adjust.R
```
File: inst/roxygen/templates/param-df-adjust.R
Content:
#' @param df_adjust Character. Degrees of freedom adjustment: "asymptotic" (default) or
#'   "finite". Affects standard errors and confidence intervals.
```

#### Template: param-n-obs.R
```
File: inst/roxygen/templates/param-n-obs.R
Content:
#' @param n_obs Integer. Sample size.
```

#### Template: param-compute-se.R
```
File: inst/roxygen/templates/param-compute-se.R
Content:
#' @param compute_se Logical. Whether to compute bootstrap standard errors (default: FALSE).
```

#### Template: param-b-reps.R
```
File: inst/roxygen/templates/param-b-reps.R
Content:
#' @param b_reps Integer. Number of bootstrap replications if compute_se = TRUE (default: 100).
```

### 2.2 Details Templates

#### Template: details-triangular-model.R
```
File: inst/roxygen/templates/details-triangular-model.R
Content:
#' @details
#' The triangular model is:
#' \deqn{Y_1 = \beta_{1,0} + \beta_{1,1}X + \gamma_1 Y_2 + \epsilon_1}
#' \deqn{Y_2 = \beta_{2,0} + \beta_{2,1}X + \epsilon_2}
```

#### Template: details-error-structure.R
```
File: inst/roxygen/templates/details-error-structure.R
Content:
#' The error structure follows a single-factor model:
#' \deqn{\epsilon_1 = \alpha_1 U + V_1}
#' \deqn{\epsilon_2 = \alpha_2 U + V_2}
```

### 2.3 Note Templates

#### Template: note-knitr.R
```
File: inst/roxygen/templates/note-knitr.R
Content:
#' @note
#' For enhanced table formatting in verbose output, install the knitr package.
```

### 2.4 Example Templates

#### Template: example-simulation-basic.R
```
File: inst/roxygen/templates/example-simulation-basic.R
Content:
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
```

## 3. REFACTORING ACTIONS

### 3.1 Config Parameter Replacements

Step 1:
```
File: R/analysis.R
Lines: 13
Replace: #' @param config List. Configuration object used for the simulation.
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 2:
```
File: R/analysis.R
Lines: 136-137
Replace: #' @param config List. Configuration object.
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 3:
```
File: R/analysis.R
Lines: 219
Replace: #' @param config List. Configuration object.
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 4:
```
File: R/analysis.R
Lines: 274
Replace: #' @param config List. Configuration object.
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 5:
```
File: R/analysis.R
Lines: 336
Replace: #' @param config List. Optional. Configuration object (not currently used).
With: #' @template param-config-optional
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 6:
```
File: R/simulation.R
Lines: 5
Replace: #' @param config List. Configuration object from create_default_config().
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 7:
```
File: R/simulation.R
Lines: 91
Replace: #' @param config List. Configuration object from create_default_config().
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 8:
```
File: R/simulation.R
Lines: 170
Replace: #' @param config List. Configuration object from create_default_config().
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 9:
```
File: R/simulation.R
Lines: 258
Replace: #' @param config List. Configuration object from create_default_config().
With: #' @template param-config
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 10:
```
File: R/visualization.R
Lines: 6
Replace: #' @param config List. Configuration object containing true parameter values.
With: #' @template param-config-viz
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 11:
```
File: R/visualization.R
Lines: 71
Replace: #' @param config List. Configuration object containing true parameter values.
With: #' @template param-config-viz
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 12:
```
File: R/visualization.R
Lines: 114
Replace: #' @param config List. Configuration object containing true parameter values.
With: #' @template param-config-viz
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 13:
```
File: R/visualization.R
Lines: 157
Replace: #' @param config List. Optional. Configuration object (not currently used).
With: #' @template param-config-optional
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 14:
```
File: R/visualization.R
Lines: 219
Replace: #' @param config List. Configuration object containing true parameter values.
With: #' @template param-config-viz
Verify: R -e "roxygen2::roxygenize('.')"
```

### 3.2 Verbose Parameter Replacements

Step 15:
```
File: R/analysis.R
Lines: 14
Replace: #' @param verbose Logical. Whether to print detailed output (default: TRUE).
With: #' @template param-verbose
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 16:
```
File: R/analysis.R
Lines: 137
Replace: #' @param verbose Logical. Whether to print output (default: TRUE).
With: #' @template param-verbose
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 17:
```
File: R/analysis.R
Lines: 220
Replace: #' @param verbose Logical. Whether to print output (default: TRUE).
With: #' @template param-verbose
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 18:
```
File: R/analysis.R
Lines: 275
Replace: #' @param verbose Logical. Whether to print output (default: TRUE).
With: #' @template param-verbose
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 19:
```
File: R/simulation.R
Lines: 7
Replace: #' @param verbose Logical. Whether to print progress messages (default: TRUE).
With: #' @template param-verbose-progress
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 20:
```
File: R/simulation.R
Lines: 93
Replace: #' @param verbose Logical. Whether to print progress messages (default: TRUE).
With: #' @template param-verbose-progress
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 21:
```
File: R/simulation.R
Lines: 172
Replace: #' @param verbose Logical. Whether to print progress messages (default: TRUE).
With: #' @template param-verbose-progress
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 22:
```
File: R/simulation.R
Lines: 260
Replace: #' @param verbose Logical. Whether to print progress messages (default: TRUE).
With: #' @template param-verbose-progress
Verify: R -e "roxygen2::roxygenize('.')"
```

### 3.3 Seeds Parameter Replacements

Step 23:
```
File: R/simulation.R
Lines: 6
Replace: #' @param seeds List. Seed object from generate_all_seeds().
With: #' @template param-seeds
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 24:
```
File: R/simulation.R
Lines: 92
Replace: #' @param seeds List. Seed object from generate_all_seeds().
With: #' @template param-seeds
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 25:
```
File: R/simulation.R
Lines: 171
Replace: #' @param seeds List. Seed object from generate_all_seeds().
With: #' @template param-seeds
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 26:
```
File: R/simulation.R
Lines: 259
Replace: #' @param seeds List. Seed object from generate_all_seeds().
With: #' @template param-seeds
Verify: R -e "roxygen2::roxygenize('.')"
```

### 3.4 df_adjust Parameter Replacements

Step 27:
```
File: R/estimation.R
Lines: 137-138
Replace: #' @param df_adjust Character. Degrees of freedom adjustment: "asymptotic" (default) or
#'   "finite". Affects standard errors and confidence intervals.
With: #' @template param-df-adjust
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 28:
```
File: R/simulation-helpers.R
Lines: 7-8
Replace: #' @param df_adjust Character. Degrees of freedom adjustment: "asymptotic" (default) or
#'   "finite". Affects standard errors and confidence intervals.
With: #' @template param-df-adjust
Verify: R -e "roxygen2::roxygenize('.')"
```

### 3.5 Mathematical Model Details Replacements

Step 29:
```
File: R/data-generation.R
Lines: 23-26
Replace: #' The triangular model is:
#' \deqn{Y_1 = \beta_{1,0} + \beta_{1,1}X + \gamma_1 Y_2 + \epsilon_1}
#' \deqn{Y_2 = \beta_{2,0} + \beta_{2,1}X + \epsilon_2}
With: #' @template details-triangular-model
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 30:
```
File: R/data-generation.R
Lines: 27-30
Replace: #' The error structure follows a single-factor model:
#' \deqn{\epsilon_1 = \alpha_1 U + V_1}
#' \deqn{\epsilon_2 = \alpha_2 U + V_2}
With: #' @template details-error-structure
Verify: R -e "roxygen2::roxygenize('.')"
```

### 3.6 Note Replacements

Step 31:
```
File: R/analysis.R
Lines: 17-19
Replace: #' @note
#' For enhanced table formatting in verbose output, install the knitr package.
With: #' @template note-knitr
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 32:
```
File: R/analysis.R
Lines: 140-142
Replace: #' @note
#' For enhanced table formatting in verbose output, install the knitr package.
With: #' @template note-knitr
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 33:
```
File: R/analysis.R
Lines: 223-225
Replace: #' @note
#' For enhanced table formatting in verbose output, install the knitr package.
With: #' @template note-knitr
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 34:
```
File: R/analysis.R
Lines: 278-280
Replace: #' @note
#' For enhanced table formatting in verbose output, install the knitr package.
With: #' @template note-knitr
Verify: R -e "roxygen2::roxygenize('.')"
```

### 3.7 Using @inheritParams for Common Parameter Sets

Step 35:
```
File: R/utils.R
Lines: 155
Replace: #' @param config List. Configuration object from create_default_config().
With: #' @inheritParams run_main_simulation
Verify: R -e "roxygen2::roxygenize('.')"
```

Step 36:
```
File: R/data-generation.R
Lines: 143
Replace: #' @param n_obs Integer. Sample size for verification (default: 10000, used with params).
With: #' @inheritParams generate_lewbel_data
Verify: R -e "roxygen2::roxygenize('.')"
```

## 4. VALIDATION PROTOCOL

### 4.1 Regenerate Documentation
```bash
# Regenerate all documentation
R -e "devtools::document(roclets = c('rd', 'collate', 'namespace'))"
```

### 4.2 Compare Checksums
```bash
# Generate new checksums
find man -name "*.Rd" -type f -exec md5sum {} \; | sort > man_checksums_after.txt

# Compare checksums
diff man_checksums_before.txt man_checksums_after.txt

# If differences exist, examine specific files
for file in $(diff man_checksums_before.txt man_checksums_after.txt | grep ">" | awk '{print $3}'); do
    echo "=== Changes in $file ==="
    diff -u "man_backup_*/$(basename $file)" "$file" | head -20
done
```

### 4.3 Run Package Check
```bash
# Full package check
R -e "devtools::check()"

# Quick documentation check only
R -e "devtools::check(document = FALSE, args = '--no-tests')"
```

### 4.4 Visual Verification
```R
# In R, verify help pages render correctly
library(hetid)

# Check a sample of functions
?run_main_simulation
?analyze_main_results
?generate_lewbel_data
?plot_estimator_distributions
```

### 4.5 Rollback Procedure
```bash
# If validation fails, restore from backup
rm -rf R/
rm -rf man/
cp -r R_backup_*/ R/
cp -r man_backup_*/ man/
rm -rf inst/roxygen/templates/

# Regenerate documentation from original source
R -e "devtools::document()"
```

## 5. COMPLETION CHECKLIST

□ All patterns from `documentation-patterns.md` addressed
  - □ 1.1 config parameter (20 occurrences)
  - □ 1.2 verbose parameter (15 occurrences)
  - □ 1.3 seeds parameter (4 occurrences)
  - □ 1.4 df_adjust parameter (8 occurrences)
  - □ 1.5 bootstrap parameters (4 occurrences)
  - □ 1.6 results parameters (4 occurrences)
  - □ 1.7 n_obs parameter (2 occurrences)
  - □ 1.8 params parameter (3 occurrences)
  - □ 2.1 triangular system equations (2 occurrences)
  - □ 2.2 error structure (2 occurrences)
  - □ 6.1 knitr note (4 occurrences)

□ No duplicate text remains in documentation
□ `devtools::document()` runs without errors
□ Checksum comparison shows no changes in `.Rd` output
□ `devtools::check()` passes with 0 errors/warnings
□ Test documentation renders identically in RStudio help viewer

## Additional Notes

1. **Template Naming Convention**: Templates are named with descriptive prefixes (param-, details-, note-, example-) for easy identification.

2. **Gradual Implementation**: This plan can be implemented incrementally. Start with the most common patterns (config, verbose) and validate before proceeding.

3. **Future Maintenance**: After implementation, new functions should use these templates rather than duplicating documentation.

4. **Documentation Build**: Remember to set `Roxygen: list(markdown = TRUE)` in DESCRIPTION if not already set.

5. **Version Control**: Commit the templates directory first, then make documentation changes in logical groups for easier review.
