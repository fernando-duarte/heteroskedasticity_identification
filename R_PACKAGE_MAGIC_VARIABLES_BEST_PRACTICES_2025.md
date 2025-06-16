# R Package Magic Variables Best Practices (June 2025)

## Overview

This document provides comprehensive best practices for handling all types of "magic variables" in R packages as of June 2025. Magic variables include:
- NSE (non-standard evaluation) variables used in tidyverse functions
- Constants (numeric, character, logical)
- Default parameter values
- Configuration settings
- Magic strings and numbers
- Environment-specific values

## 1. Non-Standard Evaluation (NSE) Variables

### 1.1 Modern Tidyverse Approach: Use `.data` Pronoun

**ALWAYS use the `.data` pronoun for column references in tidyverse functions instead of `globalVariables()`:**

```r
# BAD - requires globalVariables()
results %>%
  dplyr::group_by(group_var) %>%
  dplyr::summarise(mean_val = mean(value_var))

# GOOD - explicit and no globalVariables needed
results %>%
  dplyr::group_by(.data$group_var) %>%
  dplyr::summarise(mean_val = mean(.data$value_var))
```

### 1.2 Function Arguments with NSE: Use Embracing

**For functions accepting column names as arguments, use `{{}}` (embracing):**

```r
#' @importFrom rlang enquo !!
analyze_by_group <- function(data, group_var, value_var) {
  data %>%
    dplyr::group_by({{ group_var }}) %>%
    dplyr::summarise(
      mean_value = mean({{ value_var }}, na.rm = TRUE),
      .groups = "drop"
    )
}
```

### 1.3 Document NSE Parameters

**Always document data-masked parameters explicitly:**

```r
#' @param group_var <[`data-masked`][dplyr::dplyr_data_masking]>
#'   Variable to group by (supports tidy evaluation)
#' @param value_var <[`data-masked`][dplyr::dplyr_data_masking]>
#'   Variable to summarize (supports tidy evaluation)
```

### 1.4 Complex NSE with rlang

**For programmatic NSE, use rlang functions:**

```r
#' @importFrom rlang sym !! := enquo quo_name
create_summary_col <- function(data, col_name, source_col) {
  col_sym <- rlang::sym(col_name)
  source_quo <- rlang::enquo(source_col)

  data %>%
    dplyr::mutate(
      !!col_sym := mean(!!source_quo, na.rm = TRUE)
    )
}
```

### 1.5 Import Requirements for NSE

**Required imports in NAMESPACE:**

```r
#' @importFrom rlang .data !! := enquo quo_name
#' @importFrom dplyr %>%
```

### 1.6 Detection Tool: checkglobals

**Use checkglobals package (April 2025) instead of relying on R CMD check:**

```r
# During development
checkglobals::checkglobals("R/", verbose = TRUE)
```

## 2. Package Constants and Magic Numbers

### 2.1 Internal Constants via R/sysdata.rda

**Store package-wide constants that don't change between environments:**

```r
# In data-raw/internal-constants.R
.PACKAGE_CONSTANTS <- list(
  # Numeric constants
  MAX_ITERATIONS = 10000L,
  CONVERGENCE_TOLERANCE = 1e-8,
  MIN_SAMPLE_SIZE = 30L,
  DEFAULT_SEED = 123L,

  # Statistical constants
  SIGNIFICANCE_LEVELS = c(0.90, 0.95, 0.99),
  P_VALUE_THRESHOLD = 0.05,

  # Algorithm constants
  CHUNK_SIZE = 1000L,
  MAX_RETRIES = 3L,
  TIMEOUT_SECONDS = 300L
)

# Save to R/sysdata.rda
usethis::use_data(.PACKAGE_CONSTANTS, internal = TRUE, overwrite = TRUE)

# Access in package functions
check_convergence <- function(value) {
  value < .PACKAGE_CONSTANTS$CONVERGENCE_TOLERANCE
}
```

### 2.2 Constants Function Pattern

**For constants that need documentation or organization:**

```r
# R/constants.R
#' Internal package constants
#' @keywords internal
.pkg_constants <- function() {
  list(
    # Error messages
    errors = list(
      INVALID_INPUT = "Input must be a data.frame with at least %d rows",
      MISSING_COLUMN = "Required column '%s' not found in data",
      TYPE_MISMATCH = "Expected %s but got %s"
    ),

    # Warning messages
    warnings = list(
      SMALL_SAMPLE = "Sample size (%d) is below recommended minimum (%d)",
      CONVERGENCE_SLOW = "Algorithm converged slowly after %d iterations"
    ),

    # File patterns
    file_patterns = list(
      CSV_EXTENSION = "\\.csv$",
      R_EXTENSION = "\\.R$",
      VALID_NAME = "^[a-zA-Z][a-zA-Z0-9_]*$"
    ),

    # Method names
    methods = list(
      CORRELATION = c("pearson", "kendall", "spearman"),
      DISTANCE = c("euclidean", "manhattan", "minkowski")
    )
  )
}

# Usage
validate_method <- function(method, type = "correlation") {
  const <- .pkg_constants()
  valid_methods <- const$methods[[toupper(type)]]

  if (!method %in% valid_methods) {
    stop(sprintf(
      "Invalid %s method. Choose from: %s",
      type,
      paste(valid_methods, collapse = ", ")
    ), call. = FALSE)
  }
}
```

### 2.3 Never Use Magic Numbers Directly

**ALWAYS assign semantic names to numeric constants:**

```r
# BAD
if (first_stage_f < 10) {
  warning("Weak instrument")
}

# GOOD
.WEAK_INSTRUMENT_THRESHOLD <- 10
if (first_stage_f < .WEAK_INSTRUMENT_THRESHOLD) {
  warning(sprintf(
    "Weak instrument detected (F = %.2f < %.0f)",
    first_stage_f, .WEAK_INSTRUMENT_THRESHOLD
  ))
}
```

## 3. Configuration Management

### 3.1 Use config Package for Environment-Specific Settings

**Create config.yml for environment-specific values:**

```yaml
# inst/config/default-config.yml
default:
  # Numeric settings
  max_iterations: 1000
  tolerance: 1e-6
  n_cores: !expr parallel::detectCores() - 1

  # Character settings
  log_level: "info"
  output_format: "csv"

  # Feature flags
  use_parallel: true
  enable_logging: true

development:
  inherits: default
  max_iterations: 100
  log_level: "debug"

production:
  inherits: default
  max_iterations: 10000
  tolerance: 1e-8
  log_level: "warning"

testing:
  inherits: default
  n_cores: 2
  use_parallel: false
  enable_logging: false
```

**Access configuration:**

```r
# R/config-utils.R
get_config <- function(value = NULL, config = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  config_file <- system.file("config/default-config.yml", package = "yourpackage")

  if (file.exists("config.yml")) {
    # Allow user override
    config_file <- "config.yml"
  }

  config::get(value = value, config = config, file = config_file)
}
```

### 3.2 Hierarchical Configuration System

**Implement priority-based configuration:**

```r
#' Get configuration value with fallback hierarchy
#' Priority: function arg > R option > env var > config file > default
get_package_option <- function(name, default = NULL, envvar_prefix = "MYPKG") {
  # 1. Check R option
  opt_value <- getOption(paste0("mypkg.", name))
  if (!is.null(opt_value)) return(opt_value)

  # 2. Check environment variable
  env_name <- paste0(envvar_prefix, "_", toupper(gsub("\\.", "_", name)))
  env_value <- Sys.getenv(env_name, NA)
  if (!is.na(env_value)) {
    # Parse env var based on default type
    if (is.numeric(default)) return(as.numeric(env_value))
    if (is.logical(default)) return(tolower(env_value) %in% c("true", "1", "yes"))
    return(env_value)
  }

  # 3. Check config file
  tryCatch({
    config_value <- get_config(name)
    if (!is.null(config_value)) return(config_value)
  }, error = function(e) NULL)

  # 4. Return default
  default
}
```

## 4. Package Options Management

### 4.1 Use options Package Pattern

**Define package options formally:**

```r
# R/zzz.R or R/options.R
.onLoad <- function(libname, pkgname) {
  # Define options with defaults
  options::define_options(
    "Control verbosity of package messages",
    quiet = FALSE,

    "Maximum iterations for optimization algorithms",
    max_iterations = 1000,

    "Convergence tolerance for iterative methods",
    tolerance = 1e-6,

    "Number of cores for parallel processing (0 = auto-detect)",
    n_cores = 0L,

    "Default method for statistical calculations",
    default_method = "pearson",

    # Can reference environment variables
    "API key for external service",
    api_key = Sys.getenv("MYPKG_API_KEY", ""),

    # Can use expressions
    "Temporary directory for intermediate files",
    temp_dir = tempdir()
  )
}

# R/options-utils.R
#' @importFrom options opt
pkg_opt <- function(name) {
  options::opt(name)
}
```

### 4.2 Document Package Options

**Auto-generate documentation:**

```r
#' Package Options
#'
#' @description
#' Options that control the behavior of package functions.
#'
#' @eval options::as_roxygen_docs()
NULL
```

### 4.3 Use Options in Function Defaults

**Reference options in function signatures:**

```r
#' Run optimization
#' @param max_iter Maximum iterations (default: \code{getOption("mypkg.max_iterations", 1000)})
#' @param tol Convergence tolerance (default: \code{getOption("mypkg.tolerance", 1e-6)})
optimize_model <- function(
    data,
    max_iter = pkg_opt("max_iterations"),
    tol = pkg_opt("tolerance")
) {
  # Function body
}
```

## 5. Default Parameter Values

### 5.1 Document Defaults Clearly

**Use NULL for complex defaults:**

```r
#' Process data with configuration
#' @param config Configuration list. If NULL (default), uses \code{create_default_config()}
process_data <- function(data, config = NULL) {
  if (is.null(config)) {
    config <- create_default_config()
  }
  # Process...
}
```

### 5.2 Create Default Configuration Functions

**Centralize default creation:**

```r
#' Create default analysis configuration
#' @export
create_default_config <- function(
    n_iterations = 1000,
    method = "pearson",
    confidence_level = 0.95,
    seed = NULL
) {
  list(
    n_iterations = n_iterations,
    method = match.arg(method, c("pearson", "spearman", "kendall")),
    confidence_level = confidence_level,
    seed = seed %||% sample.int(1e6, 1),
    created_at = Sys.time(),
    version = utils::packageVersion("mypkg")
  )
}
```

### 5.3 Validate Defaults

**Always validate configuration objects:**

```r
validate_config <- function(config) {
  stopifnot(
    is.list(config),
    "n_iterations must be positive integer" =
      is.numeric(config$n_iterations) && config$n_iterations > 0,
    "confidence_level must be between 0 and 1" =
      config$confidence_level > 0 && config$confidence_level < 1,
    "method must be valid" =
      config$method %in% c("pearson", "spearman", "kendall")
  )
  invisible(config)
}
```

## 6. Magic Strings Management

### 6.1 Centralize String Constants

**Never repeat string literals:**

```r
# R/string-constants.R
.string_constants <- list(
  # Column names
  cols = list(
    ID = "id",
    TIMESTAMP = "timestamp",
    VALUE = "value",
    GROUP = "group"
  ),

  # Output formats
  formats = list(
    CSV = "csv",
    JSON = "json",
    PARQUET = "parquet"
  ),

  # Status values
  status = list(
    SUCCESS = "success",
    FAILURE = "failure",
    PENDING = "pending"
  )
)

# Usage
format_output <- function(data, format = .string_constants$formats$CSV) {
  switch(format,
    csv = write.csv(data, ...),
    json = jsonlite::write_json(data, ...),
    stop("Unknown format: ", format)
  )
}
```

### 6.2 Use Match.arg for String Parameters

**Validate string inputs:**

```r
analyze_data <- function(
    data,
    method = c("regression", "correlation", "classification")
) {
  method <- match.arg(method)
  # Now method is validated
}
```

## 7. DRY Principle for Documentation

### 7.1 Use Child Documents

**Store repeated documentation in man/fragments/:**

```
man/
└── fragments/
    ├── common-parameters.Rmd
    ├── return-values.Rmd
    └── statistical-details.Rmd
```

**Include in multiple functions:**

```r
#' @includeRmd man/fragments/common-parameters.Rmd
#' @includeRmd man/fragments/return-values.Rmd
```

### 7.2 Use Inherited Parameters

**Define parameter sets:**

```r
#' @name common_params
#' @param data Data frame containing the analysis data
#' @param method Character string specifying the method
#' @param conf_level Numeric confidence level (0-1)
NULL

#' Analyze data
#' @inheritParams common_params
analyze <- function(data, method = "pearson", conf_level = 0.95) {
  # Implementation
}
```

## 8. Best Practices Summary

### 8.1 DO's

1. **ALWAYS use `.data$` for tidyverse column references**
2. **ALWAYS use semantic names for constants**
3. **ALWAYS validate string inputs with match.arg()**
4. **ALWAYS document magic values in one place**
5. **ALWAYS use config package for environment-specific settings**
6. **ALWAYS provide defaults through functions, not literals**
7. **ALWAYS use checkglobals during development**

### 8.2 DON'Ts

1. **NEVER use globalVariables() for tidyverse NSE (use .data)**
2. **NEVER hardcode magic numbers or strings**
3. **NEVER repeat error messages or warnings**
4. **NEVER use setwd() or modify global options without restoration**
5. **NEVER assume environment variables exist**
6. **NEVER put configuration in multiple places**

### 8.3 Testing Constants

**Test different configurations:**

```r
test_that("function works with different configs", {
  withr::with_options(
    list(mypkg.max_iterations = 10),
    {
      result <- optimize_model(data)
      expect_equal(attr(result, "iterations"), 10)
    }
  )

  withr::with_envvar(
    list(MYPKG_MAX_ITERATIONS = "20"),
    {
      result <- optimize_model(data)
      expect_equal(attr(result, "iterations"), 20)
    }
  )
})
```

## 9. Migration Strategy

1. **Start with new code** - Apply these patterns to new functions
2. **Gradual refactoring** - Update existing code incrementally
3. **Maintain compatibility** - Keep old interfaces working with deprecation warnings
4. **Document changes** - Update NEWS.md with configuration changes

## 10. Tool Configuration

### 10.1 .lintr Configuration

```yaml
linters: with_defaults(
  object_usage_linter = NULL,
  undesirable_function_linter(
    fun = list(
      globalVariables = "Use .data pronoun instead",
      setwd = "Never change working directory",
      options = "Use withr::with_options or restore"
    )
  )
)
```

### 10.2 Pre-commit Hooks

```yaml
repos:
  - repo: https://github.com/lorenzwalthert/precommit
    hooks:
      - id: style-files
      - id: lintr
      - id: no-browser-statement
      - id: deps-in-desc
```
