#' Internal constants for the hetid package
#' @keywords internal
#' @description This file defines all package-wide constants using a single
#'   environment to ensure Single Source of Truth and follow R package best practices.

# Create package constants environment
.hetid_constants <- new.env(parent = emptyenv())

# Statistical parameters
.hetid_constants$ALPHA_DEFAULT <- 0.05
.hetid_constants$ALPHA_STRICT <- 0.01

# Sample sizes
.hetid_constants$N_DEFAULT <- 1000
.hetid_constants$N_SMALL <- 500
.hetid_constants$N_BOOTSTRAP_DEFAULT <- 100

# GARCH model parameters
.hetid_constants$GARCH <- list(
  OMEGA_DEFAULT = 0.2,
  ALPHA_DEFAULT = 0.1,
  BETA_DEFAULT = 0.85
)

# Numerical tolerances and thresholds
.hetid_constants$IDENTIFICATION_TOLERANCE <- 0.1  # For abs(gamma1 * gamma2 - 1) check
.hetid_constants$COV_VARIATION_THRESHOLD <- 0.2   # 20% covariance variation threshold

# System types
.hetid_constants$SYSTEM <- list(
  TRIANGULAR = "triangular",
  SIMULTANEOUS = "simultaneous"
)

# Error messages
.hetid_constants$messages <- list(
  PACKAGE_REQUIRED = "Package '%s' is required but not installed. Please install it with: install.packages('%s')",
  VARIABLE_NOT_FOUND = "Variable '%s' not found in data",
  ESTIMATION_FAILED = "%s estimation failed: %s",
  CONVERGENCE_FAILED = "Convergence not achieved in %d iterations"
)

# Package names (for dependency checks)
.hetid_constants$packages <- list(
  GMM = "gmm",
  SANDWICH = "sandwich",
  TSGARCH = "tsgarch",
  IVREG = "ivreg",
  AER = "AER"
)

# Variance-covariance matrix type
.hetid_constants$VCOV_HAC <- "HAC"

# Degrees of freedom adjustment methods
.hetid_constants$df_adjust <- list(
  ASYMPTOTIC = "asymptotic",
  FINITE = "finite"
)

# Column names
.hetid_constants$columns <- list(
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
)

# Variable names
.hetid_constants$variables <- list(
  DEFAULT_ENDOG_VAR = "Y2",
  DEFAULT_EXOG_VARS = "Xk"
)

# Stata integration
.hetid_constants$stata <- list(
  PACKAGES = c("ranktest", "ivreg2", "ivreg2h"),
  EXECUTABLES_ALL = c(
    "stata", "stata-mp", "stata-se", "StataMP", "StataSE", "Stata"
  ),
  EXECUTABLES_UNIX = c("stata", "stata-mp", "stata-se"),
  DO_EXTENSION = ".do",
  LOG_EXTENSION = ".log",
  MAC_PATHS = c(
    "/Applications/Stata/StataSE.app/Contents/MacOS/StataSE",
    "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP",
    "/Applications/Stata/Stata.app/Contents/MacOS/Stata"
  )
)

# Visualization
.hetid_constants$plot <- list(
  labels = list(
    OLS_BIASED = "OLS (Biased)",
    TSLS_LEWBEL = "2SLS (Lewbel)",
    OLS_COLUMN = "ols_gamma1",
    TSLS_COLUMN = "tsls_gamma1"
  ),
  colors = list(
    OLS = "#d95f02",
    TSLS = "#1b9e77",
    BOOTSTRAP_CI = "steelblue",
    BOOTSTRAP_CI_BAND = "lightgray",
    SAMPLE_SIZE_BOX = "lightblue",
    SENSITIVITY_BOX = "lightgreen",
    FIRST_STAGE_HIST = "steelblue",
    REFERENCE_LINE = "red"
  )
)

# Environment variables
.hetid_constants$env_vars <- list(
  VERBOSE = "VERBOSE",
  VERBOSE_TRUE = "TRUE",
  VERBOSE_FALSE = "FALSE"
)

# Lock the environment to prevent modifications
lockEnvironment(.hetid_constants, bindings = TRUE)

#' Access hetid constants
#'
#' Internal function to access package constants.
#'
#' @param path Character string specifying the constant path (e.g., "ALPHA_DEFAULT"
#'   or "stata$DO_EXTENSION" for nested values).
#'
#' @return The requested constant value.
#'
#' @keywords internal
.hetid_const <- function(path) {
  parts <- strsplit(path, "$", fixed = TRUE)[[1]]
  value <- .hetid_constants

  for (part in parts) {
    if (is.null(value[[part]])) {
      stop("Constant '", path, "' not found", call. = FALSE)
    }
    value <- value[[part]]
  }

  value
}

# For backward compatibility during transition
.hetid_strings <- function() {
  list(
    df_adjust = .hetid_constants$df_adjust,
    columns = .hetid_constants$columns,
    variables = .hetid_constants$variables,
    stata = .hetid_constants$stata,
    plot_labels = .hetid_constants$plot$labels,
    plot_colors = .hetid_constants$plot$colors,
    env_vars = .hetid_constants$env_vars
  )
}
