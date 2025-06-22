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
  CONVERGENCE_FAILED = "Convergence not achieved in %d iterations",
  NEED_TWO_REGIMES = "Need at least 2 regimes for Rigobon identification",
  CANNOT_AUTO_CONSTRUCT_Z_NO_X = paste0(
    "Cannot automatically construct Z. ",
    "No exogenous variables specified or only an intercept."
  ),
  CANNOT_AUTO_CONSTRUCT_Z_SIMUL = "Cannot auto-construct Z for simultaneous equations. No valid exogenous X for Z.",
  INVALID_N_X_PARAMS = "For n_x > 1, beta1_1 and beta2_1 must be vectors of length n_x",
  MISSING_DATA_OR_PARAMS = "Must provide either (data, config) or (n_obs, params)",
  MISSING_REGIME_PARAMS = "params must contain 'regime_probs' and 'sigma2_regimes'",
  REGIME_LENGTH_MISMATCH = "Length of sigma2_regimes must match length of regime_probs",
  REGIME_PROBS_SUM = "regime_probs must sum to 1"
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

# Constants from sysdata.rda
.hetid_constants$DEFAULT_NUM_SIMULATIONS <- 100
.hetid_constants$DEFAULT_MAIN_SAMPLE_SIZE <- 1000
.hetid_constants$DEFAULT_SAMPLE_SIZES <- c(500, 1000, 2000)
.hetid_constants$DEFAULT_BOOTSTRAP_REPS <- 100
.hetid_constants$DEFAULT_BOOTSTRAP_SUBSET_SIZE <- 10
.hetid_constants$DEFAULT_BOOTSTRAP_DEMO_SIZE <- 50
.hetid_constants$DEFAULT_BASE_SEED <- 42
.hetid_constants$SEED_MULTIPLIER_MAIN <- 10000
.hetid_constants$SEED_OFFSET_BY_N <- 20000
.hetid_constants$SEED_OFFSET_BY_DELTA <- 30000
.hetid_constants$SEED_MULTIPLIER_BOOTSTRAP <- 40000
.hetid_constants$DEFAULT_X_MEAN <- 0
.hetid_constants$DEFAULT_X_SD <- 1
.hetid_constants$MIN_EXPONENT <- -5
.hetid_constants$MAX_EXPONENT <- 5
.hetid_constants$WEAK_ID_TOLERANCE <- 1e-6
.hetid_constants$INSTRUMENT_SD_THRESHOLD <- 1e-10
.hetid_constants$WEAK_INSTRUMENT_F_THRESHOLD <- 10
.hetid_constants$ALPHA_LEVEL <- 0.05
.hetid_constants$DISPLAY_DIGITS <- 4
.hetid_constants$Z_CRITICAL_95 <- 1.96
.hetid_constants$BOOTSTRAP_TABLE_DISPLAY_LIMIT <- 20
.hetid_constants$PLOT_BASE_FONT_SIZE <- 14
.hetid_constants$PLOT_LINE_WIDTH_THIN <- 0.5
.hetid_constants$PLOT_LINE_WIDTH_NORMAL <- 1
.hetid_constants$PLOT_LINE_WIDTH_THICK <- 2
.hetid_constants$DEFAULT_PARALLEL_WORKER_OFFSET <- 1
.hetid_constants$PLOT_HISTOGRAM_BINS <- 30
.hetid_constants$PLOT_MIN_BOOTSTRAP_THRESHOLD <- 30
.hetid_constants$PLOT_BOOTSTRAP_DISPLAY_LIMIT <- 20
.hetid_constants$DEMO_SAMPLE_SIZES <- c(500, 1000)
.hetid_constants$DEMO_N_REPS <- 20
.hetid_constants$DEMO_BOOTSTRAP_REPS <- 100
.hetid_constants$DEMO_BOOTSTRAP_SIZE <- 20

# GMM options
.hetid_constants$GMM_HAC_KERNEL <- "Quadratic Spectral"
.hetid_constants$GMM_HAC_PREWHITE <- 1
.hetid_constants$GMM_HAC_AR_METHOD <- "ols"

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
