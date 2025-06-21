#' Internal numeric and string constants for the hetid package
#' @keywords internal
#' @description This file defines package-wide constants to ensure Single Source of Truth.
#' It is loaded first due to alphabetical ordering (00- prefix).

# Statistical significance levels
.ALPHA_DEFAULT <- 0.05
.ALPHA_STRICT <- 0.01

# Sample sizes
.N_DEFAULT <- 1000
.N_SMALL <- 500
.N_BOOTSTRAP_DEFAULT <- 100

# GARCH model default parameters
.GARCH_OMEGA_DEFAULT <- 0.2
.GARCH_ALPHA_DEFAULT <- 0.1
.GARCH_BETA_DEFAULT <- 0.85

# Numerical tolerances and thresholds
.IDENTIFICATION_TOLERANCE <- 0.1  # For abs(gamma1 * gamma2 - 1) check
.COV_VARIATION_THRESHOLD <- 0.2   # 20% covariance variation threshold

# System types for Lewbel GMM
.SYSTEM_TRIANGULAR <- "triangular"
.SYSTEM_SIMULTANEOUS <- "simultaneous"

# Error message templates
.MSG_PACKAGE_REQUIRED <- "Package '%s' is required but not installed. Please install it with: install.packages('%s')"
.MSG_VARIABLE_NOT_FOUND <- "Variable '%s' not found in data"
.MSG_ESTIMATION_FAILED <- "%s estimation failed: %s"
.MSG_CONVERGENCE_FAILED <- "Convergence not achieved in %d iterations"

# Package names (for dependency checks)
.PKG_GMM <- "gmm"
.PKG_SANDWICH <- "sandwich"
.PKG_TSGARCH <- "tsgarch"
.PKG_IVREG <- "ivreg"
.PKG_AER <- "AER"

# Variance-covariance matrix type
.VCOV_HAC <- "HAC"
