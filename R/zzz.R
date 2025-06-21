# Package startup and global variable declarations

# Suppress R CMD check notes about undefined global functions
# hetid_const is an internal function defined in options.R
utils::globalVariables("hetid_const")

# Suppress R CMD check notes for package constants defined in 00-constants.R
if (getRversion() >= "3.2.0") {
  utils::globalVariables(c(
    # Statistical constants
    ".ALPHA_DEFAULT", ".ALPHA_STRICT",
    # Sample sizes
    ".N_DEFAULT", ".N_SMALL", ".N_BOOTSTRAP_DEFAULT",
    # GARCH parameters
    ".GARCH_OMEGA_DEFAULT", ".GARCH_ALPHA_DEFAULT", ".GARCH_BETA_DEFAULT",
    # Thresholds
    ".IDENTIFICATION_TOLERANCE", ".COV_VARIATION_THRESHOLD",
    # System types
    ".SYSTEM_TRIANGULAR", ".SYSTEM_SIMULTANEOUS",
    # Error messages
    ".MSG_PACKAGE_REQUIRED", ".MSG_VARIABLE_NOT_FOUND",
    ".MSG_ESTIMATION_FAILED", ".MSG_CONVERGENCE_FAILED",
    # Package names
    ".PKG_GMM", ".PKG_SANDWICH", ".PKG_TSGARCH", ".PKG_IVREG", ".PKG_AER",
    # Other
    ".VCOV_HAC"
  ))
}

# Constants environment initialization happens in constants.R
