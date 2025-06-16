#' Get hetid package options with fallback to constants
#'
#' This function retrieves user-configurable options for the hetid package.
#' If a user hasn't set a specific option, it falls back to the default
#' value stored in the package's internal constants environment.
#'
#' @param option_name Character. Name of the option to retrieve. Supported
#'   options include:
#'   \itemize{
#'     \item \code{display_digits}: Number of digits for displaying results
#'     \item \code{alpha_level}: Significance level for statistical tests
#'     \item \code{weak_f_threshold}: F-statistic threshold for weak instruments
#'     \item \code{parallel_offset}: Offset for parallel processing workers
#'     \item \code{bootstrap_reps}: Number of bootstrap replications
#'   }
#'
#' @return The option value. Returns the user-set value if available,
#'   otherwise returns the default from the constants environment.
#'
#' @examples
#' # Get default display digits
#' hetid_opt("display_digits")
#'
#' # Set a custom value
#' options(hetid.display_digits = 6)
#' hetid_opt("display_digits")
#'
#' # Reset to default
#' options(hetid.display_digits = NULL)
#' hetid_opt("display_digits")
#'
#' @export
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

#' Get package constants
#' @param constant_name Character. Name of the constant to retrieve
#' @return The constant value from the package's constants environment
#' @keywords internal
hetid_const <- function(constant_name) {
  constants_env <- get("constants_env", envir = asNamespace("hetid"))
  constants_env[[constant_name]]
}
