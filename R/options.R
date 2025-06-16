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
