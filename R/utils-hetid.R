#' Check if REndo is available
#' @export
has_rendo <- function() {
  requireNamespace("REndo", quietly = TRUE)
}

#' Check if curl is available
#' @export
has_curl <- function() {
  requireNamespace("curl", quietly = TRUE)
}

#' Check if haven is available
#' @export
has_haven <- function() {
  requireNamespace("haven", quietly = TRUE)
}

#' Check if RStata is available
#' @export
has_rstata <- function() {
  requireNamespace("RStata", quietly = TRUE)
}

#' Check if Stata is available via RStata
#' @export
has_stata <- function() {
  if (!has_rstata()) {
    FALSE
  } else {
    # Check for Stata executable
    stata_paths <- c(
      "stata", "stata-mp", "stata-se",
      "StataMP", "StataSE", "Stata"
    )

    # Also check common Mac application paths
    mac_paths <- c(
      "/Applications/Stata/StataSE.app/Contents/MacOS/StataSE",
      "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP",
      "/Applications/Stata/Stata.app/Contents/MacOS/Stata"
    )

    any(nzchar(Sys.which(stata_paths))) || any(file.exists(mac_paths))
  }
}

#' Get the actual Stata executable path
#' @export
get_stata_path <- function() {
  if (!has_stata()) {
    NULL
  } else {
    # Try standard paths first
    stata_paths <- c(
      "stata", "stata-mp", "stata-se",
      "StataMP", "StataSE", "Stata"
    )

    result <- NULL

    for (path in stata_paths) {
      full_path <- Sys.which(path)
      if (nzchar(full_path)) {
        result <- as.character(full_path)
        break
      }
    }

    if (is.null(result)) {
      # Check Mac application paths
      mac_paths <- c(
        "/Applications/Stata/StataSE.app/Contents/MacOS/StataSE",
        "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP",
        "/Applications/Stata/Stata.app/Contents/MacOS/Stata"
      )

      for (path in mac_paths) {
        if (file.exists(path)) {
          result <- path
          break
        }
      }
    }

    result
  }
}

#' Helper to ensure Stata packages are installed
#' @export
ensure_stata_packages <- function() {
  stata_path <- get_stata_path()
  if (is.null(stata_path)) {
    FALSE
  } else {
    # Create a do file to check and install packages
    temp_do <- tempfile(fileext = ".do")
    writeLines(c(
      "capture which ranktest",
      "if _rc {",
      "    ssc install ranktest, replace",
      "}",
      "",
      "capture which ivreg2",
      "if _rc {",
      "    ssc install ivreg2, replace",
      "}",
      "",
      "capture which ivreg2h",
      "if _rc {",
      "    ssc install ivreg2h, replace",
      "}",
      "",
      "display \"Stata packages check complete\"",
      "exit"
    ), temp_do)

    # Run the installation
    temp_log <- tempfile(fileext = ".log")
    cmd <- sprintf("%s -b do %s", stata_path, temp_do)
    result <- system(cmd, intern = FALSE, ignore.stdout = TRUE)

    # Clean up
    unlink(temp_do)
    unlink(temp_log)

    result == 0
  }
}

#' Generate consistent test data for all comparisons
#'
#' @param n Integer. Number of observations to generate (default: 1000).
#' @param seed Integer. Random seed for reproducibility (default: 42).
#'
#' @return A data.frame with test data for Lewbel identification.
#'
#' @export
generate_hetid_test_data <- function(n = .N_DEFAULT, seed = 42) {
  set.seed(seed)

  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)

  # Rename columns for consistency across packages
  names(data)[names(data) == "Y1"] <- "y"
  names(data)[names(data) == "Y2"] <- "P" # endogenous variable
  names(data)[names(data) == "Xk"] <- "X1" # exogenous variable

  # Generate the Lewbel instrument manually for transparency
  first_stage <- lm(P ~ X1, data = data)
  e2_hat <- residuals(first_stage)
  z_demeaned <- data$Z - mean(data$Z)
  data$lewbel_iv <- z_demeaned * e2_hat
  # ensure exactly mean zero
  data$lewbel_iv <- data$lewbel_iv - mean(data$lewbel_iv)

  data
}
