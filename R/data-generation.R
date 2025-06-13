#' Generate Data for Lewbel (2012) Triangular Model
#'
#' Creates a dataset based on the triangular model with single-factor error structure
#' that satisfies Lewbel's identifying assumptions. The data generating process uses
#' a common factor structure for the errors to ensure the covariance restriction
#' Cov(Z, \epsilon_1 \epsilon_2) = 0 is satisfied.
#'
#' @param n Integer. Sample size.
#' @param params List. Parameters for the data generating process containing:
#'   \itemize{
#'     \item beta1_0, beta1_1: Parameters for first equation
#'     \item beta2_0, beta2_1: Parameters for second equation
#'     \item gamma1: Endogenous parameter (key parameter of interest)
#'     \item alpha1, alpha2: Factor loadings for common factor U
#'     \item delta_het: Heteroscedasticity strength parameter
#'   }
#'
#' @details
#' The triangular model is:
#' \deqn{Y_1 = \beta_{1,0} + \beta_{1,1}X + \gamma_1 Y_2 + \epsilon_1}
#' \deqn{Y_2 = \beta_{2,0} + \beta_{2,1}X + \epsilon_2}
#'
#' The error structure follows a single-factor model:
#' \deqn{\epsilon_1 = \alpha_1 U + V_1}
#' \deqn{\epsilon_2 = \alpha_2 U + V_2}
#'
#' where U, V_1 are independent standard normal, and V_2 ~ N(0, exp(\delta Z))
#' with Z = X^2 - E[X^2] being the heteroscedasticity driver.
#'
#' @return A data.frame with columns Y1, Y2, Xk, Z, epsilon1, epsilon2.
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data <- generate_lewbel_data(1000, params)
#' head(data)
#' }
#'
#' @export
generate_lewbel_data <- function(n, params) {
  # Generate exogenous variables
  Xk <- stats::rnorm(n, mean = 2, sd = 1)
  Z <- Xk^2 - mean(Xk^2)

  # Generate mutually independent error components
  U <- stats::rnorm(n)
  V1 <- stats::rnorm(n)

  # Add safeguard for numerical stability
  # Cap the exponent to prevent overflow
  exponent <- params$delta_het * Z
  exponent <- pmin(pmax(exponent, -10), 10) # Cap between -10 and 10
  V2 <- stats::rnorm(n) * sqrt(exp(exponent))

  # Construct structural errors using single-factor model
  epsilon1 <- params$alpha1 * U + V1
  epsilon2 <- params$alpha2 * U + V2

  # Generate endogenous variables
  Y2 <- params$beta2_0 + params$beta2_1 * Xk + epsilon2
  Y1 <- params$beta1_0 + params$beta1_1 * Xk + params$gamma1 * Y2 + epsilon1

  data.frame(Y1, Y2, Xk, Z, epsilon1, epsilon2)
}


#' Verify Lewbel's Key Identifying Assumptions
#'
#' Tests whether the data generating process satisfies the key assumptions
#' required for Lewbel's (2012) identification strategy. This includes testing
#' the covariance restriction and instrument relevance condition.
#'
#' @param data Data.frame. Optional. Pre-generated data to verify (alternative to n/params).
#' @param config List. Optional. Configuration object (used with data parameter).
#' @param n Integer. Sample size for verification (default: 10000, used with params).
#' @param params List. Parameters for the DGP (same format as generate_lewbel_data).
#' @param verbose Logical. Whether to print detailed output (default: TRUE).
#'
#' @details
#' The function tests:
#' \itemize{
#'   \item Assumption A2: Cov(Z, \epsilon_1 \epsilon_2) = 0 (covariance restriction)
#'   \item Assumption A3: Cov(Z, \epsilon_2^2) != 0 (instrument relevance)
#'   \item Endogeneity: Cov(\epsilon_1, \epsilon_2) != 0
#' }
#'
#' Can be called in two ways:
#' 1. verify_lewbel_assumptions(data, config) - using pre-generated data
#' 2. verify_lewbel_assumptions(n = 10000, params = params) - generating new data
#'
#' @return Invisibly returns a list with verification results and data.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' params <- list(
#'   beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
#'   beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
#'   alpha1 = config$alpha1, alpha2 = config$alpha2, delta_het = config$delta_het
#' )
#' verify_lewbel_assumptions(params = params)
#' }
#'
#' @export
verify_lewbel_assumptions <- function(data = NULL, config = NULL, n = 10000, params = NULL, verbose = TRUE) {
  if (verbose) {
    cat("\n--- Verifying Lewbel's Key Assumptions ---\n")
  }

  # Handle different calling patterns
  if (!is.null(data) && !is.null(config)) {
    # Called with data and config (new pattern)
    test_data <- data
    n <- nrow(data)
  } else if (!is.null(params)) {
    # Called with n and params (original pattern)
    n <- as.integer(n) # Ensure n is an integer
    test_data <- generate_lewbel_data(n, params)
  } else {
    stop("Must provide either (data, config) or (n, params)")
  }

  # Calculate sample moments
  cov_z_e1e2 <- stats::cov(test_data$Z, test_data$epsilon1 * test_data$epsilon2)
  cov_z_e2sq <- stats::cov(test_data$Z, test_data$epsilon2^2)
  cov_e1_e2 <- stats::cov(test_data$epsilon1, test_data$epsilon2)

  # Statistical test for Cov(Z, e1*e2) = 0 (always calculate)
  test_stat <- sqrt(as.numeric(n)) * cov_z_e1e2 / stats::sd(test_data$Z * test_data$epsilon1 * test_data$epsilon2)
  p_value <- 2 * (1 - stats::pnorm(abs(test_stat)))

  if (verbose) {
    cat(sprintf("Test sample size: %d\n", as.integer(n)))
    cat(sprintf("Cov(Z, e1*e2) = %.6f (should be approx. 0)\n", cov_z_e1e2))
    cat(sprintf("Cov(Z, e2^2) = %.6f (should be != 0)\n", cov_z_e2sq))
    cat(sprintf("Cov(e1, e2) = %.6f (should be != 0 for endogeneity)\n", cov_e1_e2))
    cat(sprintf("Test H0: Cov(Z, e1*e2) = 0, p-value = %.4f\n", p_value))

    if (p_value < 0.05) {
      warning("Key assumption Cov(Z, e1*e2) = 0 appears to be violated!")
    } else {
      cat("Key assumptions appear to be satisfied.\n")
    }
  }

  # Return results
  results <- list(
    cov_z_e1e2 = cov_z_e1e2,
    cov_z_e2sq = cov_z_e2sq,
    cov_e1_e2 = cov_e1_e2,
    test_stat = test_stat,
    p_value = p_value,
    data = test_data
  )

  invisible(results)
}
