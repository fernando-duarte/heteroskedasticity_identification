#' Generate Data for Lewbel (2012) Triangular Model
#'
#' Creates a dataset based on the triangular model with single-factor error structure
#' that satisfies Lewbel's identifying assumptions. The data generating process uses
#' a common factor structure for the errors to ensure the covariance restriction
#' Cov(Z, ε₁ε₂) = 0 is satisfied.
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
#' \deqn{Y₁ = β₁₀ + β₁₁X + γ₁Y₂ + ε₁}
#' \deqn{Y₂ = β₂₀ + β₂₁X + ε₂}
#' 
#' The error structure follows a single-factor model:
#' \deqn{ε₁ = α₁U + V₁}
#' \deqn{ε₂ = α₂U + V₂}
#' 
#' where U, V₁ are independent standard normal, and V₂ ~ N(0, exp(δZ))
#' with Z = X² - E[X²] being the heteroscedasticity driver.
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
  V2 <- stats::rnorm(n) * sqrt(exp(params$delta_het * Z))
  
  # Construct structural errors using single-factor model
  epsilon1 <- params$alpha1 * U + V1
  epsilon2 <- params$alpha2 * U + V2
  
  # Generate endogenous variables
  Y2 <- params$beta2_0 + params$beta2_1 * Xk + epsilon2
  Y1 <- params$beta1_0 + params$beta1_1 * Xk + params$gamma1 * Y2 + epsilon1
  
  return(data.frame(Y1, Y2, Xk, Z, epsilon1, epsilon2))
}


#' Verify Lewbel's Key Identifying Assumptions
#'
#' Tests whether the data generating process satisfies the key assumptions
#' required for Lewbel's (2012) identification strategy. This includes testing
#' the covariance restriction and instrument relevance condition.
#'
#' @param n Integer. Sample size for verification (default: 10000).
#' @param params List. Parameters for the DGP (same format as generate_lewbel_data).
#' @param verbose Logical. Whether to print detailed output (default: TRUE).
#'
#' @details
#' The function tests:
#' \itemize{
#'   \item Assumption A2: Cov(Z, ε₁ε₂) = 0 (covariance restriction)
#'   \item Assumption A3: Cov(Z, ε₂²) ≠ 0 (instrument relevance)
#'   \item Endogeneity: Cov(ε₁, ε₂) ≠ 0
#' }
#'
#' @return Invisibly returns the test dataset. Prints verification results if verbose=TRUE.
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
verify_lewbel_assumptions <- function(n = 10000, params, verbose = TRUE) {
  if (verbose) {
    cat("\n--- Verifying Lewbel's Key Assumptions ---\n")
  }
  
  # Generate large test dataset
  test_data <- generate_lewbel_data(n, params)
  
  # Calculate sample moments
  cov_Z_e1e2 <- stats::cov(test_data$Z, test_data$epsilon1 * test_data$epsilon2)
  cov_Z_e2sq <- stats::cov(test_data$Z, test_data$epsilon2^2)
  cov_e1_e2 <- stats::cov(test_data$epsilon1, test_data$epsilon2)
  
  if (verbose) {
    cat(sprintf("Test sample size: %d\n", n))
    cat(sprintf("Cov(Z, e1*e2) = %.6f (should be ≈ 0)\n", cov_Z_e1e2))
    cat(sprintf("Cov(Z, e2^2) = %.6f (should be ≠ 0)\n", cov_Z_e2sq))
    cat(sprintf("Cov(e1, e2) = %.6f (should be ≠ 0 for endogeneity)\n", cov_e1_e2))
    
    # Statistical test for Cov(Z, e1*e2) = 0
    test_stat <- sqrt(n) * cov_Z_e1e2 / stats::sd(test_data$Z * test_data$epsilon1 * test_data$epsilon2)
    p_value <- 2 * (1 - stats::pnorm(abs(test_stat)))
    cat(sprintf("Test H0: Cov(Z, e1*e2) = 0, p-value = %.4f\n", p_value))
    
    if (p_value < 0.05) {
      warning("Key assumption Cov(Z, e1*e2) = 0 appears to be violated!")
    } else {
      cat("✓ Key assumptions appear to be satisfied.\n")
    }
  }
  
  return(invisible(test_data))
}
