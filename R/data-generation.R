#' Generate Data for Lewbel (2012) Triangular Model
#'
#' Creates a dataset based on the triangular model with single-factor error
#' structure that satisfies Lewbel's identifying assumptions. The data
#' generating process uses
#' a common factor structure for the errors to ensure the covariance restriction
#' Cov(Z, \eqn{\epsilon_1 \epsilon_2}) = 0 is satisfied.
#'
#' @param n_obs Integer. Sample size.
#' @param params List. Parameters for the data generating process containing:
#'   \itemize{
#'     \item beta1_0, beta1_1: Parameters for first equation (beta1_1 can be
#'       a vector for multiple X)
#'     \item beta2_0, beta2_1: Parameters for second equation (beta2_1 can be
#'       a vector for multiple X)
#'     \item gamma1: Endogenous parameter (key parameter of interest)
#'     \item alpha1, alpha2: Factor loadings for common factor U
#'     \item delta_het: Heteroscedasticity strength parameter
#'   }
#' @param n_x Integer. Number of exogenous X variables to generate (default: 1).
#'   If n_x > 1, beta1_1 and beta2_1 should be vectors of length n_x.
#'
#' @template details-triangular-model
#'
#' @template details-error-structure
#'
#' @details
#' where U, V_1 are independent standard normal, and V_2 ~ N(0, exp(\eqn{\delta}
#' Z))
#' with Z = \eqn{X^2 - E[X^2]} being the heteroscedasticity driver.
#'
#' @return A data.frame with columns Y1, Y2, epsilon1, epsilon2, and:
#'   - If n_x = 1: Xk, Z
#'   - If n_x > 1: X1, X2, ..., Z1, Z2, ... (one Z per X)
#'
#' @examples
#' \dontrun{
#' # Single X variable (backward compatible)
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data <- generate_lewbel_data(1000, params)
#'
#' # Multiple X variables
#' params_multi <- list(
#'   beta1_0 = 0.5, beta1_1 = c(1.5, 3.0), gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = c(-1.0, 0.7),
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data_multi <- generate_lewbel_data(1000, params_multi, n_x = 2)
#' }
#'
#' @export
generate_lewbel_data <- function(n_obs, params, n_x = 1) {
  # Validate parameters for multiple X
  if (n_x > 1) {
    if (length(params$beta1_1) != n_x || length(params$beta2_1) != n_x) {
      stop("For n_x > 1, beta1_1 and beta2_1 must be vectors of length n_x")
    }
  } else {
    # Ensure scalar parameters are treated as length-1 vectors for consistency
    if (length(params$beta1_1) == 1) {
      params$beta1_1 <- as.numeric(params$beta1_1)
    }
    if (length(params$beta2_1) == 1) {
      params$beta2_1 <- as.numeric(params$beta2_1)
    }
  }

  # Generate exogenous variables
  # nolint start: object_name_linter.
  X_mat <- matrix(
    stats::rnorm(
      n_obs * n_x,
      mean = hetid_const("DEFAULT_X_MEAN"),
      sd = hetid_const("DEFAULT_X_SD")
    ),
    nrow = n_obs, ncol = n_x
  )

  # Generate Z instruments (one per X)
  Z_mat <- matrix(NA, nrow = n_obs, ncol = n_x)
  for (j in 1:n_x) {
    Z_mat[, j] <- X_mat[, j]^2 - mean(X_mat[, j]^2)
  }

  # For heteroskedasticity, use the first Z by default
  # (can be extended to use multiple Z in future)
  Z_het <- Z_mat[, 1]

  # Generate mutually independent error components
  U <- stats::rnorm(n_obs)
  V1 <- stats::rnorm(n_obs)

  # Add safeguard for numerical stability
  # Cap the exponent to prevent overflow
  exponent <- params$delta_het * Z_het
  # Cap between min and max exponent
  exponent <- pmin(
    pmax(exponent, hetid_const("MIN_EXPONENT")),
    hetid_const("MAX_EXPONENT")
  )
  V2 <- stats::rnorm(n_obs) * sqrt(exp(exponent))

  # Construct structural errors using single-factor model
  epsilon1 <- params$alpha1 * U + V1
  epsilon2 <- params$alpha2 * U + V2

  # Generate endogenous variables using all X variables
  Y2 <- params$beta2_0 + as.vector(X_mat %*% params$beta2_1) + epsilon2
  Y1 <- params$beta1_0 + as.vector(X_mat %*% params$beta1_1) +
    params$gamma1 * Y2 + epsilon1

  # Create output data frame
  result_df <- data.frame(
    Y1 = Y1, Y2 = Y2, epsilon1 = epsilon1, epsilon2 = epsilon2
  )

  # Add X variables with appropriate names
  if (n_x == 1) {
    result_df$Xk <- X_mat[, 1]
    result_df$Z <- Z_mat[, 1]
  } else {
    for (j in 1:n_x) {
      result_df[[paste0("X", j)]] <- X_mat[, j]
    }
    for (j in 1:n_x) {
      result_df[[paste0("Z", j)]] <- Z_mat[, j]
    }
  }
  # nolint end

  result_df
}


#' Verify Lewbel's Key Identifying Assumptions
#'
#' Tests whether the data generating process satisfies the key assumptions
#' required for Lewbel's (2012) identification strategy. This includes testing
#' the covariance restriction and instrument relevance condition.
#'
#' @param data Data.frame. Optional. Pre-generated data to verify (alternative
#'   to n_obs/params).
#' @param config List. Optional. Configuration object (used with data
#'   parameter).
#' @param n_obs Integer. Sample size for verification (default: 10000, used
#'   with params).
#' @param params List. Parameters for the DGP (same format as
#'   generate_lewbel_data).
#' @param verbose Logical. Whether to print detailed output (default: TRUE).
#'
#' @details
#' The function tests:
#' \itemize{
#'   \item Assumption A2: Cov(Z, \eqn{\epsilon_1 \epsilon_2}) = 0 (covariance
#'     restriction)
#'   \item Assumption A3: Cov(Z, \eqn{\epsilon_2^2}) != 0 (instrument relevance)
#'   \item Endogeneity: Cov(\eqn{\epsilon_1}, \eqn{\epsilon_2}) != 0
#' }
#'
#' Can be called in two ways:
#' 1. verify_lewbel_assumptions(data, config) - using pre-generated data
#' 2. verify_lewbel_assumptions(n_obs = 10000, params = params) - generating
#'    new data
#'
#' @return Invisibly returns a list with verification results and data.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' params <- list(
#'   beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
#'   beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
#'   alpha1 = config$alpha1, alpha2 = config$alpha2,
#'   delta_het = config$delta_het
#' )
#' verify_lewbel_assumptions(params = params)
#' }
#'
#' @export
verify_lewbel_assumptions <- function(data = NULL, config = NULL,
                                      n_obs = 10000, params = NULL,
                                      verbose = TRUE) {
  if (verbose) {
    cat("\n--- Verifying Lewbel's Key Assumptions ---\n")
  }

  # Handle different calling patterns
  current_n_obs <- NULL
  if (!is.null(data) && !is.null(config)) {
    # Called with data and config (new pattern)
    test_data <- data
    current_n_obs <- nrow(data)
  } else if (!is.null(params)) {
    # Called with n_obs and params (original pattern)
    current_n_obs <- as.integer(n_obs) # Ensure n_obs is an integer
    test_data <- generate_lewbel_data(current_n_obs, params)
  } else {
    stop("Must provide either (data, config) or (n_obs, params)")
  }

  # Calculate sample moments
  cov_z_e1e2 <- stats::cov(test_data$Z, test_data$epsilon1 * test_data$epsilon2)
  cov_z_e2sq <- stats::cov(test_data$Z, test_data$epsilon2^2)
  cov_e1_e2 <- stats::cov(test_data$epsilon1, test_data$epsilon2)

  # Statistical test for Cov(Z, e1*e2) = 0 (always calculate)
  sd_z_e1e2 <- stats::sd(test_data$Z * test_data$epsilon1 * test_data$epsilon2)
  test_stat <- sqrt(as.numeric(current_n_obs)) * cov_z_e1e2 / sd_z_e1e2
  p_value <- 2 * (1 - stats::pnorm(abs(test_stat)))

  if (verbose) {
    cat(sprintf("Test sample size: %d\n", as.integer(current_n_obs)))
    cat(sprintf("Cov(Z, e1*e2) = %.6f (should be approx. 0)\n", cov_z_e1e2))
    cat(sprintf("Cov(Z, e2^2) = %.6f (should be != 0)\n", cov_z_e2sq))
    cat(sprintf(
      "Cov(e1, e2) = %.6f (should be != 0 for endogeneity)\n",
      cov_e1_e2
    ))
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
