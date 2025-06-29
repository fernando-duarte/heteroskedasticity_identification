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
#' The data generating process uses a unified approach for both single and multiple X:
#' \itemize{
#'   \item For each j: Z_raw_j ~ Uniform(0, 1) independently
#'   \item X_j = Z_raw_j
#'   \item Z_j = Z_raw_j - mean(Z_raw_j) (centered for use as instrument)
#'   \item For heteroskedasticity:
#'     \itemize{
#'       \item If n_x = 1: Z_het = Z_raw
#'       \item If n_x > 1: Z_het = mean(Z_raw_1, ..., Z_raw_k)
#'     }
#'   \item V_2|Z_het ~ N(0, 0.5 + 2*Z_het) (variance equals 0.5 + 2*Z_het)
#' }
#'
#' @return A data.frame with columns Y1, Y2, epsilon1, epsilon2, and:
#'   - If n_x = 1: Xk, Z
#'   - If n_x > 1: X1, X2, ..., Z1, Z2, ... (one Z per X)
#'
#' @template references-lewbel
#'
#' @seealso \code{\link{verify_lewbel_assumptions}} for testing the assumptions,
#'   \code{\link{run_single_lewbel_simulation}} for using this data in simulations
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
      stop(.hetid_const("messages$INVALID_N_X_PARAMS"))
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

  # New unified approach for both single and multiple X
  # Each X_j is drawn from independent Uniform(0,1) distributions
  # This extends the single X case naturally to multiple X

  # Generate raw Z values (which equal X values)
  Z_raw_mat <- matrix(
    stats::runif(n_obs * n_x, min = 0, max = 1),
    nrow = n_obs, ncol = n_x
  )

  # X = Z_raw (X equals the raw Z values)
  X_mat <- Z_raw_mat

  # Center Z to have mean 0 (these are the instruments)
  Z_mat <- matrix(NA, nrow = n_obs, ncol = n_x)
  for (j in 1:n_x) {
    Z_mat[, j] <- Z_raw_mat[, j] - mean(Z_raw_mat[, j])
  }

  # For heteroskedasticity, use average of raw Z values
  # This maintains the [0,1] range and combines information from all X's
  if (n_x == 1) {
    Z_het <- Z_raw_mat[, 1]
  } else {
    Z_het <- rowMeans(Z_raw_mat)
  }

  # Generate error components following Lewbel (2012)
  #
  # Key assumptions from Lewbel (2012):
  # 1. X is uncorrelated with (U, V1, V2)
  # 2. Z is uncorrelated with (U^2, U*V1, U*V2, V1*V2)
  # 3. Z is correlated with V2^2 (heteroskedasticity)
  # 4. For simultaneous equations, Z is also correlated with V1^2
  #
  # Following Example 1 from the literature:
  # - U and V1 are i.i.d. N(0,1) and independent of Z
  # - V2|Z ~ N(0, sigma^2(Z)) where sigma^2(Z) depends on Z
  # - This ensures E[V2|Z] = 0 but Var[V2|Z] depends on Z

  # Generate common factor U independent of Z
  U <- stats::rnorm(n_obs)

  # Generate V1 independent of Z and U
  V1 <- stats::rnorm(n_obs)

  # Generate V2 with heteroskedastic variance
  # V2|Z ~ N(0, 0.5 + 2*Z_het) - unified approach for all n_x
  # This ensures minimum variance of 0.5 and strong heteroskedasticity
  # V2 = nu2 * sqrt(0.5 + 2*Z_het) where nu2 ~ N(0,1) independent of Z
  nu2 <- stats::rnorm(n_obs)
  V2 <- nu2 * sqrt(0.5 + 2 * Z_het)

  # Construct structural errors
  epsilon1 <- params$alpha1 * U + V1
  epsilon2 <- params$alpha2 * U + V2

  # Verification of Lewbel's conditions:
  # 1. U, V1 are independent of Z by construction: cov(Z, U) = cov(Z, V1) = 0
  # 2. V2 has E[V2|Z] = 0, so E[V2] = 0
  # 3. cov(Z, V2^2) = cov(Z, sigma^2(Z)) > 0 (heteroskedasticity)
  #
  # The cross-product conditions:
  # - cov(Z, U^2): Since U is independent of Z, this equals 0
  # - cov(Z, U*V1): Since U and V1 are independent of Z, this equals 0
  # - cov(Z, U*V2) and cov(Z, V1*V2): These are theoretically 0 under
  #   conditional independence, but with nonlinear variance functions
  #   like exp(delta*Z), finite sample covariances may be non-zero.
  #
  # In practice, the method works well even with small deviations from
  # the exact moment conditions, especially in large samples.

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
#' @param params List. Parameters for the data generating process (same format as
#'   generate_lewbel_data).
#' @template param-verbose
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
#' @template references-lewbel
#'
#' @seealso \code{\link{generate_lewbel_data}} for generating data that meets assumptions
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
    stop(.hetid_const("messages$MISSING_DATA_OR_PARAMS"))
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

    if (p_value < .hetid_const("ALPHA_DEFAULT")) {
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

#' Generate Data for Rigobon (2003) Regime-Based Model
#'
#' Creates a dataset based on the triangular model with regime-specific
#' heteroskedasticity following Rigobon (2003). This is a special case of
#' Lewbel's method where heteroskedasticity drivers are discrete regime indicators.
#'
#' @param n_obs Integer. Sample size.
#' @param params List. Parameters for the data generating process containing:
#'   \itemize{
#'     \item beta1_0, beta1_1: Parameters for first equation
#'     \item beta2_0, beta2_1: Parameters for second equation
#'     \item gamma1: Endogenous parameter (key parameter of interest)
#'     \item alpha1, alpha2: Factor loadings for common factor U
#'     \item regime_probs: Vector of regime probabilities (must sum to 1)
#'     \item sigma2_regimes: Vector of variance multipliers for each regime
#'       (length must match regime_probs)
#'   }
#' @param n_x Integer. Number of exogenous X variables to generate (default: 1).
#'
#' @return A data.frame with columns Y1, Y2, epsilon1, epsilon2, regime, and:
#'   \itemize{
#'     \item If n_x = 1: Xk, plus Z1, Z2, ... (one centered dummy per regime)
#'     \item If n_x > 1: X1, X2, ..., plus Z1, Z2, ... (one centered dummy per regime)
#'   }
#'
#' @details
#' The triangular model is:
#' \deqn{Y_1 = \beta_{1,0} + \beta_{1,1}X + \gamma_1 Y_2 + \epsilon_1}
#' \deqn{Y_2 = \beta_{2,0} + \beta_{2,1}X + \epsilon_2}
#'
#' The error structure follows Rigobon's regime heteroskedasticity:
#' \deqn{\epsilon_1 = \alpha_1 U + V_1}
#' \deqn{\epsilon_2 = \alpha_2 U + V_2}
#'
#' where V_2 has variance that depends on the regime:
#' \deqn{Var(V_2|regime = s) = \sigma^2_{2,s}}
#'
#' @template references-rigobon
#'
#' @seealso \code{\link{generate_lewbel_data}} for continuous heteroskedasticity drivers
#'
#' @examples
#' \dontrun{
#' # Two-regime example (e.g., pre/post policy change)
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0,
#'   regime_probs = c(0.4, 0.6), # 40% in regime 1, 60% in regime 2
#'   sigma2_regimes = c(1.0, 2.5) # Variance is 2.5x higher in regime 2
#' )
#' data <- generate_rigobon_data(1000, params)
#'
#' # Three-regime example
#' params_3reg <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0,
#'   regime_probs = c(0.3, 0.4, 0.3),
#'   sigma2_regimes = c(0.5, 1.0, 2.0)
#' )
#' data_3reg <- generate_rigobon_data(1000, params_3reg)
#' }
#'
#' @export
generate_rigobon_data <- function(n_obs, params, n_x = 1) {
  # Validate regime parameters
  if (!("regime_probs" %in% names(params)) || !("sigma2_regimes" %in% names(params))) {
    stop(.hetid_const("messages$MISSING_REGIME_PARAMS"))
  }

  n_regimes <- length(params$regime_probs)
  if (length(params$sigma2_regimes) != n_regimes) {
    stop(.hetid_const("messages$REGIME_LENGTH_MISMATCH"))
  }

  if (abs(sum(params$regime_probs) - 1) > 1e-10) {
    stop(.hetid_const("messages$REGIME_PROBS_SUM"))
  }

  # Validate parameters for multiple X
  if (n_x > 1) {
    if (length(params$beta1_1) != n_x || length(params$beta2_1) != n_x) {
      stop(.hetid_const("messages$INVALID_N_X_PARAMS"))
    }
  } else {
    # Ensure scalar parameters are treated as length-1 vectors
    if (length(params$beta1_1) == 1) {
      params$beta1_1 <- as.numeric(params$beta1_1)
    }
    if (length(params$beta2_1) == 1) {
      params$beta2_1 <- as.numeric(params$beta2_1)
    }
  }

  # Generate regime assignments
  regime <- sample(seq_len(n_regimes), n_obs, replace = TRUE, prob = params$regime_probs)

  # Generate exogenous variables
  # nolint start: object_name_linter.
  X_mat <- matrix(
    stats::rnorm(
      n_obs * n_x,
      mean = .hetid_const("DEFAULT_X_MEAN"),
      sd = .hetid_const("DEFAULT_X_SD")
    ),
    nrow = n_obs, ncol = n_x
  )

  # Create regime dummy variables and center them
  Z_mat <- matrix(0, nrow = n_obs, ncol = n_regimes)
  for (s in seq_len(n_regimes)) {
    # Create dummy
    Z_mat[, s] <- as.numeric(regime == s)
    # Center it: Z_s = D_s - p_s
    Z_mat[, s] <- Z_mat[, s] - mean(Z_mat[, s])
  }

  # Generate mutually independent error components
  U <- stats::rnorm(n_obs)
  V1 <- stats::rnorm(n_obs)

  # Generate V2 with regime-specific variance
  V2 <- numeric(n_obs)
  for (s in seq_len(n_regimes)) {
    regime_mask <- regime == s
    n_regime <- sum(regime_mask)
    if (n_regime > 0) {
      V2[regime_mask] <- stats::rnorm(n_regime) * sqrt(params$sigma2_regimes[s])
    }
  }

  # Construct structural errors using single-factor model
  epsilon1 <- params$alpha1 * U + V1
  epsilon2 <- params$alpha2 * U + V2

  # Generate endogenous variables using all X variables
  Y2 <- params$beta2_0 + as.vector(X_mat %*% params$beta2_1) + epsilon2
  Y1 <- params$beta1_0 + as.vector(X_mat %*% params$beta1_1) +
    params$gamma1 * Y2 + epsilon1

  # Create output data frame
  result_df <- data.frame(
    Y1 = Y1, Y2 = Y2, epsilon1 = epsilon1, epsilon2 = epsilon2,
    regime = regime
  )

  # Add X variables with appropriate names
  if (n_x == 1) {
    result_df$Xk <- X_mat[, 1]
  } else {
    for (j in 1:n_x) {
      result_df[[paste0("X", j)]] <- X_mat[, j]
    }
  }

  # Add centered regime dummies as Z variables
  for (s in seq_len(n_regimes)) {
    result_df[[paste0("Z", s)]] <- Z_mat[, s]
  }
  # nolint end

  result_df
}
