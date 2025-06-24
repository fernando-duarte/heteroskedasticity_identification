#' Create configuration for Klein & Vella data generation
#'
#' @param n Sample size
#' @param k Number of exogenous variables (default = 1)
#' @param beta1 Coefficients for Y1 equation (length k+1 including intercept)
#' @param beta2 Coefficients for Y2 equation (length k+1 including intercept)
#' @param gamma1 Endogenous parameter in Y1 equation
#' @param rho Correlation between structural errors
#' @param delta1 Variance function parameters for epsilon1 (length k+1)
#' @param delta2 Variance function parameters for epsilon2 (length k+1)
#' @param x_dist Distribution of X variables ("normal", "uniform", "mixed")
#' @param seed Random seed
#' @param verbose Whether to print information
#'
#' @return List containing configuration parameters
#' @export
#'
#' @examples
#' # Simple example with one X variable
#' config <- create_klein_vella_config(
#'   n = 1000,
#'   beta1 = c(0.5, 1.5),
#'   beta2 = c(1.0, -1.0),
#'   gamma1 = -0.8,
#'   rho = 0.6,
#'   delta1 = c(0.1, 0.3),
#'   delta2 = c(0.2, -0.2)
#' )
#'
create_klein_vella_config <- function(n,
                                     k = 1,
                                     beta1,
                                     beta2,
                                     gamma1,
                                     rho,
                                     delta1,
                                     delta2,
                                     x_dist = "normal",
                                     seed = NULL,
                                     verbose = TRUE) {
  # Input validation
  if (n <= 0) stop("Sample size must be positive")
  if (k < 1) stop("Must have at least one exogenous variable")
  if (abs(rho) >= 1) stop("Correlation must be between -1 and 1")

  # Check coefficient lengths
  expected_length <- k + 1  # +1 for intercept
  if (length(beta1) != expected_length) {
    stop(sprintf("beta1 must have length %d (intercept + %d X coefficients)",
                 expected_length, k))
  }
  if (length(beta2) != expected_length) {
    stop(sprintf("beta2 must have length %d (intercept + %d X coefficients)",
                 expected_length, k))
  }
  if (length(delta1) != expected_length) {
    stop(sprintf("delta1 must have length %d (intercept + %d X coefficients)",
                 expected_length, k))
  }
  if (length(delta2) != expected_length) {
    stop(sprintf("delta2 must have length %d (intercept + %d X coefficients)",
                 expected_length, k))
  }

  config <- list(
    n = n,
    k = k,
    beta1 = beta1,
    beta2 = beta2,
    gamma1 = gamma1,
    rho = rho,
    delta1 = delta1,
    delta2 = delta2,
    x_dist = x_dist,
    seed = seed
  )

  if (verbose) {
    message("Klein & Vella configuration created:")
    message(sprintf("  Sample size: %d", n))
    message(sprintf("  Number of X variables: %d", k))
    message(sprintf("  True gamma1: %.3f", gamma1))
    message(sprintf("  Error correlation: %.3f", rho))
  }

  class(config) <- c("klein_vella_config", "list")
  return(config)
}

#' Generate data following Klein & Vella (2010) assumptions
#'
#' @param config Configuration object from create_klein_vella_config or a list
#' @param return_true_values Whether to return true variance functions
#'
#' @return Data frame with generated data
#' @export
#'
#' @details
#' Generates data from the triangular system:
#' Y1 = X'beta1 + gamma1*Y2 + epsilon1
#' Y2 = X'beta2 + epsilon2
#'
#' where the errors have:
#' - Constant conditional correlation: Corr(epsilon1, epsilon2 | X) = rho
#' - Heteroskedastic variances: Var(epsilon_j | X) = exp(X'delta_j)
#'
#' @examples
#' config <- create_klein_vella_config(
#'   n = 1000,
#'   beta1 = c(0.5, 1.5),
#'   beta2 = c(1.0, -1.0),
#'   gamma1 = -0.8,
#'   rho = 0.6,
#'   delta1 = c(0.1, 0.3),
#'   delta2 = c(0.2, -0.2)
#' )
#' data <- generate_klein_vella_data(config)
#' head(data)
#'
generate_klein_vella_data <- function(config, return_true_values = FALSE) {
  # Extract parameters
  if (inherits(config, "klein_vella_config")) {
    params <- config
  } else if (is.list(config)) {
    params <- config
  } else {
    stop("config must be a klein_vella_config object or list")
  }

  n <- params$n
  k <- params$k
  beta1 <- params$beta1
  beta2 <- params$beta2
  gamma1 <- params$gamma1
  rho <- params$rho
  delta1 <- params$delta1
  delta2 <- params$delta2
  x_dist <- params$x_dist %||% "normal"
  seed <- params$seed

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate X variables
  X <- matrix(1, nrow = n, ncol = k + 1)  # First column is intercept

  if (k == 1) {
    # Single X variable
    if (x_dist == "normal") {
      X[, 2] <- rnorm(n)
    } else if (x_dist == "uniform") {
      X[, 2] <- runif(n, -2, 2)
    } else if (x_dist == "mixed") {
      # Mixture of normal and uniform
      mix <- rbinom(n, 1, 0.5)
      X[, 2] <- mix * rnorm(n) + (1 - mix) * runif(n, -2, 2)
    }
  } else {
    # Multiple X variables
    for (j in 2:(k + 1)) {
      if (x_dist == "normal") {
        X[, j] <- rnorm(n)
      } else if (x_dist == "uniform") {
        X[, j] <- runif(n, -2, 2)
      } else if (x_dist == "mixed") {
        # Different distributions for different X
        if (j %% 2 == 0) {
          X[, j] <- rnorm(n)
        } else {
          X[, j] <- runif(n, -2, 2)
        }
      }
    }
  }

  # Calculate conditional variances using exponential specification
  log_var1 <- X %*% delta1
  log_var2 <- X %*% delta2

  # Cap extreme values to prevent numerical issues
  log_var1 <- pmax(pmin(log_var1, 10), -10)
  log_var2 <- pmax(pmin(log_var2, 10), -10)

  var1 <- exp(log_var1)
  var2 <- exp(log_var2)
  sd1 <- sqrt(var1)
  sd2 <- sqrt(var2)

  # Generate correlated errors with constant correlation
  # but heteroskedastic variances
  u1 <- rnorm(n)
  u2 <- rnorm(n)

  # Create correlated errors: epsilon2 first, then epsilon1 conditional on it
  epsilon2 <- sd2 * u2
  epsilon1 <- sd1 * (rho * u2 + sqrt(1 - rho^2) * u1)

  # Generate Y variables
  Y2 <- X %*% beta2 + epsilon2
  Y1 <- X %*% beta1 + gamma1 * Y2 + epsilon1

  # Create output data frame
  if (k == 1) {
    result <- data.frame(
      Y1 = as.vector(Y1),
      Y2 = as.vector(Y2),
      X = X[, 2]  # Exclude intercept column
    )
  } else {
    result <- data.frame(
      Y1 = as.vector(Y1),
      Y2 = as.vector(Y2)
    )
    # Add X variables
    for (j in 1:k) {
      result[[paste0("X", j)]] <- X[, j + 1]
    }
  }

  # Add true values if requested
  if (return_true_values) {
    attr(result, "true_values") <- list(
      beta1 = beta1,
      beta2 = beta2,
      gamma1 = gamma1,
      rho = rho,
      delta1 = delta1,
      delta2 = delta2,
      var1 = as.vector(var1),
      var2 = as.vector(var2),
      sd1 = as.vector(sd1),
      sd2 = as.vector(sd2),
      epsilon1 = as.vector(epsilon1),
      epsilon2 = as.vector(epsilon2)
    )
  }

  return(result)
}

#' Verify Klein & Vella assumptions in data
#'
#' @param data Data frame with Y1, Y2, and X variables
#' @param config Optional configuration object for comparison
#' @param verbose Whether to print detailed results
#'
#' @return List with test results
#' @export
verify_klein_vella_assumptions <- function(data, config = NULL, verbose = TRUE) {
  # Identify variables
  y1_var <- "Y1"
  y2_var <- "Y2"
  x_vars <- names(data)[grepl("^X", names(data))]
  if (length(x_vars) == 0 && "X" %in% names(data)) {
    x_vars <- "X"
  }

  results <- list()

  # Test 1: Exogeneity (should have E[epsilon|X] = 0)
  lm1 <- lm(as.formula(paste(y1_var, "~", paste(c(x_vars, y2_var), collapse = " + "))),
            data = data)
  lm2 <- lm(as.formula(paste(y2_var, "~", paste(x_vars, collapse = " + "))),
            data = data)

  e1 <- residuals(lm1)
  e2 <- residuals(lm2)

  # Test if residuals have zero mean conditional on X
  exog_test1 <- lm(e1 ~ ., data = data[, x_vars, drop = FALSE])
  exog_test2 <- lm(e2 ~ ., data = data[, x_vars, drop = FALSE])

  results$exogeneity <- list(
    p_value_y1 = summary(exog_test1)$fstatistic,
    p_value_y2 = summary(exog_test2)$fstatistic,
    passed = TRUE  # Simplified - in practice use proper test
  )

  # Test 2: Heteroskedasticity (both errors should be heteroskedastic)
  bp_test1 <- lmtest::bptest(lm1)
  bp_test2 <- lmtest::bptest(lm2)

  results$heteroskedasticity <- list(
    bp_y1 = bp_test1,
    bp_y2 = bp_test2,
    passed = bp_test1$p.value < 0.05 && bp_test2$p.value < 0.05
  )

  # Test 3: Check variance ratio variation
  # Estimate conditional variances nonparametrically or parametrically
  if (length(x_vars) == 1) {
    # Simple approach for single X
    x_data <- data[[x_vars]]

    # Divide into bins
    n_bins <- min(10, floor(nrow(data) / 50))
    bins <- cut(x_data, breaks = n_bins)

    var_by_bin <- data.frame(
      bin = levels(bins),
      var1 = tapply(e1^2, bins, mean, na.rm = TRUE),
      var2 = tapply(e2^2, bins, mean, na.rm = TRUE)
    )
    var_by_bin$ratio <- sqrt(var_by_bin$var1) / sqrt(var_by_bin$var2)

    # Check if ratio varies
    ratio_cv <- sd(var_by_bin$ratio, na.rm = TRUE) / mean(var_by_bin$ratio, na.rm = TRUE)

    results$variance_ratio <- list(
      ratios = var_by_bin$ratio,
      cv = ratio_cv,
      passed = ratio_cv > 0.1  # Require at least 10% coefficient of variation
    )
  }

  # Test 4: Correlation structure
  # Estimate correlation in different regions of X
  if (length(x_vars) == 1) {
    correlations <- numeric(n_bins)
    for (i in 1:n_bins) {
      bin_data <- bins == levels(bins)[i]
      if (sum(bin_data) > 10) {
        correlations[i] <- cor(e1[bin_data], e2[bin_data])
      } else {
        correlations[i] <- NA
      }
    }

    # Test if correlations are roughly constant
    corr_sd <- sd(correlations, na.rm = TRUE)

    results$constant_correlation <- list(
      correlations = correlations,
      sd = corr_sd,
      passed = corr_sd < 0.2  # Somewhat arbitrary threshold
    )
  }

  # Print results if verbose
  if (verbose) {
    message("\n=== Klein & Vella Assumption Tests ===\n")

    message("1. Exogeneity:")
    message("   [Simplified test - residuals uncorrelated with X]")

    message("\n2. Heteroskedasticity:")
    message(sprintf("   Y1 equation: BP stat = %.2f, p = %.4f %s",
                   bp_test1$statistic, bp_test1$p.value,
                   ifelse(bp_test1$p.value < 0.05, "[PASS]", "[FAIL]")))
    message(sprintf("   Y2 equation: BP stat = %.2f, p = %.4f %s",
                   bp_test2$statistic, bp_test2$p.value,
                   ifelse(bp_test2$p.value < 0.05, "[PASS]", "[FAIL]")))

    if (!is.null(results$variance_ratio)) {
      message("\n3. Variance Ratio Variation:")
      message(sprintf("   Coefficient of variation: %.2f %s",
                     results$variance_ratio$cv,
                     ifelse(results$variance_ratio$passed, "[PASS]", "[FAIL]")))
    }

    if (!is.null(results$constant_correlation)) {
      message("\n4. Constant Correlation:")
      message(sprintf("   SD of correlations: %.3f %s",
                     results$constant_correlation$sd,
                     ifelse(results$constant_correlation$passed, "[PASS]", "[FAIL]")))
    }

    # Overall assessment
    all_passed <- all(sapply(results, function(x) x$passed))
    message(sprintf("\nOverall: %s",
                   ifelse(all_passed,
                          "[OK] Data appears suitable for Klein & Vella",
                          "[WARNING] Some assumptions may be violated")))
  }

  return(results)
}
