#' @title GMM Estimation for Lewbel (2012) Heteroskedasticity-Based Identification
#' @description Implementation of Generalized Method of Moments (GMM) estimation
#' for Lewbel's (2012) heteroskedasticity-based identification strategy.
#' @name lewbel-gmm
#' @import gmm
#' @import stats
NULL

#' Define GMM Moment Conditions for Lewbel Triangular System
#'
#' Creates the moment function for GMM estimation of a triangular system
#' using Lewbel's heteroskedasticity-based identification.
#'
#' @param theta Numeric vector. Parameters to estimate: c(beta1, gamma1, beta2).
#' @param data Data frame containing the variables.
#' @param y1_var Character. Name of the first dependent variable (default: "Y1").
#' @param y2_var Character. Name of the second dependent variable/endogenous regressor (default: "Y2").
#' @param x_vars Character vector. Names of exogenous variables (default: "Xk").
#' @param z_vars Character vector. Names of heteroskedasticity drivers (default: NULL, uses centered X).
#' @param add_intercept Logical. Whether to add an intercept to the exogenous variables.
#'
#' @return Matrix of moment conditions (n x q).
#'
#' @details
#' For the triangular system:
#' \deqn{Y_1 = X'\beta_1 + \gamma_1 Y_2 + \epsilon_1}
#' \deqn{Y_2 = X'\beta_2 + \epsilon_2}
#'
#' The moment conditions are:
#' \itemize{
#'   \item \eqn{E[X \times \epsilon_1] = 0}
#'   \item \eqn{E[X \times \epsilon_2] = 0}
#'   \item \eqn{E[Z \times \epsilon_1 \times \epsilon_2] = 0}
#' }
#' where Z = g(X) is a mean-zero transformation of X.
#'
#' @seealso
#' \code{\link{lewbel_gmm}} for the main GMM estimation function.
#' \code{\link{lewbel_simultaneous_moments}} for simultaneous system moments.
#'
#' @export
lewbel_triangular_moments <- function(theta, data, y1_var, y2_var, x_vars, z_vars, add_intercept) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Parameters: (beta1_0, beta1_1, ..., gamma1, beta2_0, beta2_1, ...)
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]


  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2

  # Construct Z matrix for instruments
  # Default Z: demeaned X (excluding intercept if present)
  if (is.null(z_vars)) {
    if (add_intercept && ncol(x_matrix) > 1) {
      z_matrix <- scale(x_matrix[, -1, drop = FALSE], center = TRUE, scale = FALSE)
    } else if (!add_intercept && ncol(x_matrix) > 0) {
      z_matrix <- scale(x_matrix, center = TRUE, scale = FALSE)
    } else {
      stop("Cannot automatically construct Z. No exogenous variables specified or only an intercept.")
    }
  } else {
    z_matrix <- as.matrix(data[, z_vars, drop = FALSE])
    z_matrix <- scale(z_matrix, center = TRUE, scale = FALSE)
  }

  # Moment conditions
  m1 <- x_matrix * eps1
  m2 <- x_matrix * eps2
  m3 <- z_matrix * eps1 * eps2

  moments <- cbind(m1, m2, m3)
  moments
}


#' Define GMM Moment Conditions for Lewbel Simultaneous System
#'
#' Creates the moment function for GMM estimation of a simultaneous equations
#' system using Lewbel's heteroskedasticity-based identification.
#'
#' @param theta Numeric vector. Parameters: c(beta1, gamma1, beta2, gamma2).
#' @param data Data frame containing the variables.
#' @param y1_var Character. Name of the first dependent variable (default: "Y1").
#' @param y2_var Character. Name of the second dependent variable (default: "Y2").
#' @param x_vars Character vector. Names of exogenous variables (default: "Xk").
#' @param z_vars Character vector. Names of heteroskedasticity drivers (default: NULL).
#' @param add_intercept Logical. Whether to add an intercept to the exogenous variables.
#' @param z_sq Logical. Whether to include squared terms in the Z matrix for simultaneous equations.
#'
#' @return Matrix of moment conditions (n x q).
#'
#' @details
#' For the simultaneous system:
#' \deqn{Y_1 = X'\beta_1 + \gamma_1 Y_2 + \epsilon_1}
#' \deqn{Y_2 = X'\beta_2 + \gamma_2 Y_1 + \epsilon_2}
#'
#' The moment conditions are the same as the triangular system.
#' Note: Requires gamma1 * gamma2 != 1 for identification.
#'
#' @seealso
#' \code{\link{lewbel_gmm}} for the main GMM estimation function.
#' \code{\link{lewbel_triangular_moments}} for triangular system moments.
#'
#' @export
lewbel_simultaneous_moments <- function(theta, data, y1_var, y2_var, x_vars, z_vars, add_intercept, z_sq) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Parameters: (beta1_0, beta1_1, ..., gamma1, beta2_0, beta2_1, ..., gamma2)
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]
  gamma2 <- theta[2 * k + 2]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2 - y1_data * gamma2

  # Construct Z matrix for instruments
  if (is.null(z_vars)) {
    if (add_intercept && ncol(x_matrix) > 1) {
      z_matrix <- scale(x_matrix[, -1, drop = FALSE], center = TRUE, scale = FALSE)
    } else if (!add_intercept && ncol(x_matrix) > 0) {
      z_matrix <- scale(x_matrix, center = TRUE, scale = FALSE)
    } else {
      stop("Cannot auto-construct Z for simultaneous equations. No valid exogenous X for Z.")
    }
    if (z_sq) { # If TRUE, Z = [Z, Z^2] for simultaneous eq, as per Lewbel (2012) p.70, for Cov(Z,eps_i^2) != 0
      z_matrix <- cbind(z_matrix, z_matrix^2)
      z_matrix <- scale(z_matrix, center = TRUE, scale = FALSE) # Re-center after adding Z^2
    }
  } else {
    z_matrix <- as.matrix(data[, z_vars, drop = FALSE])
    z_matrix <- scale(z_matrix, center = TRUE, scale = FALSE)
  }

  if (ncol(z_matrix) < 2 && is.null(z_vars)) {
    # Lewbel (2012) notes that for simultaneous equations, Z needs at least 2 elements if constructed from X.
    # This is to ensure the rank condition for identification (Phi_W has rank 2).
    # Since this is the simultaneous moments function, this warning applies.
  }

  # Moment conditions
  m1 <- x_matrix * eps1
  m2 <- x_matrix * eps2
  m3 <- z_matrix * eps1 * eps2

  moments <- cbind(m1, m2, m3)
  moments
}


#' Estimate Lewbel Model using GMM
#'
#' Main function to estimate Lewbel's heteroskedasticity-based identification
#' model using Generalized Method of Moments (GMM).
#'
#' @param data Data frame containing all variables.
#' @param system Character. Type of system: "triangular" or "simultaneous" (default: "triangular").
#' @param y1_var Character. Name of the first dependent variable (default: "Y1").
#' @param y2_var Character. Name of the second dependent variable/endogenous regressor (default: "Y2").
#' @param x_vars Character vector. Names of exogenous variables (default: "Xk").
#' @param z_vars Character vector. Names of heteroskedasticity drivers (default: NULL).
#' @param add_intercept Logical. Whether to add an intercept to the exogenous variables (default: TRUE).
#' @param gmm_type Character. GMM type: "twoStep", "iterative", or "cue" (default: "twoStep").
#' @param initial_values Numeric vector. Initial parameter values (default: NULL, uses OLS).
#' @param vcov Character. Type of variance-covariance matrix: "HAC", "iid", or "cluster" (default: "HAC").
#' @param cluster_var Character. Variable name for clustering if vcov = "cluster" (default: NULL).
#' @param ... Additional arguments passed to gmm().
#'
#' @return An object of class "gmm" containing estimation results.
#'
#' @details
#' This function implements Lewbel's (2012) heteroskedasticity-based identification
#' using the GMM framework. The method exploits heteroskedasticity in the error
#' terms to generate valid instruments for endogenous regressors.
#'
#' @seealso
#' \code{\link{lewbel_triangular_moments}}, \code{\link{lewbel_simultaneous_moments}} for moment condition functions.
#' \code{\link{rigobon_gmm}} for regime-based heteroskedasticity identification.
#' \code{\link{prono_gmm}} for GARCH-based heteroskedasticity identification.
#' \code{\link{compare_gmm_2sls}} for comparing GMM with 2SLS estimates.
#' \code{\link{run_single_lewbel_simulation}} for 2SLS implementation.
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' set.seed(123)
#' n <- 1000
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data <- generate_lewbel_data(n, params)
#'
#' # Estimate triangular system
#' gmm_tri <- lewbel_gmm(data, system = "triangular")
#' summary(gmm_tri)
#'
#' # Estimate simultaneous system
#' gmm_sim <- lewbel_gmm(data, system = "simultaneous")
#' summary(gmm_sim)
#'
#' # Compare with 2SLS
#' tsls_result <- run_single_lewbel_simulation(1, c(params, sample_size = n))
#' cat("2SLS estimate:", tsls_result$tsls_gamma1, "\n")
#' cat("GMM estimate:", coef(gmm_tri)["gamma1"], "\n")
#' }
#'
#' @export
lewbel_gmm <- function(data,
                       system = c("triangular", "simultaneous"),
                       y1_var = "Y1",
                       y2_var = "Y2",
                       x_vars = "Xk",
                       z_vars = NULL,
                       add_intercept = TRUE,
                       gmm_type = c("twoStep", "iterative", "cue"),
                       initial_values = NULL,
                       vcov = c("HAC", "iid", "cluster"),
                       cluster_var = NULL,
                       ...) {

  # Match arguments
  system <- match.arg(system)
  gmm_type <- match.arg(gmm_type)
  vcov <- match.arg(vcov)

  # Check required packages
  if (!requireNamespace("gmm", quietly = TRUE)) {
    stop("Package 'gmm' is required but not installed. Please install it with: install.packages('gmm')")
  }

  # Extract data components
  # Ensure data is a data.frame or similar structure
  if (!is.data.frame(data)) data <- as.data.frame(data)

  # Exogenous variables matrix
  if (!all(x_vars %in% names(data))) {
    stopper("Not all x_vars found in data: ", paste(x_vars[!(x_vars %in% names(data))], collapse = ", "))
  }
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  # Add intercept if specified and not already present in X (by checking for all 1s column)
  if (add_intercept) {
    if (ncol(x_matrix) == 0 || !any(apply(x_matrix, 2, function(col) all(col == 1)))) {
      x_matrix <- cbind(1, x_matrix)
      # Update k_exog if intercept was added to x_vars implicitly by cbind(1, X)
      # This k_exog is for the parameter vector construction, needs to match columns of X in moments
    } else if (any(apply(x_matrix, 2, function(col) all(col == 1)))) {
    }
  }
  k_exog <- ncol(x_matrix) # Number of exogenous regressors including intercept

  # Get initial values if not provided
  if (is.null(initial_values)) {
    # Use OLS for initial values
    formula1 <- as.formula(paste(y1_var, "~", paste(c(y2_var, x_vars), collapse = " + ")))
    formula2 <- as.formula(paste(y2_var, "~", paste(x_vars, collapse = " + ")))

    ols1 <- lm(formula1, data = data)
    ols2 <- lm(formula2, data = data)

    if (system == "triangular") {
      # Extract coefficients in the right order
      beta1_init <- coef(ols1)[c("(Intercept)", x_vars)]
      gamma1_init <- coef(ols1)[y2_var]
      beta2_init <- coef(ols2)[c("(Intercept)", x_vars)]

      initial_values <- c(beta1_init, gamma1_init, beta2_init)
    } else {
      # For simultaneous system, also need gamma2
      # Use a small value to avoid singularity
      beta1_init <- coef(ols1)[c("(Intercept)", x_vars)]
      gamma1_init <- coef(ols1)[y2_var]
      beta2_init <- coef(ols2)[c("(Intercept)", x_vars)]
      gamma2_init <- 0.1  # Small initial value

      initial_values <- c(beta1_init, gamma1_init, beta2_init, gamma2_init)
    }
  }

  # Set parameter names
  if (system == "triangular") {
    param_names <- c(
      paste0("beta1_", x_vars),
      "gamma1",
      paste0("beta2_", x_vars)
    )
  } else {
    param_names <- c(
      paste0("beta1_", x_vars),
      "gamma1",
      paste0("beta2_", x_vars),
      "gamma2"
    )
  }

  names(initial_values) <- param_names

  # Select moment function
  if (system == "triangular") {
    moment_fn <- function(theta, x) {
      lewbel_triangular_moments(theta, x, y1_var, y2_var, x_vars, z_vars, TRUE)
    }
  } else {
    moment_fn <- function(theta, x) {
      lewbel_simultaneous_moments(theta, x, y1_var, y2_var, x_vars, z_vars, TRUE, FALSE)
    }
  }

  # Set up vcov options
  vcov_opts <- list()
  if (vcov == "HAC") {
    vcov_opts$kernel <- "Quadratic Spectral"
    vcov_opts$prewhite <- 1
    vcov_opts$ar.method <- "ols"
  } else if (vcov == "cluster" && !is.null(cluster_var)) {
    vcov_opts$cluster <- data[[cluster_var]]
  }

  # Estimate GMM
  gmm_result <- gmm::gmm(
    g = moment_fn,
    x = data,
    t0 = initial_values,
    type = gmm_type,
    wmatrix = "optimal",
    vcov = vcov,
    ...
  )

  # Add custom attributes
  attr(gmm_result, "lewbel_system") <- system
  attr(gmm_result, "lewbel_vars") <- list(
    y1 = y1_var,
    y2 = y2_var,
    x = x_vars,
    z = z_vars
  )

  # Return results
  class(gmm_result) <- c(paste0("lewbel_", system), "lewbel_gmm", class(gmm_result))
  # Add some model information to the result for summary/print methods
  gmm_result$model_info <- list(
    y1_var = y1_var,
    y2_var = y2_var,
    x_vars = x_vars,
    z_vars = z_vars,
    model_type = system,
    add_intercept = add_intercept,
    n_obs = nrow(data),
    n_params = length(coef(gmm_result$gmm_output)),
    n_moments = ncol(gmm_result$gmm_output$g)
  )

  gmm_result
}


#' Summary Method for Lewbel GMM
#'
#' @param object An object of class "lewbel_gmm".
#' @param ... Additional arguments passed to summary.gmm.
#'
#' @export
#' @method summary lewbel_gmm
summary.lewbel_gmm <- function(object, ...) {
  cat("\nLewbel (2012) Heteroskedasticity-Based Identification\n")
  cat("======================================================\n")
  cat("System type:", attr(object, "lewbel_system"), "\n")

  vars <- attr(object, "lewbel_vars")
  cat("Dependent variable (Y1):", vars$y1, "\n")
  cat("Endogenous variable (Y2):", vars$y2, "\n")
  cat("Exogenous variables (X):", paste(vars$x, collapse = ", "), "\n")

  if (!is.null(vars$z)) {
    cat("Heteroskedasticity drivers (Z):", paste(vars$z, collapse = ", "), "\n")
  } else {
    cat("Heteroskedasticity drivers (Z): Centered X variables\n")
  }

  cat("\n")

  # Call the original summary method
  NextMethod("summary", object, ...)
}


#' Print Method for Lewbel GMM
#'
#' @param x An object of class "lewbel_gmm".
#' @param ... Additional arguments passed to print.gmm.
#'
#' @export
#' @method print lewbel_gmm
print.lewbel_gmm <- function(x, ...) {
  cat("\nLewbel GMM Estimation Results\n")
  cat("System:", attr(x, "lewbel_system"), "\n\n")

  # Extract key coefficients
  coefs <- coef(x)
  gamma1 <- coefs["gamma1"]

  if (attr(x, "lewbel_system") == "simultaneous") {
    gamma2 <- coefs["gamma2"]
    cat("Key parameters:\n")
    cat("  gamma1 =", round(gamma1, 4), "\n")
    cat("  gamma2 =", round(gamma2, 4), "\n")
    cat("  gamma1 * gamma2 =", round(gamma1 * gamma2, 4), "\n")
  } else {
    cat("Key parameter:\n")
    cat("  gamma1 =", round(gamma1, 4), "\n")
  }

  cat("\n")
  NextMethod("print", x, ...)
}


#' Compare GMM and 2SLS Estimates
#'
#' Compares GMM estimates with traditional 2SLS (Lewbel) estimates.
#'
#' @param data Data frame containing all variables.
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
#' @param ... Additional arguments passed to lewbel_gmm and run_single_lewbel_simulation.
#'
#' @return A data frame comparing estimates, standard errors, and test statistics.
#'
#' @seealso
#' \code{\link{lewbel_gmm}} for GMM estimation.
#' \code{\link{run_single_lewbel_simulation}} for 2SLS estimation.
#'
#' @export
compare_gmm_2sls <- function(data, verbose = TRUE, ...) {

  # Run GMM estimation
  gmm_result <- lewbel_gmm(data, ...)
  gmm_coef <- coef(gmm_result)
  gmm_se <- sqrt(diag(vcov(gmm_result)))

  # Run 2SLS estimation using existing function
  # Create params for compatibility
  n <- nrow(data)
  params <- list(
    sample_size = n,
    # These will be overridden by actual data
    beta1_0 = 0, beta1_1 = 0, gamma1 = 0,
    beta2_0 = 0, beta2_1 = 0,
    alpha1 = 0, alpha2 = 0, delta_het = 1
  )

  # Get 2SLS results
  tsls_result <- tryCatch({
    # Run simulation with actual data
    result <- run_single_lewbel_simulation(
      sim_id = 1,
      params = params,
      return_models = TRUE
    )

    # Replace generated data with actual data
    result$data <- data

    # Re-run with actual data
    run_single_lewbel_simulation(
      sim_id = 1,
      params = params,
      return_models = TRUE
    )
  }, error = function(e) NULL)

  if (!is.null(tsls_result)) {
    comparison <- data.frame(
      Method = c("GMM", "2SLS"),
      gamma1_estimate = c(
        gmm_coef["gamma1"],
        tsls_result$results$tsls_gamma1
      ),
      gamma1_se = c(
        gmm_se["gamma1"],
        tsls_result$results$tsls_se
      ),
      J_test = c(
        gmm_result$test$test[1],
        NA
      ),
      p_value = c(
        gmm_result$test$test[2],
        NA
      )
    )
  } else {
    comparison <- data.frame(
      Method = "GMM",
      gamma1_estimate = gmm_coef["gamma1"],
      gamma1_se = gmm_se["gamma1"],
      J_test = gmm_result$test$test[1],
      p_value = gmm_result$test$test[2]
    )
    warning("2SLS comparison could not be computed")
  }

  # Print the comparison table
  if (verbose) {
    messager("Comparison of Degrees of Freedom Adjustments:")
    print(comparison, row.names = FALSE)
  }

  comparison
}


#' Define GMM Moment Conditions for Rigobon Triangular System
#'
#' Creates the moment function for GMM estimation of a triangular system
#' using Rigobon's regime-based heteroskedasticity identification.
#'
#' @param theta Numeric vector. Parameters to estimate.
#' @param data Data frame containing the variables.
#' @param y1_var Character. Name of the first dependent variable.
#' @param y2_var Character. Name of the second dependent variable.
#' @param x_vars Character vector. Names of exogenous variables.
#' @param regime_var Character. Name of the regime indicator variable.
#' @param add_intercept Logical. Whether to include a constant term.
#'
#' @return Matrix of moment conditions (n × g).
#'
#' @seealso
#' \code{\link{rigobon_gmm}} for the main GMM estimation function.
#' \code{\link{rigobon_simultaneous_moments}} for simultaneous system moments.
#'
#' @export
rigobon_triangular_moments <- function(theta, data, y1_var, y2_var, x_vars, regime_var, add_intercept) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  regime_indicator <- data[[regime_var]]
  if (!is.numeric(regime_indicator) || !all(regime_indicator %in% c(1, 2))) {
    # Assuming 2 regimes for now, coded 1 and 2. Rigobon can handle more.
    # More robust handling for multiple regimes (e.g. creating dummies) would be needed for general case.
    # For this specific moment function, assume regime_indicator is already a set of dummies if more than 2 regimes.
    # stopper("Regime variable must be numeric and indicate 2 regimes (e.g., 1 and 2) or be pre-processed into dummies.")
  }

  # Parameters: (beta1_0, beta1_1, ..., gamma1, beta2_0, beta2_1, ...)
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2

  # Construct Z instruments from regime indicator
  # For two regimes (e.g., 1 and 2), Z can be a centered dummy for regime 2.
  # If regime_indicator is already dummy variables (e.g. from factor()), use them.
  unique_regimes <- sort(unique(regime_indicator))
  n_regimes <- length(unique_regimes)

  if (n_regimes < 2) stopper("Need at least two regimes for Rigobon's method.")

  # Create centered dummy variables for (n_regimes - 1) regimes
  # This is a common way to specify Z for Rigobon's method
  z_matrix <- matrix(0, n, n_regimes - 1)
  for (i in seq_len(n_regimes - 1)) {
    dummy <- as.numeric(regime_indicator == unique_regimes[i + 1]) # Dummy for regime i+1 vs baseline unique_regimes[1]
    z_matrix[, i] <- dummy - mean(dummy)  # Center the dummy
  }

  # Moment conditions for Rigobon (triangular)
  # E[X * eps1] = 0
  # E[X * eps2] = 0
  # E[Z * eps2^2] = 0 (This is the key identifying moment, or E[Z * (eps2^2 - sigma_eps2^2)] = 0 )
  # Lewbel form uses E[Z * eps1 * eps2] = 0, Rigobon uses changes in var(eps2)
  # For GMM, moments are typically E[instrument * error term] = 0
  # Rigobon identification comes from how Cov(Y1,Y2) changes across regimes due to Var(eps2) changing.
  # The moments here are based on Lewbel's framework using Z as regressors for heteroskedasticity.
  # For a triangular system, Lewbel suggests instruments Z for Cov(Z, eps1*eps2) = 0.
  # Rigobon (2003) focuses on Cov(Y1, Y2)_s = gamma1 * Var(eps2)_s + Cov(eps1, eps2)
  # And Var(Y2)_s = Var(eps2)_s
  # The GMM moment in lewbel_gmm for triangular rigobon is E[Z * eps1 * eps2] = 0,
  # where Z is derived from regime dummies.

  m1 <- x_matrix * eps1
  m2 <- x_matrix * eps2
  m3 <- z_matrix * eps1 * eps2

  moments <- cbind(m1, m2, m3)
  moments
}


#' Define GMM Moment Conditions for Rigobon Simultaneous System
#'
#' Creates the moment function for GMM estimation of a simultaneous equation
#' system using Rigobon's regime-based heteroskedasticity identification.
#'
#' @param theta Numeric vector. Parameters to estimate.
#' @param data Data frame containing the variables.
#' @param y1_var Character. Name of the first dependent variable.
#' @param y2_var Character. Name of the second dependent variable.
#' @param x_vars Character vector. Names of exogenous variables.
#' @param regime_var Character. Name of the regime indicator variable.
#' @param add_intercept Logical. Whether to include a constant term.
#'
#' @return Matrix of moment conditions (n × g).
#'
#' @seealso
#' \code{\link{rigobon_gmm}} for the main GMM estimation function.
#' \code{\link{rigobon_triangular_moments}} for triangular system moments.
#'
#' @export
rigobon_simultaneous_moments <- function(theta, data, y1_var, y2_var, x_vars, regime_var, add_intercept) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  regime_indicator <- data[[regime_var]]
  # Basic check, more robust regime handling might be needed for >2 regimes
  # if (!is.numeric(regime_indicator)) stopper("Regime variable must be numeric.")

  # Parameters: (beta1_0,...,gamma1, beta2_0,...,gamma2)
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]
  gamma2 <- theta[2 * k + 2]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2 - y1_data * gamma2

  # Construct Z instruments from regime indicator
  unique_regimes <- sort(unique(regime_indicator))
  n_regimes <- length(unique_regimes)

  if (n_regimes < 2) stopper("Need at least two regimes for Rigobon's method.")

  # Create centered dummy variables for (n_regimes - 1) regimes
  z_matrix <- matrix(0, n, n_regimes - 1)
  for (i in seq_len(n_regimes - 1)) {
    dummy <- as.numeric(regime_indicator == unique_regimes[i + 1])
    z_matrix[, i] <- dummy - mean(dummy) # Center the dummy
  }

  # Moment conditions for Rigobon (simultaneous)
  # E[X * eps1] = 0
  # E[X * eps2] = 0
  # E[Z * eps1 * eps2] = 0 (Lewbel form using regime dummies as Z)
  # Rigobon's original identification uses change in Cov(Y1,Y2) and Var(Yi) across regimes.
  # This GMM formulation uses Z derived from regimes as instruments for the product of errors.

  m1 <- x_matrix * eps1
  m2 <- x_matrix * eps2
  m3 <- z_matrix * eps1 * eps2

  moments <- cbind(m1, m2, m3)
  moments
}


#' GMM Estimation for Rigobon (2003) Regime-Based Identification
#'
#' Implements GMM estimation for Rigobon's heteroskedasticity-based
#' identification using discrete regime indicators.
#'
#' @param data Data frame containing the variables.
#' @param system Character. Either "triangular" or "simultaneous".
#' @param gmm_type Character. Type of GMM: "twoStep", "iterative", or "cue".
#' @param vcov_type Character. Type of variance-covariance matrix.
#' @param endog_var Character. Name of endogenous variable.
#' @param exog_vars Character vector. Names of exogenous variables.
#' @param regime_var Character. Name of regime indicator variable.
#' @param df_adjust Character. Degrees of freedom adjustment.
#' @param verbose Logical. Whether to print progress and diagnostics.
#'
#' @return An object of class "rigobon_gmm" (inheriting from "lewbel_gmm").
#'
#' @seealso
#' \code{\link{rigobon_triangular_moments}}, \code{\link{rigobon_simultaneous_moments}} for moment condition functions.
#' \code{\link{lewbel_gmm}} for Lewbel's continuous heteroskedasticity identification.
#' \code{\link{prono_gmm}} for GARCH-based heteroskedasticity identification.
#' \code{\link{generate_rigobon_data}} for generating data with regime-based heteroskedasticity.
#' \code{\link{run_rigobon_estimation}} for 2SLS implementation.
#'
#' @examples
#' \dontrun{
#' # Generate Rigobon-style data
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0,
#'   regime_probs = c(0.4, 0.6),
#'   sigma2_regimes = c(1.0, 2.5)
#' )
#' data <- generate_rigobon_data(1000, params)
#'
#' # GMM estimation
#' gmm_result <- rigobon_gmm(data)
#' summary(gmm_result)
#' }
#'
#' @export
rigobon_gmm <- function(data,
                        system = "triangular",
                        gmm_type = "twoStep",
                        vcov_type = "HAC",
                        endog_var = "Y2",
                        exog_vars = "Xk",
                        regime_var = "regime",
                        df_adjust = "asymptotic",
                        verbose = TRUE) {

  # Input validation
  required_vars <- c("Y1", endog_var, exog_vars, regime_var)
  if (!all(required_vars %in% names(data))) {
    missing <- setdiff(required_vars, names(data))
    stop(paste("Missing required variables:", paste(missing, collapse = ", ")))
  }

  # Check number of regimes
  regimes <- unique(data[[regime_var]])
  n_regimes <- length(regimes)

  if (n_regimes < 2) {
    stop("Need at least 2 regimes for Rigobon identification")
  }

  if (system == "simultaneous" && n_regimes < 3) {
    stop("Need at least 3 regimes for identification in simultaneous system")
  }

  if (verbose) {
    cat("\n=== Rigobon GMM Estimation ===\n")
    cat("System:", system, "\n")
    cat("Number of regimes:", n_regimes, "\n")
    cat("GMM type:", gmm_type, "\n")
    cat("Variance-covariance:", vcov_type, "\n\n")
  }

  # Get initial values from 2SLS
  rig_2sls <- run_rigobon_estimation(
    data = data,
    endog_var = endog_var,
    exog_vars = exog_vars,
    regime_var = regime_var,
    df_adjust = df_adjust,
    return_diagnostics = FALSE
  )

  # Prepare initial values
  n_x <- length(exog_vars)
  k <- n_x + 1  # Including intercept

  if (system == "triangular") {
    # Get coefficients from 2SLS first stage
    y2_formula <- as.formula(paste(endog_var, "~", paste(exog_vars, collapse = " + ")))
    first_stage <- lm(y2_formula, data = data)
    beta2_init <- coef(first_stage)

    # Initial values: beta1, gamma1, beta2
    init_values <- c(
      rep(0, k),  # beta1 (will be updated)
      rig_2sls$tsls$estimates["gamma1"],  # gamma1 from 2SLS
      beta2_init  # beta2 from first stage
    )

    # Update beta1 using gamma1
    y1_adj <- data$Y1 - data[[endog_var]] * init_values[k + 1]
    beta1_model <- lm(y1_adj ~ ., data = data[, exog_vars, drop = FALSE])
    init_values[1:k] <- coef(beta1_model)

    # Define moment function
    moment_fn <- function(theta, data) {
      rigobon_triangular_moments(
        theta = theta,
        data = data,
        y1_var = "Y1",
        y2_var = endog_var,
        x_vars = exog_vars,
        regime_var = regime_var,
        add_intercept = TRUE
      )
    }

  } else {  # simultaneous
    # Initial values: beta1, gamma1, beta2, gamma2
    init_values <- c(
      rep(0, k),  # beta1
      rig_2sls$tsls$estimates["gamma1"],  # gamma1 from 2SLS
      coef(lm(as.formula(paste(endog_var, "~", paste(exog_vars, collapse = " + "))),
              data = data)),  # beta2
      0  # gamma2 (small initial value)
    )

    # Define moment function
    moment_fn <- function(theta, data) {
      rigobon_simultaneous_moments(
        theta = theta,
        data = data,
        y1_var = "Y1",
        y2_var = endog_var,
        x_vars = exog_vars,
        regime_var = regime_var,
        add_intercept = TRUE
      )
    }
  }

  # Run GMM estimation
  gmm_result <- tryCatch({
    gmm::gmm(
      g = moment_fn,
      x = data,
      t0 = init_values,
      type = gmm_type,
      wmatrix = "optimal",
      vcov = vcov_type,
      control = list(maxit = 1000, reltol = 1e-6)
    )
  }, error = function(e) {
    stop(paste("GMM estimation failed:", e$message))
  })

  # Extract results and create custom class
  coef_vec <- coef(gmm_result)

  # Name the coefficients
  if (system == "triangular") {
    names(coef_vec) <- c(
      paste0("beta1_", c("const", exog_vars)),
      "gamma1",
      paste0("beta2_", c("const", exog_vars))
    )
  } else {
    names(coef_vec) <- c(
      paste0("beta1_", c("const", exog_vars)),
      "gamma1",
      paste0("beta2_", c("const", exog_vars)),
      "gamma2"
    )
  }

  # Create result object
  result <- list(
    coefficients = coef_vec,
    vcov = vcov(gmm_result),
    gmm_result = gmm_result,
    system = system,
    method = "Rigobon",
    n_obs = nrow(data),
    n_regimes = n_regimes,
    regime_var = regime_var,
    gmm_type = gmm_type,
    vcov_type = vcov_type,
    df_adjust = df_adjust
  )

  # Add J-test for overidentification
  if (!is.null(gmm_result$test)) {
    result$J_test <- list(
      J_stat = gmm_result$test$test[1],
      p_value = gmm_result$test$test[2],
      df = gmm_result$test$df
    )
  }

  # Add regime distribution info
  result$regime_distribution <- table(data[[regime_var]]) / nrow(data)

  class(result) <- c("rigobon_gmm", "lewbel_gmm")

  if (verbose) {
    cat("Estimation completed.\n")
    print(summary(result))
  }

  result
}


#' Define GMM Moment Conditions for Prono Triangular System
#'
#' Creates the moment function for GMM estimation of a triangular system
#' using Prono's GARCH-based heteroskedasticity identification.
#'
#' @param theta Numeric vector. Parameters to estimate: c(beta1, gamma1, beta2).
#' @param data Data frame containing the variables and fitted GARCH variances.
#' @param y1_var Character. Name of the first dependent variable.
#' @param y2_var Character. Name of the second dependent variable.
#' @param x_vars Character vector. Names of exogenous variables.
#' @param add_intercept Logical. Whether to add an intercept to the exogenous variables.
#' @param garch_res_var Character. Name of the fitted GARCH variance variable.
#'
#' @return Matrix of moment conditions (n × g).
#'
#' @seealso
#' \code{\link{prono_gmm}} for the main GMM estimation function.
#' \code{\link{lewbel_triangular_moments}}, \code{\link{rigobon_triangular_moments}} for other identification strategies.
#'
#' @export
prono_triangular_moments <- function(theta, data, y1_var, y2_var, x_vars, add_intercept, garch_res_var) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Parameters: (beta1_0, beta1_1, ..., gamma1, beta2_0, beta2_1, ...)
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2

  # Construct Z instrument from GARCH residuals (sigma_2t_hat^2)
  # garch_res_var should be the name of the column in 'data' holding sigma_2t_hat^2
  if (!(garch_res_var %in% names(data))) {
    stopper("GARCH residual variance variable (", garch_res_var, ") not found in data.")
  }
  sigma2_sq_hat <- data[[garch_res_var]]
  if (length(sigma2_sq_hat) != n) {
    stopper("Length of GARCH residual variance variable does not match data.")
  }

  z_instrument <- sigma2_sq_hat - mean(sigma2_sq_hat) # Centered sigma_2t_hat^2 as the Z instrument

  # Moment conditions for Prono (triangular)
  # E[X * eps1] = 0
  # E[X * eps2] = 0
  # E[Z * eps1 * eps2] = 0 (This is the Lewbel form, Prono uses Z for Var(eps2) or product e1e2)
  # Prono (2014, eq.7) has E[ (sigma_2t^2 - E[sigma_2t^2]) * eps_1t * eps_2t ] = 0

  m1 <- x_matrix * eps1
  m2 <- x_matrix * eps2
  m3 <- z_instrument * eps1 * eps2

  moments <- cbind(m1, m2, m3)
  moments
}


#' GMM Estimation for Prono (2014) GARCH-Based Identification
#'
#' Implements GMM estimation for Prono's heteroskedasticity-based
#' identification using GARCH conditional variances.
#'
#' @param data Data frame containing the variables.
#' @param gmm_type Character. Type of GMM: "twoStep", "iterative", or "cue".
#' @param vcov_type Character. Type of variance-covariance matrix.
#' @param endog_var Character. Name of endogenous variable.
#' @param exog_vars Character vector. Names of exogenous variables.
#' @param fit_garch Logical. Whether to fit GARCH model (TRUE) or use pre-fitted values.
#' @param garch_order Numeric vector. GARCH(p,q) order, default c(1,1).
#' @param df_adjust Character. Degrees of freedom adjustment.
#' @param verbose Logical. Whether to print progress and diagnostics.
#'
#' @return An object of class "prono_gmm" (inheriting from "lewbel_gmm").
#'
#' @seealso
#' \code{\link{prono_triangular_moments}} for moment condition functions.
#' \code{\link{lewbel_gmm}} for Lewbel's heteroskedasticity-based identification.
#' \code{\link{rigobon_gmm}} for regime-based heteroskedasticity identification.
#' \code{\link{generate_prono_data}} for generating data with GARCH heteroskedasticity.
#' \code{\link{run_single_prono_simulation}} for simulation implementation.
#' \code{\link{create_prono_config}} for default configuration.
#'
#' @examples
#' \dontrun{
#' # Generate Prono-style data
#' config <- create_prono_config(n = 500)
#' data <- generate_prono_data(
#'   n = config$n,
#'   beta1 = config$beta1,
#'   beta2 = config$beta2,
#'   gamma1 = config$gamma1,
#'   garch_params = config$garch_params
#' )
#'
#' # GMM estimation
#' gmm_result <- prono_gmm(data)
#' summary(gmm_result)
#' }
#'
#' @export
prono_gmm <- function(data,
                      gmm_type = "twoStep",
                      vcov_type = "HAC",
                      endog_var = "Y2",
                      exog_vars = NULL,
                      fit_garch = TRUE,
                      garch_order = c(1, 1),
                      df_adjust = "asymptotic",
                      verbose = TRUE) {

  # Auto-detect exogenous variables if not provided
  if (is.null(exog_vars)) {
    # Look for X1, X2, etc.
    x_cols <- grep("^X[0-9]+$", names(data), value = TRUE)
    if (length(x_cols) > 0) {
      exog_vars <- x_cols
    } else {
      stop("No exogenous variables found. Please specify exog_vars.")
    }
  }

  # Input validation
  required_vars <- c("Y1", endog_var, exog_vars)
  if (!all(required_vars %in% names(data))) {
    missing <- setdiff(required_vars, names(data))
    stop(paste("Missing required variables:", paste(missing, collapse = ", ")))
  }

  if (verbose) {
    cat("\n=== Prono GMM Estimation ===\n")
    cat("GARCH-based heteroskedasticity identification\n")
    cat("GMM type:", gmm_type, "\n")
    cat("Variance-covariance:", vcov_type, "\n\n")
  }

  # Step 1: Fit GARCH model if needed
  if (fit_garch || !"sigma2_sq_hat" %in% names(data)) {
    if (verbose) cat("Fitting GARCH model to second equation residuals...\n")

    # Estimate second equation
    y2_formula <- as.formula(paste(endog_var, "~", paste(exog_vars, collapse = " + ")))
    second_eq <- lm(y2_formula, data = data)
    e2_hat <- residuals(second_eq)

    # Fit GARCH model
    if (!requireNamespace("rugarch", quietly = TRUE)) {
      stop("Package 'rugarch' is required for GARCH modeling. Please install it.")
    }

    garch_spec <- rugarch::ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = garch_order),
      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
      distribution.model = "norm"
    )

    garch_fit <- tryCatch({
      rugarch::ugarchfit(spec = garch_spec, data = e2_hat)
    }, error = function(e) {
      if (verbose) cat("GARCH fitting failed, using squared residuals as fallback\n")
      NULL
    })

    if (!is.null(garch_fit)) {
      data$sigma2_sq_hat <- as.numeric(rugarch::sigma(garch_fit))^2

      if (verbose) {
        garch_coef <- coef(garch_fit)
        cat("GARCH coefficients:\n")
        print(round(garch_coef, 4))
      }
    } else {
      # Fallback to squared residuals
      data$sigma2_sq_hat <- e2_hat^2
    }
  }

  # Step 2: Get initial values from 2SLS
  # First construct the Prono instrument
  y2_formula <- as.formula(paste(endog_var, "~", paste(exog_vars, collapse = " + ")))
  second_eq <- lm(y2_formula, data = data)
  e2_hat <- residuals(second_eq)

  z_centered <- data$sigma2_sq_hat - mean(data$sigma2_sq_hat)
  prono_iv <- z_centered * e2_hat
  data$prono_iv <- prono_iv

  # Run 2SLS for initial values
  iv_formula <- as.formula(paste(
    "Y1 ~", endog_var, "+", paste(exog_vars, collapse = " + "),
    "|", paste(exog_vars, collapse = " + "), "+ prono_iv"
  ))

  iv_fit <- tryCatch({
    if (requireNamespace("ivreg", quietly = TRUE)) {
      ivreg::ivreg(iv_formula, data = data)
    } else if (requireNamespace("AER", quietly = TRUE)) {
      AER::ivreg(iv_formula, data = data)
    } else {
      stop("Neither ivreg nor AER package available")
    }
  }, error = function(e) {
    stop(paste("2SLS for initial values failed:", e$message))
  })

  # Prepare initial values
  n_x <- length(exog_vars)
  k <- n_x + 1  # Including intercept

  # Get coefficients from 2SLS
  beta1_init <- coef(iv_fit)[c("(Intercept)", exog_vars)]
  gamma1_init <- coef(iv_fit)[endog_var]
  beta2_init <- coef(second_eq)

  # Initial values: beta1, gamma1, beta2
  init_values <- c(beta1_init, gamma1_init, beta2_init)

  # Define moment function
  moment_fn <- function(theta, data) {
    prono_triangular_moments(
      theta = theta,
      data = data,
      y1_var = "Y1",
      y2_var = endog_var,
      x_vars = exog_vars,
      add_intercept = TRUE,
      garch_res_var = "sigma2_sq_hat"
    )
  }

  # Run GMM estimation
  gmm_result <- tryCatch({
    gmm::gmm(
      g = moment_fn,
      x = data,
      t0 = init_values,
      type = gmm_type,
      wmatrix = "optimal",
      vcov = vcov_type,
      control = list(maxit = 1000, reltol = 1e-6)
    )
  }, error = function(e) {
    stop(paste("GMM estimation failed:", e$message))
  })

  # Extract results and create custom class
  coef_vec <- coef(gmm_result)

  # Name the coefficients
  names(coef_vec) <- c(
    paste0("beta1_", c("const", exog_vars)),
    "gamma1",
    paste0("beta2_", c("const", exog_vars))
  )

  # Create result object
  result <- list(
    coefficients = coef_vec,
    vcov = vcov(gmm_result),
    gmm_result = gmm_result,
    method = "Prono",
    n_obs = nrow(data),
    garch_order = garch_order,
    gmm_type = gmm_type,
    vcov_type = vcov_type,
    df_adjust = df_adjust,
    garch_fit = if (exists("garch_fit")) garch_fit else NULL
  )

  # Add J-test for overidentification
  if (!is.null(gmm_result$test)) {
    result$J_test <- list(
      J_stat = gmm_result$test$test[1],
      p_value = gmm_result$test$test[2],
      df = gmm_result$test$df
    )
  }

  # Add first-stage F-statistic
  first_stage_formula <- as.formula(paste(
    endog_var, "~", paste(exog_vars, collapse = " + "), "+ prono_iv"
  ))
  first_stage <- lm(first_stage_formula, data = data)
  restricted_formula <- as.formula(paste(endog_var, "~", paste(exog_vars, collapse = " + ")))
  restricted <- lm(restricted_formula, data = data)

  result$first_stage_F <- tryCatch({
    anova(restricted, first_stage, test = "F")[2, "F"]
  }, error = function(e) NA)

  class(result) <- c("prono_gmm", "lewbel_gmm")

  if (verbose) {
    cat("\nEstimation completed.\n")
    cat("First-stage F-statistic:", round(result$first_stage_F, 2), "\n")
    print(summary(result))
  }

  result
}
