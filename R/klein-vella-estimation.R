#' Parametric Klein & Vella estimation
#'
#' @template param-data
#' @template param-y1-var
#' @template param-y2-var
#' @template param-x-vars
#' @param variance_type Type of variance function ("exponential", "power", "linear")
#' @param optimization_method Optimization method for nonlinear least squares
#' @template param-verbose
#'
#' @return List containing estimation results
#' @export
#'
#' @details
#' Implements the parametric version of Klein & Vella (2010) using the
#' control function approach with parametric variance specifications.
#'
#' The model is:
#' Y1 = X'beta1 + gamma1*Y2 + rho*(S1(X)/S2(X))*epsilon2 + eta1
#'
#' where S1(X) and S2(X) are conditional standard deviations.
#'
#' @examples
#' # Generate data
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
#'
#' # Estimate
#' results <- klein_vella_parametric(data)
#' print(results)
#'
klein_vella_parametric <- function(data,
                                   y1_var = "Y1",
                                   y2_var = "Y2",
                                   x_vars = NULL,
                                   variance_type = "exponential",
                                   optimization_method = "BFGS",
                                   verbose = TRUE) {
  # Auto-detect X variables if not provided
  if (is.null(x_vars)) {
    x_vars <- names(data)[grepl("^X", names(data))]
    if (length(x_vars) == 0 && "X" %in% names(data)) {
      x_vars <- "X"
    }
    if (length(x_vars) == 0) {
      stop("No X variables found. Specify x_vars explicitly.")
    }
  }

  # Extract data
  Y1 <- data[[y1_var]]
  Y2 <- data[[y2_var]]
  X <- as.matrix(cbind(1, data[, x_vars, drop = FALSE])) # Add intercept
  n <- nrow(data)
  k <- ncol(X) - 1 # Number of X variables (excluding intercept)

  if (verbose) {
    message("\n=== Klein & Vella Parametric Estimation ===")
    message(sprintf("Sample size: %d", n))
    message(sprintf("Number of X variables: %d", k))
    message(sprintf("Variance type: %s", variance_type))
  }

  # Step 1: Estimate second equation by OLS
  lm2 <- lm(as.formula(paste(y2_var, "~", paste(x_vars, collapse = " + "))), data = data)
  beta2 <- coef(lm2)
  e2 <- residuals(lm2)

  # Define variance functions based on type
  if (variance_type == "exponential") {
    variance_function <- function(X, delta) {
      log_var <- X %*% delta
      # Cap extreme values
      log_var <- pmax(pmin(log_var, .hetid_const("LOG_BOUND_MAX")), .hetid_const("LOG_BOUND_MIN"))
      exp(log_var)
    }
  } else if (variance_type == "power") {
    variance_function <- function(X, delta) {
      linear_combo <- X %*% delta
      pmax(linear_combo^2, .hetid_const("EPSILON_TOLERANCE")) # Ensure positivity
    }
  } else if (variance_type == "linear") {
    variance_function <- function(X, delta) {
      linear_combo <- X %*% delta
      pmax(linear_combo, .hetid_const("EPSILON_TOLERANCE")) # Ensure positivity
    }
  } else {
    stop("Unknown variance_type. Choose 'exponential', 'power', or 'linear'.")
  }

  # Step 2: Define objective function for NLS
  objective_function <- function(params) {
    # Extract parameters
    beta1 <- params[1:(k + 1)]
    gamma1 <- params[k + 2]
    rho <- params[k + 3]
    delta1 <- params[(k + 4):(2 * k + 4)]
    delta2 <- params[(2 * k + 5):(3 * k + 5)]

    # Ensure rho is in valid range
    rho <- tanh(rho) # Transform to [-1, 1]

    # Calculate variances
    var1 <- variance_function(X, delta1)
    var2 <- variance_function(X, delta2)
    s1 <- sqrt(var1)
    s2 <- sqrt(var2)

    # Calculate residuals for first equation including control function
    control_term <- rho * (s1 / s2) * e2
    residuals <- Y1 - X %*% beta1 - gamma1 * Y2 - control_term

    # Return sum of squared residuals
    sum(residuals^2)
  }

  # Step 3: Initial values
  # Start with OLS estimates ignoring endogeneity
  lm1_init <- lm(as.formula(paste(y1_var, "~", paste(c(x_vars, y2_var), collapse = " + "))),
    data = data
  )

  init_params <- c(
    coef(lm1_init)[1:(k + 1)], # beta1 initial
    coef(lm1_init)[k + 2], # gamma1 initial
    0, # rho initial (transformed)
    rep(.hetid_const("IDENTIFICATION_TOLERANCE"), k + 1), # delta1 initial
    rep(.hetid_const("IDENTIFICATION_TOLERANCE"), k + 1) # delta2 initial
  )

  # Step 4: Optimize
  if (verbose) message("\nOptimizing...")

  opt_result <- optim(
    par = init_params,
    fn = objective_function,
    method = optimization_method,
    control = list(maxit = .hetid_const("MAX_ITERATIONS_DEFAULT"), reltol = .hetid_const("CONVERGENCE_TOLERANCE"))
  )

  if (opt_result$convergence != 0) {
    warning("Optimization did not converge. Results may be unreliable.")
  }

  # Extract final estimates
  final_params <- opt_result$par
  beta1_est <- final_params[1:(k + 1)]
  gamma1_est <- final_params[k + 2]
  rho_est <- tanh(final_params[k + 3]) # Transform back
  delta1_est <- final_params[(k + 4):(2 * k + 4)]
  delta2_est <- final_params[(2 * k + 5):(3 * k + 5)]

  # Calculate fitted values and residuals
  var1_est <- variance_function(X, delta1_est)
  var2_est <- variance_function(X, delta2_est)
  s1_est <- sqrt(var1_est)
  s2_est <- sqrt(var2_est)
  control_est <- rho_est * (s1_est / s2_est) * e2

  y1_fitted <- X %*% beta1_est + gamma1_est * Y2 + control_est
  residuals_final <- Y1 - y1_fitted

  # Calculate standard errors (using sandwich estimator)
  # This is a simplified version - in practice use more sophisticated methods
  n_params <- length(final_params)

  # Numerical gradient
  grad_fn <- function(params) {
    eps <- .hetid_const("EPSILON_TOLERANCE")
    grad <- numeric(n_params)
    f0 <- objective_function(params)

    for (i in 1:n_params) {
      params_plus <- params
      params_plus[i] <- params[i] + eps
      grad[i] <- (objective_function(params_plus) - f0) / eps
    }

    grad
  }

  # Hessian approximation
  hessian <- matrix(0, n_params, n_params)
  eps <- .hetid_const("EPSILON_TOLERANCE")
  for (i in 1:n_params) {
    params_plus <- final_params
    params_plus[i] <- final_params[i] + eps
    grad_plus <- grad_fn(params_plus)
    grad_base <- grad_fn(final_params)
    hessian[i, ] <- (grad_plus - grad_base) / eps
  }

  # Make symmetric
  hessian <- (hessian + t(hessian)) / 2

  # Calculate covariance matrix
  sigma2 <- sum(residuals_final^2) / (n - n_params)
  vcov_matrix <- tryCatch(
    sigma2 * solve(hessian),
    error = function(e) {
      warning("Could not compute standard errors: ", e$message)
      matrix(NA, n_params, n_params)
    }
  )

  # Extract standard errors
  se_all <- sqrt(diag(abs(vcov_matrix)))

  # Create results object
  results <- list(
    estimates = c(
      setNames(beta1_est, paste0("beta1_", 0:k)),
      gamma1 = gamma1_est,
      rho = rho_est
    ),
    variance_params = list(
      delta1 = setNames(delta1_est, paste0("delta1_", 0:k)),
      delta2 = setNames(delta2_est, paste0("delta2_", 0:k))
    ),
    se = c(
      setNames(se_all[1:(k + 1)], paste0("se_beta1_", 0:k)),
      se_gamma1 = se_all[k + 2],
      se_rho = se_all[k + 3] * (1 - rho_est^2) # Delta method for tanh transform
    ),
    fitted_values = as.vector(y1_fitted),
    residuals = as.vector(residuals_final),
    control_function = as.vector(control_est),
    variance_functions = list(
      S1_squared = as.vector(var1_est),
      S2_squared = as.vector(var2_est)
    ),
    convergence = opt_result$convergence,
    n = n,
    k = k,
    variance_type = variance_type,
    call = match.call()
  )

  class(results) <- c("klein_vella_fit", "list")

  if (verbose) {
    message("\nEstimation complete.")
    message(sprintf("gamma1 estimate: %.4f (SE: %.4f)", gamma1_est, se_all[k + 2]))
    message(sprintf("rho estimate: %.4f (SE: %.4f)", rho_est, results$se["se_rho"]))
  }

  return(results)
}

#' Print method for Klein & Vella estimation results
#'
#' @param x A klein_vella_fit object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.klein_vella_fit <- function(x, ...) {
  cat("\nKlein & Vella Estimation Results\n")
  cat("================================\n")
  cat(sprintf("Sample size: %d\n", x$n))
  cat(sprintf("Variance type: %s\n", x$variance_type))
  cat(sprintf("Convergence: %s\n\n", ifelse(x$convergence == 0, "Yes", "No")))

  # Main estimates
  cat("Parameter Estimates:\n")
  cat("-------------------\n")

  # Extract key parameters
  gamma1_idx <- which(names(x$estimates) == "gamma1")
  rho_idx <- which(names(x$estimates) == "rho")

  # Print beta1 coefficients
  beta1_names <- names(x$estimates)[!names(x$estimates) %in% c("gamma1", "rho")]
  for (i in seq_along(beta1_names)) {
    cat(sprintf(
      "%-12s %8.4f  (SE: %6.4f)\n",
      beta1_names[i], x$estimates[i], x$se[i]
    ))
  }

  # Print gamma1
  cat(sprintf(
    "%-12s %8.4f  (SE: %6.4f) ***\n",
    "gamma1", x$estimates[gamma1_idx], x$se[gamma1_idx]
  ))

  # Print rho
  cat(sprintf(
    "%-12s %8.4f  (SE: %6.4f)\n",
    "rho", x$estimates[rho_idx], x$se[rho_idx + 1]
  ))

  cat("\n*** Endogenous parameter\n")

  invisible(x)
}

#' Summary method for Klein & Vella estimation results
#'
#' @param object A klein_vella_fit object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.klein_vella_fit <- function(object, ...) {
  # Calculate additional statistics
  rss <- sum(object$residuals^2)
  tss <- sum((object$fitted_values + object$residuals -
    mean(object$fitted_values + object$residuals))^2)
  r_squared <- 1 - rss / tss

  # Variance ratio statistics
  var_ratio <- sqrt(object$variance_functions$S1_squared) /
    sqrt(object$variance_functions$S2_squared)

  summary_obj <- list(
    call = object$call,
    estimates = object$estimates,
    se = object$se,
    variance_params = object$variance_params,
    r_squared = r_squared,
    n = object$n,
    k = object$k,
    variance_type = object$variance_type,
    var_ratio_summary = c(
      Min = min(var_ratio),
      Q1 = quantile(var_ratio, 0.25),
      Median = median(var_ratio),
      Mean = mean(var_ratio),
      Q3 = quantile(var_ratio, 0.75),
      Max = max(var_ratio),
      SD = sd(var_ratio),
      CV = sd(var_ratio) / mean(var_ratio)
    )
  )

  class(summary_obj) <- "summary.klein_vella_fit"
  summary_obj
}

#' Print summary of Klein & Vella estimation results
#'
#' @param x A summary.klein_vella_fit object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.summary.klein_vella_fit <- function(x, ...) {
  cat("\nKlein & Vella Estimation Summary\n")
  cat("================================\n\n")

  cat("Call:\n")
  print(x$call)

  cat(sprintf("\nSample size: %d", x$n))
  cat(sprintf("\nR-squared: %.4f", x$r_squared))
  cat(sprintf("\nVariance type: %s\n", x$variance_type))

  # Parameter table
  cat("\nCoefficients:\n")
  cat("             Estimate Std.Error t-value\n")

  for (i in seq_along(x$estimates)) {
    param_name <- names(x$estimates)[i]
    estimate <- x$estimates[i]
    se <- x$se[i]
    t_val <- estimate / se

    cat(sprintf(
      "%-12s %8.4f  %8.4f %7.2f",
      param_name, estimate, se, t_val
    ))

    # Add significance stars
    if (abs(t_val) > qnorm(1 - .hetid_const("ALPHA_STRICT") / 2)) {
      cat(" ***")
    } else if (abs(t_val) > .hetid_const("Z_CRITICAL_95")) {
      cat(" **")
    } else if (abs(t_val) > qnorm(1 - .hetid_const("ALPHA_DEFAULT") / 2)) {
      cat(" *")
    }

    cat("\n")
  }

  cat("\n---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05\n")

  # Variance ratio information
  cat("\nVariance Ratio S1(X)/S2(X) Summary:\n")
  var_stats <- x$var_ratio_summary
  cat(sprintf(
    "   Min: %.3f   Q1: %.3f   Median: %.3f\n",
    var_stats["Min"], var_stats["Q1"], var_stats["Median"]
  ))
  cat(sprintf(
    "   Mean: %.3f  Q3: %.3f   Max: %.3f\n",
    var_stats["Mean"], var_stats["Q3"], var_stats["Max"]
  ))
  cat(sprintf(
    "   SD: %.3f     CV: %.3f\n",
    var_stats["SD"], var_stats["CV"]
  ))

  if (var_stats["CV"] < .hetid_const("IDENTIFICATION_TOLERANCE")) {
    cat(sprintf("\nWarning: Low variation in variance ratio (CV < %.1f).\n", .hetid_const("IDENTIFICATION_TOLERANCE")))
    cat("         Identification may be weak.\n")
  }

  invisible(x)
}

#' Run Klein & Vella demonstration
#'
#' @param n Sample size (default: 500)
#' @param verbose Whether to print results
#'
#' @return Invisibly returns the results
#' @export
#'
#' @examples
#' run_klein_vella_demo()
#'
run_klein_vella_demo <- function(n = .hetid_const("N_SMALL"), verbose = TRUE) {
  if (verbose) {
    cat("\n====================================\n")
    cat("Klein & Vella (2010) Demonstration\n")
    cat("====================================\n\n")
  }

  # Create configuration
  config <- create_klein_vella_config(
    n = n,
    beta1 = c(0.5, 1.5),
    beta2 = c(1.0, -1.0),
    gamma1 = -0.8,
    rho = 0.6,
    delta1 = c(0.1, 0.3),
    delta2 = c(0.2, -0.2),
    verbose = verbose
  )

  # Generate data
  if (verbose) cat("\nGenerating data...\n")
  data <- generate_klein_vella_data(config)

  # OLS (biased) estimate
  if (verbose) cat("\nOLS estimation (ignoring endogeneity)...\n")
  ols_model <- lm(Y1 ~ X + Y2, data = data)
  ols_gamma1 <- coef(ols_model)["Y2"]

  # Klein & Vella parametric estimation
  if (verbose) cat("\nKlein & Vella parametric estimation...\n")
  kv_results <- klein_vella_parametric(data, verbose = verbose)

  # Compare results
  if (verbose) {
    cat("\n\n=== RESULTS COMPARISON ===\n")
    cat(sprintf("True gamma1:        %8.4f\n", config$gamma1))
    cat(sprintf(
      "OLS estimate:       %8.4f (bias: %+.4f)\n",
      ols_gamma1, ols_gamma1 - config$gamma1
    ))
    cat(sprintf(
      "K&V estimate:       %8.4f (bias: %+.4f)\n",
      kv_results$estimates["gamma1"],
      kv_results$estimates["gamma1"] - config$gamma1
    ))

    cat("\nTrue rho:           ", sprintf("%8.4f", config$rho), "\n")
    cat("K&V rho estimate:   ", sprintf("%8.4f", kv_results$estimates["rho"]), "\n")

    # Check if semiparametric is available
    if (requireNamespace("np", quietly = TRUE)) {
      cat("\nNote: For semiparametric estimation, use klein_vella_semiparametric()\n")
    } else {
      cat("\nNote: Install 'np' package for semiparametric estimation\n")
    }
  }

  invisible(list(
    config = config,
    data = data,
    ols_results = ols_model,
    kv_results = kv_results
  ))
}
