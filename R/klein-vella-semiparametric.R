#' Semiparametric Klein & Vella estimation
#'
#' @param data Data frame containing Y1, Y2, and X variables
#' @param y1_var Name of first endogenous variable (default: "Y1")
#' @param y2_var Name of second endogenous variable (default: "Y2")
#' @param x_vars Names of exogenous variables (default: auto-detect)
#' @param bandwidth_method Method for bandwidth selection ("cv.aic", "cv.ls", "rule.of.thumb")
#' @param kernel_type Kernel type for nonparametric regression
#' @param max_iter Maximum iterations for the nested optimization
#' @param tol Convergence tolerance
#' @param verbose Whether to print progress information
#'
#' @return List containing estimation results
#' @export
#'
#' @details
#' Implements the semiparametric version of Klein & Vella (2010) using
#' nonparametric estimation of the conditional variance functions.
#'
#' Requires the 'np' package for nonparametric regression.
#'
#' @examples
#' \dontrun{
#' # Requires np package
#' if (requireNamespace("np", quietly = TRUE)) {
#'   config <- create_klein_vella_config(
#'     n = .hetid_const("N_SMALL"),
#'     beta1 = c(0.5, 1.5),
#'     beta2 = c(1.0, -1.0),
#'     gamma1 = -0.8,
#'     rho = 0.6,
#'     delta1 = c(0.1, 0.3),
#'     delta2 = c(0.2, -0.2)
#'   )
#'   data <- generate_klein_vella_data(config)
#'   results <- klein_vella_semiparametric(data)
#'   print(results)
#' }
#' }
#'
klein_vella_semiparametric <- function(data,
                                       y1_var = "Y1",
                                       y2_var = "Y2",
                                       x_vars = NULL,
                                       bandwidth_method = "cv.aic",
                                       kernel_type = "gaussian",
                                       max_iter = .hetid_const("MAX_ITERATIONS_KLEIN_VELLA"),
                                       tol = .hetid_const("EPSILON_TOLERANCE"),
                                       verbose = TRUE) {
  # Check if np package is available
  if (!requireNamespace("np", quietly = TRUE)) {
    stop(
      "The 'np' package is required for semiparametric estimation.\n",
      "Please install it with: install.packages('np')"
    )
  }

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
  x_data <- as.matrix(data[, x_vars, drop = FALSE])
  X <- as.matrix(cbind(1, x_data)) # Add intercept for parametric part
  n <- nrow(data)
  k <- ncol(X) - 1

  if (verbose) {
    message("\n=== Klein & Vella Semiparametric Estimation ===")
    message(sprintf("Sample size: %d", n))
    message(sprintf("Number of X variables: %d", k))
    message(sprintf("Bandwidth method: %s", bandwidth_method))
  }

  # Step 1: Estimate second equation by OLS
  lm2 <- lm(as.formula(paste(y2_var, "~", paste(x_vars, collapse = " + "))), data = data)
  beta2 <- coef(lm2)
  e2 <- residuals(lm2)

  # Step 2: Estimate conditional variance of e2 nonparametrically
  if (verbose) message("\nEstimating conditional variance of Y2 residuals...")

  # Prepare data for np
  e2_squared <- e2^2

  # Estimate E[e2^2 | X] nonparametrically
  tryCatch(
    {
      # Create bandwidth object
      if (k == 1) {
        # Single X variable
        bw2 <- np::npregbw(
          formula = e2_squared ~ x_data,
          regtype = "ll", # Local linear
          bwmethod = bandwidth_method,
          ckertype = kernel_type
        )
      } else {
        # Multiple X variables
        x_formula <- as.formula(paste(
          "e2_squared ~",
          paste(x_vars, collapse = " + ")
        ))
        bw2 <- np::npregbw(
          formula = x_formula,
          data = cbind(data[x_vars], e2_squared = e2_squared),
          regtype = "ll",
          bwmethod = bandwidth_method,
          ckertype = kernel_type
        )
      }

      # Estimate conditional variance
      np_fit2 <- np::npreg(bw2)
      s2_squared <- fitted(np_fit2)
      s2_squared <- pmax(s2_squared, .hetid_const("EPSILON_TOLERANCE")) # Ensure positivity
      s2 <- sqrt(s2_squared)
    },
    error = function(e) {
      stop("Error in nonparametric variance estimation: ", e$message)
    }
  )

  # Step 3: Iterative estimation of main equation
  if (verbose) message("\nIterative estimation of main equation...")

  # Initial values from OLS
  lm1_init <- lm(
    as.formula(paste(
      y1_var, "~",
      paste(c(x_vars, y2_var), collapse = " + ")
    )),
    data = data
  )

  beta1_old <- coef(lm1_init)[1:(k + 1)]
  gamma1_old <- coef(lm1_init)[k + 2]
  rho_old <- 0

  converged <- FALSE
  iter <- 0

  while (!converged && iter < max_iter) {
    iter <- iter + 1

    # Calculate residuals for Y1 equation
    e1 <- Y1 - X %*% beta1_old - gamma1_old * Y2
    e1_squared <- e1^2

    # Estimate conditional variance of e1 nonparametrically
    tryCatch(
      {
        if (k == 1) {
          bw1 <- np::npregbw(
            formula = e1_squared ~ x_data,
            regtype = "ll",
            bwmethod = bandwidth_method,
            ckertype = kernel_type
          )
        } else {
          x_formula <- as.formula(paste(
            "e1_squared ~",
            paste(x_vars, collapse = " + ")
          ))
          bw1 <- np::npregbw(
            formula = x_formula,
            data = cbind(data[x_vars], e1_squared = e1_squared),
            regtype = "ll",
            bwmethod = bandwidth_method,
            ckertype = kernel_type
          )
        }

        np_fit1 <- np::npreg(bw1)
        s1_squared <- fitted(np_fit1)
        s1_squared <- pmax(s1_squared, .hetid_const("EPSILON_TOLERANCE"))
        s1 <- sqrt(s1_squared)
      },
      error = function(e) {
        stop("Error in nonparametric variance estimation: ", e$message)
      }
    )

    # Calculate control function
    control_function <- (s1 / s2) * e2

    # Update parameters by OLS with control function
    y1_augmented <- lm(Y1 ~ X[, -1] + Y2 + control_function - 1) # -1 to exclude automatic intercept

    new_coefs <- coef(y1_augmented)
    beta1_new <- new_coefs[1:(k + 1)]
    gamma1_new <- new_coefs[k + 2]
    rho_new <- new_coefs[k + 3]

    # Check convergence
    param_change <- sqrt(sum((beta1_new - beta1_old)^2) +
      (gamma1_new - gamma1_old)^2 +
      (rho_new - rho_old)^2)

    if (verbose && iter %% 10 == 0) {
      message(sprintf("  Iteration %d: parameter change = %.6f", iter, param_change))
    }

    if (param_change < tol) {
      converged <- TRUE
      if (verbose) message(sprintf("Converged after %d iterations", iter))
    }

    # Update parameters
    beta1_old <- beta1_new
    gamma1_old <- gamma1_new
    rho_old <- rho_new
  }

  if (!converged) {
    warning(sprintf("Did not converge after %d iterations", max_iter))
  }

  # Final estimates
  beta1_est <- beta1_new
  gamma1_est <- gamma1_new
  rho_est <- rho_new

  # Calculate fitted values and residuals
  control_est <- rho_est * (s1 / s2) * e2
  y1_fitted <- X %*% beta1_est + gamma1_est * Y2 + control_est
  residuals_final <- Y1 - y1_fitted

  # Bootstrap standard errors (simplified version)
  if (verbose) message("\nCalculating standard errors...")

  # Use the augmented regression for standard errors
  se_augmented <- summary(y1_augmented)$coefficients[, "Std. Error"]

  # Create results object
  results <- list(
    estimates = c(
      setNames(beta1_est, paste0("beta1_", 0:k)),
      gamma1 = gamma1_est,
      rho = rho_est
    ),
    se = c(
      setNames(se_augmented[1:(k + 1)], paste0("se_beta1_", 0:k)),
      se_gamma1 = se_augmented[k + 2],
      se_rho = se_augmented[k + 3]
    ),
    fitted_values = as.vector(y1_fitted),
    residuals = as.vector(residuals_final),
    control_function = as.vector(control_est),
    variance_functions = list(
      S1_squared = as.vector(s1_squared),
      S2_squared = as.vector(s2_squared),
      S1 = as.vector(s1),
      S2 = as.vector(s2)
    ),
    np_objects = list(
      bw1 = bw1,
      bw2 = bw2,
      np_fit1 = np_fit1,
      np_fit2 = np_fit2
    ),
    iterations = iter,
    converged = converged,
    n = n,
    k = k,
    bandwidth_method = bandwidth_method,
    call = match.call()
  )

  class(results) <- c("klein_vella_semipar", "list")

  if (verbose) {
    message("\nEstimation complete.")
    message(sprintf("gamma1 estimate: %.4f (SE: %.4f)", gamma1_est, se_augmented[k + 2]))
    message(sprintf("rho estimate: %.4f (SE: %.4f)", rho_est, se_augmented[k + 3]))
  }

  return(results)
}

#' Print method for semiparametric Klein & Vella results
#'
#' @param x A klein_vella_semipar object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.klein_vella_semipar <- function(x, ...) {
  cat("\nKlein & Vella Semiparametric Estimation Results\n")
  cat("==============================================\n")
  cat(sprintf("Sample size: %d\n", x$n))
  cat(sprintf("Bandwidth method: %s\n", x$bandwidth_method))
  cat(sprintf("Iterations: %d\n", x$iterations))
  cat(sprintf("Converged: %s\n\n", ifelse(x$converged, "Yes", "No")))

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

  # Bandwidth information
  if (!is.null(x$np_objects$bw1)) {
    cat("\nBandwidth Information:\n")
    cat("---------------------\n")
    cat("Y1 variance bandwidth:", format(x$np_objects$bw1$bw, digits = 4), "\n")
    cat("Y2 variance bandwidth:", format(x$np_objects$bw2$bw, digits = 4), "\n")
  }

  invisible(x)
}

#' Compare Klein & Vella methods
#'
#' @param data Data to use for comparison
#' @param true_values Optional list of true parameter values
#' @param methods Which methods to compare
#' @param verbose Whether to print progress
#'
#' @return Data frame with comparison results
#' @export
compare_klein_vella_methods <- function(data,
                                        true_values = NULL,
                                        methods = c("ols", "kv_parametric", "kv_semiparametric", "lewbel"),
                                        verbose = TRUE) {
  results <- list()

  # Identify variables
  y1_var <- "Y1"
  y2_var <- "Y2"
  x_vars <- names(data)[grepl("^X", names(data))]
  if (length(x_vars) == 0 && "X" %in% names(data)) {
    x_vars <- "X"
  }

  # OLS
  if ("ols" %in% methods) {
    if (verbose) message("Running OLS...")
    ols_model <- lm(
      as.formula(paste(
        y1_var, "~",
        paste(c(x_vars, y2_var), collapse = " + ")
      )),
      data = data
    )
    results$ols <- list(
      gamma1 = coef(ols_model)[y2_var],
      se = summary(ols_model)$coefficients[y2_var, "Std. Error"]
    )
  }

  # Klein & Vella parametric
  if ("kv_parametric" %in% methods) {
    if (verbose) message("Running Klein & Vella parametric...")
    kv_param <- klein_vella_parametric(data, verbose = FALSE)
    results$kv_parametric <- list(
      gamma1 = kv_param$estimates["gamma1"],
      se = kv_param$se["se_gamma1"],
      rho = kv_param$estimates["rho"]
    )
  }

  # Klein & Vella semiparametric
  if ("kv_semiparametric" %in% methods && requireNamespace("np", quietly = TRUE)) {
    if (verbose) message("Running Klein & Vella semiparametric...")
    kv_semi <- klein_vella_semiparametric(data, verbose = FALSE)
    results$kv_semiparametric <- list(
      gamma1 = kv_semi$estimates["gamma1"],
      se = kv_semi$se["se_gamma1"],
      rho = kv_semi$estimates["rho"]
    )
  }

  # Lewbel method
  if ("lewbel" %in% methods && requireNamespace("AER", quietly = TRUE)) {
    if (verbose) message("Running Lewbel 2SLS...")

    # Add Z variable if not present
    if (!"Z" %in% names(data)) {
      if ("X" %in% names(data)) {
        data$Z <- data$X^2 - mean(data$X^2)
      } else {
        data$Z <- data[[x_vars[1]]]^2 - mean(data[[x_vars[1]]]^2)
      }
    }

    # Lewbel estimation
    e2_lewbel <- residuals(lm(
      as.formula(paste(
        y2_var, "~",
        paste(x_vars, collapse = " + ")
      )),
      data = data
    ))
    iv_lewbel <- data$Z * e2_lewbel

    lewbel_model <- AER::ivreg(
      as.formula(paste(
        y1_var, "~",
        paste(c(x_vars, y2_var), collapse = " + "),
        "|",
        paste(c(x_vars, "iv_lewbel"), collapse = " + ")
      )),
      data = cbind(data, iv_lewbel = iv_lewbel)
    )

    results$lewbel <- list(
      gamma1 = coef(lewbel_model)[y2_var],
      se = summary(lewbel_model)$coefficients[y2_var, "Std. Error"]
    )
  }

  # Create comparison data frame
  comparison_df <- data.frame(
    Method = character(),
    Estimate = numeric(),
    StdError = numeric(),
    stringsAsFactors = FALSE
  )

  for (method in names(results)) {
    comparison_df <- rbind(comparison_df, data.frame(
      Method = method,
      Estimate = results[[method]]$gamma1,
      StdError = results[[method]]$se,
      stringsAsFactors = FALSE
    ))
  }

  # Add bias if true values provided
  if (!is.null(true_values) && "gamma1" %in% names(true_values)) {
    comparison_df$Bias <- comparison_df$Estimate - true_values$gamma1
    comparison_df$RelBias <- comparison_df$Bias / abs(true_values$gamma1)

    # Add true value row
    comparison_df <- rbind(data.frame(
      Method = "True Value",
      Estimate = true_values$gamma1,
      StdError = NA,
      Bias = 0,
      RelBias = 0,
      stringsAsFactors = FALSE
    ), comparison_df)
  }

  if (verbose) {
    message("\nComparison Results:")
    print(comparison_df, digits = 4)
  }

  return(comparison_df)
}

#' Run Klein & Vella Monte Carlo simulation
#'
#' @param config Klein & Vella configuration object
#' @param n_sims Number of simulations
#' @param methods Methods to compare
#' @param parallel Whether to use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param progress Whether to show progress bar
#' @param seed Random seed
#'
#' @return Data frame with Monte Carlo results
#' @export
run_klein_vella_monte_carlo <- function(config,
                                        n_sims = .hetid_const("N_SMALL"),
                                        methods = c("ols", "klein_vella_param"),
                                        parallel = FALSE,
                                        n_cores = NULL,
                                        progress = TRUE,
                                        seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Function to run one simulation
  run_one_sim <- function(sim_id) {
    # Generate data
    data_sim <- generate_klein_vella_data(config)

    # Results for this simulation
    sim_results <- data.frame(
      sim_id = sim_id,
      method = character(),
      gamma1_est = numeric(),
      gamma1_se = numeric(),
      converged = logical(),
      stringsAsFactors = FALSE
    )

    # OLS
    if ("ols" %in% methods) {
      ols_model <- lm(Y1 ~ X + Y2, data = data_sim)
      sim_results <- rbind(sim_results, data.frame(
        sim_id = sim_id,
        method = "ols",
        gamma1_est = coef(ols_model)["Y2"],
        gamma1_se = summary(ols_model)$coefficients["Y2", "Std. Error"],
        converged = TRUE,
        stringsAsFactors = FALSE
      ))
    }

    # Klein & Vella parametric
    if ("klein_vella_param" %in% methods) {
      tryCatch(
        {
          kv_result <- klein_vella_parametric(data_sim, verbose = FALSE)
          sim_results <- rbind(sim_results, data.frame(
            sim_id = sim_id,
            method = "klein_vella_param",
            gamma1_est = kv_result$estimates["gamma1"],
            gamma1_se = kv_result$se["se_gamma1"],
            converged = kv_result$convergence == 0,
            stringsAsFactors = FALSE
          ))
        },
        error = function(e) {
          sim_results <- rbind(sim_results, data.frame(
            sim_id = sim_id,
            method = "klein_vella_param",
            gamma1_est = NA,
            gamma1_se = NA,
            converged = FALSE,
            stringsAsFactors = FALSE
          ))
        }
      )
    }

    # Lewbel
    if ("lewbel" %in% methods && requireNamespace("AER", quietly = TRUE)) {
      tryCatch(
        {
          # Add Z if needed
          if (!"Z" %in% names(data_sim)) {
            data_sim$Z <- data_sim$X^2 - mean(data_sim$X^2)
          }

          e2 <- residuals(lm(Y2 ~ X, data = data_sim))
          iv <- data_sim$Z * e2
          lewbel_model <- AER::ivreg(Y1 ~ X + Y2 | X + iv,
            data = cbind(data_sim, iv = iv)
          )

          sim_results <- rbind(sim_results, data.frame(
            sim_id = sim_id,
            method = "lewbel",
            gamma1_est = coef(lewbel_model)["Y2"],
            gamma1_se = summary(lewbel_model)$coefficients["Y2", "Std. Error"],
            converged = TRUE,
            stringsAsFactors = FALSE
          ))
        },
        error = function(e) {
          sim_results <- rbind(sim_results, data.frame(
            sim_id = sim_id,
            method = "lewbel",
            gamma1_est = NA,
            gamma1_se = NA,
            converged = FALSE,
            stringsAsFactors = FALSE
          ))
        }
      )
    }

    sim_results
  }

  # Run simulations
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }

    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, c(
      "config", "methods", "generate_klein_vella_data",
      "klein_vella_parametric"
    ),
    envir = environment()
    )

    results_list <- parallel::parLapply(cl, 1:n_sims, run_one_sim)
    parallel::stopCluster(cl)
  } else {
    # Sequential processing
    results_list <- list()

    if (progress) {
      pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
    }

    for (i in 1:n_sims) {
      results_list[[i]] <- run_one_sim(i)

      if (progress) {
        setTxtProgressBar(pb, i)
      }
    }

    if (progress) {
      close(pb)
    }
  }

  # Combine results
  results <- do.call(rbind, results_list)

  return(results)
}
