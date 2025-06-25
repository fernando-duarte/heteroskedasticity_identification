#' Prono (2014) Heteroskedasticity-Based Identification with GARCH
#'
#' This file implements the Prono (2014) procedure for using conditional
#' heteroskedasticity (GARCH) to generate instruments for identification
#' in triangular systems with endogenous regressors.
#'
#' @references
#' Prono, T. (2014). "The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor." Journal of Applied Econometrics,
#' 29(5), 800-824.

#' Generate time series data for Prono's triangular model
#'
#' Generates data matching Prono (2014) asset pricing application with
#' returns in percent (like the original paper).
#'
#' @param n Sample size
#' @param beta1 Coefficient vector for X in first equation (portfolio return equation).
#'   Default c(0.05, 0.01) gives realistic portfolio returns in percent.
#' @param beta2 Coefficient vector for X in second equation (market return equation).
#'   Default c(0.097, -0.005) gives mean market excess return of 0.097% matching Prono.
#' @param gamma1 Coefficient on Y2 in first equation (the "beta" in asset pricing)
#' @param k Number of exogenous variables (excluding constant)
#' @param garch_params List with GARCH parameters: omega, alpha, beta.
#'   Default values give realistic volatility clustering for weekly returns.
#' @param sigma1 Standard deviation of epsilon1 in percent (portfolio idiosyncratic risk)
#' @param rho Correlation between epsilon1 and epsilon2 (endogeneity)
#' @param seed Random seed
#'
#' @return Data frame with generated variables (Y1 and Y2 are in percent)
#'
#' @references
#' Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
#' 29(5), 800-824. \doi{10.1002/jae.2387}
#'
#' @seealso
#' \code{\link{run_single_prono_simulation}} for running a single simulation
#' \code{\link{create_prono_config}} for default configuration
#' \code{\link{run_prono_monte_carlo}} for Monte Carlo analysis
#'
#' @export
generate_prono_data <- function(n = .hetid_const("N_SMALL"),
                                beta1 = c(0.05, 0.01),
                                beta2 = c(0.097, -0.005),
                                gamma1 = 1.0,
                                k = 1,
                                garch_params = list(omega = .hetid_const("GARCH$OMEGA_DEFAULT"), alpha = .hetid_const("GARCH$ALPHA_DEFAULT"), beta = .hetid_const("GARCH$BETA_DEFAULT")),
                                sigma1 = 1.5,
                                rho = 0.3,
                                seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Validate GARCH parameters
  if (garch_params$alpha + garch_params$beta >= 1) {
    stop("GARCH parameters must satisfy alpha + beta < 1 for stationarity")
  }

  # Generate exogenous variables
  x <- matrix(rnorm(n * k), nrow = n, ncol = k)
  x_with_const <- cbind(1, x)
  colnames(x_with_const) <- c("const", paste0("X", seq_len(k)))

  # Generate GARCH(1,1) errors for epsilon2
  eps2 <- numeric(n)
  sigma2_sq <- numeric(n)

  # Initialize
  sigma2_sq[1] <- garch_params$omega / (1 - garch_params$alpha - garch_params$beta)

  # Generate innovations
  z1 <- rnorm(n)
  z2 <- rnorm(n)

  # Generate epsilon2 with GARCH structure
  for (t in 1:n) {
    if (t > 1) {
      sigma2_sq[t] <- garch_params$omega +
        garch_params$alpha * eps2[t - 1]^2 +
        garch_params$beta * sigma2_sq[t - 1]
    }
    eps2[t] <- sqrt(sigma2_sq[t]) * z2[t]
  }

  # Generate correlated epsilon1
  eps1 <- sigma1 * (rho * z2 + sqrt(1 - rho^2) * z1)

  # Generate endogenous variables
  y2 <- x_with_const %*% beta2 + eps2
  y1 <- x_with_const %*% beta1 + gamma1 * y2 + eps1

  # Create data frame
  df <- data.frame(
    Y1 = as.vector(y1),
    Y2 = as.vector(y2),
    x_with_const,
    eps1 = eps1,
    eps2 = eps2,
    sigma2_sq = sigma2_sq,
    time = 1:n
  )

  df
}

#' Run single Prono simulation with GARCH-based instruments
#'
#' @param config Configuration list with simulation parameters
#' @param return_details If TRUE, return detailed results
#'
#' @return List with estimation results including gamma1_true, gamma1_ols, gamma1_iv,
#' standard errors, biases, F-statistic, and optionally full model objects
#'
#' @references
#' Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
#' 29(5), 800-824. \doi{10.1002/jae.2387}
#'
#' @seealso
#' \code{\link{generate_prono_data}} for data generation
#' \code{\link{run_prono_monte_carlo}} for Monte Carlo analysis
#' \code{\link{prono_gmm}} for GMM estimation
#'
#' @export
run_single_prono_simulation <- function(config, return_details = FALSE) {
  # Generate data
  df <- generate_prono_data(
    n = config$n,
    beta1 = config$beta1,
    beta2 = config$beta2,
    gamma1 = config$gamma1,
    k = config$k,
    garch_params = config$garch_params,
    sigma1 = config$sigma1,
    rho = config$rho,
    seed = config$seed
  )

  # Extract variables
  n <- nrow(df)
  k <- config$k
  x_vars <- paste0("X", seq_len(k))

  # True parameters
  true_gamma1 <- config$gamma1

  # Step 1: OLS estimation (biased)
  ols_formula <- as.formula(paste("Y1 ~ Y2 +", paste(x_vars, collapse = " + ")))
  ols_fit <- lm(ols_formula, data = df)
  gamma1_ols <- coef(ols_fit)["Y2"]
  se_ols <- extract_se_lm(ols_fit, config$se_type)["Y2"]

  # Step 2: Estimate second equation and get residuals
  second_eq_formula <- as.formula(paste("Y2 ~", paste(x_vars, collapse = " + ")))
  second_eq_fit <- lm(second_eq_formula, data = df)
  e2_hat <- residuals(second_eq_fit)

  # Step 3: Fit GARCH(1,1) model to residuals
  sigma2_sq_hat <- NULL
  garch_fit <- NULL

  tryCatch(
    {
      # Check if tsgarch is available
      if (!requireNamespace(.hetid_const("packages$TSGARCH"), quietly = TRUE)) {
        stop(sprintf(
          .hetid_const("messages$PACKAGE_REQUIRED"),
          .hetid_const("packages$TSGARCH"),
          .hetid_const("packages$TSGARCH")
        ))
      }

      # Convert to xts object as required by tsgarch
      dates <- as.Date("2000-01-01") + seq_along(e2_hat) - 1
      e2_xts <- xts::xts(e2_hat, order.by = dates)

      # Specify GARCH(1,1) model using tsgarch
      garch_spec <- tsgarch::garch_modelspec(
        y = e2_xts,
        model = "garch",
        order = c(1, 1),
        constant = TRUE,
        distribution = "norm"
      )

      # Fit GARCH model
      # Note: tsgarch may produce "NaNs produced" warnings when fitting GARCH to data
      # with no ARCH effects (alpha ≈ 0). This is expected behavior indicating that
      # the conditional variance is essentially constant, making the Hessian matrix
      # singular when computing standard errors. The warning is harmless and does not
      # affect the validity of the conditional variance estimates.
      garch_fit <- tsmethods::estimate(garch_spec)

      # Extract conditional variances (sigma returns standard deviations)
      sigma2_sq_hat <- as.numeric(sigma(garch_fit))^2
    },
    error = function(e) {
      warning("GARCH fitting failed, using squared residuals as proxy: ", e$message)
      # Fallback: use squared residuals as proxy for conditional variance
      sigma2_sq_hat <- e2_hat^2
    }
  )

  # Ensure we have sigma2_sq_hat
  if (is.null(sigma2_sq_hat)) {
    sigma2_sq_hat <- e2_hat^2
  }

  # Step 4: Construct Prono instrument
  # z_t = fitted conditional variance (demeaned)
  z_t <- sigma2_sq_hat - mean(sigma2_sq_hat)
  prono_iv <- z_t * e2_hat

  # Add instrument to data frame
  df$prono_iv <- prono_iv

  # Step 5: 2SLS with Prono instrument
  iv_formula <- as.formula(
    paste(
      "Y1 ~ Y2 +", paste(x_vars, collapse = " + "),
      "| ", paste(x_vars, collapse = " + "), " + prono_iv"
    )
  )

  # Try different IV packages
  iv_fit <- NULL
  iv_package <- NULL

  # Try ivreg package first
  if (requireNamespace(.hetid_const("packages$IVREG"), quietly = TRUE)) {
    tryCatch(
      {
        iv_fit <- ivreg::ivreg(iv_formula, data = df)
        iv_package <- "ivreg"
      },
      error = function(e) {
        iv_fit <- NULL
      }
    )
  }

  # Fallback to AER if ivreg fails
  if (is.null(iv_fit) && requireNamespace("AER", quietly = TRUE)) {
    tryCatch(
      {
        iv_fit <- AER::ivreg(iv_formula, data = df)
        iv_package <- "AER"
      },
      error = function(e) {
        stop("IV estimation failed with both ivreg and AER packages")
      }
    )
  }

  if (is.null(iv_fit)) {
    stop("Neither ivreg nor AER package is available for IV estimation")
  }

  # Extract coefficient and standard error
  gamma1_iv <- coef(iv_fit)["Y2"]
  se_iv <- extract_se_ivreg(iv_fit, config$se_type)["Y2"]

  # First-stage F-statistic
  first_stage_formula <- as.formula(
    paste("Y2 ~", paste(x_vars, collapse = " + "), " + prono_iv")
  )
  first_stage <- lm(first_stage_formula, data = df)

  # F-stat for excluded instrument
  restricted_formula <- as.formula(paste("Y2 ~", paste(x_vars, collapse = " + ")))
  restricted <- lm(restricted_formula, data = df)

  f_stat <- tryCatch(
    {
      anova(restricted, first_stage, test = "F")[2, "F"]
    },
    error = function(e) NA
  )

  # Compile results
  results <- list(
    gamma1_true = true_gamma1,
    gamma1_ols = as.numeric(gamma1_ols),
    gamma1_iv = as.numeric(gamma1_iv),
    se_ols = se_ols,
    se_iv = se_iv,
    bias_ols = as.numeric(gamma1_ols - true_gamma1),
    bias_iv = as.numeric(gamma1_iv - true_gamma1),
    f_stat = f_stat,
    n = n
  )

  if (return_details) {
    results$data <- df
    results$iv_fit <- iv_fit
    results$ols_fit <- ols_fit
    results$garch_fit <- garch_fit
  }

  results
}

#' Run Prono Monte Carlo simulation
#'
#' @param config Configuration list
#' @param n_sims Number of simulations
#' @param parallel Whether to use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param progress Whether to show progress bar
#'
#' @return Data frame with simulation results
#'
#' @references
#' Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
#' 29(5), 800-824. \doi{10.1002/jae.2387}
#'
#' @seealso
#' \code{\link{run_single_prono_simulation}} for single simulation
#' \code{\link{create_prono_config}} for configuration setup
#'
#' @examples
#' \dontrun{
#' # Time-consuming Monte Carlo simulation
#' # For actual research, use n_sims = 1000+
#' config <- create_prono_config(n = 500)
#' mc_results <- run_prono_monte_carlo(config, n_sims = 100)
#' }
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
run_prono_monte_carlo <- function(config,
                                  n_sims = .hetid_const("N_DEFAULT"),
                                  parallel = FALSE,
                                  n_cores = NULL,
                                  progress = TRUE) {
  messager("Starting Prono Monte Carlo simulation", v = config$verbose)
  messager(
    sprintf(
      "Parameters: n=%d, k=%d, gamma1=%.2f, n_sims=%d",
      config$n, config$k, config$gamma1, n_sims
    ),
    v = config$verbose
  )

  # Generate seeds for reproducibility
  seeds <- if (!is.null(config$seed)) {
    set.seed(config$seed)
    sample.int(.Machine$integer.max / 2, n_sims)
  } else {
    sample.int(.Machine$integer.max / 2, n_sims)
  }

  # Run simulations
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    messager(sprintf("Running in parallel with %d cores", n_cores),
      v = config$verbose
    )

    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export necessary objects
    parallel::clusterExport(cl, c(
      "run_single_prono_simulation",
      "generate_prono_data",
      "extract_se_lm",
      "extract_se_ivreg"
    ),
    envir = environment()
    )

    results_list <- parallel::parLapply(cl, seeds, function(s) {
      config$seed <- s
      run_single_prono_simulation(config)
    })
  } else {
    results_list <- vector("list", n_sims)

    if (progress) {
      pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
    }

    for (i in seq_len(n_sims)) {
      config$seed <- seeds[i]
      results_list[[i]] <- tryCatch(
        run_single_prono_simulation(config),
        error = function(e) {
          warning(sprintf("Simulation %d failed: %s", i, e$message))
          NULL
        }
      )

      if (progress) setTxtProgressBar(pb, i)
    }

    if (progress) close(pb)
  }

  # Remove failed simulations
  results_list <- Filter(Negate(is.null), results_list)

  # Convert to data frame
  results_df <- do.call(rbind, lapply(results_list, as.data.frame))

  messager(sprintf("Completed %d successful simulations", nrow(results_df)),
    v = config$verbose
  )

  results_df
}

#' Create default configuration for Prono simulations
#'
#' Creates configuration matching Prono (2014) with returns in percent.
#'
#' @param n Sample size
#' @param k Number of exogenous variables
#' @param ... Additional parameters to override defaults
#'
#' @return Configuration list with parameters scaled for percent returns
#' @export
create_prono_config <- function(n = .hetid_const("N_SMALL"), k = 1, ...) {
  # Default configuration for percent-scale returns matching Prono (2014)
  config <- list(
    # Sample size
    n = n,

    # Model parameters (scaled for percent returns)
    k = k,
    beta1 = c(0.05, rep(0.01, k)), # Portfolio return equation
    beta2 = c(0.097, rep(-0.005, k)), # Market return equation (mean = 0.097%)
    gamma1 = 1.0, # True beta (unitless)

    # GARCH parameters for percent returns
    garch_params = list(
      omega = .hetid_const("GARCH$OMEGA_DEFAULT"), # For ~2% weekly volatility: 0.2/(1-0.1-0.85) = 4, sqrt(4) = 2%
      alpha = .hetid_const("GARCH$ALPHA_DEFAULT"), # ARCH coefficient
      beta = .hetid_const("GARCH$BETA_DEFAULT") # GARCH coefficient (alpha + beta < 1)
    ),

    # Error parameters
    sigma1 = 1.5, # Portfolio idiosyncratic volatility in percent
    rho = 0.3, # Correlation (endogeneity)

    # Estimation options
    se_type = "asymptotic", # Can be "asymptotic" or "finite"

    # Other options
    seed = 123,
    verbose = TRUE
  )

  # Override with user-supplied values
  user_args <- list(...)
  for (arg in names(user_args)) {
    config[[arg]] <- user_args[[arg]]
  }

  config
}

#' Run Prono demonstration
#'
#' @param n Sample size
#' @param print_results Whether to print results
#'
#' @return Results from single simulation (invisibly)
#'
#' @references
#' Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
#' 29(5), 800-824. \doi{10.1002/jae.2387}
#'
#' @seealso
#' \code{\link{run_single_prono_simulation}} for the underlying simulation
#' \code{\link{create_prono_config}} for configuration
#' \code{\link{run_prono_monte_carlo}} for full Monte Carlo analysis
#'
#' @examples
#' \dontrun{
#' # Quick demonstration with reduced sample size
#' run_prono_demo(n = 200, print_results = TRUE)
#' }
#'
#' @export
run_prono_demo <- function(n = .hetid_const("N_SMALL"), print_results = TRUE) {
  cat("==================================================\n")
  cat("Prono (2014) GARCH-Based Identification Demo\n")
  cat("==================================================\n\n")

  # Create configuration
  config <- create_prono_config(n = n, verbose = FALSE)

  # Run single simulation with details
  results <- run_single_prono_simulation(config, return_details = TRUE)

  if (print_results) {
    cat("Model: Y1 = X'beta1 + gamma1*Y2 + epsilon1\n")
    cat("       Y2 = X'beta2 + epsilon2\n")
    cat("where epsilon2 follows GARCH(1,1)\n\n")

    cat(sprintf("True gamma1: %.3f\n", results$gamma1_true))
    cat(sprintf(
      "OLS estimate: %.3f (bias: %.3f)\n",
      results$gamma1_ols, results$bias_ols
    ))
    cat(sprintf(
      "Prono IV estimate: %.3f (bias: %.3f)\n",
      results$gamma1_iv, results$bias_iv
    ))
    cat(sprintf("First-stage F-statistic: %.2f\n", results$f_stat))

    # Check if GARCH fit is available
    if (!is.null(results$garch_fit)) {
      cat("\nGARCH(1,1) parameters:\n")
      garch_coef <- coef(results$garch_fit)
      cat(sprintf("  omega: %.4f\n", garch_coef[names(garch_coef) == "omega"]))
      cat(sprintf("  alpha: %.4f\n", garch_coef[names(garch_coef) == "alpha1"]))
      cat(sprintf("  beta:  %.4f\n", garch_coef[names(garch_coef) == "beta1"]))
    }
  }

  invisible(results)
}
