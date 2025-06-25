#' Rigobon (2003) Regime-Based Heteroskedasticity Identification
#'
#' This file implements the Rigobon (2003) procedure for using discrete
#' regime indicators to generate instruments for identification in triangular
#' systems with endogenous regressors. The method exploits heteroskedasticity
#' across different regimes (e.g., policy periods, market conditions).
#'
#' @references
#' Rigobon, R. (2003). "Identification Through Heteroskedasticity."
#' The Review of Economics and Statistics, 85(4), 777-792.
#'
#' @details
#' The Rigobon method is a special case of Lewbel's (2012) approach where
#' the heteroskedasticity drivers are discrete regime indicators rather than
#' continuous functions of exogenous variables.

#' Run Complete Rigobon Analysis
#'
#' This is the main function that performs a complete Rigobon (2003)
#' heteroskedasticity-based identification analysis. It combines data generation,
#' estimation, and diagnostic testing in a single workflow.
#'
#' @param n_obs Integer. Sample size (default: 1000).
#' @param params List. Parameters for data generation. If NULL, uses default
#'   parameters suitable for demonstration.
#' @param data Data.frame. Optional. Pre-existing data with regime indicators.
#'   If provided, skips data generation.
#' @template param-regime-var
#' @param endog_var Character. Name of endogenous variable (default: "Y2").
#' @template param-x-vars
#' @template param-verbose
#' @param return_all Logical. Whether to return all intermediate results
#'   (default: FALSE).
#'
#' @return A list containing:
#'   \itemize{
#'     \item estimates: Data frame comparing OLS and Rigobon 2SLS estimates
#'     \item diagnostics: Heteroskedasticity test results and first-stage F-stats
#'     \item data: The data used (generated or provided)
#'     \item models: Fitted model objects (if return_all = TRUE)
#'   }
#'
#' @examples
#' \dontrun{
#' # Quick analysis with default parameters
#' results <- run_rigobon_analysis()
#'
#' # Custom parameters for stronger identification
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0,
#'   regime_probs = c(0.3, 0.7),
#'   sigma2_regimes = c(1.0, 3.0) # Large difference in variances
#' )
#' results <- run_rigobon_analysis(n_obs = 2000, params = params)
#'
#' # Using existing data
#' # Assume you have data with a regime indicator
#' results <- run_rigobon_analysis(data = my_data, regime_var = "period")
#' }
#'
#' @template references-rigobon
#'
#' @seealso \code{\link{generate_rigobon_data}}, \code{\link{run_rigobon_estimation}},
#'   \code{\link{validate_rigobon_assumptions}}
#'
#' @export
run_rigobon_analysis <- function(n_obs = .hetid_const("N_DEFAULT"),
                                 params = NULL,
                                 data = NULL,
                                 regime_var = "regime",
                                 endog_var = "Y2",
                                 exog_vars = "Xk",
                                 verbose = TRUE,
                                 return_all = FALSE) {
  # Step 1: Data preparation
  if (is.null(data)) {
    # Generate data if not provided
    if (is.null(params)) {
      # Default parameters for demonstration
      params <- list(
        beta1_0 = 0.5,
        beta1_1 = 1.5,
        gamma1 = -0.8, # True parameter to estimate
        beta2_0 = 1.0,
        beta2_1 = -1.0,
        alpha1 = -0.5,
        alpha2 = 1.0,
        regime_probs = c(0.4, 0.6),
        sigma2_regimes = c(1.0, 2.5)
      )
    }

    if (verbose) {
      cat("Generating Rigobon data with", n_obs, "observations...\n")
    }

    data <- generate_rigobon_data(n_obs, params)
  } else {
    if (verbose) {
      cat("Using provided data with", nrow(data), "observations...\n")
    }

    # Extract true gamma1 if available in params
    if (!is.null(params) && "gamma1" %in% names(params)) {
      true_gamma1 <- params$gamma1
    } else {
      true_gamma1 <- NA
    }
  }

  # Step 2: Run estimation with diagnostics
  if (verbose) {
    cat("\nRunning Rigobon estimation...\n")
  }

  estimation_results <- run_rigobon_estimation(
    data = data,
    endog_var = endog_var,
    exog_vars = exog_vars,
    regime_var = regime_var,
    return_diagnostics = TRUE
  )

  # Step 3: Prepare summary of estimates
  estimates_df <- data.frame(
    Method = c("OLS (Biased)", "Rigobon 2SLS"),
    Estimate = c(
      estimation_results$ols$estimates["gamma1"],
      estimation_results$tsls$estimates["gamma1"]
    ),
    StdError = c(
      estimation_results$ols$se["gamma1"],
      estimation_results$tsls$se["gamma1"]
    ),
    stringsAsFactors = FALSE
  )

  # Add bias if true value is known
  if (!is.null(params) && "gamma1" %in% names(params)) {
    estimates_df$TrueValue <- params$gamma1
    estimates_df$Bias <- estimates_df$Estimate - params$gamma1
    estimates_df$RelativeBias <- estimates_df$Bias / abs(params$gamma1) * 100
  }

  # Step 4: Prepare diagnostics summary
  diagnostics <- list(
    heteroskedasticity_test = estimation_results$heteroskedasticity_test,
    first_stage_F = estimation_results$first_stage_f_stats,
    regime_proportions = estimation_results$regime_props,
    n_regimes = length(estimation_results$regime_props)
  )

  # Step 5: Print results if verbose
  if (verbose) {
    cat("\n========== RIGOBON ANALYSIS RESULTS ==========\n")

    cat("\nEstimation Results:\n")
    print(estimates_df, row.names = FALSE)

    cat("\nFirst-Stage F-Statistics:\n")
    print(estimation_results$first_stage_f_stats)

    cat("\nHeteroskedasticity Test:\n")
    cat(sprintf("  F-statistic: %.2f\n", diagnostics$heteroskedasticity_test$F_stat))
    cat(sprintf("  p-value: %.4f\n", diagnostics$heteroskedasticity_test$p_value))
    cat(sprintf("  %s\n", diagnostics$heteroskedasticity_test$interpretation))

    cat("\nRegime Distribution:\n")
    regime_table <- as.data.frame(diagnostics$regime_proportions)
    names(regime_table) <- "Proportion"
    print(regime_table)

    # Instrument strength assessment
    avg_f <- mean(estimation_results$first_stage_f_stats)
    if (avg_f < .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD")) {
      cat(
        "\nWARNING: Average first-stage F =", round(avg_f, 2),
        "< 10 (weak instruments)\n"
      )
    } else {
      cat(
        "\nInstrument strength: Average F =", round(avg_f, 2),
        "(strong instruments)\n"
      )
    }
  }

  # Step 6: Prepare return object
  results <- list(
    estimates = estimates_df,
    diagnostics = diagnostics,
    data = data
  )

  if (return_all) {
    results$models <- list(
      ols = estimation_results$ols$model,
      tsls = estimation_results$tsls$model
    )
    results$instruments <- estimation_results$instruments
  }

  invisible(results)
}


#' Validate Rigobon Assumptions
#'
#' Tests whether the data satisfies the key assumptions required for
#' Rigobon's (2003) identification strategy.
#'
#' @param data Data.frame. Must contain Y1, Y2, regime, and X variables.
#' @param regime_var Character. Name of regime variable (default: "regime").
#' @param exog_vars Character vector. Names of exogenous variables.
#' @template param-verbose
#'
#' @return A list containing test results:
#'   \itemize{
#'     \item regime_heteroskedasticity: Test for different variances across regimes
#'     \item covariance_restriction: Test that Cov(Z, e1*e2) = 0
#'     \item constant_covariance: Test that Cov(e1, e2) is constant across regimes
#'     \item all_valid: Logical indicating if all assumptions are satisfied
#'   }
#'
#' @details
#' The function tests three key assumptions:
#' 1. Heteroskedasticity across regimes in at least one equation
#' 2. The covariance restriction (centered regime dummies uncorrelated with
#'    error product)
#' 3. Constant covariance between errors across regimes
#'
#' @examples
#' \dontrun{
#' # Generate test data
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0,
#'   regime_probs = c(0.4, 0.6),
#'   sigma2_regimes = c(1.0, 2.5)
#' )
#' data <- generate_rigobon_data(1000, params)
#'
#' # Validate assumptions
#' validation <- validate_rigobon_assumptions(data)
#' }
#'
#' @template references-rigobon
#'
#' @export
validate_rigobon_assumptions <- function(data,
                                         regime_var = "regime",
                                         exog_vars = "Xk",
                                         verbose = TRUE) {
  if (verbose) {
    cat("\n--- Validating Rigobon Assumptions ---\n")
  }

  # Get residuals from both equations
  y2_formula <- as.formula(paste("Y2 ~", paste(exog_vars, collapse = " + ")))
  y1_formula <- as.formula(paste("Y1 ~ Y2 +", paste(exog_vars, collapse = " + ")))

  e2 <- residuals(lm(y2_formula, data = data))
  e1 <- residuals(lm(y1_formula, data = data)) # Note: biased due to endogeneity

  # Get regime indicators
  regimes <- unique(data[[regime_var]])
  n_regimes <- length(regimes)

  # Test 1: Heteroskedasticity across regimes
  het_test_results <- list()

  for (eq in c("equation1", "equation2")) {
    errors <- if (eq == "equation1") e1 else e2
    variances <- numeric(n_regimes)

    for (i in seq_along(regimes)) {
      regime_mask <- data[[regime_var]] == regimes[i]
      variances[i] <- var(errors[regime_mask])
    }

    # Bartlett's test for equal variances
    regime_factor <- factor(data[[regime_var]])
    bartlett_test <- bartlett.test(errors ~ regime_factor)

    het_test_results[[eq]] <- list(
      variances = variances,
      p_value = bartlett_test$p.value,
      significant = bartlett_test$p.value < .hetid_const("ALPHA_DEFAULT")
    )
  }

  # Test 2: Covariance restriction
  # Create centered regime dummies
  cov_tests <- list()

  for (i in seq_along(regimes)) {
    dummy <- as.numeric(data[[regime_var]] == regimes[i])
    z_centered <- dummy - mean(dummy)

    # Test Cov(Z, e1*e2) = 0
    cov_val <- cov(z_centered, e1 * e2)
    n <- length(z_centered)
    se_cov <- sd(z_centered * e1 * e2) / sqrt(n)
    t_stat <- cov_val / se_cov
    p_val <- 2 * (1 - pnorm(abs(t_stat)))

    cov_tests[[paste0("regime", i)]] <- list(
      covariance = cov_val,
      t_stat = t_stat,
      p_value = p_val,
      valid = p_val > .hetid_const("ALPHA_DEFAULT")
    )
  }

  # Test 3: Constant covariance between errors
  covariances <- numeric(n_regimes)

  for (i in seq_along(regimes)) {
    regime_mask <- data[[regime_var]] == regimes[i]
    covariances[i] <- cov(e1[regime_mask], e2[regime_mask])
  }

  # Simple test: coefficient of variation
  cv_covariances <- sd(covariances) / abs(mean(covariances))
  constant_cov_valid <- cv_covariances < .hetid_const("COV_VARIATION_THRESHOLD") # Less than 20% variation

  # Compile results
  results <- list(
    regime_heteroskedasticity = het_test_results,
    covariance_restriction = cov_tests,
    constant_covariance = list(
      covariances = covariances,
      cv = cv_covariances,
      valid = constant_cov_valid
    ),
    all_valid = (
      any(sapply(het_test_results, function(x) x$significant)) &&
        all(sapply(cov_tests, function(x) x$valid)) &&
        constant_cov_valid
    )
  )

  # Print summary if verbose
  if (verbose) {
    cat("\n1. Heteroskedasticity across regimes:\n")
    for (eq in names(het_test_results)) {
      cat(sprintf(
        "   %s: p-value = %.4f %s\n",
        eq,
        het_test_results[[eq]]$p_value,
        ifelse(het_test_results[[eq]]$significant,
          "(significant)", "(not significant)"
        )
      ))
    }

    cat("\n2. Covariance restriction Cov(Z, e1*e2) = 0:\n")
    all_valid <- TRUE
    for (regime in names(cov_tests)) {
      cat(sprintf(
        "   %s: p-value = %.4f %s\n",
        regime,
        cov_tests[[regime]]$p_value,
        ifelse(cov_tests[[regime]]$valid, "(valid)", "(violated)")
      ))
      all_valid <- all_valid && cov_tests[[regime]]$valid
    }

    cat("\n3. Constant covariance between errors:\n")
    cat(sprintf(
      "   Coefficient of variation: %.2f %s\n",
      cv_covariances,
      ifelse(constant_cov_valid, "(acceptable)", "(too variable)")
    ))

    cat("\nOverall: ")
    if (results$all_valid) {
      cat("All Rigobon assumptions appear to be satisfied.\n")
    } else {
      cat("Some assumptions may be violated. Check individual tests.\n")
    }
  }

  invisible(results)
}


#' Compare Rigobon with Other Methods
#'
#' Compares Rigobon's regime-based identification with OLS and standard
#' Lewbel identification (if applicable).
#'
#' @param data Data.frame. Must contain required variables for all methods.
#' @param true_gamma1 Numeric. Optional. True value of the endogenous parameter.
#' @param methods Character vector. Methods to compare (default:
#'   c("OLS", "Rigobon", "Lewbel")).
#' @template param-verbose
#'
#' @return A data frame comparing the methods with columns for estimates,
#'   standard errors, bias (if true value provided), and method-specific
#'   diagnostics.
#'
#' @examples
#' \dontrun{
#' # Generate data with known true parameter
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0,
#'   regime_probs = c(0.4, 0.6),
#'   sigma2_regimes = c(1.0, 2.5)
#' )
#' data <- generate_rigobon_data(1000, params)
#'
#' # Compare methods
#' comparison <- compare_rigobon_methods(data, true_gamma1 = params$gamma1)
#' }
#'
#' @template references-rigobon
#'
#' @seealso \code{\link{run_rigobon_estimation}}, \code{\link{run_single_lewbel_simulation}}
#'
#' @export
compare_rigobon_methods <- function(data,
                                    true_gamma1 = NULL,
                                    methods = c("OLS", "Rigobon", "Lewbel"),
                                    verbose = TRUE) {
  results <- list()

  # Method 1: OLS (biased baseline)
  if ("OLS" %in% methods) {
    ols_fit <- lm(Y1 ~ Y2 + Xk, data = data)
    results$OLS <- list(
      estimate = as.numeric(coef(ols_fit)[.hetid_const("columns$Y2")]),
      se = as.numeric(summary(ols_fit)$coefficients[.hetid_const("columns$Y2"), .hetid_const("columns$STD_ERROR")]),
      method_info = "Standard OLS (ignores endogeneity)"
    )
  }

  # Method 2: Rigobon
  if ("Rigobon" %in% methods && "regime" %in% names(data)) {
    rigobon_results <- run_rigobon_estimation(data, return_diagnostics = TRUE)
    results$Rigobon <- list(
      estimate = as.numeric(rigobon_results$tsls$estimates["gamma1"]),
      se = as.numeric(rigobon_results$tsls$se["gamma1"]),
      first_stage_F = mean(rigobon_results$first_stage_f_stats),
      method_info = sprintf(
        "Rigobon 2SLS (%d regimes)",
        length(rigobon_results$regime_props)
      )
    )
  }

  # Method 3: Standard Lewbel (if Z variable exists)
  if ("Lewbel" %in% methods && "Z" %in% names(data)) {
    # Construct Lewbel instrument
    e2_hat <- residuals(lm(Y2 ~ Xk, data = data))
    lewbel_iv <- (data$Z - mean(data$Z)) * e2_hat

    # Run 2SLS
    if (requireNamespace(.hetid_const("packages$IVREG"), quietly = TRUE)) {
      iv_formula <- Y1 ~ Y2 + Xk | Xk + lewbel_iv
      lewbel_fit <- ivreg::ivreg(iv_formula, data = data)

      # First-stage F
      first_stage <- lm(Y2 ~ Xk + lewbel_iv, data = data)
      f_stat <- as.numeric(summary(first_stage)$fstatistic[1])

      results$Lewbel <- list(
        estimate = as.numeric(coef(lewbel_fit)[.hetid_const("columns$Y2")]),
        se = as.numeric(summary(lewbel_fit)$coefficients[
          .hetid_const("columns$Y2"), .hetid_const("columns$STD_ERROR")
        ]),
        first_stage_F = f_stat,
        method_info = "Standard Lewbel (continuous Z)"
      )
    }
  }

  # Compile comparison table - ensure all data frames have same columns
  comparison_list <- lapply(names(results), function(method) {
    res <- results[[method]]

    # Create data frame with all columns in consistent order
    df <- data.frame(
      Method = method,
      Estimate = as.numeric(res$estimate),
      StdError = as.numeric(res$se),
      TrueValue = if (!is.null(true_gamma1)) as.numeric(true_gamma1) else NA_real_,
      Bias = if (!is.null(true_gamma1)) as.numeric(res$estimate - true_gamma1) else NA_real_,
      RelativeBias_pct = if (!is.null(true_gamma1)) {
        as.numeric((res$estimate - true_gamma1) / abs(true_gamma1) * 100)
      } else {
        NA_real_
      },
      FirstStageF = if (!is.null(res$first_stage_F)) as.numeric(res$first_stage_F) else NA_real_,
      Details = res$method_info,
      stringsAsFactors = FALSE
    )

    df
  })

  # Now rbind all data frames which have the same structure
  comparison_df <- do.call(rbind, comparison_list)

  rownames(comparison_df) <- NULL

  # Print comparison if verbose
  if (verbose) {
    cat("\n========== METHOD COMPARISON ==========\n")
    print(comparison_df, digits = 4)

    if (!is.null(true_gamma1)) {
      cat(sprintf("\nTrue gamma1 = %.4f\n", true_gamma1))

      # Find best method by absolute bias
      best_method <- comparison_df$Method[which.min(abs(comparison_df$Bias))]
      cat(sprintf("Best method by bias: %s\n", best_method))
    }
  }

  invisible(comparison_df)
}
