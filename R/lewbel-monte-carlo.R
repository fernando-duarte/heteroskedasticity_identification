#' Run Complete Lewbel (2012) Monte Carlo Simulation
#'
#' Executes a comprehensive Monte Carlo simulation to evaluate the performance
#' of Lewbel's (2012) heteroscedasticity-based identification strategy. This
#' is the main function that orchestrates all simulation components.
#'
#' @param config List. Configuration object from create_default_config().
#'   If NULL, uses default configuration.
#' @param run_verification Logical. Whether to run assumption verification
#'   (default: TRUE).
#' @param run_bootstrap_demo Logical. Whether to run bootstrap demonstration
#'   (default: TRUE).
#' @param run_sample_analysis Logical. Whether to run sample size analysis
#'   (default: TRUE).
#' @param run_sensitivity Logical. Whether to run sensitivity analysis
#'   (default: TRUE).
#' @param generate_plots Logical. Whether to generate visualization plots
#'   (default: TRUE).
#' @param verbose Logical. Whether to print progress and results (default:
#'   TRUE).
#'
#' @details
#' This function runs a complete Monte Carlo evaluation including:
#' \itemize{
#'   \item Verification of Lewbel's identifying assumptions
#'   \item Main simulation comparing OLS vs 2SLS (Lewbel) estimators
#'   \item Bootstrap demonstration for set identification bounds
#'   \item Sample size consistency analysis
#'   \item Sensitivity analysis to heteroscedasticity strength
#'   \item Comprehensive result analysis and visualization
#' }
#'
#' The simulation uses parallel processing for efficiency and includes proper
#' seed management for reproducibility.
#'
#' @return A list containing:
#'   \itemize{
#'     \item config: Configuration used
#'     \item results_main: Main simulation results
#'     \item results_by_n: Sample size analysis results (if run)
#'     \item results_by_delta: Sensitivity analysis results (if run)
#'     \item bootstrap_demo: Bootstrap demonstration results (if run)
#'     \item analysis: Summary analysis
#'     \item plots: Visualization plots (if generated)
#'   }
#'
#' @examples
#' \dontrun{
#' # Run with default settings
#' results <- run_lewbel_monte_carlo()
#'
#' # Run with custom configuration
#' custom_config <- create_default_config(
#'   num_simulations = 500,
#'   gamma1 = -0.5,
#'   delta_het = 1.5
#' )
#' results <- run_lewbel_monte_carlo(custom_config)
#'
#' # Run only main simulation without extras
#' results <- run_lewbel_monte_carlo(
#'   run_bootstrap_demo = FALSE,
#'   run_sample_analysis = FALSE,
#'   run_sensitivity = FALSE
#' )
#' }
#'
#' @export
run_lewbel_monte_carlo <- function(config = NULL,
                                   run_verification = TRUE,
                                   run_bootstrap_demo = TRUE,
                                   run_sample_analysis = TRUE,
                                   run_sensitivity = TRUE,
                                   generate_plots = TRUE,
                                   verbose = TRUE) {
  # Use default config if none provided
  if (is.null(config)) {
    config <- create_default_config()
  }

  # Generate all seeds for reproducibility
  seeds <- generate_all_seeds(config)

  # Verify assumptions if requested
  if (run_verification) {
    params_for_verification <- list(
      beta1_0 = config$beta1_0,
      beta1_1 = config$beta1_1,
      gamma1 = config$gamma1,
      beta2_0 = config$beta2_0,
      beta2_1 = config$beta2_1,
      alpha1 = config$alpha1,
      alpha2 = config$alpha2,
      delta_het = config$delta_het
    )
    verify_lewbel_assumptions(
      params = params_for_verification,
      verbose = verbose
    )
  }

  # Run main simulation
  results_main <- run_main_simulation(config, seeds, verbose = verbose)

  # Initialize optional results
  results_by_n <- NULL
  results_by_delta <- NULL
  bootstrap_demo <- NULL

  # Run bootstrap demonstration if requested
  if (run_bootstrap_demo) {
    bootstrap_demo <- run_bootstrap_demonstration(
      config,
      seeds,
      verbose = verbose
    )
  }

  # Run sample size analysis if requested
  if (run_sample_analysis) {
    results_by_n <- run_sample_size_analysis(
      config,
      seeds,
      verbose = verbose
    )
  }

  # Run sensitivity analysis if requested
  if (run_sensitivity) {
    results_by_delta <- run_sensitivity_analysis(
      config,
      seeds,
      verbose = verbose
    )
  }

  # Analyze results
  main_analysis <- analyze_main_results(results_main, config, verbose = verbose)

  bootstrap_analysis <- NULL
  if (!is.null(bootstrap_demo)) {
    bootstrap_analysis <- analyze_bootstrap_results(
      results_main,
      bootstrap_demo,
      config,
      verbose = verbose
    )
  }

  sample_analysis <- NULL
  if (!is.null(results_by_n)) {
    sample_analysis <- analyze_sample_size_results(
      results_by_n,
      config,
      verbose = verbose
    )
  }

  sensitivity_analysis <- NULL
  if (!is.null(results_by_delta)) {
    sensitivity_analysis <- analyze_sensitivity_results(
      results_by_delta,
      config,
      verbose = verbose
    )
  }

  # Generate plots if requested
  plots <- NULL
  if (generate_plots) {
    # Use empty data.frames for missing components
    if (is.null(results_by_n)) {
      results_by_n <- data.frame()
    }
    if (is.null(results_by_delta)) {
      results_by_delta <- data.frame()
    }
    if (is.null(bootstrap_analysis)) {
      bootstrap_analysis <- data.frame()
    }

    plots <- generate_all_plots(
      results_main,
      results_by_n,
      results_by_delta,
      bootstrap_analysis,
      config,
      verbose = verbose
    )
  }

  # Print summary
  if (verbose) {
    print_simulation_summary()
  }

  # Return comprehensive results
  list(
    config = config,
    results_main = results_main,
    results_by_n = results_by_n,
    results_by_delta = results_by_delta,
    bootstrap_demo = bootstrap_demo,
    analysis = list(
      main = main_analysis,
      bootstrap = bootstrap_analysis,
      sample_size = sample_analysis,
      sensitivity = sensitivity_analysis
    ),
    plots = plots
  )
}


#' Run Quick Lewbel Monte Carlo Demo
#'
#' Runs a quick demonstration of the Lewbel Monte Carlo simulation with
#' reduced parameters for faster execution.
#'
#' @param num_simulations Integer. Number of simulations to run (default: 100).
#' @param verbose Logical. Whether to print output (default: TRUE).
#'
#' @return Results from run_lewbel_monte_carlo() with reduced parameters.
#'
#' @examples
#' \dontrun{
#' # Quick demo with 50 simulations
#' demo_results <- run_lewbel_demo(50)
#'
#' # Silent demo
#' demo_results <- run_lewbel_demo(100, verbose = FALSE)
#' }
#'
#' @export
run_lewbel_demo <- function(num_simulations = 100, verbose = TRUE) {
  demo_config <- create_default_config(
    num_simulations = num_simulations,
    sample_sizes = c(500, 1000), # Reduced sample sizes
    bootstrap_reps = 50 # Reduced bootstrap reps
  )

  # Modify additional parameters that aren't in create_default_config
  demo_config$n_reps_by_n <- 50 # Reduced reps for sample analysis
  demo_config$n_reps_by_delta <- 50 # Reduced reps for sensitivity
  demo_config$bootstrap_demo_size <- 3 # Reduced bootstrap demo size

  if (verbose) {
    cat("Running Lewbel Monte Carlo Demo with reduced parameters...\n")
  }

  run_lewbel_monte_carlo(
    config = demo_config,
    verbose = verbose
  )
}

#' Run Rigobon (2003) Identification Demo
#'
#' Demonstrates Rigobon's regime-based heteroskedasticity identification
#' method with example data and analysis.
#'
#' @param n_obs Integer. Sample size (default: 1000).
#' @param n_regimes Integer. Number of regimes (default: 2).
#' @param verbose Logical. Whether to print detailed output (default: TRUE).
#'
#' @return A list containing:
#'   \itemize{
#'     \item data: Generated data
#'     \item params: True parameters used
#'     \item results: Estimation results
#'     \item comparison: Comparison of OLS vs Rigobon estimates
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic two-regime demo
#' demo <- run_rigobon_demo()
#'
#' # Three-regime example with larger sample
#' demo_3reg <- run_rigobon_demo(n_obs = 2000, n_regimes = 3)
#'
#' # Silent demo
#' demo_quiet <- run_rigobon_demo(verbose = FALSE)
#' }
#'
#' @export
run_rigobon_demo <- function(n_obs = 1000, n_regimes = 2, verbose = TRUE) {

  if (verbose) {
    cat("\n========================================\n")
    cat("RIGOBON (2003) IDENTIFICATION DEMO\n")
    cat("========================================\n\n")
    cat("Demonstrating identification through regime-based heteroskedasticity\n")
    cat(sprintf("Sample size: %d, Number of regimes: %d\n\n", n_obs, n_regimes))
  }

  # Set up parameters based on number of regimes
  if (n_regimes == 2) {
    regime_probs <- c(0.4, 0.6)
    sigma2_regimes <- c(1.0, 2.5)  # Variance 2.5x higher in regime 2
    regime_labels <- c("Low Volatility", "High Volatility")
  } else if (n_regimes == 3) {
    regime_probs <- c(0.3, 0.4, 0.3)
    sigma2_regimes <- c(0.5, 1.0, 2.0)  # Increasing variance across regimes
    regime_labels <- c("Low Vol", "Medium Vol", "High Vol")
  } else {
    # General case
    regime_probs <- rep(1 / n_regimes, n_regimes)
    sigma2_regimes <- seq(0.5, 2.5, length.out = n_regimes)
    regime_labels <- paste("Regime", seq_len(n_regimes))
  }

  # True parameters
  params <- list(
    beta1_0 = 0.5,
    beta1_1 = 1.5,
    gamma1 = -0.8,  # True endogenous parameter
    beta2_0 = 1.0,
    beta2_1 = -1.0,
    alpha1 = -0.5,
    alpha2 = 1.0,
    regime_probs = regime_probs,
    sigma2_regimes = sigma2_regimes
  )

  if (verbose) {
    cat("True Parameters:\n")
    cat(sprintf("  gamma1 (endogenous parameter): %.2f\n", params$gamma1))
    cat("\nRegime Structure:\n")
    for (i in seq_len(n_regimes)) {
      cat(sprintf("  %s: %.1f%% of obs, variance multiplier = %.2f\n",
                  regime_labels[i],
                  100 * regime_probs[i],
                  sigma2_regimes[i]))
    }
    cat("\n")
  }

  # Generate data
  set.seed(42)  # For reproducibility
  data <- generate_rigobon_data(n_obs, params)

  # Run estimation
  results <- run_rigobon_estimation(data, return_diagnostics = TRUE)

  if (verbose) {
    cat("Estimation Results:\n")
    cat("==================\n\n")

    # OLS results
    cat("OLS (biased):\n")
    cat(sprintf("  Estimate: %.4f (true: %.4f)\n",
                results$ols$estimates["gamma1"], params$gamma1))
    cat(sprintf("  Std Error: %.4f\n", results$ols$se["gamma1"]))
    cat(sprintf("  Bias: %.4f\n\n",
                results$ols$estimates["gamma1"] - params$gamma1))

    # Rigobon 2SLS results
    cat("Rigobon 2SLS:\n")
    cat(sprintf("  Estimate: %.4f (true: %.4f)\n",
                results$tsls$estimates["gamma1"], params$gamma1))
    cat(sprintf("  Std Error: %.4f\n", results$tsls$se["gamma1"]))
    cat(sprintf("  Bias: %.4f\n\n",
                results$tsls$estimates["gamma1"] - params$gamma1))

    # First-stage strength
    cat("First-Stage F-statistics:\n")
    for (i in seq_len(n_regimes)) {
      cat(sprintf("  %s: F = %.2f\n",
                  regime_labels[i],
                  results$first_stage_F[i]))
    }

    # Heteroskedasticity test
    cat(sprintf("\nHeteroskedasticity Test:\n"))
    cat(sprintf("  F-stat: %.2f, p-value: %.4f\n",
                results$heteroskedasticity_test$F_stat,
                results$heteroskedasticity_test$p_value))
    cat(sprintf("  %s\n", results$heteroskedasticity_test$interpretation))

        # Efficiency comparison
    if (!is.na(results$ols$se["gamma1"]) && !is.na(results$tsls$se["gamma1"])) {
      efficiency_gain <- (results$ols$se["gamma1"] / results$tsls$se["gamma1"])^2
      cat(sprintf("\nRelative efficiency (OLS/Rigobon variance): %.2f\n",
                  efficiency_gain))

      if (efficiency_gain < 1) {
        cat("  (Rigobon estimator is more efficient than OLS)\n")
      }
    } else {
      cat("\nRelative efficiency: Cannot compute (NA values)\n")
    }
  }

  # Create comparison data frame
  comparison <- data.frame(
    Method = c("OLS", "Rigobon 2SLS"),
    Estimate = c(results$ols$estimates["gamma1"],
                 results$tsls$estimates["gamma1"]),
    StdError = c(results$ols$se["gamma1"],
                 results$tsls$se["gamma1"]),
    Bias = c(results$ols$estimates["gamma1"] - params$gamma1,
             results$tsls$estimates["gamma1"] - params$gamma1),
    RMSE = c(sqrt((results$ols$estimates["gamma1"] - params$gamma1)^2 +
                   results$ols$se["gamma1"]^2),
             sqrt((results$tsls$estimates["gamma1"] - params$gamma1)^2 +
                   results$tsls$se["gamma1"]^2))
  )

  # Return results
  invisible(list(
    data = data,
    params = params,
    results = results,
    comparison = comparison,
    regime_labels = regime_labels
  ))
}
