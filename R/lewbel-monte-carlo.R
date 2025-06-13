#' Run Complete Lewbel (2012) Monte Carlo Simulation
#'
#' Executes a comprehensive Monte Carlo simulation to evaluate the performance of
#' Lewbel's (2012) heteroscedasticity-based identification strategy. This is the
#' main function that orchestrates all simulation components.
#'
#' @param config List. Configuration object from create_default_config(). If NULL,
#'   uses default configuration.
#' @param run_verification Logical. Whether to run assumption verification (default: TRUE).
#' @param run_bootstrap_demo Logical. Whether to run bootstrap demonstration (default: TRUE).
#' @param run_sample_analysis Logical. Whether to run sample size analysis (default: TRUE).
#' @param run_sensitivity Logical. Whether to run sensitivity analysis (default: TRUE).
#' @param generate_plots Logical. Whether to generate visualization plots (default: TRUE).
#' @param verbose Logical. Whether to print progress and results (default: TRUE).
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
      beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
      beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
      alpha1 = config$alpha1, alpha2 = config$alpha2, delta_het = config$delta_het
    )
    verify_lewbel_assumptions(params = params_for_verification, verbose = verbose)
  }

  # Run main simulation
  results_main <- run_main_simulation(config, seeds, verbose = verbose)

  # Initialize optional results
  results_by_n <- NULL
  results_by_delta <- NULL
  bootstrap_demo <- NULL

  # Run bootstrap demonstration if requested
  if (run_bootstrap_demo) {
    bootstrap_demo <- run_bootstrap_demonstration(config, seeds, verbose = verbose)
  }

  # Run sample size analysis if requested
  if (run_sample_analysis) {
    results_by_n <- run_sample_size_analysis(config, seeds, verbose = verbose)
  }

  # Run sensitivity analysis if requested
  if (run_sensitivity) {
    results_by_delta <- run_sensitivity_analysis(config, seeds, verbose = verbose)
  }

  # Analyze results
  main_analysis <- analyze_main_results(results_main, config, verbose = verbose)

  bootstrap_analysis <- NULL
  if (!is.null(bootstrap_demo)) {
    bootstrap_analysis <- analyze_bootstrap_results(results_main, bootstrap_demo, config, verbose = verbose)
  }

  sample_analysis <- NULL
  if (!is.null(results_by_n)) {
    sample_analysis <- analyze_sample_size_results(results_by_n, config, verbose = verbose)
  }

  sensitivity_analysis <- NULL
  if (!is.null(results_by_delta)) {
    sensitivity_analysis <- analyze_sensitivity_results(results_by_delta, config, verbose = verbose)
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
      results_main, results_by_n, results_by_delta,
      bootstrap_analysis, config,
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
