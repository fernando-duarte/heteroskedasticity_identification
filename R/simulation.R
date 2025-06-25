#' Run Main Lewbel Monte Carlo Simulation
#'
#' Executes the main Monte Carlo simulation to evaluate the performance of
#' Lewbel's (2012) heteroscedasticity-based identification strategy.
#'
#' @template param-config
#' @template param-seeds
#' @template param-verbose-progress
#'
#' @return A data.frame containing results from all simulation runs.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config(num_simulations = 100)
#' seeds <- generate_all_seeds(config)
#' results <- run_main_simulation(config, seeds)
#' }
#'
#' @export
run_main_simulation <- function(config, seeds, verbose = TRUE) {
  # Set the base seed for reproducibility
  set.seed(seeds$main[1])

  # Use helper function to run parallel simulation
  .run_parallel_simulation(
    n_simulations = config$num_simulations,
    config = config,
    sim_function = run_single_lewbel_simulation,
    compute_bounds_se_rule = function(i) i <= config$bootstrap_subset_size,
    verbose = verbose,
    progress_message = sprintf(
      "Starting main simulation with %d runs...",
      config$num_simulations
    )
  )
}


#' Run Bootstrap Demonstration
#'
#' Runs a separate demonstration of bootstrap standard errors for
#' set identification bounds.
#'
#' @template param-config
#' @template param-seeds
#' @template param-verbose-progress
#'
#' @return A data.frame containing bootstrap demonstration results.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' bootstrap_results <- run_bootstrap_demonstration(config, seeds)
#' }
#'
#' @export
run_bootstrap_demonstration <- function(config, seeds, verbose = TRUE) {
  # Set the base seed for reproducibility
  set.seed(seeds$bootstrap_demo[1])

  # Use helper function to run parallel simulation
  .run_parallel_simulation(
    n_simulations = config$bootstrap_demo_size,
    config = config,
    sim_function = run_single_lewbel_simulation,
    compute_bounds_se_rule = TRUE, # Always compute bounds SE for demo
    verbose = verbose,
    progress_message = "Running separate bootstrap SE demonstration..."
  )
}


#' Run Sample Size Analysis
#'
#' Analyzes the consistency of estimators across different sample sizes.
#'
#' @template param-config
#' @template param-seeds
#' @template param-verbose-progress
#'
#' @return A data.frame containing results for different sample sizes.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' size_results <- run_sample_size_analysis(config, seeds)
#' }
#'
#' @export
run_sample_size_analysis <- function(config, seeds, verbose = TRUE) {
  # Use helper function to run parameter sweep
  .run_parameter_sweep(
    param_values = config$sample_sizes,
    param_name = "sample_size",
    n_reps = config$n_reps_by_n,
    config = config,
    seeds = seeds$by_n,
    sim_function = run_single_lewbel_simulation,
    verbose = verbose,
    progress_prefix = "sample size consistency"
  )
}


#' Run Sensitivity Analysis
#'
#' Analyzes sensitivity of results to heteroscedasticity strength.
#'
#' @template param-config
#' @template param-seeds
#' @template param-verbose-progress
#'
#' @return A data.frame containing results for different heteroscedasticity
#'   parameters.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' sensitivity_results <- run_sensitivity_analysis(config, seeds)
#' }
#'
#' @export
run_sensitivity_analysis <- function(config, seeds, verbose = TRUE) {
  # Use helper function to run parameter sweep
  .run_parameter_sweep(
    param_values = config$delta_het_values,
    param_name = "delta_het",
    n_reps = config$n_reps_by_delta,
    config = config,
    seeds = seeds$by_delta,
    sim_function = run_single_lewbel_simulation,
    verbose = verbose,
    progress_prefix = "heteroscedasticity sensitivity"
  )
}
