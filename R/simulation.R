#' Run Main Lewbel Monte Carlo Simulation
#'
#' Executes the main Monte Carlo simulation to evaluate the performance of
#' Lewbel's (2012) heteroscedasticity-based identification strategy.
#'
#' @param config List. Configuration object from create_default_config().
#' @param seeds List. Seed object from generate_all_seeds().
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
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
  if (verbose) {
    cat(sprintf("\nStarting main simulation with %d runs...\n", config$num_simulations))
  }
  
  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)
  
  # Run main simulation with bootstrap for first few runs
  results <- furrr::future_map_dfr(
    1:config$num_simulations,
    function(i) {
      run_single_lewbel_simulation(i, 
        list(
          sample_size = config$main_sample_size,
          beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
          beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
          alpha1 = config$alpha1, alpha2 = config$alpha2,
          delta_het = config$delta_het, tau_set_id = config$tau_set_id,
          bootstrap_reps = config$bootstrap_reps
        ),
        endog_var = config$endog_var_name,
        exog_vars = config$exog_var_names,
        compute_bounds_se = (i <= config$bootstrap_subset_size)
      )
    },
    .options = furrr::furrr_options(seed = seeds$main, chunk_size = NULL),
    .progress = verbose
  )
  
  # Clean up parallel workers
  future::plan(future::sequential)
  
  return(results)
}


#' Run Bootstrap Demonstration
#'
#' Runs a separate demonstration of bootstrap standard errors for
#' set identification bounds.
#'
#' @param config List. Configuration object from create_default_config().
#' @param seeds List. Seed object from generate_all_seeds().
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
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
  if (verbose) {
    cat("\nRunning separate bootstrap SE demonstration...\n")
  }
  
  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)
  
  bootstrap_demo <- furrr::future_map_dfr(
    1:config$bootstrap_demo_size,
    function(i) {
      run_single_lewbel_simulation(i, 
        list(
          sample_size = config$main_sample_size,
          beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
          beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
          alpha1 = config$alpha1, alpha2 = config$alpha2,
          delta_het = config$delta_het, tau_set_id = config$tau_set_id,
          bootstrap_reps = config$bootstrap_reps
        ),
        endog_var = config$endog_var_name,
        exog_vars = config$exog_var_names,
        compute_bounds_se = TRUE
      )
    },
    .options = furrr::furrr_options(seed = seeds$bootstrap_demo)
  )
  
  # Clean up parallel workers
  future::plan(future::sequential)
  
  return(bootstrap_demo)
}


#' Run Sample Size Analysis
#'
#' Analyzes the consistency of estimators across different sample sizes.
#'
#' @param config List. Configuration object from create_default_config().
#' @param seeds List. Seed object from generate_all_seeds().
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
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
  if (verbose) {
    cat("\nRunning sample size consistency analysis...\n")
  }
  
  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)
  
  results_by_n <- purrr::map2_dfr(config$sample_sizes, 1:length(config$sample_sizes), 
    function(n, idx) {
      if (verbose) {
        cat(sprintf("  Sample size %d...\n", n))
      }
      furrr::future_map_dfr(
        1:config$n_reps_by_n,
        function(j) {
          run_single_lewbel_simulation(j, 
            list(
              sample_size = n,
              beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
              beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
              alpha1 = config$alpha1, alpha2 = config$alpha2,
              delta_het = config$delta_het, tau_set_id = config$tau_set_id,
              bootstrap_reps = config$bootstrap_reps
            ),
            endog_var = config$endog_var_name,
            exog_vars = config$exog_var_names,
            compute_bounds_se = FALSE
          )
        },
        .options = furrr::furrr_options(seed = seeds$by_n[idx, ])
      )
    }
  )
  
  # Clean up parallel workers
  future::plan(future::sequential)
  
  return(results_by_n)
}


#' Run Sensitivity Analysis
#'
#' Analyzes sensitivity of results to heteroscedasticity strength.
#'
#' @param config List. Configuration object from create_default_config().
#' @param seeds List. Seed object from generate_all_seeds().
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
#'
#' @return A data.frame containing results for different heteroscedasticity parameters.
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
  if (verbose) {
    cat("\nRunning heteroscedasticity sensitivity analysis...\n")
  }
  
  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)
  
  results_by_delta <- purrr::map2_dfr(config$delta_het_values, 1:length(config$delta_het_values),
    function(d, idx) {
      if (verbose) {
        cat(sprintf("  Delta = %.1f...\n", d))
      }
      furrr::future_map_dfr(
        1:config$n_reps_by_delta,
        function(j) {
          run_single_lewbel_simulation(j, 
            list(
              sample_size = config$main_sample_size,
              beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
              beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
              alpha1 = config$alpha1, alpha2 = config$alpha2,
              delta_het = d, tau_set_id = config$tau_set_id,
              bootstrap_reps = config$bootstrap_reps
            ),
            endog_var = config$endog_var_name,
            exog_vars = config$exog_var_names,
            compute_bounds_se = FALSE
          )
        },
        .options = furrr::furrr_options(seed = seeds$by_delta[idx, ])
      )
    }
  )
  
  # Clean up parallel workers
  future::plan(future::sequential)
  
  return(results_by_delta)
}
