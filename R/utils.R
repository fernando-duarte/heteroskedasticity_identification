#' Generate Seed Matrix for Reproducible Parallel Simulations
#'
#' Pre-generates seeds for reproducible parallel simulations to ensure
#' consistent results across different computing environments.
#'
#' @param base_seed Integer. Base seed for random number generation.
#' @param n_experiments Integer. Number of experiments (rows in matrix).
#' @param n_reps_each Integer. Number of replications per experiment (columns).
#'
#' @return A matrix of seeds with dimensions n_experiments x n_reps_each.
#'
#' @examples
#' \dontrun{
#' seeds <- generate_seed_matrix(123, 3, 100)
#' dim(seeds)  # 3 x 100
#' }
#'
#' @export
generate_seed_matrix <- function(base_seed, n_experiments, n_reps_each) {
  set.seed(base_seed)
  seeds <- matrix(
    sample.int(.Machine$integer.max / 2, n_experiments * n_reps_each),
    nrow = n_experiments,
    ncol = n_reps_each
  )
  return(seeds)
}


#' Create Default Configuration for Lewbel Monte Carlo Simulations
#'
#' Creates a comprehensive configuration list with all parameters needed
#' for running Lewbel (2012) Monte Carlo simulations.
#'
#' @param num_simulations Integer. Number of main simulation runs (default: 1000).
#' @param sample_sizes Integer vector. Sample sizes for consistency analysis (default: c(500, 1000, 2000)).
#' @param main_sample_size Integer. Primary sample size for main results (default: 1000).
#' @param set_seed Integer. Base seed for reproducibility (default: 123).
#' @param gamma1 Numeric. True value of the endogenous parameter (default: -0.8).
#' @param delta_het Numeric. Heteroscedasticity strength parameter (default: 1.2).
#' @param tau_set_id Numeric. Tau parameter for set identification (default: 0.2).
#' @param bootstrap_reps Integer. Number of bootstrap replications (default: 100).
#'
#' @return A list containing all configuration parameters.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' config$gamma1  # -0.8
#' 
#' # Custom configuration
#' custom_config <- create_default_config(
#'   num_simulations = 500,
#'   gamma1 = -0.5
#' )
#' }
#'
#' @export
create_default_config <- function(num_simulations = 1000,
                                  sample_sizes = c(500, 1000, 2000),
                                  main_sample_size = 1000,
                                  set_seed = 123,
                                  gamma1 = -0.8,
                                  delta_het = 1.2,
                                  tau_set_id = 0.2,
                                  bootstrap_reps = 100) {
  
  list(
    # Simulation Controls
    num_simulations = num_simulations,
    sample_sizes = sample_sizes,
    main_sample_size = main_sample_size,
    set_seed = set_seed,
    
    # True Model Parameters (Triangular System)
    beta1_0 = 0.5,
    beta1_1 = 1.5,
    gamma1 = gamma1,
    beta2_0 = 1.0,
    beta2_1 = -1.0,
    
    # Error Structure Parameters (Single-Factor Model)
    alpha1 = -0.5,
    alpha2 = 1.0,
    delta_het = delta_het,
    delta_het_values = c(0.6, 1.2, 1.8),
    
    # Set Identification Parameters
    tau_set_id = tau_set_id,
    
    # Bootstrap parameters
    bootstrap_reps = bootstrap_reps,
    bootstrap_subset_size = 10,
    bootstrap_demo_size = 5,
    
    # Variable names (for flexibility)
    endog_var_name = "Y2",
    exog_var_names = c("Xk"),
    
    # Auxiliary simulation controls
    n_reps_by_n = 200,
    n_reps_by_delta = 200
  )
}


#' Generate All Seeds for Lewbel Simulation
#'
#' Pre-generates all seeds needed for different parts of the Lewbel simulation
#' to ensure reproducibility across parallel execution.
#'
#' @param config List. Configuration object from create_default_config().
#'
#' @return A list containing seed vectors/matrices for different simulation parts.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' names(seeds)  # "main", "by_n", "by_delta", "bootstrap_demo"
#' }
#'
#' @export
generate_all_seeds <- function(config) {
  list(
    main = 1:config$num_simulations + config$set_seed * 1000,
    by_n = generate_seed_matrix(config$set_seed + 1, 
                                length(config$sample_sizes), 
                                config$n_reps_by_n),
    by_delta = generate_seed_matrix(config$set_seed + 2, 
                                    length(config$delta_het_values), 
                                    config$n_reps_by_delta),
    bootstrap_demo = 1:config$bootstrap_demo_size + config$set_seed * 3000
  )
}
