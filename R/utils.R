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
#' dim(seeds) # 3 x 100
#' }
#'
#' @export
generate_seed_matrix <- function(base_seed, n_experiments, n_reps_each) {
  set.seed(base_seed)
  seeds <- matrix(
    sample.int(
      .Machine$integer.max / 2, n_experiments * n_reps_each
    ),
    nrow = n_experiments,
    ncol = n_reps_each
  )
  seeds
}


#' Create Default Configuration for Lewbel Monte Carlo Simulations
#'
#' Creates a comprehensive configuration list with all parameters needed
#' for running Lewbel (2012) Monte Carlo simulations.
#'
#' @param num_simulations Integer. Number of main simulation runs
#'   (default: 1000).
#' @param main_sample_size Integer. Primary sample size for main results
#'   (default: 1000).
#' @param sample_sizes Integer vector. Sample sizes for consistency analysis
#'   (default: c(500, 1000, 2000)).
#' @param delta_het Numeric. Heteroscedasticity strength parameter
#'   (default: 1.2).
#' @param delta_het_values Numeric vector. Delta values for sensitivity
#'   analysis (default: c(0.4, 0.8, 1.2)).
#' @param n_reps_by_n Integer. Replications per sample size
#'   (default: 100).
#' @param n_reps_by_delta Integer. Replications per delta value
#'   (default: 100).
#' @param bootstrap_reps Integer. Number of bootstrap replications
#'   (default: 100).
#' @param bootstrap_subset_size Integer. Size of bootstrap subset
#'   (default: 10).
#' @param bootstrap_demo_size Integer. Size of bootstrap demo
#'   (default: 5).
#' @param beta1_0 Numeric. Intercept for first equation (default: 0.5).
#' @param beta1_1 Numeric. Slope for first equation (default: 1.5).
#' @param gamma1 Numeric. True value of the endogenous parameter
#'   (default: -0.8).
#' @param beta2_0 Numeric. Intercept for second equation (default: 1.0).
#' @param beta2_1 Numeric. Slope for second equation (default: -1.0).
#' @param alpha1 Numeric. Factor loading for first error (default: -0.5).
#' @param alpha2 Numeric. Factor loading for second error (default: 1.0).
#' @param tau_set_id Numeric. Tau parameter for set identification
#'   (default: 0.2).
#' @param endog_var_name Character. Name of endogenous variable
#'   (default: "Y2").
#' @param exog_var_names Character. Name of exogenous variable
#'   (default: "Xk").
#' @param df_adjust Character. Degrees of freedom adjustment method
#'   (default: "asymptotic"). Options: "asymptotic", "finite".
#'
#' @return A list containing all configuration parameters.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' config$gamma1 # -0.8
#'
#' # Custom configuration
#' custom_config <- create_default_config(
#'   num_simulations = 500,
#'   gamma1 = -0.5
#' )
#' }
#'
#' @export
create_default_config <- function(num_simulations = 200,
                                  main_sample_size = 500,
                                  sample_sizes = c(250, 500, 1000, 2000),
                                  delta_het = 0.8,
                                  delta_het_values = c(0.4, 0.8, 1.2),
                                  n_reps_by_n = 100,
                                  n_reps_by_delta = 100,
                                  bootstrap_reps = 100,
                                  bootstrap_subset_size = 10,
                                  bootstrap_demo_size = 5,
                                  beta1_0 = 0.5,
                                  beta1_1 = 1.5,
                                  gamma1 = -0.8,
                                  beta2_0 = 1.0,
                                  beta2_1 = -1.0,
                                  alpha1 = -0.5,
                                  alpha2 = 1.0,
                                  tau_set_id = 0.2,
                                  endog_var_name = "Y2",
                                  exog_var_names = "Xk",
                                  df_adjust = "asymptotic") {
  list(
    # Simulation Controls
    num_simulations = num_simulations,
    sample_sizes = sample_sizes,
    main_sample_size = main_sample_size,
    set_seed = 123, # Base seed, not configurable through function arg

    # True Model Parameters (Triangular System)
    beta1_0 = beta1_0,
    beta1_1 = beta1_1,
    gamma1 = gamma1,
    beta2_0 = beta2_0,
    beta2_1 = beta2_1,

    # Error Structure Parameters (Single-Factor Model)
    alpha1 = alpha1,
    alpha2 = alpha2,
    delta_het = delta_het,
    delta_het_values = delta_het_values,

    # Set Identification Parameters
    tau_set_id = tau_set_id,

    # Bootstrap parameters
    bootstrap_reps = bootstrap_reps,
    bootstrap_subset_size = bootstrap_subset_size,
    bootstrap_demo_size = bootstrap_demo_size,

    # Variable names (for flexibility)
    endog_var_name = endog_var_name,
    exog_var_names = exog_var_names,

    # Auxiliary simulation controls
    n_reps_by_n = n_reps_by_n,
    n_reps_by_delta = n_reps_by_delta,

    # Statistical inference controls
    df_adjust = df_adjust
  )
}


#' Generate All Seeds for Lewbel Simulation
#'
#' Pre-generates all seeds needed for different parts of the Lewbel simulation
#' to ensure reproducibility across parallel execution.
#'
#' @param config List. Configuration object from create_default_config().
#'
#' @return A list containing seed vectors/matrices for different simulation
#'   parts.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' names(seeds) # "main", "by_n", "by_delta", "bootstrap_demo"
#' }
#'
#' @export
generate_all_seeds <- function(config) {
  list(
    main = 1:config$num_simulations + config$set_seed * 1000,
    by_n = generate_seed_matrix(
      base_seed = config$set_seed + 1,
      n_experiments = length(config$sample_sizes),
      n_reps_each = config$n_reps_by_n
    ),
    by_delta = generate_seed_matrix(
      base_seed = config$set_seed + 2,
      n_experiments = length(config$delta_het_values),
      n_reps_each = config$n_reps_by_delta
    ),
    bootstrap_demo = 1:config$bootstrap_demo_size + config$set_seed * 3000
  )
}
