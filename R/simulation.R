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
    cat(sprintf(
      "\nStarting main simulation with %d runs...\n",
      config$num_simulations
    ))
  }

  # Set the base seed for reproducibility
  set.seed(seeds$main[1])

  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)

  # Run main simulation with bootstrap for first few runs
  results <- furrr::future_map_dfr(
    1:config$num_simulations,
    function(i) {
      params_list <- list(
        sample_size = config$main_sample_size,
        beta1_0 = config$beta1_0,
        beta1_1 = config$beta1_1,
        gamma1 = config$gamma1,
        beta2_0 = config$beta2_0,
        beta2_1 = config$beta2_1,
        alpha1 = config$alpha1,
        alpha2 = config$alpha2,
        delta_het = config$delta_het,
        tau_set_id = config$tau_set_id,
        bootstrap_reps = config$bootstrap_reps
      )
      run_single_lewbel_simulation(
        sim_id = i,
        params = params_list,
        endog_var = config$endog_var_name,
        exog_vars = config$exog_var_names,
        compute_bounds_se = (i <= config$bootstrap_subset_size),
        df_adjust = if (is.null(config$df_adjust)) {
          "asymptotic"
        } else {
          config$df_adjust
        }
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      chunk_size = NULL,
      globals = list(
        run_single_lewbel_simulation = run_single_lewbel_simulation,
        generate_lewbel_data = generate_lewbel_data,
        calculate_lewbel_bounds = calculate_lewbel_bounds,
        extract_se_lm = extract_se_lm,
        extract_se_ivreg = extract_se_ivreg,
        get_critical_value = get_critical_value,
        adjust_se_for_df = adjust_se_for_df
      )
    ),
    .progress = verbose
  )

  # Clean up parallel workers
  future::plan(future::sequential)

  results
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

  # Set the base seed for reproducibility
  set.seed(seeds$bootstrap_demo[1])

  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)

  bootstrap_demo <- furrr::future_map_dfr(
    1:config$bootstrap_demo_size,
    function(i) {
      params_list <- list(
        sample_size = config$main_sample_size,
        beta1_0 = config$beta1_0,
        beta1_1 = config$beta1_1,
        gamma1 = config$gamma1,
        beta2_0 = config$beta2_0,
        beta2_1 = config$beta2_1,
        alpha1 = config$alpha1,
        alpha2 = config$alpha2,
        delta_het = config$delta_het,
        tau_set_id = config$tau_set_id,
        bootstrap_reps = config$bootstrap_reps
      )
      run_single_lewbel_simulation(
        sim_id = i,
        params = params_list,
        endog_var = config$endog_var_name,
        exog_vars = config$exog_var_names,
        compute_bounds_se = TRUE,
        df_adjust = if (is.null(config$df_adjust)) {
          "asymptotic"
        } else {
          config$df_adjust
        }
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      globals = list(
        run_single_lewbel_simulation = run_single_lewbel_simulation,
        generate_lewbel_data = generate_lewbel_data,
        calculate_lewbel_bounds = calculate_lewbel_bounds,
        extract_se_lm = extract_se_lm,
        extract_se_ivreg = extract_se_ivreg,
        get_critical_value = get_critical_value,
        adjust_se_for_df = adjust_se_for_df
      )
    )
  )

  # Clean up parallel workers
  future::plan(future::sequential)

  bootstrap_demo
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

  results_by_n <- purrr::map2_dfr(
    config$sample_sizes, seq_along(config$sample_sizes),
    function(n_val, idx) {
      if (verbose) {
        cat(sprintf("  Sample size %d...\n", n_val))
      }

      # Set seed based on the first seed in the matrix row
      set.seed(seeds$by_n[idx, 1])

      furrr::future_map_dfr(
        1:config$n_reps_by_n,
        function(j) {
          params_list <- list(
            sample_size = n_val,
            beta1_0 = config$beta1_0,
            beta1_1 = config$beta1_1,
            gamma1 = config$gamma1,
            beta2_0 = config$beta2_0,
            beta2_1 = config$beta2_1,
            alpha1 = config$alpha1,
            alpha2 = config$alpha2,
            delta_het = config$delta_het,
            tau_set_id = config$tau_set_id,
            bootstrap_reps = config$bootstrap_reps
          )
          run_single_lewbel_simulation(
            sim_id = j,
            params = params_list,
            endog_var = config$endog_var_name,
            exog_vars = config$exog_var_names,
            compute_bounds_se = FALSE,
            df_adjust = if (is.null(config$df_adjust)) {
              "asymptotic"
            } else {
              config$df_adjust
            }
          )
        },
        .options = furrr::furrr_options(
          seed = TRUE,
          globals = list(
            run_single_lewbel_simulation = run_single_lewbel_simulation,
            generate_lewbel_data = generate_lewbel_data,
            calculate_lewbel_bounds = calculate_lewbel_bounds,
            extract_se_lm = extract_se_lm,
            extract_se_ivreg = extract_se_ivreg,
            get_critical_value = get_critical_value,
            adjust_se_for_df = adjust_se_for_df
          )
        )
      )
    }
  )

  # Clean up parallel workers
  future::plan(future::sequential)

  results_by_n
}


#' Run Sensitivity Analysis
#'
#' Analyzes sensitivity of results to heteroscedasticity strength.
#'
#' @param config List. Configuration object from create_default_config().
#' @param seeds List. Seed object from generate_all_seeds().
#' @param verbose Logical. Whether to print progress messages (default: TRUE).
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
  if (verbose) {
    cat("\nRunning heteroscedasticity sensitivity analysis...\n")
  }

  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)

  results_by_delta <- purrr::map2_dfr(
    config$delta_het_values, seq_along(config$delta_het_values),
    function(d_val, idx) {
      if (verbose) {
        cat(sprintf("  Delta = %.1f...\n", d_val))
      }

      # Set seed based on the first seed in the matrix row
      set.seed(seeds$by_delta[idx, 1])

      furrr::future_map_dfr(
        1:config$n_reps_by_delta,
        function(j) {
          params_list <- list(
            sample_size = config$main_sample_size,
            beta1_0 = config$beta1_0,
            beta1_1 = config$beta1_1,
            gamma1 = config$gamma1,
            beta2_0 = config$beta2_0,
            beta2_1 = config$beta2_1,
            alpha1 = config$alpha1,
            alpha2 = config$alpha2,
            delta_het = d_val,
            tau_set_id = config$tau_set_id,
            bootstrap_reps = config$bootstrap_reps
          )
          run_single_lewbel_simulation(
            sim_id = j,
            params = params_list,
            endog_var = config$endog_var_name,
            exog_vars = config$exog_var_names,
            compute_bounds_se = FALSE,
            df_adjust = if (is.null(config$df_adjust)) {
              "asymptotic"
            } else {
              config$df_adjust
            }
          )
        },
        .options = furrr::furrr_options(
          seed = TRUE,
          globals = list(
            run_single_lewbel_simulation = run_single_lewbel_simulation,
            generate_lewbel_data = generate_lewbel_data,
            calculate_lewbel_bounds = calculate_lewbel_bounds,
            extract_se_lm = extract_se_lm,
            extract_se_ivreg = extract_se_ivreg,
            get_critical_value = get_critical_value,
            adjust_se_for_df = adjust_se_for_df
          )
        )
      )
    }
  )

  # Clean up parallel workers
  future::plan(future::sequential)

  results_by_delta
}
