#' @title Internal Helper Functions for Simulations
#' @description Helper functions to reduce code duplication in simulation functions
#' @name simulation-helpers
#' @keywords internal
NULL

#' Run Parallel Simulation with Common Setup
#'
#' Internal helper that encapsulates the common pattern for running
#' parallel simulations with furrr. Used by run_main_simulation and
#' run_bootstrap_demonstration to avoid code duplication.
#'
#' @param n_simulations Integer. Number of simulations to run
#' @template param-config
#' @param sim_function Function. The simulation function to call (e.g., run_single_lewbel_simulation)
#' @param compute_bounds_se_rule Function or logical. Rule for determining compute_bounds_se per iteration
#' @template param-verbose
#' @param progress_message Character. Message to display when starting
#'
#' @return Data frame with simulation results
#' @keywords internal
#' @noRd
.run_parallel_simulation <- function(n_simulations,
                                     config,
                                     sim_function,
                                     compute_bounds_se_rule,
                                     verbose = TRUE,
                                     progress_message = "Starting simulation...") {
  if (verbose) {
    cat(sprintf("\n%s\n", progress_message))
  }

  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)

  # Build parameter list once (same for all iterations)
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

  # Run simulations in parallel
  results <- furrr::future_map_dfr(
    seq_len(n_simulations),
    function(i) {
      # Determine compute_bounds_se for this iteration
      compute_bounds_se <- if (is.function(compute_bounds_se_rule)) {
        compute_bounds_se_rule(i)
      } else {
        compute_bounds_se_rule
      }

      sim_function(
        sim_id = i,
        params = params_list,
        endog_var = config$endog_var_name,
        exog_vars = config$exog_var_names,
        compute_bounds_se = compute_bounds_se,
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

#' Run Parameter Sweep Analysis
#'
#' Internal helper that encapsulates the common pattern for running
#' parameter sweep analyses. Used by run_sample_size_analysis and
#' run_sensitivity_analysis to avoid code duplication.
#'
#' @param param_values Vector. Values of the parameter to sweep over
#' @param param_name Character. Name of the parameter being varied
#' @param n_reps Integer. Number of replications per parameter value
#' @template param-config
#' @template param-seeds
#' @param sim_function Function. The simulation function to call
#' @template param-verbose
#' @param progress_prefix Character. Prefix for progress messages
#'
#' @return Data frame with parameter sweep results
#' @keywords internal
#' @noRd
.run_parameter_sweep <- function(param_values,
                                 param_name,
                                 n_reps,
                                 config,
                                 seeds,
                                 sim_function,
                                 verbose = TRUE,
                                 progress_prefix = "Parameter") {
  if (verbose) {
    cat(sprintf("\nRunning %s analysis...\n", progress_prefix))
  }

  # Set up parallel processing
  future::plan(future::multisession, workers = future::availableCores() - 1)

  results <- purrr::map2_dfr(
    param_values, seq_along(param_values),
    function(param_val, idx) {
      if (verbose) {
        if (is.numeric(param_val)) {
          cat(sprintf("  %s = %.1f...\n", param_name, param_val))
        } else {
          cat(sprintf("  %s = %s...\n", param_name, param_val))
        }
      }

      # Set seed based on the first seed in the matrix row
      set.seed(seeds[idx, 1])

      furrr::future_map_dfr(
        seq_len(n_reps),
        function(j) {
          # Build parameter list with the varying parameter
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

          # Override the varying parameter
          params_list[[param_name]] <- param_val

          sim_function(
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

  results
}
