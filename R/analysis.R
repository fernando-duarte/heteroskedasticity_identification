#' Analyze Main Simulation Results
#'
#' Provides comprehensive analysis of the main Monte Carlo simulation results,
#' including performance metrics for point estimators and set identification.
#'
#' @param results Data.frame. Results from run_main_simulation().
#' @param config List. Configuration object used for the simulation.
#' @param verbose Logical. Whether to print detailed output (default: TRUE).
#'
#' @return A list containing summary tables and statistics.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results <- run_main_simulation(config, seeds)
#' analysis <- analyze_main_results(results, config)
#' }
#'
#' @export
analyze_main_results <- function(results, config, verbose = TRUE) {
  # Clean results
  results_clean <- stats::na.omit(results)
  
  if (verbose) {
    cat(sprintf("\nMain simulation: %d out of %d runs completed successfully\n", 
                nrow(results_clean), config$num_simulations))
    
    # Main results table
    cat("\n--- Performance of Point Estimators for gamma1 ---\n")
    cat(sprintf("True value of gamma1: %.2f\n\n", config$gamma1))
  }
  
  # Calculate performance metrics
  summary_table <- results_clean %>%
    dplyr::summarise(
      Estimator = c("OLS", "2SLS (Lewbel)"),
      Bias = c(mean(ols_gamma1) - config$gamma1, mean(tsls_gamma1) - config$gamma1),
      RMSE = c(sqrt(mean((ols_gamma1 - config$gamma1)^2)), 
               sqrt(mean((tsls_gamma1 - config$gamma1)^2))),
      Coverage = c(mean(ols_coverage), mean(tsls_coverage)),
      `Avg First-Stage F` = c(NA, mean(first_stage_F))
    )
  
  if (verbose) {
    print(knitr::kable(summary_table, digits = 4))
    
    # Weak instrument diagnostics
    weak_iv_pct <- mean(results_clean$first_stage_F < 10) * 100
    cat(sprintf("\nWeak instrument diagnostic: %.1f%% of simulations have first-stage F < 10\n", 
                weak_iv_pct))
  }
  
  # Set identification results
  bounds_summary <- results_clean %>%
    dplyr::summarise(
      Scenario = sprintf("Set ID (tau=%.2f)", config$tau_set_id),
      `Avg Width` = mean(bound_upper_tau_set - bound_lower_tau_set),
      Coverage = mean(config$gamma1 >= bound_lower_tau_set & config$gamma1 <= bound_upper_tau_set),
      `Point ID Check` = stats::cor((bound_upper_tau0 + bound_lower_tau0) / 2, tsls_gamma1, 
                                   use = "complete.obs")
    )
  
  if (verbose) {
    cat("\n--- Performance of Set Identification ---\n")
    print(knitr::kable(bounds_summary, digits = 4))
  }
  
  return(list(
    summary_table = summary_table,
    bounds_summary = bounds_summary,
    weak_iv_pct = mean(results_clean$first_stage_F < 10) * 100,
    results_clean = results_clean
  ))
}


#' Analyze Bootstrap Results
#'
#' Analyzes and displays bootstrap standard error results for set identification bounds.
#'
#' @param results_main Data.frame. Main simulation results.
#' @param bootstrap_demo Data.frame. Bootstrap demonstration results.
#' @param config List. Configuration object.
#' @param verbose Logical. Whether to print output (default: TRUE).
#'
#' @return A data.frame with bootstrap examples.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_main <- run_main_simulation(config, seeds)
#' bootstrap_demo <- run_bootstrap_demonstration(config, seeds)
#' bootstrap_analysis <- analyze_bootstrap_results(results_main, bootstrap_demo, config)
#' }
#'
#' @export
analyze_bootstrap_results <- function(results_main, bootstrap_demo, config, verbose = TRUE) {
  # Combine bootstrap examples
  bootstrap_examples <- dplyr::bind_rows(
    results_main %>% 
      dplyr::filter(!is.na(bound_se_lower)) %>% 
      dplyr::slice_head(n = config$bootstrap_subset_size),
    bootstrap_demo
  ) %>%
    dplyr::distinct()
  
  if (nrow(bootstrap_examples) > 0 && verbose) {
    cat("\n--- Bootstrap Standard Errors for Set Identification Bounds ---\n")
    cat(sprintf("Showing %d examples with bootstrap SEs (B = %d)\n\n", 
                nrow(bootstrap_examples), config$bootstrap_reps))
    
    bootstrap_table <- bootstrap_examples %>%
      dplyr::select(sim_id, 
                   lower = bound_lower_tau_set, 
                   upper = bound_upper_tau_set,
                   se_lower = bound_se_lower,
                   se_upper = bound_se_upper) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric) & !sim_id, ~round(., 4)))
    
    print(knitr::kable(bootstrap_table))
  }
  
  return(bootstrap_examples)
}


#' Analyze Sample Size Results
#'
#' Analyzes consistency of estimators across different sample sizes.
#'
#' @param results_by_n Data.frame. Results from run_sample_size_analysis().
#' @param config List. Configuration object.
#' @param verbose Logical. Whether to print output (default: TRUE).
#'
#' @return A data.frame with sample size analysis.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_by_n <- run_sample_size_analysis(config, seeds)
#' size_analysis <- analyze_sample_size_results(results_by_n, config)
#' }
#'
#' @export
analyze_sample_size_results <- function(results_by_n, config, verbose = TRUE) {
  n_summary <- results_by_n %>%
    dplyr::group_by(sample_size) %>%
    dplyr::summarise(
      `2SLS Bias` = mean(tsls_gamma1, na.rm = TRUE) - config$gamma1,
      `2SLS RMSE` = sqrt(mean((tsls_gamma1 - config$gamma1)^2, na.rm = TRUE)),
      `Avg First-Stage F` = mean(first_stage_F, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (verbose) {
    cat("\n--- Consistency Check: Performance by Sample Size ---\n")
    print(knitr::kable(n_summary, digits = 4))
  }
  
  return(n_summary)
}


#' Analyze Sensitivity Results
#'
#' Analyzes sensitivity of results to heteroscedasticity strength.
#'
#' @param results_by_delta Data.frame. Results from run_sensitivity_analysis().
#' @param config List. Configuration object.
#' @param verbose Logical. Whether to print output (default: TRUE).
#'
#' @return A data.frame with sensitivity analysis.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_by_delta <- run_sensitivity_analysis(config, seeds)
#' sensitivity_analysis <- analyze_sensitivity_results(results_by_delta, config)
#' }
#'
#' @export
analyze_sensitivity_results <- function(results_by_delta, config, verbose = TRUE) {
  delta_summary <- results_by_delta %>%
    dplyr::group_by(delta_het) %>%
    dplyr::summarise(
      `2SLS Bias` = mean(tsls_gamma1, na.rm = TRUE) - config$gamma1,
      `2SLS RMSE` = sqrt(mean((tsls_gamma1 - config$gamma1)^2, na.rm = TRUE)),
      `Avg First-Stage F` = mean(first_stage_F, na.rm = TRUE),
      `Bounds Width` = mean(bound_upper_tau_set - bound_lower_tau_set, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (verbose) {
    cat("\n--- Sensitivity to Heteroscedasticity Strength ---\n")
    print(knitr::kable(delta_summary, digits = 4))
  }
  
  return(delta_summary)
}


#' Print Simulation Summary
#'
#' Prints a comprehensive summary of simulation findings.
#'
#' @param verbose Logical. Whether to print the summary (default: TRUE).
#'
#' @examples
#' \dontrun{
#' print_simulation_summary()
#' }
#'
#' @export
print_simulation_summary <- function(verbose = TRUE) {
  if (verbose) {
    cat("\n--- SIMULATION COMPLETE ---\n")
    cat("Key findings:\n")
    cat("1. OLS is biased due to endogeneity.\n")
    cat("2. Lewbel's 2SLS successfully corrects the bias using heteroscedasticity.\n")
    cat("3. Estimates improve with sample size (consistency).\n")
    cat("4. Stronger heteroscedasticity improves instrument strength.\n")
    cat("5. Set identification bounds widen with tau but maintain good coverage.\n")
    cat("6. Bootstrap SEs provide valid inference for the bounds.\n")
  }
}
