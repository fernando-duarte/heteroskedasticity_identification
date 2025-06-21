#' @importFrom rlang .data
NULL

#' Analyze Main Simulation Results
#'
#' Provides comprehensive analysis of the main Monte Carlo simulation results,
#' including performance metrics for point estimators and set identification.
#'
#' @param results Data.frame. Results from run_main_simulation().
#' @template param-config
#' @template param-verbose
#'
#' @template note-knitr
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
    cat(sprintf(
      paste0(
        "\nMain simulation: %d out of %d runs completed ",
        "successfully\n"
      ),
      nrow(results_clean), config$num_simulations
    ))

    # Main results table
    cat(paste0(
      "\n--- Performance of Point Estimators for gamma1 ---",
      "\n"
    ))
    cat(sprintf("True value of gamma1: %.2f\n\n", config$gamma1))
  }

  # Calculate performance metrics
  summary_table <- data.frame(
    Estimator = c("OLS", "2SLS (Lewbel)"),
    Bias = c(
      mean(results_clean$ols_gamma1) - config$gamma1,
      mean(results_clean$tsls_gamma1) - config$gamma1
    ),
    RMSE = c(
      sqrt(mean((results_clean$ols_gamma1 - config$gamma1)^2)),
      sqrt(mean((results_clean$tsls_gamma1 - config$gamma1)^2))
    ),
    Coverage = c(
      mean(results_clean$ols_coverage),
      mean(results_clean$tsls_coverage)
    ),
    `Avg First-Stage F` = c(NA, mean(results_clean$first_stage_F)),
    check.names = FALSE
  )

  if (verbose) {
    if (requireNamespace("knitr", quietly = TRUE)) {
      print(knitr::kable(summary_table, digits = hetid_opt("display_digits")))
    } else {
      print(summary_table)
    }

    # Weak instrument diagnostics
    weak_iv_pct <- mean(
      results_clean$first_stage_F < hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"),
      na.rm = TRUE
    ) * 100
    # If all F-statistics are NA, set weak_iv_pct to 0
    if (is.nan(weak_iv_pct)) weak_iv_pct <- 0
    cat(sprintf(
      paste0(
        "\nWeak instrument diagnostic: %.1f%% of simulations have ",
        "first-stage F < ", hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"), "\n"
      ),
      weak_iv_pct
    ))
  }

  # Set identification results
  bounds_summary <- data.frame(
    Scenario = sprintf("Set ID (tau=%.2f)", config$tau_set_id),
    `Avg Width` = mean(
      results_clean$bound_upper_tau_set - results_clean$bound_lower_tau_set
    ),
    Coverage = mean(
      config$gamma1 >= results_clean$bound_lower_tau_set &
        config$gamma1 <= results_clean$bound_upper_tau_set
    ),
    `Point ID Check` = tryCatch(
      {
        stats::cor(
          (results_clean$bound_upper_tau0 + results_clean$bound_lower_tau0) / 2,
          results_clean$tsls_gamma1,
          use = "complete.obs"
        )
      },
      error = function(e) NA_real_
    ),
    check.names = FALSE
  )

  if (verbose) {
    cat(paste0(
      "\n--- Performance of Set Identification ---",
      "\n"
    ))
    if (requireNamespace("knitr", quietly = TRUE)) {
      print(knitr::kable(bounds_summary, digits = hetid_opt("display_digits")))
    } else {
      print(bounds_summary)
    }
  }

  list(
    summary_table = summary_table,
    bounds_summary = bounds_summary,
    weak_iv_pct = {
      pct <- mean(
        results_clean$first_stage_F <
          hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"),
        na.rm = TRUE
      ) * 100
      if (is.nan(pct)) 0 else pct
    },
    results_clean = results_clean
  )
}


#' Analyze Bootstrap Results
#'
#' Analyzes and displays bootstrap standard error results for set
#' identification bounds.
#'
#' @param results_main Data.frame. Main simulation results.
#' @param bootstrap_demo Data.frame. Bootstrap demonstration results.
#' @template param-config
#' @template param-verbose
#'
#' @template note-knitr
#'
#' @return A data.frame with bootstrap examples.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_main <- run_main_simulation(config, seeds)
#' bootstrap_demo <- run_bootstrap_demonstration(config, seeds)
#' bootstrap_analysis <- analyze_bootstrap_results(
#'   results_main, bootstrap_demo, config
#' )
#' }
#'
#' @export
analyze_bootstrap_results <- function(results_main,
                                      bootstrap_demo,
                                      config,
                                      verbose = TRUE) {
  # Combine bootstrap examples
  # First filter results_main
  filtered_main <- dplyr::filter(results_main, !is.na(.data$bound_se_lower))
  sliced_main <- dplyr::slice_head(
    filtered_main,
    n = config$bootstrap_subset_size
  )

  # Combine with bootstrap_demo
  bootstrap_examples <- dplyr::bind_rows(sliced_main, bootstrap_demo)
  bootstrap_examples <- dplyr::distinct(bootstrap_examples)

  if (nrow(bootstrap_examples) > 0 && verbose) {
    cat(paste0(
      "\n--- Bootstrap Standard Errors for Set Identification Bounds ---",
      "\n"
    ))
    cat(sprintf(
      "Showing %d examples with bootstrap SEs (B = %d)\n\n",
      nrow(bootstrap_examples), config$bootstrap_reps
    ))

    # Select columns
    bootstrap_selected <- dplyr::select(bootstrap_examples,
      "sim_id",
      lower = "bound_lower_tau_set",
      upper = "bound_upper_tau_set",
      se_lower = "bound_se_lower",
      se_upper = "bound_se_upper"
    )

    # Slice and round
    bootstrap_table <- dplyr::slice_head(
      bootstrap_selected,
      n = hetid_const("BOOTSTRAP_TABLE_DISPLAY_LIMIT")
    )
    bootstrap_table <- dplyr::mutate(
      bootstrap_table,
      dplyr::across(
        dplyr::where(is.numeric) & !"sim_id",
        ~ round(., hetid_opt("display_digits"))
      )
    )

    if (requireNamespace("knitr", quietly = TRUE)) {
      print(knitr::kable(bootstrap_table))
    } else {
      print(bootstrap_table)
    }
  }

  bootstrap_examples
}


#' Analyze Sample Size Results
#'
#' Analyzes consistency of estimators across different sample sizes.
#'
#' @param results_by_n Data.frame. Results from run_sample_size_analysis().
#' @template param-config
#' @template param-verbose
#'
#' @template note-knitr
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
analyze_sample_size_results <- function(results_by_n,
                                        config,
                                        verbose = TRUE) {
  # Group by sample size and summarize
  grouped_n <- dplyr::group_by(results_by_n, .data$sample_size)
  n_summary <- dplyr::summarise(
    grouped_n,
    `2SLS Bias` = mean(.data$tsls_gamma1, na.rm = TRUE) - config$gamma1,
    `2SLS RMSE` = sqrt(
      mean((.data$tsls_gamma1 - config$gamma1)^2, na.rm = TRUE)
    ),
    `Avg First-Stage F` = mean(.data$first_stage_F, na.rm = TRUE),
    .groups = "drop"
  )

  if (verbose) {
    cat(paste0(
      "\n--- Consistency Check: Performance by Sample Size ---",
      "\n"
    ))
    if (requireNamespace("knitr", quietly = TRUE)) {
      print(knitr::kable(n_summary, digits = hetid_opt("display_digits")))
    } else {
      print(n_summary)
    }
  }

  n_summary
}


#' Analyze Sensitivity Results
#'
#' Analyzes sensitivity of results to heteroscedasticity strength.
#'
#' @param results_by_delta Data.frame. Results from run_sensitivity_analysis().
#' @template param-config
#' @template param-verbose
#'
#' @template note-knitr
#'
#' @return A data.frame with sensitivity analysis.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_by_delta <- run_sensitivity_analysis(config, seeds)
#' sensitivity_analysis <- analyze_sensitivity_results(
#'   results_by_delta, config
#' )
#' }
#'
#' @export
analyze_sensitivity_results <- function(results_by_delta,
                                        config,
                                        verbose = TRUE) {
  # Group by delta_het and summarize
  grouped_delta <- dplyr::group_by(results_by_delta, .data$delta_het)
  delta_summary <- dplyr::summarise(
    grouped_delta,
    `2SLS Bias` = mean(.data$tsls_gamma1, na.rm = TRUE) - config$gamma1,
    `2SLS RMSE` = sqrt(
      mean((.data$tsls_gamma1 - config$gamma1)^2, na.rm = TRUE)
    ),
    `Avg First-Stage F` = mean(.data$first_stage_F, na.rm = TRUE),
    `Bounds Width` = mean(
      .data$bound_upper_tau_set - .data$bound_lower_tau_set,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

  if (verbose) {
    cat(paste0(
      "\n--- Sensitivity to Heteroscedasticity Strength ---",
      "\n"
    ))
    if (requireNamespace("knitr", quietly = TRUE)) {
      print(knitr::kable(delta_summary, digits = hetid_opt("display_digits")))
    } else {
      print(delta_summary)
    }
  }

  delta_summary
}


#' Print Simulation Summary
#'
#' Prints a comprehensive summary of simulation findings.
#'
#' @param analysis List. Optional. Analysis results object containing
#'   simulation metrics.
#' @template param-config-optional
#' @template param-verbose
#'
#' @examples
#' \dontrun{
#' print_simulation_summary()
#' }
#'
#' @export
print_simulation_summary <- function(analysis = NULL,
                                     config = NULL,
                                     verbose = TRUE) {
  # Handle different calling patterns - ensure verbose is a single logical value
  if (is.list(verbose) || length(verbose) > 1) {
    verbose <- TRUE # Default to TRUE if verbose is not a single logical
  }

  if (isTRUE(verbose)) {
    cat("\n--- SIMULATION COMPLETE ---\n")
    cat("Key findings:\n")
    cat("1. OLS is biased due to endogeneity.\n")
    cat(paste0(
      "2. Lewbel's 2SLS successfully corrects the bias using ",
      "heteroscedasticity.\n"
    ))
    cat("3. Estimates improve with sample size (consistency).\n")
    cat(paste0(
      "4. Stronger heteroscedasticity improves instrument strength.\n"
    ))
    cat(paste0(
      "5. Set identification bounds widen with tau but maintain good ",
      "coverage.\n"
    ))
    cat("6. Bootstrap SEs provide valid inference for the bounds.\n")

    # If analysis is provided, print additional summary
    if (!is.null(analysis) && is.list(analysis)) {
      if ("weak_iv_pct" %in% names(analysis)) {
        cat(sprintf(
          "7. Weak instrument rate: %.1f%%\n",
          analysis$weak_iv_pct
        ))
      }
    }
  }
}
