#' Run Lewbel Monte Carlo Simulation with DF Adjustment
#'
#' This is a high-level wrapper around run_lewbel_monte_carlo that adds
#' support for degrees of freedom adjustment.
#'
#' @param config Configuration list from create_default_config()
#' @template param-df-adjust
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#'
#' @return List containing simulation results and configuration
#' @export
run_lewbel_monte_carlo_df <- function(config,
                                      df_adjust = "asymptotic",
                                      verbose = TRUE) {
  # Add df_adjust to config
  config$df_adjust <- df_adjust

  if (verbose) {
    cat(sprintf("Running Monte Carlo with %s standard errors\n", df_adjust))
  }

  # Run the standard Monte Carlo
  run_lewbel_monte_carlo(config, verbose = verbose)
}

#' Compare Results with Different DF Adjustments
#'
#' Run simulations with both asymptotic and finite sample standard errors
#' and compare the results.
#'
#' @param config Configuration list
#' @param verbose Logical. Whether to print progress
#'
#' @return List containing results for both methods
#' @export
compare_df_adjustments <- function(config, verbose = TRUE) {
  if (verbose) {
    cat("Running comparison of DF adjustments...\n")
  }

  # Run with asymptotic SEs
  results_asymp <- run_lewbel_monte_carlo_df(config,
    df_adjust = "asymptotic",
    verbose = verbose
  )

  # Run with finite sample SEs
  results_finite <- run_lewbel_monte_carlo_df(config,
    df_adjust = "finite",
    verbose = verbose
  )

  # Compare coverage rates
  comparison <- data.frame(
    method = c("Asymptotic", "Finite Sample"),
    ols_coverage = c(
      mean(results_asymp$results$main$ols_coverage, na.rm = TRUE),
      mean(results_finite$results$main$ols_coverage, na.rm = TRUE)
    ),
    tsls_coverage = c(
      mean(results_asymp$results$main$tsls_coverage, na.rm = TRUE),
      mean(results_finite$results$main$tsls_coverage, na.rm = TRUE)
    ),
    mean_ols_se = c(
      mean(results_asymp$results$main$ols_se, na.rm = TRUE),
      mean(results_finite$results$main$ols_se, na.rm = TRUE)
    ),
    mean_tsls_se = c(
      mean(results_asymp$results$main$tsls_se, na.rm = TRUE),
      mean(results_finite$results$main$tsls_se, na.rm = TRUE)
    )
  )

  if (verbose) {
    cat("\n=== Coverage Rate Comparison ===\n")
    print(comparison)

    # Calculate SE ratio
    se_ratio <- comparison$mean_tsls_se[2] / comparison$mean_tsls_se[1]
    cat(sprintf("\nFinite/Asymptotic SE ratio: %.4f\n", se_ratio))
  }

  list(
    asymptotic = results_asymp,
    finite = results_finite,
    comparison = comparison
  )
}
