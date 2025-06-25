# Test analysis functions
# This consolidates tests from all test levels

# Main results analysis ------------------------------------------------------

test_that("analyze_main_results produces valid output", {
  skip_if_not_test_level("fast")

  # Create test config
  test_config_minimal <- create_test_config(
    num_simulations = 5,
    bootstrap_reps = 10
  )
  seeds_small <- generate_all_seeds(test_config_minimal)

  suppressMessages({
    main_results <- run_main_simulation(test_config_minimal, seeds_small)
    analysis <- analyze_main_results(
      main_results,
      test_config_minimal,
      verbose = FALSE
    )
  })

  expect_type(analysis, "list")
  expect_true(all(c(
    "summary_table", "bounds_summary",
    "weak_iv_pct", "results_clean"
  ) %in% names(analysis)))

  assert_valid_dataframe(analysis$summary_table)
  assert_valid_dataframe(analysis$bounds_summary)
  assert_valid_dataframe(analysis$results_clean)
  assert_valid_numeric(analysis$weak_iv_pct, length = 1)

  # Check summary table has expected structure
  expect_true("Estimator" %in% names(analysis$summary_table))
  expect_true(nrow(analysis$summary_table) >= 2) # OLS and 2SLS
})

test_that("analyze_main_results handles knitr formatting", {
  skip_if_not_test_level("integration")
  skip_if_not_installed("knitr")

  test_config_minimal <- create_test_config(
    num_simulations = 5,
    bootstrap_reps = 10
  )
  seeds <- generate_all_seeds(test_config_minimal)

  suppressMessages({
    main_results <- run_main_simulation(test_config_minimal, seeds)

    # Test with knitr available
    analysis_knitr <- analyze_main_results(
      main_results,
      test_config_minimal,
      verbose = FALSE
    )
  })

  # Should have kable-formatted tables when knitr is available
  expect_true("summary_table" %in% names(analysis_knitr))
})

# Sample size analysis -------------------------------------------------------

test_that("analyze_sample_size_results summarizes correctly", {
  skip_if_not_test_level("fast")

  config_tiny <- create_test_config(
    n_reps_by_n = 2,
    sample_sizes = c(100, 200)
  )
  seeds_tiny <- generate_all_seeds(config_tiny)

  suppressMessages({
    sample_results <- run_sample_size_analysis(config_tiny, seeds_tiny)
    sample_analysis <- analyze_sample_size_results(
      sample_results,
      config_tiny,
      verbose = FALSE
    )
  })

  assert_valid_dataframe(sample_analysis)
  expect_true("sample_size" %in% names(sample_analysis))
  expect_true("2SLS Bias" %in% names(sample_analysis))
  expect_equal(nrow(sample_analysis), length(config_tiny$sample_sizes))
})

# Sensitivity analysis -------------------------------------------------------

test_that("analyze_sensitivity_results works with different delta values", {
  skip_if_not_test_level("fast")

  config_delta <- create_test_config(
    n_reps_by_delta = 2,
    delta_het_values = c(0.5, 1.0)
  )
  seeds_delta <- generate_all_seeds(config_delta)

  suppressMessages({
    delta_results <- run_sensitivity_analysis(config_delta, seeds_delta)
    delta_analysis <- analyze_sensitivity_results(
      delta_results,
      config_delta,
      verbose = FALSE
    )
  })

  assert_valid_dataframe(delta_analysis)
  expect_true("delta_het" %in% names(delta_analysis))
  expect_true("2SLS Bias" %in% names(delta_analysis))
  expect_equal(nrow(delta_analysis), length(config_delta$delta_het_values))
})

# Bootstrap analysis ---------------------------------------------------------

test_that("analyze_bootstrap_results computes standard errors", {
  skip_if_not_test_level("integration")

  config_boot <- create_test_config(
    num_simulations = 5,
    bootstrap_demo_size = 3,
    bootstrap_reps = 20
  )
  seeds_boot <- generate_all_seeds(config_boot)

  suppressMessages({
    main_results <- run_main_simulation(config_boot, seeds_boot)
    boot_results <- run_bootstrap_demonstration(config_boot, seeds_boot)
    boot_analysis <- analyze_bootstrap_results(
      main_results,
      boot_results,
      config_boot,
      verbose = FALSE
    )
  })

  assert_valid_dataframe(boot_analysis)
  expect_true(all(c(
    "sim_id", "bound_lower_tau_set", "bound_upper_tau_set",
    "bound_se_lower", "bound_se_upper"
  ) %in% names(boot_analysis)))

  # Standard errors should be positive
  expect_true(all(boot_analysis$bound_se_lower > 0))
  expect_true(all(boot_analysis$bound_se_upper > 0))
})

# Edge cases and error handling ----------------------------------------------

test_that("analysis functions handle empty results gracefully", {
  skip_if_not_test_level("fast")

  # Create empty results
  empty_results <- data.frame(
    ols_gamma1 = numeric(0),
    tsls_gamma1 = numeric(0),
    first_stage_F = numeric(0)
  )

  test_config_minimal <- create_test_config(
    num_simulations = 5,
    bootstrap_reps = 10
  )

  analysis <- analyze_main_results(
    empty_results,
    test_config_minimal,
    verbose = FALSE
  )

  expect_type(analysis, "list")
  expect_equal(nrow(analysis$results_clean), 0)
})

test_that("analysis functions handle NA values", {
  skip_if_not_test_level("integration")

  # Create results with some NAs
  results_with_na <- data.frame(
    ols_gamma1 = c(0.5, NA, 0.7),
    tsls_gamma1 = c(NA, 0.6, 0.8),
    first_stage_F = c(10, 15, NA)
  )

  # Create config for this test
  test_config_minimal <- create_test_config(
    num_simulations = 5,
    bootstrap_reps = 10
  )

  analysis <- analyze_main_results(
    results_with_na,
    test_config_minimal,
    verbose = FALSE
  )

  # Should handle NAs appropriately
  expect_true(nrow(analysis$results_clean) <= nrow(results_with_na))
})

# Comprehensive tests --------------------------------------------------------

test_that("full analysis pipeline works end-to-end", {
  skip_if_not_test_level("comprehensive")

  config <- create_test_config(
    num_simulations = 50,
    bootstrap_demo_size = 10,
    sample_sizes = c(100, 200, 500),
    delta_het_values = c(0.5, 1.0, 1.5)
  )
  seeds <- generate_all_seeds(config)

  suppressMessages({
    # Run all simulations
    main_results <- run_main_simulation(config, seeds)
    sample_results <- run_sample_size_analysis(config, seeds)
    delta_results <- run_sensitivity_analysis(config, seeds)
    boot_results <- run_bootstrap_demonstration(config, seeds)

    # Analyze all results
    main_analysis <- analyze_main_results(main_results, config, verbose = FALSE)
    sample_analysis <- analyze_sample_size_results(sample_results, config, verbose = FALSE)
    delta_analysis <- analyze_sensitivity_results(delta_results, config, verbose = FALSE)
    boot_analysis <- analyze_bootstrap_results(
      main_results, boot_results, config,
      verbose = FALSE
    )
  })

  # Verify all analyses produced valid output
  expect_type(main_analysis$weak_iv_pct, "double")
  expect_true(main_analysis$weak_iv_pct >= 0 && main_analysis$weak_iv_pct <= 100)

  expect_equal(nrow(sample_analysis), length(config$sample_sizes))
  expect_equal(nrow(delta_analysis), length(config$delta_het_values))
  expect_true(nrow(boot_analysis) > 0)
})

# Print summary function -----------------------------------------------------

test_that("print_simulation_summary produces output", {
  skip_if_not_test_level("fast")

  output <- capture.output(print_simulation_summary())

  expect_true(length(output) > 0)
  expect_true(any(grepl("SIMULATION COMPLETE", output)))
  expect_true(any(grepl("Key findings", output)))
})
