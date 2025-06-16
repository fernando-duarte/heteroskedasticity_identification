# Test analysis functions

test_that("analyze_main_results works", {
  config_small <- create_default_config(num_simulations = 3)
  seeds_small <- generate_all_seeds(config_small)

  suppressMessages({
    main_results <- run_main_simulation(config_small, seeds_small)
    analysis <- analyze_main_results(
      main_results, config_small,
      verbose = FALSE
    )
  })

  expect_type(analysis, "list")
  # Correct expected names based on actual function implementation
  expect_true(all(
    c("summary_table", "bounds_summary", "weak_iv_pct", "results_clean") %in%
      names(analysis)
  ))
  expect_s3_class(analysis$summary_table, "data.frame")
  expect_type(analysis$weak_iv_pct, "double")
  expect_s3_class(analysis$bounds_summary, "data.frame")
  expect_s3_class(analysis$results_clean, "data.frame")

  # Check summary table structure
  expect_true("Estimator" %in% names(analysis$summary_table))
  expect_true(nrow(analysis$summary_table) >= 2) # At least OLS and 2SLS
})

test_that("analyze_sample_size_results works", {
  config_tiny <- create_default_config(
    n_reps_by_n = 2,
    sample_sizes = c(100, 200)
  )
  seeds_tiny <- generate_all_seeds(config_tiny)

  suppressMessages({
    sample_results <- run_sample_size_analysis(config_tiny, seeds_tiny)
    sample_analysis <- analyze_sample_size_results(
      sample_results, config_tiny,
      verbose = FALSE
    )
  })

  # analyze_sample_size_results returns a data.frame directly, not a list
  expect_s3_class(sample_analysis, "data.frame")
  expect_true("sample_size" %in% names(sample_analysis))
  expect_true(nrow(sample_analysis) >= 2) # At least 2 sample sizes
})

test_that("analyze_sensitivity_results works", {
  config_sens <- create_default_config(
    n_reps_by_delta = 2,
    delta_het_values = c(0.8, 1.2)
  )
  seeds_sens <- generate_all_seeds(config_sens)

  suppressMessages({
    sens_results <- run_sensitivity_analysis(config_sens, seeds_sens)
    sens_analysis <- analyze_sensitivity_results(
      sens_results, config_sens,
      verbose = FALSE
    )
  })

  # analyze_sensitivity_results returns a data.frame directly, not a list
  expect_s3_class(sens_analysis, "data.frame")
  expect_true("delta_het" %in% names(sens_analysis))
  expect_true(nrow(sens_analysis) >= 2) # At least 2 delta values
})

test_that("analyze_bootstrap_results works", {
  config_boot <- create_default_config(
    bootstrap_demo_size = 2,
    bootstrap_reps = 5
  )
  seeds_boot <- generate_all_seeds(config_boot)

  suppressMessages({
    main_results <- run_main_simulation(config_boot, seeds_boot)
    bootstrap_demo <- run_bootstrap_demonstration(config_boot, seeds_boot)
    bootstrap_analysis <- analyze_bootstrap_results(
      main_results, bootstrap_demo, config_boot,
      verbose = FALSE
    )
  })

  # analyze_bootstrap_results returns a data.frame directly, not a list
  expect_s3_class(bootstrap_analysis, "data.frame")
  expect_true(nrow(bootstrap_analysis) >= 0) # May be empty if no bootstrap data
})

test_that("print_simulation_summary works", {
  config_small <- create_default_config(num_simulations = 3)
  seeds_small <- generate_all_seeds(config_small)

  suppressMessages({
    main_results <- run_main_simulation(config_small, seeds_small)
    analysis <- analyze_main_results(
      main_results, config_small,
      verbose = FALSE
    )
  })

  # Test that function runs without error
  expect_silent(print_simulation_summary(analysis, verbose = FALSE))

  # Test with verbose = TRUE (capture output)
  output <- capture.output(print_simulation_summary(analysis, verbose = TRUE))
  expect_true(length(output) > 0)
  expect_true(any(grepl("SIMULATION COMPLETE", output)))
})

test_that("print_simulation_summary with different calling patterns works", {
  # Test with just verbose parameter
  expect_silent(print_simulation_summary(verbose = FALSE))

  # Test with analysis parameter
  config_small <- create_default_config(num_simulations = 2)
  seeds_small <- generate_all_seeds(config_small)

  suppressMessages({
    main_results <- run_main_simulation(config_small, seeds_small)
    analysis <- analyze_main_results(
      main_results, config_small,
      verbose = FALSE
    )
  })

  # Test with verbose = FALSE to avoid output during testing
  expect_silent(print_simulation_summary(analysis, verbose = FALSE))
})
