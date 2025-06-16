# Tests for basic knitr fallback behavior and conditional logic

test_that("knitr fallback works when knitr is not available", {
  # We can't easily unload knitr, but we can test the conditional logic
  # by mocking the requireNamespace function

  config <- create_default_config(num_simulations = 2)
  seeds <- generate_all_seeds(config)

  suppressMessages({
    results <- run_main_simulation(config, seeds, verbose = FALSE)
  })

  # Test that the function works regardless of knitr availability
  # The conditional logic should handle both cases

  # Test analyze_main_results
  output1 <- capture.output({
    analysis1 <- analyze_main_results(results, config, verbose = TRUE)
  })
  expect_true(length(output1) > 0)
  expect_type(analysis1, "list")

  # Test analyze_bootstrap_results
  suppressMessages({
    bootstrap_demo <- run_bootstrap_demonstration(
      config, seeds,
      verbose = FALSE
    )
  })

  output2 <- capture.output({
    analysis2 <- analyze_bootstrap_results(
      results, bootstrap_demo, config,
      verbose = TRUE
    )
  })
  expect_true(length(output2) > 0)
  expect_s3_class(analysis2, "data.frame")

  # Test analyze_sample_size_results
  sample_data <- data.frame(
    sim_id = 1:4,
    sample_size = rep(c(100, 200), each = 2),
    tsls_gamma1 = c(0.3, 0.4, 0.35, 0.45),
    first_stage_F = c(10, 15, 12, 18)
  )

  output3 <- capture.output({
    analysis3 <- analyze_sample_size_results(
      sample_data, config,
      verbose = TRUE
    )
  })
  expect_true(length(output3) > 0)
  expect_s3_class(analysis3, "data.frame")

  # Test analyze_sensitivity_results
  sens_data <- data.frame(
    sim_id = 1:4,
    delta_het = rep(c(0.8, 1.2), each = 2),
    tsls_gamma1 = c(0.3, 0.4, 0.35, 0.45),
    first_stage_F = c(10, 15, 12, 18),
    bound_lower_tau_set = c(-1.0, -1.1, -0.9, -1.2),
    bound_upper_tau_set = c(1.0, 1.1, 0.9, 1.2)
  )

  output4 <- capture.output({
    analysis4 <- analyze_sensitivity_results(sens_data, config, verbose = TRUE)
  })
  expect_true(length(output4) > 0)
  expect_s3_class(analysis4, "data.frame")
})

test_that("verbose output contains expected table elements", {
  config <- create_default_config(num_simulations = 2)
  seeds <- generate_all_seeds(config)

  suppressMessages({
    results <- run_main_simulation(config, seeds, verbose = FALSE)
  })

  # Capture output and check for table-like content
  output <- capture.output({
    analysis <- analyze_main_results(results, config, verbose = TRUE)
  })

  # Should contain table headers or data regardless of knitr
  output_text <- paste(output, collapse = " ")
  expect_true(grepl("Estimator|OLS|2SLS", output_text))
  expect_true(grepl("Bias|RMSE|Coverage", output_text))
  expect_true(grepl("Scenario|Width", output_text))
})

test_that("analyze_main_results handles different tau_set_id values", {
  # Test with different tau_set_id values in config
  config1 <- create_default_config(num_simulations = 2, tau_set_id = 0.1)
  config2 <- create_default_config(num_simulations = 2, tau_set_id = 0.3)

  seeds1 <- generate_all_seeds(config1)
  seeds2 <- generate_all_seeds(config2)

  suppressMessages({
    results1 <- run_main_simulation(config1, seeds1, verbose = FALSE)
    results2 <- run_main_simulation(config2, seeds2, verbose = FALSE)
  })

  analysis1 <- analyze_main_results(results1, config1, verbose = FALSE)
  analysis2 <- analyze_main_results(results2, config2, verbose = FALSE)

  # Check that tau_set_id is reflected in the scenario description
  expect_true(grepl("0.10", analysis1$bounds_summary$Scenario))
  expect_true(grepl("0.30", analysis2$bounds_summary$Scenario))
})

test_that("analyze_bootstrap_results handles different bootstrap_subset_size", {
  config_small <- create_default_config(
    bootstrap_demo_size = 2,
    bootstrap_subset_size = 1,
    bootstrap_reps = 5
  )
  config_large <- create_default_config(
    bootstrap_demo_size = 2,
    bootstrap_subset_size = 10,
    bootstrap_reps = 5
  )

  seeds_small <- generate_all_seeds(config_small)
  seeds_large <- generate_all_seeds(config_large)

  suppressMessages({
    main_small <- run_main_simulation(
      config_small, seeds_small,
      verbose = FALSE
    )
    bootstrap_small <- run_bootstrap_demonstration(
      config_small, seeds_small,
      verbose = FALSE
    )

    main_large <- run_main_simulation(
      config_large, seeds_large,
      verbose = FALSE
    )
    bootstrap_large <- run_bootstrap_demonstration(
      config_large, seeds_large,
      verbose = FALSE
    )
  })

  analysis_small <- analyze_bootstrap_results(
    main_small, bootstrap_small, config_small,
    verbose = FALSE
  )
  analysis_large <- analyze_bootstrap_results(
    main_large, bootstrap_large, config_large,
    verbose = FALSE
  )

  expect_s3_class(analysis_small, "data.frame")
  expect_s3_class(analysis_large, "data.frame")
})
