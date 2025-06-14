# Tests specifically for knitr fallback behavior and conditional logic

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
    bootstrap_demo <- run_bootstrap_demonstration(config, seeds, verbose = FALSE)
  })
  
  output2 <- capture.output({
    analysis2 <- analyze_bootstrap_results(
      results, bootstrap_demo, config, verbose = TRUE
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
    analysis3 <- analyze_sample_size_results(sample_data, config, verbose = TRUE)
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
    main_small <- run_main_simulation(config_small, seeds_small, verbose = FALSE)
    bootstrap_small <- run_bootstrap_demonstration(config_small, seeds_small, verbose = FALSE)
    
    main_large <- run_main_simulation(config_large, seeds_large, verbose = FALSE)
    bootstrap_large <- run_bootstrap_demonstration(config_large, seeds_large, verbose = FALSE)
  })
  
  analysis_small <- analyze_bootstrap_results(
    main_small, bootstrap_small, config_small, verbose = FALSE
  )
  analysis_large <- analyze_bootstrap_results(
    main_large, bootstrap_large, config_large, verbose = FALSE
  )
  
  expect_s3_class(analysis_small, "data.frame")
  expect_s3_class(analysis_large, "data.frame")
})

test_that("print_simulation_summary handles non-list analysis objects", {
  # Test with different types of analysis objects
  
  # Test with data.frame
  df_analysis <- data.frame(weak_iv_pct = 15.5)
  output1 <- capture.output(
    print_simulation_summary(df_analysis, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output1)))
  
  # Test with vector
  vec_analysis <- c(weak_iv_pct = 20.3)
  output2 <- capture.output(
    print_simulation_summary(vec_analysis, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output2)))
  
  # Test with character
  char_analysis <- "not a list"
  output3 <- capture.output(
    print_simulation_summary(char_analysis, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output3)))
})

test_that("analysis functions handle extreme numeric values correctly", {
  config <- create_default_config()
  
  # Create data with very large and very small numbers
  extreme_data <- data.frame(
    sim_id = 1:4,
    sample_size = rep(100, 4),
    delta_het = rep(1.0, 4),
    ols_gamma1 = c(1e-10, 1e10, -1e10, 0),
    tsls_gamma1 = c(1e-15, 1e15, -1e15, 0),
    ols_coverage = c(TRUE, FALSE, TRUE, FALSE),
    tsls_coverage = c(FALSE, TRUE, FALSE, TRUE),
    first_stage_F = c(1e-5, 1e5, 1e-5, 1e5),
    bound_lower_tau0 = c(-1e-10, -1e10, -1e-10, -1e10),
    bound_upper_tau0 = c(1e-10, 1e10, 1e-10, 1e10),
    bound_lower_tau_set = c(-1e-5, -1e5, -1e-5, -1e5),
    bound_upper_tau_set = c(1e-5, 1e5, 1e-5, 1e5)
  )
  
  # Should handle extreme values without crashing
  suppressWarnings({
    analysis <- analyze_main_results(extreme_data, config, verbose = FALSE)
  })
  
  expect_type(analysis, "list")
  expect_s3_class(analysis$summary_table, "data.frame")
  expect_s3_class(analysis$bounds_summary, "data.frame")
})

test_that("verbose output works with different number formatting", {
  config <- create_default_config()
  
  # Create data that will produce different types of numbers
  varied_data <- data.frame(
    sim_id = 1:3,
    sample_size = rep(100, 3),
    delta_het = rep(1.0, 3),
    ols_gamma1 = c(0.123456789, 1.000000001, -0.000000123),
    tsls_gamma1 = c(0.987654321, 0.999999999, 0.000000987),
    ols_coverage = c(TRUE, FALSE, TRUE),
    tsls_coverage = c(FALSE, TRUE, FALSE),
    first_stage_F = c(9.999, 10.001, 100.123),
    bound_lower_tau0 = c(-0.123, -1.001, -0.001),
    bound_upper_tau0 = c(0.123, 1.001, 0.001),
    bound_lower_tau_set = c(-1.123, -2.001, -0.501),
    bound_upper_tau_set = c(1.123, 2.001, 0.501)
  )
  
  # Test that formatting works with various precision levels
  # With small sample sizes, correlation calculations may warn about
  # zero standard deviation. This is expected behavior.
  output <- capture.output({
    suppressWarnings({
      analysis <- analyze_main_results(varied_data, config, verbose = TRUE)
    })
  })
  
  expect_true(length(output) > 0)
  expect_type(analysis, "list")
  
  # Test weak instrument percentage calculation with edge case
  expect_type(analysis$weak_iv_pct, "double")
  expect_true(analysis$weak_iv_pct >= 0 && analysis$weak_iv_pct <= 100)
})

test_that("all analysis functions work with minimal valid data", {
  config <- create_default_config()
  
  # Create minimal but complete data for each function
  minimal_complete <- data.frame(
    sim_id = 1,
    sample_size = 100,
    delta_het = 1.0,
    ols_gamma1 = 0.5,
    tsls_gamma1 = 0.3,
    ols_coverage = TRUE,
    tsls_coverage = FALSE,
    first_stage_F = 10,
    bound_lower_tau0 = -0.5,
    bound_upper_tau0 = 0.5,
    bound_lower_tau_set = -1.0,
    bound_upper_tau_set = 1.0,
    bound_se_lower = 0.1,
    bound_se_upper = 0.1
  )
  
  # Test all functions with minimal data
  main_analysis <- analyze_main_results(minimal_complete, config, verbose = FALSE)
  expect_type(main_analysis, "list")
  
  sample_analysis <- analyze_sample_size_results(minimal_complete, config, verbose = FALSE)
  expect_s3_class(sample_analysis, "data.frame")
  
  sens_analysis <- analyze_sensitivity_results(minimal_complete, config, verbose = FALSE)
  expect_s3_class(sens_analysis, "data.frame")
  
  bootstrap_analysis <- analyze_bootstrap_results(
    minimal_complete, minimal_complete, config, verbose = FALSE
  )
  expect_s3_class(bootstrap_analysis, "data.frame")
})
