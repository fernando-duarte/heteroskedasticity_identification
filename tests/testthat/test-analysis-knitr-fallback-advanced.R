# Tests for advanced knitr fallback formatting and edge cases

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
  main_analysis <- analyze_main_results(
    minimal_complete, config,
    verbose = FALSE
  )
  expect_type(main_analysis, "list")

  sample_analysis <- analyze_sample_size_results(
    minimal_complete, config,
    verbose = FALSE
  )
  expect_s3_class(sample_analysis, "data.frame")

  sens_analysis <- analyze_sensitivity_results(
    minimal_complete, config,
    verbose = FALSE
  )
  expect_s3_class(sens_analysis, "data.frame")

  bootstrap_analysis <- analyze_bootstrap_results(
    minimal_complete, minimal_complete, config,
    verbose = FALSE
  )
  expect_s3_class(bootstrap_analysis, "data.frame")
})
