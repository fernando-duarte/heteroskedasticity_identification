# Tests for analysis function edge cases (correlation, zero variance, extreme values)

test_that("analyze_main_results handles correlation edge cases", {
  config <- create_default_config()

  # Create results where correlation calculation might fail
  problematic_results <- data.frame(
    sim_id = 1:3,
    sample_size = rep(100, 3),
    delta_het = rep(1.0, 3),
    ols_gamma1 = c(0.5, 0.6, 0.7),
    tsls_gamma1 = c(0.3, 0.4, 0.5),
    ols_coverage = c(TRUE, FALSE, TRUE),
    tsls_coverage = c(FALSE, TRUE, FALSE),
    first_stage_F = c(10, 15, 20),
    bound_lower_tau0 = c(0.3, 0.4, 0.5), # Same as tsls_gamma1
    bound_upper_tau0 = c(0.3, 0.4, 0.5), # Same as tsls_gamma1
    bound_lower_tau_set = c(-1.0, -1.1, -1.2),
    bound_upper_tau_set = c(1.0, 1.1, 1.2)
  )

  analysis <- analyze_main_results(
    problematic_results, config,
    verbose = FALSE
  )

  # Should handle perfect correlation case
  expect_type(analysis$bounds_summary$`Point ID Check`, "double")
  expect_true(is.finite(analysis$bounds_summary$`Point ID Check`))
})

test_that("analyze_main_results handles zero variance cases", {
  config <- create_default_config()

  # Create results with zero variance in some variables
  zero_var_results <- data.frame(
    sim_id = 1:3,
    sample_size = rep(100, 3),
    delta_het = rep(1.0, 3),
    ols_gamma1 = rep(0.5, 3), # Zero variance
    tsls_gamma1 = rep(0.3, 3), # Zero variance
    ols_coverage = rep(TRUE, 3),
    tsls_coverage = rep(FALSE, 3),
    first_stage_F = rep(10, 3), # Zero variance
    bound_lower_tau0 = rep(0.3, 3),
    bound_upper_tau0 = rep(0.3, 3),
    bound_lower_tau_set = rep(-1.0, 3),
    bound_upper_tau_set = rep(1.0, 3)
  )

  # When all values are identical (zero variance), cor() will warn about
  # "the standard deviation is zero". This is expected behavior.
  suppressWarnings({
    analysis <- analyze_main_results(
      zero_var_results, config,
      verbose = FALSE
    )
  })

  # Should handle zero variance gracefully
  # RMSE is calculated against config$gamma1, so won't be 0 unless estimates equal gamma1
  expect_type(analysis$summary_table$RMSE, "double")
  expect_true(all(is.finite(analysis$summary_table$RMSE)))
  expect_equal(analysis$weak_iv_pct, 0) # No weak instruments (F=10 > 10 threshold)
})

test_that("analysis functions handle single row data frames", {
  config <- create_default_config()

  # Single row for main analysis
  single_row <- data.frame(
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
    bound_upper_tau_set = 1.0
  )

  analysis <- analyze_main_results(single_row, config, verbose = FALSE)
  expect_type(analysis, "list")
  expect_equal(nrow(analysis$summary_table), 2)

  # Single row for sample size analysis
  single_sample <- data.frame(
    sim_id = 1,
    sample_size = 100,
    tsls_gamma1 = 0.3,
    first_stage_F = 10
  )

  sample_analysis <- analyze_sample_size_results(
    single_sample, config,
    verbose = FALSE
  )
  expect_s3_class(sample_analysis, "data.frame")
  expect_equal(nrow(sample_analysis), 1)

  # Single row for sensitivity analysis
  single_sens <- data.frame(
    sim_id = 1,
    delta_het = 1.0,
    tsls_gamma1 = 0.3,
    first_stage_F = 10,
    bound_lower_tau_set = -1.0,
    bound_upper_tau_set = 1.0
  )

  sens_analysis <- analyze_sensitivity_results(
    single_sens, config,
    verbose = FALSE
  )
  expect_s3_class(sens_analysis, "data.frame")
  expect_equal(nrow(sens_analysis), 1)
})

test_that("verbose output formatting works with different table sizes", {
  config <- create_default_config()

  # Create results with many rows to test table formatting
  many_rows <- data.frame(
    sim_id = 1:10,
    sample_size = rep(c(100, 200), each = 5),
    delta_het = rep(c(0.8, 1.2), each = 5),
    ols_gamma1 = runif(10, 0.4, 0.6),
    tsls_gamma1 = runif(10, 0.2, 0.4),
    ols_coverage = sample(c(TRUE, FALSE), 10, replace = TRUE),
    tsls_coverage = sample(c(TRUE, FALSE), 10, replace = TRUE),
    first_stage_F = runif(10, 5, 25),
    bound_lower_tau0 = runif(10, -0.6, -0.4),
    bound_upper_tau0 = runif(10, 0.4, 0.6),
    bound_lower_tau_set = runif(10, -1.2, -0.8),
    bound_upper_tau_set = runif(10, 0.8, 1.2)
  )

  # Test main analysis verbose output
  output_main <- capture.output({
    analyze_main_results(many_rows, config, verbose = TRUE)
  })
  expect_true(length(output_main) > 0)

  # Test sample size analysis verbose output
  output_sample <- capture.output({
    analyze_sample_size_results(many_rows, config, verbose = TRUE)
  })
  expect_true(length(output_sample) > 0)

  # Test sensitivity analysis verbose output
  output_sens <- capture.output({
    analyze_sensitivity_results(many_rows, config, verbose = TRUE)
  })
  expect_true(length(output_sens) > 0)
})


