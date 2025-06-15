# Test advanced edge cases for analysis functions

test_that("analyze_sensitivity_results handles edge cases", {
  config <- create_default_config()

  # Empty results
  empty_results <- data.frame()
  expect_error(
    analyze_sensitivity_results(empty_results, config, verbose = FALSE)
  )

  # Extreme delta values
  extreme_results <- data.frame(
    sim_id = 1:6,
    delta_het = rep(c(0.001, 100), each = 3),
    tsls_gamma1 = c(0.5, 0.6, 0.7, 1e6, -1e6, 0),
    first_stage_F = c(0.1, 0.2, 0.3, 1e6, 1e6, 1e6),
    bound_lower_tau_set = c(-1, -2, -3, -1e6, -1e6, -1e6),
    bound_upper_tau_set = c(1, 2, 3, 1e6, 1e6, 1e6)
  )

  analysis_extreme <- analyze_sensitivity_results(
    extreme_results, config,
    verbose = FALSE
  )
  expect_equal(nrow(analysis_extreme), 2)
  expect_true(all(is.finite(analysis_extreme$`Bounds Width`)))
})

test_that("print_simulation_summary handles edge cases", {
  # Test with NULL analysis
  expect_silent(
    print_simulation_summary(NULL, NULL, verbose = FALSE)
  )

  # Test with malformed analysis object
  bad_analysis <- list(some_field = "value")
  output <- capture.output(
    print_simulation_summary(bad_analysis, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output)))

  # Test with analysis containing weak_iv_pct
  analysis_with_weak <- list(weak_iv_pct = 75.5)
  output_weak <- capture.output(
    print_simulation_summary(analysis_with_weak, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("75.5%", output_weak)))

  # Test with various verbose parameter types
  expect_silent(
    print_simulation_summary(verbose = NULL)
  )
  expect_silent(
    print_simulation_summary(verbose = NA)
  )
  expect_silent(
    print_simulation_summary(verbose = character(0))
  )
})

test_that("analysis functions handle Inf and -Inf values", {
  config <- create_default_config()

  # Results with infinite values
  inf_results <- data.frame(
    sim_id = 1:4,
    sample_size = rep(100, 4),
    delta_het = rep(1.0, 4),
    ols_gamma1 = c(Inf, -Inf, 0.5, 0.6),
    tsls_gamma1 = c(0.3, 0.4, Inf, -Inf),
    ols_coverage = rep(TRUE, 4),
    tsls_coverage = rep(FALSE, 4),
    first_stage_F = c(10, Inf, -Inf, 15),
    bound_lower_tau0 = c(-0.5, -Inf, -0.5, -0.5),
    bound_upper_tau0 = c(Inf, 0.5, 0.5, 0.5),
    bound_lower_tau_set = c(-1.0, -1.0, -Inf, -1.0),
    bound_upper_tau_set = c(1.0, 1.0, 1.0, Inf)
  )

  suppressWarnings({
    analysis_inf <- analyze_main_results(
      inf_results, config,
      verbose = FALSE
    )
  })

  expect_type(analysis_inf, "list")
  # Some statistics should be Inf but not NaN
  expect_true(any(is.infinite(analysis_inf$summary_table$RMSE)))
})

test_that("analysis functions handle factor and character columns", {
  config <- create_default_config()

  # Results with unexpected column types
  mixed_types <- data.frame(
    sim_id = as.character(1:3), # Character instead of numeric
    sample_size = factor(c(100, 100, 100)), # Factor instead of numeric
    delta_het = c("1.0", "1.0", "1.0"), # Character
    ols_gamma1 = c(0.5, 0.6, 0.7),
    tsls_gamma1 = c(0.3, 0.4, 0.5),
    ols_coverage = c("TRUE", "FALSE", "TRUE"), # Character
    tsls_coverage = c(TRUE, FALSE, TRUE),
    first_stage_F = c(10, 15, 20),
    bound_lower_tau0 = c(-0.5, -0.6, -0.7),
    bound_upper_tau0 = c(0.5, 0.6, 0.7),
    bound_lower_tau_set = c(-1.0, -1.1, -1.2),
    bound_upper_tau_set = c(1.0, 1.1, 1.2)
  )

  # Should handle type conversions gracefully
  expect_no_error({
    suppressWarnings(
      analyze_main_results(mixed_types, config, verbose = FALSE)
    )
  })
})
