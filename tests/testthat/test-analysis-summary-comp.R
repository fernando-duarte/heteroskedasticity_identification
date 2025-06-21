# Tests for simulation summary and error handling functions
# Covers print_simulation_summary() with various parameter combinations
# and edge cases
# Also tests missing column handling across all analysis functions
# Dependencies: testthat, hetid package functions

test_that("print_simulation_summary handles all parameter combinations", {
  skip_if_not_comprehensive_test()
  # Test with NULL analysis and config
  expect_silent(print_simulation_summary(NULL, NULL, verbose = FALSE))

  # Test with verbose = TRUE and NULL inputs
  output1 <- capture.output(
    print_simulation_summary(NULL, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output1)))
  expect_true(any(grepl("Key findings:", output1)))

  # Test with analysis object containing weak_iv_pct
  analysis_with_weak <- list(weak_iv_pct = 25.7)
  output2 <- capture.output(
    print_simulation_summary(analysis_with_weak, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("25.7%", output2)))
  expect_true(any(grepl("Weak instrument rate", output2)))

  # Test with analysis object without weak_iv_pct
  analysis_without_weak <- list(other_field = "value")
  output3 <- capture.output(
    print_simulation_summary(analysis_without_weak, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output3)))
  expect_false(any(grepl("Weak instrument rate", output3)))
})

test_that("print_simulation_summary handles edge cases in verbose parameter", {
  skip_if_not_comprehensive_test()
  # Test with list as verbose parameter - should produce output
  # (defaults to TRUE)
  output1 <- capture.output(
    print_simulation_summary(verbose = list(a = 1, b = 2))
  )
  expect_true(length(output1) > 0)

  # Test with vector as verbose parameter - should produce output
  # (defaults to TRUE)
  output2 <- capture.output(
    print_simulation_summary(verbose = c(TRUE, FALSE, TRUE))
  )
  expect_true(length(output2) > 0)

  # Test with numeric verbose parameter
  expect_silent(
    print_simulation_summary(verbose = 1)
  )

  # Test with character verbose parameter
  expect_silent(
    print_simulation_summary(verbose = "true")
  )
})

test_that("all analysis functions handle missing columns gracefully", {
  skip_if_not_comprehensive_test()
  config <- create_default_config()

  # Test with minimal data frame missing some expected columns
  minimal_results <- data.frame(
    sim_id = 1:2,
    ols_gamma1 = c(0.5, 0.6),
    tsls_gamma1 = c(0.3, 0.4)
  )

  # Should complete with warnings due to missing required columns
  # When columns are missing, mean() will warn about non-numeric arguments
  # This is expected behavior when required columns are absent
  suppressWarnings({
    result <- analyze_main_results(minimal_results, config, verbose = FALSE)
  })

  expect_type(result, "list")
  # Should have NA values for missing columns
  expect_true(is.na(result$bounds_summary$`Point ID Check`))

  # Test sample size analysis with missing columns
  minimal_sample <- data.frame(
    sim_id = 1:2,
    sample_size = c(100, 200)
  )

  expect_error(
    analyze_sample_size_results(minimal_sample, config, verbose = FALSE)
  )

  # Test sensitivity analysis with missing columns
  minimal_sens <- data.frame(
    sim_id = 1:2,
    delta_het = c(0.8, 1.2)
  )

  expect_error(
    analyze_sensitivity_results(minimal_sens, config, verbose = FALSE)
  )
})
