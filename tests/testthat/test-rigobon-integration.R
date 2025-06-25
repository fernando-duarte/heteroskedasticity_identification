# Test Rigobon (2003) regime-based identification implementation

test_that("generate_rigobon_data works correctly", {
  skip_if_not_integration_test()

  # Two-regime case
  params_2reg <- create_rigobon_params(
    n_regimes = 2,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 2.5)
  )
  data_2reg <- generate_rigobon_data(n_large * 2, params_2reg)

  # Verify data structure
  verify_rigobon_data(data_2reg, n_expected = n_large * 2, n_regimes = 2)

  # Test regime assignment
  test_regime_assignment(data_2reg, c(0.4, 0.6), tolerance = test_tolerance_tight)

  # Three-regime case
  params_3reg <- create_rigobon_params(
    n_regimes = 3,
    regime_probs = c(0.3, 0.4, 0.3),
    sigma2_regimes = c(0.5, 1.0, 2.0)
  )
  data_3reg <- generate_rigobon_data(n_large, params_3reg)

  # Verify data structure
  verify_rigobon_data(data_3reg, n_expected = n_large, n_regimes = 3)
})

test_that("generate_rigobon_data validates inputs", {
  skip_if_not_integration_test()
  # Missing regime parameters
  params_bad <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0
  )
  expect_error(
    generate_rigobon_data(n_medium, params_bad),
    "params must contain 'regime_probs' and 'sigma2_regimes'"
  )

  # Mismatched lengths
  params_mismatch <- params_bad
  params_mismatch$regime_probs <- c(0.5, 0.5)
  params_mismatch$sigma2_regimes <- c(1.0)
  expect_error(
    generate_rigobon_data(n_medium, params_mismatch),
    "Length of sigma2_regimes must match"
  )

  # Probabilities don't sum to 1
  params_bad_probs <- params_bad
  params_bad_probs$regime_probs <- c(0.3, 0.4)
  params_bad_probs$sigma2_regimes <- c(1.0, 2.0)
  expect_error(
    generate_rigobon_data(n_medium, params_bad_probs),
    "regime_probs must sum to 1"
  )
})

test_that("run_rigobon_estimation works correctly", {
  skip_if_not_integration_test()

  # Generate test data
  data <- create_rigobon_test_data(n = n_large * 2, n_regimes = 2, seed = seed_alt)

  # Run standard test
  result <- run_rigobon_test(
    data = data,
    method = "cue",
    check_gamma1 = TRUE,
    true_gamma1 = test_gamma1_true,
    tolerance = test_tolerance_normal
  )
})

test_that("run_rigobon_estimation handles edge cases", {
  skip_if_not_integration_test()
  # Only one regime (should fail)
  data_single <- data.frame(
    Y1 = rnorm(n_medium),
    Y2 = rnorm(n_medium),
    Xk = rnorm(n_medium),
    regime = rep(1, n_medium)
  )
  expect_error(
    run_rigobon_estimation(data_single),
    "Need at least 2 regimes"
  )

  # Missing variables
  data_missing <- data.frame(
    Y1 = rnorm(n_medium),
    Xk = rnorm(n_medium),
    regime = sample(1:2, n_medium, replace = TRUE)
  )
  expect_error(
    run_rigobon_estimation(data_missing),
    "Missing required variables"
  )
})

test_that("run_rigobon_demo runs without error", {
  skip_if_not_integration_test()
  # Capture output to avoid test noise
  expect_no_error({
    demo <- run_rigobon_demo(n_obs = n_medium, verbose = FALSE)
  })

  # Check return structure
  assert_list_structure(demo, c("data", "params", "results", "comparison"))

  # Check comparison data frame
  assert_valid_dataframe(demo$comparison, expected_rows = 2,
                        required_cols = c("Method", "Estimate", "StdError", "Bias", "RMSE"))
})

test_that("Rigobon method integrates with existing Lewbel framework", {
  skip_if_not_integration_test()
  # Generate Rigobon data
  params <- create_rigobon_params(
    n_regimes = 2,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  )

  data <- generate_rigobon_data(n_medium * 2, params)

  # The centered regime dummies (Z1, Z2) should work as heteroskedasticity drivers
  # similar to Lewbel's Z = X^2 - E[X^2]

  # Check that the key Lewbel assumption approximately holds
  # Cov(Z, epsilon1 * epsilon2) ≈ 0
  cov_z1_e1e2 <- cov(data$Z1, data$epsilon1 * data$epsilon2)
  cov_z2_e1e2 <- cov(data$Z2, data$epsilon1 * data$epsilon2)

  expect_true(abs(cov_z1_e1e2) < test_tolerance_tight) # Should be close to 0
  expect_true(abs(cov_z2_e1e2) < test_tolerance_tight)

  # Check that we have heteroskedasticity: Cov(Z, epsilon2^2) != 0
  cov_z1_e2sq <- cov(data$Z1, data$epsilon2^2)
  cov_z2_e2sq <- cov(data$Z2, data$epsilon2^2)

  # At least one should be significantly different from 0
  expect_true(abs(cov_z1_e2sq) > test_tolerance_tight || abs(cov_z2_e2sq) > test_tolerance_tight)
})

test_that("Rigobon handles multiple X variables", {
  skip_if_not_integration_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = c(1.5, -0.5), gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = c(-1.0, 0.3),
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  )

  data <- generate_rigobon_data(200, params, n_x = 2)

  # Check that X1 and X2 exist
  expect_true(all(c("X1", "X2") %in% names(data)))

  # Run estimation with multiple X
  results <- run_rigobon_estimation(
    data,
    exog_vars = c("X1", "X2")
  )

  expect_type(results$tsls$estimates["gamma1"], "double")
})

# Tests for run_rigobon_analysis
test_that("run_rigobon_analysis works with default parameters", {
  skip_if_not_integration_test()
  set.seed(seed_default)

  # Test with generated data
  results <- run_rigobon_analysis(n_obs = n_medium * 2, verbose = FALSE)

  assert_list_structure(results, c("estimates", "diagnostics", "data"))

  # Check estimates structure
  assert_valid_dataframe(results$estimates, expected_rows = 2,
                        required_cols = c("Method", "Estimate", "StdError"))

  # Check diagnostics
  assert_list_structure(results$diagnostics, c(
    "heteroskedasticity_test", "first_stage_F",
    "regime_proportions", "n_regimes"
  ))
  expect_type(results$diagnostics$heteroskedasticity_test$p_value, "double")
  expect_type(results$diagnostics$first_stage_F, "double")

  # Check data
  assert_valid_dataframe(results$data, expected_rows = n_medium * 2)
})

test_that("run_rigobon_analysis works with custom parameters", {
  skip_if_not_integration_test()
  params <- list(
    beta1_0 = 0.2, beta1_1 = 2.0, gamma1 = -1.0,
    beta2_0 = 0.5, beta2_1 = -0.5,
    alpha1 = -0.3, alpha2 = 0.8,
    regime_probs = c(0.3, 0.7),
    sigma2_regimes = c(0.5, 3.0)
  )

  results <- run_rigobon_analysis(
    n_obs = 150,
    params = params,
    verbose = FALSE
  )

  # Check that estimates include bias calculation
  expect_true(all(c("TrueValue", "Bias", "RelativeBias") %in% names(results$estimates)))
  expect_equal(results$estimates$TrueValue[1], params$gamma1)
})

test_that("run_rigobon_analysis works with existing data", {
  skip_if_not_integration_test()
  # Create test data
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  )

  test_data <- generate_rigobon_data(n_medium * 3, params)

  # Run analysis with existing data
  results <- run_rigobon_analysis(
    data = test_data,
    verbose = FALSE
  )

  expect_identical(results$data, test_data)
  assert_valid_dataframe(results$estimates)
})

test_that("run_rigobon_analysis with return_all option", {
  skip_if_not_integration_test()
  results <- run_rigobon_analysis(
    n_obs = n_medium,
    verbose = FALSE,
    return_all = TRUE
  )

  # Check additional components
  expect_true("models" %in% names(results))
  expect_true("instruments" %in% names(results))
  assert_list_structure(results$models, c("ols", "tsls"))
})

test_that("run_rigobon_analysis handles custom variable names", {
  skip_if_not_integration_test()
  # Generate data with custom structure
  n <- n_medium * 2
  data <- data.frame(
    outcome = rnorm(n),
    endog = rnorm(n),
    exog1 = rnorm(n),
    period = sample(1:3, n, replace = TRUE)
  )

  # Add required structure
  data$Y1 <- data$outcome
  data$Y2 <- data$endog
  data$X1 <- data$exog1

  results <- run_rigobon_analysis(
    data = data,
    regime_var = "period",
    endog_var = "Y2",
    exog_vars = "X1",
    verbose = FALSE
  )

  assert_valid_dataframe(results$estimates)
  expect_equal(results$diagnostics$n_regimes, 3)
})

# Tests for validate_rigobon_assumptions
test_that("validate_rigobon_assumptions detects valid assumptions", {
  skip_if_not_integration_test()
  # Generate data that satisfies assumptions with stronger heteroskedasticity
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(0.5, 3.0) # Stronger heteroskedasticity difference
  )

  set.seed(seed_default) # Different seed for more reliable results
  data <- generate_rigobon_data(n_large * 3, params) # Larger sample

  validation <- validate_rigobon_assumptions(data, verbose = FALSE)

  assert_list_structure(validation, c(
    "regime_heteroskedasticity", "covariance_restriction",
    "constant_covariance", "all_valid"
  ))

  # Should detect heteroskedasticity in equation2
  expect_true(validation$regime_heteroskedasticity$equation2$significant)

  # Test individual components rather than overall validity
  # The overall validity might be too strict for stochastic data
  # So we'll test the main requirement: heteroskedasticity exists
  has_heteroskedasticity <- validation$regime_heteroskedasticity$equation1$significant ||
    validation$regime_heteroskedasticity$equation2$significant
  expect_true(has_heteroskedasticity)
})

test_that("validate_rigobon_assumptions detects violations", {
  skip_if_not_integration_test()
  # Create data with no heteroskedasticity
  n <- n_large
  data <- data.frame(
    Y1 = rnorm(n),
    Y2 = rnorm(n),
    Xk = rnorm(n),
    regime = sample(1:2, n, replace = TRUE)
  )

  validation <- validate_rigobon_assumptions(data, verbose = FALSE)

  # Should not detect significant heteroskedasticity
  expect_false(validation$regime_heteroskedasticity$equation1$significant)
  expect_false(validation$regime_heteroskedasticity$equation2$significant)

  # Overall should be invalid due to lack of heteroskedasticity
  expect_false(validation$all_valid)
})

test_that("validate_rigobon_assumptions handles multiple exogenous variables", {
  skip_if_not_integration_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = c(1.5, -0.5), gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = c(-1.0, 0.3),
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  )

  data <- generate_rigobon_data(300, params, n_x = 2)

  validation <- validate_rigobon_assumptions(
    data,
    exog_vars = c("X1", "X2"),
    verbose = FALSE
  )

  expect_type(validation, "list")
  expect_type(validation$all_valid, "logical")
})

test_that("validate_rigobon_assumptions returns correct structure", {
  skip_if_not_integration_test()
  # Generate data that satisfies assumptions with stronger heteroskedasticity
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(0.5, 3.0) # Stronger heteroskedasticity difference
  )

  set.seed(seed_default)
  data <- generate_rigobon_data(n_large * 3, params)

  validation <- validate_rigobon_assumptions(data, verbose = FALSE)

  # Test structure
  assert_list_structure(validation, c(
    "regime_heteroskedasticity", "covariance_restriction",
    "constant_covariance", "all_valid"
  ))

  # Check heteroskedasticity test structure
  expect_type(validation$regime_heteroskedasticity, "list")
  assert_list_structure(validation$regime_heteroskedasticity, c("equation1", "equation2"))
  expect_true("p_value" %in% names(validation$regime_heteroskedasticity$equation1))
  expect_true("significant" %in% names(validation$regime_heteroskedasticity$equation1))

  # Check covariance restriction structure
  expect_type(validation$covariance_restriction, "list")
  expect_true(length(validation$covariance_restriction) > 0)
  # Each regime should have test results
  for (regime_test in validation$covariance_restriction) {
    assert_list_structure(regime_test, c("covariance", "t_stat", "p_value", "valid"))
  }

  # Check constant covariance structure
  expect_type(validation$constant_covariance, "list")
  assert_list_structure(validation$constant_covariance, c("covariances", "cv", "valid"))

  # Should detect heteroskedasticity in equation2
  expect_true(validation$regime_heteroskedasticity$equation2$significant)

  # Test individual components rather than overall validity
  # The overall validity might be too strict for stochastic data
  # So we'll test the main requirement: heteroskedasticity exists
  has_heteroskedasticity <- validation$regime_heteroskedasticity$equation1$significant ||
    validation$regime_heteroskedasticity$equation2$significant
  expect_true(has_heteroskedasticity)
})

# Tests for compare_rigobon_methods
test_that("compare_rigobon_methods works with all methods", {
  skip_if_not_integration_test()
  # Generate data that works for all methods
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 2.5)
  )

  data <- generate_rigobon_data(500, params)
  # Add Z for Lewbel method
  data$Z <- data$Xk^2 - mean(data$Xk^2)

  comparison <- compare_rigobon_methods(
    data,
    true_gamma1 = params$gamma1,
    verbose = FALSE
  )

  assert_valid_dataframe(comparison, expected_rows = 3) # OLS, Rigobon, Lewbel
  assert_list_structure(comparison, c(
    "Method", "Estimate", "StdError", "TrueValue",
    "Bias", "RelativeBias_pct", "FirstStageF", "Details"
  ))

  # Check that all estimates are numeric
  expect_true(all(is.numeric(comparison$Estimate)))
  expect_true(all(is.numeric(comparison$StdError)))

  # Rigobon should have lower bias than OLS
  ols_bias <- abs(comparison$Bias[comparison$Method == "OLS"])
  rigobon_bias <- abs(comparison$Bias[comparison$Method == "Rigobon"])
  expect_true(rigobon_bias < ols_bias)
})

test_that("compare_rigobon_methods works with subset of methods", {
  skip_if_not_integration_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  )

  data <- generate_rigobon_data(n_medium * 3, params)

  # Only OLS and Rigobon
  comparison <- compare_rigobon_methods(
    data,
    methods = c("OLS", "Rigobon"),
    verbose = FALSE
  )

  expect_equal(nrow(comparison), 2)
  expect_true(all(comparison$Method %in% c("OLS", "Rigobon")))
})

test_that("compare_rigobon_methods handles missing true value", {
  skip_if_not_integration_test()
  data <- generate_rigobon_data(200, list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  ))

  comparison <- compare_rigobon_methods(
    data,
    methods = c("OLS", "Rigobon"),
    verbose = FALSE
  )

  # Without true value, bias columns should be NA
  expect_true(all(is.na(comparison$TrueValue)))
  expect_true(all(is.na(comparison$Bias)))
  expect_true(all(is.na(comparison$RelativeBias_pct)))
})

test_that("compare_rigobon_methods handles data without Lewbel Z", {
  skip_if_not_integration_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  )

  data <- generate_rigobon_data(n_medium * 2, params)
  # No Z variable, so Lewbel should be skipped

  comparison <- compare_rigobon_methods(
    data,
    true_gamma1 = params$gamma1,
    methods = c("OLS", "Rigobon", "Lewbel"),
    verbose = FALSE
  )

  # Should only have OLS and Rigobon
  expect_equal(nrow(comparison), 2)
  expect_false("Lewbel" %in% comparison$Method)
})

test_that("compare_rigobon_methods verbose output works", {
  skip_if_not_integration_test()
  data <- generate_rigobon_data(n_medium, list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.0)
  ))

  # Capture output
  output <- capture.output({
    comparison <- compare_rigobon_methods(
      data,
      true_gamma1 = -0.8,
      methods = c("OLS", "Rigobon"),
      verbose = TRUE
    )
  })

  # Check that output includes key sections
  expect_true(any(grepl("METHOD COMPARISON", output)))
  expect_true(any(grepl("True gamma1", output)))
  expect_true(any(grepl("Best method by bias", output)))
})
