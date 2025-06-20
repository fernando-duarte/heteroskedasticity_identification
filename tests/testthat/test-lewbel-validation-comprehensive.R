# Core validation tests for hetid package
library(hetid)

# Skip all tests if AER is not available
if (!requireNamespace("AER", quietly = TRUE)) {
  skip("AER package not available")
}
library(AER)

test_that("hetid methods work with pre-generated lewbel_sim data", {
  skip_if_not_comprehensive_test()
  withr::with_seed(1, {
    # Load built-in test data
    data("lewbel_sim", package = "hetid", envir = environment())

    # lewbel_sim has columns: id, y, P, X1, X2
    # where P is endogenous, X1 and X2 are exogenous

    # Generate residuals from first stage
    first_stage <- lm(P ~ X1 + X2, data = lewbel_sim)
    e2_hat <- residuals(first_stage)

    # Construct Lewbel instrument using X2 for heteroskedasticity
    z2_val <- lewbel_sim$X2^2 - mean(lewbel_sim$X2^2)
    lewbel_iv <- (z2_val - mean(z2_val)) * e2_hat
    # Ensure instrument is exactly mean-zero
    lewbel_iv <- lewbel_iv - mean(lewbel_iv)
    lewbel_sim$lewbel_iv <- lewbel_iv

    # Run 2SLS
    tsls_model <- ivreg(
      y ~ X1 + X2 + P | X1 + X2 + lewbel_iv,
      data = lewbel_sim
    )

    # Extract coefficient for P
    coef_p_val <- coef(tsls_model)["P"]

    # Test that coefficient is reasonable (true value is -1.0)
    expect_true(coef_p_val < -0.8 && coef_p_val > -1.2)

    # Test that instrument is mean-zero
    expect_equal(mean(lewbel_iv), 0, tolerance = 1e-10)

    # Test standard methods work
    expect_s3_class(tsls_model, "ivreg")
    expect_no_error(summary(tsls_model))
    expect_equal(length(fitted(tsls_model)), nrow(lewbel_sim))
  })
})

test_that("run_single_lewbel_simulation generates valid results", {
  skip_if_not_comprehensive_test()
  # Test with internally generated data
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    sample_size = 500,
    tau_set_id = 0,
    bootstrap_reps = 50
  )

  result <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params,
    endog_var = "Y2",
    exog_vars = "Xk",
    return_models = TRUE
  )

  # Check that results were generated
  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("models" %in% names(result))

  # Check results data frame
  expect_equal(nrow(result$results), 1)
  expect_true("tsls_gamma1" %in% names(result$results))

  # If model is not NULL, check it
  if (!is.null(result$models$tsls_model)) {
    expect_s3_class(result$models$tsls_model, "ivreg")

    # Check that estimate is reasonable
    coef_endog <- coef(result$models$tsls_model)["Y2"]
    expect_true(abs(coef_endog - params$gamma1) < 0.5)
  }
})

test_that("Multiple X variables work correctly", {
  skip_if_not_comprehensive_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = c(1.5, 3.0), gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = c(-1.0, 0.7),
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    sample_size = 500,
    n_x = 2,
    tau_set_id = 0,
    bootstrap_reps = 50
  )

  result <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params,
    endog_var = "Y2",
    exog_vars = c("X1", "X2"),
    return_models = TRUE
  )

  # Check that results were generated
  expect_type(result, "list")

  # If successful, check the model
  if (!is.null(result$models$tsls_model)) {
    # Model should include both X variables
    model_vars <- names(coef(result$models$tsls_model))
    expect_true("X1" %in% model_vars)
    expect_true("X2" %in% model_vars)
    expect_true("Y2" %in% model_vars)
  }
})

test_that("Instrument order invariance holds", {
  skip_if_not_comprehensive_test()
  # Generate consistent data
  set.seed(42)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  n <- 200
  data1 <- generate_lewbel_data(n, params)

  # Create instrument with original order
  first_stage1 <- lm(Y2 ~ Xk, data = data1)
  e2_hat1 <- residuals(first_stage1)
  z1_val <- data1$Z
  iv1 <- (z1_val - mean(z1_val)) * e2_hat1

  # Shuffle and recreate
  shuffled_idx <- sample(n)
  data2 <- data1[shuffled_idx, ]

  first_stage2 <- lm(Y2 ~ Xk, data = data2)
  e2_hat2 <- residuals(first_stage2)
  z2_val <- data2$Z
  iv2 <- (z2_val - mean(z2_val)) * e2_hat2

  # Reorder back
  reorder_idx <- order(shuffled_idx)
  iv2_reordered <- iv2[reorder_idx]

  # Should be identical
  expect_equal(iv1, iv2_reordered, tolerance = 1e-10)
})

test_that("generate_hetid_test_data creates valid data", {
  skip_if_not_comprehensive_test()
  # Test the new helper function
  data <- generate_hetid_test_data(n = 500, seed = 123)

  # Check structure
  expect_true(is.data.frame(data))
  expect_equal(nrow(data), 500)
  expect_true(all(c("y", "P", "X1", "Z", "lewbel_iv") %in% names(data)))

  # Check instrument properties
  expect_equal(mean(data$lewbel_iv), 0, tolerance = 1e-10)
  expect_true(var(data$lewbel_iv) > 0)

  # Check that we can run 2SLS with this data
  tsls_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = data
  )

  expect_s3_class(tsls_model, "ivreg")
  expect_true(abs(coef(tsls_model)["P"] - (-0.8)) < 0.5)
})
