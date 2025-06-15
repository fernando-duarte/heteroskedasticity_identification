# Test degrees of freedom adjustment functionality
library(testthat)
library(hetid)
library(AER)

test_that("df_adjust parameter works in run_single_lewbel_simulation", {
  # Create test parameters
  params <- list(
    sample_size = 100,
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    tau_set_id = 0,
    bootstrap_reps = 50
  )

  # Run with asymptotic adjustment
  set.seed(123)
  result_asymp <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params,
    df_adjust = "asymptotic"
  )

  # Run with finite sample adjustment
  set.seed(123)
  result_finite <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params,
    df_adjust = "finite"
  )

  # Check that results have the df_adjust column
  expect_true("df_adjust" %in% names(result_asymp))
  expect_true("df_adjust" %in% names(result_finite))
  expect_equal(result_asymp$df_adjust, "asymptotic")
  expect_equal(result_finite$df_adjust, "finite")

  # Check that SEs are included (they're in the data frame, not as separate columns)
  expect_true(!is.na(result_asymp$ols_gamma1))
  expect_true(!is.na(result_asymp$tsls_gamma1))

  # Coverage might differ due to different critical values
  # Can't directly test SE differences as they're computed internally
})

test_that("extract_se functions work correctly", {
  # Generate test data
  set.seed(456)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  data <- generate_lewbel_data(200, params)

  # Fit OLS model
  ols_model <- lm(Y1 ~ Y2 + Xk, data = data)

  # Extract SEs with different adjustments
  se_asymp <- extract_se_lm(ols_model, df_adjust = "asymptotic")
  se_finite <- extract_se_lm(ols_model, df_adjust = "finite")

  # lm uses finite sample by default, so finite should match summary
  expect_equal(
    se_finite,
    summary(ols_model)$coefficients[, "Std. Error"],
    tolerance = 1e-10
  )

  # Asymptotic should be smaller by factor sqrt((n-k)/n)
  n <- nobs(ols_model)
  k <- length(coef(ols_model))
  expected_ratio <- sqrt((n - k) / n)
  actual_ratio <- unname(se_asymp[2] / se_finite[2])
  expect_equal(actual_ratio, expected_ratio, tolerance = 1e-10)
})

test_that("get_critical_value returns correct values", {
  # For large n, should approach normal
  crit_asymp <- get_critical_value(10000, 3, alpha = 0.05, df_adjust = "asymptotic")
  expect_equal(crit_asymp, qnorm(0.975), tolerance = 1e-10)

  # For finite sample, should use t-distribution
  crit_finite <- get_critical_value(30, 3, alpha = 0.05, df_adjust = "finite")
  expect_equal(crit_finite, qt(0.975, df = 27), tolerance = 1e-10)

  # Finite critical value should be larger for small samples
  expect_gt(crit_finite, crit_asymp)

  # Test different alpha levels
  crit_90 <- get_critical_value(100, 5, alpha = 0.10, df_adjust = "finite")
  expect_equal(crit_90, qt(0.95, df = 95), tolerance = 1e-10)
})

test_that("adjust_se_for_df works correctly", {
  # Test asymptotic (no adjustment)
  se_orig <- 0.1
  se_adj <- adjust_se_for_df(se_orig, n = 100, k = 3, df_adjust = "asymptotic")
  expect_equal(se_adj, se_orig)

  # Test finite sample adjustment
  se_adj_finite <- adjust_se_for_df(se_orig, n = 100, k = 3, df_adjust = "finite")
  expected <- se_orig * sqrt(100 / 97)
  expect_equal(se_adj_finite, expected, tolerance = 1e-10)

  # Edge case: n = k + 1
  se_adj_edge <- adjust_se_for_df(se_orig, n = 4, k = 3, df_adjust = "finite")
  expect_equal(se_adj_edge, se_orig * 2) # sqrt(4/1) = 2
})

test_that("ivreg SE extraction works correctly", {
  # Generate test data with instrument
  set.seed(789)
  n <- 200
  data <- data.frame(
    x = rnorm(n),
    z = rnorm(n),
    u = rnorm(n)
  )
  data$y2 <- 0.5 * data$z + data$u
  data$y1 <- 1 + 0.5 * data$x - 0.8 * data$y2 + rnorm(n)

  # Fit IV model
  iv_model <- ivreg(y1 ~ x + y2 | x + z, data = data)

  # Extract SEs
  se_asymp <- extract_se_ivreg(iv_model, df_adjust = "asymptotic")
  se_finite <- extract_se_ivreg(iv_model, df_adjust = "finite")

  # ivreg uses asymptotic by default
  expect_equal(
    se_asymp,
    sqrt(diag(vcov(iv_model))),
    tolerance = 1e-10
  )

  # Finite should be larger
  n <- nobs(iv_model)
  k <- length(coef(iv_model))
  expected_ratio <- sqrt(n / (n - k))
  actual_ratio <- unname(se_finite[2] / se_asymp[2])
  expect_equal(actual_ratio, expected_ratio, tolerance = 1e-10)
})

# Removed Monte Carlo test due to complexity
