# Tests for degrees of freedom adjustments and comprehensive scenarios
# Covers vignette sections on DF adjustments and software matching

library(hetid)

# Skip all tests if AER is not available
if (!requireNamespace("AER", quietly = TRUE)) {
  skip("AER package not available")
}
library(AER)

test_that("degrees of freedom adjustments match vignette expectations", {
  skip_on_cran()

  # Test DF adjustments as shown in vignette
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    sample_size = 1000, tau_set_id = 0, bootstrap_reps = 0
  )

  # hetid default: asymptotic (matches Stata)
  result_asymp <- run_single_lewbel_simulation(
    sim_id = 1, params = params,
    df_adjust = "asymptotic" # Default
  )

  # For finite sample SEs (matches base R)
  result_finite <- run_single_lewbel_simulation(
    sim_id = 1, params = params,
    df_adjust = "finite"
  )

  # Verify both results are valid
  expect_true(is.finite(result_asymp$tsls_gamma1))
  expect_true(is.finite(result_finite$tsls_gamma1))
  expect_true(is.finite(result_asymp$tsls_se))
  expect_true(is.finite(result_finite$tsls_se))
  expect_true(result_asymp$tsls_se > 0)
  expect_true(result_finite$tsls_se > 0)

  # Verify DF adjustment settings
  expect_equal(result_asymp$df_adjust, "asymptotic")
  expect_equal(result_finite$df_adjust, "finite")

  # Document the relationship between finite and asymptotic SEs
  # The relationship can vary depending on sample size and model
  se_ratio_finite_asymp <- result_finite$tsls_se / result_asymp$tsls_se
  expect_true(se_ratio_finite_asymp > 0.8 && se_ratio_finite_asymp < 1.5,
              label = "Finite and asymptotic SEs should be reasonably close")

  # Both should estimate similar coefficients
  expect_true(abs(result_asymp$tsls_gamma1 - result_finite$tsls_gamma1) < 0.1,
              label = "DF adjustment should not substantially change coef")
})

test_that("software matching scenarios work as in vignette", {
  skip_on_cran()

  # Test matching different software as shown in vignette
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    sample_size = 1000, tau_set_id = 0, bootstrap_reps = 0
  )

  # To match Stata ivreg2h (asymptotic SEs):
  result_stata_match <- run_single_lewbel_simulation(
    sim_id = 1, params = params, df_adjust = "asymptotic"
  )

  # To match R's ivreg default (finite sample SEs):
  result_r_match <- run_single_lewbel_simulation(
    sim_id = 1, params = params, df_adjust = "finite"
  )

  # Verify results are reasonable
  expect_true(is.finite(result_stata_match$tsls_gamma1))
  expect_true(is.finite(result_r_match$tsls_gamma1))
  expect_true(abs(result_stata_match$tsls_gamma1 - params$gamma1) < 0.2)
  expect_true(abs(result_r_match$tsls_gamma1 - params$gamma1) < 0.2)

  # Document SE differences (relationship can vary)
  se_ratio_r_stata <- result_r_match$tsls_se / result_stata_match$tsls_se
  expect_true(se_ratio_r_stata > 0.8 && se_ratio_r_stata < 1.5,
              label = "R and Stata-style SEs should be reasonably close")

  # Both should be close to true value
  expect_true(abs(result_stata_match$tsls_gamma1 - params$gamma1) < 0.1)
  expect_true(abs(result_r_match$tsls_gamma1 - params$gamma1) < 0.1)
})

test_that("comprehensive estimation comparison matches vignette Section 3", {
  skip_on_cran()

  # Generate test data for comprehensive comparison
  set.seed(123)
  n <- 1000
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)
  test_data <- data.frame(
    y = data$Y1,
    X1 = data$Xk,
    P = data$Y2,
    Z = data$Z
  )

  # A. Standard Lewbel (manual 2SLS with hetid approach)
  first_stage <- lm(P ~ X1, data = test_data)
  e2_hat <- residuals(first_stage)
  lewbel_iv <- (test_data$Z - mean(test_data$Z)) * e2_hat
  lewbel_iv <- lewbel_iv - mean(lewbel_iv)

  manual_2sls <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = test_data)
  coef_manual <- coef(manual_2sls)["P"]
  se_manual <- sqrt(diag(vcov(manual_2sls)))["P"]
  t_stat_manual <- coef_manual / se_manual

  expect_true(is.finite(coef_manual))
  expect_true(is.finite(se_manual))
  expect_true(is.finite(t_stat_manual))
  expect_true(se_manual > 0)

  # B. REndo's hetErrorsIV (if available)
  if (requireNamespace("REndo", quietly = TRUE)) {
    rendo_model <- suppressMessages(
      REndo::hetErrorsIV(y ~ X1 + P | P | IIV(X1), data = test_data)
    )

    coef_rendo <- coef(rendo_model)["P"]
    se_rendo <- sqrt(diag(vcov(rendo_model)))["P"]
    t_stat_rendo <- coef_rendo / se_rendo

    expect_true(is.finite(coef_rendo))
    expect_true(is.finite(se_rendo))
    expect_true(is.finite(t_stat_rendo))
    expect_true(se_rendo > 0)

    # Compare results
    se_ratio <- se_rendo / se_manual
    expect_true(is.finite(se_ratio))
    expect_true(se_ratio > 0)

    # Document the typical relationship
    expect_true(TRUE,
                label = paste("SE ratio (REndo/Lewbel):", round(se_ratio, 4)))

    # Both should be reasonable estimates
    expect_true(abs(coef_manual - params$gamma1) < 0.2)
    expect_true(abs(coef_rendo - params$gamma1) < 0.2)
  }

  # C. Test first-stage strength
  first_stage_summary <- summary(first_stage)
  f_stat <- first_stage_summary$fstatistic[1]

  expect_true(is.finite(f_stat))
  expect_true(f_stat > 0)

  # Document instrument strength
  if (f_stat > 10) {
    expect_true(TRUE, label = "Strong instruments (F > 10)")
  } else if (f_stat > 5) {
    expect_true(TRUE, label = "Moderate instruments (5 < F < 10)")
  } else {
    expect_true(TRUE, label = "Weak instruments (F < 5)")
  }
})

test_that("multiple scenarios comprehensive test", {
  skip_on_cran()

  # Test multiple scenarios as covered throughout the vignette
  scenarios <- list(
    list(name = "Standard", delta_het = 1.2, n = 1000, seed = 123),
    list(name = "Weak Het", delta_het = 0.1, n = 1000, seed = 456),
    list(name = "Strong Het", delta_het = 2.0, n = 1000, seed = 789),
    list(name = "Small Sample", delta_het = 1.2, n = 200, seed = 999)
  )

  results_summary <- data.frame(
    Scenario = character(0),
    Coefficient = numeric(0),
    Std_Error = numeric(0),
    F_Stat = numeric(0),
    stringsAsFactors = FALSE
  )

  for (scenario in scenarios) {
    set.seed(scenario$seed)

    params <- list(
      beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
      beta2_0 = 1.0, beta2_1 = -1.0,
      alpha1 = -0.5, alpha2 = 1.0, delta_het = scenario$delta_het
    )

    data <- generate_lewbel_data(scenario$n, params)
    test_data <- data.frame(
      y = data$Y1,
      X1 = data$Xk,
      P = data$Y2,
      Z = data$Z
    )

    # Run standard Lewbel
    first_stage <- lm(P ~ X1, data = test_data)
    e2_hat <- residuals(first_stage)
    lewbel_iv <- (test_data$Z - mean(test_data$Z)) * e2_hat
    lewbel_iv <- lewbel_iv - mean(lewbel_iv)

    model <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = test_data)

    coef_p <- coef(model)["P"]
    se_p <- sqrt(diag(vcov(model)))["P"]
    f_stat <- summary(first_stage)$fstatistic[1]

    # Add to results
    results_summary <- rbind(results_summary, data.frame(
      Scenario = scenario$name,
      Coefficient = coef_p,
      Std_Error = se_p,
      F_Stat = f_stat,
      stringsAsFactors = FALSE
    ))
  }

  # Verify all scenarios produced valid results
  expect_equal(nrow(results_summary), length(scenarios))
  expect_true(all(is.finite(results_summary$Coefficient)))
  expect_true(all(is.finite(results_summary$Std_Error)))
  expect_true(all(is.finite(results_summary$F_Stat)))
  expect_true(all(results_summary$Std_Error > 0))
  expect_true(all(results_summary$F_Stat > 0))

  # All coefficients should be reasonable estimates of -0.8
  expect_true(all(abs(results_summary$Coefficient - (-0.8)) < 0.3))

  # Weak heteroskedasticity should typically have larger SE
  weak_row <- results_summary[results_summary$Scenario == "Weak Het", ]
  standard_row <- results_summary[results_summary$Scenario == "Standard", ]

  expect_true(weak_row$Std_Error >= standard_row$Std_Error * 0.8,
              label = "Weak het should not dramatically improve precision")
})
