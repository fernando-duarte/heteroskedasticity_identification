# Tests for weak heteroskedasticity scenarios from vignette Section 5
# Tests summary statistics and findings from vignette Section 6

library(hetid)

# Skip all tests if AER is not available
if (!requireNamespace("AER", quietly = TRUE)) {
  skip("AER package not available")
}
library(AER)

test_that("weak heteroskedasticity testing matches vignette Section 5", {
  skip_if_not_comprehensive_test()
  skip_on_cran()

  # Generate data with very weak heteroskedasticity as in vignette
  set.seed(123)
  n <- 1000
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  # Test with weak heteroskedasticity
  params_weak <- params
  params_weak$delta_het <- 0.1 # Very weak
  data_weak <- generate_lewbel_data(n, params_weak)

  test_data_weak <- data.frame(
    y = data_weak$Y1,
    X1 = data_weak$Xk,
    P = data_weak$Y2,
    Z = data_weak$Z
  )

  # Construct Lewbel IV
  e2_hat_weak <- residuals(lm(P ~ X1, data = test_data_weak))
  lewbel_iv_weak <- (test_data_weak$Z - mean(test_data_weak$Z)) * e2_hat_weak
  lewbel_iv_weak <- lewbel_iv_weak - mean(lewbel_iv_weak)
  test_data_weak$lewbel_iv <- lewbel_iv_weak

  # Run both methods
  manual_weak <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = test_data_weak)

  expect_s3_class(manual_weak, "ivreg")
  expect_true(is.finite(coef(manual_weak)["P"]))

  se_manual_weak <- sqrt(diag(vcov(manual_weak)))["P"]
  expect_true(is.finite(se_manual_weak))
  expect_true(se_manual_weak > 0)

  # Test with REndo if available
  if (requireNamespace("REndo", quietly = TRUE)) {
    rendo_weak <- tryCatch(
      {
        suppressWarnings(
          REndo::hetErrorsIV(y ~ X1 + P | P | IIV(X1), data = test_data_weak)
        )
      },
      error = function(e) NULL
    )

    if (!is.null(rendo_weak)) {
      se_rendo_weak <- sqrt(diag(vcov(rendo_weak)))["P"]
      expect_true(is.finite(se_rendo_weak))
      expect_true(se_rendo_weak > 0)

      # Compare standard errors
      se_ratio <- se_rendo_weak / se_manual_weak
      expect_true(is.finite(se_ratio))
      expect_true(se_ratio > 0)

      # Document the relationship (could be larger or smaller)
      expect_true(TRUE, label = paste("SE ratio with weak het:", round(se_ratio, 2)))
    }
  }

  # Compare with strong heteroskedasticity
  params_strong <- params
  params_strong$delta_het <- 2.0 # Strong
  data_strong <- generate_lewbel_data(n, params_strong)

  test_data_strong <- data.frame(
    y = data_strong$Y1,
    X1 = data_strong$Xk,
    P = data_strong$Y2,
    Z = data_strong$Z
  )

  e2_hat_strong <- residuals(lm(P ~ X1, data = test_data_strong))
  lewbel_iv_strong <- (test_data_strong$Z - mean(test_data_strong$Z)) * e2_hat_strong
  lewbel_iv_strong <- lewbel_iv_strong - mean(lewbel_iv_strong)

  manual_strong <- ivreg(y ~ X1 + P | X1 + lewbel_iv_strong, data = test_data_strong)
  se_manual_strong <- sqrt(diag(vcov(manual_strong)))["P"]

  # With new DGP for n_x=1, delta_het is ignored and heteroskedasticity is fixed
  # Both weak and strong will have same heteroskedasticity pattern
  # Just verify both produce valid standard errors
  expect_true(se_manual_strong > 0,
    label = "Strong het SE should be positive"
  )
  expect_true(se_manual_weak > 0,
    label = "Weak het SE should be positive"
  )

  # SEs should be similar since heteroskedasticity is the same
  se_ratio <- se_manual_strong / se_manual_weak
  expect_true(abs(se_ratio - 1) < 0.2,
    label = paste("SE ratio should be close to 1, got", round(se_ratio, 3))
  )
})

test_that("summary statistics generation matches vignette Section 6", {
  skip_if_not_comprehensive_test()
  skip_on_cran()

  # Generate test data for summary
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

  # Run standard Lewbel
  e2_hat <- residuals(lm(P ~ X1, data = test_data))
  lewbel_iv <- (test_data$Z - mean(test_data$Z)) * e2_hat
  lewbel_iv <- lewbel_iv - mean(lewbel_iv)
  manual_model <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = test_data)

  coef_manual <- coef(manual_model)["P"]
  se_manual <- sqrt(diag(vcov(manual_model)))["P"]

  # Test overidentified model if possible
  overid_model <- tryCatch(
    {
      ivreg(y ~ X1 + P | X1 + P + lewbel_iv, data = test_data)
    },
    error = function(e) NULL
  )

  # Weak heteroskedasticity test
  params_weak <- params
  params_weak$delta_het <- 0.1
  data_weak <- generate_lewbel_data(n, params_weak)
  test_data_weak <- data.frame(
    y = data_weak$Y1,
    X1 = data_weak$Xk,
    P = data_weak$Y2,
    Z = data_weak$Z
  )

  e2_hat_weak <- residuals(lm(P ~ X1, data = test_data_weak))
  lewbel_iv_weak <- (test_data_weak$Z - mean(test_data_weak$Z)) * e2_hat_weak
  lewbel_iv_weak <- lewbel_iv_weak - mean(lewbel_iv_weak)
  manual_weak <- ivreg(y ~ X1 + P | X1 + lewbel_iv_weak, data = test_data_weak)

  # Create comprehensive summary table as in vignette
  summary_methods <- c("True value", "Standard Lewbel")
  summary_coefficients <- c(params$gamma1, coef_manual)
  summary_std_errors <- c(NA, se_manual)

  if (!is.null(overid_model)) {
    summary_methods <- c(summary_methods, "Overidentified (manual)")
    summary_coefficients <- c(summary_coefficients, coef(overid_model)["P"])
    summary_std_errors <- c(summary_std_errors, sqrt(diag(vcov(overid_model)))["P"])
  }

  summary_methods <- c(summary_methods, "Weak het - Lewbel")
  summary_coefficients <- c(summary_coefficients, coef(manual_weak)["P"])
  summary_std_errors <- c(summary_std_errors, sqrt(diag(vcov(manual_weak)))["P"])

  # Test with REndo if available
  if (requireNamespace("REndo", quietly = TRUE)) {
    rendo_model <- tryCatch(
      {
        suppressMessages(REndo::hetErrorsIV(y ~ X1 + P | P | IIV(X1), data = test_data))
      },
      error = function(e) NULL
    )

    rendo_weak <- tryCatch(
      {
        suppressWarnings(suppressMessages(
          REndo::hetErrorsIV(y ~ X1 + P | P | IIV(X1), data = test_data_weak)
        ))
      },
      error = function(e) NULL
    )

    if (!is.null(rendo_model)) {
      summary_methods <- c(summary_methods, "REndo hetErrorsIV")
      summary_coefficients <- c(summary_coefficients, coef(rendo_model)["P"])
      summary_std_errors <- c(summary_std_errors, sqrt(diag(vcov(rendo_model)))["P"])
    }

    if (!is.null(rendo_weak)) {
      summary_methods <- c(summary_methods, "Weak het - REndo")
      summary_coefficients <- c(summary_coefficients, coef(rendo_weak)["P"])
      summary_std_errors <- c(summary_std_errors, sqrt(diag(vcov(rendo_weak)))["P"])
    }
  }

  # Create summary table
  summary_table <- data.frame(
    Method = summary_methods,
    Coefficient = summary_coefficients,
    Std_Error = summary_std_errors
  )

  # Verify summary table structure
  expect_s3_class(summary_table, "data.frame")
  expect_true("Method" %in% names(summary_table))
  expect_true("Coefficient" %in% names(summary_table))
  expect_true("Std_Error" %in% names(summary_table))
  expect_true(nrow(summary_table) >= 3)

  # Verify true value is included
  true_value_row <- summary_table[summary_table$Method == "True value", ]
  expect_equal(nrow(true_value_row), 1)
  expect_equal(true_value_row$Coefficient, params$gamma1)
  expect_true(is.na(true_value_row$Std_Error))

  # Verify standard Lewbel results
  lewbel_row <- summary_table[summary_table$Method == "Standard Lewbel", ]
  expect_equal(nrow(lewbel_row), 1)
  expect_true(is.finite(lewbel_row$Coefficient))
  expect_true(is.finite(lewbel_row$Std_Error))
  expect_true(lewbel_row$Std_Error > 0)

  # Verify coefficient is reasonable
  expect_true(abs(lewbel_row$Coefficient - params$gamma1) < 0.2)
})
