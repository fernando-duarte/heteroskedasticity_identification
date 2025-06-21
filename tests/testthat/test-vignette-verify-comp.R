# Complete verification tests matching the package comparison vignette
# Tests the complete verify_lewbel_implementations() function and all scenarios

library(hetid)

# Skip all tests if AER is not available
if (!requireNamespace("AER", quietly = TRUE)) {
  skip("AER package not available")
}
library(AER)

test_that("complete verification function works end-to-end", {
  skip_if_not_comprehensive_test()
  skip_on_cran()

  # Test the complete verification function from the vignette
  verify_lewbel_implementations <- function(n = 1000, seed = 12345) {
    set.seed(seed)

    # Generate data
    params <- list(
      beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
      beta2_0 = 1.0, beta2_1 = -1.0,
      alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.5
    )
    data <- generate_lewbel_data(n, params)
    test_data <- data.frame(y = data$Y1, x = data$Xk, p = data$Y2, z = data$Z)

    # Standard Lewbel (hetid/Stata)
    e2_hat <- residuals(lm(p ~ x, data = test_data))
    iv_z <- (test_data$z - mean(test_data$z)) * e2_hat
    iv_z <- iv_z - mean(iv_z)
    model_z <- ivreg(y ~ x + p | x + iv_z, data = test_data)

    # Alternative with X (REndo approach)
    iv_x <- (test_data$x - mean(test_data$x)) * e2_hat
    iv_x <- iv_x - mean(iv_x)
    model_x <- ivreg(y ~ x + p | x + iv_x, data = test_data)

    # REndo (if available)
    if (requireNamespace("REndo", quietly = TRUE)) {
      rendo <- REndo::hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data)

      results <- data.frame(
        Method = c(
          "hetid/Stata (Z=X²-E[X²])", "Manual with Z=X",
          "REndo hetErrorsIV"
        ),
        Coefficient = c(
          coef(model_z)["p"], coef(model_x)["p"],
          coef(rendo)["p"]
        ),
        SE = c(
          sqrt(diag(vcov(model_z)))["p"],
          sqrt(diag(vcov(model_x)))["p"],
          sqrt(diag(vcov(rendo)))["p"]
        )
      )
    } else {
      results <- data.frame(
        Method = c("hetid/Stata (Z=X²-E[X²])", "Manual with Z=X"),
        Coefficient = c(coef(model_z)["p"], coef(model_x)["p"]),
        SE = c(
          sqrt(diag(vcov(model_z)))["p"],
          sqrt(diag(vcov(model_x)))["p"]
        )
      )
    }

    results
  }

  # Run the verification function
  results <- verify_lewbel_implementations(n = 1000, seed = 12345)

  # Verify the function produces expected structure
  expect_s3_class(results, "data.frame")
  expect_true("Method" %in% names(results))
  expect_true("Coefficient" %in% names(results))
  expect_true("SE" %in% names(results))
  expect_true(nrow(results) >= 2)

  # Verify the Z-based approach produces expected results
  z_result <- results[results$Method == "hetid/Stata (Z=X²-E[X²])", ]
  expect_equal(nrow(z_result), 1)
  expect_true(is.finite(z_result$Coefficient))
  expect_true(is.finite(z_result$SE))
  expect_true(z_result$SE > 0)

  # Verify coefficient is reasonable (close to true value -0.8)
  expect_true(abs(z_result$Coefficient - (-0.8)) < 0.1)
})

test_that("vignette Method 1 exact replication works", {
  skip_if_not_comprehensive_test()
  skip_on_cran()

  # Generate test data exactly as in vignette Method 1
  set.seed(12345)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  data <- generate_lewbel_data(1000, params)

  # Standard Lewbel implementation
  e2_hat <- residuals(lm(Y2 ~ Xk, data = data))
  lewbel_iv <- (data$Z - mean(data$Z)) * e2_hat
  lewbel_iv <- lewbel_iv - mean(lewbel_iv)
  model <- ivreg(Y1 ~ Xk + Y2 | Xk + lewbel_iv, data = data)

  # Verify results match vignette expectations
  coef_y2 <- coef(model)["Y2"]
  se_y2 <- sqrt(diag(vcov(model)))["Y2"]

  expect_true(is.finite(coef_y2))
  expect_true(is.finite(se_y2))
  expect_true(se_y2 > 0)

  # Should be close to true value
  expect_true(abs(coef_y2 - params$gamma1) < 0.1)

  # Instrument should be mean-zero
  expect_equal(mean(lewbel_iv), 0, tolerance = 1e-10)
  expect_true(var(lewbel_iv) > 0)
})

test_that("vignette Method 3 direct comparison works", {
  skip_if_not_comprehensive_test()
  skip_on_cran()

  # Prove REndo uses X instead of Z as in vignette Method 3
  set.seed(42)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  data <- generate_lewbel_data(1000, params)
  test_data <- data.frame(y = data$Y1, x = data$Xk, p = data$Y2, z = data$Z)

  # Standard Lewbel with Z
  e2_hat <- residuals(lm(p ~ x, data = test_data))
  iv_z <- (test_data$z - mean(test_data$z)) * e2_hat
  model_z <- ivreg(y ~ x + p | x + iv_z, data = test_data)

  # Alternative with X (what REndo does)
  iv_x <- (test_data$x - mean(test_data$x)) * e2_hat
  model_x <- ivreg(y ~ x + p | x + iv_x, data = test_data)

  # Verify both approaches work
  expect_s3_class(model_z, "ivreg")
  expect_s3_class(model_x, "ivreg")
  expect_true(is.finite(coef(model_z)["p"]))
  expect_true(is.finite(coef(model_x)["p"]))

  # They should produce different results
  expect_false(
    abs(coef(model_z)["p"] - coef(model_x)["p"]) < 1e-8
  )

  # REndo comparison if available
  if (requireNamespace("REndo", quietly = TRUE)) {
    rendo <- suppressMessages(
      REndo::hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data)
    )

    # REndo should match the X-based approach more closely than Z-based
    diff_x <- abs(coef(rendo)["p"] - coef(model_x)["p"])
    diff_z <- abs(coef(rendo)["p"] - coef(model_z)["p"])

    # This relationship should hold in most cases
    expect_true(diff_x <= diff_z || diff_x < 0.01)
  }
})
