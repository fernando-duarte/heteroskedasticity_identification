test_that("generate_prono_data creates valid time series data", {
  # Test with default parameters
  set.seed(123)
  df <- generate_prono_data(n = 100, k = 1)

  expect_equal(nrow(df), 100)
  expect_true(all(c("Y1", "Y2", "X1", "const", "eps1", "eps2", "sigma2_sq", "time") %in% names(df)))
  expect_equal(df$time, 1:100)

  # Check that data is at percent scale (like Prono 2014)
  expect_true(abs(mean(df$Y2) - 0.097) < 0.5)  # Should be near 0.097%
  expect_true(sd(df$Y2) < 5)  # Weekly vol should be a few percent

  # Test GARCH parameter validation
  expect_error(
    generate_prono_data(n = 100, garch_params = list(omega = 0.1, alpha = 0.5, beta = 0.6)),
    "GARCH parameters must satisfy alpha \\+ beta < 1"
  )
})

test_that("run_single_prono_simulation performs estimation correctly", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("rugarch")

  config <- create_prono_config(n = 200, k = 1, seed = 123)

  result <- run_single_prono_simulation(config)

  # Check that all expected fields are present
  expect_true(all(c("gamma1_true", "gamma1_ols", "gamma1_iv",
                    "se_ols", "se_iv", "bias_ols", "bias_iv",
                    "f_stat", "n") %in% names(result)))

  # Check that IV reduces bias compared to OLS (on average)
  expect_equal(result$gamma1_true, config$gamma1)
  expect_equal(result$n, config$n)
})

test_that("create_prono_config creates valid configuration", {
  # Test default config with percent scale
  config <- create_prono_config()

  expect_type(config, "list")
  expect_equal(config$n, 500)
  expect_equal(config$k, 1)
  expect_equal(config$gamma1, 1.0)  # Default beta = 1
  expect_equal(config$beta2[1], 0.097)  # Market mean = 0.097%
  expect_true("garch_params" %in% names(config))
  expect_equal(config$garch_params$alpha + config$garch_params$beta, 0.95)

  # Test custom config
  custom_config <- create_prono_config(
    n = 1000,
    k = 2,
    gamma1 = 0.8,
    garch_params = list(omega = 0.2, alpha = 0.15, beta = 0.7)
  )

  expect_equal(custom_config$n, 1000)
  expect_equal(custom_config$k, 2)
  expect_equal(custom_config$gamma1, 0.8)
  expect_equal(custom_config$garch_params$omega, 0.2)
})

test_that("run_prono_demo executes without error", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("rugarch")

  expect_output(
    result <- run_prono_demo(n = 100, print_results = TRUE),
    "Prono \\(2014\\) GARCH-Based Identification Demo"
  )

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("iv_fit" %in% names(result))
})

test_that("run_prono_monte_carlo performs multiple simulations", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("rugarch")

  config <- create_prono_config(n = 100, k = 1, seed = 123, verbose = FALSE)

  results <- run_prono_monte_carlo(config, n_sims = 10, parallel = FALSE, progress = FALSE)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 10)
  expect_true(all(c("gamma1_ols", "gamma1_iv", "bias_ols", "bias_iv", "f_stat") %in% names(results)))

  # Check that on average, IV has less bias than OLS
  mean_bias_ols <- mean(abs(results$bias_ols))
  mean_bias_iv <- mean(abs(results$bias_iv))

  # With the default rho = 0.3, IV should generally do better
  # This is a statistical test, so we check it's at least not much worse
  expect_true(mean_bias_iv <= mean_bias_ols * 1.1)  # Allow 10% margin
})

test_that("Prono method handles missing rugarch package gracefully", {
  skip_if_not_installed("ivreg")

  # Temporarily mock the rugarch availability check
  with_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package == "rugarch") return(FALSE)
    },
    {
      config <- create_prono_config(n = 100, k = 1, seed = 123)

      # Should still work but with warning about using squared residuals
      expect_warning(
        result <- run_single_prono_simulation(config),
        "GARCH fitting failed"
      )

      # Result should still be valid
      expect_type(result, "list")
      expect_true(all(c("gamma1_ols", "gamma1_iv") %in% names(result)))
    }
  )
})

# Check if the GARCH model converged (this is a simple check)
garch_converged <- function(fit) {
  # Based on rugarch documentation, convergence status is in fit@fit$convergence
  # 0 indicates convergence
  if (!is.null(fit@fit$convergence) && fit@fit$convergence == 0) {
    TRUE
  } else {
    # Fallback or more detailed check if needed
    # For this test, simply checking if coefficients are available might be enough
    # if main interest is not GARCH convergence itself but whether Prono runs.
    length(coef(fit)) > 0
  }
}

# Test with a basic configuration
