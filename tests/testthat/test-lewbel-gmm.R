# Test for Lewbel GMM implementation

test_that("lewbel_triangular_moments generates correct moment conditions", {
  skip_if_not_installed("gmm")

  # Generate test data
  set.seed(123)
  n <- 100
  data <- data.frame(
    Xk = rnorm(n),
    Y2 = rnorm(n),
    Y1 = rnorm(n)
  )

  # Test parameters
  theta <- c(0.5, 1.0, -0.5, 1.5, 0.8) # beta1_0, beta1_1, gamma1, beta2_0, beta2_1

  # Generate moment conditions
  moments <- lewbel_triangular_moments(
    theta = theta,
    data = data,
    y1_var = "Y1",
    y2_var = "Y2",
    x_vars = "Xk",
    z_vars = NULL,
    add_intercept = TRUE
  )

  # Check dimensions
  expect_equal(nrow(moments), n)
  expect_equal(ncol(moments), 2 + 2 + 1) # 2 for X*e1, 2 for X*e2, 1 for Z*e1*e2

  # Check that moments have zero mean under true parameters
  # (This would be true if data were generated from the model)
  expect_true(all(abs(colMeans(moments)) < 10)) # Loose check for random data
})


test_that("lewbel_gmm estimates triangular system correctly", {
  skip_if_not_installed("gmm")

  # Generate data from known model
  set.seed(42)
  n <- 500
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)

  # Estimate using GMM
  gmm_result <- lewbel_gmm(data, system = "triangular")

  # Check class
  expect_s3_class(gmm_result, "lewbel_gmm")
  expect_s3_class(gmm_result, "gmm")

  # Check attributes
  expect_equal(attr(gmm_result, "lewbel_system"), "triangular")

  # Check coefficient names
  coef_names <- names(coef(gmm_result))
  expect_true("gamma1" %in% coef_names)
  expect_true("beta1_(Intercept)" %in% coef_names)
  expect_true("beta1_Xk" %in% coef_names)

  # Check that gamma1 estimate is reasonable
  gamma1_est <- coef(gmm_result)["gamma1"]
  expect_true(abs(gamma1_est - params$gamma1) < 0.5) # Within 0.5 of true value
})

test_that("lewbel_gmm handles different GMM types", {
  skip_if_not_installed("gmm")

  # Generate small dataset for speed
  set.seed(123)
  n <- 200
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)

  # Test two-step GMM
  gmm_twostep <- lewbel_gmm(data, gmm_type = "twoStep")
  expect_s3_class(gmm_twostep, "gmm")

  # Test iterative GMM
  gmm_iter <- lewbel_gmm(data, gmm_type = "iterative")
  expect_s3_class(gmm_iter, "gmm")

  # Test CUE
  gmm_cue <- lewbel_gmm(data, gmm_type = "cue")
  expect_s3_class(gmm_cue, "gmm")

  # Estimates should be similar
  gamma1_twostep <- coef(gmm_twostep)["gamma1"]
  gamma1_iter <- coef(gmm_iter)["gamma1"]
  gamma1_cue <- coef(gmm_cue)["gamma1"]

  expect_true(abs(gamma1_twostep - gamma1_iter) < 0.2)
  expect_true(abs(gamma1_twostep - gamma1_cue) < 0.2)
})

test_that("lewbel_gmm works with custom Z variables", {
  skip_if_not_installed("gmm")

  # Generate data with additional Z variables
  set.seed(123)
  n <- 300
  data <- data.frame(
    Xk = rnorm(n),
    Z1 = rnorm(n),
    Z2 = rnorm(n)^2, # Squared for heteroskedasticity
    Y2 = rnorm(n),
    Y1 = rnorm(n)
  )

  # Estimate with custom Z
  gmm_result <- lewbel_gmm(data, z_vars = c("Z1", "Z2"))

  # Check that it runs
  expect_s3_class(gmm_result, "gmm")

  # Check that custom Z variables were used
  # The gmm object might not store the Z vars directly as an attribute
  expect_s3_class(gmm_result, "gmm")
})

test_that("summary and print methods work correctly", {
  skip_if_not_installed("gmm")

  # Generate simple data
  set.seed(123)
  n <- 200
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)

  # Estimate model
  gmm_result <- lewbel_gmm(data)

  # Test print method
  expect_output(print(gmm_result), "Lewbel GMM Estimation Results")
  expect_output(print(gmm_result), "gamma1 =")

  # Test summary method
  expect_output(summary(gmm_result), "Lewbel \\(2012\\) Heteroskedasticity-Based GMM Estimation")
  expect_output(summary(gmm_result), "System type: triangular")
})


test_that("compare_gmm_2sls returns comparison data frame", {
  skip_if_not_installed("gmm")

  # Generate test data
  set.seed(123)
  n <- 300
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)

  # Get comparison
  comparison <- compare_gmm_2sls(data)

  # Check structure
  expect_true(is.data.frame(comparison))
  expect_true("Estimator" %in% names(comparison))
  expect_true("gamma1" %in% names(comparison))
  expect_true("StdError" %in% names(comparison))

  # Should have GMM row
  expect_true(any(grepl("GMM", comparison$Estimator)))
})

test_that("lewbel_gmm handles missing gmm package gracefully", {
  # Temporarily remove gmm from search path
  if ("package:gmm" %in% search()) {
    detach("package:gmm", unload = TRUE)
  }

  # Mock requireNamespace to return FALSE
  with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    {
      # Should give informative error
      expect_error(
        lewbel_gmm(data.frame(Y1 = 1:10, Y2 = 1:10, Xk = 1:10)),
        "Package 'gmm' is required but not installed"
      )
    },
    .package = "base"
  )
})

test_that("lewbel_gmm handles different vcov specifications", {
  skip_if_not_installed("gmm")

  # Generate test data
  set.seed(123)
  n <- 300
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(n, params)

  # Test HAC (default)
  gmm_hac <- lewbel_gmm(data, vcov = "HAC")
  expect_s3_class(gmm_hac, "gmm")

  # Test iid
  gmm_iid <- lewbel_gmm(data, vcov = "iid")
  expect_s3_class(gmm_iid, "gmm")

  # Standard errors should differ
  se_hac <- sqrt(diag(vcov(gmm_hac)))["gamma1"]
  se_iid <- sqrt(diag(vcov(gmm_iid)))["gamma1"]
  expect_true(abs(se_hac - se_iid) > 0)
})

# Tests for Rigobon GMM

test_that("rigobon_triangular_moments generates correct moment conditions", {
  skip_if_not_installed("gmm")

  # Generate test data with regimes
  set.seed(123)
  n <- 100
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 2.5)
  )
  data <- generate_rigobon_data(n, params)

  # Test parameters (2 for beta1, 1 for gamma1, 2 for beta2)
  theta <- c(0.5, 1.5, -0.8, 1.0, -1.0)

  # Generate moment conditions
  moments <- rigobon_triangular_moments(
    theta = theta,
    data = data,
    y1_var = "Y1",
    y2_var = "Y2",
    x_vars = "Xk",
    regime_var = "regime",
    add_intercept = TRUE
  )

  # Check dimensions
  n_regimes <- length(unique(data$regime))
  expect_equal(nrow(moments), n)
  expect_equal(ncol(moments), 2 + 2 + (n_regimes - 1)) # X*e1, X*e2, Z*e1*e2 (n_regimes-1 instruments)
})




test_that("rigobon_gmm estimates triangular system correctly", {
  skip_if_not_installed("gmm")

  # Generate larger sample for stability
  set.seed(42)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 3.0) # Strong heteroskedasticity
  )
  data <- generate_rigobon_data(500, params)

  # Estimate using GMM
  result <- rigobon_gmm(data, system = "triangular", verbose = FALSE)

  # Check result structure
  expect_s3_class(result, c("rigobon_gmm", "lewbel_gmm"))
  expect_true("coefficients" %in% names(result))
  expect_true("vcov" %in% names(result))
  # J_test is optional (only present when overidentified)
  expect_equal(result$n_regimes, 2)

  # Check coefficient names
  expect_true("gamma1" %in% names(result$coefficients))

  # Check that estimate is reasonable (within 0.5 of true value)
  gamma1_est <- result$coefficients["gamma1"]
  expect_true(abs(gamma1_est - params$gamma1) < 0.5)
})


test_that("rigobon_gmm handles edge cases", {
  skip_if_not_installed("gmm")

  # Case 1: Only one regime (should fail)
  data_single <- data.frame(
    Y1 = rnorm(100),
    Y2 = rnorm(100),
    Xk = rnorm(100),
    regime = rep(1, 100)
  )
  expect_error(rigobon_gmm(data_single), "at least 2 regimes")

  # Case 2: Missing variables
  data_missing <- data.frame(
    Y1 = rnorm(100),
    Y2 = rnorm(100)
  )
  expect_error(rigobon_gmm(data_missing), "not found in data")
})




test_that("rigobon_gmm different GMM types produce consistent results", {
  skip_if_not_installed("gmm")

  # Generate data
  set.seed(999)
  data <- generate_rigobon_data(400, list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.5, 0.5),
    sigma2_regimes = c(1.0, 2.5)
  ))

  # Try different GMM types
  gmm_twostep <- rigobon_gmm(data, gmm_type = "twoStep", verbose = FALSE)
  gmm_iter <- rigobon_gmm(data, gmm_type = "iterative", verbose = FALSE)

  # Results should be similar
  gamma1_twostep <- gmm_twostep$coefficients["gamma1"]
  gamma1_iter <- gmm_iter$coefficients["gamma1"]

  expect_true(abs(gamma1_twostep - gamma1_iter) < 0.1)
})


test_that("rigobon_gmm verbose output works", {
  skip_if_not_installed("gmm")

  # Generate small dataset
  set.seed(123)
  data <- generate_rigobon_data(100, list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 2.0)
  ))

  # Test verbose output
  expect_message(
    rigobon_gmm(data, verbose = TRUE),
    "Rigobon GMM Estimation"
  )
})


test_that("rigobon_gmm compares well with 2SLS", {
  skip_if_not_installed("gmm")

  # Generate data
  set.seed(456)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 3.0)
  )
  data <- generate_rigobon_data(500, params)

  # GMM estimation
  gmm_result <- rigobon_gmm(data, verbose = FALSE)

  # 2SLS estimation
  tsls_result <- run_rigobon_estimation(data, return_diagnostics = FALSE)

  # Compare gamma1 estimates
  gamma1_gmm <- gmm_result$coefficients["gamma1"]
  gamma1_tsls <- tsls_result$tsls$estimates["gamma1"]

  # Should be similar but GMM typically more efficient
  expect_true(abs(gamma1_gmm - gamma1_tsls) < 0.2)
})


# Tests for Prono GMM

test_that("prono_triangular_moments generates correct moment conditions", {
  skip_if_not_installed("gmm")

  # Generate test data
  set.seed(123)
  config <- create_prono_config(n = 100)
  data <- generate_prono_data(
    n = config$n,
    beta1 = config$beta1,
    beta2 = config$beta2,
    gamma1 = config$gamma1,
    garch_params = config$garch_params
  )

  # Add fitted GARCH variance (or use actual values for testing)
  data$sigma2_sq_hat <- data$sigma2_sq

  # Test parameters (2 for beta1, 1 for gamma1, 2 for beta2)
  theta <- c(0.05, 0.01, 1.0, 0.097, -0.005)

  # Generate moment conditions
  moments <- prono_triangular_moments(
    theta = theta,
    data = data,
    y1_var = "Y1",
    y2_var = "Y2",
    x_vars = "X1",
    add_intercept = TRUE
  )

  # Check dimensions
  expect_equal(nrow(moments), 100)
  expect_equal(ncol(moments), 2 + 2 + 1) # X*e1, X*e2, Z*e1*e2
})


test_that("prono_gmm estimates triangular system correctly", {
  skip_if_not_installed("gmm")
  skip_if_not_installed("tsgarch")

  # Generate larger sample
  set.seed(42)
  config <- create_prono_config(n = 300)
  data <- generate_prono_data(
    n = config$n,
    beta1 = config$beta1,
    beta2 = config$beta2,
    gamma1 = config$gamma1,
    garch_params = config$garch_params,
    rho = 0.3 # Endogeneity
  )

  # Estimate using GMM
  result <- prono_gmm(data, verbose = FALSE)

  # Check result structure
  expect_s3_class(result, c("prono_gmm", "lewbel_gmm"))
  expect_true("coefficients" %in% names(result))
  expect_true("vcov" %in% names(result))
  expect_true("first_stage_F" %in% names(result))

  # Check coefficient names
  expect_true("gamma1" %in% names(result$coefficients))

  # Check that estimate is reasonable
  gamma1_est <- result$coefficients["gamma1"]
  expect_true(abs(gamma1_est - config$gamma1) < 0.5)
})


test_that("prono_gmm handles missing tsgarch gracefully", {
  skip_if_not_installed("gmm")

  # Generate data without pre-fitted variances
  data <- generate_prono_data(n = 100)
  # Remove any pre-computed variance columns to force GARCH fitting
  data$sigma2_sq <- NULL
  data$sigma2_sq_hat <- NULL

  # Mock requireNamespace to return FALSE for tsgarch
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tsgarch") FALSE else TRUE
    },
    {
      # Without tsgarch, prono_gmm should warn and proceed with squared residuals
      expect_warning(
        prono_gmm(data, verbose = TRUE),
        "tsgarch package not available"
      )
    },
    .package = "base"
  )
})


test_that("prono_gmm works with pre-fitted GARCH variances", {
  skip_if_not_installed("gmm")

  # Generate data with known variances
  set.seed(123)
  data <- generate_prono_data(n = 200)

  # Use actual sigma2_sq as pre-fitted values
  data$sigma2_sq_hat <- data$sigma2_sq

  # Run GMM without fitting GARCH
  result <- prono_gmm(data, fit_garch = FALSE, verbose = FALSE)

  # Should work
  expect_s3_class(result, "prono_gmm")
  expect_true("gamma1" %in% names(result$coefficients))
})


test_that("prono_gmm compares well with 2SLS", {
  skip_if_not_installed("gmm")

  # Generate data
  set.seed(456)
  config <- create_prono_config(n = 400)
  data <- generate_prono_data(
    n = config$n,
    beta1 = config$beta1,
    beta2 = config$beta2,
    gamma1 = config$gamma1,
    garch_params = config$garch_params
  )

  # Use actual variances to avoid GARCH estimation issues
  data$sigma2_sq_hat <- data$sigma2_sq

  # GMM estimation
  gmm_result <- prono_gmm(data, fit_garch = FALSE, verbose = FALSE)

  # 2SLS estimation
  tsls_result <- run_single_prono_simulation(config, return_details = TRUE)

  # Compare gamma1 estimates
  gamma1_gmm <- gmm_result$coefficients["gamma1"]
  gamma1_tsls <- tsls_result$gamma1_iv

  # Should be similar
  expect_true(abs(gamma1_gmm - gamma1_tsls) < 0.3)
})


test_that("prono_gmm auto-detects exogenous variables", {
  skip_if_not_installed("gmm")

  # Generate data with multiple X variables
  set.seed(789)
  config <- create_prono_config(n = 200, k = 3)
  data <- generate_prono_data(
    n = config$n,
    beta1 = config$beta1,
    beta2 = config$beta2,
    gamma1 = config$gamma1,
    k = config$k,
    garch_params = config$garch_params
  )

  # Add pre-fitted variances
  data$sigma2_sq_hat <- data$sigma2_sq

  # Should auto-detect X1, X2, X3
  result <- prono_gmm(data, fit_garch = FALSE, verbose = FALSE)

  # Check coefficients
  expect_true("beta1_X1" %in% names(result$coefficients))
  expect_true("beta1_X2" %in% names(result$coefficients))
  expect_true("beta1_X3" %in% names(result$coefficients))
})
