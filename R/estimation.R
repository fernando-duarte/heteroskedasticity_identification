#' Calculate Set Identification Bounds for Lewbel Estimator
#'
#' Computes set identification bounds for the endogenous parameter under
#' a relaxed covariance restriction. Optionally computes bootstrap standard
#' errors for the bounds.
#'
#' @param data Data.frame. Dataset containing Y1, Y2, Xk, Z variables.
#' @param tau Numeric. Relaxation parameter for covariance restriction (0 <= tau < 1).
#'   When tau = 0, gives point identification.
#' @param compute_se Logical. Whether to compute bootstrap standard errors (default: FALSE).
#' @param B Integer. Number of bootstrap replications if compute_se = TRUE (default: 100).
#'
#' @details
#' Under the relaxed assumption |Corr(Z, \eqn{\epsilon_1 \epsilon_2})| <= tau |Corr(Z, \eqn{\epsilon_2^2})|,
#' the parameter gamma_1 is set-identified. The bounds are computed as the
#' real roots of a quadratic equation in gamma_1.
#'
#' @return A list containing:
#'   \itemize{
#'     \item bounds: Numeric vector of length 2 with lower and upper bounds
#'     \item se: Numeric vector of length 2 with bootstrap standard errors (if requested)
#'   }
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data <- generate_lewbel_data(1000, params)
#'
#' # Point identification (tau = 0)
#' bounds_point <- calculate_lewbel_bounds(data, tau = 0)
#'
#' # Set identification with bootstrap SE
#' bounds_set <- calculate_lewbel_bounds(data, tau = 0.2, compute_se = TRUE, B = 100)
#' }
#'
#' @export
calculate_lewbel_bounds <- function(data, tau, compute_se = FALSE, B = 100) {
  # Main calculation function
  calculate_bounds_internal <- function(df, indices = seq_len(nrow(df))) {
    d <- df[indices, ]

    # Compute reduced-form residuals
    d$W1 <- stats::residuals(stats::lm(Y1 ~ Xk, data = d))
    d$W2 <- stats::residuals(stats::lm(Y2 ~ Xk, data = d))

    # Calculate required covariances and variances
    cov_z_w1w2 <- stats::cov(d$Z, d$W1 * d$W2)
    cov_z_w2sq <- stats::cov(d$Z, d$W2^2)

    # Check for weak identification
    if (abs(cov_z_w2sq) < 1e-6) {
      return(c(NA, NA))
    }

    var_w1w2 <- stats::var(d$W1 * d$W2)
    var_w2sq <- stats::var(d$W2^2)
    cov_w1w2_w2sq <- stats::cov(d$W1 * d$W2, d$W2^2)

    # Coefficients of quadratic equation in gamma1
    A <- 1 - tau^2
    B <- 2 * ((cov_w1w2_w2sq / var_w2sq) * tau^2 - (cov_z_w1w2 / cov_z_w2sq))
    C <- (cov_z_w1w2 / cov_z_w2sq)^2 - (var_w1w2 / var_w2sq) * tau^2

    # Solve quadratic equation
    discriminant <- B^2 - 4 * A * C
    if (is.na(discriminant) || discriminant < 0) {
      return(c(NA, NA))
    }

    root1 <- (-B + sqrt(discriminant)) / (2 * A)
    root2 <- (-B - sqrt(discriminant)) / (2 * A)

    sort(c(root1, root2))
  }

  # Calculate main bounds
  bounds <- calculate_bounds_internal(data)

  # Bootstrap standard errors if requested
  if (compute_se && !any(is.na(bounds))) {
    boot_result <- tryCatch(
      boot::boot(data, calculate_bounds_internal, R = B),
      error = function(e) NULL
    )

    if (!is.null(boot_result)) {
      se_lower <- stats::sd(boot_result$t[, 1], na.rm = TRUE)
      se_upper <- stats::sd(boot_result$t[, 2], na.rm = TRUE)
      return(list(bounds = bounds, se = c(se_lower, se_upper)))
    }
  }

  list(bounds = bounds, se = c(NA, NA))
}


#' Run Single Lewbel Simulation
#'
#' Executes a single Monte Carlo simulation run comparing OLS, 2SLS (Lewbel),
#' and set identification approaches for estimating the endogenous parameter.
#'
#' @param sim_id Integer. Simulation run identifier.
#' @param params List. Parameters for data generation and estimation.
#' @param endog_var Character. Name of endogenous variable (default: "Y2").
#' @param exog_vars Character vector. Names of exogenous variables (default: "Xk").
#' @param compute_bounds_se Logical. Whether to compute bootstrap SE for bounds (default: FALSE).
#'
#' @return A data.frame with one row containing simulation results including:
#'   OLS and 2SLS estimates, coverage indicators, first-stage F-statistic,
#'   and identification bounds.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' params <- list(
#'   sample_size = config$main_sample_size,
#'   beta1_0 = config$beta1_0, beta1_1 = config$beta1_1, gamma1 = config$gamma1,
#'   beta2_0 = config$beta2_0, beta2_1 = config$beta2_1,
#'   alpha1 = config$alpha1, alpha2 = config$alpha2,
#'   delta_het = config$delta_het, tau_set_id = config$tau_set_id,
#'   bootstrap_reps = config$bootstrap_reps
#' )
#' result <- run_single_lewbel_simulation(1, params)
#' }
#'
#' @export
run_single_lewbel_simulation <- function(sim_id, params,
                                         endog_var = "Y2",
                                         exog_vars = "Xk",
                                         compute_bounds_se = FALSE) {
  # Wrap entire function in tryCatch for robustness
  tryCatch(
    {
      # Generate data
      df <- generate_lewbel_data(params$sample_size, params)

      # Create formula strings
      y1_formula <- stats::as.formula(paste("Y1 ~", endog_var, "+", paste(exog_vars, collapse = " + ")))
      y2_formula <- stats::as.formula(paste(endog_var, "~", paste(exog_vars, collapse = " + ")))

      # --- OLS Estimation ---
      ols_model <- tryCatch(
        stats::lm(y1_formula, data = df),
        error = function(e) NULL
      )

      if (is.null(ols_model)) {
        ols_est <- NA
        ols_se <- NA
        ols_covers <- NA
      } else {
        ols_est <- stats::coef(ols_model)[endog_var]
        ols_se <- summary(ols_model)$coefficients[endog_var, "Std. Error"]
        ols_covers <- (params$gamma1 >= ols_est - 1.96 * ols_se &&
          params$gamma1 <= ols_est + 1.96 * ols_se)
      }

      # --- 2SLS (Lewbel) Estimation ---
      # Generate residuals from second equation
      second_stage_model <- tryCatch(
        stats::lm(y2_formula, data = df),
        error = function(e) NULL
      )

      if (is.null(second_stage_model)) {
        e2_hat <- rep(NA, nrow(df))
      } else {
        e2_hat <- stats::residuals(second_stage_model)
      }

      # Construct Lewbel instrument
      lewbel_iv <- (df$Z - mean(df$Z)) * e2_hat

      # Check for invalid instrument
      if (any(is.na(lewbel_iv)) || sd(lewbel_iv, na.rm = TRUE) < 1e-10) {
        tsls_est <- NA
        tsls_se <- NA
        tsls_covers <- NA
        first_stage_f <- NA
      } else {
        # First-stage regression for F-statistic
        df$lewbel_iv <- lewbel_iv
        first_stage_formula <- stats::as.formula(paste(
          endog_var, "~", paste(exog_vars, collapse = " + "), "+ lewbel_iv"
        ))
        first_stage <- tryCatch(
          stats::lm(first_stage_formula, data = df),
          error = function(e) NULL
        )

        if (is.null(first_stage)) {
          first_stage_f <- NA
        } else {
          first_stage_f <- summary(first_stage)$fstatistic[1]
        }

        # 2SLS estimation using AER::ivreg
        iv_formula <- stats::as.formula(paste(
          "Y1 ~", endog_var, "+", paste(exog_vars, collapse = " + "),
          "|", paste(exog_vars, collapse = " + "), "+ lewbel_iv"
        ))
        tsls_model <- tryCatch(AER::ivreg(iv_formula, data = df),
          error = function(e) NULL
        )

        if (is.null(tsls_model)) {
          tsls_est <- NA
          tsls_se <- NA
          tsls_covers <- NA
        } else {
          tsls_coef <- tryCatch(stats::coef(tsls_model)[endog_var], error = function(e) NA)
          tsls_summary <- tryCatch(summary(tsls_model), error = function(e) NULL)

          if (is.na(tsls_coef) || is.null(tsls_summary)) {
            tsls_est <- NA
            tsls_se <- NA
            tsls_covers <- NA
          } else {
            tsls_est <- tsls_coef
            tsls_se <- tsls_summary$coefficients[endog_var, "Std. Error"]
            tsls_covers <- (params$gamma1 >= tsls_est - 1.96 * tsls_se &&
              params$gamma1 <= tsls_est + 1.96 * tsls_se)
          }
        }
      }

      # --- Set Identification Bounds ---
      bounds_tau0 <- tryCatch(
        calculate_lewbel_bounds(df, 0, compute_se = compute_bounds_se, B = params$bootstrap_reps),
        error = function(e) list(bounds = c(NA, NA), se = c(NA, NA))
      )

      bounds_tau_set <- tryCatch(
        calculate_lewbel_bounds(df, params$tau_set_id, compute_se = compute_bounds_se, B = params$bootstrap_reps),
        error = function(e) list(bounds = c(NA, NA), se = c(NA, NA))
      )

      # Return results
      data.frame(
        sim_id = sim_id,
        sample_size = params$sample_size,
        delta_het = params$delta_het,
        ols_gamma1 = ols_est,
        tsls_gamma1 = tsls_est,
        ols_coverage = ols_covers,
        tsls_coverage = tsls_covers,
        first_stage_F = first_stage_f,
        bound_lower_tau0 = bounds_tau0$bounds[1],
        bound_upper_tau0 = bounds_tau0$bounds[2],
        bound_lower_tau_set = bounds_tau_set$bounds[1],
        bound_upper_tau_set = bounds_tau_set$bounds[2],
        bound_se_lower = bounds_tau_set$se[1],
        bound_se_upper = bounds_tau_set$se[2]
      )
    },
    error = function(e) {
      # Return a row with NA values if entire function fails
      data.frame(
        sim_id = sim_id,
        sample_size = params$sample_size,
        delta_het = params$delta_het,
        ols_gamma1 = NA,
        tsls_gamma1 = NA,
        ols_coverage = NA,
        tsls_coverage = NA,
        first_stage_F = NA,
        bound_lower_tau0 = NA,
        bound_upper_tau0 = NA,
        bound_lower_tau_set = NA,
        bound_upper_tau_set = NA,
        bound_se_lower = NA,
        bound_se_upper = NA
      )
    }
  )
}
