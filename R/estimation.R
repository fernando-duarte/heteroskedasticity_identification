#' Calculate Set Identification Bounds for Lewbel Estimator
#'
#' Computes set identification bounds for the endogenous parameter under
#' a relaxed covariance restriction. Optionally computes bootstrap standard
#' errors for the bounds.
#'
#' @importFrom stats coef lm nobs qnorm qt residuals vcov as.formula cov var sd
#'
#' @param data Data.frame. Dataset containing Y1, Y2, Xk, Z variables.
#' @param tau Numeric. Relaxation parameter for covariance restriction
#'   (0 <= tau < 1). When tau = 0, gives point identification.
#' @param compute_se Logical. Whether to compute bootstrap standard errors
#'   (default: FALSE).
#' @param b_reps Integer. Number of bootstrap replications if compute_se = TRUE
#'   (default: 100).
#' @template param-df-adjust
#' @details Note: This parameter currently only affects the
#'   interpretation of bootstrap SEs, not the bounds calculation itself.
#'
#' @details
#' Under the relaxed assumption
#' |Corr(Z, \eqn{\epsilon_1 \epsilon_2})| <= tau |Corr(Z, \eqn{\epsilon_2^2})|,
#' the parameter gamma_1 is set-identified. The bounds are computed as the
#' real roots of a quadratic equation in gamma_1.
#'
#' @return A list containing:
#'   \itemize{
#'     \item bounds: Numeric vector of length 2 with lower and upper bounds
#'     \item se: Numeric vector of length 2 with bootstrap standard errors
#'       (if requested)
#'   }
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' # TODO: Update generate_lewbel_data call if its return columns change to
#' # snake_case
#' data <- generate_lewbel_data(1000, params)
#'
#' # Point identification (tau = 0)
#' bounds_point <- calculate_lewbel_bounds(data, tau = 0)
#'
#' # Set identification with bootstrap SE
#' bounds_set <- calculate_lewbel_bounds(
#'   data,
#'   tau = 0.2, compute_se = TRUE, b_reps = 100
#' )
#' }
#'
#' @export
calculate_lewbel_bounds <- function(data,
                                    tau,
                                    compute_se = FALSE,
                                    b_reps = 100,
                                    df_adjust = "asymptotic") {
  # Main calculation function
  calculate_bounds_internal <- function(df, indices = seq_len(nrow(df))) {
    d <- df[indices, ]

    # Compute reduced-form residuals
    # TODO: Update column names (e.g., d$Xk to d$x_k) after
    # R/data-generation.R refactor
    d$W1 <- stats::residuals(stats::lm(Y1 ~ Xk, data = d))
    d$W2 <- stats::residuals(stats::lm(Y2 ~ Xk, data = d))

    # Calculate required covariances and variances
    # TODO: Update column names (e.g., d$Z to d$z_var) after
    # R/data-generation.R refactor
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
    coeff_a <- 1 - tau^2
    coeff_b <- 2 * ((cov_w1w2_w2sq / var_w2sq) * tau^2 -
      (cov_z_w1w2 / cov_z_w2sq))
    coeff_c <- (cov_z_w1w2 / cov_z_w2sq)^2 - (var_w1w2 / var_w2sq) * tau^2

    # Solve quadratic equation
    discriminant <- coeff_b^2 - 4 * coeff_a * coeff_c
    if (is.na(discriminant) || discriminant < 0) {
      return(c(NA, NA))
    }

    root1 <- (-coeff_b + sqrt(discriminant)) / (2 * coeff_a)
    root2 <- (-coeff_b - sqrt(discriminant)) / (2 * coeff_a)

    sort(c(root1, root2))
  }

  # Calculate main bounds
  bounds <- calculate_bounds_internal(data)

  # Bootstrap standard errors if requested
  if (compute_se && !any(is.na(bounds))) {
    boot_result <- tryCatch(
      boot::boot(data, calculate_bounds_internal, R = b_reps),
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
#' @param exog_vars Character vector. Names of exogenous variables
#'   (default: "Xk").
#' @param compute_bounds_se Logical. Whether to compute bootstrap SE for bounds
#'   (default: FALSE).
#' @param return_models Logical. Whether to return the fitted model objects
#'   (default: FALSE).
#' @template param-df-adjust
#'
#' @return If return_models = FALSE: A data.frame with one row containing
#'   simulation results including OLS and 2SLS estimates, coverage indicators,
#'   first-stage F-statistic, and identification bounds.
#'   If return_models = TRUE: A list containing:
#'   \itemize{
#'     \item results: The data.frame described above
#'     \item models: A list with ols_model, first_stage_model, tsls_model
#'     \item data: The generated data
#'   }
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
#'
#' # With models
#' result_with_models <- run_single_lewbel_simulation(
#'   1, params,
#'   return_models = TRUE
#' )
#' }
#'
#' @export
run_single_lewbel_simulation <- function(sim_id,
                                         params,
                                         endog_var = "Y2",
                                         exog_vars = "Xk",
                                         compute_bounds_se = FALSE,
                                         return_models = FALSE,
                                         df_adjust = "asymptotic") {
  # Wrap entire function in tryCatch for robustness
  tryCatch(
    {
      # Generate data
      # Support multiple X variables if n_x is specified in params
      n_x <- if (!is.null(params$n_x)) params$n_x else 1
      df <- generate_lewbel_data(params$sample_size, params, n_x = n_x)

      # Create formula strings
      # TODO: Update Y1, endog_var, exog_vars if column names change after
      # R/data-generation.R refactor
      y1_formula_str <- paste(
        "Y1 ~", endog_var, "+", paste(exog_vars, collapse = " + ")
      )
      y1_formula <- stats::as.formula(y1_formula_str)

      y2_formula_str <- paste(
        endog_var, "~", paste(exog_vars, collapse = " + ")
      )
      y2_formula <- stats::as.formula(y2_formula_str)

      # --- OLS Estimation ---
      ols_model <- tryCatch(
        stats::lm(y1_formula, data = df),
        error = function(e) NULL
      )

      if (is.null(ols_model)) {
        ols_est <- NA_real_
        ols_se <- NA_real_
        ols_covers <- NA
      } else {
        ols_est <- stats::coef(ols_model)[endog_var]
        # Extract SE with df adjustment
        all_se <- extract_se_lm(ols_model, df_adjust = df_adjust)
        ols_se <- all_se[endog_var]

        # Get critical value
        n <- nobs(ols_model)
        k <- length(coef(ols_model))
        crit_val <- get_critical_value(
          n, k,
          alpha = 0.05, df_adjust = df_adjust
        )

        ols_covers <- (params$gamma1 >= ols_est - crit_val * ols_se &&
          params$gamma1 <= ols_est + crit_val * ols_se)
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

      # Construct Lewbel instrument(s)
      # Handle both single and multiple X variables
      if (n_x == 1) {
        lewbel_iv <- (df$Z - mean(df$Z)) * e2_hat
      } else {
        # For multiple X, we can use multiple instruments (one per Z)
        # For now, use the first Z as in the original implementation
        lewbel_iv <- (df$Z1 - mean(df$Z1)) * e2_hat
      }

      # Initialize model variables for return_models
      first_stage <- NULL
      tsls_model <- NULL

      # Check for invalid instrument
      if (any(is.na(lewbel_iv)) || stats::sd(lewbel_iv, na.rm = TRUE) < 1e-10) {
        tsls_est <- NA_real_
        tsls_se <- NA_real_
        tsls_covers <- NA
        first_stage_f <- NA_real_
      } else {
        # First-stage regression for F-statistic
        df$lewbel_iv <- lewbel_iv
        first_stage_formula_str <- paste(
          endog_var, "~", paste(exog_vars, collapse = " + "), "+ lewbel_iv"
        )
        first_stage_formula <- stats::as.formula(first_stage_formula_str)

        first_stage <- tryCatch(
          stats::lm(first_stage_formula, data = df),
          error = function(e) NULL
        )

        if (is.null(first_stage)) {
          first_stage_f <- NA_real_
        } else {
          first_stage_f <- summary(first_stage)$fstatistic[1]
        }

        # 2SLS estimation using AER::ivreg
        iv_formula_str <- paste(
          "Y1 ~", endog_var, "+", paste(exog_vars, collapse = " + "),
          "|", paste(exog_vars, collapse = " + "), "+ lewbel_iv"
        )
        iv_formula <- stats::as.formula(iv_formula_str)

        tsls_model <- tryCatch(
          AER::ivreg(iv_formula, data = df),
          error = function(e) NULL
        )

        if (is.null(tsls_model)) {
          tsls_est <- NA_real_
          tsls_se <- NA_real_
          tsls_covers <- NA
        } else {
          tsls_coef <- tryCatch(
            stats::coef(tsls_model)[endog_var],
            error = function(e) NA
          )
          tsls_summary <- tryCatch(
            summary(tsls_model),
            error = function(e) NULL
          )

          if (is.na(tsls_coef) || is.null(tsls_summary)) {
            tsls_est <- NA_real_
            tsls_se <- NA_real_
            tsls_covers <- NA
          } else {
            tsls_est <- tsls_coef
            # Extract SE with df adjustment
            all_se <- extract_se_ivreg(tsls_model, df_adjust = df_adjust)
            tsls_se <- all_se[endog_var]

            # Get critical value
            n <- nobs(tsls_model)
            k <- length(coef(tsls_model))
            crit_val <- get_critical_value(
              n, k,
              alpha = 0.05, df_adjust = df_adjust
            )

            tsls_covers <- (params$gamma1 >= tsls_est - crit_val * tsls_se &&
              params$gamma1 <= tsls_est + crit_val * tsls_se)
          }
        }
      }

      # --- Set Identification Bounds ---
      bounds_tau0 <- tryCatch(
        calculate_lewbel_bounds(
          df, 0,
          compute_se = compute_bounds_se, b_reps = params$bootstrap_reps,
          df_adjust = df_adjust
        ),
        error = function(e) list(bounds = c(NA, NA), se = c(NA, NA))
      )

      bounds_tau_set <- tryCatch(
        calculate_lewbel_bounds(
          df,
          params$tau_set_id,
          compute_se = compute_bounds_se,
          b_reps = params$bootstrap_reps
        ),
        error = function(e) list(bounds = c(NA, NA), se = c(NA, NA))
      )

      # Prepare results data frame
      results_df <- data.frame(
        sim_id = sim_id,
        sample_size = params$sample_size,
        delta_het = params$delta_het,
        ols_gamma1 = ols_est,
        tsls_gamma1 = tsls_est,
        ols_se = ols_se,
        tsls_se = tsls_se,
        ols_coverage = ols_covers,
        tsls_coverage = tsls_covers,
        first_stage_F = first_stage_f,
        bound_lower_tau0 = bounds_tau0$bounds[1],
        bound_upper_tau0 = bounds_tau0$bounds[2],
        bound_lower_tau_set = bounds_tau_set$bounds[1],
        bound_upper_tau_set = bounds_tau_set$bounds[2],
        bound_se_lower = bounds_tau_set$se[1],
        bound_se_upper = bounds_tau_set$se[2],
        df_adjust = df_adjust
      )

      # Return results based on return_models parameter
      if (return_models) {
        list(
          results = results_df,
          models = list(
            ols_model = ols_model,
            first_stage_model = first_stage,
            tsls_model = tsls_model
          ),
          data = df
        )
      } else {
        results_df
      }
    },
    error = function(e) {
      # Return a row with NA values if entire function fails
      # Ensure NA values have correct types
      results_df <- data.frame(
        sim_id = sim_id,
        sample_size = params$sample_size,
        delta_het = params$delta_het,
        ols_gamma1 = NA_real_,
        tsls_gamma1 = NA_real_,
        ols_se = NA_real_,
        tsls_se = NA_real_,
        ols_coverage = NA,
        tsls_coverage = NA,
        first_stage_F = NA_real_,
        bound_lower_tau0 = NA_real_,
        bound_upper_tau0 = NA_real_,
        bound_lower_tau_set = NA_real_,
        bound_upper_tau_set = NA_real_,
        bound_se_lower = NA_real_,
        bound_se_upper = NA_real_,
        df_adjust = df_adjust
      )

      if (return_models) {
        list(
          results = results_df,
          models = list(
            ols_model = NULL,
            first_stage_model = NULL,
            tsls_model = NULL
          ),
          data = NULL
        )
      } else {
        results_df
      }
    }
  )
}
