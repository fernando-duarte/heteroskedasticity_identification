#' Adjust standard errors for degrees of freedom
#'
#' @param se Asymptotic standard error
#' @param n Sample size
#' @param k Number of parameters
#' @param df_adjust Character string: "asymptotic" (default) or "finite"
#' @return Adjusted standard error
#' @export
adjust_se_for_df <- function(se, n, k, df_adjust = "asymptotic") {
  if (df_adjust == "finite") {
    # Finite sample correction: multiply by sqrt(n/(n-k))
    se * sqrt(n / (n - k))
  } else {
    # Asymptotic: no adjustment
    se
  }
}

#' Get critical value for confidence intervals
#'
#' @param n Sample size
#' @param k Number of parameters
#' @param alpha Significance level (default 0.05)
#' @param df_adjust Character string: "asymptotic" (default) or "finite"
#' @return Critical value
#' @export
get_critical_value <- function(n, k, alpha = .ALPHA_DEFAULT, df_adjust = "asymptotic") {
  if (df_adjust == "finite") {
    # Use t-distribution with n-k degrees of freedom
    qt(1 - alpha / 2, df = n - k)
  } else {
    # Use normal distribution (asymptotic)
    qnorm(1 - alpha / 2)
  }
}

#' Extract adjusted standard errors from ivreg model
#'
#' @param model An ivreg model object
#' @param df_adjust Character string: "asymptotic" (default) or "finite"
#' @return Named vector of adjusted standard errors
#' @export
extract_se_ivreg <- function(model, df_adjust = "asymptotic") {
  # Get the variance-covariance matrix
  vcov_mat <- vcov(model)

  # Extract raw standard errors
  se_raw <- sqrt(diag(vcov_mat))

  if (df_adjust == "finite") {
    # ivreg already uses finite sample SEs by default (like lm)
    se_adjusted <- se_raw
  } else {
    # Convert to asymptotic by undoing the finite sample correction
    n <- nobs(model)
    k <- length(coef(model))
    se_adjusted <- se_raw * sqrt((n - k) / n)
  }

  se_adjusted
}

#' Extract adjusted standard errors from lm model
#'
#' @param model An lm model object
#' @param df_adjust Character string: "asymptotic" (default) or "finite"
#' @return Named vector of adjusted standard errors
#' @export
extract_se_lm <- function(model, df_adjust = "asymptotic") {
  summ <- summary(model)

  if (df_adjust == "finite") {
    # lm already uses finite sample SEs by default
    se_adjusted <- summ$coefficients[, "Std. Error"]
  } else {
    # Convert to asymptotic by undoing the finite sample correction
    n <- nobs(model)
    k <- length(coef(model))
    se_adjusted <- summ$coefficients[, "Std. Error"] * sqrt((n - k) / n)
  }

  se_adjusted
}
