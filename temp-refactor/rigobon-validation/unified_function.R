#' Define GMM Moment Conditions for Rigobon Systems (Unified)
#'
#' Creates a unified moment function for GMM estimation for both triangular and
#' simultaneous systems using Rigobon's (2003) heteroskedasticity-based
#' identification. This function replaces `rigobon_triangular_moments` and
#' `rigobon_simultaneous_moments` with a single, more abstract implementation.
#'
#' @param theta Numeric vector. Parameters to estimate.
#' @param data data.frame. The data for estimation.
#' @param system Character. The type of system, either "triangular" or "simultaneous".
#' @param y1_var,y2_var Character. Names of the dependent variables.
#' @param x_vars Character vector. Names of exogenous variables.
#' @param regime_var Character. Name of the regime indicator variable.
#' @param add_intercept Logical. Whether to add an intercept.
#'
#' @return A matrix of moment conditions.
#' @keywords internal
#'
rigobon_moment_conditions <- function(theta, data, system, y1_var, y2_var, x_vars,
                                      regime_var, add_intercept = TRUE) {

  # --- 1. Unified Setup ---
  n <- nrow(data)
  k <- length(x_vars) + if (add_intercept) 1 else 0

  # This two-step logic for creating x_matrix is critical to replicate the
  # original function's column naming behavior for the intercept.
  x_matrix_base <- as.matrix(data[, x_vars, drop = FALSE])
  x_matrix <- if (add_intercept) cbind(1, x_matrix_base) else x_matrix_base

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]

  # --- 2. Parameter and Error Calculation ---
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]

  if (system == "triangular") {
    eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
    eps2 <- y2_data - x_matrix %*% beta2
  } else { # simultaneous
    gamma2 <- theta[2 * k + 2]
    eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
    eps2 <- y2_data - x_matrix %*% beta2 - y1_data * gamma2
  }

  # --- 3. Moment Assembly (replicating original logic) ---
  regimes <- data[[regime_var]]
  unique_regimes <- sort(unique(regimes))

  m1 <- x_matrix * as.vector(eps1)
  m2 <- x_matrix * as.vector(eps2)

  rigobon_moments_list <- list()
  for (i in 1:(length(unique_regimes) - 1)) {
    for (j in (i + 1):length(unique_regimes)) {
      regime_i <- regimes == unique_regimes[i]
      regime_j <- regimes == unique_regimes[j]

      m_ij <- numeric(n)
      m_ij[regime_i] <- eps1[regime_i] * eps2[regime_i]
      m_ij[regime_j] <- -eps1[regime_j] * eps2[regime_j]

      # Replicate original naming difference for perfect equivalence
      if (system == "triangular") {
        moment_name <- paste0("r", i, "_", j)
      } else {
        moment_name <- paste0("r", i, "_", j, "_cov")
      }
      rigobon_moments_list[[moment_name]] <- m_ij
    }
  }

  # Combine all moments.
  moments <- cbind(m1, m2, do.call(cbind, rigobon_moments_list))
  return(moments)
}
