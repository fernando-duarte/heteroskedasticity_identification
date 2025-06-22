rigobon_triangular_moments <- function(theta, data, y1_var, y2_var, x_vars,
                                       regime_var, add_intercept = TRUE) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Parameters
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2

  # Get regimes
  regimes <- data[[regime_var]]
  unique_regimes <- sort(unique(regimes))

  # Standard moment conditions
  m1 <- x_matrix * as.vector(eps1)
  m2 <- x_matrix * as.vector(eps2)

  # Rigobon moments: cross-products of errors across regimes
  rigobon_moments_list <- list()
  for (i in 1:(length(unique_regimes) - 1)) {
    for (j in (i + 1):length(unique_regimes)) {
      regime_i <- regimes == unique_regimes[i]
      regime_j <- regimes == unique_regimes[j]

      # E[eps1*eps2 | regime i] - E[eps1*eps2 | regime j] = 0
      m_ij <- numeric(n)
      m_ij[regime_i] <- eps1[regime_i] * eps2[regime_i]
      m_ij[regime_j] <- -eps1[regime_j] * eps2[regime_j]
      rigobon_moments_list[[paste0("r", i, "_", j)]] <- m_ij
    }
  }

  # Combine all moments
  moments <- cbind(m1, m2, do.call(cbind, rigobon_moments_list))
  moments
}


rigobon_simultaneous_moments <- function(theta, data, y1_var, y2_var, x_vars,
                                         regime_var, add_intercept = TRUE) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Parameters for simultaneous system
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]
  gamma2 <- theta[2 * k + 2]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2 - y1_data * gamma2

  # Get regimes
  regimes <- data[[regime_var]]
  unique_regimes <- sort(unique(regimes))

  # Standard moment conditions
  m1 <- x_matrix * as.vector(eps1)
  m2 <- x_matrix * as.vector(eps2)

  # Rigobon moments
  rigobon_moments_list <- list()
  for (i in 1:(length(unique_regimes) - 1)) {
    for (j in (i + 1):length(unique_regimes)) {
      regime_i <- regimes == unique_regimes[i]
      regime_j <- regimes == unique_regimes[j]

      # Difference in covariances across regimes
      m_ij1 <- numeric(n)
      m_ij1[regime_i] <- eps1[regime_i] * eps2[regime_i]
      m_ij1[regime_j] <- -eps1[regime_j] * eps2[regime_j]

      rigobon_moments_list[[paste0("r", i, "_", j, "_cov")]] <- m_ij1
    }
  }

  # Combine all moments
  moments <- cbind(m1, m2, do.call(cbind, rigobon_moments_list))
  moments
}
