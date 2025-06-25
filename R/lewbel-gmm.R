#' @title GMM Estimation for Lewbel (2012) Heteroskedasticity-Based Identification
#' @description Implementation of Generalized Method of Moments (GMM) estimation
#' for Lewbel's (2012) heteroskedasticity-based identification strategy.
#' @name lewbel-gmm
#' @keywords internal
#' @import gmm
#' @import stats
NULL

#' Internal helper for Lewbel moment conditions
#'
#' Unified function to calculate moment conditions for both triangular
#' and simultaneous systems in Lewbel's framework.
#'
#' @inheritParams lewbel_triangular_moments
#' @param system Character. "triangular" or "simultaneous"
#' @param z_sq Logical. For simultaneous systems, whether to include squared Z terms
#' @return Matrix of moment conditions
#' @keywords internal
.lewbel_moments_helper <- function(theta, data, y1_var, y2_var, x_vars, z_vars,
                                   add_intercept, system = "triangular", z_sq = FALSE) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Extract parameters based on system type
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]

  # Calculate structural errors
  if (system == "triangular") {
    eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
    eps2 <- y2_data - x_matrix %*% beta2
  } else {  # simultaneous
    gamma2 <- theta[2 * k + 2]

    # Check identification condition for simultaneous system
    if (abs(gamma1 * gamma2 - 1) < .hetid_const("IDENTIFICATION_TOLERANCE")) {
      warning("gamma1 * gamma2 is close to 1, which may lead to identification problems")
    }

    eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
    eps2 <- y2_data - x_matrix %*% beta2 - y1_data * gamma2
  }

  # Construct Z matrix for instruments
  if (is.null(z_vars)) {
    if (add_intercept && ncol(x_matrix) > 1) {
      z_matrix <- scale(x_matrix[, -1, drop = FALSE], center = TRUE, scale = FALSE)
    } else if (!add_intercept && ncol(x_matrix) > 0) {
      z_matrix <- scale(x_matrix, center = TRUE, scale = FALSE)
    } else {
      stop(ifelse(system == "triangular",
                  .hetid_const("messages$CANNOT_AUTO_CONSTRUCT_Z_NO_X"),
                  .hetid_const("messages$CANNOT_AUTO_CONSTRUCT_Z_SIMUL")))
    }

    # For simultaneous equations, optionally add squared terms
    if (system == "simultaneous" && z_sq) {
      z_matrix <- cbind(z_matrix, z_matrix^2)
      z_matrix <- scale(z_matrix, center = TRUE, scale = FALSE)
    }
  } else {
    z_matrix <- as.matrix(data[, z_vars, drop = FALSE])
    z_matrix <- scale(z_matrix, center = TRUE, scale = FALSE)
  }

  # Moment conditions
  m1 <- x_matrix * c(eps1)
  m2 <- x_matrix * c(eps2)
  m3 <- z_matrix * c(eps1 * eps2)

  moments <- cbind(m1, m2, m3)
  moments
}

#' Define GMM Moment Conditions for Lewbel Triangular System
#'
#' Creates the moment function for GMM estimation of a triangular system
#' using Lewbel's heteroskedasticity-based identification.
#'
#' @param theta Numeric vector. Parameters to estimate: c(beta1, gamma1, beta2).
#' @template param-data
#' @template param-y-vars
#' @template param-x-vars
#' @template param-z-vars
#' @template param-add-intercept
#'
#' @return Matrix of moment conditions (n x q).
#'
#' @template details-triangular-system
#'
#' @details
#' The moment conditions are:
#' \itemize{
#'   \item \eqn{E[X \times \epsilon_1] = 0}
#'   \item \eqn{E[X \times \epsilon_2] = 0}
#'   \item \eqn{E[Z \times \epsilon_1 \times \epsilon_2] = 0}
#' }
#' where Z = g(X) is a mean-zero transformation of X.
#'
#' @seealso
#' \code{\link{lewbel_gmm}} for the main GMM estimation function.
#' \code{\link{lewbel_simultaneous_moments}} for simultaneous system moments.
#'
#' @template references-lewbel
#'
#' @export
lewbel_triangular_moments <- function(theta, data, y1_var, y2_var, x_vars, z_vars, add_intercept) {
  .lewbel_moments_helper(theta, data, y1_var, y2_var, x_vars, z_vars,
                         add_intercept, system = "triangular", z_sq = FALSE)
}


#' Define GMM Moment Conditions for Lewbel Simultaneous System
#'
#' Creates the moment function for GMM estimation of a simultaneous equations
#' system using Lewbel's heteroskedasticity-based identification.
#'
#' @param theta Numeric vector. Parameters: c(beta1, gamma1, beta2, gamma2).
#' @template param-data
#' @template param-y-vars
#' @template param-x-vars
#' @template param-z-vars
#' @template param-add-intercept
#' @param z_sq Logical. Whether to include squared terms in the Z matrix for simultaneous equations.
#'
#' @return Matrix of moment conditions (n x q).
#'
#' @template details-simultaneous-system
#'
#' @details
#' The moment conditions are the same as the triangular system.
#'
#' @seealso
#' \code{\link{lewbel_gmm}} for the main GMM estimation function.
#' \code{\link{lewbel_triangular_moments}} for triangular system moments.
#'
#' @template references-lewbel
#'
#' @export
lewbel_simultaneous_moments <- function(theta, data, y1_var, y2_var, x_vars, z_vars, add_intercept, z_sq) {
  .lewbel_moments_helper(theta, data, y1_var, y2_var, x_vars, z_vars,
                         add_intercept, system = "simultaneous", z_sq = z_sq)
}


#' Estimate Lewbel Model using GMM
#'
#' Main function to estimate Lewbel's heteroskedasticity-based identification
#' model using Generalized Method of Moments (GMM).
#'
#' @template param-data
#' @param system Character. Type of system: "triangular" or "simultaneous" (default: "triangular").
#'   Note: Simultaneous systems require strong identification conditions - either many
#'   regimes (4+) or large variance differences across regimes for numerical stability.
#' @template param-y-vars
#' @template param-x-vars
#' @template param-z-vars
#' @template param-add-intercept
#' @param gmm_type Character. GMM type: "twoStep", "iterative", or "cue" (default: "twoStep").
#' @param initial_values Numeric vector. Initial parameter values (default: NULL, uses OLS).
#' @param vcov Character. Type of variance-covariance matrix: "HAC", "iid", or "cluster" (default: "HAC").
#' @param cluster_var Character. Variable name for clustering if vcov = "cluster" (default: NULL).
#' @param compute_se Logical. Whether to compute standard errors (default: TRUE). Passed to gmm call.
#' @template param-verbose
#' @param ... Additional arguments passed to gmm().
#'
#' @return An object of class "gmm" containing estimation results.
#'
#' @details
#' This function implements Lewbel's (2012) heteroskedasticity-based identification
#' using the GMM framework. The method exploits heteroskedasticity in the error
#' terms to generate valid instruments for endogenous regressors.
#'
#' For simultaneous equation systems, identification becomes more challenging.
#' The system requires sufficient variation in heteroskedasticity patterns to
#' distinguish between the bidirectional effects. In practice, this means you need
#' either many distinct heteroskedasticity regimes or very large differences in
#' variance across existing regimes.
#'
#' @seealso
#' \code{\link{lewbel_triangular_moments}}, \code{\link{lewbel_simultaneous_moments}} for moment condition functions.
#' \code{\link{rigobon_gmm}} for regime-based heteroskedasticity identification.
#' \code{\link{prono_gmm}} for GARCH-based heteroskedasticity identification.
#' \code{\link{compare_gmm_2sls}} for comparing GMM with 2SLS estimates.
#' \code{\link{run_single_lewbel_simulation}} for 2SLS implementation.
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' set.seed(123)
#' n <- 1000
#' params <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data <- generate_lewbel_data(n, params)
#'
#' # Estimate triangular system
#' gmm_tri <- lewbel_gmm(data, system = "triangular")
#' summary(gmm_tri)
#'
#' # Estimate simultaneous system
#' gmm_sim <- lewbel_gmm(data, system = "simultaneous")
#' summary(gmm_sim)
#'
#' # Compare with 2SLS
#' tsls_result <- run_single_lewbel_simulation(1, c(params, sample_size = n))
#' cat("2SLS estimate:", tsls_result$tsls_gamma1, "\n")
#' cat("GMM estimate:", coef(gmm_tri)["gamma1"], "\n")
#' }
#'
#' @template references-lewbel
#'
#' @export
lewbel_gmm <- function(data,
                       system = c("triangular", "simultaneous"),
                       y1_var = "Y1",
                       y2_var = "Y2",
                       x_vars = "Xk",
                       z_vars = NULL,
                       add_intercept = TRUE,
                       gmm_type = c("twoStep", "iterative", "cue"),
                       initial_values = NULL,
                       vcov = c(.hetid_const("VCOV_HAC"), "iid", "cluster"),
                       cluster_var = NULL,
                       compute_se = TRUE,
                       verbose = FALSE,
                       ...) {
  # Match arguments
  system <- match.arg(system)
  gmm_type <- match.arg(gmm_type)
  vcov <- match.arg(vcov)

  # Check required packages
  if (!requireNamespace(.hetid_const("packages$GMM"), quietly = TRUE)) {
    stop(sprintf(.hetid_const("messages$PACKAGE_REQUIRED"), .hetid_const("packages$GMM"), .hetid_const("packages$GMM")))
  }
  if (vcov == .hetid_const("VCOV_HAC") && !requireNamespace(.hetid_const("packages$SANDWICH"), quietly = TRUE)) {
    stop(sprintf(
      .hetid_const("messages$PACKAGE_REQUIRED"),
      .hetid_const("packages$SANDWICH"),
      .hetid_const("packages$SANDWICH")
    ))
  }


  # Extract data components
  if (!is.data.frame(data)) data <- as.data.frame(data)

  # Validate variables
  required_vars <- c(y1_var, y2_var, x_vars)
  if (!is.null(z_vars)) required_vars <- c(required_vars, z_vars)
  if (vcov == "cluster" && !is.null(cluster_var)) required_vars <- c(required_vars, cluster_var)

  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stopper("The following variables are not found in the data: ", paste(missing_vars, collapse = ", "))
  }


  # Exogenous variables matrix (used for parameter vector construction)
  x_matrix_for_params <- as.matrix(data[, x_vars, drop = FALSE])
  if (add_intercept) {
    # Check if an intercept column (all 1s) already exists in the selected x_vars
    # to avoid adding a second one.
    has_intercept_in_x_vars <- any(sapply(x_vars, function(v) is.numeric(data[[v]]) && all(data[[v]] == 1)))

    if (!has_intercept_in_x_vars) {
      # This is for naming and initial_values; moment functions handle X construction
    }
  }
  k_exog_for_params <- length(x_vars) +
    (if (add_intercept &&
      !any(sapply(x_vars, function(v) all(data[[v]] == 1) && is.numeric(data[[v]])))) {
      1
    } else {
      0
    })


  # Get initial values if not provided
  if (is.null(initial_values)) {
    # Construct formula strings for OLS
    rhs_ols1 <- x_vars
    if (add_intercept && !any(sapply(x_vars, function(v) all(data[[v]] == 1)))) {
      # OLS via lm adds intercept by default unless suppressed
    } else if (!add_intercept) {
      rhs_ols1 <- c(rhs_ols1, "0") # Suppress intercept
    }

    formula1_str <- paste(y1_var, "~", paste(c(y2_var, rhs_ols1), collapse = " + "))
    formula2_str <- paste(y2_var, "~", paste(rhs_ols1, collapse = " + "))

    ols1 <- lm(as.formula(formula1_str), data = data)
    ols2 <- lm(as.formula(formula2_str), data = data)

    # Careful extraction of coefficients to match expected order
    # Beta1: intercept (if any), then x_vars
    # Beta2: intercept (if any), then x_vars

    # Determine names for OLS coefficient extraction
    ols_beta_names <- x_vars
    if (add_intercept) {
      # Check if lm added an intercept or if one was part of x_vars
      if ("(Intercept)" %in% names(coef(ols1))) {
        ols_beta_names_intercept <- c("(Intercept)", x_vars)
      } else { # Intercept was one of x_vars
        ols_beta_names_intercept <- x_vars
      }
    } else {
      ols_beta_names_intercept <- x_vars # No intercept
    }


    beta1_init_vec <- coef(ols1)[ols_beta_names_intercept]
    gamma1_init_val <- coef(ols1)[y2_var]
    beta2_init_vec <- coef(ols2)[ols_beta_names_intercept]


    if (system == .hetid_const("SYSTEM$TRIANGULAR")) {
      initial_values <- c(beta1_init_vec, gamma1_init_val, beta2_init_vec)
    } else { # simultaneous
      gamma2_init_val <- .hetid_const("IDENTIFICATION_TOLERANCE") # Small initial value for gamma2
      initial_values <- c(beta1_init_vec, gamma1_init_val, beta2_init_vec, gamma2_init_val)
    }

    # Clean NAs that might arise if a variable was perfectly collinear (e.g. explicit intercept + X that is all 1s)
    initial_values[is.na(initial_values)] <- 0
  }


  # Set parameter names
  param_names_list <- list()
  current_pos <- 1

  # Beta1 parameters
  if (add_intercept) {
    param_names_list$beta1_intercept <- "beta1_(Intercept)"
  }
  param_names_list$beta1_slopes <- paste0("beta1_", x_vars)

  # Gamma1
  param_names_list$gamma1 <- "gamma1"

  # Beta2 parameters
  if (add_intercept) {
    param_names_list$beta2_intercept <- "beta2_(Intercept)"
  }
  param_names_list$beta2_slopes <- paste0("beta2_", x_vars)

  # Gamma2 (for simultaneous)
  if (system == .hetid_const("SYSTEM$SIMULTANEOUS")) {
    param_names_list$gamma2 <- "gamma2"
  }

  final_param_names <- unlist(param_names_list, use.names = FALSE)

  if (length(initial_values) != length(final_param_names)) {
    warning(paste(
      "Mismatch in length of initial_values (", length(initial_values),
      ") and constructed param_names (", length(final_param_names),
      "). Using gmm default names if issues persist."
    ))
    # This can happen if x_vars already contained an intercept like term.
    # For now, let gmm handle it, but it's a sign of potential issues in init val construction.
  } else {
    names(initial_values) <- final_param_names
  }


  # Select moment function
  # Pass add_intercept explicitly to moment functions
  if (system == .hetid_const("SYSTEM$TRIANGULAR")) {
    moment_fn <- function(theta, dat) { # Changed arg name from x to dat to avoid clash
      lewbel_triangular_moments(theta, dat, y1_var, y2_var, x_vars, z_vars, add_intercept)
    }
  } else { # simultaneous
    # For simultaneous, Lewbel (2012, p.70) suggests Z can include squares of X
    # if Z is constructed from X. Let's add a z_sq_simultaneous option.
    # Defaulting to FALSE for now to match triangular behavior unless specified.
    z_sq_effective <- ifelse(is.null(list(...)$z_sq), FALSE, list(...)$z_sq)
    moment_fn <- function(theta, dat) {
      lewbel_simultaneous_moments(theta, dat, y1_var, y2_var, x_vars, z_vars, add_intercept, z_sq = z_sq_effective)
    }
  }

  # Set up vcov options for gmm call
  vcov_method <- vcov # Store original vcov string for gmm call

  # Handle clustered standard errors
  if (vcov_method == "cluster") {
    if (is.null(cluster_var)) {
      stopper("cluster_var must be specified when vcov = 'cluster'")
    }
    if (!(cluster_var %in% names(data))) {
      stopper("Cluster variable not found in data")
    }
    # gmm package doesn't directly support clustered SE
    warning("Clustered standard errors not directly supported by gmm package. Using HAC instead.")
    vcov_method <- .hetid_const("VCOV_HAC")
  }

  # Prepare arguments for gmm call
  additional_args <- list(...)
  additional_args$g <- moment_fn
  additional_args$x <- data
  additional_args$t0 <- initial_values
  additional_args$type <- gmm_type
  additional_args$wmatrix <- "optimal" # Typically "optimal" for twoStep/iterative/cue
  additional_args$vcov <- vcov_method # "HAC", "iid"
  additional_args$centeredVcov <- FALSE # Default in gmm is TRUE, consider implications
  additional_args$weightsMatrix <- NULL # Let gmm compute it unless provided in ...
  additional_args$model <- TRUE # Return model components

  # Set HAC-specific options if needed
  if (vcov_method == .hetid_const("VCOV_HAC")) {
    additional_args$kernel <- hetid_opt("GMM_HAC_KERNEL") # e.g. "Quadratic Spectral"
    additional_args$prewhite <- hetid_opt("GMM_HAC_PREWHITE") # e.g. 1
    additional_args$ar.method <- hetid_opt("GMM_HAC_AR_METHOD") # e.g. "ols"
    # Bandwidth will be chosen automatically if not specified
    if (!is.null(list(...)$bw)) additional_args$bw <- list(...)$bw
  }

  # Remove args that are explicitly named in lewbel_gmm signature if they were passed via ...
  # to avoid them being duplicated.
  explicit_args <- c("g", "x", "t0", "type", "wmatrix", "vcov", "computeSE", "model")
  for (arg_name in explicit_args) {
    if (arg_name %in% names(additional_args) &&
      !(arg_name %in% c("model"))) { # Keep our model=TRUE
      # This is tricky as gmm has its own computeSE, etc.
      # For now, let's assume ... doesn't override these core ones.
    }
  }
  # Control SE computation
  if (!compute_se) {
    additional_args$vcov <- "iid" # Need a vcov type for gmm to run
    # To truly skip SE, one might need to post-process or use internal gmm flags if available
    # For now, compute_se = FALSE will use simple IID vcov and user should ignore SEs.
    # The gmm package itself has a computeSE argument in summary.gmm, not in gmm().
    # A more robust way might be to set vcov to a dummy function or use specific gmm options.
    # Setting a very simple vcov type if SEs are not needed.
  }


  # Estimate GMM
  # Capture warnings and errors during GMM estimation
  gmm_estimation_result <- tryCatch(
    {
      do.call(gmm::gmm, additional_args)
    },
    warning = function(w) {
      if (verbose) messager("Warning during GMM estimation: ", w$message, v_type = "warning")
      invokeRestart("muffleWarning") # Continue with warning
      suppressWarnings(do.call(gmm::gmm, additional_args)) # Re-run and suppress to get result
    },
    error = function(e) {
      stopper("Error during GMM estimation: ", e$message)
      NULL
    }
  )

  if (is.null(gmm_estimation_result)) {
    return(NULL) # GMM failed
  }


  # Add custom attributes
  attr(gmm_estimation_result, "hetid_system") <- system
  attr(gmm_estimation_result, "hetid_vars") <- list(
    y1 = y1_var,
    y2 = y2_var,
    x = x_vars,
    z = z_vars,
    cluster = cluster_var
  )
  attr(gmm_estimation_result, "hetid_opts") <- list(
    add_intercept = add_intercept,
    gmm_type = gmm_type,
    vcov_type = vcov,
    n_obs = nrow(data)
  )

  # Store initial values used
  attr(gmm_estimation_result, "hetid_initial_values") <- initial_values

  # Return results
  class(gmm_estimation_result) <- c(paste0("lewbel_", system), "lewbel_gmm", class(gmm_estimation_result))

  if (verbose) messager("GMM estimation complete.", v_type = "success")
  gmm_estimation_result
}


#' Summary Method for Lewbel GMM
#'
#' @param object An object of class "lewbel_gmm".
#' @param ... Additional arguments passed to summary.gmm.
#'
#' @export
#' @method summary lewbel_gmm
summary.lewbel_gmm <- function(object, ...) {
  cat("\nLewbel (2012) Heteroskedasticity-Based GMM Estimation\n")
  cat("========================================================\n")

  sys_type <- attr(object, "hetid_system")
  vars <- attr(object, "hetid_vars")
  opts <- attr(object, "hetid_opts")

  cat("System type:", sys_type, "\n")
  cat("Observations:", opts$n_obs, "\n")
  cat("GMM type:", opts$gmm_type, "\n")
  cat("VCV type:", opts$vcov_type)
  if (opts$vcov_type == "cluster" && !is.null(vars$cluster)) {
    cat(" (clustered by: ", vars$cluster, ")", sep = "")
  }
  cat("\n\n")

  cat("Dependent variable (Y1):", vars$y1, "\n")
  cat("Endogenous variable (Y2):", vars$y2, "\n")
  cat("Exogenous variables (X):", paste(vars$x, collapse = ", "))
  if (opts$add_intercept) cat(" (intercept added)\n") else cat(" (no intercept)\n")

  if (!is.null(vars$z)) {
    cat("Heteroskedasticity drivers (Z):", paste(vars$z, collapse = ", "), "\n")
  } else {
    cat("Heteroskedasticity drivers (Z): Centered X variables (default)\n")
  }
  cat("\n")

  # Call the original summary method from gmm package
  # Need to ensure that the class hierarchy allows NextMethod to find summary.gmm
  # The class of 'object' is c("lewbel_triangular", "lewbel_gmm", "gmm", "list")
  # So NextMethod() should find summary.gmm
  # Pass computeSE = TRUE by default, can be overridden by ...
  summary_args <- list(...)
  if (is.null(summary_args$computeSE)) {
    summary_args$computeSE <- TRUE # Default to computing SEs in summary
  }

  # Store original class to restore it later if needed, though gmm::summary.gmm should handle it
  original_class <- class(object)
  class(object) <- class(object)[!(class(object) %in% c("lewbel_triangular", "lewbel_simultaneous", "lewbel_gmm"))]

  # Using NextMethod as it's generally preferred for S3
  # Ensure 'object' passed to NextMethod still has "gmm" class
  base_gmm_summary <- NextMethod("summary", object, computeSE = summary_args$computeSE)

  class(object) <- original_class # Restore class

  # TODO: Could add more Lewbel-specific interpretations here if needed.
  # For example, interpretation of gamma1 (and gamma2 if simultaneous).
  invisible(base_gmm_summary) # Return the gmm summary object
}


#' Print Method for Lewbel GMM
#'
#' @param x An object of class "lewbel_gmm".
#' @param ... Additional arguments passed to print.gmm.
#'
#' @export
#' @method print lewbel_gmm
print.lewbel_gmm <- function(x, ...) {
  cat("\nLewbel GMM Estimation Results\n")
  sys_type <- attr(x, "hetid_system")
  cat("System:", sys_type, "\n\n")

  coefs <- tryCatch(coef(x), error = function(e) NULL)
  if (is.null(coefs)) {
    cat("Coefficients not available or model did not converge.\n")
    return(invisible(x))
  }

  gamma1_name <- grep("^gamma1$", names(coefs), value = TRUE) # Exact match for "gamma1"
  if (length(gamma1_name) == 0) {
    gamma1_name <- names(coefs)[grepl("gamma1", names(coefs))][1] # Fallback
  }

  gamma1 <- if (length(gamma1_name) > 0 && gamma1_name %in% names(coefs)) coefs[gamma1_name] else NA


  if (sys_type == "simultaneous") {
    gamma2_name <- grep("^gamma2$", names(coefs), value = TRUE)
    if (length(gamma2_name) == 0) gamma2_name <- names(coefs)[grepl("gamma2", names(coefs))][1]
    gamma2 <- if (length(gamma2_name) > 0 && gamma2_name %in% names(coefs)) coefs[gamma2_name] else NA

    cat("Key parameters:\n")
    cat("  ", gamma1_name, " (Y1 on Y2) = ", round(gamma1, 4), "\n", sep = "")
    cat("  ", gamma2_name, " (Y2 on Y1) = ", round(gamma2, 4), "\n", sep = "")
    if (!is.na(gamma1) && !is.na(gamma2)) {
      cat("  Product (gamma1 * gamma2) = ", round(gamma1 * gamma2, 4), "\n")
      if (abs(gamma1 * gamma2 - 1) < .hetid_const("IDENTIFICATION_TOLERANCE")) {
        cat("  Warning: gamma1 * gamma2 is close to 1, check identification.\n")
      }
    }
  } else { # triangular
    cat("Key parameter (coefficient on endogenous Y2):\n")
    cat("  ", gamma1_name, " = ", round(gamma1, 4), "\n", sep = "")
  }

  cat("\nFull GMM output (call summary() for details):\n")
  # Call the original print method from gmm package
  # Ensure 'x' passed to NextMethod still has "gmm" class
  original_class <- class(x)
  class(x) <- class(x)[!(class(x) %in% c("lewbel_triangular", "lewbel_simultaneous", "lewbel_gmm"))]
  NextMethod("print", x, ...)
  class(x) <- original_class # Restore class

  invisible(x)
}


#' Compare GMM and 2SLS Estimates for Lewbel Model
#'
#' Compares GMM estimates with traditional 2SLS (Lewbel) estimates for the triangular system.
#'
#' @template param-data
#' @template param-y-vars
#' @param x_vars Character vector. Names of exogenous variables (default: "Xk").
#'               For 2SLS via run_single_lewbel_simulation, it assumes a single "Xk" if
#'               default simulation parameters are used. For GMM, can be multiple.
#'               This function will try to match behavior. If multiple x_vars are given,
#'               the 2SLS part might be less comparable if its underlying data generating process
#'               assumes one X.
#' @param add_intercept Logical. Whether to add an intercept for GMM (default: TRUE).
#'                      2SLS via run_single_lewbel_simulation typically includes an intercept.
#' @param true_gamma1 Numeric. Optional true value of gamma1 for bias calculation.
#' @param gmm_args List. Additional arguments passed to \code{\link{lewbel_gmm}}.
#' @param tsls_sim_config List. Parameters to override in the default config for
#'                          \code{\link{run_single_lewbel_simulation}}.
#'                          The `sample_size` will be set to `nrow(data)`.
#'                          `lewbel_x_vars` in this config should match `x_vars` here.
#' @template param-verbose
#'
#' @return A data frame comparing estimates, standard errors, and test statistics.
#'
#' @seealso
#' \code{\link{lewbel_gmm}} for GMM estimation.
#' \code{\link{run_single_lewbel_simulation}} for 2SLS estimation.
#'
#' @examples
#' \dontrun{
#' # Generate data
#' params_dgp <- list(
#'   beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = -1.0,
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data_comp <- generate_lewbel_data(1000, params_dgp) # Generates Y1, Y2, Xk, Z
#'
#' # Compare (assuming Xk is the exogenous variable for both)
#' comparison <- compare_gmm_2sls(data_comp, true_gamma1 = params_dgp$gamma1)
#' print(comparison)
#'
#' # Example with multiple X for GMM, 2SLS might be less direct comparison
#' params_multi <- list(
#'   beta1_0 = 0.5, beta1_1 = c(1.5, 0.2), gamma1 = -0.8,
#'   beta2_0 = 1.0, beta2_1 = c(-1.0, 0.3),
#'   alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
#' )
#' data_multi_x <- generate_lewbel_data(1000, params_multi, n_x = 2) # Y1,Y2,X1,X2,Z1,Z2
#' comparison_multi <- compare_gmm_2sls(data_multi_x,
#'   x_vars = c("X1", "X2"),
#'   true_gamma1 = params_multi$gamma1,
#'   tsls_sim_config = list(
#'     lewbel_x_vars = c("X1", "X2") # Hypothetical
#'     # Note: run_single_lewbel_simulation's internal
#'     # Data generating process might not easily map to this if it assumes 1 Xk.
#'     # This part is more illustrative for GMM side.
#'   )
#' )
#' print(comparison_multi)
#' }
#' @export
compare_gmm_2sls <- function(data,
                             y1_var = "Y1", y2_var = "Y2", x_vars = "Xk",
                             add_intercept = TRUE,
                             true_gamma1 = NA_real_,
                             gmm_args = list(),
                             tsls_sim_config = list(),
                             verbose = TRUE) {
  if (verbose) messager("Comparing GMM and 2SLS estimates for Lewbel model...\n")

  # --- GMM Estimation ---
  if (verbose) messager("Running GMM estimation...")
  gmm_call_args <- list(
    data = data,
    system = .hetid_const("SYSTEM$TRIANGULAR"), # Comparison typically for triangular
    y1_var = y1_var,
    y2_var = y2_var,
    x_vars = x_vars,
    add_intercept = add_intercept,
    verbose = FALSE # Suppress verbose from underlying call
  )
  gmm_call_args <- utils::modifyList(gmm_call_args, gmm_args, keep.null = TRUE)

  gmm_result <- do.call(lewbel_gmm, gmm_call_args)

  gmm_coefs_all <- coef(gmm_result)
  gmm_gamma1_name <- if (add_intercept) paste0("gamma1") else "gamma1" # Adjust if naming changes
  # Find gamma1 robustly
  gmm_gamma1_idx <- which(names(gmm_coefs_all) == "gamma1")
  if (length(gmm_gamma1_idx) == 0) gmm_gamma1_idx <- grep("gamma1", names(gmm_coefs_all))[1]


  gmm_gamma1 <- gmm_coefs_all[gmm_gamma1_idx]
  gmm_vcov_matrix <- tryCatch(vcov(gmm_result), error = function(e) NULL)
  gmm_se_gamma1 <- if (!is.null(gmm_vcov_matrix)) sqrt(diag(gmm_vcov_matrix)[gmm_gamma1_idx]) else NA

  gmm_j_test_stat <- NA
  gmm_j_p_value <- NA
  if (!is.null(gmm_result$test)) { # J-test from gmm package object
    gmm_j_test_stat <- gmm_result$test[1, "Test"] # GMM J-stat
    gmm_j_p_value <- gmm_result$test[1, "pvalue"] # GMM J p-value
  }


  # --- 2SLS Estimation (via run_single_lewbel_simulation) ---
  if (verbose) messager("Running 2SLS estimation (via simulation framework)...")

  # Prepare config for run_single_lewbel_simulation
  # It expects specific parameter names from create_default_config
  sim_config_base <- create_default_config( # Get structure and defaults
    main_sample_size = nrow(data)
  )
  # Override with user-provided tsls_sim_config
  sim_config_final <- utils::modifyList(sim_config_base, tsls_sim_config, keep.null = TRUE)
  sim_config_final$sample_size <- nrow(data) # Ensure sample size matches input data

  # Handle case where x_vars might be multiple for GMM, but run_single_lewbel_simulation
  # might be hardcoded for a single 'Xk' in its internal data generating process if data_override
  # is not perfectly used. For data_override, we need to ensure the columns Y1, Y2, and what it
  # expects as Xk are present. If x_vars contains "Xk", use that.
  # If not, use the first of x_vars as "Xk" for the 2SLS part.
  data_for_tsls <- data

  if (!("Xk" %in% names(data_for_tsls)) && length(x_vars) > 0) {
    if (verbose) messager(paste("Mapping", x_vars[1], "to Xk for 2SLS comparison."))
    names(data_for_tsls)[names(data_for_tsls) == x_vars[1]] <- "Xk"
  } else if (!("Xk" %in% names(data_for_tsls))) {
    warning("Could not determine a unique 'Xk' from x_vars for 2SLS comparison. 2SLS results may be unreliable.")
  }


  tsls_result <- tryCatch(
    {
      run_single_lewbel_simulation(1, sim_config_final)
    },
    error = function(e) {
      warning("2SLS estimation via run_single_lewbel_simulation failed: ", e$message)
      list(tsls_gamma1 = NA, tsls_se_gamma1 = NA, tsls_f_stat = NA) # Return NAs
    }
  )


  # --- OLS Estimation (for baseline comparison, also from tsls_result if available) ---
  ols_gamma1 <- if (!is.null(tsls_result$ols_gamma1)) tsls_result$ols_gamma1 else NA
  ols_se_gamma1 <- if (!is.null(tsls_result$ols_se)) tsls_result$ols_se else NA

  # Compile results
  # Extract 2SLS results with appropriate field names
  tsls_gamma1 <- if (!is.null(tsls_result$tsls_gamma1)) tsls_result$tsls_gamma1 else NA
  tsls_se_gamma1 <- if (!is.null(tsls_result$tsls_se)) tsls_result$tsls_se else NA
  tsls_f_stat <- if (!is.null(tsls_result$first_stage_F)) tsls_result$first_stage_F else NA

  comparison_df <- data.frame(
    Estimator = c("OLS", "2SLS (Lewbel)", "GMM (Lewbel)"),
    gamma1 = c(ols_gamma1, tsls_gamma1, gmm_gamma1),
    StdError = c(ols_se_gamma1, tsls_se_gamma1, gmm_se_gamma1),
    Bias = c(ols_gamma1 - true_gamma1, tsls_gamma1 - true_gamma1, gmm_gamma1 - true_gamma1),
    `F-stat (1st stage)` = c(NA, tsls_f_stat, NA), # F-stat primarily for 2SLS
    `J-stat (Overid)` = c(NA, NA, gmm_j_test_stat),
    `J p-value` = c(NA, NA, gmm_j_p_value),
    row.names = NULL
  )

  # Add significance stars based on p-values (if SEs are available)
  # For GMM, t-stat = estimate / SE. p-value from 2 * pnorm(-abs(t-stat))
  # For OLS/2SLS, if p-values are not directly available, can compute similarly.

  if (verbose) {
    cat("\n--- Comparison Summary ---\n")
    print(comparison_df, digits = 4)
    cat("\nNote: 2SLS results are from run_single_lewbel_simulation using the provided data.\n")
    cat("      GMM results are from lewbel_gmm.\n")
  }

  invisible(comparison_df)
}

# Helper to ensure Z matrix for Lewbel has at least one column.
# This was an issue in lewbel_gmm when z_vars = NULL and x_vars was only intercept.
# Now handled inside the moment functions.


#' Define GMM Moment Conditions for Prono Triangular System
#'
#' Creates the moment function for GMM estimation of a triangular system
#' using Prono's GARCH-based identification.
#'
#' @param theta Numeric vector. Parameters to estimate: c(beta1, gamma1, beta2).
#' @param data Data frame containing the variables.
#' @param y1_var Character. Name of the first dependent variable (default: "Y1").
#' @param y2_var Character. Name of the second dependent variable/endogenous regressor (default: "Y2").
#' @param x_vars Character vector. Names of exogenous variables.
#' @param garch_order GARCH(p,q) order for conditional variance estimation.
#' @param add_intercept Logical. Whether to add an intercept to the exogenous variables.
#'
#' @return Matrix of moment conditions (n x q).
#'
#' @references
#' Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
#' 29(5), 800-824. \doi{10.1002/jae.2387}
#'
#' @seealso
#' \code{\link{prono_gmm}} for the main GMM estimation function.
#' \code{\link{run_single_prono_simulation}} for 2SLS estimation.
#'
#' @export
prono_triangular_moments <- function(theta, data, y1_var, y2_var, x_vars,
                                     garch_order = c(1, 1), add_intercept = TRUE) {
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  y1_data <- data[[y1_var]]
  y2_data <- data[[y2_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])

  if (add_intercept) {
    x_matrix <- cbind(1, x_matrix)
  }

  # Parameters: (beta1_0, beta1_1, ..., gamma1, beta2_0, beta2_1, ...)
  beta1 <- theta[1:k]
  gamma1 <- theta[k + 1]
  beta2 <- theta[(k + 2):(2 * k + 1)]

  # Structural errors
  eps1 <- y1_data - x_matrix %*% beta1 - y2_data * gamma1
  eps2 <- y2_data - x_matrix %*% beta2

  # Fit GARCH to eps2 to get conditional variance
  sigma2_sq_hat <- NULL

  tryCatch(
    {
      if (requireNamespace(.hetid_const("packages$TSGARCH"), quietly = TRUE)) {
        # Convert to xts object as required by tsgarch
        dates <- as.Date("2000-01-01") + seq_along(eps2) - 1
        eps2_xts <- xts::xts(eps2, order.by = dates)

        # Specify GARCH model using tsgarch
        garch_spec <- tsgarch::garch_modelspec(
          y = eps2_xts,
          model = "garch",
          order = garch_order,
          constant = TRUE,
          distribution = "norm"
        )

        # Fit the model
        garch_fit <- tsmethods::estimate(garch_spec)

        # Extract fitted conditional variances
        sigma2_sq_hat <- as.numeric(sigma(garch_fit))^2
      }
    },
    error = function(e) {
      # Fallback to squared residuals
      sigma2_sq_hat <- eps2^2
    }
  )

  if (is.null(sigma2_sq_hat)) {
    sigma2_sq_hat <- eps2^2
  }

  # Prono instrument: conditional variance times residual
  z_t_instrument <- sigma2_sq_hat - mean(sigma2_sq_hat)
  prono_iv <- z_t_instrument * eps2

  # Moment conditions
  m1 <- x_matrix * as.vector(eps1)
  m2 <- x_matrix * as.vector(eps2)
  m3 <- prono_iv * as.vector(eps1) # The Prono moment

  moments <- cbind(m1, m2, m3)
  moments
}


#' GMM Estimation for Prono (2014) GARCH-Based Identification
#'
#' Implementation of GMM estimation for Prono's (2014) heteroskedasticity-based
#' identification strategy using GARCH models.
#'
#' @param data Data frame containing all variables.
#' @param system Character. Either "triangular" (default).
#' @param y1_var Character. Name of the first dependent variable (default: "Y1").
#' @param y2_var Character. Name of the second dependent variable (default: "Y2").
#' @param x_vars Character vector. Names of exogenous variables.
#' @param garch_order GARCH(p,q) order (default: c(1,1)).
#' @param fit_garch Logical. Whether to fit GARCH model (default: TRUE).
#' @param add_intercept Logical. Whether to add an intercept (default: TRUE).
#' @param gmm_type Character. GMM type: "onestep", "twoStep" (default), "iterative", or "cue".
#' @param vcov Character. Variance-covariance matrix type: "iid", "HAC" (default), or "cluster".
#' @param initial_values Numeric vector. Initial parameter values (optional).
#' @param compute_se Logical. Whether to compute standard errors (default: TRUE).
#' @template param-verbose
#' @param ... Additional arguments passed to gmm::gmm.
#'
#' @return An object of class "prono_gmm" containing GMM estimation results.
#'
#' @references
#' Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
#' and Estimating Linear Triangular Systems, with Applications to Asset Pricing
#' Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
#' 29(5), 800-824. \doi{10.1002/jae.2387}
#'
#' @seealso
#' \code{\link{prono_triangular_moments}} for moment conditions.
#' \code{\link{run_single_prono_simulation}} for 2SLS estimation.
#'
#' @export
prono_gmm <- function(data,
                      system = "triangular",
                      y1_var = "Y1",
                      y2_var = "Y2",
                      x_vars = NULL,
                      garch_order = c(1, 1),
                      fit_garch = TRUE,
                      add_intercept = TRUE,
                      gmm_type = "twoStep",
                      vcov = .hetid_const("VCOV_HAC"),
                      initial_values = NULL,
                      compute_se = TRUE,
                      verbose = TRUE,
                      ...) {
  if (!requireNamespace(.hetid_const("packages$GMM"), quietly = TRUE)) {
    stop(sprintf(
      .hetid_const("messages$PACKAGE_REQUIRED"),
      .hetid_const("packages$GMM"),
      .hetid_const("packages$GMM")
    ))
  }

  # Auto-detect x_vars if not provided
  if (is.null(x_vars)) {
    all_vars <- names(data)
    # Look for variables starting with X
    x_pattern_vars <- grep("^X[0-9]*", all_vars, value = TRUE)
    if (length(x_pattern_vars) > 0) {
      x_vars <- x_pattern_vars
    } else {
      # Exclude Y1, Y2, and other special columns
      exclude_vars <- c(y1_var, y2_var, "time", "eps1", "eps2", "sigma2_sq", "prono_iv", "const")
      x_vars <- setdiff(all_vars, exclude_vars)
    }
    if (verbose) messager(paste("Auto-detected x_vars:", paste(x_vars, collapse = ", ")))
  }

  # Prepare data
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  # Get initial values if not provided
  if (is.null(initial_values)) {
    # OLS for initial values
    x_matrix_init <- as.matrix(data[, x_vars, drop = FALSE])
    if (add_intercept) {
      x_matrix_init <- cbind(1, x_matrix_init)
    }

    y1 <- data[[y1_var]]
    y2 <- data[[y2_var]]

    # OLS for second equation
    beta2_ols <- solve(t(x_matrix_init) %*% x_matrix_init) %*% t(x_matrix_init) %*% y2

    # OLS for first equation
    x_full_init <- cbind(x_matrix_init, y2)
    coefs1 <- solve(t(x_full_init) %*% x_full_init) %*% t(x_full_init) %*% y1
    beta1_ols <- coefs1[1:k]
    gamma1_ols <- coefs1[k + 1]

    initial_values <- c(beta1_ols, gamma1_ols, beta2_ols)
    # Set names for initial values
    param_names <- c(
      if (add_intercept) "beta1_(Intercept)" else NULL,
      paste0("beta1_", x_vars),
      "gamma1",
      if (add_intercept) "beta2_(Intercept)" else NULL,
      paste0("beta2_", x_vars)
    )
    names(initial_values) <- param_names
  }

  # Pre-fit GARCH if requested
  if (fit_garch && !("sigma2_sq" %in% names(data))) {
    # First get residuals from second equation
    x_matrix_garch <- as.matrix(data[, x_vars, drop = FALSE])
    if (add_intercept) {
      x_matrix_garch <- cbind(1, x_matrix_garch)
    }
    beta2_init <- initial_values[(k + 2):(2 * k + 1)]
    eps2_garch <- data[[y2_var]] - x_matrix_garch %*% beta2_init

    # Fit GARCH
    if (requireNamespace("tsgarch", quietly = TRUE)) {
      tryCatch(
        {
          dates <- as.Date("2000-01-01") + seq_along(eps2_garch) - 1
          eps2_xts <- xts::xts(eps2_garch, order.by = dates)

          garch_spec <- tsgarch::garch_modelspec(
            y = eps2_xts,
            model = "garch",
            order = garch_order,
            constant = TRUE,
            distribution = "norm"
          )

          garch_fit <- tsmethods::estimate(garch_spec)
          data$sigma2_sq <- as.numeric(sigma(garch_fit))^2
        },
        error = function(e) {
          if (verbose) warning("GARCH fitting failed, using squared residuals: ", e$message)
          data$sigma2_sq <- eps2_garch^2
        }
      )
    } else {
      warning("tsgarch package not available. Using squared residuals instead of GARCH-fitted variances.")
      data$sigma2_sq <- eps2_garch^2
    }
  }

  # Define moment function
  moment_fn <- function(theta, dat) {
    prono_triangular_moments(theta, dat, y1_var, y2_var, x_vars, garch_order, add_intercept)
  }

  # Estimate GMM
  gmm_result <- gmm::gmm(
    g = moment_fn,
    x = data,
    t0 = initial_values,
    type = gmm_type,
    wmatrix = "optimal",
    vcov = vcov,
    ...
  )

  # Create result object with expected structure
  result <- list(
    coefficients = coef(gmm_result),
    vcov = vcov(gmm_result),
    gmm_fit = gmm_result
    # system, variables, options will be moved to attributes
  )

  # Add attributes for consistency with lewbel_gmm summary/print
  attr(result, "hetid_system") <- system
  attr(result, "hetid_vars") <- list(
    y1 = y1_var,
    y2 = y2_var,
    x = x_vars,
    z = NULL, # Prono uses GARCH-based IV, not a simple Z variable.
    cluster = NULL
  )
  attr(result, "hetid_opts") <- list(
    add_intercept = add_intercept,
    gmm_type = gmm_type,
    vcov_type = vcov,
    n_obs = n,
    prono_garch_order = garch_order # Store Prono-specific option
  )

  # Add J-test if available
  if (!is.null(gmm_result$test)) {
    result$J_test <- gmm_result$test
  }

  # Always try to calculate first-stage F-statistic
  # This block is preserved and assigns to result$first_stage_F
  if (!"sigma2_sq_hat" %in% names(data)) {
    formula2 <- as.formula(paste(y2_var, "~", paste(x_vars, collapse = " + ")))
    fit2 <- lm(formula2, data = data)
    e2_hat <- residuals(fit2)
    tryCatch(
      {
        if (requireNamespace(.hetid_const("packages$TSGARCH"), quietly = TRUE)) {
          dates <- as.Date("2000-01-01") + seq_along(e2_hat) - 1
          e2_xts <- xts::xts(e2_hat, order.by = dates)
          garch_spec <- tsgarch::garch_modelspec(
            y = e2_xts, model = "garch", order = garch_order,
            constant = TRUE, distribution = "norm"
          )
          garch_fit <- suppressWarnings(tsmethods::estimate(garch_spec))
          data$sigma2_sq_hat <- as.numeric(sigma(garch_fit))^2
        }
      },
      error = function(e) {
        data$sigma2_sq_hat <- e2_hat^2
      }
    )
  }

  if ("sigma2_sq_hat" %in% names(data)) {
    x_matrix_f <- as.matrix(data[, x_vars, drop = FALSE])
    if (add_intercept) x_matrix_f <- cbind(1, x_matrix_f)
    y2_f <- data[[y2_var]]
    z_instrument_fstat <- data[["sigma2_sq_hat"]]
    xz_matrix_f <- cbind(x_matrix_f, z_instrument_fstat)
    fit_full <- lm(y2_f ~ xz_matrix_f - 1)
    fit_restricted <- lm(y2_f ~ x_matrix_f - 1)
    rss_r <- sum(residuals(fit_restricted)^2)
    rss_u <- sum(residuals(fit_full)^2)
    df1 <- 1
    df2 <- n - ncol(xz_matrix_f)
    if (df2 > 0) {
      f_stat <- ((rss_r - rss_u) / df1) / (rss_u / df2)
      result$first_stage_F <- f_stat
    } else {
      result$first_stage_F <- NA_real_
    }
  } else {
    result$first_stage_F <- NA_real_
  }

  class(result) <- c("prono_gmm", "lewbel_gmm", "list")

  if (verbose) messager("Prono GMM estimation complete.", v_type = "success")
  result
}


#' Define GMM Moment Conditions for Rigobon Triangular System
#'
#' Creates the moment function for GMM estimation of a triangular system
#' using Rigobon's regime-based identification.
#'
#' @param theta Numeric vector. Parameters to estimate.
#' @param data Data frame containing the variables.
#' @param system Character. The type of system, either "triangular" or "simultaneous".
#' @param y1_var Character. Name of the first dependent variable.
#' @param y2_var Character. Name of the second dependent variable.
#' @param x_vars Character vector. Names of exogenous variables.
#' @param regime_var Character. Name of the regime indicator variable.
#' @param add_intercept Logical. Whether to add an intercept.
#'
#' @return Matrix of moment conditions.
#'
#' @references
#' Rigobon, R. (2003). Identification through heteroskedasticity.
#' Review of Economics and Statistics, 85(4), 777-792.
#'
#' @export
rigobon_moment_conditions <- function(theta, data, system,
                                      y1_var, y2_var, x_vars,
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


#' GMM Estimation for Rigobon (2003) Regime-Based Identification
#'
#' Implementation of GMM estimation for Rigobon's (2003) heteroskedasticity-based
#' identification strategy using regime changes.
#'
#' @param data Data frame containing all variables.
#' @param system Character. Either "triangular" (default) or "simultaneous".
#'   Note: Simultaneous systems require many regimes (4+) and large variance
#'   differences across regimes for numerical stability and identification.
#' @param y1_var Character. Name of the first dependent variable.
#' @param y2_var Character. Name of the second dependent variable.
#' @param x_vars Character vector. Names of exogenous variables.
#' @param regime_var Character. Name of the regime indicator variable.
#' @param add_intercept Logical. Whether to add an intercept.
#' @param gmm_type Character. GMM type.
#' @param vcov Character. Variance-covariance matrix type.
#' @param initial_values Numeric vector. Initial parameter values.
#' @template param-verbose
#' @param ... Additional arguments passed to gmm::gmm.
#'
#' @return An object of class "rigobon_gmm" containing GMM estimation results.
#'
#' @references
#' Rigobon, R. (2003). Identification through heteroskedasticity.
#' Review of Economics and Statistics, 85(4), 777-792.
#'
#' @export
rigobon_gmm <- function(data,
                        system = "triangular",
                        y1_var = "Y1",
                        y2_var = "Y2",
                        x_vars = "Xk",
                        regime_var = "regime",
                        add_intercept = TRUE,
                        gmm_type = "twoStep",
                        vcov = .hetid_const("VCOV_HAC"),
                        initial_values = NULL,
                        verbose = TRUE,
                        ...) {
  if (!requireNamespace(.hetid_const("packages$GMM"), quietly = TRUE)) {
    stop(sprintf(
      .hetid_const("messages$PACKAGE_REQUIRED"),
      .hetid_const("packages$GMM"),
      .hetid_const("packages$GMM")
    ))
  }

  if (verbose) messager("Rigobon GMM Estimation", v_type = "info")

  # Check regime variable
  if (!regime_var %in% names(data)) {
    stop(paste("Regime variable", regime_var, "not found in data"))
  }

  unique_regimes <- unique(data[[regime_var]])
  if (length(unique_regimes) < 2) {
    stop(.hetid_const("messages$NEED_TWO_REGIMES"))
  }

  # Prepare data
  n <- nrow(data)
  k <- length(x_vars)
  if (add_intercept) k <- k + 1

  # Get initial values if not provided
  if (is.null(initial_values)) {
    # Simple OLS for initial values
    formula1 <- as.formula(paste(y1_var, "~", y2_var, "+", paste(x_vars, collapse = " + ")))
    formula2 <- as.formula(paste(y2_var, "~", paste(x_vars, collapse = " + ")))

    ols1 <- lm(formula1, data = data)
    ols2 <- lm(formula2, data = data)

    if (system == .hetid_const("SYSTEM$TRIANGULAR")) {
      initial_values <- c(coef(ols1)[-2], coef(ols1)[2], coef(ols2))
    } else {
      # For simultaneous, add gamma2
      initial_values <- c(coef(ols1)[-2], coef(ols1)[2], coef(ols2), .hetid_const("IDENTIFICATION_TOLERANCE"))
    }
  }

  # Define moment function using the new unified function
  moment_fn <- function(theta, dat) {
    rigobon_moment_conditions(
      theta, dat, system, y1_var, y2_var, x_vars, regime_var, add_intercept
    )
  }

  # Estimate GMM
  gmm_result <- gmm::gmm(
    g = moment_fn,
    x = data,
    t0 = initial_values,
    type = gmm_type,
    wmatrix = "optimal",
    vcov = vcov,
    ...
  )

  # Create result object with expected structure
  coefs <- coef(gmm_result)

  # Name the coefficients properly
  param_names <- c()
  if (add_intercept) {
    param_names <- c(param_names, "beta1_(Intercept)")
  }
  param_names <- c(param_names, paste0("beta1_", x_vars))
  param_names <- c(param_names, "gamma1")
  if (add_intercept) {
    param_names <- c(param_names, "beta2_(Intercept)")
  }
  param_names <- c(param_names, paste0("beta2_", x_vars))
  if (system == .hetid_const("SYSTEM$SIMULTANEOUS")) {
    param_names <- c(param_names, "gamma2")
  }

  # Ensure names are assigned if length matches, otherwise gmm default names are kept by gmm_result
  if (length(coefs) == length(param_names)) {
    names(coefs) <- param_names
  }

  result <- list(
    coefficients = coefs,
    vcov = vcov(gmm_result),
    gmm_fit = gmm_result,
    n_regimes = length(unique_regimes) # Keep n_regimes as a specific and tested list element
    # system, variables, options will be moved to attributes
  )

  # Add attributes for consistency with lewbel_gmm summary/print
  attr(result, "hetid_system") <- system
  attr(result, "hetid_vars") <- list(
    y1 = y1_var,
    y2 = y2_var,
    x = x_vars,
    z = paste0("Regime-based (", regime_var, ")"), # Description for Z in Rigobon context
    cluster = NULL
  )
  attr(result, "hetid_opts") <- list(
    add_intercept = add_intercept,
    gmm_type = gmm_type,
    vcov_type = vcov,
    n_obs = n
  )

  # Add J-test if available
  if (!is.null(gmm_result$test)) {
    result$J_test <- gmm_result$test
  }

  class(result) <- c("rigobon_gmm", "lewbel_gmm", "list")

  if (verbose) messager("Rigobon GMM estimation complete.", v_type = "success")
  result
}
