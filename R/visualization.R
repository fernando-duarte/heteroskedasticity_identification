#' Create Distribution Plot of Estimators
#'
#' Creates a density plot comparing the distributions of OLS and 2SLS
#' estimators.
#'
#' @param results_clean Data.frame. Cleaned simulation results.
#' @param config List. Configuration object containing true parameter values.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results <- run_main_simulation(config, seeds)
#' results_clean <- na.omit(results)
#' p1 <- plot_estimator_distributions(results_clean, config)
#' print(p1)
#' }
#'
#' @export
plot_estimator_distributions <- function(results_clean, config) {
  # Select and reshape data
  plot_data <- dplyr::select(results_clean, ols_gamma1, tsls_gamma1)
  plot_data_long <- tidyr::pivot_longer(plot_data,
    cols = dplyr::everything(),
    names_to = "Estimator", values_to = "Estimate"
  )
  plot_data_long <- dplyr::mutate(plot_data_long,
    Estimator = dplyr::recode(Estimator,
      "ols_gamma1" = "OLS (Biased)",
      "tsls_gamma1" = "2SLS (Lewbel)"
    )
  )

  # Create plot
  ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(x = Estimate, fill = Estimator)
  ) +
    ggplot2::geom_density(alpha = 0.7) +
    ggplot2::geom_vline(
      xintercept = config$gamma1,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    ggplot2::annotate("text",
      x = config$gamma1, y = Inf, label = paste("True =", config$gamma1),
      color = "red", angle = 90, vjust = 1.5, hjust = 1.1
    ) +
    ggplot2::labs(
      title = "Distribution of gamma1 Estimates",
      subtitle = sprintf(
        "N = %d, Replications = %d",
        config$main_sample_size, config$num_simulations
      ),
      x = "Estimated gamma1", y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::scale_fill_manual(
      values = c("OLS (Biased)" = "#d95f02", "2SLS (Lewbel)" = "#1b9e77")
    )
}


#' Create Sample Size Consistency Plot
#'
#' Creates a boxplot showing 2SLS estimate consistency across sample sizes.
#'
#' @param results_by_n Data.frame. Results from sample size analysis.
#' @param config List. Configuration object containing true parameter values.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_by_n <- run_sample_size_analysis(config, seeds)
#' p2 <- plot_sample_size_consistency(results_by_n, config)
#' print(p2)
#' }
#'
#' @export
plot_sample_size_consistency <- function(results_by_n, config) {
  # Filter data
  plot_data <- dplyr::filter(results_by_n, !is.na(tsls_gamma1))

  # Create plot
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = factor(sample_size), y = tsls_gamma1)
  ) +
    ggplot2::geom_boxplot(fill = "lightblue", alpha = 0.7) +
    ggplot2::geom_hline(
      yintercept = config$gamma1, linetype = "dashed", color = "red"
    ) +
    ggplot2::labs(
      title = "2SLS Consistency: Estimates by Sample Size",
      subtitle = paste0(
        "Estimates should concentrate around true value as N increases"
      ),
      x = "Sample Size", y = "2SLS Estimate of gamma1"
    ) +
    ggplot2::theme_minimal(base_size = 14)
}


#' Create Heteroscedasticity Sensitivity Plot
#'
#' Creates a boxplot showing 2SLS performance by heteroscedasticity strength.
#'
#' @param results_by_delta Data.frame. Results from sensitivity analysis.
#' @param config List. Configuration object containing true parameter values.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_by_delta <- run_sensitivity_analysis(config, seeds)
#' p3 <- plot_het_sensitivity(results_by_delta, config)
#' print(p3)
#' }
#'
#' @export
plot_het_sensitivity <- function(results_by_delta, config) {
  # Filter data
  plot_data <- dplyr::filter(results_by_delta, !is.na(tsls_gamma1))

  # Create plot
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = factor(delta_het), y = tsls_gamma1)
  ) +
    ggplot2::geom_boxplot(fill = "lightgreen", alpha = 0.7) +
    ggplot2::geom_hline(
      yintercept = config$gamma1, linetype = "dashed", color = "red"
    ) +
    ggplot2::labs(
      title = "2SLS Performance by Heteroscedasticity Strength",
      subtitle = "Stronger heteroscedasticity should improve precision",
      x = "Delta (Heteroscedasticity Parameter)",
      y = "2SLS Estimate of gamma1"
    ) +
    ggplot2::theme_minimal(base_size = 14)
}


#' Create First-Stage F Distribution Plot
#'
#' Creates a histogram of first-stage F-statistics with weak instrument
#' threshold.
#'
#' @param results_clean Data.frame. Cleaned simulation results.
#' @param weak_iv_pct Numeric. Percentage of simulations with weak instruments.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results <- run_main_simulation(config, seeds)
#' results_clean <- na.omit(results)
#' weak_iv_pct <- mean(results_clean$first_stage_F < 10) * 100
#' p4 <- plot_first_stage_f_dist(results_clean, weak_iv_pct)
#' print(p4)
#' }
#'
#' @export
plot_first_stage_f_dist <- function(results_clean,
                                    config = NULL,
                                    weak_iv_pct = NULL) {
  # Handle different calling patterns
  if (is.null(weak_iv_pct)) {
    # Calculate weak_iv_pct if not provided
    if ("first_stage_F" %in% names(results_clean)) {
      weak_iv_pct <- mean(results_clean$first_stage_F < 10, na.rm = TRUE) * 100
    } else {
      weak_iv_pct <- 0
    }
  }

  # Ensure we have the first_stage_F column
  if (!"first_stage_F" %in% names(results_clean)) {
    warning("first_stage_F column not found in results_clean")
    return(NULL)
  }

  ggplot2::ggplot(results_clean, ggplot2::aes(x = first_stage_F)) +
    ggplot2::geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 10, linetype = "dashed", color = "red") +
    ggplot2::annotate("text",
      x = 10, y = Inf, label = "F = 10",
      color = "red", angle = 90, vjust = 1.5, hjust = -0.5
    ) +
    ggplot2::labs(
      title = "Distribution of First-Stage F-Statistics",
      subtitle = sprintf(
        "%.1f%% have F < 10 (weak instrument threshold)",
        weak_iv_pct
      ),
      x = "First-Stage F-Statistic", y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 14)
}


#' Create Bootstrap Confidence Intervals Plot
#'
#' Creates a plot showing set identification bounds with bootstrap confidence
#' intervals.
#'
#' @param bootstrap_examples Data.frame. Bootstrap examples with standard
#'   errors.
#' @param config List. Configuration object containing true parameter values.
#'
#' @return A ggplot2 object or NULL if insufficient data.
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_main <- run_main_simulation(config, seeds)
#' bootstrap_demo <- run_bootstrap_demonstration(config, seeds)
#' bootstrap_examples <- analyze_bootstrap_results(
#'   results_main, bootstrap_demo, config
#' )
#' p5 <- plot_bootstrap_ci(bootstrap_examples, config)
#' if (!is.null(p5)) print(p5)
#' }
#'
#' @export
plot_bootstrap_ci <- function(bootstrap_examples, config) {
  if (nrow(bootstrap_examples) < 5) {
    return(NULL)
  }

  # Prepare data
  plot_data <- dplyr::slice_head(bootstrap_examples, n = 20)
  plot_data <- dplyr::mutate(plot_data, sim_id_ordered = dplyr::row_number())

  # Create plot
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = bound_lower_tau_set - 1.96 * bound_se_lower,
        xend = bound_upper_tau_set + 1.96 * bound_se_upper,
        y = sim_id_ordered, yend = sim_id_ordered
      ),
      color = "lightgray", linewidth = 3, alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = bound_lower_tau_set, xend = bound_upper_tau_set,
        y = sim_id_ordered, yend = sim_id_ordered
      ),
      color = "steelblue", linewidth = 2
    ) +
    ggplot2::geom_vline(
      xintercept = config$gamma1, linetype = "dashed", color = "red"
    ) +
    ggplot2::labs(
      title = paste0(
        "Set Identification Bounds with Bootstrap Confidence Intervals"
      ),
      subtitle = paste0(
        "Blue: point estimates, Gray: 95% CIs based on bootstrap SEs"
      ),
      x = "gamma1", y = "Example"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}


#' Generate All Simulation Plots
#'
#' Generates all visualization plots for the Lewbel simulation results.
#'
#' @param results_main Data.frame. Main simulation results.
#' @param results_by_n Data.frame. Sample size analysis results.
#' @param results_by_delta Data.frame. Sensitivity analysis results.
#' @param bootstrap_examples Data.frame. Bootstrap examples.
#' @param config List. Configuration object.
#' @param verbose Logical. Whether to print plots (default: TRUE).
#'
#' @return A list of ggplot2 objects.
#'
#' @examples
#' \dontrun{
#' # Run full simulation
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results_main <- run_main_simulation(config, seeds)
#' results_by_n <- run_sample_size_analysis(config, seeds)
#' results_by_delta <- run_sensitivity_analysis(config, seeds)
#' bootstrap_demo <- run_bootstrap_demonstration(config, seeds)
#' bootstrap_examples <- analyze_bootstrap_results(
#'   results_main, bootstrap_demo, config
#' )
#'
#' plots <- generate_all_plots(
#'   results_main, results_by_n, results_by_delta,
#'   bootstrap_examples, config
#' )
#' }
#'
#' @export
generate_all_plots <- function(results_main,
                               results_by_n,
                               results_by_delta,
                               bootstrap_examples,
                               config,
                               verbose = TRUE) {
  if (verbose) {
    cat("\nGenerating enhanced plots...\n")
  }

  results_clean <- stats::na.omit(results_main)
  weak_iv_pct <- mean(results_clean$first_stage_F < 10, na.rm = TRUE) * 100

  # Generate all plots
  plots <- list(
    distributions = plot_estimator_distributions(results_clean, config),
    sample_size = plot_sample_size_consistency(results_by_n, config),
    sensitivity = plot_het_sensitivity(results_by_delta, config),
    first_stage_f = plot_first_stage_f_dist(
      results_clean,
      weak_iv_pct = weak_iv_pct
    ),
    bootstrap_ci = plot_bootstrap_ci(bootstrap_examples, config)
  )

  # Print plots if requested
  if (verbose) {
    print(plots$distributions)
    print(plots$sample_size)
    print(plots$sensitivity)
    print(plots$first_stage_f)
    if (!is.null(plots$bootstrap_ci)) {
      print(plots$bootstrap_ci)
    }
  }

  plots
}
