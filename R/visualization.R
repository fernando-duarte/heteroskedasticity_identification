#' Internal helper for creating boxplot visualizations
#'
#' Creates standardized boxplot for 2SLS estimates across different factors
#'
#' @param plot_data Data frame with plotting data
#' @param x_var Character. Name of x-axis variable
#' @param y_var Character. Name of y-axis variable
#' @param true_value Numeric. True parameter value for reference line
#' @param fill_color Character. Color for boxplot fill
#' @param title Character. Plot title
#' @param subtitle Character. Plot subtitle
#' @param x_label Character. X-axis label
#' @param y_label Character. Y-axis label
#' @return ggplot object
#' @keywords internal
.create_boxplot_visualization <- function(plot_data, x_var, y_var, true_value,
                                         fill_color, title, subtitle,
                                         x_label, y_label) {
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = factor(.data[[x_var]]), y = .data[[y_var]])
  ) +
    ggplot2::geom_boxplot(fill = fill_color, alpha = 0.7) +
    ggplot2::geom_hline(
      yintercept = true_value,
      linetype = "dashed",
      color = .hetid_const("plot$colors$REFERENCE_LINE")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = .hetid_const("PLOT_BASE_FONT_SIZE"))
}

#' Create Distribution Plot of Estimators
#'
#' Creates a density plot comparing the distributions of OLS and 2SLS
#' estimators.
#'
#' @param results_clean Data.frame. Cleaned simulation results.
#' @template param-config-viz
#'
#' @template return-ggplot
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
#' @importFrom stats setNames
#' @export
plot_estimator_distributions <- function(results_clean, config) {
  # Select and reshape data
  plot_data <- dplyr::select(results_clean, "ols_gamma1", "tsls_gamma1")
  plot_data_long <- tidyr::pivot_longer(plot_data,
    cols = dplyr::everything(),
    names_to = "Estimator", values_to = "Estimate"
  )
  plot_data_long <- dplyr::mutate(plot_data_long,
    Estimator = dplyr::case_when(
      .data$Estimator == .hetid_const("plot$labels$OLS_COLUMN") ~
        .hetid_const("plot$labels$OLS_BIASED"),
      .data$Estimator == .hetid_const("plot$labels$TSLS_COLUMN") ~
        .hetid_const("plot$labels$TSLS_LEWBEL"),
      TRUE ~ .data$Estimator
    )
  )

  # Create plot
  ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(x = .data$Estimate, fill = .data$Estimator)
  ) +
    ggplot2::geom_density(alpha = 0.7) +
    ggplot2::geom_vline(
      xintercept = config$gamma1,
      linetype = "dashed",
      color = .hetid_const("plot$colors$REFERENCE_LINE"),
      linewidth = .hetid_const("PLOT_LINE_WIDTH_THIN")
    ) +
    ggplot2::annotate("text",
      x = config$gamma1, y = Inf, label = paste("True =", config$gamma1),
      color = .hetid_const("plot$colors$REFERENCE_LINE"), angle = 90, vjust = 1.5, hjust = 1.1
    ) +
    ggplot2::labs(
      title = "Distribution of gamma1 Estimates",
      subtitle = sprintf(
        "N = %d, Replications = %d",
        config$main_sample_size, config$num_simulations
      ),
      x = "Estimated gamma1", y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = .hetid_const("PLOT_BASE_FONT_SIZE")) +
    ggplot2::scale_fill_manual(
      values = setNames(
        c(
          .hetid_const("plot$colors$OLS"),
          .hetid_const("plot$colors$TSLS")
        ),
        c(
          .hetid_const("plot$labels$OLS_BIASED"),
          .hetid_const("plot$labels$TSLS_LEWBEL")
        )
      )
    )
}


#' Create Sample Size Consistency Plot
#'
#' Creates a boxplot showing 2SLS estimate consistency across sample sizes.
#'
#' @param results_by_n Data.frame. Results from sample size analysis.
#' @template param-config-viz
#'
#' @template return-ggplot
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
  plot_data <- dplyr::filter(results_by_n, !is.na(.data$tsls_gamma1))

  # Use helper function
  .create_boxplot_visualization(
    plot_data = plot_data,
    x_var = "sample_size",
    y_var = "tsls_gamma1",
    true_value = config$gamma1,
    fill_color = .hetid_const("plot$colors$SAMPLE_SIZE_BOX"),
    title = "2SLS Consistency: Estimates by Sample Size",
    subtitle = "Estimates should concentrate around true value as N increases",
    x_label = "Sample Size",
    y_label = "2SLS Estimate of gamma1"
  )
}


#' Create Heteroscedasticity Sensitivity Plot
#'
#' Creates a boxplot showing 2SLS performance by heteroscedasticity strength.
#'
#' @param results_by_delta Data.frame. Results from sensitivity analysis.
#' @template param-config-viz
#'
#' @template return-ggplot
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
  plot_data <- dplyr::filter(results_by_delta, !is.na(.data$tsls_gamma1))

  # Use helper function
  .create_boxplot_visualization(
    plot_data = plot_data,
    x_var = "delta_het",
    y_var = "tsls_gamma1",
    true_value = config$gamma1,
    fill_color = .hetid_const("plot$colors$SENSITIVITY_BOX"),
    title = "2SLS Performance by Heteroscedasticity Strength",
    subtitle = "Stronger heteroscedasticity should improve precision",
    x_label = "Delta (Heteroscedasticity Parameter)",
    y_label = "2SLS Estimate of gamma1"
  )
}


#' Create First-Stage F Distribution Plot
#'
#' Creates a histogram of first-stage F-statistics with weak instrument
#' threshold.
#'
#' @param results_clean Data.frame. Cleaned simulation results.
#' @template param-config-optional
#' @param weak_iv_pct Numeric. Percentage of simulations with weak instruments.
#'
#' @template return-ggplot
#'
#' @examples
#' \dontrun{
#' config <- create_default_config()
#' seeds <- generate_all_seeds(config)
#' results <- run_main_simulation(config, seeds)
#' results_clean <- na.omit(results)
#' weak_iv_pct <- mean(results_clean$first_stage_F <
#'   .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD")) * 100
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
      weak_iv_pct <- mean(
        results_clean$first_stage_F <
          .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"),
        na.rm = TRUE
      ) * 100
    } else {
      weak_iv_pct <- 0
    }
  }

  # Ensure we have the first_stage_F column
  if (!"first_stage_F" %in% names(results_clean)) {
    warning("first_stage_F column not found in results_clean")
    return(NULL)
  }

  ggplot2::ggplot(results_clean, ggplot2::aes(x = .data$first_stage_F)) +
    ggplot2::geom_histogram(
      bins = .hetid_const("PLOT_HISTOGRAM_BINS"),
      fill = .hetid_const("plot$colors$FIRST_STAGE_HIST"), alpha = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"),
      linetype = "dashed", color = .hetid_const("plot$colors$REFERENCE_LINE")
    ) +
    ggplot2::annotate("text",
      x = .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"), y = Inf,
      label = paste("F =", .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD")),
      color = .hetid_const("plot$colors$REFERENCE_LINE"), angle = 90, vjust = 1.5, hjust = -0.5
    ) +
    ggplot2::labs(
      title = "Distribution of First-Stage F-Statistics",
      subtitle = sprintf(
        paste0(
          "%.1f%% have F < ", .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"),
          " (weak instrument threshold)"
        ),
        weak_iv_pct
      ),
      x = "First-Stage F-Statistic", y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = .hetid_const("PLOT_BASE_FONT_SIZE"))
}


#' Create Bootstrap Confidence Intervals Plot
#'
#' Creates a plot showing set identification bounds with bootstrap confidence
#' intervals.
#'
#' @param bootstrap_examples Data.frame. Bootstrap examples with standard
#'   errors.
#' @template param-config-viz
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
  if (nrow(bootstrap_examples) < .hetid_const("PLOT_MIN_BOOTSTRAP_THRESHOLD")) {
    return(NULL)
  }

  # Prepare data
  plot_data <- dplyr::slice_head(
    bootstrap_examples,
    n = .hetid_const("PLOT_BOOTSTRAP_DISPLAY_LIMIT")
  )
  plot_data <- dplyr::mutate(plot_data, sim_id_ordered = dplyr::row_number())

  # Create plot
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$bound_lower_tau_set -
          .hetid_const("Z_CRITICAL_95") * .data$bound_se_lower,
        xend = .data$bound_upper_tau_set +
          .hetid_const("Z_CRITICAL_95") * .data$bound_se_upper,
        y = .data$sim_id_ordered, yend = .data$sim_id_ordered
      ),
      color = .hetid_const("plot$colors$BOOTSTRAP_CI_BAND"),
      linewidth = .hetid_const("PLOT_LINE_WIDTH_THICK"),
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$bound_lower_tau_set, xend = .data$bound_upper_tau_set,
        y = .data$sim_id_ordered, yend = .data$sim_id_ordered
      ),
      color = .hetid_const("plot$colors$BOOTSTRAP_CI"), linewidth = .hetid_const("PLOT_LINE_WIDTH_NORMAL")
    ) +
    ggplot2::geom_vline(
      xintercept = config$gamma1, linetype = "dashed", color = .hetid_const("plot$colors$REFERENCE_LINE")
    ) +
    ggplot2::labs(
      title = paste0(
        "Set Identification Bounds with Bootstrap Confidence Intervals"
      ),
      subtitle = "Point estimates with 95% bootstrap confidence intervals",
      x = "gamma1", y = "Example"
    ) +
    ggplot2::theme_minimal(base_size = .hetid_const("PLOT_BASE_FONT_SIZE")) +
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
#' @template param-config-viz
#' @template param-verbose
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
  weak_iv_pct <- mean(
    results_clean$first_stage_F < .hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"),
    na.rm = TRUE
  ) * 100

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
