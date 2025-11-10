#' Create All Publication Figures
#'
#' Generate and save all 14 publication-ready figures for HMC vs MH comparison.
#'
#' @param data_dir Character string. Path to directory containing combined datasets.
#'   Default: "outputs/combined_results"
#' @param output_dir Character string. Path to directory where figures will be saved.
#'   Default: "outputs/figures"
#' @param width Numeric. Default figure width in inches. Default: 8
#' @param height Numeric. Default figure height in inches. Default: 6
#' @param dpi Numeric. Resolution in dots per inch. Default: 320
#' @param formats Character vector. Output formats ("png", "pdf"). Default: "png"
#'
#' @return Invisibly returns a named list of ggplot objects
#'
#' @details
#' Creates the following figures:
#' \itemize{
#'   \item fig1a_rhat_ecdf.png - R-hat ECDFs by sample size
#'   \item fig1b_ess_ridges.png - ESS distributions by scenario
#'   \item fig2_coverage.png - Coverage of 95% CIs with MC uncertainty
#'   \item fig3a_bias.png - Bias by parameter and sample size
#'   \item fig3b_rmse.png - RMSE by parameter and sample size
#'   \item fig4_ess_per_sec.png - Sampling efficiency
#'   \item fig6_survival_cells.png - Weighted survival curves by design cell
#'   \item fig7_means_agreement.png - Agreement of posterior means
#'   \item figA1_cis_vs_truth.png - Credible intervals vs true value
#'   \item figA2_ci_matrix.png - CI matrix by parameter/sample size/method
#'   \item figB1_precision_tradeoff.png - Precision-coverage trade-off
#'   \item figB2_coverage_bias_heatmap.png - Global bias in coverage
#'   \item figB3_ci_width_violin.png - Distribution of CI widths
#' }
#'
#' @examples
#' \dontrun{
#' # Save all figures with defaults
#' plots <- save_all_figures()
#'
#' # Custom settings
#' plots <- save_all_figures(
#'   data_dir = "my_outputs/combined",
#'   output_dir = "my_figures",
#'   width = 10,
#'   height = 8,
#'   dpi = 300
#' )
#' }
#'
#' @export
save_all_figures <- function(
  data_dir = "outputs/combined_results",
  output_dir = "outputs/figures",
  width = 8,
  height = 6,
  dpi = 320,
  formats = "png"
) {
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("Package 'ggridges' is required but not installed.")
  }
  if (!requireNamespace("tidybayes", quietly = TRUE)) {
    stop("Package 'tidybayes' is required but not installed.")
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("Creating publication figures...\n\n")

  # Prepare data
  plot_data <- prepare_plot_data(data_dir)

  # Create all figures
  plots <- list()

  cat("  Creating Figure 1a: R-hat ECDFs...\n")
  plots$fig1a <- create_figure1a_rhat_ecdf(plot_data)
  save_figure(
    plots$fig1a,
    file.path(output_dir, "fig1a_rhat_ecdf"),
    width = 9,
    height = 3.2,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 1b: ESS ridges...\n")
  plots$fig1b <- create_figure1b_ess_ridges(plot_data)
  save_figure(
    plots$fig1b,
    file.path(output_dir, "fig1b_ess_ridges"),
    width = 9,
    height = 3.2,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 2: Coverage...\n")
  plots$fig2 <- create_figure2_coverage(plot_data)
  save_figure(
    plots$fig2,
    file.path(output_dir, "fig2_coverage"),
    width = 10,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 3a: Bias...\n")
  plots$fig3a <- create_figure3a_bias(plot_data)
  save_figure(
    plots$fig3a,
    file.path(output_dir, "fig3a_bias"),
    width = 9,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 3b: RMSE...\n")
  plots$fig3b <- create_figure3b_rmse(plot_data)
  save_figure(
    plots$fig3b,
    file.path(output_dir, "fig3b_rmse"),
    width = 9,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 3c: Bias–RMSE trade-off...\n")
  plots$fig3c <- create_figure3c_bias_rmse_tradeoff(plot_data)
  save_figure(
    plots$fig3c,
    file.path(output_dir, "fig3c_bias_rmse_tradeoff"),
    width = 9,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 4: ESS per second...\n")
  plots$fig4 <- create_figure4_ess_per_sec(plot_data)
  save_figure(
    plots$fig4,
    file.path(output_dir, "fig4_ess_per_sec"),
    width = 9,
    height = 3.2,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 5b: Speed-up heatmap...\n")
  plots$fig5b <- create_figure5b_speedup_heatmap(plot_data)
  save_figure(
    plots$fig5b,
    file.path(output_dir, "fig5b_speedup_heatmap"),
    width = 8,
    height = 6,
    dpi = dpi,
    formats = formats
  )
  cat("  Creating Figure 5c: Paired median slope plot...\n")
  plots$fig5c <- create_figure5c_paired_slopes(plot_data)
  save_figure(
    plots$fig5c,
    file.path(output_dir, "fig5c_runtime_slope"),
    width = 9,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 6: Survival cells (this may take a few minutes)...\n")
  plots$fig6 <- create_figure6_survival_cells()
  save_figure(
    plots$fig6,
    file.path(output_dir, "fig6_survival_cells"),
    width = 12,
    height = 16,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure 7: Means agreement...\n")
  plots$fig7 <- create_figure7_means_agreement(plot_data)
  save_figure(
    plots$fig7,
    file.path(output_dir, "fig7_means_agreement"),
    width = 9,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure A1: CIs vs truth...\n")
  plots$figA1 <- create_figureA1_cis_vs_truth(plot_data)
  save_figure(
    plots$figA1,
    file.path(output_dir, "figA1_cis_vs_truth"),
    width = 11,
    height = 7,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure A2: CI matrix...\n")
  plots$figA2 <- create_figureA2_ci_matrix(plot_data)
  save_figure(
    plots$figA2,
    file.path(output_dir, "figA2_ci_matrix"),
    width = 10,
    height = 18,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure B1: Precision trade-off...\n")
  plots$figB1 <- create_figureB1_precision_tradeoff(plot_data)
  save_figure(
    plots$figB1,
    file.path(output_dir, "figB1_precision_tradeoff"),
    width = 10,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure B2: Coverage heatmap...\n")
  plots$figB2 <- create_figureB2_coverage_heatmap(plot_data)
  save_figure(
    plots$figB2,
    file.path(output_dir, "figB2_coverage_bias_heatmap"),
    width = 10,
    height = 5,
    dpi = dpi,
    formats = formats
  )

  cat("  Creating Figure B3: CI width violin...\n")
  plots$figB3 <- create_figureB3_ci_width_violin(plot_data)
  save_figure(
    plots$figB3,
    file.path(output_dir, "figB3_ci_width_violin"),
    width = 9,
    height = 6,
    dpi = dpi,
    formats = formats
  )

  cat("\nAll figures saved to:", output_dir, "\n")

  invisible(plots)
}

# Helper Functions -------------------------------------------------------------

#' Prepare plot data from combined results
#'
#' @param data_dir Path to data directory
#' @return List with prepared data frames for plotting
#' @keywords internal
prepare_plot_data <- function(data_dir) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(forcats)
    library(tidyr)
  })

  # Load data
  res <- read.csv(file.path(data_dir, "combined_summaries.csv"))
  diagnostics <- read.csv(file.path(data_dir, "combined_diagnostics.csv"))

  # Standardize method labels
  std_method <- function(x) toupper(as.character(x))

  # Process summaries
  res <- res %>%
    as_tibble() %>%
    rename(
      parameter = variable,
      ess_bulk = ess,
      weight = weight_type,
      runtime_summary = total_time
    ) %>%
    filter(parameter %in% c("alpha", "beta", "gamma")) %>%
    mutate(method = std_method(method))

  # Process diagnostics
  diagnostics <- diagnostics %>%
    as_tibble() %>%
    rename(
      weight = weight_type,
      ess_tail = min_ess_tail,
      runtime_diag = total_time_sec
    ) %>%
    mutate(method = std_method(method)) %>%
    select(
      method,
      scenario_id,
      replicate,
      n_obs,
      censoring,
      weight,
      ess_tail,
      ess_per_sec,
      runtime_diag
    )

  # Merge and process
  res <- res %>%
    left_join(
      diagnostics,
      by = c(
        "method",
        "scenario_id",
        "replicate",
        "n_obs",
        "censoring",
        "weight"
      )
    ) %>%
    mutate(
      runtime_s = coalesce(runtime_diag, runtime_summary),
      ess_per_sec = coalesce(
        ess_per_sec,
        if_else(
          !is.na(runtime_s) & runtime_s > 0,
          ess_bulk / runtime_s,
          NA_real_
        )
      ),
      bias = error,
      rmse = sqrt(squared_error)
    ) %>%
    select(-runtime_diag, -runtime_summary)

  # Factor conversions
  res <- res %>%
    mutate(
      n_obs = factor(
        n_obs,
        levels = c(200, 2000, 10000),
        labels = c("n = 200", "n = 2000", "n = 10000")
      ),
      weight = factor(weight, levels = c("none", "low", "high")),
      censoring = factor(
        censoring,
        levels = c(0.1, 0.3, 0.5),
        labels = c("C=0.1", "C=0.3", "C=0.5")
      ),
      method = factor(std_method(method), levels = c("HMC", "MH")),
      scenario = interaction(censoring, weight, sep = ", ")
    )

  list(res = res)
}

#' Get standard color palette
#' @keywords internal
get_palette <- function() {
  c(HMC = "#2C7FB8", MH = "#F28E2B")
}

#' Get standard theme
#' @keywords internal
get_theme_sci <- function() {
  ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Extract the primary plotting tibble from prepared data
#' @keywords internal
get_plot_res <- function(plot_data) {
  res <- plot_data$res
  if (is.list(res) && !inherits(res, "data.frame")) {
    res <- res[[1]]
  }
  res
}

#' Summarise bias and RMSE for beta_1 across design cells
#' @keywords internal
compute_beta_metrics <- function(plot_data) {
  res <- get_plot_res(plot_data)

  res %>%
    dplyr::filter(parameter == "beta", !is.na(bias), !is.na(rmse)) %>%
    dplyr::mutate(
      sample_size = readr::parse_number(as.character(n_obs)),
      sample_size = factor(
        sample_size,
        levels = c(200, 2000, 10000),
        ordered = TRUE
      ),
      sample_size_label = forcats::fct_inorder(as.character(sample_size)),
      weight_regime = forcats::fct_relevel(weight, "none", "low", "high"),
      weight_regime = forcats::fct_recode(
        weight_regime,
        "No Weights" = "none",
        "Low Dispersion" = "low",
        "High Dispersion" = "high"
      ),
      censoring_level = forcats::fct_relevel(
        censoring,
        "C=0.1",
        "C=0.3",
        "C=0.5"
      ),
      censoring_level = forcats::fct_recode(
        censoring_level,
        "10% censoring" = "C=0.1",
        "30% censoring" = "C=0.3",
        "50% censoring" = "C=0.5"
      ),
      method = forcats::fct_relevel(method, "HMC", "MH")
    ) %>%
    dplyr::group_by(
      sample_size,
      sample_size_label,
      weight_regime,
      censoring_level,
      method
    ) %>%
    dplyr::summarise(
      mean_bias = mean(bias, na.rm = TRUE),
      bias_mcse = stats::sd(bias, na.rm = TRUE) / sqrt(dplyr::n()),
      mean_rmse = mean(rmse, na.rm = TRUE),
      rmse_mcse = stats::sd(rmse, na.rm = TRUE) / sqrt(dplyr::n()),
      median_ci_width = stats::median(ci_width, na.rm = TRUE),
      ci_width_mcse = stats::sd(ci_width, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      abs_bias = abs(mean_bias)
    )
}

#' Save figure in multiple formats
#'
#' @param plot ggplot object
#' @param filename Base filename (without extension)
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution
#' @param formats Character vector of formats ("png", "pdf")
#' @keywords internal
save_figure <- function(
  plot,
  filename,
  width = 8,
  height = 6,
  dpi = 320,
  formats = "png"
) {
  for (fmt in formats) {
    file_path <- paste0(filename, ".", fmt)
    ggplot2::ggsave(
      filename = file_path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }
  invisible(NULL)
}

# Individual Figure Functions --------------------------------------------------

#' Create Figure 1a: R-hat ECDF by sample size
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure1a_rhat_ecdf <- function(plot_data) {
  facet_labels <- plot_data$res %>%
    dplyr::filter(!is.na(rhat)) %>%
    dplyr::group_by(n_obs) %>%
    dplyr::summarise(
      pct_below = mean(rhat <= 1.01, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      label = sprintf(
        "%s \u2264 1.01",
        scales::percent(pct_below, accuracy = 0.1)
      ),
      x = 1.019,
      y = 0.88
    )

  ggplot2::ggplot(plot_data$res, ggplot2::aes(rhat, colour = method)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = 0.6) +
    ggplot2::geom_hline(
      yintercept = 0.95,
      linetype = "dotted",
      colour = "grey65",
      linewidth = 0.4
    ) +
    ggplot2::geom_vline(
      xintercept = 1.005,
      linetype = "dashed",
      colour = "grey70",
      linewidth = 0.4,
      alpha = 0.6
    ) +
    ggplot2::geom_vline(
      xintercept = 1.01,
      linetype = 2,
      colour = "grey30",
      linewidth = 0.6
    ) +
    ggplot2::geom_text(
      data = facet_labels,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      size = 3
    ) +
    ggplot2::facet_wrap(~n_obs, nrow = 1) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "R-hat",
      y = "ECDF",
      colour = "Method",
      title = "R-hat ECDFs by sample size",
      subtitle = "Vertical dashed lines mark thresholds at 1.005 and 1.01"
    ) +
    ggplot2::coord_cartesian(xlim = c(1.000, 1.020), ylim = c(0, 1)) +
    get_theme_sci()
}

#' Create Figure 1b: ESS ridges by scenario
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure1b_ess_ridges <- function(plot_data) {
  ess_data <- plot_data$res %>%
    dplyr::filter(!is.na(ess_bulk), ess_bulk > 0)

  median_points <- ess_data %>%
    dplyr::group_by(n_obs, scenario, method) %>%
    dplyr::summarise(
      median_ess = stats::median(ess_bulk, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(
    ess_data,
    ggplot2::aes(x = ess_bulk, y = scenario, fill = method, colour = method)
  ) +
    ggplot2::geom_vline(
      xintercept = 400,
      linetype = 2,
      colour = "grey40",
      linewidth = 0.5
    ) +
    ggridges::geom_density_ridges(
      alpha = 0.55,
      scale = 0.75,
      linewidth = 0.25,
      rel_min_height = 0.01
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(median_points, method == "HMC"),
      mapping = ggplot2::aes(x = median_ess, y = scenario, colour = method),
      inherit.aes = FALSE,
      position = ggplot2::position_nudge(y = 0.11),
      size = 1.8,
      shape = 16,
      stroke = 0.2
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(median_points, method == "MH"),
      mapping = ggplot2::aes(x = median_ess, y = scenario, colour = method),
      inherit.aes = FALSE,
      position = ggplot2::position_nudge(y = -0.11),
      size = 1.8,
      shape = 17,
      stroke = 0.2
    ) +
    ggplot2::facet_wrap(~n_obs, nrow = 1) +
    ggplot2::scale_x_log10(
      breaks = c(400, 1000, 3000, 10000),
      labels = c("400", "1K", "3K", "10K")
    ) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "Effective sample size (log scale)",
      y = "Scenario (Censoring, Weight)",
      fill = "Method",
      colour = "Method",
      title = "ESS distributions by scenario",
      subtitle = "Dashed line marks ESS = 400; points indicate method-specific medians"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      panel.spacing = grid::unit(1.5, "lines"),
      axis.text.y = ggplot2::element_text(vjust = 0.5),
      legend.position = "bottom"
      #     ) +
      #     ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.5, 0.30))
    )
}

#' Create Figure 2: Coverage of 95% CIs
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure2_coverage <- function(plot_data) {
  cov <- plot_data$res %>%
    transmute(
      n_obs,
      censoring,
      weight,
      method,
      parameter,
      covered = contains_truth
    ) %>%
    filter(parameter == "beta")

  cov_sum <- cov %>%
    group_by(n_obs, censoring, weight, method, parameter) %>%
    summarise(
      coverage = mean(covered),
      R = dplyr::n(),
      .groups = "drop"
    ) %>%
    mutate(
      mcse = sqrt(0.95 * 0.05 / R),
      lo = 0.95 - 2 * mcse,
      hi = 0.95 + 2 * mcse,
      scnx = interaction(censoring, weight, sep = ","),
      scnx = forcats::fct_inorder(scnx),
      scnx_id = as.numeric(scnx)
    ) %>%
    filter(!is.na(coverage)) %>%
    mutate(parameter = factor(parameter, levels = "beta"))

  cov_band <- cov_sum %>%
    distinct(n_obs, parameter, scnx_id, lo, hi)

  ggplot2::ggplot(
    cov_sum,
    ggplot2::aes(
      x = scnx_id,
      y = coverage,
      colour = method,
      shape = method,
      group = method
    )
  ) +
    ggplot2::geom_hline(yintercept = 0.95, colour = "grey40") +
    # ggplot2::annotate(
    #   "text",
    #   x = 1,
    #   y = 0.95,
    #   vjust = -0.5,
    #   hjust = 0,
    #   size = 3,
    #   colour = "grey30"
    # ) +
    ggplot2::geom_ribbon(
      data = cov_band,
      ggplot2::aes(x = scnx_id, ymin = lo, ymax = hi),
      inherit.aes = FALSE,
      fill = "grey80",
      alpha = 0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = interaction(method, censoring)),
      position = ggplot2::position_dodge(width = 0.4),
      linewidth = 0.5,
      alpha = 0.7
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4),
      size = 2.6,
      stroke = 0.4
    ) +
    ggplot2::facet_grid(n_obs ~ ., labeller = ggplot2::label_value) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(cov_sum$scnx_id)),
      labels = levels(cov_sum$scnx)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(),
      limits = c(0.75, 1.0),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "Scenario (Censoring, Weight)",
      y = "Coverage proportion",
      colour = "Method",
      shape = "Method",
      title = expression("Coverage of 95% credible intervals for " * beta[1]),
      caption = "Shaded band marks 95% ± 2 × MCSE from scenario replicates."
    ) +
    get_theme_sci() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.spacing.y = grid::unit(1.5, "lines")
    )
}

#' Create Figure 3a: Bias by parameter
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure3a_bias <- function(plot_data) {
  metrics <- compute_beta_metrics(plot_data)
  dodge <- ggplot2::position_dodge(width = 0.5)
  y_radius <- max(abs(metrics$mean_bias) + metrics$bias_mcse, na.rm = TRUE)
  if (!is.finite(y_radius)) {
    y_radius <- 0.1
  }
  y_limits <- c(-y_radius, y_radius)

  ggplot2::ggplot(
    metrics,
    ggplot2::aes(
      x = sample_size_label,
      y = mean_bias,
      colour = method,
      shape = method
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey20",
      linewidth = 0.5
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = mean_bias - bias_mcse,
        ymax = mean_bias + bias_mcse
      ),
      position = dodge,
      linewidth = 0.6
    ) +
    ggplot2::facet_grid(weight_regime ~ censoring_level) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::scale_shape_manual(values = c(HMC = 16, MH = 17)) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      labels = scales::label_number(accuracy = 0.01),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = "Sample size",
      y = expression("Mean bias of " * beta[1]),
      colour = "Method",
      shape = "Method",
      title = "Mean bias of \u03B2\u2081 across simulation scenarios"
    ) +
    get_theme_sci()
}

#' Create Figure 3b: RMSE by parameter
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure3b_rmse <- function(plot_data) {
  metrics <- compute_beta_metrics(plot_data)
  dodge <- ggplot2::position_dodge(width = 0.5)
  y_max <- max(metrics$mean_rmse + metrics$rmse_mcse, na.rm = TRUE)
  if (!is.finite(y_max) || y_max <= 0) {
    y_max <- 0.5
  }
  y_limits <- c(0, y_max * 1.15)

  pct_labels <- metrics %>%
    dplyr::group_by(weight_regime, censoring_level, sample_size_label) %>%
    dplyr::mutate(
      group_max_y = max(mean_rmse + rmse_mcse, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      weight_regime,
      censoring_level,
      sample_size_label,
      method,
      mean_rmse,
      group_max_y
    ) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = mean_rmse
    ) %>%
    dplyr::mutate(
      pct_diff = (HMC - MH) / MH,
      label = dplyr::if_else(
        is.finite(pct_diff),
        scales::percent(pct_diff, accuracy = 0.1),
        NA_character_
      ),
      y = group_max_y * 1.05
    ) %>%
    dplyr::filter(!is.na(label))

  ggplot2::ggplot(
    metrics,
    ggplot2::aes(
      x = sample_size_label,
      y = mean_rmse,
      colour = method,
      shape = method
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = method),
      position = dodge,
      linewidth = 0.5
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = pmax(mean_rmse - rmse_mcse, 0),
        ymax = mean_rmse + rmse_mcse
      ),
      position = dodge,
      linewidth = 0.6
    ) +
    ggplot2::facet_grid(weight_regime ~ censoring_level) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::scale_shape_manual(values = c(HMC = 16, MH = 17)) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      labels = scales::label_number(accuracy = 0.01),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    # ggplot2::geom_text(
    #   data = pct_labels,
    #   mapping = ggplot2::aes(
    #     x = sample_size_label,
    #     y = y,
    #     label = label
    #   ),
    #   inherit.aes = FALSE,
    #   size = 2.6,
    #   colour = "black",
    #   fontface = "bold",
    #   vjust = 0
    # ) +
    ggplot2::labs(
      x = "Sample size",
      y = "Root Mean Squared Error of \u03B2\u2081",
      colour = "Method",
      shape = "Method",
      title = "RMSE of \u03B2\u2081 across simulation scenarios"
    ) +
    get_theme_sci()
}

#' Create Figure 3c: Precision–accuracy trade-off
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure3c_bias_rmse_tradeoff <- function(plot_data) {
  metrics <- compute_beta_metrics(plot_data)
  ci_segments <- metrics %>%
    dplyr::mutate(
      ci_half = median_ci_width / 2,
      seg_xmin = pmax(abs_bias - ci_half, 0),
      seg_xmax = abs_bias + ci_half
    )

  x_max <- max(ci_segments$seg_xmax, na.rm = TRUE)
  y_max <- max(metrics$mean_rmse + metrics$rmse_mcse, na.rm = TRUE)

  if (!is.finite(x_max) || x_max <= 0) {
    x_max <- 0.1
  }
  if (!is.finite(y_max) || y_max <= 0) {
    y_max <- 0.5
  }

  ggplot2::ggplot(
    metrics,
    ggplot2::aes(
      x = abs_bias,
      y = mean_rmse,
      colour = method,
      shape = method,
      size = sample_size_label
    )
  ) +
    ggplot2::geom_segment(
      data = ci_segments,
      mapping = ggplot2::aes(
        x = seg_xmin,
        xend = seg_xmax,
        y = mean_rmse,
        yend = mean_rmse,
        colour = method
      ),
      inherit.aes = FALSE,
      linewidth = 0.9,
      alpha = 0.45
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = pmax(mean_rmse - rmse_mcse, 0),
        ymax = mean_rmse + rmse_mcse
      ),
      width = 0,
      alpha = 0.5,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(alpha = 0.95) +
    ggplot2::facet_grid(weight_regime ~ censoring_level) +
    ggplot2::scale_colour_manual(values = get_palette(), name = "Method") +
    ggplot2::scale_shape_manual(
      values = c(HMC = 16, MH = 17),
      name = "Method"
    ) +
    ggplot2::scale_size_manual(
      values = c("200" = 2.4, "2000" = 3.0, "10000" = 3.6),
      guide = ggplot2::guide_legend(
        title = "Sample size",
        override.aes = list(shape = 16, colour = "grey30", alpha = 1)
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, x_max * 1.1),
      expand = ggplot2::expansion(mult = c(0, 0.05)),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_max * 1.1),
      expand = ggplot2::expansion(mult = c(0, 0.05)),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::labs(
      x = expression("Absolute bias of " * beta[1]),
      y = "RMSE of \u03B2\u2081",
      colour = "Method",
      title = "Bias–RMSE trade-off for \u03B2\u2081"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey88"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Create Figure 4: ESS per second
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure4_ess_per_sec <- function(plot_data) {
  ess_df <- plot_data$res %>%
    dplyr::filter(!is.na(ess_per_sec), ess_per_sec > 0)

  median_labels <- ess_df %>%
    dplyr::group_by(n_obs, method) %>%
    dplyr::summarise(
      ess_median = stats::median(ess_per_sec, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      y_text = ess_median * 1.08,
      label = scales::number(
        ess_median,
        accuracy = 0.1,
        scale_cut = scales::cut_short_scale()
      )
    )

  y_limits <- range(ess_df$ess_per_sec, na.rm = TRUE)
  if (!is.finite(y_limits[1]) || !is.finite(y_limits[2])) {
    y_limits <- c(1, 1e4)
  }
  y_limits[2] <- max(y_limits[2], max(median_labels$y_text, na.rm = TRUE))

  ggplot2::ggplot(
    ess_df,
    ggplot2::aes(x = method, y = ess_per_sec, fill = method)
  ) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.65, linewidth = 0.35) +
    ggplot2::stat_summary(
      fun = median,
      geom = "point",
      colour = "black",
      size = 1.6,
      position = ggplot2::position_dodge(width = 0.75)
    ) +
    ggplot2::facet_wrap(~n_obs, nrow = 1) +
    ggplot2::scale_y_log10(
      limits = y_limits,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::labs(
      x = NULL,
      y = "ESS / second (log scale)",
      title = "Sampling efficiency",
      subtitle = "Median ESS/s shown by black dots"
    ) +
    get_theme_sci() +
    ggplot2::theme(legend.position = "none")
}

#' Create Figure 5: Runtime by scenario
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure5_runtime <- function(plot_data) {
  run_df <- get_plot_res(plot_data) %>%
    filter(!is.na(runtime_s))

  ggplot2::ggplot(
    run_df,
    ggplot2::aes(x = method, y = runtime_s, fill = method)
  ) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.7, width = 0.7) +
    ggplot2::stat_summary(
      fun = median,
      geom = "point",
      colour = "black",
      size = 1.2
    ) +
    ggplot2::facet_grid(
      rows = vars(n_obs, weight),
      cols = vars(censoring)
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(mult = c(0.4, 0.4))
    ) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::labs(
      x = NULL,
      y = "Runtime (seconds, log scale)",
      title = "Total runtime by sample size, censoring, and weight"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.placement = "outside",
      strip.text.y = ggplot2::element_text(angle = 0),
      panel.spacing.y = grid::unit(2.0, "lines")
    )
}

#' Create Figure 5b: Runtime speed-up heatmap (MH vs HMC)
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure5b_speedup_heatmap <- function(plot_data) {
  speedup_df <- get_plot_res(plot_data) %>%
    dplyr::filter(!is.na(runtime_s)) %>%
    dplyr::group_by(n_obs, censoring, weight, method) %>%
    dplyr::summarise(
      median_runtime = stats::median(runtime_s, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = median_runtime,
      names_prefix = "median_"
    ) %>%
    dplyr::rename(
      median_hmc = median_HMC,
      median_mh = median_MH
    ) %>%
    dplyr::mutate(
      speedup_ratio = median_mh / median_hmc,
      label = dplyr::if_else(
        is.finite(speedup_ratio),
        sprintf("%s\u00D7", scales::number(speedup_ratio, accuracy = 0.1)),
        "NA"
      )
    )

  max_dev <- max(abs(log(speedup_df$speedup_ratio)), na.rm = TRUE)
  if (!is.finite(max_dev) || max_dev == 0) {
    fill_limits <- c(0.5, 2)
  } else {
    fill_limits <- exp(c(-max_dev, max_dev))
  }

  ggplot2::ggplot(
    speedup_df,
    ggplot2::aes(x = censoring, y = weight, fill = speedup_ratio)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      colour = "black",
      size = 3
    ) +
    ggplot2::facet_wrap(~n_obs) +
    ggplot2::scale_fill_gradient2(
      low = "#F28E2B",
      mid = "white",
      high = "#2C7FB8",
      midpoint = 1,
      limits = fill_limits,
      oob = scales::squish,
      name = "Speed-up (MH ÷ HMC)",
      labels = function(x) sprintf("%s x", scales::number(x, accuracy = 0.1)),
      guide = ggplot2::guide_colourbar(
        title.position = "left",
        #title.vjust = 0.5,
        direction = "horizontal",
        label.position = "bottom",
        label.hjust = 0.5,
        barwidth = grid::unit(4.5, "cm"),
        barheight = grid::unit(0.4, "cm"),
        label.theme = ggplot2::element_text(
          margin = ggplot2::margin(t = 2, r = 6, l = 6)
        )
      )
    ) +
    ggplot2::labs(
      x = "Censoring proportion",
      y = "Weighting scheme",
      title = "Relative runtime of MH vs HMC",
      subtitle = "Tiles display MH runtime divided by HMC runtime (blue = HMC faster)"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank()
    )
}


#' Create Figure 5c: Paired median runtime slope plot
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure5c_paired_slopes <- function(plot_data) {
  runtime_summary <- get_plot_res(plot_data) %>%
    dplyr::filter(!is.na(runtime_s)) %>%
    dplyr::group_by(n_obs, censoring, weight, method) %>%
    dplyr::summarise(
      median_runtime = stats::median(runtime_s, na.rm = TRUE),
      .groups = "drop"
    )

  runtime_pairs <- runtime_summary %>%
    tidyr::pivot_wider(
      names_from = method,
      values_from = median_runtime,
      names_prefix = "median_"
    ) %>%
    dplyr::rename(
      median_hmc = median_HMC,
      median_mh = median_MH
    ) %>%
    dplyr::mutate(
      speedup_ratio = median_mh / median_hmc,
      faster = dplyr::case_when(
        is.na(speedup_ratio) ~ "No data",
        speedup_ratio > 1 ~ "HMC faster",
        speedup_ratio < 1 ~ "MH faster",
        TRUE ~ "Parity"
      ),
      faster = factor(
        faster,
        levels = c("HMC faster", "Parity", "MH faster", "No data")
      ),
      ratio_label = dplyr::if_else(
        is.finite(speedup_ratio),
        sprintf("%s\u00D7", scales::number(speedup_ratio, accuracy = 0.1)),
        "NA"
      ),
      geom_mean = sqrt(median_hmc * median_mh)
    )
  runtime_long <- runtime_summary %>%
    dplyr::mutate(method = factor(method, levels = c("HMC", "MH"))) %>%
    dplyr::left_join(
      runtime_pairs %>%
        dplyr::select(
          n_obs,
          censoring,
          weight,
          speedup_ratio,
          faster,
          ratio_label,
          geom_mean
        ),
      by = c("n_obs", "censoring", "weight")
    )

  label_data <- runtime_pairs %>%
    dplyr::transmute(
      n_obs,
      censoring,
      weight,
      faster,
      ratio_label,
      y = geom_mean,
      x = 1.5
    )
  ggplot2::ggplot(
    runtime_long,
    ggplot2::aes(
      x = method,
      y = median_runtime,
      group = interaction(n_obs, censoring, weight)
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(colour = faster),
      linewidth = 0.7,
      alpha = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = method, shape = weight),
      size = 2.3,
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = x, y = y, label = ratio_label, colour = faster),
      size = 2.4,
      vjust = -0.35,
      show.legend = FALSE
    ) +
    ggplot2::facet_grid(n_obs ~ censoring) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(mult = c(0.15, 0.15))
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "HMC faster" = "#1b9e77",
        "Parity" = "grey50",
        "MH faster" = "#d95f02",
        "No data" = "grey80"
      ),
      name = "Faster method",
      breaks = c("HMC faster", "MH faster", "Parity"),
      guide = ggplot2::guide_legend(
        override.aes = list(linewidth = 1.2, alpha = 1)
      )
    ) +
    ggplot2::scale_fill_manual(values = get_palette(), guide = "none") +
    ggplot2::scale_shape_manual(
      values = c(none = 21, low = 22, high = 24),
      name = "Weighting"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Median runtime (seconds, log scale)",
      title = "Paired median runtimes by method",
      subtitle = "Lines connect HMC and MH within each design cell; annotations show MH/HMC speed-up ratio"
    ) +
    get_theme_sci() +
    ggplot2::theme(legend.position = "bottom")
}

#' Create Figure 7: Agreement of posterior means
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure7_means_agreement <- function(plot_data) {
  est_df <- plot_data$res %>%
    mutate(method_chr = as.character(method)) %>%
    select(
      parameter,
      n_obs,
      censoring,
      weight,
      scenario_id,
      replicate,
      method_chr,
      mean
    ) %>%
    tidyr::pivot_wider(
      names_from = method_chr,
      values_from = mean,
      names_prefix = "mean_"
    ) %>%
    rename(
      mean_hmc = mean_HMC,
      mean_mh = mean_MH
    ) %>%
    tidyr::drop_na(mean_hmc, mean_mh) %>%
    mutate(
      n_obs = factor(n_obs, levels = c("n = 200", "n = 2000", "n = 10000"))
    )

  range_df <- est_df %>%
    dplyr::group_by(parameter, n_obs) %>%
    dplyr::summarise(
      axis_min = min(c(mean_hmc, mean_mh), na.rm = TRUE),
      axis_max = max(c(mean_hmc, mean_mh), na.rm = TRUE),
      .groups = "drop"
    )

  stats_df <- est_df %>%
    dplyr::group_by(parameter, n_obs) %>%
    dplyr::summarise(
      r = stats::cor(mean_hmc, mean_mh),
      slope = stats::coef(stats::lm(mean_mh ~ mean_hmc))[2],
      intercept = stats::coef(stats::lm(mean_mh ~ mean_hmc))[1],
      .groups = "drop"
    ) %>%
    dplyr::left_join(range_df, by = c("parameter", "n_obs")) %>%
    dplyr::mutate(
      x = axis_min + 0.05 * (axis_max - axis_min),
      y = axis_max - 0.08 * (axis_max - axis_min),
      label = sprintf(
        "r = %.3f\nslope = %.2f\nint. = %.2f",
        r,
        slope,
        intercept
      )
    )

  ggplot2::ggplot(est_df, ggplot2::aes(x = mean_hmc, y = mean_mh)) +
    # 2D density contours in background
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ggplot2::after_stat(level)),
      geom = "polygon",
      alpha = 0.2,
      bins = 4,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(low = "grey80", high = "grey20") +
    # 1:1 reference line
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      colour = "black",
      linewidth = 0.5
    ) +
    # Points with reduced alpha (red)
    ggplot2::geom_point(
      colour = "#2C7FB8",
      alpha = 0.45,
      size = 0.8,
      position = ggplot2::position_jitter(width = 0.01, height = 0.01)
    ) +
    ggplot2::geom_blank(
      data = range_df,
      mapping = ggplot2::aes(x = axis_min, y = axis_min)
    ) +
    ggplot2::geom_blank(
      data = range_df,
      mapping = ggplot2::aes(x = axis_max, y = axis_max)
    ) +
    ggplot2::facet_grid(n_obs ~ parameter) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "HMC posterior mean",
      y = "MH posterior mean",
      title = "Agreement of posterior means (HMC vs MH)"
    ) +
    get_theme_sci()
}

#' Create Figure A1: Credible intervals vs true value
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figureA1_cis_vs_truth <- function(plot_data) {
  scenario_levels <- tidyr::expand_grid(
    censoring = levels(plot_data$res$censoring),
    weight = levels(plot_data$res$weight)
  ) %>%
    mutate(level = paste(censoring, weight, sep = ", ")) %>%
    pull(level)

  ci <- plot_data$res %>%
    transmute(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      n_obs,
      censoring,
      weight,
      method,
      lower = q2.5,
      upper = q97.5,
      est = mean,
      hit = contains_truth
    ) %>%
    filter(!is.na(lower), !is.na(upper), !is.na(est), !is.na(parameter)) %>%
    mutate(
      scenario = factor(
        paste(censoring, weight, sep = ", "),
        levels = scenario_levels
      ),
      n_obs = factor(
        n_obs,
        levels = c("n = 200", "n = 2000", "n = 10000")
      )
    )

  truth <- plot_data$res %>%
    distinct(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      truth = true_value
    ) %>%
    tidyr::drop_na(truth)

  truth_lines <- tidyr::crossing(
    truth,
    n_obs = levels(plot_data$res$n_obs)
  )

  param_limits <- ci %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(
      xmin = min(lower, na.rm = TRUE),
      xmax = max(upper, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(truth, by = "parameter") %>%
    dplyr::mutate(
      xmin = pmin(xmin, truth),
      xmax = pmax(xmax, truth)
    ) %>%
    tidyr::crossing(
      n_obs = levels(ci$n_obs),
      scenario = levels(ci$scenario)
    )

  dodge <- ggplot2::position_dodge(width = 0.7)

  ggplot2::ggplot(
    ci,
    ggplot2::aes(y = scenario, xmin = lower, xmax = upper, colour = method)
  ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(alpha = hit),
      height = 0.3,
      position = dodge,
      linewidth = 0.55
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = est, shape = method, alpha = hit),
      position = dodge,
      size = 1.6,
      stroke = 0.8
    ) +
    ggplot2::geom_vline(
      data = truth_lines,
      ggplot2::aes(xintercept = truth, linetype = "True value"),
      colour = "grey30",
      linewidth = 0.6
    ) +
    ggplot2::geom_blank(
      data = param_limits,
      mapping = ggplot2::aes(x = xmin, y = scenario),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_blank(
      data = param_limits,
      mapping = ggplot2::aes(x = xmax, y = scenario),
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(parameter ~ n_obs, scales = "free_y") +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::scale_shape_manual(values = c(HMC = 19, MH = 2)) +
    ggplot2::scale_linetype_manual(
      values = c("True value" = "dashed"),
      guide = ggplot2::guide_legend(
        override.aes = list(colour = "grey30"),
        order = 2,
        title = ""
      )
    ) +
    ggplot2::scale_alpha_manual(
      values = c(`TRUE` = 0.9, `FALSE` = 0.4),
      guide = "none"
    ) +
    ggplot2::labs(
      x = "Estimate / 95% CI",
      y = "Scenario (Censoring, Weight)",
      colour = "Method",
      shape = "Method",
      title = "Credible intervals vs true value",
      subtitle = "Dashed vertical line indicates truth; point opacity encodes coverage"
    ) +
    get_theme_sci()
}

#' Create Figure A2: CI matrix
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figureA2_ci_matrix <- function(plot_data) {
  scenario_levels <- tidyr::expand_grid(
    censoring = levels(plot_data$res$censoring),
    weight = levels(plot_data$res$weight)
  ) %>%
    mutate(level = paste(censoring, weight, sep = ", ")) %>%
    pull(level)

  ci <- plot_data$res %>%
    transmute(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      n_obs,
      censoring,
      weight,
      method,
      lower = q2.5,
      upper = q97.5,
      est = mean,
      hit = contains_truth
    ) %>%
    filter(!is.na(lower), !is.na(upper), !is.na(est), !is.na(parameter)) %>%
    mutate(
      scenario = factor(
        paste(censoring, weight, sep = ", "),
        levels = scenario_levels
      )
    )

  ci_sum <- ci %>%
    mutate(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      n_obs = factor(
        n_obs,
        levels = c("n = 200", "n = 2000", "n = 10000")
      ),
      weight = factor(weight, levels = c("none", "low", "high")),
      censoring = factor(
        censoring,
        levels = c("C=0.1", "C=0.3", "C=0.5")
      ),
      method = factor(method, levels = c("MH", "HMC")),
      scenario = factor(
        paste(censoring, weight, sep = ", "),
        levels = scenario_levels
      )
    ) %>%
    group_by(parameter, n_obs, method, scenario, censoring, weight) %>%
    summarise(
      lower = median(lower, na.rm = TRUE),
      upper = median(upper, na.rm = TRUE),
      est = median(est, na.rm = TRUE),
      cover = mean(hit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(parameter, n_obs, method, censoring, weight) %>%
    group_by(parameter, n_obs, method) %>%
    mutate(
      scenario = forcats::fct_inorder(scenario),
      cover_flag = factor(
        dplyr::if_else(cover >= 0.95, "Covered", "Under-covered"),
        levels = c("Covered", "Under-covered")
      )
    ) %>%
    ungroup()

  truth <- plot_data$res %>%
    distinct(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      truth = true_value
    ) %>%
    tidyr::drop_na(truth)

  truth_panel <- tidyr::expand_grid(
    parameter = levels(ci_sum$parameter),
    n_obs = levels(ci_sum$n_obs),
    method = levels(ci_sum$method)
  ) %>%
    left_join(
      truth %>% mutate(parameter = as.character(parameter)),
      by = "parameter"
    ) %>%
    tidyr::drop_na(truth)

  axis_limits <- ci_sum %>%
    dplyr::group_by(parameter, n_obs) %>%
    dplyr::summarise(
      xmin = min(lower, na.rm = TRUE),
      xmax = max(upper, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      truth %>% mutate(parameter = as.character(parameter)),
      by = "parameter"
    ) %>%
    dplyr::mutate(
      xmin = pmin(xmin, truth),
      xmax = pmax(xmax, truth)
    ) %>%
    tidyr::crossing(
      method = levels(ci_sum$method),
      scenario = levels(ci_sum$scenario)
    )

  ggplot2::ggplot(
    ci_sum,
    ggplot2::aes(y = scenario, xmin = lower, xmax = upper, colour = method)
  ) +
    ggplot2::geom_errorbarh(
      height = 0.25,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = est, shape = cover_flag),
      size = 2.2,
      fill = "white",
      stroke = 0.6
    ) +
    ggplot2::geom_vline(
      data = truth_panel,
      mapping = ggplot2::aes(xintercept = truth),
      colour = "grey35",
      linetype = 2,
      linewidth = 0.7
    ) +
    ggplot2::geom_blank(
      data = axis_limits,
      mapping = ggplot2::aes(x = xmin, y = scenario),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_blank(
      data = axis_limits,
      mapping = ggplot2::aes(x = xmax, y = scenario),
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parameter, n_obs),
      cols = ggplot2::vars(method),
      scales = "free_y"
    ) +
    ggplot2::scale_colour_manual(values = get_palette(), drop = FALSE) +
    ggplot2::scale_shape_manual(
      values = c("Covered" = 16, "Under-covered" = 1),
      drop = FALSE,
      name = "\u25CF covered / \u25CB under-covered"
    ) +
    ggplot2::labs(
      title = "Credible intervals vs true value by parameter, sample size, and method",
      x = "Estimate / 95% CI",
      y = "Scenario (Censoring, Weight)"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      panel.spacing.y = grid::unit(1.3, "lines"),
      panel.spacing.x = grid::unit(1.1, "lines"),
      axis.text.y = ggplot2::element_text(size = 9)
    )
}

#' Create Figure B1: Precision-coverage trade-off
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figureB1_precision_tradeoff <- function(plot_data) {
  scenario_levels <- tidyr::expand_grid(
    censoring = levels(plot_data$res$censoring),
    weight = levels(plot_data$res$weight)
  ) %>%
    mutate(level = paste(censoring, weight, sep = ", ")) %>%
    pull(level)

  ci <- plot_data$res %>%
    transmute(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      n_obs,
      censoring,
      weight,
      method,
      lower = q2.5,
      upper = q97.5,
      est = mean,
      hit = contains_truth
    ) %>%
    filter(!is.na(lower), !is.na(upper), !is.na(est), !is.na(parameter)) %>%
    mutate(
      scenario = factor(
        paste(censoring, weight, sep = ", "),
        levels = scenario_levels
      ),
      width = upper - lower
    )

  precision_summary <- ci %>%
    group_by(parameter, n_obs, scenario, method) %>%
    summarise(
      coverage = mean(hit, na.rm = TRUE),
      R = dplyr::n(),
      mcse = sqrt(pmax(coverage * (1 - coverage), 0) / R),
      width_med = median(width, na.rm = TRUE),
      width_se = sd(width, na.rm = TRUE) / sqrt(R),
      .groups = "drop"
    ) %>%
    mutate(
      width_se = tidyr::replace_na(width_se, 0),
      coverage_lo = coverage - 2 * mcse,
      coverage_hi = coverage + 2 * mcse,
      scenario = factor(scenario, levels = scenario_levels)
    )

  # Create pair IDs for connecting HMC-MH pairs
  pair_summary <- precision_summary %>%
    group_by(parameter, n_obs, scenario) %>%
    mutate(pair_id = paste(parameter, n_obs, scenario, sep = "_")) %>%
    ungroup()

  ggplot2::ggplot(
    pair_summary,
    ggplot2::aes(x = width_med, y = coverage, colour = method, shape = method)
  ) +
    ggplot2::geom_hline(
      yintercept = 0.95,
      colour = "grey60",
      linetype = 3,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = Inf,
      y = 0.95,
      label = "Target 95%",
      hjust = 1.05,
      vjust = -0.5,
      size = 2.8,
      colour = "grey30"
    ) +
    # Connect HMC-MH pairs with line segments
    ggplot2::geom_line(
      ggplot2::aes(group = pair_id),
      colour = "grey70",
      linewidth = 0.4,
      alpha = 0.6
    ) +
    # Error bars with reduced alpha
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = coverage_lo, ymax = coverage_hi),
      linewidth = 0.35,
      width = 0,
      alpha = 0.6
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = width_med - width_se, xmax = width_med + width_se),
      linewidth = 0.35,
      height = 0.008,
      alpha = 0.6
    ) +
    # Points on top with distinct shapes
    ggplot2::geom_point(
      size = 2.5,
      stroke = 0.8
    ) +
    ggplot2::facet_grid(parameter ~ n_obs, scales = "free_x") +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::scale_shape_manual(values = c(HMC = 16, MH = 17)) + # Circle, triangle
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      x = "Median 95% CI width",
      y = "Coverage proportion",
      colour = "Method",
      shape = "Method",
      title = "Precision–coverage trade-off (lines connect HMC-MH pairs)"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Create Figure B2: Coverage bias heatmap
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figureB2_coverage_heatmap <- function(plot_data) {
  censor_levels <- levels(plot_data$res$censoring)
  weight_levels <- levels(plot_data$res$weight)
  scenario_levels <- unlist(
    lapply(
      weight_levels,
      function(w) paste(censor_levels, w, sep = ", ")
    )
  )

  ci <- plot_data$res %>%
    transmute(
      parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
      n_obs,
      censoring,
      weight,
      method,
      hit = contains_truth
    ) %>%
    filter(!is.na(parameter), !is.na(hit)) %>%
    mutate(
      scenario = factor(
        paste(censoring, weight, sep = ", "),
        levels = scenario_levels
      )
    )

  precision_summary <- ci %>%
    group_by(parameter, n_obs, scenario, method) %>%
    summarise(
      coverage = mean(hit, na.rm = TRUE),
      .groups = "drop"
    )

  coverage_heatmap <- precision_summary %>%
    mutate(
      coverage_bias = coverage - 0.95,
      panel_id = interaction(parameter, n_obs, sep = " | ")
    ) %>%
    mutate(
      label = dplyr::if_else(
        is.finite(coverage_bias),
        sprintf("%+.1f", coverage_bias * 100),
        "NA"
      )
    )

  ggplot2::ggplot(
    coverage_heatmap,
    ggplot2::aes(x = scenario, y = panel_id, fill = coverage_bias)
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      colour = "grey15",
      size = 2.6
    ) +
    ggplot2::facet_wrap(~method, nrow = 1) +
    ggplot2::scale_fill_gradient2(
      limits = c(-0.10, 0.10),
      breaks = seq(-0.10, 0.10, 0.05),
      oob = scales::squish,
      low = "#B2182B",
      mid = "#f7f7f7",
      high = "#2166AC",
      midpoint = 0,
      name = "observed\u20130.95 (absolute %)",
      labels = function(x) sprintf("%+.1f", x * 100)
    ) +
    ggplot2::labs(
      x = "Scenario (Censoring, Weight)",
      y = "Parameter | Sample size",
      title = "Global bias in coverage"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      legend.position = "right"
    )
}

#' Create Figure B3: CI width violin plots
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figureB3_ci_width_violin <- function(plot_data) {
  ci_detail <- plot_data$res %>%
    filter(!is.na(q2.5), !is.na(q97.5)) %>%
    mutate(width = q97.5 - q2.5)

  iqr_summary <- function(y) {
    qs <- stats::quantile(y, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    data.frame(y = qs[2], ymin = qs[1], ymax = qs[3])
  }

  ggplot2::ggplot(
    ci_detail,
    ggplot2::aes(x = method, y = width, fill = method)
  ) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.65, linewidth = 0.35) +
    ggplot2::stat_summary(
      fun.data = iqr_summary,
      geom = "linerange",
      colour = "black",
      linewidth = 1.1
    ) +
    ggplot2::stat_summary(
      fun = median,
      geom = "point",
      colour = "black",
      size = 1.4
    ) +
    ggplot2::facet_grid(parameter ~ n_obs) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::labs(
      x = NULL,
      y = "CI width",
      fill = "Method",
      title = "Distribution of credible interval widths",
      subtitle = "Black dot = median; thick bar = interquartile range"
    ) +
    get_theme_sci() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Create Figure 6: Weighted Survival Curves by Design Cell
#'
#' Creates a 27-panel figure showing weighted interval-censored survival curves
#' across all design cells (3 censoring levels × 3 weight types × 3 sample sizes).
#' Unlike other figures in this module, this uses raw simulation data with
#' parametric fits (icenReg) rather than pre-computed MCMC results.
#'
#' @param data_dirs Character vector of paths to simulation data directories.
#'   Default: c("sim_data/n200", "sim_data/n2000", "sim_data/n10000")
#' @param which Character. Type of curve to plot: "S" (survival), "H" (cumulative
#'   hazard), or "h" (hazard). Default: "S"
#' @param n_ghost Integer. Maximum number of replicate curves to show per cell.
#'   Default: 50
#' @param true_alpha Numeric. True baseline scale parameter. Default: 5.0
#' @param true_gamma Numeric. True shape parameter. Default: 1.5
#' @param true_beta Numeric. True AFT coefficient. Default: -0.5
#'
#' @return A ggplot object with 27 panels (9 rows × 3 columns)
#'
#' @details
#' This figure provides exploratory visualization of survival curve behavior
#' across the simulation design space. Each panel shows:
#' \itemize{
#'   \item Ghost curves: Up to 50 semi-transparent replicate survival curves
#'   \item Uncertainty ribbons: 50% and 95% pointwise credible intervals
#'   \item Mean curve: Average survival curve across replicates (gray)
#'   \item True curve: Population survival curve based on true parameters (black)
#' }
#'
#' Faceting structure:
#' \itemize{
#'   \item Rows: Sample size × censoring (9 levels: n=200/C=0.1, n=200/C=0.3, ...)
#'   \item Columns: Weight type (3 levels: none, low, high)
#' }
#'
#' @export
create_figure6_survival_cells <- function(
  data_dirs = c("data/n200", "data/n2000", "data/n10000"),
  which = "S",
  n_ghost = 10,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5
) {
  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }

  suppressPackageStartupMessages({
    library(dplyr)
  })

  # Initialize storage
  results_list <- list()
  design_df_list <- list()

  # Loop through each sample size directory
  for (data_dir in data_dirs) {
    message("\nProcessing directory: ", data_dir)

    if (!dir.exists(data_dir)) {
      warning("Directory does not exist: ", data_dir, ". Skipping.")
      next
    }

    # Discover design cells in this directory
    design_df <- create_design_df_from_files(
      path = data_dir,
      true_alpha = true_alpha,
      true_gamma = true_gamma,
      true_beta = true_beta
    )

    # Filter to only include desired censoring levels
    design_df <- design_df %>%
      filter(censoring %in% c(0.1, 0.3, 0.5))

    if (nrow(design_df) == 0) {
      warning("No design cells found in ", data_dir)
      next
    }

    message("  Found ", nrow(design_df), " design cells")

    # Process each design cell
    for (i in 1:nrow(design_df)) {
      cell_meta <- design_df[i, ]
      cell_id <- cell_meta$cell_id

      message("  Processing cell: ", cell_id)

      # Build file pattern
      pattern <- sprintf(
        "sim_s.*_n%04d_c%.1f_w%s\\.rds$",
        cell_meta$n,
        cell_meta$censoring,
        cell_meta$weight_type
      )

      # Load simulation data
      sims <- tryCatch(
        {
          load_sims_from_dir(
            path = data_dir,
            pattern = pattern,
            require_cols = c("L", "R", "weight", "X1")
          )
        },
        error = function(e) {
          warning("  Failed to load data for ", cell_id, ": ", e$message)
          return(list())
        }
      )

      if (length(sims) == 0) {
        message("    No data found")
        next
      }

      # Limit to n_ghost replicates
      if (length(sims) > n_ghost) {
        sims <- sims[1:n_ghost]
      }

      message("    Loaded ", length(sims), " replicates")

      # Fit parametric models
      results <- tryCatch(
        {
          summarise_weighted_curves(
            sims = sims,
            weight_metric = "cv",
            include_covariate = TRUE
          )
        },
        error = function(e) {
          warning("    Error fitting ", cell_id, ": ", e$message)
          return(NULL)
        }
      )

      if (!is.null(results)) {
        results_list[[cell_id]] <- results
        message(
          "    Converged: ",
          results$meta$n_converged,
          " / ",
          results$meta$n_total
        )
      }
    }

    # Store design_df for this directory
    design_df_list[[data_dir]] <- design_df
  }

  # Check if we have any results
  if (length(results_list) == 0) {
    stop("No results to plot. All design cells failed.")
  }

  # Combine all design dataframes
  design_df_combined <- bind_rows(design_df_list)

  # Ensure weight_type has correct factor ordering
  design_df_combined <- design_df_combined %>%
    mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high"))
    )

  # Create composite row labels for faceting
  design_df_combined <- design_df_combined %>%
    mutate(
      row_label = factor(
        paste0("n=", n, ", C=", censoring),
        levels = c(
          "n=200, C=0.1",
          "n=200, C=0.3",
          "n=200, C=0.5",
          "n=2000, C=0.1",
          "n=2000, C=0.3",
          "n=2000, C=0.5",
          "n=10000, C=0.1",
          "n=10000, C=0.3",
          "n=10000, C=0.5"
        )
      )
    )

  # Combine all rep_df and sum_df with cell_id
  all_rep <- list()
  all_sum <- list()

  for (cell_id in names(results_list)) {
    res <- results_list[[cell_id]]

    # Add cell_id to both dataframes
    rep_with_id <- res$rep_df %>%
      mutate(cell_id = cell_id)

    sum_with_id <- res$sum_df %>%
      mutate(cell_id = cell_id)

    all_rep[[cell_id]] <- rep_with_id
    all_sum[[cell_id]] <- sum_with_id
  }

  combined_rep <- bind_rows(all_rep)
  combined_sum <- bind_rows(all_sum)

  # Join with design_df to add design factors
  combined_rep <- combined_rep %>%
    left_join(design_df_combined, by = "cell_id")

  combined_sum <- combined_sum %>%
    left_join(design_df_combined, by = "cell_id")

  # Transform curves if needed
  if (which %in% c("H", "h")) {
    combined_sum <- combined_sum %>%
      mutate(
        S_mean = -log(S_mean),
        S_med = -log(S_med),
        S_q50l = -log(S_q50l),
        S_q50u = -log(S_q50u),
        S_q95l = -log(S_q95l),
        S_q95u = -log(S_q95u)
      )

    combined_rep <- combined_rep %>%
      mutate(S = -log(S))
  }

  if (which == "h") {
    # Hazard via finite differences
    combined_sum <- combined_sum %>%
      group_by(cell_id, if ("X1" %in% names(.)) X1 else NULL) %>%
      arrange(t) %>%
      mutate(
        dt = c(diff(t), diff(t)[n() - 1]),
        S_mean = pmax(c(diff(S_mean), 0) / dt, 0),
        S_q50l = pmax(c(diff(S_q50l), 0) / dt, 0),
        S_q50u = pmax(c(diff(S_q50u), 0) / dt, 0),
        S_q95l = pmax(c(diff(S_q95l), 0) / dt, 0),
        S_q95u = pmax(c(diff(S_q95u), 0) / dt, 0)
      ) %>%
      select(-dt) %>%
      ungroup()

    combined_rep <- combined_rep %>%
      group_by(cell_id, rep_id, if ("X1" %in% names(.)) X1 else NULL) %>%
      arrange(t) %>%
      mutate(
        dt = c(diff(t), diff(t)[n() - 1]),
        S = pmax(c(diff(S), 0) / dt, 0)
      ) %>%
      select(-dt) %>%
      ungroup()
  }

  # Sample ghost replicates per cell (already limited to n_ghost during loading)
  ghost_df <- combined_rep

  # Compute true curves per cell
  true_list <- lapply(unique(combined_sum$cell_id), function(cid) {
    cell_meta <- design_df_combined %>% filter(cell_id == cid)

    t_grid <- combined_sum %>%
      filter(cell_id == cid) %>%
      pull(t) %>%
      unique() %>%
      sort()

    true_vals <- compute_marginal_survival(
      t = t_grid,
      alpha = cell_meta$true_alpha[1],
      gamma = cell_meta$true_gamma[1],
      beta = cell_meta$true_beta[1],
      X1_dist = c(0.5, 0.5),
      type = which
    )

    data.frame(
      cell_id = cid,
      t = t_grid,
      S_true = true_vals
    ) %>%
      left_join(design_df_combined, by = "cell_id")
  })

  true_df <- bind_rows(true_list)

  weight_pal <- c(
    "none" = "#4D4D4D",
    "low" = "#1B9E77",
    "high" = "#A6CEE3"
  )

  # Build faceted plot
  y_lab <- switch(
    which,
    "S" = "Survival S(t)",
    "H" = "Cumulative Hazard H(t)",
    "h" = "Hazard h(t)"
  )

  p <- ggplot2::ggplot(combined_sum, ggplot2::aes(x = t)) +
    # 95% ribbon
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = S_q95l, ymax = S_q95u),
      fill = "grey70",
      alpha = 0.3
    ) +
    # 50% ribbon
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = S_q50l, ymax = S_q50u),
      fill = "grey50",
      alpha = 0.4
    ) +
    # Ghost lines
    ggplot2::geom_line(
      data = ghost_df,
      ggplot2::aes(y = S, group = rep_id),
      colour = "grey55",
      alpha = 0.1,
      linewidth = 0.28
    ) +
    # Mean curve (dashed)
    ggplot2::geom_line(
      ggplot2::aes(y = S_mean),
      colour = "grey40",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    # Median curve emphasised by weight regime
    ggplot2::geom_line(
      ggplot2::aes(y = S_med, colour = weight_type),
      linewidth = 1.2
    ) +
    # True curve
    ggplot2::geom_line(
      data = true_df,
      ggplot2::aes(x = t, y = S_true),
      colour = "red",
      linewidth = 1.1
    ) +
    # Facets with composite row labels
    ggplot2::facet_grid(row_label ~ weight_type, scales = "free") +
    ggplot2::scale_colour_manual(
      values = weight_pal,
      name = "Weight regime"
    ) +
    # Theme
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90"),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "Time",
      y = y_lab,
      title = "Weighted Survival Estimates Across Design Cells",
      subtitle = paste0(
        "Ghost curves: ",
        n_ghost,
        " replicates per cell (grey, \u03B1 = 0.1) | ",
        "Ribbons: 50% (dark) and 95% (light) pointwise intervals | ",
        "Bold colour: median curve by weight regime | ",
        "Red line: true population curve"
      )
    )

  p +
    ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(linewidth = 1.6))
    )
}

# ZIMPHIA Figures -------------------------------------------------------------

# Provide fallbacks if helper functions are not yet defined (e.g., when sourcing
# this file in isolation during development).
if (!exists("get_zimphia_paths")) {
  get_zimphia_paths <- function(base_dir = "mcmc_outputs/zimphia") {
    list(
      prepared = file.path(base_dir, "zimphia_prepared_data.rds"),
      method_comparison = file.path(base_dir, "zimphia_method_comparison.csv"),
      runtime_comparison = file.path(
        base_dir,
        "zimphia_runtime_comparison.csv"
      ),
      hmc_summary = file.path(
        base_dir,
        "hmc",
        "summaries",
        "zimphia_hmc_summary.csv"
      ),
      mh_summary = file.path(
        base_dir,
        "mh",
        "summaries",
        "zimphia_mh_summary.csv"
      ),
      hmc_diag = file.path(
        base_dir,
        "hmc",
        "diagnostics",
        "zimphia_hmc_diagnostics.csv"
      ),
      mh_diag = file.path(
        base_dir,
        "mh",
        "diagnostics",
        "zimphia_mh_diagnostics.csv"
      ),
      hmc_draws = file.path(base_dir, "hmc", "draws", "zimphia_hmc_draws.rds"),
      mh_draws = file.path(base_dir, "mh", "draws", "zimphia_mh_draws.rds"),
      mh_fit = file.path(base_dir, "mh", "fits", "zimphia_mh_fit.rds")
    )
  }
}

if (!exists("load_zimphia_prepared_data")) {
  load_zimphia_prepared_data <- function(base_dir = "mcmc_outputs/zimphia") {
    paths <- get_zimphia_paths(base_dir)
    if (!file.exists(paths$prepared)) {
      stop("Prepared ZIMPHIA data not found at: ", paths$prepared)
    }
    readRDS(paths$prepared)
  }
}

if (!exists("load_zimphia_summaries")) {
  load_zimphia_summaries <- function(base_dir = "mcmc_outputs/zimphia") {
    paths <- get_zimphia_paths(base_dir)
    list(
      hmc = readr::read_csv(paths$hmc_summary, show_col_types = FALSE),
      mh = readr::read_csv(paths$mh_summary, show_col_types = FALSE)
    )
  }
}

#' Save ZIMPHIA-specific figures
#'
#' @param output_dir Directory where figures should be written.
#' @param zimphia_dir Directory containing ZIMPHIA outputs.
#' @param sim_root Root directory containing simulation datasets (data/n*).
#' @param formats File formats to save (default png).
#' @param width Default width for saved figures.
#' @param height Default height for saved figures.
#' @param dpi Resolution used by ggsave.
#'
#' @return Invisibly returns list of ggplot objects.
#' @export
save_zimphia_figures <- function(
  output_dir = "outputs/figures",
  zimphia_dir = "mcmc_outputs/zimphia",
  sim_root = "data",
  sim_reps_per_cell = 3,
  formats = "png",
  width = 10,
  height = 6,
  dpi = 320
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  plots <- list()

  message("Creating ZIMPHIA interval censoring patterns...")
  plots$fig3_6 <- create_figure3_6_zimphia_interval_patterns(
    zimphia_dir = zimphia_dir,
    sim_root = sim_root,
    per_cell = sim_reps_per_cell
  )
  save_figure(
    plots$fig3_6,
    file.path(output_dir, "fig3_6_zimphia_interval_patterns"),
    width = width,
    height = height,
    dpi = dpi,
    formats = formats
  )

  message("Creating ZIMPHIA convergence diagnostics comparison...")
  plots$fig3_7 <- create_figure3_7_zimphia_convergence_traces(
    zimphia_dir = zimphia_dir
  )
  save_figure(
    plots$fig3_7,
    file.path(output_dir, "fig3_7_zimphia_convergence"),
    width = 15,
    height = height,
    dpi = dpi,
    formats = formats
  )

  message(
    "Creating ZIMPHIA posterior distributions for seroconversion model..."
  )
  plots$fig3_8 <- create_figure3_8_zimphia_posterior_forest(
    zimphia_dir = zimphia_dir
  )
  save_figure(
    plots$fig3_8,
    file.path(output_dir, "fig3_8_zimphia_posterior_forest"),
    width = width,
    height = height,
    dpi = dpi,
    formats = formats
  )

  invisible(plots)
}

#' Internal helper: load ZIMPHIA draws
#' @keywords internal
load_zimphia_draws <- function(
  method = c("hmc", "mh"),
  base_dir = "mcmc_outputs/zimphia"
) {
  method <- match.arg(method)
  paths <- get_zimphia_paths(base_dir)
  target <- if (method == "hmc") paths$hmc_draws else paths$mh_draws
  if (!file.exists(target)) {
    stop("ZIMPHIA draws not found at: ", target)
  }
  readRDS(target)
}

#' Internal helper: sample simulation interval widths
#' @keywords internal
collect_sim_interval_widths <- function(
  sim_root = "data",
  per_cell = 3,
  sample_sizes = c(200, 2000, 10000)
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(purrr)
    library(tibble)
  })

  width_list <- purrr::map(sample_sizes, function(n) {
    dir_path <- file.path(sim_root, paste0("n", n))
    if (!dir.exists(dir_path)) {
      warning("Simulation directory not found: ", dir_path)
      return(tibble())
    }

    files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) == 0) {
      return(tibble())
    }

    meta <- tibble(file = files) %>%
      dplyr::mutate(
        fname = basename(file)
      ) %>%
      tidyr::extract(
        fname,
        regex = "sim_s(\\d+)_r(\\d+)_n(\\d+)_c([0-9.]+)_w([a-z]+)\\.rds",
        into = c("scenario", "rep", "n_obs", "censoring", "weight_type"),
        convert = TRUE
      ) %>%
      tidyr::drop_na()

    if (nrow(meta) == 0) {
      return(tibble())
    }

    meta <- meta %>%
      dplyr::group_by(n_obs, censoring, weight_type) %>%
      dplyr::arrange(rep) %>%
      dplyr::slice_head(n = per_cell) %>%
      dplyr::ungroup()

    purrr::map2_dfr(
      meta$file,
      seq_len(nrow(meta)),
      function(fpath, idx) {
        dat <- readRDS(fpath)
        finite_idx <- is.finite(dat$R)
        if (!any(finite_idx)) {
          return(tibble())
        }
        tibble(
          width = pmax(dat$R[finite_idx] - dat$L[finite_idx], 0),
          gender = if_else(dat$X1[finite_idx] == 1, "Female", "Male"),
          source = "Simulation",
          sample_size = meta$n_obs[idx],
          censoring = meta$censoring[idx],
          weight_type = meta$weight_type[idx],
          plot_weight = 1
        )
      }
    )
  })

  dplyr::bind_rows(width_list)
}

#' ZIMPHIA interval censoring patterns vs simulation
#'
#' @param zimphia_dir Directory containing prepared ZIMPHIA data.
#' @param sim_root Root directory containing simulation datasets.
#' @param per_cell Number of simulation replicates per design cell.
#'
#' @return Patchwork/ggplot object
#' @export
create_figure3_6_zimphia_interval_patterns <- function(
  zimphia_dir = "mcmc_outputs/zimphia",
  sim_root = "data",
  per_cell = 3
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(patchwork)
    library(scales)
  })

  zimphia_df <- load_zimphia_prepared_data(zimphia_dir) %>%
    dplyr::filter(is.finite(R)) %>%
    dplyr::mutate(
      width = pmax(R - L, 0),
      gender = dplyr::if_else(X1 == 1, "Female", "Male"),
      source = "ZIMPHIA",
      plot_weight = weight * (n() / sum(weight, na.rm = TRUE))
    )

  sim_df <- collect_sim_interval_widths(
    sim_root = sim_root,
    per_cell = per_cell
  )

  if (nrow(sim_df) == 0) {
    warning("Simulation interval widths unavailable; plotting ZIMPHIA only.")
  }

  combined_overall <- dplyr::bind_rows(
    zimphia_df %>% dplyr::select(width, source, plot_weight),
    sim_df %>%
      dplyr::mutate(plot_weight = 1) %>%
      dplyr::select(width, source, plot_weight)
  ) %>%
    dplyr::filter(is.finite(width))

  xmax <- stats::quantile(combined_overall$width, 0.99, na.rm = TRUE) %>%
    as.numeric()

  median_lines <- combined_overall %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(
      median_width = stats::median(width, na.rm = TRUE),
      .groups = "drop"
    )

  interval_stats <- combined_overall %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(
      median = stats::median(width, na.rm = TRUE),
      p25 = stats::quantile(width, 0.25, na.rm = TRUE),
      p75 = stats::quantile(width, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      summary = sprintf(
        "%s median %.1f [%.1f–%.1f]",
        source,
        median,
        p25,
        p75
      )
    )

  palette_sources <- c(ZIMPHIA = "#7D3C98", Simulation = "#1B9E77")

  # Compute sample sizes for panel annotation
  n_overall <- combined_overall %>%
    dplyr::group_by(source) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  p_overall <- ggplot2::ggplot(
    combined_overall,
    ggplot2::aes(
      x = width,
      fill = source,
      colour = source,
      weight = plot_weight
    )
  ) +
    ggplot2::geom_histogram(
      position = "identity",
      alpha = 0.35,
      binwidth = 1,
      boundary = 0
    ) +
    ggplot2::geom_vline(
      data = median_lines,
      ggplot2::aes(xintercept = median_width, colour = source),
      linetype = "dashed",
      linewidth = 0.7
    ) +
    ggplot2::scale_fill_manual(values = palette_sources, name = "Data source") +
    ggplot2::scale_colour_manual(values = palette_sources) +
    ggplot2::guides(colour = "none", fill = ggplot2::guide_legend(override.aes = list(alpha = 0.7))) +
    ggplot2::scale_y_log10(
      labels = scales::comma,
      breaks = scales::breaks_log(n = 6),
      limits = c(0.5, NA),
      oob = scales::squish
    ) +
    ggplot2::coord_cartesian(xlim = c(0, xmax)) +
    ggplot2::labs(
      title = "Panel A — Interval width distribution",
      x = "Interval width (years)",
      y = "Weighted count (log scale)",
      subtitle = paste(interval_stats$summary, collapse = " | ")
    ) +
    get_theme_sci() +
    ggplot2::theme(legend.position = "none")

  gender_df <- dplyr::bind_rows(
    zimphia_df %>% dplyr::select(width, gender, source, plot_weight),
    sim_df %>%
      dplyr::mutate(plot_weight = 1) %>%
      dplyr::select(width, gender, source, plot_weight)
  ) %>%
    dplyr::filter(is.finite(width))

  gender_labels <- zimphia_df %>%
    dplyr::count(gender, wt = plot_weight) %>%
    dplyr::mutate(
      gender_label = sprintf("%s (n = %s)", gender, scales::comma(round(n)))
    )

  gender_df <- gender_df %>%
    dplyr::left_join(gender_labels, by = "gender")

  gender_medians <- gender_df %>%
    dplyr::group_by(gender_label, source) %>%
    dplyr::summarise(
      median_width = stats::median(width, na.rm = TRUE),
      .groups = "drop"
    )

  p_gender <- ggplot2::ggplot(
    gender_df,
    ggplot2::aes(
      x = width,
      colour = source,
      fill = source,
      weight = plot_weight
    )
  ) +
    ggplot2::geom_density(alpha = 0.12, adjust = 1.2, linewidth = 0.9) +
    ggplot2::geom_vline(
      data = gender_medians,
      ggplot2::aes(xintercept = median_width, colour = source),
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~gender_label, nrow = 1) +
    ggplot2::scale_colour_manual(values = palette_sources, guide = "none") +
    ggplot2::scale_fill_manual(values = palette_sources, guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0, xmax)) +
    ggplot2::labs(
      title = "Panel B — Stratified by gender",
      x = "Interval width (years)",
      y = "Weighted density"
    ) +
    get_theme_sci() +
    ggplot2::theme(legend.position = "none")

  # Combine with patchwork and center legend at bottom
  combined <- p_overall +
    p_gender +
    patchwork::plot_layout(ncol = 2, widths = c(1.1, 0.9), guides = "collect") +
    patchwork::plot_annotation(
      title = "ZIMPHIA interval censoring patterns",
      subtitle = "Observed histograms are survey-weighted; dashed lines mark medians for ZIMPHIA (purple) and simulation (green) sources"
    ) &
    ggplot2::theme(legend.position = "bottom")

  return(combined)
}

#' ZIMPHIA convergence diagnostics via trace plots
#'
#' @param zimphia_dir Directory containing ZIMPHIA outputs.
#'
#' @return ggplot object
#' @export
create_figure3_7_zimphia_convergence_traces <- function(
  zimphia_dir = "mcmc_outputs/zimphia"
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(patchwork)
  })

  # Load draws and summaries
  hmc_draws <- load_zimphia_draws("hmc", zimphia_dir)
  mh_draws <- load_zimphia_draws("mh", zimphia_dir)
  summaries <- load_zimphia_summaries(zimphia_dir)

  # Prepare summary stats for both methods
  param_summary <- dplyr::bind_rows(
    summaries$hmc %>%
      dplyr::select(variable, median, rhat, ess_bulk) %>%
      dplyr::mutate(method = "HMC"),
    summaries$mh %>%
      dplyr::select(variable, median, rhat, ess = ess) %>%
      dplyr::mutate(method = "MH", ess_bulk = ess)
  ) %>%
    dplyr::filter(variable %in% c("alpha", "beta"))

  # Helper function to create individual trace plot
  make_trace_plot <- function(draws_df, method_name, param_name, param_label,
                               show_y_label = TRUE, show_x_label = TRUE) {
    # Thin draws for plotting
    thin_every <- 5
    plot_df <- draws_df %>%
      dplyr::select(.chain, .iteration, !!rlang::sym(param_name)) %>%
      dplyr::rename(value = !!rlang::sym(param_name)) %>%
      dplyr::group_by(.chain) %>%
      dplyr::arrange(.iteration) %>%
      dplyr::filter(
        row_number() %% thin_every == 1 | row_number() == dplyr::n()
      ) %>%
      dplyr::ungroup()

    # Get diagnostics for this method/parameter
    diag_info <- param_summary %>%
      dplyr::filter(variable == param_name, method == method_name)

    median_val <- diag_info$median
    rhat_val <- diag_info$rhat
    ess_val <- diag_info$ess_bulk

    # Create diagnostic label using R-hat notation
    diag_label <- sprintf(
      "Rhat = %.3f\nESS = %s%s",
      rhat_val,
      scales::comma(round(ess_val)),
      if (method_name == "HMC") "\nDiv = 0" else ""
    )

    # Build plot
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .iteration, y = value, colour = factor(.chain))
    ) +
      ggplot2::geom_line(alpha = 0.7, linewidth = 0.3) +
      ggplot2::geom_hline(
        yintercept = median_val,
        colour = "grey40",
        linetype = "dashed",
        linewidth = 0.5
      ) +
      ggplot2::annotate(
        "text",
        x = Inf,
        y = Inf,
        label = diag_label,
        hjust = 1.02,
        vjust = 1.3,
        size = 3,
        colour = "grey25"
      ) +
      ggplot2::labs(
        title = paste(method_name, "—", param_label),
        x = if (show_x_label) "Iteration" else NULL,
        y = if (show_y_label) "Parameter value" else NULL,
        colour = "Chain"
      ) +
      get_theme_sci() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(size = 11, face = "bold")
      )

    return(p)
  }

  # Create 4 individual plots
  p_hmc_alpha <- make_trace_plot(
    hmc_draws, "HMC", "alpha", "α (baseline)",
    show_y_label = TRUE, show_x_label = FALSE
  )

  p_hmc_beta <- make_trace_plot(
    hmc_draws, "HMC", "beta", "β (gender)",
    show_y_label = FALSE, show_x_label = FALSE
  )

  p_mh_alpha <- make_trace_plot(
    mh_draws, "MH", "alpha", "α (baseline)",
    show_y_label = TRUE, show_x_label = TRUE
  )

  p_mh_beta <- make_trace_plot(
    mh_draws, "MH", "beta", "β (gender)",
    show_y_label = FALSE, show_x_label = TRUE
  )

  # Combine with patchwork
  combined <- (p_hmc_alpha | p_hmc_beta) /
              (p_mh_alpha | p_mh_beta) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = "ZIMPHIA convergence diagnostics via trace plots",
      subtitle = "Dashed horizontal line = posterior median; annotations show split-Rhat, bulk ESS, and HMC divergences",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11)
      )
    ) &
    ggplot2::theme(legend.position = "bottom")

  return(combined)
}

#' ZIMPHIA posterior distributions for seroconversion model
#'
#' @param zimphia_dir Directory containing ZIMPHIA outputs.
#'
#' @return ggplot object
#' @export
create_figure3_8_zimphia_posterior_forest <- function(
  zimphia_dir = "mcmc_outputs/zimphia"
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(tidyr)
  })

  zimphia_dat <- load_zimphia_prepared_data(zimphia_dir)
  observed_n <- nrow(zimphia_dat)
  observed_censoring <- mean(is.infinite(zimphia_dat$R))
  observed_weight_cv <- stats::sd(zimphia_dat$weight) / mean(zimphia_dat$weight)

  summaries <- load_zimphia_summaries(zimphia_dir)
  combined <- dplyr::bind_rows(
    summaries$hmc %>% dplyr::mutate(method = "HMC"),
    summaries$mh %>% dplyr::mutate(method = "MH")
  ) %>%
    dplyr::filter(variable %in% c("alpha", "beta", "gamma"))

  sim_sample_sizes <- c(200, 2000, 10000)
  target_n <- sim_sample_sizes[which.min(abs(sim_sample_sizes - observed_n))]
  censor_levels <- c(0.1, 0.3, 0.5)
  target_censoring <- censor_levels[which.min(abs(
    censor_levels - observed_censoring
  ))]
  weight_targets <- c(none = 0, low = 1 / sqrt(10), high = 1)
  target_weight <- names(weight_targets)[which.min(abs(
    weight_targets - observed_weight_cv
  ))]

  param_labels <- tibble::tibble(
    variable = c("alpha", "beta", "gamma"),
    label = c(
      "α — Baseline median (years)",
      "β — Gender effect (female vs male)",
      "γ — Log-logistic shape"
    ),
    facet = c(
      "Alpha — median years",
      "Beta — covariate effect",
      "Gamma — shape parameter"
    )
  )

  summary_df <- combined %>%
    dplyr::select(variable, median, q2.5, q97.5, method) %>%
    dplyr::left_join(param_labels, by = "variable") %>%
    dplyr::mutate(
      method = factor(method, levels = c("HMC", "MH")),
      facet = factor(facet, levels = param_labels$facet)
    )

  sim_base <- readRDS("outputs/combined_results/combined_summaries.rds")

  sim_filtered <- sim_base %>%
    dplyr::filter(
      variable %in% c("alpha", "beta", "gamma"),
      n_obs == target_n,
      dplyr::near(censoring, target_censoring),
      weight_type == target_weight
    )

  if (nrow(sim_filtered) == 0) {
    stop(
      "No simulation summaries matched the inferred ZIMPHIA scenario (n = ",
      target_n,
      ", censoring = ",
      target_censoring,
      ", weights = ",
      target_weight,
      ")."
    )
  }

  # Programmatic guard: verify simulation scenario matches ZIMPHIA characteristics
  sim_setup_check <- list(
    n = target_n,
    censoring = target_censoring,
    weight_type = target_weight
  )
  zimphia_setup_check <- list(
    n = observed_n,
    censoring = observed_censoring,
    weight_cv = observed_weight_cv
  )

  # Warn if the match is not close
  n_diff_pct <- abs(observed_n - target_n) / observed_n * 100
  censor_diff <- abs(observed_censoring - target_censoring)

  if (n_diff_pct > 10) {
    warning(
      "Sample size mismatch: ZIMPHIA n = ",
      observed_n,
      ", simulation n = ",
      target_n,
      " (", round(n_diff_pct, 1), "% difference)"
    )
  }

  if (censor_diff > 0.1) {
    warning(
      "Censoring rate mismatch: ZIMPHIA = ",
      round(observed_censoring, 3),
      ", simulation = ",
      target_censoring,
      " (difference = ", round(censor_diff, 3), ")"
    )
  }

  sim_summary <- sim_filtered %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      truth = stats::median(true_value, na.rm = TRUE),
      band_lo = stats::quantile(median, 0.025, na.rm = TRUE),
      band_hi = stats::quantile(median, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(param_labels, by = "variable")

  zero_line_df <- tibble::tibble(
    facet = factor(
      param_labels$facet[param_labels$variable == "beta"],
      levels = param_labels$facet
    ),
    x = 0
  )

  sim_reference_text <- sprintf(
    "Simulation reference: n = %s, censoring = %.1f, weights = %s",
    scales::comma(target_n),
    target_censoring,
    target_weight
  )
  obs_context_text <- sprintf(
    "ZIMPHIA observed: n = %s, censoring = %.2f, weight CV = %.2f",
    scales::comma(observed_n),
    observed_censoring,
    observed_weight_cv
  )

  subtitle_text <- paste(
    "Grey bands show simulation 95% range; dotted line = simulation truth",
    sprintf("%s | %s", sim_reference_text, obs_context_text),
    sep = "\n"
  )

  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(y = method, x = median, colour = method)
  ) +
    ggplot2::geom_vline(
      data = zero_line_df,
      ggplot2::aes(xintercept = x),
      colour = "grey85",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::geom_rect(
      data = sim_summary,
      ggplot2::aes(
        xmin = band_lo,
        xmax = band_hi,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey90",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = sim_summary,
      ggplot2::aes(xintercept = truth),
      colour = "grey40",
      linetype = "dotted",
      linewidth = 0.7
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = q2.5, xmax = q97.5),
      height = 0.3,
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    ggplot2::geom_point(
      size = 2.2,
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    ggplot2::facet_wrap(~facet, scales = "free_x", ncol = 1) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "Posterior median with 95% CrI",
      y = "",
      colour = "Sampler",
      title = "Posterior distributions for ZIMPHIA seroconversion model",
      subtitle = subtitle_text
    ) +
    get_theme_sci() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.subtitle = ggplot2::element_text(size = 10)
    )

  p
}
