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
#'   \item fig5_runtime.png - Total runtime by scenario
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

  cat("  Creating Figure 5: Runtime...\n")
  plots$fig5 <- create_figure5_runtime(plot_data)
  save_figure(
    plots$fig5,
    file.path(output_dir, "fig5_runtime"),
    width = 10,
    height = 6,
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
    height = 12,
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
  ggplot2::theme_minimal(base_size = 11) +
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
  ggplot2::ggplot(plot_data$res, ggplot2::aes(rhat, colour = method)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 1.01, linetype = 2, colour = "grey40") +
    ggplot2::annotate(
      "text",
      x = 1.01,
      y = 0.9,
      label = "Convergence threshold",
      hjust = -0.05,
      size = 3,
      colour = "grey30"
    ) +
    ggplot2::facet_wrap(~n_obs, nrow = 1) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "R-hat",
      y = "ECDF",
      colour = "Method",
      title = "R-hat ECDFs by sample size"
    ) +
    ggplot2::coord_cartesian(xlim = c(1, 1.02)) +
    get_theme_sci()
}

#' Create Figure 1b: ESS ridges by scenario
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure1b_ess_ridges <- function(plot_data) {
  label_data <- plot_data$res %>%
    dplyr::distinct(n_obs) %>%
    dplyr::mutate(
      x = 400,
      y = Inf,
      label = "Minimum ESS (400)"
    )

  ggplot2::ggplot(
    plot_data$res,
    ggplot2::aes(x = ess_bulk, y = scenario, fill = method, colour = method)
  ) +
    ggplot2::geom_vline(
      xintercept = 400,
      linetype = 2,
      colour = "grey40",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = label_data,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = -0.05,
      vjust = 1.3,
      size = 2.8,
      colour = "grey30"
    ) +
    ggridges::geom_density_ridges(alpha = 0.5, scale = 0.9, linewidth = 0.2) +
    ggplot2::facet_wrap(~n_obs, nrow = 1) +
    ggplot2::scale_x_log10(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "Effective sample size (log scale)",
      y = "Scenario (Censoring, Weight)",
      fill = "Method",
      colour = "Method",
      title = "ESS distributions by scenario"
    ) +
    get_theme_sci()
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
    )

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
    filter(!is.na(coverage))

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
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4),
      size = 2
    ) +
    ggplot2::facet_grid(n_obs ~ parameter) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(cov_sum$scnx_id)),
      labels = levels(cov_sum$scnx)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_colour_manual(values = get_palette()) +
    ggplot2::labs(
      x = "Scenario (Censoring, Weight)",
      y = "Coverage proportion",
      colour = "Method",
      shape = "Method",
      title = "Coverage of 95% credible intervals with MC uncertainty bands"
    ) +
    get_theme_sci() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

#' Create Figure 3a: Bias by parameter
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure3a_bias <- function(plot_data) {
  perf <- plot_data$res %>%
    filter(!is.na(parameter), !is.na(bias), !is.na(rmse), rmse > 0)

  ggplot2::ggplot(perf, ggplot2::aes(x = method, y = bias, fill = method)) +
    tidybayes::stat_halfeye(
      adjust = 0.6,
      width = 0.6,
      .width = 0.5,
      justification = -0.2,
      slab_alpha = 0.7,
      point_colour = "black"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 1,
      colour = "black",
      linewidth = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x = 1.5,
      y = 0,
      label = "No bias",
      vjust = -0.5,
      size = 2.8,
      colour = "grey30"
    ) +
    ggplot2::facet_grid(n_obs ~ parameter) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::labs(
      x = NULL,
      y = "Bias",
      title = "Bias by parameter and sample size"
    ) +
    get_theme_sci() +
    ggplot2::theme(legend.position = "none")
}

#' Create Figure 3b: RMSE by parameter
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure3b_rmse <- function(plot_data) {
  perf <- plot_data$res %>%
    filter(!is.na(parameter), !is.na(bias), !is.na(rmse), rmse > 0)

  ggplot2::ggplot(perf, ggplot2::aes(x = method, y = rmse, fill = method)) +
    tidybayes::stat_halfeye(
      adjust = 0.6,
      width = 0.6,
      .width = 0.5,
      justification = -0.2,
      slab_alpha = 0.7,
      point_colour = "black"
    ) +
    ggplot2::facet_grid(n_obs ~ parameter) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::labs(
      x = NULL,
      y = "RMSE (log scale)",
      title = "RMSE by parameter and sample size"
    ) +
    get_theme_sci() +
    ggplot2::theme(legend.position = "none")
}

#' Create Figure 4: ESS per second
#' @param plot_data Prepared plot data from prepare_plot_data()
#' @export
create_figure4_ess_per_sec <- function(plot_data) {
  ggplot2::ggplot(
    plot_data$res,
    ggplot2::aes(x = method, y = ess_per_sec, fill = method)
  ) +
    ggplot2::geom_violin(trim = FALSE, alpha = 0.7) +
    ggplot2::stat_summary(
      fun = median,
      geom = "point",
      colour = "black",
      size = 1.2
    ) +
    ggplot2::facet_wrap(~n_obs, nrow = 1) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::scale_fill_manual(values = get_palette()) +
    ggplot2::labs(
      x = NULL,
      y = "ESS / second (log scale)",
      title = "Sampling efficiency"
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
      panel.spacing.y = grid::unit(1.25, "lines")
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
      log2_ratio = log2(speedup_ratio),
      label = dplyr::if_else(
        is.finite(speedup_ratio),
        sprintf("%s\u00D7", scales::number(speedup_ratio, accuracy = 0.1)),
        "NA"
      )
    )

  max_abs <- max(abs(speedup_df$log2_ratio), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs == 0) {
    fill_limits <- c(-1, 1)
  } else {
    fill_limits <- c(-max_abs, max_abs)
  }
  legend_labels <- function(x) {
    ratio <- 2^x
    sprintf("%s\u00D7", scales::number(ratio, accuracy = 0.1))
  }

  ggplot2::ggplot(
    speedup_df,
    ggplot2::aes(x = censoring, y = weight, fill = log2_ratio)
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
      midpoint = 0,
      limits = fill_limits,
      name = "Relative speed (log2 ratio)",
      labels = legend_labels
    ) +
    ggplot2::labs(
      x = "Censoring proportion",
      y = "Weighting scheme",
      title = "Fig. 5b. Relative runtime of MH vs HMC",
      subtitle = "Tiles display log2(MH / HMC) speed-up; positive values (blue) indicate that HMC is faster"
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
      title = "Fig. 5c. Paired median runtimes by method",
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
        linetype = 2,
        colour = "grey50",
        linewidth = 0.6
      ) +
      # Annotation for diagonal
      ggplot2::annotate(
        "text",
        x = -Inf,
        y = Inf,
        label = "Perfect agreement",
        angle = 45,
        hjust = -0.2,
        vjust = 1.5,
        size = 2.8,
        colour = "grey30"
      ) +
      # Points with reduced alpha (red)
      ggplot2::geom_point(colour = "red", alpha = 0.35, size = 0.6) +
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

    ggplot2::ggplot(
      ci,
      ggplot2::aes(y = scenario, xmin = lower, xmax = upper, colour = method)
    ) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(alpha = hit),
        height = 0.3,
        position = ggplot2::position_dodge(width = 0.6),
        linewidth = 0.6
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = est, shape = method, alpha = hit),
        position = ggplot2::position_dodge(width = 0.6),
        size = 1.8,
        stroke = 1
      ) +
      ggplot2::geom_vline(
        data = truth_lines,
        ggplot2::aes(xintercept = truth, linetype = "True value"),
        colour = "grey30",
        linewidth = 0.6
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
        title = "Credible intervals vs true value"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        strip.text = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(size = 8)
      )
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
          if_else(cover >= 0.95, ">=95%", "<95%"),
          levels = c(">=95%", "<95%")
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
        colour = "grey40",
        linetype = 2,
        linewidth = 0.7
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(parameter, n_obs),
        cols = ggplot2::vars(method),
        scales = "free_y"
      ) +
      ggplot2::scale_colour_manual(values = get_palette(), drop = FALSE) +
      ggplot2::scale_shape_manual(
        values = c(">=95%" = 16, "<95%" = 1),
        drop = FALSE,
        name = "Coverage"
      ) +
      ggplot2::labs(
        title = "Credible intervals vs true value by parameter, sample size, and method",
        x = "Estimate / 95% CI",
        y = "Scenario (Censoring, Weight)"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.y = ggplot2::element_text(size = 8)
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
        linewidth = 0.3,
        alpha = 0.7
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
      ggplot2::geom_point(size = 2.5, stroke = 0.8) +
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
      )

    ggplot2::ggplot(
      coverage_heatmap,
      ggplot2::aes(x = scenario, y = panel_id, fill = coverage_bias)
    ) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::facet_wrap(~method, nrow = 1) +
      ggplot2::scale_fill_gradient2(
        limits = c(-0.10, 0.10),
        breaks = seq(-0.10, 0.10, 0.05),
        oob = scales::squish,
        low = "#B2182B",
        mid = "#f7f7f7",
        high = "#2166AC",
        midpoint = 0,
        name = "Coverage bias\n(observed - 0.95)"
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

    ggplot2::ggplot(
      ci_detail,
      ggplot2::aes(x = method, y = width, fill = method)
    ) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.7) +
      ggplot2::stat_summary(
        fun = median,
        geom = "point",
        colour = "black",
        size = 1
      ) +
      ggplot2::facet_grid(parameter ~ n_obs) +
      ggplot2::scale_fill_manual(values = get_palette()) +
      ggplot2::labs(
        x = NULL,
        y = "CI width",
        fill = "Method",
        title = "Distribution of credible interval widths"
      ) +
      get_theme_sci() +
      ggplot2::theme(legend.position = "bottom")
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
    n_ghost = 30,
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
        ggplot2::aes(y = S, group = rep_id, color = weight_metric_value),
        alpha = 0.08,
        linewidth = 0.3
      ) +
      # Mean curve
      ggplot2::geom_line(
        ggplot2::aes(y = S_mean),
        color = "grey30",
        linewidth = 0.9
      ) +
      # True curve
      ggplot2::geom_line(
        data = true_df,
        ggplot2::aes(x = t, y = S_true),
        color = "red",
        linewidth = 1.1
      ) +
      # Facets with composite row labels
      ggplot2::facet_grid(row_label ~ weight_type, scales = "free") +
      # Color scale
      ggplot2::scale_color_viridis_c(
        name = "CV(weights)",
        guide = "none"
      ) +
      # Theme
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill = "grey90"),
        strip.text = ggplot2::element_text(face = "bold", size = 9),
        legend.position = "none"
      ) +
      ggplot2::labs(
        x = "Time",
        y = y_lab,
        title = "Weighted Survival Estimates Across Design Cells",
        subtitle = paste0(
          "Ghost curves: ",
          n_ghost,
          " replicates per cell | ",
          "Ribbons: 50% (dark) and 95% (light) pointwise intervals | ",
          "Red line: true population curve"
        )
      )

    p
  }
