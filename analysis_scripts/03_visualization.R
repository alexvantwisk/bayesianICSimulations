#!/usr/bin/env Rscript
# ==============================================================================
# 03_visualization.R
#
# Purpose: Create publication-ready tables and figures for HMC vs MH comparison
#
# Inputs:  results/analysis/scenario_summaries.rds
#          results/analysis/convergence_analysis.csv
#          results/analysis/efficiency_comparisons.csv
#          results/analysis/statistical_tests.rds
#
# Outputs: results/tables/*.csv (gt tables)
#          results/figures/*.png and *.pdf (ggplot figures)
#
# Author: Alexander van Twisk
# Date: 2025-10-24
# ==============================================================================

# Load required packages -------------------------------------------------------
library(tidyverse)
library(gt)
library(patchwork)
library(scales)

# Setup ------------------------------------------------------------------------
# Assume script is run from HPC directory or set working directory appropriately
if (file.exists("results") && file.exists("analysis_scripts")) {
  base_dir <- getwd()
} else if (file.exists("../results") && file.exists("../data")) {
  # If running from analysis_scripts directory, go up one level
  base_dir <- dirname(getwd())
} else {
  # Default to current directory
  base_dir <- getwd()
}
results_dir <- file.path(base_dir, "results")
analysis_dir <- file.path(results_dir, "analysis")
tables_dir <- file.path(results_dir, "tables")
figures_dir <- file.path(results_dir, "figures")

# Create output directories
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Creating Tables and Figures: HMC vs MH Comparison\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

# Load Data --------------------------------------------------------------------
cat("Loading analysis results...\n")

# Load aggregated analysis results
scenario_summaries <- readRDS(file.path(analysis_dir, "scenario_summaries.rds"))
convergence_analysis <- read_csv(file.path(analysis_dir, "convergence_analysis.csv"),
                                  show_col_types = FALSE)
efficiency_comparisons <- read_csv(file.path(analysis_dir, "efficiency_comparisons.csv"),
                                    show_col_types = FALSE)
statistical_tests <- readRDS(file.path(analysis_dir, "statistical_tests.rds"))

# Load replicate-level data for paired comparison plots
data_dir <- file.path(base_dir, "data")
combined_summaries <- readRDS(file.path(data_dir, "combined_summaries.rds"))

# Filter out log_alpha variable (derived quantity, causes NA panels)
scenario_summaries <- scenario_summaries %>%
  filter(variable != "log_alpha")
combined_summaries <- combined_summaries %>%
  filter(variable != "log_alpha")

cat("  ✓ Data loaded successfully\n\n")

# Helper Functions -------------------------------------------------------------

#' Save plot in multiple formats
save_plot <- function(plot, filename, width = 10, height = 6) {
  # Save as PNG (300 dpi for publication)
  ggsave(
    filename = file.path(figures_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )

  # Save as PDF (vector)
  ggsave(
    filename = file.path(figures_dir, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    device = "pdf"
  )

  cat(sprintf("  ✓ Saved %s.png and .pdf\n", filename))
}

# Set ggplot theme -------------------------------------------------------------
theme_set(theme_bw(base_size = 11))
theme_update(
  panel.grid.minor = element_blank(),
  strip.background = element_rect(fill = "grey90"),
  legend.position = "bottom"
)

# Color palette (colorblind-friendly)
method_colors <- c("hmc" = "#0072B2", "mh" = "#D55E00")
method_labels <- c("hmc" = "HMC", "mh" = "MH")

# TABLES =======================================================================

cat("Creating tables...\n")

# Table 1: Study Overview ------------------------------------------------------

table1_data <- convergence_analysis %>%
  group_by(method, n_obs) %>%
  summarise(
    n_scenarios = n_distinct(scenario_id),
    observed_fits = sum(n_observed),  # CHANGED: was n_total
    expected_fits = sum(n_expected),   # NEW: total expected
    missing_fits = sum(n_missing),     # NEW: total missing
    converged_fits = sum(n_converged),
    pct_complete = 100 * observed_fits / expected_fits,  # NEW
    pct_converged = 100 * converged_fits / expected_fits,  # CHANGED: % of expected
    .groups = "drop"
  ) %>%
  mutate(
    method = factor(method, levels = c("hmc", "mh"), labels = c("HMC", "MH"))
  )

table1 <- table1_data %>%
  gt() %>%
  tab_header(
    title = "Table 1: Study Overview and Completion Rates"
  ) %>%
  cols_label(
    method = "Method",
    n_obs = "Sample Size",
    n_scenarios = "Scenarios",
    observed_fits = "Observed",   # CHANGED
    expected_fits = "Expected",   # NEW
    missing_fits = "Missing",     # NEW
    converged_fits = "Converged",
    pct_complete = "% Complete",  # NEW
    pct_converged = "% Converged"
  ) %>%
  fmt_number(
    columns = c(observed_fits, expected_fits, missing_fits, converged_fits),  # UPDATED
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(pct_complete, pct_converged),  # UPDATED
    decimals = 1
  ) %>%
  tab_style(
    style = cell_fill(color = "grey95"),
    locations = cells_body(rows = method == "HMC")
  )

# Save as HTML and CSV
table1 %>%
  gtsave(filename = file.path(tables_dir, "table1_overview.html"))

table1_data %>%
  write_csv(file.path(tables_dir, "table1_overview.csv"))

cat("  ✓ Saved Table 1: Study Overview\n")

# Table 2: Convergence Summary -------------------------------------------------

table2_data <- convergence_analysis %>%
  group_by(method, n_obs) %>%
  summarise(
    mean_rhat = mean(mean_rhat, na.rm = TRUE),
    max_rhat = max(max_rhat_obs, na.rm = TRUE),
    mean_ess = mean(mean_ess, na.rm = TRUE),
    min_ess = min(min_ess_obs, na.rm = TRUE),
    pct_converged = mean(pct_converged, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    method = factor(method, levels = c("hmc", "mh"), labels = c("HMC", "MH"))
  )

table2 <- table2_data %>%
  gt() %>%
  tab_header(
    title = "Table 2: Convergence Diagnostics by Method and Sample Size"
  ) %>%
  cols_label(
    method = "Method",
    n_obs = "Sample Size",
    mean_rhat = "Mean R̂",
    max_rhat = "Max R̂",
    mean_ess = "Mean ESS",
    min_ess = "Min ESS",
    pct_converged = "% Converged"
  ) %>%
  fmt_number(
    columns = c(mean_rhat, max_rhat),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(mean_ess, min_ess),
    decimals = 0
  ) %>%
  fmt_number(
    columns = pct_converged,
    decimals = 1
  ) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      columns = mean_rhat,
      rows = mean_rhat > 1.01
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      columns = mean_ess,
      rows = mean_ess < 400
    )
  )

table2 %>%
  gtsave(filename = file.path(tables_dir, "table2_convergence.html"))

table2_data %>%
  write_csv(file.path(tables_dir, "table2_convergence.csv"))

cat("  ✓ Saved Table 2: Convergence Summary\n")

# Table 3: Posterior Accuracy (Main Results) ----------------------------------

table3_data <- scenario_summaries %>%
  filter(variable != "lp__") %>%
  group_by(method, variable, n_obs) %>%
  summarise(
    # All fits
    mean_bias = mean(abs(bias), na.rm = TRUE),
    mean_bias_mcse = mean(bias_mcse, na.rm = TRUE),
    mean_rmse = mean(rmse, na.rm = TRUE),
    mean_coverage = mean(coverage, na.rm = TRUE),
    mean_coverage_mcse = mean(coverage_mcse, na.rm = TRUE),

    # Converged only
    mean_bias_conv = mean(abs(bias_converged), na.rm = TRUE),
    mean_rmse_conv = mean(rmse_converged, na.rm = TRUE),
    mean_coverage_conv = mean(coverage_converged, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    method = factor(method, levels = c("hmc", "mh"), labels = c("HMC", "MH")),
    parameter = factor(variable, levels = c("alpha", "beta", "gamma"),
                      labels = c("α", "β", "γ"))
  )

table3 <- table3_data %>%
  select(method, parameter, n_obs,
         mean_bias, mean_rmse, mean_coverage, mean_coverage_mcse,
         mean_bias_conv, mean_rmse_conv, mean_coverage_conv) %>%
  gt(groupname_col = "parameter") %>%
  tab_header(
    title = "Table 3: Posterior Accuracy by Parameter, Method, and Sample Size",
    subtitle = "All Fits and Converged Fits"
  ) %>%
  tab_spanner(
    label = "All Fits",
    columns = c(mean_bias, mean_rmse, mean_coverage, mean_coverage_mcse)
  ) %>%
  tab_spanner(
    label = "Converged Only",
    columns = c(mean_bias_conv, mean_rmse_conv, mean_coverage_conv)
  ) %>%
  cols_label(
    method = "Method",
    n_obs = "n",
    mean_bias = "|Bias|",
    mean_rmse = "RMSE",
    mean_coverage = "Coverage",
    mean_coverage_mcse = "MCSE",
    mean_bias_conv = "|Bias|",
    mean_rmse_conv = "RMSE",
    mean_coverage_conv = "Coverage"
  ) %>%
  fmt_number(
    columns = c(mean_bias, mean_bias_conv, mean_rmse, mean_rmse_conv),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(mean_coverage, mean_coverage_conv),
    decimals = 3
  ) %>%
  fmt_number(
    columns = mean_coverage_mcse,
    decimals = 4
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = c(mean_coverage, mean_coverage_conv),
      rows = (mean_coverage >= 0.945 & mean_coverage <= 0.955) |
             (mean_coverage_conv >= 0.945 & mean_coverage_conv <= 0.955)
    )
  )

table3 %>%
  gtsave(filename = file.path(tables_dir, "table3_accuracy.html"))

table3_data %>%
  write_csv(file.path(tables_dir, "table3_accuracy.csv"))

cat("  ✓ Saved Table 3: Posterior Accuracy\n")

# Table 4: Sampling Efficiency ------------------------------------------------

table4_data <- scenario_summaries %>%
  filter(variable == "alpha") %>%  # Use one parameter (metrics same across params)
  group_by(method, n_obs, censoring, weight_type) %>%
  summarise(
    mean_ess_per_sec = mean(mean_ess_per_sec, na.rm = TRUE),
    median_ess_per_sec = mean(median_ess_per_sec, na.rm = TRUE),
    mean_runtime = mean(mean_runtime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    method = factor(method, levels = c("hmc", "mh"), labels = c("HMC", "MH"))
  )

table4 <- table4_data %>%
  gt() %>%
  tab_header(
    title = "Table 4: Sampling Efficiency (ESS/sec) and Runtime"
  ) %>%
  cols_label(
    method = "Method",
    n_obs = "Sample Size",
    censoring = "Censoring",
    weight_type = "Weights",
    mean_ess_per_sec = "Mean ESS/sec",
    median_ess_per_sec = "Median ESS/sec",
    mean_runtime = "Mean Runtime (sec)"
  ) %>%
  fmt_number(
    columns = c(mean_ess_per_sec, median_ess_per_sec),
    decimals = 1
  ) %>%
  fmt_number(
    columns = mean_runtime,
    decimals = 2
  )

table4 %>%
  gtsave(filename = file.path(tables_dir, "table4_efficiency.html"))

table4_data %>%
  write_csv(file.path(tables_dir, "table4_efficiency.csv"))

cat("  ✓ Saved Table 4: Sampling Efficiency\n")

# NOTE: Table 5 (HMC Diagnostics) removed - all divergence values were zero

cat("\n")

# FIGURES ======================================================================

cat("Creating figures...\n")

# Figure 1: Convergence Diagnostics -------------------------------------------

# Panel A: R̂ distributions by sample size
fig1a <- convergence_analysis %>%
  ggplot(aes(x = method, y = mean_rhat, fill = method)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, outlier.shape = NA) +
  geom_hline(yintercept = 1.01, linetype = "dashed", color = "red") +
  facet_wrap(~n_obs, labeller = label_both) +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  scale_x_discrete(labels = method_labels) +
  labs(
    title = "A) R-hat Distributions",
    x = "Method",
    y = "Mean R-hat"
  ) +
  theme(legend.position = "none")

# Panel B: ESS distributions by sample size
fig1b <- convergence_analysis %>%
  ggplot(aes(x = method, y = mean_ess, fill = method)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, outlier.shape = NA) +
  geom_hline(yintercept = 400, linetype = "dashed", color = "red") +
  facet_wrap(~n_obs, labeller = label_both) +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  scale_x_discrete(labels = method_labels) +
  scale_y_log10(labels = comma) +
  labs(
    title = "B) Effective Sample Size (ESS)",
    x = "Method",
    y = "Mean ESS (log scale)"
  ) +
  theme(legend.position = "none")

# Panel C: Convergence failure rates by sample size
fig1c <- convergence_analysis %>%
  group_by(method, n_obs) %>%
  summarise(
    failure_rate = 100 * (1 - mean(pct_converged / 100)),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = method, y = failure_rate, fill = method)) +
  geom_col(alpha = 0.7, width = 0.6) +
  facet_wrap(~n_obs, labeller = label_both) +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  scale_x_discrete(labels = method_labels) +
  labs(
    title = "C) Convergence Failure Rates",
    x = "Method",
    y = "Failure Rate (%)"
  )

# Combine panels vertically for better readability with sample size facets
fig1 <- fig1a / fig1b / fig1c +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

save_plot(fig1, "fig1_convergence", width = 12, height = 12)

# Figure 2: Coverage Analysis (with MC uncertainty) ---------------------------

# Prepare data for coverage plot
coverage_data <- scenario_summaries %>%
  filter(variable != "lp__") %>%
  mutate(
    variable_label = factor(variable,
                           levels = c("alpha", "beta", "gamma"),
                           labels = c("α (Scale)", "β (AFT Coef.)", "γ (Shape)")),
    scenario_label_short = paste0("c", censoring, "_", weight_type),
    n_obs_label = paste0("n = ", n_obs)
  )

fig2 <- coverage_data %>%
  ggplot(aes(x = scenario_label_short, y = coverage, color = method, shape = method)) +
  # Target coverage line
  geom_hline(yintercept = 0.95, linetype = "solid", color = "black", linewidth = 0.5) +
  # Monte Carlo uncertainty ribbon (0.95 ± 2*MCSE)
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0.95 - 2 * mean(coverage_data$coverage_mcse, na.rm = TRUE),
    ymax = 0.95 + 2 * mean(coverage_data$coverage_mcse, na.rm = TRUE),
    alpha = 0.2,
    fill = "gray70"
  ) +
  # Coverage points
  geom_point(size = 2.5, alpha = 0.7, position = position_dodge(width = 0.5)) +
  facet_grid(n_obs_label ~ variable_label) +
  scale_color_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  scale_shape_manual(
    values = c("hmc" = 16, "mh" = 17),
    labels = method_labels,
    name = "Method"
  ) +
  labs(
    title = "Figure 2: Coverage of 95% Credible Intervals with Monte Carlo Uncertainty",
    subtitle = "Gray ribbon shows target coverage ± 2×MCSE; Rows = sample size, Columns = parameter",
    x = "Scenario (Censoring_Weight)",
    y = "Coverage Proportion"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

save_plot(fig2, "fig2_coverage", width = 12, height = 10)

# Figure 3: Bias and RMSE Comparisons -----------------------------------------

# Panel A: Bias
fig3a <- scenario_summaries %>%
  filter(variable != "lp__") %>%
  mutate(
    variable_label = factor(variable,
                           levels = c("alpha", "beta", "gamma"),
                           labels = c("α", "β", "γ"))
  ) %>%
  ggplot(aes(x = variable_label, y = bias, fill = method)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.7, trim = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.2, alpha = 0.5, outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~n_obs, labeller = label_both) +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  labs(
    title = "A) Bias by Parameter and Sample Size",
    x = "Parameter",
    y = "Bias"
  ) +
  theme(legend.position = "none")

# Panel B: RMSE
fig3b <- scenario_summaries %>%
  filter(variable != "lp__") %>%
  mutate(
    variable_label = factor(variable,
                           levels = c("alpha", "beta", "gamma"),
                           labels = c("α", "β", "γ"))
  ) %>%
  ggplot(aes(x = variable_label, y = rmse, fill = method)) +
  geom_violin(position = position_dodge(width = 0.8), alpha = 0.7, trim = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.2, alpha = 0.5, outlier.shape = NA) +
  facet_wrap(~n_obs, labeller = label_both) +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  labs(
    title = "B) RMSE by Parameter and Sample Size",
    x = "Parameter",
    y = "RMSE"
  )

# Combine panels
fig3 <- fig3a / fig3b +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

save_plot(fig3, "fig3_bias_rmse", width = 12, height = 10)

# Figure 4: Sampling Efficiency -----------------------------------------------

fig4_data <- scenario_summaries %>%
  filter(variable == "alpha") %>%
  filter(!is.na(mean_ess_per_sec) & is.finite(mean_ess_per_sec))

fig4 <- fig4_data %>%
  ggplot(aes(x = method, y = mean_ess_per_sec, fill = method)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, outlier.shape = NA) +
  facet_wrap(~n_obs, labeller = label_both, scales = "free_y") +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  scale_x_discrete(labels = method_labels) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Figure 4: Sampling Efficiency (ESS per Second)",
    x = "Method",
    y = "ESS per Second (log scale)"
  ) +
  theme(legend.position = "bottom")

save_plot(fig4, "fig4_efficiency", width = 12, height = 5)

# Figure 5: Runtime Comparisons -----------------------------------------------

fig5_data <- scenario_summaries %>%
  filter(variable == "alpha") %>%
  filter(!is.na(mean_runtime) & is.finite(mean_runtime)) %>%
  mutate(
    censoring_label = paste0("Censoring: ", censoring),
    n_obs_label = paste0("n = ", n_obs)
  )

fig5 <- fig5_data %>%
  ggplot(aes(x = method, y = mean_runtime, fill = method)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(
    width = 0.2,
    alpha = 0.5,
    outlier.shape = NA
  ) +
  scale_fill_manual(
    values = method_colors,
    labels = method_labels,
    name = "Method"
  ) +
  scale_x_discrete(labels = method_labels) +
  scale_y_log10(labels = comma) +
  facet_grid(n_obs_label ~ censoring_label) +
  labs(
    title = "Figure 5: Total Runtime by Sample Size and Censoring",
    subtitle = "Rows = sample size, Columns = censoring level",
    x = "Method",
    y = "Runtime (seconds, log scale)"
  ) +
  theme(legend.position = "bottom")

save_plot(fig5, "fig5_runtime", width = 12, height = 10)

# Figure 6: HMC Diagnostics ---------------------------------------------------

# Panel A: Divergences by sample size
fig6a <- convergence_analysis %>%
  filter(method == "hmc") %>%
  ggplot(aes(x = factor(n_obs), y = mean_divergences)) +
  geom_violin(fill = method_colors["hmc"], alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, outlier.shape = NA) +
  facet_wrap(~n_obs, labeller = label_both, scales = "free_x") +
  labs(
    title = "A) Divergences by Sample Size",
    x = "Sample Size",
    y = "Mean Divergences per Fit"
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Panel B: Scenarios with divergences by sample size
fig6b <- convergence_analysis %>%
  filter(method == "hmc") %>%
  mutate(has_div = n_divergences > 0) %>%
  ggplot(aes(x = factor(n_obs), fill = has_div)) +
  geom_bar(position = "fill", alpha = 0.7) +
  facet_wrap(~n_obs, labeller = label_both, scales = "free_x") +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "lightgreen"),
    labels = c("TRUE" = "With Divergences", "FALSE" = "No Divergences"),
    name = ""
  ) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "B) Proportion with Divergences by Sample Size",
    x = "Sample Size",
    y = "Proportion of Scenarios"
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Combine panels
fig6 <- fig6a / fig6b +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

save_plot(fig6, "fig6_hmc_diagnostics", width = 12, height = 8)

# Figure 7: Parameter Estimate Distributions ----------------------------------

fig7_data <- combined_summaries %>%
  filter(variable != "lp__") %>%
  select(method, variable, n_obs, scenario_id, censoring, weight_type, replicate, mean) %>%
  pivot_wider(
    names_from = method,
    values_from = mean,
    names_glue = "{.value}_{method}"
  ) %>%
  filter(!is.na(mean_hmc) & !is.na(mean_mh))

fig7 <- fig7_data %>%
  mutate(
    variable_label = factor(variable,
                           levels = c("alpha", "beta", "gamma"),
                           labels = c("α (True = 5.0)", "β (True = -0.5)", "γ (True = 1.5)")),
    n_obs_label = paste0("n = ", n_obs)
  ) %>%
  ggplot(aes(x = mean_hmc, y = mean_mh)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.3, size = 1) +
  facet_grid(n_obs_label ~ variable_label, scales = "free") +
  labs(
    title = "Figure 7: Posterior Mean Estimates (HMC vs MH)",
    subtitle = "Red line shows perfect agreement; Rows = sample size, Columns = parameter",
    x = "HMC Posterior Mean",
    y = "MH Posterior Mean"
  ) +
  theme(aspect.ratio = 1)

save_plot(fig7, "fig7_estimates", width = 12, height = 10)

cat("\n")

# Summary Report ---------------------------------------------------------------

cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Visualization Complete!\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

cat("Tables saved to results/tables/:\n")
cat("  ✓ table1_overview.html/.csv\n")
cat("  ✓ table2_convergence.html/.csv\n")
cat("  ✓ table3_accuracy.html/.csv\n")
cat("  ✓ table4_efficiency.html/.csv\n\n")

cat("Figures saved to results/figures/:\n")
cat("  ✓ fig1_convergence.png/.pdf\n")
cat("  ✓ fig2_coverage.png/.pdf\n")
cat("  ✓ fig3_bias_rmse.png/.pdf\n")
cat("  ✓ fig4_efficiency.png/.pdf\n")
cat("  ✓ fig5_runtime.png/.pdf\n")
cat("  ✓ fig6_hmc_diagnostics.png/.pdf\n")
cat("  ✓ fig7_estimates.png/.pdf\n\n")
