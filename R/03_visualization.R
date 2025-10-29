suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(ggridges)
  library(tidybayes)
  library(tidyr)
})

res <- read.csv("data/combined_summaries.csv")
diagnostics <- read.csv("data/combined_diagnostics.csv")

std_method <- function(x) toupper(as.character(x))

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

res <- res %>%
  left_join(
    diagnostics,
    by = c("method", "scenario_id", "replicate", "n_obs", "censoring", "weight")
  ) %>%
  mutate(
    runtime_s = coalesce(runtime_diag, runtime_summary),
    ess_per_sec = coalesce(
      ess_per_sec,
      if_else(!is.na(runtime_s) & runtime_s > 0, ess_bulk / runtime_s, NA_real_)
    ),
    bias = error,
    rmse = sqrt(squared_error)
  ) %>%
  select(-runtime_diag, -runtime_summary)

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
    scenario = if (!"scenario" %in% names(.)) {
      interaction(censoring, weight, sep = ", ")
    } else {
      scenario
    }
  )

pal <- c(HMC = "#2C7FB8", MH = "#F28E2B")

theme_sci <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

p1a <- ggplot(res, aes(rhat, colour = method)) +
  stat_ecdf(geom = "step", linewidth = 0.5) +
  geom_vline(xintercept = 1.01, linetype = 2, colour = "grey40") +
  facet_wrap(~n_obs, nrow = 1) +
  scale_colour_manual(values = pal) +
  labs(
    x = "R-hat",
    y = "ECDF",
    colour = "Method",
    title = "R-hat ECDFs by sample size"
  ) +
  coord_cartesian(xlim = c(1, 1.02)) +
  theme_sci

p1b <- ggplot(
  res,
  aes(x = ess_bulk, y = scenario, fill = method, colour = method)
) +
  geom_density_ridges(alpha = 0.6, scale = 0.95, linewidth = 0.2) +
  facet_wrap(~n_obs, nrow = 1) +
  scale_x_log10() +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  labs(
    x = "Effective sample size (log10)",
    y = "Scenario (Censoring, Weight)",
    fill = "Method",
    colour = "Method",
    title = "ESS distributions by scenario"
  ) +
  theme_sci

cov <- res %>%
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

p2 <- ggplot(
  cov_sum,
  aes(
    x = scnx_id,
    y = coverage,
    colour = method,
    shape = method,
    group = method
  )
) +
  geom_hline(yintercept = 0.95, colour = "grey40") +
  geom_ribbon(
    data = cov_band,
    aes(x = scnx_id, ymin = lo, ymax = hi),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.3
  ) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  facet_grid(n_obs ~ parameter) +
  scale_x_continuous(
    breaks = sort(unique(cov_sum$scnx_id)),
    labels = levels(cov_sum$scnx)
  ) +
  scale_colour_manual(values = pal) +
  labs(
    x = "Scenario (Censoring, Weight)",
    y = "Coverage proportion",
    colour = "Method",
    shape = "Method",
    title = "Coverage of 95% credible intervals with MC uncertainty bands"
  ) +
  theme_sci +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

perf <- res %>%
  filter(!is.na(parameter), !is.na(bias), !is.na(rmse), rmse > 0)

p3_bias <- ggplot(perf, aes(x = method, y = bias, fill = method)) +
  stat_halfeye(
    adjust = 0.6,
    width = 0.6,
    .width = 0.5,
    justification = -0.2,
    slab_alpha = 0.7,
    point_colour = "black"
  ) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
  facet_grid(n_obs ~ parameter) +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "Bias",
    title = "Bias by parameter and sample size"
  ) +
  theme_sci +
  theme(legend.position = "none")

p3_rmse <- ggplot(perf, aes(x = method, y = rmse, fill = method)) +
  stat_halfeye(
    adjust = 0.6,
    width = 0.6,
    .width = 0.5,
    justification = -0.2,
    slab_alpha = 0.7,
    point_colour = "black"
  ) +
  facet_grid(n_obs ~ parameter) +
  scale_y_continuous(trans = "log10") +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "RMSE (log scale)",
    title = "RMSE by parameter and sample size"
  ) +
  theme_sci +
  theme(legend.position = "none")

p4 <- ggplot(res, aes(x = method, y = ess_per_sec, fill = method)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", colour = "black", size = 1.2) +
  facet_wrap(~n_obs, nrow = 1) +
  scale_y_log10() +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "ESS / second (log scale)",
    title = "Sampling efficiency"
  ) +
  theme_sci +
  theme(legend.position = "none")

run_df <- res %>%
  filter(!is.na(runtime_s))

p5 <- ggplot(run_df, aes(x = method, y = runtime_s, fill = method)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", colour = "black", size = 1.2) +
  facet_grid(n_obs + weight ~ censoring) +
  scale_y_log10() +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "Runtime (seconds, log scale)",
    title = "Total runtime by sample size, censoring, and weight"
  ) +
  theme_sci +
  theme(legend.position = "bottom")

est_df <- res %>%
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
  pivot_wider(
    names_from = method_chr,
    values_from = mean,
    names_prefix = "mean_"
  ) %>%
  rename(
    mean_hmc = mean_HMC,
    mean_mh = mean_MH
  ) %>%
  drop_na(mean_hmc, mean_mh)

est_df <- est_df %>%
  mutate(
    n_obs = factor(n_obs, levels = c("n = 200", "n = 2000", "n = 10000"))
  )

p7 <- ggplot(est_df, aes(x = mean_hmc, y = mean_mh)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.5, size = 0.6) +
  facet_grid(n_obs ~ parameter) +
  coord_equal() +
  labs(
    x = "HMC posterior mean",
    y = "MH posterior mean",
    title = "Agreement of posterior means (HMC vs MH)"
  ) +
  theme_sci

scenario_levels <- expand_grid(
  censoring = levels(res$censoring),
  weight = levels(res$weight)
) %>%
  mutate(level = paste(censoring, weight, sep = ", ")) %>%
  pull(level)

ci <- res %>%
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

truth <- res %>%
  distinct(
    parameter = factor(parameter, levels = c("alpha", "beta", "gamma")),
    truth = true_value
  ) %>%
  drop_na(truth)

truth_lines <- tidyr::crossing(
  truth,
  n_obs = levels(res$n_obs)
)

p_ci <- ggplot(
  ci,
  aes(y = scenario, xmin = lower, xmax = upper, colour = method)
) +
  geom_errorbarh(
    aes(alpha = hit),
    height = 0.3,
    position = position_dodge(width = 0.6),
    linewidth = 0.6
  ) +
  geom_point(
    aes(x = est, shape = method, alpha = hit),
    position = position_dodge(width = 0.6),
    size = 1.8,
    stroke = 1
  ) +
  geom_vline(
    data = truth_lines,
    aes(xintercept = truth, linetype = "True value"),
    colour = "grey30",
    linewidth = 0.6
  ) +
  facet_grid(parameter ~ n_obs, scales = "free_y") +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values = c(HMC = 19, MH = 2)) +
  scale_linetype_manual(
    values = c("True value" = "dashed"),
    guide = guide_legend(
      override.aes = list(colour = "grey30"),
      order = 2,
      title = ""
    )
  ) +
  scale_alpha_manual(values = c(`TRUE` = 0.9, `FALSE` = 0.4), guide = "none") +
  labs(
    x = "Estimate / 95% CI",
    y = "Scenario (Censoring, Weight)",
    colour = "Method",
    shape = "Method",
    title = "Credible intervals vs true value"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
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

truth_panel <- expand_grid(
  parameter = levels(ci_sum$parameter),
  n_obs = levels(ci_sum$n_obs),
  method = levels(ci_sum$method)
) %>%
  left_join(
    truth %>% mutate(parameter = as.character(parameter)),
    by = "parameter"
  ) %>%
  drop_na(truth)

p_ci_matrix <- ggplot(
  ci_sum,
  aes(y = scenario, xmin = lower, xmax = upper, colour = method)
) +
  geom_errorbarh(
    height = 0.25,
    linewidth = 0.7
  ) +
  geom_point(
    aes(x = est, shape = cover_flag),
    size = 2.2,
    fill = "white",
    stroke = 0.6
  ) +
  geom_vline(
    data = truth_panel,
    mapping = aes(xintercept = truth),
    colour = "grey40",
    linetype = 2,
    linewidth = 0.7
  ) +
  facet_grid(
    rows = vars(parameter, n_obs),
    cols = vars(method),
    scales = "free_y"
  ) +
  scale_colour_manual(values = pal, drop = FALSE) +
  scale_shape_manual(
    values = c(">=95%" = 16, "<95%" = 1),
    drop = FALSE,
    name = "Coverage"
  ) +
  labs(
    title = "Credible intervals vs true value by parameter, sample size, and method",
    x = "Estimate / 95% CI",
    y = "Scenario (Censoring, Weight)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 8)
  )

ci_detail <- ci %>%
  mutate(width = upper - lower)
precision_summary <- ci_detail %>%
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
p_precision_tradeoff <- ggplot(
  precision_summary,
  aes(x = width_med, y = coverage, colour = method, shape = method)
) +
  geom_hline(yintercept = 0.95, colour = "grey60", linetype = 3) +
  geom_errorbar(
    aes(ymin = coverage_lo, ymax = coverage_hi),
    linewidth = 0.4,
    width = 0
  ) +
  geom_errorbarh(
    aes(xmin = width_med - width_se, xmax = width_med + width_se),
    linewidth = 0.4,
    height = 0.08
  ) +
  geom_point(size = 2) +
  facet_grid(parameter ~ n_obs) +
  scale_colour_manual(values = pal) +
  labs(
    x = "Median CI width",
    y = "Coverage",
    colour = "Method",
    shape = "Method",
    title = "Precision–coverage trade-off"
  ) +
  theme_sci +
  theme(legend.position = "bottom")
coverage_heatmap <- precision_summary %>%
  mutate(
    coverage_bias = coverage - 0.95,
    panel_id = interaction(parameter, n_obs, sep = " | ")
  )
p_coverage_heatmap <- ggplot(
  coverage_heatmap,
  aes(x = scenario, y = panel_id, fill = coverage_bias)
) +
  geom_tile(colour = "white") +
  facet_wrap(~method, nrow = 1) +
  scale_fill_gradient2(
    limits = c(-0.1, 0.1),
    oob = scales::squish,
    low = "#B2182B",
    mid = "#f7f7f7",
    high = "#2166AC",
    midpoint = 0,
    name = "Bias"
  ) +
  labs(
    x = "Scenario (Censoring, Weight)",
    y = "Parameter | Sample size",
    title = "Global bias in coverage"
  ) +
  theme_sci +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )
p_ci_width_violin <- ggplot(
  ci_detail,
  aes(x = method, y = width, fill = method)
) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(
    fun = median,
    geom = "point",
    colour = "black",
    size = 1
  ) +
  facet_grid(parameter ~ n_obs) +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "CI width",
    fill = "Method",
    title = "Distribution of credible interval widths"
  ) +
  theme_sci +
  theme(legend.position = "bottom")

save_fig <- function(p, file, w = 8, h = 4, dpi = 320) {
  ggsave(
    filename = file,
    plot = p,
    width = w,
    height = h,
    dpi = dpi,
    bg = "white"
  )
}

save_fig(p1a, "results/figures/fig1a_rhat_ecdf.png", w = 9, h = 3.2)
save_fig(p1b, "results/figures/fig1b_ess_ridges.png", w = 9, h = 3.2)
save_fig(p2, "results/figures/fig2_coverage.png", w = 10, h = 6)
save_fig(p3_bias, "results/figures/fig3a_bias.png", w = 9, h = 6)
save_fig(p3_rmse, "results/figures/fig3b_rmse.png", w = 9, h = 6)
save_fig(p4, "results/figures/fig4_ess_per_sec.png", w = 9, h = 3.2)
save_fig(p5, "results/figures/fig5_runtime.png", w = 10, h = 6)
save_fig(p7, "results/figures/fig7_means_agreement.png", w = 9, h = 6)
save_fig(p_ci, "results/figures/figA1_cis_vs_truth.png", w = 11, h = 7)
save_fig(p_ci_matrix, "results/figures/figA2_ci_matrix.png", w = 10, h = 12)
save_fig(
  p_precision_tradeoff,
  "results/figures/figB1_precision_tradeoff.png",
  w = 10,
  h = 6
)
save_fig(
  p_coverage_heatmap,
  "results/figures/figB2_coverage_bias_heatmap.png",
  w = 10,
  h = 5
)
save_fig(
  p_ci_width_violin,
  "results/figures/figB3_ci_width_violin.png",
  w = 9,
  h = 6
)
