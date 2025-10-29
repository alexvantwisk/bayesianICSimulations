#!/usr/bin/env Rscript
# ==============================================================================
# 02_analysis.R
#
# Purpose: Perform comprehensive statistical analysis of HMC vs MH results
#          following MSc Protocol objectives and ADEMP framework
#
# Inputs:  data/combined_summaries.rds
#          data/combined_diagnostics.rds
#          data/scenario_metadata.rds
#
# Outputs: results/scenario_summaries.csv
#          results/convergence_analysis.csv
#          results/efficiency_comparisons.csv
#          results/statistical_tests.rds
#
# Author: Alexander van Twisk
# Date: 2025-10-24
# ==============================================================================

# Load required packages -------------------------------------------------------
library(tidyverse)
library(broom)

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
data_dir <- file.path(base_dir, "data")
results_dir <- file.path(base_dir, "results", "analysis")

# Create output directories
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  cat("Created directory:", results_dir, "\n")
}

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Statistical Analysis: HMC vs MH Comparison\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

# Load Data --------------------------------------------------------------------
cat("Loading processed data...\n")

combined_summaries <- readRDS(file.path(data_dir, "combined_summaries.rds")) %>%
  dplyr::filter(variable %in% c("alpha", "beta", "gamma"))
combined_diagnostics <- readRDS(file.path(data_dir, "combined_diagnostics.rds"))
scenario_metadata <- readRDS(file.path(data_dir, "scenario_metadata.rds"))
study_design <- readRDS(file.path(data_dir, "study_design.rds"))

cat(sprintf("  ✓ Loaded %d summary records\n", nrow(combined_summaries)))
cat(sprintf("  ✓ Loaded %d diagnostic records\n", nrow(combined_diagnostics)))
cat(sprintf("  ✓ Loaded %d scenarios\n", nrow(scenario_metadata)))
cat(sprintf(
  "  ✓ Loaded study design (expected: %d replicates/scenario)\n\n",
  study_design$replicates_per_scenario
))

# MCSE Calculation Functions ---------------------------------------------------

#' Calculate MCSE for mean/bias
#' MCSE = sd(estimates) / sqrt(n)
mcse_mean <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n <= 1) {
    return(NA_real_)
  }
  sd(x) / sqrt(n)
}

#' Calculate MCSE for proportion (e.g., coverage)
#' MCSE = sqrt(p * (1-p) / n)
mcse_prop <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }
  p <- mean(x)
  sqrt(p * (1 - p) / n)
}

#' Calculate bias and RMSE with MCSEs
calc_bias_rmse <- function(estimates, truth) {
  estimates <- estimates[is.finite(estimates)]
  n <- length(estimates)

  if (n == 0) {
    return(tibble(
      bias = NA_real_,
      bias_mcse = NA_real_,
      rmse = NA_real_,
      rmse_mcse = NA_real_
    ))
  }

  errors <- estimates - truth
  bias <- mean(errors)
  rmse <- sqrt(mean(errors^2))

  # MCSE for bias is sd(errors)/sqrt(n)
  bias_mcse <- sd(errors) / sqrt(n)

  # MCSE for RMSE (approximation)
  rmse_mcse <- sd((errors^2 - rmse^2)) / (2 * rmse * sqrt(n))

  tibble(
    bias = bias,
    bias_mcse = bias_mcse,
    rmse = rmse,
    rmse_mcse = rmse_mcse
  )
}

# 1. CONVERGENCE ANALYSIS ------------------------------------------------------

cat("1. Analyzing convergence diagnostics...\n")

convergence_analysis <- combined_diagnostics %>%
  group_by(method, n_obs, scenario_id, censoring, weight_type) %>%
  summarise(
    n_observed = n(), # Actual observations present
    n_expected = study_design$replicates_per_scenario, # FIXED: Always 200
    n_missing = n_expected - n_observed,
    n_converged = sum(converged, na.rm = TRUE),
    n_rhat_ok = sum(rhat_ok, na.rm = TRUE),
    n_ess_ok = sum(ess_ok, na.rm = TRUE),
    pct_converged = 100 * (n_converged / n_expected), # FIXED: % of expected
    pct_complete = 100 * (n_observed / n_expected), # NEW: Completion rate
    mean_rhat = mean(max_rhat, na.rm = TRUE),
    median_rhat = median(max_rhat, na.rm = TRUE),
    max_rhat_obs = max(max_rhat, na.rm = TRUE),
    mean_ess = mean(min_ess, na.rm = TRUE),
    median_ess = median(min_ess, na.rm = TRUE),
    min_ess_obs = min(min_ess, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    scenario_metadata,
    by = c("scenario_id", "n_obs", "censoring", "weight_type")
  )

# Add HMC-specific convergence issues
hmc_issues <- combined_diagnostics %>%
  filter(method == "hmc") %>%
  group_by(n_obs, scenario_id, censoring, weight_type) %>%
  summarise(
    n_divergences = sum(divergences > 0, na.rm = TRUE),
    mean_divergences = mean(divergences, na.rm = TRUE),
    n_treedepth_issues = sum(max_treedepth_hit > 0, na.rm = TRUE),
    .groups = "drop"
  )

convergence_analysis <- convergence_analysis %>%
  left_join(
    hmc_issues,
    by = c("scenario_id", "n_obs", "censoring", "weight_type")
  )

cat(sprintf(
  "  ✓ Convergence analysis complete for %d scenarios\n",
  nrow(convergence_analysis)
))

# Overall convergence summary
cat("\nOverall Convergence Rates:\n")
overall_convergence <- combined_diagnostics %>%
  group_by(method) %>%
  summarise(
    n_observed = n(),
    n_expected = study_design$total_datasets, # FIXED: Always 3600
    n_missing = n_expected - n_observed,
    converged = sum(converged, na.rm = TRUE),
    pct_observed = 100 * (n_observed / n_expected),
    pct_converged = 100 * (converged / n_expected) # FIXED: % of expected
  )
print(overall_convergence)

# 2. SAMPLING EFFICIENCY ANALYSIS ----------------------------------------------

cat("\n2. Analyzing sampling efficiency...\n")

efficiency_analysis <- combined_diagnostics %>%
  group_by(method, n_obs, scenario_id, censoring, weight_type) %>%
  summarise(
    # ESS metrics
    mean_min_ess = mean(min_ess, na.rm = TRUE),
    median_min_ess = median(min_ess, na.rm = TRUE),
    sd_min_ess = sd(min_ess, na.rm = TRUE),

    # ESS per second (efficiency)
    mean_ess_per_sec = mean(mean_ess_per_sec, na.rm = TRUE),
    median_ess_per_sec = median(mean_ess_per_sec, na.rm = TRUE),
    sd_ess_per_sec = sd(mean_ess_per_sec, na.rm = TRUE),

    # Runtime
    mean_runtime = mean(total_time_sec, na.rm = TRUE),
    median_runtime = median(total_time_sec, na.rm = TRUE),
    sd_runtime = sd(total_time_sec, na.rm = TRUE),

    n = n(),
    .groups = "drop"
  ) %>%
  left_join(
    scenario_metadata,
    by = c("scenario_id", "n_obs", "censoring", "weight_type")
  )

cat(sprintf(
  "  ✓ Efficiency analysis complete for %d scenarios\n",
  nrow(efficiency_analysis)
))

# Calculate relative efficiency (HMC vs MH)
efficiency_comparison <- efficiency_analysis %>%
  select(
    scenario_id,
    n_obs,
    censoring,
    weight_type,
    method,
    mean_ess_per_sec,
    mean_runtime
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = c(mean_ess_per_sec, mean_runtime),
    names_glue = "{.value}_{method}"
  ) %>%
  mutate(
    ess_per_sec_ratio = mean_ess_per_sec_hmc / mean_ess_per_sec_mh,
    runtime_ratio = mean_runtime_hmc / mean_runtime_mh
  )

cat("\nMean ESS/sec Ratio (HMC / MH) by sample size:\n")
ess_ratio_summary <- efficiency_comparison %>%
  group_by(n_obs) %>%
  summarise(
    mean_ratio = mean(ess_per_sec_ratio, na.rm = TRUE),
    median_ratio = median(ess_per_sec_ratio, na.rm = TRUE)
  )
print(ess_ratio_summary)

# 3. POSTERIOR ACCURACY ANALYSIS -----------------------------------------------

cat("\n3. Analyzing posterior accuracy (bias, RMSE, coverage)...\n")

# Calculate metrics for ALL fits
accuracy_all <- combined_summaries %>%
  group_by(method, n_obs, scenario_id, censoring, weight_type, variable) %>%
  summarise(
    n_observed = n(), # Actual replicates present
    n_expected = study_design$replicates_per_scenario, # FIXED: Always 200

    # Bias and RMSE (using observed data but expected n for MCSE)
    {
      estimates <- mean[is.finite(mean)]
      truth <- first(true_value)
      errors <- estimates - truth
      n_actual <- length(estimates)

      # Use expected n for MCSE, not observed n (FIXED)
      tibble(
        bias = mean(errors),
        bias_mcse = if (n_actual > 1) {
          sd(errors) / sqrt(n_expected)
        } else {
          NA_real_
        },
        rmse = sqrt(mean(errors^2)),
        rmse_mcse = if (n_actual > 1) {
          sd((errors^2 - sqrt(mean(errors^2))^2)) /
            (2 * sqrt(mean(errors^2)) * sqrt(n_expected))
        } else {
          NA_real_
        }
      )
    },

    # Coverage (use expected n for MCSE)
    coverage = mean(contains_truth, na.rm = TRUE),
    coverage_mcse = sqrt(coverage * (1 - coverage) / n_expected), # FIXED

    # Credible interval width
    mean_ci_width = mean(ci_width, na.rm = TRUE),
    median_ci_width = median(ci_width, na.rm = TRUE),

    .groups = "drop"
  )

# Calculate metrics for CONVERGED fits only
accuracy_converged <- combined_summaries %>%
  left_join(
    combined_diagnostics %>%
      select(
        method,
        scenario_id,
        replicate,
        n_obs,
        censoring,
        weight_type,
        converged
      ),
    by = c(
      "method",
      "scenario_id",
      "replicate",
      "n_obs",
      "censoring",
      "weight_type"
    )
  ) %>%
  filter(converged) %>%
  group_by(method, n_obs, scenario_id, censoring, weight_type, variable) %>%
  summarise(
    n_converged_obs = n(), # Actual converged observations
    # Note: n_expected will come from accuracy_all join, no need to duplicate

    # Bias and RMSE (converged only, but use expected n for MCSE)
    {
      estimates <- mean[is.finite(mean)]
      truth <- first(true_value)
      errors <- estimates - truth
      n_actual <- length(estimates)
      n_exp <- study_design$replicates_per_scenario # Local variable

      # Use expected n for MCSE, not actual converged (FIXED)
      tibble(
        bias_converged = mean(errors),
        bias_mcse_converged = if (n_actual > 1) {
          sd(errors) / sqrt(n_exp)
        } else {
          NA_real_
        },
        rmse_converged = sqrt(mean(errors^2)),
        rmse_mcse_converged = if (n_actual > 1) {
          sd((errors^2 - sqrt(mean(errors^2))^2)) /
            (2 * sqrt(mean(errors^2)) * sqrt(n_exp))
        } else {
          NA_real_
        }
      )
    },

    # Coverage (converged only, use expected n for MCSE)
    {
      n_exp <- study_design$replicates_per_scenario # Local variable
      cov <- mean(contains_truth, na.rm = TRUE)
      tibble(
        coverage_converged = cov,
        coverage_mcse_converged = sqrt(cov * (1 - cov) / n_exp) # FIXED
      )
    },

    # CI width (converged only)
    mean_ci_width_converged = mean(ci_width, na.rm = TRUE),

    .groups = "drop"
  )

# Combine all and converged metrics
accuracy_analysis <- accuracy_all %>%
  left_join(
    accuracy_converged,
    by = c(
      "method",
      "n_obs",
      "scenario_id",
      "censoring",
      "weight_type",
      "variable"
    )
  ) %>%
  left_join(
    scenario_metadata,
    by = c("scenario_id", "n_obs", "censoring", "weight_type")
  ) %>%
  mutate(
    # Exclusion rate: (expected - converged) / expected (FIXED)
    n_failed = n_expected - n_converged_obs,
    exclusion_rate = n_failed / n_expected,
    exclusion_rate = if_else(is.na(exclusion_rate), 1, exclusion_rate)
  )

cat(sprintf(
  "  ✓ Accuracy analysis complete for %d parameter × scenario combinations\n",
  nrow(accuracy_analysis)
))

# Summary of coverage by parameter
cat("\nMean Coverage Rates by Parameter and Method:\n")
coverage_summary <- accuracy_analysis %>%
  group_by(variable, method) %>%
  summarise(
    mean_coverage_all = mean(coverage, na.rm = TRUE),
    mean_coverage_converged = mean(coverage_converged, na.rm = TRUE),
    .groups = "drop"
  )
print(coverage_summary)

# 4. STATISTICAL TESTS ---------------------------------------------------------

cat("\n4. Performing statistical tests (HMC vs MH)...\n")

# For paired comparisons, we need datasets that have both HMC and MH results
# Group by scenario, n_obs, replicate, variable and compare

# Prepare paired data for ESS/sec
paired_ess <- combined_diagnostics %>%
  select(
    method,
    scenario_id,
    replicate,
    n_obs,
    censoring,
    weight_type,
    mean_ess_per_sec
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = mean_ess_per_sec,
    names_prefix = "ess_per_sec_"
  ) %>%
  filter(!is.na(ess_per_sec_hmc) & !is.na(ess_per_sec_mh))

# Prepare paired data for bias (by parameter)
paired_bias <- combined_summaries %>%
  select(
    method,
    scenario_id,
    replicate,
    n_obs,
    censoring,
    weight_type,
    variable,
    error
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = error,
    names_prefix = "bias_"
  ) %>%
  filter(!is.na(bias_hmc) & !is.na(bias_mh))

# Wilcoxon signed-rank tests
statistical_tests <- list()

# Test 1: ESS per second
if (nrow(paired_ess) > 0) {
  ess_test <- wilcox.test(
    paired_ess$ess_per_sec_hmc,
    paired_ess$ess_per_sec_mh,
    paired = TRUE,
    alternative = "two.sided"
  )

  statistical_tests$ess_per_sec <- tibble(
    test = "ESS per second",
    n_pairs = nrow(paired_ess),
    statistic = ess_test$statistic,
    p_value = ess_test$p.value,
    mean_diff = mean(paired_ess$ess_per_sec_hmc - paired_ess$ess_per_sec_mh),
    median_hmc = median(paired_ess$ess_per_sec_hmc),
    median_mh = median(paired_ess$ess_per_sec_mh)
  )
}

# Test 2: Absolute bias by parameter
bias_tests <- paired_bias %>%
  group_by(variable) %>%
  summarise(
    n_pairs = n(),
    test_result = list(wilcox.test(
      abs(bias_hmc),
      abs(bias_mh),
      paired = TRUE,
      alternative = "two.sided"
    )),
    .groups = "drop"
  ) %>%
  mutate(
    statistic = map_dbl(test_result, ~ .$statistic),
    p_value = map_dbl(test_result, ~ .$p.value),
    mean_abs_bias_hmc = map_dbl(variable, function(v) {
      mean(abs(paired_bias$bias_hmc[paired_bias$variable == v]))
    }),
    mean_abs_bias_mh = map_dbl(variable, function(v) {
      mean(abs(paired_bias$bias_mh[paired_bias$variable == v]))
    })
  ) %>%
  select(-test_result) %>%
  mutate(test = paste("Absolute bias -", variable))

statistical_tests$bias <- bias_tests

# Calculate effect sizes (Cohen's d)
cohen_d <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  pooled_sd <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  (mean(x) - mean(y)) / pooled_sd
}

effect_sizes <- combined_diagnostics %>%
  group_by(n_obs) %>%
  summarise(
    cohens_d_ess = cohen_d(
      mean_ess_per_sec[method == "hmc"],
      mean_ess_per_sec[method == "mh"]
    ),
    cohens_d_runtime = cohen_d(
      total_time_sec[method == "hmc"],
      total_time_sec[method == "mh"]
    ),
    .groups = "drop"
  )

statistical_tests$effect_sizes <- effect_sizes

cat("  ✓ Statistical tests complete\n")

# 5. SCENARIO-LEVEL SUMMARY ----------------------------------------------------

cat("\n5. Creating scenario-level summary...\n")

# Combine all metrics into one comprehensive table
scenario_summaries <- accuracy_analysis %>%
  left_join(
    convergence_analysis %>%
      select(
        method,
        n_obs,
        scenario_id,
        censoring,
        weight_type,
        # Note: n_observed and n_expected already in accuracy_analysis, so skip them
        n_missing,
        pct_complete,
        n_converged_diag = n_converged, # Rename to avoid ambiguity
        pct_converged,
        mean_rhat,
        mean_ess,
        n_divergences,
        n_treedepth_issues
      ),
    by = c("method", "n_obs", "scenario_id", "censoring", "weight_type")
  ) %>%
  left_join(
    efficiency_analysis %>%
      select(
        method,
        n_obs,
        scenario_id,
        censoring,
        weight_type,
        mean_ess_per_sec,
        median_ess_per_sec,
        mean_runtime
      ),
    by = c("method", "n_obs", "scenario_id", "censoring", "weight_type")
  ) %>%
  select(
    # Identifiers
    method,
    n_obs,
    scenario_id,
    censoring,
    weight_type,
    scenario_label,
    variable,

    # Sample sizes (UPDATED column names)
    n_observed, # Actual replicates present (from accuracy_all)
    n_expected, # Expected replicates (200) (from accuracy_all)
    n_missing, # Missing replicates (from convergence_analysis)
    pct_complete, # % of expected that are present (from convergence_analysis)
    n_converged_obs, # Actual converged (from accuracy_converged)
    pct_converged, # % of expected that converged (from convergence_analysis)
    n_failed, # Expected - converged (calculated in accuracy_analysis)
    exclusion_rate, # % failed or missing (calculated in accuracy_analysis)

    # Convergence
    mean_rhat,
    mean_ess,

    # Efficiency
    mean_ess_per_sec,
    median_ess_per_sec,
    mean_runtime,

    # Accuracy - All fits
    bias,
    bias_mcse,
    rmse,
    rmse_mcse,
    coverage,
    coverage_mcse,
    mean_ci_width,

    # Accuracy - Converged only
    bias_converged,
    bias_mcse_converged,
    rmse_converged,
    rmse_mcse_converged,
    coverage_converged,
    coverage_mcse_converged,
    mean_ci_width_converged,

    # HMC-specific
    n_divergences,
    n_treedepth_issues
  ) %>%
  arrange(method, n_obs, scenario_id, variable)

cat(sprintf(
  "  ✓ Scenario summary complete with %d rows\n",
  nrow(scenario_summaries)
))

# Save Results -----------------------------------------------------------------

cat("\nSaving analysis results...\n")

# Save scenario summaries
write_csv(scenario_summaries, file.path(results_dir, "scenario_summaries.csv"))
saveRDS(scenario_summaries, file.path(results_dir, "scenario_summaries.rds"))
cat("  ✓ Saved scenario_summaries.csv/.rds\n")

# Save convergence analysis
write_csv(
  convergence_analysis,
  file.path(results_dir, "convergence_analysis.csv")
)
cat("  ✓ Saved convergence_analysis.csv\n")

# Save efficiency comparisons
write_csv(
  efficiency_comparison,
  file.path(results_dir, "efficiency_comparisons.csv")
)
cat("  ✓ Saved efficiency_comparisons.csv\n")

# Save statistical tests
saveRDS(statistical_tests, file.path(results_dir, "statistical_tests.rds"))
cat("  ✓ Saved statistical_tests.rds\n")

# Print key findings -----------------------------------------------------------

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Key Findings Summary\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

cat("1. CONVERGENCE:\n")
cat(sprintf(
  "   HMC: %.1f%% observed, %.1f%% converged (n=%d/%d expected)\n",
  overall_convergence$pct_observed[overall_convergence$method == "hmc"],
  overall_convergence$pct_converged[overall_convergence$method == "hmc"],
  overall_convergence$converged[overall_convergence$method == "hmc"],
  overall_convergence$n_expected[overall_convergence$method == "hmc"]
))
cat(sprintf(
  "   MH:  %.1f%% observed, %.1f%% converged (n=%d/%d expected)\n",
  overall_convergence$pct_observed[overall_convergence$method == "mh"],
  overall_convergence$pct_converged[overall_convergence$method == "mh"],
  overall_convergence$converged[overall_convergence$method == "mh"],
  overall_convergence$n_expected[overall_convergence$method == "mh"]
))

cat("\n2. EFFICIENCY (ESS/sec):\n")
for (i in 1:nrow(ess_ratio_summary)) {
  cat(sprintf(
    "   n=%d: HMC %.2fx faster than MH (mean ratio)\n",
    ess_ratio_summary$n_obs[i],
    ess_ratio_summary$mean_ratio[i]
  ))
}

if (!is.null(statistical_tests$ess_per_sec)) {
  cat(sprintf(
    "\n   Wilcoxon test: p = %.4f (n=%d pairs)\n",
    statistical_tests$ess_per_sec$p_value,
    statistical_tests$ess_per_sec$n_pairs
  ))
}

cat("\n3. COVERAGE (95% CrIs):\n")
for (i in 1:nrow(coverage_summary)) {
  cat(sprintf(
    "   %s (%s): %.3f (all), %.3f (converged)\n",
    coverage_summary$variable[i],
    coverage_summary$method[i],
    coverage_summary$mean_coverage_all[i],
    coverage_summary$mean_coverage_converged[i]
  ))
}

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Analysis complete!\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

cat("Output files in results/analysis/:\n")
cat("  - scenario_summaries.csv/.rds\n")
cat("  - convergence_analysis.csv\n")
cat("  - efficiency_comparisons.csv\n")
cat("  - statistical_tests.rds\n\n")
