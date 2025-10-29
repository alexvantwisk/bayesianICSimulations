#' Perform Statistical Analysis of HMC vs MH Results
#'
#' Comprehensive statistical comparison of HMC and MH simulation results
#' following the ADEMP framework (Aims, Data-generating mechanisms, Estimands,
#' Methods, Performance measures).
#'
#' @param data_dir Character string. Path to directory containing combined
#'   datasets from \code{\link{combine_results}}. Default: "data"
#' @param output_dir Character string. Path to directory where analysis results
#'   will be saved. Default: "outputs/analysis"
#' @param verbose Logical. Print progress messages? Default: TRUE
#'
#' @return Invisibly returns a list with four elements:
#'   \item{scenario_summaries}{Tibble with comprehensive scenario-level metrics}
#'   \item{convergence_analysis}{Tibble with convergence diagnostics by scenario}
#'   \item{efficiency_comparison}{Tibble with ESS/sec and runtime comparisons}
#'   \item{statistical_tests}{List with paired statistical test results}
#'
#' @details
#' This function performs:
#' \enumerate{
#'   \item Convergence analysis (Rhat, ESS, divergences)
#'   \item Sampling efficiency analysis (ESS/sec, runtime)
#'   \item Posterior accuracy analysis (bias, RMSE, coverage)
#'   \item Statistical tests (Wilcoxon signed-rank, effect sizes)
#'   \item Scenario-level summaries combining all metrics
#' }
#'
#' Saves four files to output_dir:
#' \itemize{
#'   \item scenario_summaries.csv/.rds
#'   \item convergence_analysis.csv
#'   \item efficiency_comparisons.csv
#'   \item statistical_tests.rds
#' }
#'
#' @seealso \code{\link{combine_results}} for preparing input data
#'
#' @examples
#' \dontrun{
#' # Basic usage (after running combine_results())
#' analysis <- perform_statistical_analysis()
#'
#' # Custom directories
#' analysis <- perform_statistical_analysis(
#'   data_dir = "my_data",
#'   output_dir = "my_outputs/analysis"
#' )
#'
#' # Access results
#' scenario_summaries <- analysis$scenario_summaries
#' tests <- analysis$statistical_tests
#' }
#'
#' @export
perform_statistical_analysis <- function(data_dir = "data",
                                          output_dir = "outputs/analysis",
                                          verbose = TRUE) {
  # Load required packages
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("Package 'tidyverse' is required but not installed.")
  }
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop("Package 'broom' is required but not installed.")
  }

  library(tidyverse)
  library(broom)

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) cat("Created directory:", output_dir, "\n")
  }

  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 78), collapse = ""), "\n")
    cat("Statistical Analysis: HMC vs MH Comparison\n")
    cat(paste(rep("=", 78), collapse = ""), "\n\n")
    cat("Loading processed data...\n")
  }

  # Load data
  combined_summaries <- readRDS(file.path(data_dir, "combined_summaries.rds")) %>%
    dplyr::filter(variable %in% c("alpha", "beta", "gamma"))
  combined_diagnostics <- readRDS(file.path(data_dir, "combined_diagnostics.rds"))
  scenario_metadata <- readRDS(file.path(data_dir, "scenario_metadata.rds"))
  study_design <- readRDS(file.path(data_dir, "study_design.rds"))

  if (verbose) {
    cat(sprintf("  ✓ Loaded %d summary records\n", nrow(combined_summaries)))
    cat(sprintf("  ✓ Loaded %d diagnostic records\n", nrow(combined_diagnostics)))
    cat(sprintf("  ✓ Loaded %d scenarios\n", nrow(scenario_metadata)))
    cat(sprintf(
      "  ✓ Loaded study design (expected: %d replicates/scenario)\n\n",
      study_design$replicates_per_scenario
    ))
  }

  # 1. CONVERGENCE ANALYSIS
  if (verbose) cat("1. Analyzing convergence diagnostics...\n")

  convergence_analysis <- combined_diagnostics %>%
    group_by(method, n_obs, scenario_id, censoring, weight_type) %>%
    summarise(
      n_observed = n(),
      n_expected = study_design$replicates_per_scenario,
      n_missing = n_expected - n_observed,
      n_converged = sum(converged, na.rm = TRUE),
      n_rhat_ok = sum(rhat_ok, na.rm = TRUE),
      n_ess_ok = sum(ess_ok, na.rm = TRUE),
      pct_converged = 100 * (n_converged / n_expected),
      pct_complete = 100 * (n_observed / n_expected),
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

  # Add HMC-specific issues
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

  if (verbose) {
    cat(sprintf(
      "  ✓ Convergence analysis complete for %d scenarios\n",
      nrow(convergence_analysis)
    ))

    cat("\nOverall Convergence Rates:\n")
    overall_convergence <- combined_diagnostics %>%
      group_by(method) %>%
      summarise(
        n_observed = n(),
        n_expected = study_design$total_datasets,
        n_missing = n_expected - n_observed,
        converged = sum(converged, na.rm = TRUE),
        pct_observed = 100 * (n_observed / n_expected),
        pct_converged = 100 * (converged / n_expected)
      )
    print(overall_convergence)
  }

  # 2. SAMPLING EFFICIENCY ANALYSIS
  if (verbose) cat("\n2. Analyzing sampling efficiency...\n")

  efficiency_analysis <- combined_diagnostics %>%
    group_by(method, n_obs, scenario_id, censoring, weight_type) %>%
    summarise(
      mean_min_ess = mean(min_ess, na.rm = TRUE),
      median_min_ess = median(min_ess, na.rm = TRUE),
      sd_min_ess = sd(min_ess, na.rm = TRUE),
      mean_ess_per_sec = mean(mean_ess_per_sec, na.rm = TRUE),
      median_ess_per_sec = median(mean_ess_per_sec, na.rm = TRUE),
      sd_ess_per_sec = sd(mean_ess_per_sec, na.rm = TRUE),
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

  if (verbose) {
    cat(sprintf(
      "  ✓ Efficiency analysis complete for %d scenarios\n",
      nrow(efficiency_analysis)
    ))
  }

  # Calculate relative efficiency
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

  if (verbose) {
    cat("\nMean ESS/sec Ratio (HMC / MH) by sample size:\n")
    ess_ratio_summary <- efficiency_comparison %>%
      group_by(n_obs) %>%
      summarise(
        mean_ratio = mean(ess_per_sec_ratio, na.rm = TRUE),
        median_ratio = median(ess_per_sec_ratio, na.rm = TRUE)
      )
    print(ess_ratio_summary)
  }

  # 3. POSTERIOR ACCURACY ANALYSIS
  if (verbose) cat("\n3. Analyzing posterior accuracy (bias, RMSE, coverage)...\n")

  # All fits
  accuracy_all <- combined_summaries %>%
    group_by(method, n_obs, scenario_id, censoring, weight_type, variable) %>%
    summarise(
      n_observed = n(),
      n_expected = study_design$replicates_per_scenario,
      {
        estimates <- mean[is.finite(mean)]
        truth <- first(true_value)
        errors <- estimates - truth
        n_actual <- length(estimates)

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
      coverage = mean(contains_truth, na.rm = TRUE),
      coverage_mcse = sqrt(coverage * (1 - coverage) / n_expected),
      mean_ci_width = mean(ci_width, na.rm = TRUE),
      median_ci_width = median(ci_width, na.rm = TRUE),
      .groups = "drop"
    )

  # Converged fits only
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
      n_converged_obs = n(),
      {
        estimates <- mean[is.finite(mean)]
        truth <- first(true_value)
        errors <- estimates - truth
        n_actual <- length(estimates)
        n_exp <- study_design$replicates_per_scenario

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
      {
        n_exp <- study_design$replicates_per_scenario
        cov <- mean(contains_truth, na.rm = TRUE)
        tibble(
          coverage_converged = cov,
          coverage_mcse_converged = sqrt(cov * (1 - cov) / n_exp)
        )
      },
      mean_ci_width_converged = mean(ci_width, na.rm = TRUE),
      .groups = "drop"
    )

  # Combine metrics
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
      n_failed = n_expected - n_converged_obs,
      exclusion_rate = n_failed / n_expected,
      exclusion_rate = if_else(is.na(exclusion_rate), 1, exclusion_rate)
    )

  if (verbose) {
    cat(sprintf(
      "  ✓ Accuracy analysis complete for %d parameter × scenario combinations\n",
      nrow(accuracy_analysis)
    ))

    cat("\nMean Coverage Rates by Parameter and Method:\n")
    coverage_summary <- accuracy_analysis %>%
      group_by(variable, method) %>%
      summarise(
        mean_coverage_all = mean(coverage, na.rm = TRUE),
        mean_coverage_converged = mean(coverage_converged, na.rm = TRUE),
        .groups = "drop"
      )
    print(coverage_summary)
  }

  # 4. STATISTICAL TESTS
  if (verbose) cat("\n4. Performing statistical tests (HMC vs MH)...\n")

  # Paired ESS/sec data
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

  # Paired bias data
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

  # Test 2: Absolute bias
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

  # Effect sizes
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

  if (verbose) cat("  ✓ Statistical tests complete\n")

  # 5. SCENARIO-LEVEL SUMMARY
  if (verbose) cat("\n5. Creating scenario-level summary...\n")

  scenario_summaries <- accuracy_analysis %>%
    left_join(
      convergence_analysis %>%
        select(
          method,
          n_obs,
          scenario_id,
          censoring,
          weight_type,
          n_missing,
          pct_complete,
          n_converged_diag = n_converged,
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
      method,
      n_obs,
      scenario_id,
      censoring,
      weight_type,
      scenario_label,
      variable,
      n_observed,
      n_expected,
      n_missing,
      pct_complete,
      n_converged_obs,
      pct_converged,
      n_failed,
      exclusion_rate,
      mean_rhat,
      mean_ess,
      mean_ess_per_sec,
      median_ess_per_sec,
      mean_runtime,
      bias,
      bias_mcse,
      rmse,
      rmse_mcse,
      coverage,
      coverage_mcse,
      mean_ci_width,
      bias_converged,
      bias_mcse_converged,
      rmse_converged,
      rmse_mcse_converged,
      coverage_converged,
      coverage_mcse_converged,
      mean_ci_width_converged,
      n_divergences,
      n_treedepth_issues
    ) %>%
    arrange(method, n_obs, scenario_id, variable)

  if (verbose) {
    cat(sprintf(
      "  ✓ Scenario summary complete with %d rows\n",
      nrow(scenario_summaries)
    ))
  }

  # SAVE RESULTS
  if (verbose) cat("\nSaving analysis results...\n")

  write_csv(scenario_summaries, file.path(output_dir, "scenario_summaries.csv"))
  saveRDS(scenario_summaries, file.path(output_dir, "scenario_summaries.rds"))
  if (verbose) cat("  ✓ Saved scenario_summaries.csv/.rds\n")

  write_csv(
    convergence_analysis,
    file.path(output_dir, "convergence_analysis.csv")
  )
  if (verbose) cat("  ✓ Saved convergence_analysis.csv\n")

  write_csv(
    efficiency_comparison,
    file.path(output_dir, "efficiency_comparisons.csv")
  )
  if (verbose) cat("  ✓ Saved efficiency_comparisons.csv\n")

  saveRDS(statistical_tests, file.path(output_dir, "statistical_tests.rds"))
  if (verbose) cat("  ✓ Saved statistical_tests.rds\n")

  # PRINT KEY FINDINGS
  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 78), collapse = ""), "\n")
    cat("Key Findings Summary\n")
    cat(paste(rep("=", 78), collapse = ""), "\n\n")

    overall_convergence <- combined_diagnostics %>%
      group_by(method) %>%
      summarise(
        n_observed = n(),
        n_expected = study_design$total_datasets,
        converged = sum(converged, na.rm = TRUE),
        pct_observed = 100 * (n_observed / n_expected),
        pct_converged = 100 * (converged / n_expected)
      )

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

    ess_ratio_summary <- efficiency_comparison %>%
      group_by(n_obs) %>%
      summarise(
        mean_ratio = mean(ess_per_sec_ratio, na.rm = TRUE),
        median_ratio = median(ess_per_sec_ratio, na.rm = TRUE)
      )

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

    coverage_summary <- accuracy_analysis %>%
      group_by(variable, method) %>%
      summarise(
        mean_coverage_all = mean(coverage, na.rm = TRUE),
        mean_coverage_converged = mean(coverage_converged, na.rm = TRUE),
        .groups = "drop"
      )

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

    cat("Output files in", output_dir, ":\n")
    cat("  - scenario_summaries.csv/.rds\n")
    cat("  - convergence_analysis.csv\n")
    cat("  - efficiency_comparisons.csv\n")
    cat("  - statistical_tests.rds\n\n")
  }

  # Return results invisibly
  invisible(list(
    scenario_summaries = scenario_summaries,
    convergence_analysis = convergence_analysis,
    efficiency_comparison = efficiency_comparison,
    statistical_tests = statistical_tests
  ))
}
