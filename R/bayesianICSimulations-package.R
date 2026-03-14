#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".", ".chain", ".iteration", "HMC", "L", "MH", "R", "S", "S_mean",
  "S_med", "S_q50l", "S_q50u", "S_q95l", "S_q95u", "S_true", "X1",
  "abs_bias", "age", "as.formula", "as_latex", "as_tibble", "axis_max",
  "axis_min", "band_hi", "band_lo", "bias", "bias_converged", "bias_hmc",
  "bias_mcse", "bias_mcse_converged", "bias_mh", "bt_status", "btwt0",
  "censoring", "censoring_level", "ci_half", "ci_width", "coalesce",
  "coef", "cols_hide", "cols_label", "contains_truth", "converged",
  "cover", "cover_flag", "coverage", "coverage_bias", "coverage_converged",
  "coverage_hi", "coverage_lo", "coverage_mcse", "coverage_mcse_converged",
  "coverage_pct", "covered", "detail", "digits", "distinct", "divergences",
  "dt", "error", "ess", "ess_bulk", "ess_median", "ess_ok", "ess_per_sec",
  "ess_per_sec_hmc", "ess_per_sec_mh", "ess_per_sec_obs",
  "ess_per_sec_pred", "ess_per_sec_ratio", "ess_tail", "est", "estimate",
  "exclusion_rate", "facet", "faster", "first", "firstsxage", "fmt_number",
  "fname", "gender", "gender_label", "geom_mean", "group", "group_by",
  "group_max_y", "gt", "head", "hi", "hit", "hiv_status",
  "hivstatusfinal", "hivtfposy", "hmc", "hmc_hi", "hmc_lo",
  "hmc_median", "hmc_obs", "hmc_pred", "hmc_ratio", "hmc_text",
  "intercept", "label", "level", "lo", "lower", "map_dbl", "map_dfr",
  "max_rhat", "max_treedepth_hit", "mcse", "mean_HMC", "mean_MH",
  "mean_bias", "mean_ci_width", "mean_ci_width_converged", "mean_ess",
  "mean_ess_per_sec_hmc", "mean_ess_per_sec_mh", "mean_hmc", "mean_mh",
  "mean_rhat", "mean_rmse", "mean_runtime_hmc", "mean_runtime_mh",
  "mean_time", "median", "median_HMC", "median_MH", "median_ci_width",
  "median_ess", "median_ess_per_sec", "median_hmc", "median_mh",
  "median_runtime", "median_width", "method", "method_chr", "metric",
  "metric_value", "mh", "mh_hi", "mh_lo", "mh_median", "mh_obs",
  "mh_pred", "mh_ratio", "mh_text", "min_ess", "min_ess_tail", "n",
  "n10000", "n200", "n2000", "n_converged", "n_converged_obs",
  "n_divergences", "n_expected", "n_failed", "n_missing", "n_obs",
  "n_observed", "n_treedepth_issues", "observed_hmc_mh", "p25", "p75",
  "pair_id", "panel_id", "parameter", "pct_below", "pct_complete",
  "pct_converged", "pct_diff", "pct_remaining", "peak", "personid",
  "pivot_wider", "plot_weight", "predicted", "pull", "q2.5", "q97.5",
  "quantile", "r", "ratio", "ratio_10x", "ratio_5x", "ratio_label",
  "ratio_type", "ratio_value", "rbeta", "rbinom", "read.csv",
  "remaining", "removed", "rename", "rep_id", "rgamma", "rhat",
  "rhat_ok", "rmse", "rmse_converged", "rmse_mcse", "rmse_mcse_converged",
  "runif", "runtime", "runtime_diag", "runtime_mins", "runtime_obs",
  "runtime_s", "runtime_secs", "runtime_summary", "sample_size",
  "sample_size_label", "scenario", "scenario_id", "scenario_label",
  "scnx", "scnx_id", "sd", "sd_across_reps", "section", "seg_xmax",
  "seg_xmin", "sexever", "slope", "speedup_ratio", "squared_error",
  "starts_with", "step", "str_replace", "summarise", "tab_header",
  "tab_options", "tab_row_group", "tab_spanner", "target_censoring_prop",
  "test_result", "total_time", "total_time_sec", "transmute",
  "true_value", "truth", "ungroup", "update", "upper", "value", "var",
  "variable", "vars", "weight", "weight_metric_value", "weight_regime",
  "weight_type", "width", "width_med", "width_se", "wilcox.test", "x",
  "xmax", "xmin", "y"
))

#' bayesianICSimulations: Bayesian Interval-Censored Survival Data Simulation and Analysis
#'
#' Tools for comparing Bayesian MCMC methods (HMC via Stan, Metropolis-Hastings
#' via JAGS) for fitting log-logistic AFT models to interval-censored survival
#' data with survey weights. Designed for MSc research following the ADEMP
#' simulation framework.
#'
#' @section Main Functions:
#'
#' \strong{Model Fitting:}
#' \itemize{
#'   \item \code{\link{fit_logistic_hmc}} - Fit models using Hamiltonian Monte Carlo (Stan)
#'   \item \code{\link{fit_logistic_mh}} - Fit models using Metropolis-Hastings (JAGS)
#'   \item \code{\link{compute_derived_quantities_mh}} - Calculate derived quantities from MH samples
#' }
#'
#' \strong{Data Loading:}
#' \itemize{
#'   \item \code{\link{parse_filename}} - Parse metadata from simulation filenames
#'   \item \code{\link{load_summaries}} - Load all summary files for a method/sample size
#'   \item \code{\link{load_diagnostics}} - Load all diagnostic files
#'   \item \code{\link{combine_results}} - High-level wrapper to combine all results
#' }
#'
#' \strong{Statistical Analysis:}
#' \itemize{
#'   \item \code{\link{mcse_mean}} - Monte Carlo standard error for means
#'   \item \code{\link{mcse_prop}} - Monte Carlo standard error for proportions
#'   \item \code{\link{calc_bias_rmse}} - Calculate bias and RMSE with MCSEs
#'   \item \code{\link{cohen_d}} - Effect size calculation
#' }
#'
#' \strong{Visualization:}
#' \itemize{
#'   \item \code{\link{load_sims_from_dir}} - Load simulation replicates from directory
#'   \item \code{\link{summarise_weighted_curves}} - Fit parametric models and summarize
#'   \item \code{\link{plot_weighted_spaghetti}} - Create ghosted spaghetti plots
#'   \item \code{\link{facet_design_panels}} - Multi-panel faceted visualization
#'   \item \code{\link{compute_true_loglogistic}} - Calculate true survival curves
#'   \item \code{\link{compute_marginal_survival}} - Calculate marginal survival
#' }
#'
#' @section HPC Support:
#' The package includes HPC workflow scripts in \code{inst/hpc/}:
#' \itemize{
#'   \item \code{setup_hpc.sh} - One-time environment setup
#'   \item \code{submit_all.pbs} - Submit all 5400 datasets
#'   \item \code{submit_n{200,2000,10000}.pbs} - Sample-size specific submissions
#' }
#'
#' Analysis pipeline scripts are in \code{inst/scripts/}:
#' \itemize{
#'   \item \code{01_combine_results.R} - Aggregate results
#'   \item \code{02_analysis.R} - Statistical analysis
#'   \item \code{03_visualization.R} - Publication figures
#' }
#'
#' @section Dependencies:
#' \strong{Required:} tidyverse, progressr, future
#'
#' \strong{Optional but recommended:}
#' \itemize{
#'   \item \code{cmdstanr} - For HMC (install from https://mc-stan.org/r-packages/)
#'   \item \code{rjags} - For MH (requires JAGS: https://mcmc-jags.sourceforge.io/)
#'   \item \code{icenReg} - For survival visualization
#'   \item \code{future.batchtools} - For HPC parallelization
#' }
#'
#' @docType _PACKAGE
#' @name bayesianICSimulations-package
NULL
