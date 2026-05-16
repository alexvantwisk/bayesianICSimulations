#!/usr/bin/env Rscript
# Task E: Design-based variance via ZIMPHIA replicate weights
#
# Long-running (~2.4h on 8 cores). Run from package root with cmdstanr installed:
#   Rscript inst/scripts/07_zimphia_design_variance.R
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr)
})

prepared_data_file <- "mcmc_outputs/zimphia/zimphia_prepared_data.rds"
weights_csv <- "ZIMPHIA/ZIMPHIA 2020 Intermediary Weights (CSV)/zimphia2020indintermediarywts.csv"
primary_summary_csv <- "mcmc_outputs/zimphia/hmc/summaries/zimphia_hmc_summary.csv"

for (path in c(prepared_data_file, weights_csv, primary_summary_csv)) {
  if (!file.exists(path)) {
    stop("Required input not found: ", path, call. = FALSE)
  }
}

base <- readRDS(prepared_data_file)
weights <- load_replicate_weights(base = base, csv_path = weights_csv, n_reps = 100L)

t0 <- Sys.time()
combined <- fit_zimphia_design_replicates(
  analysis_data = base,
  weights_long = weights,
  output_dir = "mcmc_outputs/zimphia_design_replicates",
  stan_model_file = "inst/models/loglogistic_interval.stan",
  n_chains = 1L
)
cat(sprintf(
  "\nTotal wall time: %.2f hours\n",
  as.numeric(difftime(Sys.time(), t0, units = "hours"))
))

# Design-based CrI per parameter
design_ci <- combined |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    design_lo = quantile(median, 0.025, na.rm = TRUE),
    design_hi = quantile(median, 0.975, na.rm = TRUE),
    design_width = design_hi - design_lo,
    .groups = "drop"
  )

primary <- readr::read_csv(primary_summary_csv, show_col_types = FALSE) |>
  dplyr::transmute(
    variable,
    model_lo = q2.5,
    model_hi = q97.5,
    model_width = q97.5 - q2.5
  )

comparison <- dplyr::left_join(design_ci, primary, by = "variable") |>
  dplyr::mutate(inflation = design_width / model_width)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(comparison, "outputs/tables/tab_design_variance.csv")
print(comparison)

# Rubin-rules combination: design-aware total variance T = W̄ + (1 + 1/m) B
# (within-replicate variance W̄ + between-replicate variance B). This is the
# statistically defensible combination of Bayesian fits over survey-design
# replicate weights (Beaumont & Bocci 2008; Rubin 1987).
rep_summaries <- list.files(
  "mcmc_outputs/zimphia_design_replicates",
  pattern = "summary\\.rds$", recursive = TRUE, full.names = TRUE
)
all_summ <- dplyr::bind_rows(lapply(rep_summaries, readRDS))
rubin <- combine_design_replicates(all_summ)
readr::write_csv(rubin, "outputs/tables/tab_design_variance_rubin.csv")
message("Rubin combination written to outputs/tables/tab_design_variance_rubin.csv")
print(rubin)

# Side-by-side: model-based vs Rubin total interval for the headline parameter
beta_row <- dplyr::filter(rubin, variable == "beta")
beta_model <- dplyr::filter(primary, variable == "beta")
if (nrow(beta_row) == 1L && nrow(beta_model) == 1L) {
  rubin_width <- beta_row$ci_upper - beta_row$ci_lower
  ff <- rubin_width / beta_model$model_width
  message(sprintf(
    "\nbeta_sex headline (manuscript fill values):\n  model CrI:  [%.3f, %.3f]  width %.3f\n  Rubin CrI:  [%.3f, %.3f]  width %.3f\n  inflation:  %.3f\n",
    beta_model$model_lo, beta_model$model_hi, beta_model$model_width,
    beta_row$ci_lower, beta_row$ci_upper, rubin_width, ff
  ))
}
