#!/usr/bin/env Rscript
# Task E: Design-based variance via ZIMPHIA replicate weights
#
# Long-running (~2.4h on 8 cores). Run from package root with cmdstanr installed:
#   Rscript inst/scripts/07_zimphia_design_variance.R
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr)
  library(ggplot2)
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
    design_lo = quantile(median, 0.025),
    design_hi = quantile(median, 0.975),
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
