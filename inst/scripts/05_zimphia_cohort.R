#!/usr/bin/env Rscript
# Task C: Birth-cohort stratified ZIMPHIA fits
#
# Run from package root with cmdstanr installed:
#   Rscript inst/scripts/05_zimphia_cohort.R
suppressPackageStartupMessages(devtools::load_all())

prepared_data_file <- "mcmc_outputs/zimphia/zimphia_prepared_data.rds"
if (!file.exists(prepared_data_file)) {
  stop(
    "Prepared data not found: ", prepared_data_file,
    "\nRun inst/scripts/03_zimphia_analysis.R first.",
    call. = FALSE
  )
}
analysis_data <- readRDS(prepared_data_file)

res <- fit_zimphia_cohort(
  analysis_data,
  output_dir = "mcmc_outputs/zimphia_cohort",
  stan_model_file = "inst/models/loglogistic_interval.stan"
)
print(res)
saveRDS(res, "mcmc_outputs/zimphia_cohort/combined_summary.rds")
