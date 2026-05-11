#!/usr/bin/env Rscript
# Task B: Multivariable ZIMPHIA fit (sex + urban_rural + age_band)
#
# Run from package root with cmdstanr installed:
#   Rscript inst/scripts/04_zimphia_multivariable.R
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr)
  library(readr)
})

prepared_data_file <- "mcmc_outputs/zimphia/zimphia_prepared_data.rds"
if (!file.exists(prepared_data_file)) {
  stop(
    "Prepared data not found: ", prepared_data_file,
    "\nRun the primary ZIMPHIA analysis first (inst/scripts/03_zimphia_analysis.R).",
    call. = FALSE
  )
}
base <- readRDS(prepared_data_file)

indiv_file <- "ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)/zimphia2020adultind.csv"
if (!file.exists(indiv_file)) {
  stop("ZIMPHIA individual file not found: ", indiv_file, call. = FALSE)
}
indiv <- read_csv(
  indiv_file,
  col_select = c(personid, age, gender, urban),
  show_col_types = FALSE
)

prep <- prepare_zimphia_multivariable_data(
  base = base,
  indiv = indiv,
  covariates = c("sex", "urban_rural", "age_band")
)

cat(sprintf(
  "Design matrix: %d rows, %d columns\n",
  nrow(prep$X), ncol(prep$X)
))
print(head(prep$X))

res <- fit_zimphia_multivariable(
  prep,
  output_dir = "mcmc_outputs/zimphia_multivariable"
)

cat("\nMultivariable summary:\n")
print(res$summary)
cat(sprintf("\nRuntime: %.1f seconds\n", res$runtime_secs))
