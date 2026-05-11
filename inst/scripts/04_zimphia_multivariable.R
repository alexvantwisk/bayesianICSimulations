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

base <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")

indiv <- read_csv(
  "ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)/zimphia2020adultind.csv",
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
