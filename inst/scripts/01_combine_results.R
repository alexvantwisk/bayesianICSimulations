#!/usr/bin/env Rscript
# ==============================================================================
# 01_combine_results.R
#
# Purpose: Load and combine all HMC and MH simulation results into
#          analysis-ready datasets
#
# Inputs:  mcmc_outputs/n{200,2000,10000}/{hmc,mh}/{summaries,diagnostics}/*.rds
# Outputs: data/combined_summaries.rds
#          data/combined_diagnostics.rds
#          data/scenario_metadata.rds
#
# Author: Alexander van Twisk
# Date: 2025-10-24
# ==============================================================================

# Load package
library(bayesianICSimulations)

# Setup paths -----------------------------------------------------------------
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
results_dir <- file.path(base_dir, "mcmc_outputs")
data_dir <- file.path(base_dir, "data")

# Run data combination --------------------------------------------------------
results <- combine_results(
  results_dir = results_dir,
  data_dir = data_dir,
  verbose = TRUE
)
