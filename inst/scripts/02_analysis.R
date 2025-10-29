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
# Outputs: outputs/analysis/scenario_summaries.csv
#          outputs/analysis/convergence_analysis.csv
#          outputs/analysis/efficiency_comparisons.csv
#          outputs/analysis/statistical_tests.rds
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
data_dir <- file.path(base_dir, "outputs", "combined_results")
results_dir <- file.path(base_dir, "outputs", "analysis")

# Run statistical analysis -----------------------------------------------------
analysis_results <- perform_statistical_analysis(
  data_dir = data_dir,
  output_dir = results_dir,
  verbose = TRUE
)
