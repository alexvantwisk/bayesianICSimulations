#!/usr/bin/env Rscript
# ==============================================================================
# 03_visualization.R
#
# Purpose: Generate all publication-ready figures and tables
#
# Inputs:  data/combined_summaries.csv
#          data/combined_diagnostics.csv
#
# Outputs: outputs/figures/*.png (13 figures)
#
# Author: Alexander van Twisk
# Date: 2025-10-24
# ==============================================================================

# Load package
library(bayesianICSimulations)

# Setup paths ------------------------------------------------------------------
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
output_dir <- file.path(base_dir, "outputs", "figures")

# Generate and save all figures ------------------------------------------------
plots <- save_all_figures(
  data_dir = data_dir,
  output_dir = output_dir,
  width = 8,
  height = 6,
  dpi = 320,
  formats = "png"
)

cat("\nVisualization complete!\n")
