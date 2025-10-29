#!/usr/bin/env Rscript
# ==============================================================================
# 01_combine_results.R
#
# Purpose: Load and combine all HMC and MH simulation results into
#          analysis-ready datasets
#
# Inputs:  results/n{200,2000,10000}/{hmc,mh}/{summaries,diagnostics}/*.rds
# Outputs: data/combined_summaries.rds
#          data/combined_diagnostics.rds
#          data/scenario_metadata.rds
#
# Author: Alexander van Twisk
# Date: 2025-10-24
# ==============================================================================

# Load required packages -------------------------------------------------------
library(tidyverse)
library(progressr)

# Setup -----------------------------------------------------------------------
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
results_dir <- file.path(base_dir, "results")
data_dir <- file.path(base_dir, "data")

# Create output directory if it doesn't exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "\n")
}

# Define true parameter values (from MSc Protocol)
TRUE_VALUES <- list(
  alpha = 5.0, # exp(log(5))
  beta = -0.5, # AFT coefficient
  gamma = 1.5 # Shape parameter
)

# Study design constants (fixed expected counts)
STUDY_DESIGN <- list(
  replicates_per_scenario = 200,
  scenarios_per_n = 9, # 3 censoring × 3 weight types
  datasets_per_n = 1800, # 9 scenarios × 200 replicates
  total_datasets = 5400, # 27 scenarios × 200 replicates
  scenarios_total = 27,
  sample_sizes = c(200, 2000, 10000),
  censoring_levels = c(0.1, 0.3, 0.5),
  weight_types = c("none", "low", "high")
)

# Helper Functions ------------------------------------------------------------

#' Parse metadata from filename
#'
#' @param filename Character string like "sim_s001_r002_n0200_c0.1_whigh_summary.rds"
#' @return Named list with scenario, replicate, n_obs, censoring, weight_type
parse_filename <- function(filename) {
  # Remove path and extension
  basename <- tools::file_path_sans_ext(basename(filename))

  # Extract components using regex
  # Pattern captures weight type but stops before _summary, _fit, _draws, _diag suffixes
  pattern <- "sim_s(\\d+)_r(\\d+)_n(\\d+)_c([0-9.]+)_w([a-z]+)"
  matches <- str_match(basename, pattern)

  if (is.na(matches[1])) {
    warning("Could not parse filename: ", filename)
    return(NULL)
  }

  list(
    scenario_id = as.integer(matches[2]),
    replicate = as.integer(matches[3]),
    n_obs = as.integer(matches[4]),
    censoring = as.numeric(matches[5]),
    weight_type = matches[6]
  )
}

#' Load a single summary file with metadata
#'
#' @param filepath Path to summary .rds file
#' @param method Character, either "hmc" or "mh"
#' @return Tibble with parameter estimates and metadata
load_summary_file <- function(filepath, method) {
  # Parse metadata from filename
  metadata <- parse_filename(filepath)

  if (is.null(metadata)) {
    return(NULL)
  }

  # Load the summary data
  tryCatch(
    {
      summary_data <- readRDS(filepath)

      # Add metadata columns
      summary_data %>%
        as_tibble() %>%
        mutate(
          method = method,
          scenario_id = metadata$scenario_id,
          replicate = metadata$replicate,
          n_obs = metadata$n_obs,
          censoring = metadata$censoring,
          weight_type = metadata$weight_type,
          filepath = filepath
        ) %>%
        # Remove lp__ for consistency (only in HMC)
        filter(variable != "lp__")
    },
    error = function(e) {
      warning("Error loading ", filepath, ": ", e$message)
      return(NULL)
    }
  )
}

#' Load a single diagnostic file with metadata
#'
#' @param filepath Path to diagnostic .rds file
#' @param method Character, either "hmc" or "mh"
#' @return Tibble with diagnostics and metadata
load_diagnostic_file <- function(filepath, method) {
  # Parse metadata from filename
  metadata <- parse_filename(filepath)

  if (is.null(metadata)) {
    return(NULL)
  }

  # Load the diagnostic data
  tryCatch(
    {
      diag_data <- readRDS(filepath)

      # Convert to tibble and add metadata
      diag_data %>%
        as_tibble() %>%
        mutate(
          method = method,
          scenario_id = metadata$scenario_id,
          replicate = metadata$replicate,
          n_obs = metadata$n_obs,
          censoring = metadata$censoring,
          weight_type = metadata$weight_type,
          filepath = filepath
        )
    },
    error = function(e) {
      warning("Error loading ", filepath, ": ", e$message)
      return(NULL)
    }
  )
}

#' Load all summary files for a given method and sample size
#'
#' @param method Character, either "hmc" or "mh"
#' @param n_obs Integer, sample size (200, 2000, or 10000)
#' @return Tibble with all summaries
load_summaries <- function(method, n_obs) {
  summaries_dir <- file.path(
    results_dir,
    paste0("n", n_obs),
    method,
    "summaries"
  )

  if (!dir.exists(summaries_dir)) {
    warning("Directory not found: ", summaries_dir)
    return(tibble())
  }

  # Get all summary files
  files <- list.files(
    summaries_dir,
    pattern = "_summary\\.rds$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No summary files found in: ", summaries_dir)
    return(tibble())
  }

  cat(sprintf(
    "Loading %d %s summary files for n=%d...\n",
    length(files),
    toupper(method),
    n_obs
  ))

  # Load all files with progress bar
  with_progress({
    p <- progressor(steps = length(files))

    summaries <- map_dfr(files, function(f) {
      p()
      load_summary_file(f, method)
    })
  })

  summaries
}

#' Load all diagnostic files for a given method and sample size
#'
#' @param method Character, either "hmc" or "mh"
#' @param n_obs Integer, sample size (200, 2000, or 10000)
#' @return Tibble with all diagnostics
load_diagnostics <- function(method, n_obs) {
  diag_dir <- file.path(results_dir, paste0("n", n_obs), method, "diagnostics")

  if (!dir.exists(diag_dir)) {
    warning("Directory not found: ", diag_dir)
    return(tibble())
  }

  # Get all diagnostic files
  files <- list.files(diag_dir, pattern = "_diag\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No diagnostic files found in: ", diag_dir)
    return(tibble())
  }

  cat(sprintf(
    "Loading %d %s diagnostic files for n=%d...\n",
    length(files),
    toupper(method),
    n_obs
  ))

  # Load all files with progress bar
  with_progress({
    p <- progressor(steps = length(files))

    diagnostics <- map_dfr(files, function(f) {
      p()
      load_diagnostic_file(f, method)
    })
  })

  diagnostics
}

# Main Data Loading -----------------------------------------------------------

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Loading HMC and MH Simulation Results\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

# Sample sizes to process
sample_sizes <- c(200, 2000, 10000)
methods <- c("hmc", "mh")

# Load all summaries
all_summaries <- map_dfr(sample_sizes, function(n) {
  map_dfr(methods, function(m) {
    load_summaries(m, n)
  })
})

cat(sprintf("\nTotal summary records loaded: %d\n", nrow(all_summaries)))

# Load all diagnostics
all_diagnostics <- map_dfr(sample_sizes, function(n) {
  map_dfr(methods, function(m) {
    load_diagnostics(m, n)
  })
})

cat(sprintf("Total diagnostic records loaded: %d\n\n", nrow(all_diagnostics)))

# Data Processing -------------------------------------------------------------

cat("Processing combined data...\n")

# Add true values to summaries
combined_summaries <- all_summaries %>%
  mutate(
    true_value = case_when(
      variable == "alpha" ~ TRUE_VALUES$alpha,
      variable == "beta" ~ TRUE_VALUES$beta,
      variable == "gamma" ~ TRUE_VALUES$gamma,
      TRUE ~ NA_real_
    )
  ) %>%
  # Calculate error metrics
  mutate(
    error = mean - true_value,
    abs_error = abs(error),
    squared_error = error^2,
    contains_truth = (q2.5 <= true_value) & (true_value <= q97.5),
    ci_width = q97.5 - q2.5
  )

# Add convergence flags to diagnostics
combined_diagnostics <- all_diagnostics %>%
  mutate(
    converged = (max_rhat <= 1.01) & (min_ess >= 400),
    rhat_ok = max_rhat <= 1.01,
    ess_ok = min_ess >= 400,
    ess_per_sec = mean_ess_per_sec # Already calculated in diagnostics
  ) %>%
  # Add divergence info for HMC (NA for MH)
  mutate(
    has_divergences = if_else(method == "hmc", divergences > 0, NA),
    has_treedepth_issues = if_else(method == "hmc", max_treedepth_hit > 0, NA)
  )

# Create scenario metadata lookup table
scenario_metadata <- combined_summaries %>%
  distinct(scenario_id, n_obs, censoring, weight_type) %>%
  arrange(scenario_id) %>%
  mutate(
    # Fix weight_type parsing issue (remove "_summary" suffix if present)
    weight_type = str_replace(weight_type, "_summary$", ""),
    # Add expected counts from study design
    expected_replicates = STUDY_DESIGN$replicates_per_scenario,
    expected_datasets_per_n = STUDY_DESIGN$datasets_per_n,
    scenario_label = sprintf(
      "s%02d_n%d_c%.1f_%s",
      scenario_id,
      n_obs,
      censoring,
      weight_type
    )
  )

# Summary Statistics ----------------------------------------------------------

cat("\n")
cat("Data Summary\n")
cat(paste(rep("-", 78), collapse = ""), "\n")

# Counts by method and sample size
count_summary <- combined_diagnostics %>%
  count(method, n_obs, name = "n_fits") %>%
  pivot_wider(names_from = method, values_from = n_fits, values_fill = 0)

print(count_summary)

# Convergence rates
cat("\nConvergence Rates:\n")
convergence_summary <- combined_diagnostics %>%
  group_by(method, n_obs) %>%
  summarise(
    expected = STUDY_DESIGN$datasets_per_n, # Use constant
    observed = n(),
    converged = sum(converged),
    pct_converged = 100 * (converged / expected), # % of expected
    .groups = "drop"
  )

print(convergence_summary)

# Missing data summary
cat("\nExpected vs Actual Fits (per sample size):\n")

missing_summary <- combined_diagnostics %>%
  count(method, n_obs) %>%
  mutate(
    expected = STUDY_DESIGN$datasets_per_n, # Use constant
    missing = expected - n,
    pct_complete = 100 * (n / expected)
  )

print(missing_summary)

# Save Processed Data ---------------------------------------------------------

cat("\nSaving processed datasets...\n")

saveRDS(combined_summaries, file.path(data_dir, "combined_summaries.rds"))
cat(sprintf(
  "  ✓ Saved combined_summaries.rds (%d rows)\n",
  nrow(combined_summaries)
))

saveRDS(combined_diagnostics, file.path(data_dir, "combined_diagnostics.rds"))
cat(sprintf(
  "  ✓ Saved combined_diagnostics.rds (%d rows)\n",
  nrow(combined_diagnostics)
))

saveRDS(scenario_metadata, file.path(data_dir, "scenario_metadata.rds"))
cat(sprintf(
  "  ✓ Saved scenario_metadata.rds (%d scenarios)\n",
  nrow(scenario_metadata)
))

# Also save as CSV for easy inspection
write_csv(scenario_metadata, file.path(data_dir, "scenario_metadata.csv"))

# Save study design constants
saveRDS(STUDY_DESIGN, file.path(data_dir, "study_design.rds"))
cat(sprintf("  ✓ Saved study_design.rds (reference constants)\n"))

cat("\n")
cat(paste(rep("=", 78), collapse = ""), "\n")
cat("Data combination complete!\n")
cat(paste(rep("=", 78), collapse = ""), "\n\n")

cat("Output files:\n")
cat("  - data/combined_summaries.rds\n")
cat("  - data/combined_diagnostics.rds\n")
cat("  - data/scenario_metadata.rds\n")
cat("  - data/scenario_metadata.csv\n")
cat("  - data/study_design.rds\n\n")
