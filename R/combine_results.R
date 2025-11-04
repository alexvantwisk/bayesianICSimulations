#' Combine HMC and MH Simulation Results
#'
#' Load and combine all HMC and MH simulation results from MCMC outputs into
#' analysis-ready datasets with error metrics and convergence diagnostics.
#'
#' @param results_dir Character string. Path to directory containing
#'   n{200,2000,10000}/{hmc,mh}/{summaries,diagnostics}/*.rds files.
#'   Default: "mcmc_outputs"
#' @param data_dir Character string. Path to directory where combined datasets
#'   will be saved. Default: "outputs/combined_results"
#' @param verbose Logical. Print progress messages? Default: TRUE
#'
#' @return Invisibly returns a list with four elements:
#'   \item{combined_summaries}{Tibble with all parameter estimates, error metrics,
#'     and coverage statistics}
#'   \item{combined_diagnostics}{Tibble with convergence diagnostics and timing info}
#'   \item{scenario_metadata}{Tibble with scenario lookup table}
#'   \item{study_design}{List with study design constants}
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Loads all summary and diagnostic .rds files from MCMC outputs
#'   \item Parses metadata from filenames (scenario, replicate, n, censoring, weights)
#'   \item Calculates error metrics (bias, RMSE, coverage, CI width)
#'   \item Adds convergence flags (Rhat ≤ 1.01 & ESS ≥ 400)
#'   \item Saves four files to data_dir (default: outputs/combined_results/):
#'     - combined_summaries.rds/csv
#'     - combined_diagnostics.rds/csv
#'     - scenario_metadata.rds/csv
#'     - study_design.rds
#' }
#'
#' True parameter values (from MSc Protocol):
#' \itemize{
#'   \item alpha = 5.0
#'   \item beta = -0.5
#'   \item gamma = 1.5
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- combine_results()
#'
#' # Custom directories
#' results <- combine_results(
#'   results_dir = "my_mcmc_outputs",
#'   data_dir = "my_outputs/combined"
#' )
#'
#' # Access combined data
#' summaries <- results$combined_summaries
#' diagnostics <- results$combined_diagnostics
#' }
#'
#' @export
combine_results <- function(
  results_dir = "mcmc_outputs",
  data_dir = file.path("outputs", "combined_results"),
  verbose = TRUE
) {
  # Load required packages
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("Package 'tidyverse' is required but not installed.")
  }

  library(tidyverse)
  log_msg <- function(...) {
    if (isTRUE(verbose)) {
      message(paste0(...))
    }
  }

  # Define constants
  TRUE_VALUES <- list(
    alpha = 5.0,
    beta = -0.5,
    gamma = 1.5
  )

  STUDY_DESIGN <- list(
    replicates_per_scenario = 200,
    scenarios_per_n = 9,
    datasets_per_n = 1800,
    total_datasets = 5400,
    scenarios_total = 27,
    sample_sizes = c(200, 2000, 10000),
    censoring_levels = c(0.1, 0.3, 0.5),
    weight_types = c("none", "low", "high")
  )

  if (!dir.exists(results_dir)) {
    stop("Results directory not found: ", results_dir)
  }
  results_dir <- normalizePath(results_dir, mustWork = TRUE)

  is_absolute_path <- function(path) {
    if (.Platform$OS.type == "windows") {
      grepl("^[A-Za-z]:[/\\\\]", path) || grepl("^\\\\\\\\", path)
    } else {
      substr(path, 1, 1) == "/"
    }
  }

  if (!is_absolute_path(data_dir)) {
    data_dir <- file.path(dirname(results_dir), data_dir)
  }

  data_dir <- sub("[/\\\\]+$", "", data_dir)
  if (!identical(basename(data_dir), "combined_results")) {
    data_dir <- file.path(data_dir, "combined_results")
  }

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  data_dir <- normalizePath(data_dir, mustWork = TRUE)
  log_msg("Combining simulation outputs from: ", results_dir)
  log_msg("Saving combined results to: ", data_dir)

  # Load data
  sample_sizes <- c(200, 2000, 10000)
  methods <- c("hmc", "mh")

  all_summaries <- map_dfr(sample_sizes, function(n) {
    map_dfr(methods, function(m) {
      load_summaries(m, n, results_dir, verbose)
    })
  })

  log_msg("Loaded ", nrow(all_summaries), " summary rows.")

  all_diagnostics <- map_dfr(sample_sizes, function(n) {
    map_dfr(methods, function(m) {
      load_diagnostics(m, n, results_dir, verbose)
    })
  })

  log_msg("Loaded ", nrow(all_diagnostics), " diagnostic rows.")
  log_msg("Processing combined data.")

  # Process data
  combined_summaries <- all_summaries %>%
    mutate(
      true_value = case_when(
        variable == "alpha" ~ TRUE_VALUES$alpha,
        variable == "beta" ~ TRUE_VALUES$beta,
        variable == "gamma" ~ TRUE_VALUES$gamma,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      error = mean - true_value,
      abs_error = abs(error),
      squared_error = error^2,
      contains_truth = (q2.5 <= true_value) & (true_value <= q97.5),
      ci_width = q97.5 - q2.5
    )

  combined_diagnostics <- all_diagnostics %>%
    mutate(
      converged = (max_rhat <= 1.01) & (min_ess >= 400),
      rhat_ok = max_rhat <= 1.01,
      ess_ok = min_ess >= 400,
      ess_per_sec = mean_ess_per_sec
    ) %>%
    mutate(
      has_divergences = if_else(method == "hmc", divergences > 0, NA),
      has_treedepth_issues = if_else(method == "hmc", max_treedepth_hit > 0, NA)
    )

  scenario_metadata <- combined_summaries %>%
    distinct(scenario_id, n_obs, censoring, weight_type) %>%
    arrange(scenario_id) %>%
    mutate(
      weight_type = str_replace(weight_type, "_summary$", ""),
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

  # Save processed data

  summaries_rds <- file.path(data_dir, "combined_summaries.rds")
  summaries_csv <- file.path(data_dir, "combined_summaries.csv")
  saveRDS(combined_summaries, summaries_rds)
  write_csv(combined_summaries, summaries_csv)
  log_msg("Saved combined_summaries.rds (", nrow(combined_summaries), " rows).")
  log_msg("Saved combined_summaries.csv (", nrow(combined_summaries), " rows).")

  diagnostics_rds <- file.path(data_dir, "combined_diagnostics.rds")
  diagnostics_csv <- file.path(data_dir, "combined_diagnostics.csv")
  saveRDS(combined_diagnostics, diagnostics_rds)
  write_csv(combined_diagnostics, diagnostics_csv)
  log_msg(
    "Saved combined_diagnostics.rds (",
    nrow(combined_diagnostics),
    " rows)."
  )
  log_msg(
    "Saved combined_diagnostics.csv (",
    nrow(combined_diagnostics),
    " rows)."
  )

  scenario_rds <- file.path(data_dir, "scenario_metadata.rds")
  scenario_csv <- file.path(data_dir, "scenario_metadata.csv")
  saveRDS(scenario_metadata, scenario_rds)
  write_csv(scenario_metadata, scenario_csv)
  log_msg(
    "Saved scenario_metadata.rds (",
    nrow(scenario_metadata),
    " scenarios)."
  )
  log_msg("Saved scenario_metadata.csv.")

  saveRDS(STUDY_DESIGN, file.path(data_dir, "study_design.rds"))
  log_msg("Saved study_design.rds.")

  # Return results invisibly
  invisible(list(
    combined_summaries = combined_summaries,
    combined_diagnostics = combined_diagnostics,
    scenario_metadata = scenario_metadata,
    study_design = STUDY_DESIGN
  ))
}

# Helper functions -------------------------------------------------------------

#' Parse metadata from simulation filename
#'
#' @param filename Character string like "sim_s001_r002_n0200_c0.1_whigh_summary.rds"
#' @return Named list with scenario, replicate, n_obs, censoring, weight_type
#' @keywords internal
parse_filename <- function(filename) {
  basename <- tools::file_path_sans_ext(basename(filename))
  pattern <- "sim_s(\\d+)_r(\\d+)_n(\\d+)_c([0-9.]+)_w([a-z]+)"
  matches <- stringr::str_match(basename, pattern)

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
#' @keywords internal
load_summary_file <- function(filepath, method) {
  metadata <- parse_filename(filepath)

  if (is.null(metadata)) {
    return(NULL)
  }

  tryCatch(
    {
      summary_data <- readRDS(filepath)

      summary_data %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          method = method,
          n_obs = metadata$n_obs,
          censoring = metadata$censoring,
          weight_type = metadata$weight_type,
          scenario_id = metadata$scenario_id,
          replicate = metadata$replicate,
          filepath = filepath
        ) %>%
        dplyr::filter(variable != "lp__", variable != "log_alpha")
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
#' @keywords internal
load_diagnostic_file <- function(filepath, method) {
  metadata <- parse_filename(filepath)

  if (is.null(metadata)) {
    return(NULL)
  }

  tryCatch(
    {
      diag_data <- readRDS(filepath)

      diag_data %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          method = method,
          n_obs = metadata$n_obs,
          censoring = metadata$censoring,
          weight_type = metadata$weight_type,
          scenario_id = metadata$scenario_id,
          replicate = metadata$replicate,
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
#' @param results_dir Path to results directory
#' @param verbose Print progress messages?
#' @return Tibble with all summaries
#' @keywords internal
load_summaries <- function(method, n_obs, results_dir, verbose = TRUE) {
  summaries_dir <- file.path(
    results_dir,
    paste0("n", n_obs),
    method,
    "summaries"
  )

  if (!dir.exists(summaries_dir)) {
    warning("Directory not found: ", summaries_dir)
    return(tibble::tibble())
  }

  files <- list.files(
    summaries_dir,
    pattern = "_summary\\.rds$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No summary files found in: ", summaries_dir)
    return(tibble::tibble())
  }

  if (verbose) {
    message(sprintf(
      "Loading %d %s summary files for n=%d...",
      length(files),
      toupper(method),
      n_obs
    ))
  }

  purrr::map_dfr(files, function(f) {
    load_summary_file(f, method)
  })
}

#' Load all diagnostic files for a given method and sample size
#'
#' @param method Character, either "hmc" or "mh"
#' @param n_obs Integer, sample size (200, 2000, or 10000)
#' @param results_dir Path to results directory
#' @param verbose Print progress messages?
#' @return Tibble with all diagnostics
#' @keywords internal
load_diagnostics <- function(method, n_obs, results_dir, verbose = TRUE) {
  diag_dir <- file.path(results_dir, paste0("n", n_obs), method, "diagnostics")

  if (!dir.exists(diag_dir)) {
    warning("Directory not found: ", diag_dir)
    return(tibble::tibble())
  }

  files <- list.files(diag_dir, pattern = "_diag\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No diagnostic files found in: ", diag_dir)
    return(tibble::tibble())
  }

  if (verbose) {
    message(sprintf(
      "Loading %d %s diagnostic files for n=%d...",
      length(files),
      toupper(method),
      n_obs
    ))
  }

  purrr::map_dfr(files, function(f) {
    load_diagnostic_file(f, method)
  })
}
