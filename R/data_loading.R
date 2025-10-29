#' Parse metadata from simulation filename
#'
#' Extracts scenario ID, replicate, sample size, censoring level, and weight type
#' from standardized simulation filenames.
#'
#' @param filename Character string. Filename (with or without path) following
#'   the pattern: \code{sim_s###_r###_n####_c#.#_w[type]_*.rds}
#'
#' @return Named list with components:
#'   \itemize{
#'     \item \code{scenario_id} - Integer scenario identifier
#'     \item \code{replicate} - Integer replicate number
#'     \item \code{n_obs} - Integer sample size
#'     \item \code{censoring} - Numeric censoring proportion
#'     \item \code{weight_type} - Character weight type (none, low, high)
#'   }
#'   Returns \code{NULL} if filename doesn't match expected pattern.
#'
#' @examples
#' \dontrun{
#' parse_filename("sim_s001_r015_n0200_c0.1_whigh_summary.rds")
#' # $scenario_id: 1
#' # $replicate: 15
#' # $n_obs: 200
#' # $censoring: 0.1
#' # $weight_type: "high"
#' }
#'
#' @export
parse_filename <- function(filename) {
  # Remove path and extension
  basename_only <- tools::file_path_sans_ext(basename(filename))

  # Pattern captures weight type but stops before suffixes
  pattern <- "sim_s(\\d+)_r(\\d+)_n(\\d+)_c([0-9.]+)_w([a-z]+)"
  matches <- stringr::str_match(basename_only, pattern)

  if (is.na(matches[1])) {
    warning("Could not parse filename: ", filename)
    return(NULL)
  }

  list(
    scenario_id = as.integer(matches[2]),
    replicate   = as.integer(matches[3]),
    n_obs       = as.integer(matches[4]),
    censoring   = as.numeric(matches[5]),
    weight_type = matches[6]
  )
}


#' Load a single summary file with metadata
#'
#' Reads a posterior summary .rds file and attaches metadata parsed from the
#' filename. Useful for loading individual results or building custom pipelines.
#'
#' @param filepath Character string. Path to a summary .rds file.
#' @param method Character string. Analysis method, either \code{"hmc"} or \code{"mh"}.
#'
#' @return A tibble with posterior summaries (mean, median, sd, quantiles, diagnostics)
#'   and added metadata columns (method, scenario_id, replicate, n_obs, censoring,
#'   weight_type, filepath). Returns \code{NULL} if file cannot be loaded.
#'
#' @examples
#' \dontrun{
#' summ <- load_summary_file(
#'   "results/n200/hmc/summaries/sim_s001_r001_n0200_c0.1_whigh_summary.rds",
#'   method = "hmc"
#' )
#' }
#'
#' @export
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
        tibble::as_tibble() %>%
        dplyr::mutate(
          method      = method,
          scenario_id = metadata$scenario_id,
          replicate   = metadata$replicate,
          n_obs       = metadata$n_obs,
          censoring   = metadata$censoring,
          weight_type = metadata$weight_type,
          filepath    = filepath
        ) %>%
        # Remove lp__ for consistency (only in HMC)
        dplyr::filter(variable != "lp__")
    },
    error = function(e) {
      warning("Error loading ", filepath, ": ", e$message)
      return(NULL)
    }
  )
}


#' Load a single diagnostic file with metadata
#'
#' Reads a diagnostics .rds file and attaches metadata parsed from the filename.
#'
#' @param filepath Character string. Path to a diagnostics .rds file.
#' @param method Character string. Analysis method, either \code{"hmc"} or \code{"mh"}.
#'
#' @return A tibble with diagnostics (rhat, ess, timing, convergence metrics) and
#'   added metadata columns. Returns \code{NULL} if file cannot be loaded.
#'
#' @examples
#' \dontrun{
#' diag <- load_diagnostic_file(
#'   "results/n200/hmc/diagnostics/sim_s001_r001_n0200_c0.1_whigh_diag.rds",
#'   method = "hmc"
#' )
#' }
#'
#' @export
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
        tibble::as_tibble() %>%
        dplyr::mutate(
          method      = method,
          scenario_id = metadata$scenario_id,
          replicate   = metadata$replicate,
          n_obs       = metadata$n_obs,
          censoring   = metadata$censoring,
          weight_type = metadata$weight_type,
          filepath    = filepath
        )
    },
    error = function(e) {
      warning("Error loading ", filepath, ": ", e$message)
      return(NULL)
    }
  )
}


#' Load all summary files for a method and sample size
#'
#' Loads and combines all posterior summary files for a given MCMC method and
#' sample size combination. Includes progress reporting via \pkg{progressr}.
#'
#' @param method Character string. Analysis method, either \code{"hmc"} or \code{"mh"}.
#' @param n_obs Integer. Sample size (typically 200, 2000, or 10000).
#' @param results_dir Character string. Path to results directory. Defaults to
#'   \code{"results"} in current working directory.
#'
#' @return A tibble with all summaries for the specified method and sample size.
#'   Returns empty tibble if directory doesn't exist or no files are found.
#'
#' @examples
#' \dontrun{
#' # Load all HMC summaries for n=200
#' hmc_n200 <- load_summaries("hmc", 200)
#'
#' # With custom results directory
#' hmc_n2000 <- load_summaries("hmc", 2000, results_dir = "output/results")
#' }
#'
#' @export
load_summaries <- function(method, n_obs, results_dir = "mcmc_outputs") {
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

  # Get all summary files
  files <- list.files(
    summaries_dir,
    pattern = "_summary\\.rds$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No summary files found in: ", summaries_dir)
    return(tibble::tibble())
  }

  message(sprintf(
    "Loading %d %s summary files for n=%d...",
    length(files),
    toupper(method),
    n_obs
  ))

  # Load all files with progress bar
  progressr::with_progress({
    p <- progressr::progressor(steps = length(files))

    summaries <- purrr::map_dfr(files, function(f) {
      p()
      load_summary_file(f, method)
    })
  })

  summaries
}


#' Load all diagnostic files for a method and sample size
#'
#' Loads and combines all diagnostic files for a given MCMC method and sample
#' size combination. Includes progress reporting via \pkg{progressr}.
#'
#' @param method Character string. Analysis method, either \code{"hmc"} or \code{"mh"}.
#' @param n_obs Integer. Sample size (typically 200, 2000, or 10000).
#' @param results_dir Character string. Path to results directory. Defaults to
#'   \code{"results"} in current working directory.
#'
#' @return A tibble with all diagnostics for the specified method and sample size.
#'   Returns empty tibble if directory doesn't exist or no files are found.
#'
#' @examples
#' \dontrun{
#' # Load all HMC diagnostics for n=200
#' hmc_diag <- load_diagnostics("hmc", 200)
#'
#' # Check convergence
#' table(hmc_diag$max_rhat <= 1.01, hmc_diag$min_ess >= 400)
#' }
#'
#' @export
load_diagnostics <- function(method, n_obs, results_dir = "mcmc_outputs") {
  diag_dir <- file.path(results_dir, paste0("n", n_obs), method, "diagnostics")

  if (!dir.exists(diag_dir)) {
    warning("Directory not found: ", diag_dir)
    return(tibble::tibble())
  }

  # Get all diagnostic files
  files <- list.files(diag_dir, pattern = "_diag\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No diagnostic files found in: ", diag_dir)
    return(tibble::tibble())
  }

  message(sprintf(
    "Loading %d %s diagnostic files for n=%d...",
    length(files),
    toupper(method),
    n_obs
  ))

  # Load all files with progress bar
  progressr::with_progress({
    p <- progressr::progressor(steps = length(files))

    diagnostics <- purrr::map_dfr(files, function(f) {
      p()
      load_diagnostic_file(f, method)
    })
  })

  diagnostics
}


#' Combine all results into analysis-ready datasets
#'
#' High-level wrapper function that loads all summaries and diagnostics for
#' specified methods and sample sizes, calculates error metrics, and returns
#' combined datasets ready for analysis.
#'
#' @param methods Character vector. Methods to load (default: \code{c("hmc", "mh")}).
#' @param sample_sizes Integer vector. Sample sizes to load (default: \code{c(200, 2000, 10000)}).
#' @param results_dir Character string. Path to results directory.
#' @param true_values Named list with true parameter values. Default:
#'   \code{list(alpha = 5.0, beta = -0.5, gamma = 1.5)}.
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{summaries} - Combined posterior summaries with error metrics
#'     \item \code{diagnostics} - Combined diagnostics with convergence flags
#'     \item \code{scenario_metadata} - Scenario lookup table
#'   }
#'
#' @examples
#' \dontrun{
#' # Load all results
#' data <- combine_results()
#'
#' # Load only HMC for n=200
#' data_subset <- combine_results(
#'   methods = "hmc",
#'   sample_sizes = 200
#' )
#'
#' # Access components
#' summaries <- data$summaries
#' diagnostics <- data$diagnostics
#' metadata <- data$scenario_metadata
#' }
#'
#' @export
combine_results <- function(
    methods = c("hmc", "mh"),
    sample_sizes = c(200, 2000, 10000),
    results_dir = "mcmc_outputs",
    true_values = list(alpha = 5.0, beta = -0.5, gamma = 1.5)
) {

  message("\n", paste(rep("=", 70), collapse = ""))
  message("Loading Results")
  message(paste(rep("=", 70), collapse = ""), "\n")

  # Load all summaries
  all_summaries <- purrr::map_dfr(sample_sizes, function(n) {
    purrr::map_dfr(methods, function(m) {
      load_summaries(m, n, results_dir = results_dir)
    })
  })

  message(sprintf("\nTotal summary records loaded: %d", nrow(all_summaries)))

  # Load all diagnostics
  all_diagnostics <- purrr::map_dfr(sample_sizes, function(n) {
    purrr::map_dfr(methods, function(m) {
      load_diagnostics(m, n, results_dir = results_dir)
    })
  })

  message(sprintf("Total diagnostic records loaded: %d\n", nrow(all_diagnostics)))

  # Add true values and error metrics to summaries
  combined_summaries <- all_summaries %>%
    dplyr::mutate(
      true_value = dplyr::case_when(
        variable == "alpha" ~ true_values$alpha,
        variable == "beta"  ~ true_values$beta,
        variable == "gamma" ~ true_values$gamma,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::mutate(
      error          = mean - true_value,
      abs_error      = abs(error),
      squared_error  = error^2,
      contains_truth = (q2.5 <= true_value) & (true_value <= q97.5),
      ci_width       = q97.5 - q2.5
    )

  # Add convergence flags to diagnostics
  combined_diagnostics <- all_diagnostics %>%
    dplyr::mutate(
      converged             = (max_rhat <= 1.01) & (min_ess >= 400),
      rhat_ok               = max_rhat <= 1.01,
      ess_ok                = min_ess >= 400,
      ess_per_sec           = dplyr::if_else(is.na(mean_ess_per_sec), min_ess / total_time_sec, mean_ess_per_sec),
      has_divergences       = dplyr::if_else(method == "hmc", divergences > 0, NA),
      has_treedepth_issues  = dplyr::if_else(method == "hmc", max_treedepth_hit > 0, NA)
    )

  # Create scenario metadata
  scenario_metadata <- combined_summaries %>%
    dplyr::distinct(scenario_id, n_obs, censoring, weight_type) %>%
    dplyr::arrange(scenario_id) %>%
    dplyr::mutate(
      weight_type = stringr::str_replace(weight_type, "_summary$", ""),
      scenario_label = sprintf(
        "s%02d_n%d_c%.1f_%s",
        scenario_id,
        n_obs,
        censoring,
        weight_type
      )
    )

  message("Data processing complete!\n")

  list(
    summaries          = combined_summaries,
    diagnostics        = combined_diagnostics,
    scenario_metadata  = scenario_metadata
  )
}
