#' Fit log-logistic interval-censored AFT model via Hamiltonian Monte Carlo
#'
#' Runs CmdStanR on each simulated dataset (.rds file) found in \code{sim_dir},
#' writing standardized results to \code{results_dir}. Progress is reported via
#' \pkg{progressr} and execution is parallelized with \pkg{future}.
#'
#' @section Statistical Model:
#' Fits a log-logistic accelerated failure time (AFT) model with interval
#' censoring and optional survey weights. The model uses the parameterization:
#' \deqn{log(\lambda_i) = log(\alpha) + \beta \cdot X_i}
#' where \code{alpha} is baseline scale, \code{beta} is the AFT coefficient,
#' and \code{gamma} is the shape parameter.
#'
#' @section Parallelization:
#' Two parallelization modes are supported:
#' \itemize{
#'   \item \strong{Mode 1 (HPC)}: Uses \pkg{future.batchtools} to submit each
#'         dataset as a separate PBS job. Within each job, MCMC chains run in
#'         parallel using all allocated cores.
#'   \item \strong{Mode 2 (Local)}: Uses \pkg{future} with multicore/multisession
#'         plan to process datasets in parallel on a local machine.
#' }
#' The future plan is automatically restored to sequential on exit.
#'
#' @section Output Structure:
#' Creates standardized subdirectories under \code{results_dir}:
#' \itemize{
#'   \item \code{summaries/} - Posterior summaries with diagnostics
#'   \item \code{fits/} - Full CmdStanR fit objects
#'   \item \code{draws/} - Posterior draws as data frames
#'   \item \code{diagnostics/} - Aggregated diagnostics
#' }
#'
#' @param sim_dir Character string. Directory containing simulated .rds datasets.
#'   Each dataset should contain columns: L (left bound), R (right bound),
#'   X1 (binary covariate), and optionally weight.
#' @param results_dir Character string. Root directory where outputs will be
#'   written. Subdirectories are created automatically.
#' @param model_file Character string. Path to the Stan model file. Defaults to
#'   the package-installed model. Can be overridden with a custom path.
#' @param save Character vector. Controls what to save for each fit. Options:
#'   \code{"summary"}, \code{"fit"}, \code{"draws"}, \code{"diagnostics"}.
#'   Default saves all components.
#' @param draws_variables Optional character vector. Parameter names to export
#'   when \code{save} includes \code{"draws"}. If NULL, all monitored parameters
#'   are exported.
#' @param draws_compress Character string. Compression method for saved draws RDS
#'   files. One of: \code{"none"}, \code{"gzip"}, \code{"bzip2"}, \code{"xz"}.
#'   Default is \code{"xz"} for maximum compression.
#' @param workers Numeric. Number of parallel workers (datasets processed
#'   simultaneously). If NULL, defaults to the number of input files.
#'
#' @return Invisibly returns a tibble with one row per input file, containing:
#'   \itemize{
#'     \item \code{input} - Path to input .rds file
#'     \item \code{summary} - Path to summary output (if saved)
#'     \item \code{fit} - Path to fit object (if saved)
#'     \item \code{draws} - Path to draws (if saved)
#'     \item \code{diagnostics} - Path to diagnostics (if saved)
#'     \item \code{max_rhat} - Maximum R-hat across parameters
#'     \item \code{min_ess} - Minimum effective sample size
#'   }
#'
#' @section MCMC Settings:
#' \itemize{
#'   \item Chains: 4
#'   \item Warmup: 1000 iterations
#'   \item Sampling: 5000 iterations
#'   \item Total draws: 20,000 (4 chains × 5000)
#'   \item Seed: 2025 (fixed for reproducibility)
#' }
#'
#' @section Dependencies:
#' Requires \pkg{cmdstanr} (not on CRAN). Install via:
#' \code{install.packages("cmdstanr", repos = "https://mc-stan.org/r-packages/")}
#'
#' @examples
#' \dontrun{
#' # Basic usage (local execution)
#' fit_logistic_hmc(
#'   sim_dir = "sim_data/n200",
#'   results_dir = "mcmc_outputs/hmc_n200"
#' )
#'
#' # HPC execution with future.batchtools
#' library(future.batchtools)
#' plan(batchtools_pbs, template = "batchtools.pbs.tmpl")
#' fit_logistic_hmc(
#'   sim_dir = "sim_data/n2000",
#'   results_dir = "mcmc_outputs/hmc_n2000",
#'   workers = 100
#' )
#'
#' # Save only summaries and diagnostics (skip heavy outputs)
#' fit_logistic_hmc(
#'   sim_dir = "sim_data/n10000",
#'   results_dir = "mcmc_outputs/hmc_n10000",
#'   save = c("summary", "diagnostics")
#' )
#' }
#'
#' @seealso \code{\link{fit_logistic_mh}} for Metropolis-Hastings implementation
#'
#' @export
fit_logistic_hmc <- function(
    sim_dir = "sim_data",
    results_dir = "mcmc_outputs/hmc",
    model_file = NULL,
    save = c("summary", "fit", "draws", "diagnostics"),
    draws_variables = NULL,
    draws_compress = "xz",
    workers = NULL
) {

  # Check dependencies
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required but not installed.\n",
         "Install from: https://mc-stan.org/r-packages/")
  }
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("Package 'posterior' is required but not installed.")
  }

  save <- match.arg(save, c("summary", "fit", "draws", "diagnostics"), several.ok = TRUE)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

  # Output directories
  dir_summary     <- file.path(results_dir, "summaries")
  dir_fit         <- file.path(results_dir, "fits")
  dir_draws       <- file.path(results_dir, "draws")
  dir_diagnostics <- file.path(results_dir, "diagnostics")
  for (d in c(dir_summary, dir_fit, dir_draws, dir_diagnostics)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # Input files
  sim_files <- list.files(path = sim_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(sim_files) == 0) {
    warning("No simulation files found in ", sim_dir)
    return(invisible(NULL))
  }

  # Model file path
  if (is.null(model_file)) {
    model_file <- system.file("models", "loglogistic_interval.stan",
                              package = "bayesianICSimulations")
    if (!nzchar(model_file)) {
      # Fallback to local file during development
      model_file <- "inst/models/loglogistic_interval.stan"
    }
  }
  if (!file.exists(model_file)) {
    stop("Stan model file not found: ", model_file)
  }

  # Compile Stan model
  mod_aft <- cmdstanr::cmdstan_model(model_file)

  # Determine number of workers
  if (is.null(workers)) workers <- length(sim_files)

  # Define fitting function for single dataset
  fit_one <- function(file_path) {
    dat <- readRDS(file_path)

    # Prepare Stan data
    stan_data <- list(
      N = nrow(dat),
      L = pmax(dat$L, 1e-10),  # Floor at small positive value
      R = dat$R,
      X = as.numeric(dat$X1),
      w = if (!is.null(dat$weight)) as.numeric(dat$weight) else rep(1, nrow(dat))
    )

    # Fit model
    start_time <- Sys.time()
    fit <- mod_aft$sample(
      data            = stan_data,
      chains          = 4,
      parallel_chains = 4,      # Use all 4 cores for parallel chains
      iter_warmup     = 1000,
      iter_sampling   = 5000,
      seed            = 2025,   # Fixed seed for reproducibility
      refresh         = 0
    )
    end_time   <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Summarize posterior
    dr <- fit$draws(inc_warmup = FALSE)
    summ <- posterior::summarise_draws(
      dr,
      "mean",
      "median",
      "sd",
      ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
      "rhat",
      "ess_bulk",
      "ess_tail"
    )

    # Standardize column names
    if ("ess_bulk" %in% names(summ)) summ$ess <- summ$ess_bulk

    # Aggregate diagnostics
    max_rhat <- suppressWarnings(max(summ$rhat, na.rm = TRUE))
    min_ess  <- suppressWarnings(min(summ$ess,  na.rm = TRUE))

    # File naming
    stem    <- basename(sub("\\.rds$", "", file_path))
    written <- list(input = file_path)

    # Save summary
    if ("summary" %in% save) {
      summ$max_rhat   <- max_rhat
      summ$min_ess    <- min_ess
      summ$total_time <- total_time

      path <- file.path(dir_summary, paste0(stem, "_summary.rds"))
      saveRDS(summ[, c("variable", "mean", "median", "sd", "q2.5", "q97.5",
                       "rhat", "ess", "max_rhat", "min_ess", "total_time")],
              path, compress = "xz")
      written$summary <- path
    }

    # Save fit object
    if ("fit" %in% save) {
      path <- file.path(dir_fit, paste0(stem, "_fit.rds"))
      suppressMessages(fit$save_object(path))
      written$fit <- path
    }

    # Save draws
    if ("draws" %in% save) {
      dr    <- fit$draws(variables = draws_variables, inc_warmup = FALSE)
      dr_df <- posterior::as_draws_df(dr)  # Includes .chain, .iteration, .draw
      path  <- file.path(dir_draws, paste0(stem, "_draws.rds"))
      saveRDS(dr_df, path, compress = draws_compress)
      written$draws <- path
    }

    # Save diagnostics
    if ("diagnostics" %in% save) {
      diag_arr <- fit$sampler_diagnostics()
      divs <- tryCatch(sum(diag_arr[, , "divergent__"]), error = function(e) NA_integer_)
      mtd  <- tryCatch(fit$metadata()$max_treedepth, error = function(e) NA_integer_)
      tdh  <- tryCatch(sum(diag_arr[, , "treedepth__"] >= mtd), error = function(e) NA_integer_)
      mean_ess_per_sec <- suppressWarnings(mean(summ$ess, na.rm = TRUE) / total_time)

      diag_summary <- data.frame(
        max_rhat          = max_rhat,
        min_ess           = min_ess,
        min_ess_tail      = if ("ess_tail" %in% names(summ)) min(summ$ess_tail, na.rm = TRUE) else NA_real_,
        mean_ess_per_sec  = mean_ess_per_sec,
        divergences       = divs,
        max_treedepth_hit = tdh,
        total_time_sec    = total_time,
        n_chains          = fit$metadata()$chains %||% 4,
        iter_warmup       = fit$metadata()$iter_warmup %||% 1000,
        iter_sampling     = fit$metadata()$iter_sampling %||% 5000
      )

      path <- file.path(dir_diagnostics, paste0(stem, "_diag.rds"))
      saveRDS(diag_summary, path)
      written$diagnostics <- path
    }

    list(
      input       = file_path,
      outputs     = written,
      timing      = list(total = total_time, sampling = total_time),
      diagnostics = list(max_rhat = max_rhat, min_ess = min_ess)
    )
  }

  # Execute with progress reporting
  res <- NULL
  progressr::with_progress({
    p <- progressr::progressor(along = sim_files)
    res <- future.apply::future_lapply(
      X = sim_files,
      FUN = function(fp) {
        p(sprintf("Fitting HMC: %s", basename(fp)))
        fit_one(fp)
      },
      future.seed = TRUE
    )
  })

  # Return results summary
  tibble::tibble(
    input       = vapply(res, `[[`, character(1), "input"),
    summary     = vapply(res, function(x) x$outputs$summary     %||% NA_character_, character(1)),
    fit         = vapply(res, function(x) x$outputs$fit         %||% NA_character_, character(1)),
    draws       = vapply(res, function(x) x$outputs$draws       %||% NA_character_, character(1)),
    diagnostics = vapply(res, function(x) x$outputs$diagnostics %||% NA_character_, character(1)),
    max_rhat    = vapply(res, function(x) x$diagnostics$max_rhat %||% NA_real_, numeric(1)),
    min_ess     = vapply(res, function(x) x$diagnostics$min_ess  %||% NA_real_, numeric(1))
  ) %>% invisible()
}
