#' Fit log-logistic interval-censored AFT model via Metropolis-Hastings (JAGS)
#'
#' Runs JAGS on each simulated dataset (.rds file) found in \code{sim_dir},
#' writing standardized results to \code{results_dir}. Progress is reported via
#' \pkg{progressr} and execution is parallelized with \pkg{future}.
#'
#' @section Statistical Model:
#' Fits a log-logistic accelerated failure time (AFT) model with interval
#' censoring and optional survey weights using JAGS's Metropolis-Hastings
#' algorithm. The model uses the "zeros trick" with \code{dpois} to implement
#' the weighted likelihood. Model parameterization matches \code{\link{fit_logistic_hmc}}.
#'
#' @section Parallelization:
#' Two parallelization modes are supported:
#' \itemize{
#'   \item \strong{Mode 1 (HPC)}: Uses \pkg{future.batchtools} to submit each
#'         dataset as a separate PBS job. Within each job, JAGS runs chains
#'         sequentially but benefits from dedicated compute resources.
#'   \item \strong{Mode 2 (Local)}: Uses \pkg{future} with multicore/multisession
#'         plan to process datasets in parallel on a local machine.
#' }
#' The future plan is automatically restored to sequential on exit.
#'
#' @section Output Structure:
#' Creates standardized subdirectories under \code{results_dir}:
#' \itemize{
#'   \item \code{summaries/} - Posterior summaries with diagnostics
#'   \item \code{fits/} - Full coda mcmc.list objects
#'   \item \code{draws/} - Posterior draws as data frames
#'   \item \code{diagnostics/} - Aggregated diagnostics
#' }
#'
#' @param sim_dir Character string. Directory containing simulated .rds datasets.
#'   Each dataset should contain columns: L (left bound), R (right bound),
#'   X1 (binary covariate), and optionally weight.
#' @param results_dir Character string. Root directory where outputs will be
#'   written. Subdirectories are created automatically.
#' @param model_string Character string. JAGS model code. If NULL, uses the
#'   package-installed JAGS model file.
#' @param save Character vector. Controls what to save for each fit. Options:
#'   \code{"summary"}, \code{"fit"}, \code{"draws"}, \code{"diagnostics"}.
#'   Default saves all components.
#' @param draws_variables Character vector. Parameter names to monitor and export.
#'   Defaults to \code{c("alpha", "beta", "gamma")}.
#' @param draws_compress Character string. Compression method for saved draws RDS
#'   files. One of: \code{"none"}, \code{"gzip"}, \code{"bzip2"}, \code{"xz"}.
#'   Default is \code{"xz"} for maximum compression.
#' @param workers Numeric. Number of parallel workers (datasets processed
#'   simultaneously). If NULL, defaults to the number of input files.
#' @param n_chains Numeric. Number of MCMC chains. Default is 4.
#' @param n_adapt Numeric. Number of adaptation iterations. Default is 1000.
#' @param n_burnin Numeric. Number of burn-in iterations (after adaptation).
#'   Default is 1000.
#' @param n_iter Numeric. Number of sampling iterations (after burn-in).
#'   Default is 5000.
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
#'   \item Chains: 4 (configurable)
#'   \item Adaptation: 1000 iterations (configurable)
#'   \item Burn-in: 1000 iterations (configurable)
#'   \item Sampling: 5000 iterations (configurable)
#'   \item Total draws: 20,000 (4 chains × 5000)
#'   \item RNG: Wichmann-Hill (JAGS default)
#' }
#'
#' @section Dependencies:
#' Requires \pkg{rjags} and a working JAGS installation. Install JAGS from
#' \url{https://mcmc-jags.sourceforge.io/}.
#'
#' @examples
#' \dontrun{
#' # Basic usage (local execution)
#' fit_logistic_mh(
#'   sim_dir = "sim_data/n200",
#'   results_dir = "mcmc_outputs/mh_n200"
#' )
#'
#' # HPC execution with future.batchtools
#' library(future.batchtools)
#' plan(batchtools_pbs, template = "batchtools.pbs.tmpl")
#' fit_logistic_mh(
#'   sim_dir = "sim_data/n2000",
#'   results_dir = "mcmc_outputs/mh_n2000",
#'   workers = 100
#' )
#'
#' # Custom MCMC settings
#' fit_logistic_mh(
#'   sim_dir = "sim_data/n10000",
#'   results_dir = "mcmc_outputs/mh_n10000",
#'   n_chains = 3,
#'   n_iter = 10000,
#'   save = c("summary", "diagnostics")
#' )
#' }
#'
#' @seealso
#'   \code{\link{fit_logistic_hmc}} for HMC implementation,
#'   \code{\link{compute_derived_quantities_mh}} for derived quantities
#'
#' @export
fit_logistic_mh <- function(
    sim_dir = "sim_data",
    results_dir = "mcmc_outputs/mh",
    model_string = NULL,
    save = c("summary", "fit", "draws", "diagnostics"),
    draws_variables = NULL,
    draws_compress = "xz",
    workers = NULL,
    n_chains = 4,
    n_adapt = 1000,
    n_burnin = 1000,
    n_iter = 5000
) {

  # Check dependencies
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("Package 'rjags' is required but not installed.\n",
         "Install JAGS from: https://mcmc-jags.sourceforge.io/")
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required but not installed.")
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

  # Load JAGS model
  if (is.null(model_string)) {
    model_file <- system.file("models", "loglogistic_interval.jags",
                              package = "bayesianICSimulations")
    if (!nzchar(model_file)) {
      # Fallback to local file during development
      model_file <- "inst/models/loglogistic_interval.jags"
    }
    if (!file.exists(model_file)) {
      stop("JAGS model file not found: ", model_file)
    }
    model_string <- paste(readLines(model_file), collapse = "\n")
  }

  # Default monitored variables
  if (is.null(draws_variables)) {
    draws_variables <- c("alpha", "beta", "gamma")
  }

  # Determine number of workers
  if (is.null(workers)) workers <- length(sim_files)

  # Define fitting function for single dataset
  fit_one <- function(file_path) {
    dat <- readRDS(file_path)

    # Prepare JAGS data
    jags_data <- list(
      N = nrow(dat),
      L = pmax(dat$L, 1e-10),  # Floor at small positive value
      R = ifelse(is.infinite(dat$R), 1e12, dat$R),  # Replace Inf with sentinel
      X = as.numeric(dat$X1),
      w = if (!is.null(dat$weight)) as.numeric(dat$weight) else rep(1, nrow(dat)),
      zeros = rep(0, nrow(dat))  # For zeros trick
    )

    # Write model to temporary file
    temp_model <- tempfile(fileext = ".txt")
    writeLines(model_string, temp_model)

    # Fit model
    start_time <- Sys.time()

    # Initialize model
    jags_model <- rjags::jags.model(
      file = temp_model,
      data = jags_data,
      n.chains = n_chains,
      n.adapt = n_adapt,
      quiet = TRUE
    )

    # Burn-in
    update(jags_model, n.iter = n_burnin, progress.bar = "none")

    # Sample
    samples <- rjags::coda.samples(
      jags_model,
      variable.names = draws_variables,
      n.iter = n_iter,
      progress.bar = "none"
    )

    end_time   <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Clean up
    unlink(temp_model)

    # Calculate diagnostics
    summ <- summary(samples)
    ess  <- coda::effectiveSize(samples)
    rhat <- coda::gelman.diag(samples, multivariate = FALSE)$psrf

    # Create summary data frame matching Stan format
    param_names <- colnames(as.matrix(samples))
    summary_df <- data.frame(
      variable = param_names,
      mean     = summ$statistics[, "Mean"],
      median   = apply(as.matrix(samples), 2, median),
      sd       = summ$statistics[, "SD"],
      q2.5     = summ$quantiles[, "2.5%"],
      q97.5    = summ$quantiles[, "97.5%"],
      rhat     = if (is.matrix(rhat)) rhat[, "Point est."] else rhat,
      ess      = ess,
      stringsAsFactors = FALSE
    )

    # Aggregate diagnostics
    max_rhat <- max(summary_df$rhat, na.rm = TRUE)
    min_ess  <- min(summary_df$ess, na.rm = TRUE)

    # File naming
    stem    <- basename(sub("\\.rds$", "", file_path))
    written <- list(input = file_path)

    # Save summary
    if ("summary" %in% save) {
      summary_df$max_rhat   <- max_rhat
      summary_df$min_ess    <- min_ess
      summary_df$total_time <- total_time

      path <- file.path(dir_summary, paste0(stem, "_summary.rds"))
      saveRDS(summary_df, path, compress = "xz")
      written$summary <- path
    }

    # Save fit object
    if ("fit" %in% save) {
      path <- file.path(dir_fit, paste0(stem, "_fit.rds"))
      saveRDS(samples, path, compress = "xz")
      written$fit <- path
    }

    # Save draws
    if ("draws" %in% save) {
      draws_df <- as.data.frame(as.matrix(samples))
      draws_df$.chain     <- rep(1:n_chains, each = n_iter)
      draws_df$.iteration <- rep(1:n_iter, n_chains)
      draws_df$.draw      <- 1:nrow(draws_df)

      path <- file.path(dir_draws, paste0(stem, "_draws.rds"))
      saveRDS(draws_df, path, compress = draws_compress)
      written$draws <- path
    }

    # Save diagnostics
    if ("diagnostics" %in% save) {
      diag_summary <- data.frame(
        max_rhat         = max_rhat,
        min_ess          = min_ess,
        mean_ess_per_sec = mean(summary_df$ess) / total_time,
        total_time_sec   = total_time,
        n_chains         = n_chains,
        n_iter           = n_iter,
        n_adapt          = n_adapt,
        n_burnin         = n_burnin
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
        p(sprintf("Fitting MH: %s", basename(fp)))
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


#' Compute derived quantities from JAGS samples
#'
#' Calculates derived quantities from Metropolis-Hastings posterior samples,
#' matching the generated quantities block in the Stan model. Useful for
#' comparing HMC and MH results on derived parameters.
#'
#' @param samples An \code{mcmc.list} object from \code{rjags::coda.samples}
#'   containing posterior samples for \code{alpha}, \code{beta}, and \code{gamma}.
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{samples} - Data frame with derived quantity samples (one row per draw)
#'     \item \code{summary} - Matrix with summary statistics (mean, sd, quantiles)
#'   }
#'
#' @section Derived Quantities:
#' \itemize{
#'   \item \code{median_survival_X0} - Baseline median survival (X=0)
#'   \item \code{median_survival_X1} - Covariate median survival (X=1)
#'   \item \code{acceleration_factor} - AFT acceleration factor
#'   \item \code{hazard_ratio} - Approximate hazard ratio
#'   \item \code{log_median_ratio} - Log ratio of medians
#'   \item \code{median_ratio} - Ratio of medians
#' }
#'
#' @examples
#' \dontrun{
#' # After fitting with JAGS
#' samples <- rjags::coda.samples(jags_model, c("alpha", "beta", "gamma"), 5000)
#' derived <- compute_derived_quantities_mh(samples)
#'
#' # View summary
#' print(derived$summary)
#'
#' # Access posterior samples
#' head(derived$samples)
#' }
#'
#' @seealso \code{\link{fit_logistic_mh}}
#'
#' @export
compute_derived_quantities_mh <- function(samples) {
  # Extract parameter chains
  samples_matrix <- as.matrix(samples)
  alpha_chain <- samples_matrix[, "alpha"]
  beta_chain  <- samples_matrix[, "beta"]
  gamma_chain <- samples_matrix[, "gamma"]

  # Compute derived quantities (matching Stan generated quantities block)
  derived <- data.frame(
    median_survival_X0  = alpha_chain * exp(beta_chain * 0),  # Baseline median (X=0)
    median_survival_X1  = alpha_chain * exp(beta_chain * 1),  # Covariate median (X=1)
    acceleration_factor = exp(-beta_chain),                   # AFT acceleration factor
    hazard_ratio        = exp(-beta_chain),                   # Approximate HR
    log_median_ratio    = beta_chain,                         # log(median_X1 / median_X0)
    median_ratio        = exp(beta_chain)                     # median_X1 / median_X0
  )

  # Summary statistics
  derived_summary <- t(apply(derived, 2, function(x) {
    c(
      mean  = mean(x),
      sd    = sd(x),
      q2.5  = quantile(x, 0.025),
      q50   = quantile(x, 0.5),
      q97.5 = quantile(x, 0.975)
    )
  }))

  list(
    samples = derived,
    summary = derived_summary
  )
}
