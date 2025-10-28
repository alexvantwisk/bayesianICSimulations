#' Fit a log-logistic interval-censoring AFT model on pilot data 
#' via Metropolis-Hastings (JAGS)
#'
#' Runs JAGS on each simulated dataset (.rds) found in sim_dir,
#' writing results to results_dir. Progress is reported via \pkg{progressr}
#' and execution is parallelized with \pkg{future}.
#'
#' @section Parallelization:
#' Uses future.batchtools to submit each dataset as a separate PBS job on the
#' HPC cluster. Within each job, JAGS runs chains sequentially but benefits
#' from dedicated compute resources. The future plan is restored to sequential on exit.
#'
#' @importFrom rjags jags.model coda.samples
#' @importFrom coda gelman.diag effectiveSize
#' @importFrom future plan sequential tweak
#' @importFrom future.batchtools batchtools_pbs
#' @importFrom progressr with_progress progressor
#' @importFrom future.apply future_lapply
#'
#' @param sim_dir Directory containing simulated .rds datasets.
#' @param results_dir Root directory where outputs will be written. Subfolders
#'   are created as needed.
#' @param model_string JAGS model string. If NULL, uses the default AFT model.
#' @param save Character vector controlling what to persist for each fit.
#'   Any of c("summary","fit","draws","diagnostics"). Default saves both
#'   the compact summary and the full fit.
#' @param draws_variables Optional character vector of parameter names to export
#'   when save includes "draws". If NULL, monitors c("alpha","beta","gamma").
#' @param draws_compress Compression for saved draws RDS.
#' @param workers Number of parallel PBS jobs to submit (one per dataset).
#'   Default is NULL, which submits all datasets as separate jobs.
#' @param n_chains Number of MCMC chains (default: 4).
#' @param n_adapt Number of adaptation iterations (default: 1000).
#' @param n_burnin Number of burn-in iterations (default: 1000).
#' @param n_iter Number of sampling iterations (default: 5000).
#'
#' @return Invisibly returns a tibble indexing input files and written outputs.
#' @export
fit_logistic_mh <- function(
    sim_dir = "data/pilot_data",
    results_dir = "results/mh_summary",
    model_string = NULL,
    save = c("summary","fit","draws","diagnostics"),
    draws_variables = NULL,
    draws_compress = "xz",
    workers = NULL,
    n_chains = 4,
    n_adapt = 1000,
    n_burnin = 1000,
    n_iter = 5000) {
  
  # Load required libraries
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("Package 'rjags' is required but not installed.")
  }
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' is required but not installed.")
  }
  
  save <- match.arg(save, c("summary","fit","draws","diagnostics"), several.ok = TRUE)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  # Create output directories
  dir_summary     <- file.path(results_dir, "summaries")
  dir_fit         <- file.path(results_dir, "fits")
  dir_draws       <- file.path(results_dir, "draws")
  dir_diagnostics <- file.path(results_dir, "diagnostics")
  for (d in c(dir_summary, dir_fit, dir_draws, dir_diagnostics)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }
  
  # Find simulation files
  sim_files <- list.files(path = sim_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(sim_files) == 0) {
    warning("No simulation files found in ", sim_dir)
    return(invisible(NULL))
  }
  
  # Default JAGS model (AFT parameterization matching Stan)
  if (is.null(model_string)) {
    model_string <- paste(readLines("inst/models/loglogistic_interval.jags"), collapse = "\n")
  }
  
  # Default monitored variables
  if (is.null(draws_variables)) {
    draws_variables <- c("alpha", "beta", "gamma")
  }

  # HPC parallelism via PBS
  # Each dataset gets its own PBS job; if workers is NULL, submit all
  if (is.null(workers)) workers <- length(sim_files)

  # Configure PBS resources for batchtools
  pbs_resources <- list(
    ncpus = 4,              # 4 cores (JAGS runs chains sequentially, but reserves resources)
    memory = 8192,          # 8 GB RAM (in MB)
    walltime = "12:00:00",  # 12 hours
    queue = "day"           # Adjust to your HPC queue
  )

  # Set up future.batchtools with PBS
  future::plan(
    future.batchtools::batchtools_pbs,
    template = "batchtools.pbs.tmpl",
    resources = pbs_resources,
    workers = workers
  )
  on.exit(future::plan(future::sequential), add = TRUE)
  
  # Function to fit one dataset
  fit_one <- function(file_path) {
    dat <- readRDS(file_path)
    
    # Prepare JAGS data
    jags_data <- list(
      N = nrow(dat),
      L = pmax(dat$L, 1e-10),  # Avoid exact zeros for numerical stability
      R = ifelse(is.infinite(dat$R), 1e12, dat$R),  # Replace Inf with large number for JAGS
      X = as.numeric(dat$X1),
      w = if (!is.null(dat$weight)) as.numeric(dat$weight) else rep(1, nrow(dat)),
      zeros = rep(0, nrow(dat))  # For zeros trick
    )
    
    # Write model to temporary file
    model_file <- tempfile(fileext = ".txt")
    writeLines(model_string, model_file)
    
    # Fit model
    start_time <- Sys.time()
    
    # Initialize model
    jags_model <- rjags::jags.model(
      file = model_file,
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
    
    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Clean up
    unlink(model_file)
    
    # Calculate diagnostics
    summ <- summary(samples)
    ess <- coda::effectiveSize(samples)
    rhat <- coda::gelman.diag(samples, multivariate = FALSE)$psrf
    
    # Create summary data frame matching Stan format
    param_names <- colnames(as.matrix(samples))
    summary_df <- data.frame(
      variable = param_names,
      mean = summ$statistics[,"Mean"],
      median = apply(as.matrix(samples), 2, median),
      sd = summ$statistics[,"SD"],
      q2.5 = summ$quantiles[,"2.5%"],
      q97.5 = summ$quantiles[,"97.5%"], 
      rhat = if(is.matrix(rhat)) rhat[,"Point est."] else rhat,
      ess = ess,
      stringsAsFactors = FALSE
    )
    
    # Add overall diagnostics
    max_rhat <- max(summary_df$rhat, na.rm = TRUE)
    min_ess <- min(summary_df$ess, na.rm = TRUE)
    
    stem <- basename(sub("\\.rds$", "", file_path))
    written <- list(input = file_path)
    
    # Save outputs based on 'save' parameter
    if ("summary" %in% save) {
      # Add diagnostics to summary
      summary_df$max_rhat <- max_rhat
      summary_df$min_ess <- min_ess
      summary_df$total_time <- total_time
      
      path <- file.path(dir_summary, paste0(stem, "_summary.rds"))
      saveRDS(summary_df, path, compress = "xz")
      written$summary <- path
    }
    
    if ("fit" %in% save) {
      # Save the entire MCMC object
      path <- file.path(dir_fit, paste0(stem, "_fit.rds"))
      saveRDS(samples, path, compress = "xz")
      written$fit <- path
    }
    
    if ("draws" %in% save) {
      # Convert to data frame format similar to posterior::as_draws_df
      draws_df <- as.data.frame(as.matrix(samples))
      draws_df$.chain <- rep(1:n_chains, each = n_iter * n_chains / n_chains)
      draws_df$.iteration <- rep(1:n_iter, n_chains)
      draws_df$.draw <- 1:nrow(draws_df)
      
      path <- file.path(dir_draws, paste0(stem, "_draws.rds"))
      saveRDS(draws_df, path, compress = draws_compress)
      written$draws <- path
    }
    
    if ("diagnostics" %in% save) {
      # Save comprehensive diagnostics
      diag_summary <- data.frame(
        max_rhat = max_rhat,
        min_ess = min_ess,
        mean_ess_per_sec = mean(summary_df$ess) / total_time,
        total_time_sec = total_time,
        n_chains = n_chains,
        n_iter = n_iter,
        n_adapt = n_adapt,
        n_burnin = n_burnin
      )
      
      path <- file.path(dir_diagnostics, paste0(stem, "_diag.rds"))
      saveRDS(diag_summary, path)
      written$diagnostics <- path
    }
    
    list(
      input = file_path,
      outputs = written,
      timing = list(
        total = total_time,
        sampling = total_time  # Simplified timing for JAGS
      ),
      diagnostics = list(
        max_rhat = max_rhat,
        min_ess = min_ess
      )
    )
  }
  
  # Run fits with progress bar
  res <- NULL
  progressr::with_progress({
    p <- progressr::progressor(along = sim_files)
    res <- future.apply::future_lapply(
      X = sim_files,
      FUN = function(fp) {
        p(sprintf("Running JAGS: %s", basename(fp)))
        fit_one(fp)
      },
      future.seed = TRUE
    )
  })
  
  # Clean up parallelization
  future::plan(future::sequential)
  
  # Return summary tibble
  tibble::tibble(
    input = vapply(res, `[[`, character(1), "input"),
    summary = vapply(res, function(x) x$outputs$summary %||% NA_character_, character(1)),
    fit = vapply(res, function(x) x$outputs$fit %||% NA_character_, character(1)),
    draws = vapply(res, function(x) x$outputs$draws %||% NA_character_, character(1)),
    diagnostics = vapply(res, function(x) x$outputs$diagnostics %||% NA_character_, character(1)),
    max_rhat = vapply(res, function(x) x$diagnostics$max_rhat %||% NA_real_, numeric(1)),
    min_ess = vapply(res, function(x) x$diagnostics$min_ess %||% NA_real_, numeric(1))
  ) %>% invisible()
}

# ---- Helper function for derived quantities --------------------------------
#' Compute derived quantities from JAGS samples (matching Stan generated quantities)
#' 
#' @param samples MCMC samples from rjags::coda.samples
#' @return List with samples and summary of derived quantities
compute_derived_quantities_mh <- function(samples) {
  # Extract parameter chains
  samples_matrix <- as.matrix(samples)
  alpha_chain <- samples_matrix[, "alpha"]
  beta_chain <- samples_matrix[, "beta"]
  gamma_chain <- samples_matrix[, "gamma"]
  
  # Compute derived quantities (matching Stan generated quantities)
  derived <- data.frame(
    median_survival_X0 = alpha_chain * exp(beta_chain * 0),  # baseline median (X=0)
    median_survival_X1 = alpha_chain * exp(beta_chain * 1),  # covariate median (X=1)
    acceleration_factor = exp(-beta_chain),                  # AFT acceleration factor
    hazard_ratio = exp(-beta_chain),                         # Approximate HR
    log_median_ratio = beta_chain,                           # log(median_X1 / median_X0)
    median_ratio = exp(beta_chain)                           # median_X1 / median_X0
  )
  
  # Summary statistics
  derived_summary <- apply(derived, 2, function(x) {
    c(mean = mean(x), 
      sd = sd(x),
      q2.5 = quantile(x, 0.025),
      q50 = quantile(x, 0.5),
      q97.5 = quantile(x, 0.975))
  })
  
  return(list(
    samples = derived,
    summary = derived_summary
  ))
}
