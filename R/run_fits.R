#!/usr/bin/env Rscript
#
# run_fits.R
#
# Processes a single simulated dataset with both HMC and MH models.
# Designed to be called from PBS array jobs with dataset index as argument.
#
# Usage: Rscript run_fits.R <dataset_index> <sample_size>
#   dataset_index: 1-1200 (PBS_ARRAYID)
#   sample_size: 200, 2000, or 10000
#
# Reproducibility:
#   HMC seed: 2025 (fixed for all datasets)
#   MH seed:  2025 + dataset_index (unique per dataset: 2026-3225)

suppressPackageStartupMessages({
  library(cmdstanr)
  library(rjags)
  library(coda)
  library(posterior)
  library(tibble)
  library(parallel)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Ensure external libraries do not oversubscribe CPU resources on shared nodes
Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  BLIS_NUM_THREADS = "1"
)

# Detect the number of cores allocated to this job (PBS, SLURM, or fallback)
available_cores <- local({
  np <- suppressWarnings(as.integer(Sys.getenv("PBS_NP")))
  if (is.na(np) || np <= 0) {
    np <- suppressWarnings(as.integer(Sys.getenv("NCPUS")))
  }
  if (is.na(np) || np <= 0) {
    np <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")))
  }
  if (is.na(np) || np <= 0) {
    np <- suppressWarnings(parallel::detectCores(logical = FALSE))
  }
  if (is.na(np) || np <= 0) {
    np <- 1L
  }
  np
})
available_cores <- max(1L, available_cores)
options(mc.cores = available_cores)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2 || length(args) > 3) {
  stop("Usage: Rscript run_fits.R <dataset_index> <sample_size> [data_dir]")
}

dataset_idx <- as.integer(args[1])
sample_size <- as.integer(args[2])
data_dir <- if (length(args) == 3) args[3] else "sim_data"

if (!sample_size %in% c(200, 2000, 10000)) {
  stop("sample_size must be 200, 2000, or 10000")
}

# Setup paths
base_dir <- getwd()
sim_dir <- file.path(base_dir, data_dir, paste0("n", sample_size))
results_base <- file.path(base_dir, "results", paste0("n", sample_size))
stan_model_file <- file.path(base_dir, "loglogistic_interval.stan")
jags_model_file <- file.path(base_dir, "loglogistic_interval.jags")

# Get list of datasets and select the one for this task
# Cache the directory listing to reduce repeated metadata lookups across jobs
index_cache <- file.path(sim_dir, ".file_index.rds")
sim_files <- NULL
if (file.exists(index_cache)) {
  sim_files <- tryCatch(readRDS(index_cache), error = function(e) NULL)
}
if (is.null(sim_files)) {
  sim_files <- sort(list.files(sim_dir, pattern = "\\.rds$", full.names = TRUE))
  if (!file.exists(index_cache) && length(sim_files) > 0) {
    tmp_cache <- paste0(index_cache, ".", Sys.getpid(), ".tmp")
    success <- FALSE
    tryCatch(
      {
        saveRDS(sim_files, tmp_cache)
        success <- file.rename(tmp_cache, index_cache)
      },
      error = function(e) {
        if (file.exists(tmp_cache)) unlink(tmp_cache)
      },
      finally = {
        if (!success && file.exists(tmp_cache)) unlink(tmp_cache)
      }
    )
  }
}
if (length(sim_files) == 0) {
  stop("No .rds files found in ", sim_dir)
}
if (dataset_idx < 1 || dataset_idx > length(sim_files)) {
  stop(
    "dataset_idx ",
    dataset_idx,
    " out of range [1, ",
    length(sim_files),
    "]"
  )
}

input_file <- sim_files[dataset_idx]
file_stem <- sub("\\.rds$", "", basename(input_file))

cat("========================================\n")
cat("Processing dataset:", file_stem, "\n")
cat("Index:", dataset_idx, "of", length(sim_files), "\n")
cat("Sample size:", sample_size, "\n")
cat("Data directory:", data_dir, "\n")
cat("========================================\n\n")

# Skip work if both HMC and MH outputs already exist --------------------------
hmc_summary_path <- file.path(
  results_base,
  "hmc",
  "summaries",
  paste0(file_stem, "_summary.rds")
)
hmc_diag_path <- file.path(
  results_base,
  "hmc",
  "diagnostics",
  paste0(file_stem, "_diag.rds")
)
mh_summary_path <- file.path(
  results_base,
  "mh",
  "summaries",
  paste0(file_stem, "_summary.rds")
)
mh_diag_path <- file.path(
  results_base,
  "mh",
  "diagnostics",
  paste0(file_stem, "_diag.rds")
)

existing_outputs <- c(
  hmc_summary_path,
  hmc_diag_path,
  mh_summary_path,
  mh_diag_path
)

if (all(file.exists(existing_outputs))) {
  cat("Outputs already exist for ", file_stem, ". Skipping fit.\n", sep = "")
  quit(status = 0)
}

# Load data
dat <- readRDS(input_file)
cat("Loaded data:", nrow(dat), "observations\n\n")

# Prepare Stan data
stan_data <- list(
  N = nrow(dat),
  L = pmax(dat$L, 1e-10),
  R = dat$R,
  X = as.numeric(dat$X1),
  w = if (!is.null(dat$weight)) as.numeric(dat$weight) else rep(1, nrow(dat))
)

# Prepare JAGS data
jags_data <- list(
  N = nrow(dat),
  L = pmax(dat$L, 1e-10),
  R = ifelse(is.infinite(dat$R), 1e12, dat$R),
  X = as.numeric(dat$X1),
  w = if (!is.null(dat$weight)) as.numeric(dat$weight) else rep(1, nrow(dat)),
  zeros = rep(0, nrow(dat))
)

# ============================================================================
# HMC (Stan) Fit
# ============================================================================
cat("Starting HMC fit (Stan)...\n")
cat("Using", available_cores, "CPU core(s) for this job\n")
hmc_start <- Sys.time()

# Load pre-compiled Stan model (if available) to avoid compilation overhead
compiled_dir <- file.path(base_dir, "compiled_models")
model_info_file <- file.path(compiled_dir, "model_info.rds")

if (file.exists(model_info_file)) {
  # Use pre-compiled model
  model_info <- readRDS(model_info_file)
  cat("Using pre-compiled model:", model_info$exe_file, "\n")
  mod_stan <- cmdstan_model(exe_file = model_info$exe_file)
} else {
  # Fall back to compiling (backwards compatibility)
  cat("Pre-compiled model not found, compiling now...\n")
  cat("(Run compile_stan.R first to avoid this overhead)\n")
  mod_stan <- cmdstan_model(stan_model_file)
}

# Run HMC
stan_parallel_chains <- min(4L, available_cores)

fit_hmc <- mod_stan$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = stan_parallel_chains,
  iter_warmup = 1000,
  iter_sampling = 5000,
  seed = 2025,
  refresh = 0,
  show_messages = FALSE
)

hmc_end <- Sys.time()
hmc_time <- as.numeric(difftime(hmc_end, hmc_start, units = "secs"))

# Extract HMC summaries
draws_hmc <- fit_hmc$draws(inc_warmup = FALSE)
summ_hmc <- posterior::summarise_draws(
  draws_hmc,
  "mean",
  "median",
  "sd",
  ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
  "rhat",
  "ess_bulk",
  "ess_tail"
)

if ("ess_bulk" %in% names(summ_hmc)) {
  summ_hmc$ess <- summ_hmc$ess_bulk
}

# HMC diagnostics
max_rhat_hmc <- max(summ_hmc$rhat, na.rm = TRUE)
min_ess_hmc <- min(summ_hmc$ess, na.rm = TRUE)

# Divergences and treedepth
diag_arr_hmc <- fit_hmc$sampler_diagnostics()
divs_hmc <- sum(diag_arr_hmc[,, "divergent__"])
mtd <- fit_hmc$metadata()$max_treedepth %||% 10
tdh_hmc <- sum(diag_arr_hmc[,, "treedepth__"] >= mtd)

cat(sprintf("HMC completed in %.1f seconds\n", hmc_time))
cat(sprintf("  Max Rhat: %.4f, Min ESS: %.0f\n", max_rhat_hmc, min_ess_hmc))
cat(sprintf("  Divergences: %d, Max treedepth hits: %d\n\n", divs_hmc, tdh_hmc))

# Save HMC summary
summ_hmc$max_rhat <- max_rhat_hmc
summ_hmc$min_ess <- min_ess_hmc
summ_hmc$total_time <- hmc_time

dir_hmc_summary <- file.path(results_base, "hmc", "summaries")
dir.create(dir_hmc_summary, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  summ_hmc[, c(
    "variable",
    "mean",
    "median",
    "sd",
    "q2.5",
    "q97.5",
    "rhat",
    "ess",
    "max_rhat",
    "min_ess",
    "total_time"
  )],
  file.path(dir_hmc_summary, paste0(file_stem, "_summary.rds")),
  compress = "xz"
)

# Save HMC diagnostics
dir_hmc_diag <- file.path(results_base, "hmc", "diagnostics")
dir.create(dir_hmc_diag, recursive = TRUE, showWarnings = FALSE)
diag_hmc <- data.frame(
  max_rhat = max_rhat_hmc,
  min_ess = min_ess_hmc,
  min_ess_tail = min(summ_hmc$ess_tail, na.rm = TRUE),
  mean_ess_per_sec = mean(summ_hmc$ess, na.rm = TRUE) / hmc_time,
  divergences = divs_hmc,
  max_treedepth_hit = tdh_hmc,
  total_time_sec = hmc_time,
  n_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)
saveRDS(diag_hmc, file.path(dir_hmc_diag, paste0(file_stem, "_diag.rds")))

# Clean up HMC fit object
rm(fit_hmc, draws_hmc)
gc()

# ============================================================================
# MH (JAGS) Fit - PARALLELIZED
# ============================================================================
cat("Starting MH fit (JAGS) - running 4 chains in parallel...\n")
mh_start <- Sys.time()

# Load JAGS model
jags_model_string <- paste(readLines(jags_model_file), collapse = "\n")

# Set reproducible seed for JAGS (dataset-specific for uniqueness)
jags_seed <- 2025 + dataset_idx

# Function to run a single JAGS chain
run_jags_chain <- function(chain_id) {
  # Each chain gets its own model object and seed
  chain_model <- jags.model(
    textConnection(jags_model_string),
    data = jags_data,
    n.chains = 1,
    n.adapt = 1000,
    inits = list(
      .RNG.name = "base::Wichmann-Hill",
      .RNG.seed = jags_seed + chain_id
    ),
    quiet = TRUE
  )

  # Burn-in
  update(chain_model, n.iter = 1000, progress.bar = "none")

  # Sample
  chain_samples <- coda.samples(
    chain_model,
    variable.names = c("alpha", "beta", "gamma"),
    n.iter = 5000,
    progress.bar = "none"
  )

  return(chain_samples[[1]]) # Extract the single chain from mcmc.list
}

# Run 4 chains, parallelised across the cores allocated to this job
jags_chains <- 4L
jags_cores <- min(available_cores, jags_chains)

chain_ids <- seq_len(jags_chains)
chain_list <- if (jags_cores > 1L) {
  mclapply(
    chain_ids,
    run_jags_chain,
    mc.cores = jags_cores,
    mc.preschedule = FALSE
  )
} else {
  lapply(chain_ids, run_jags_chain)
}

# Combine into mcmc.list object
samples_mh <- as.mcmc.list(chain_list)

mh_end <- Sys.time()
mh_time <- as.numeric(difftime(mh_end, mh_start, units = "secs"))

# Extract MH summaries
summ_obj <- summary(samples_mh)
ess_mh <- effectiveSize(samples_mh)
rhat_mh <- gelman.diag(samples_mh, multivariate = FALSE)$psrf

param_names <- colnames(as.matrix(samples_mh))
summ_mh <- data.frame(
  variable = param_names,
  mean = summ_obj$statistics[, "Mean"],
  median = apply(as.matrix(samples_mh), 2, median),
  sd = summ_obj$statistics[, "SD"],
  q2.5 = summ_obj$quantiles[, "2.5%"],
  q97.5 = summ_obj$quantiles[, "97.5%"],
  rhat = if (is.matrix(rhat_mh)) rhat_mh[, "Point est."] else rhat_mh,
  ess = ess_mh,
  stringsAsFactors = FALSE
)

# MH diagnostics
max_rhat_mh <- max(summ_mh$rhat, na.rm = TRUE)
min_ess_mh <- min(summ_mh$ess, na.rm = TRUE)

cat(sprintf("MH completed in %.1f seconds\n", mh_time))
cat(sprintf("  Max Rhat: %.4f, Min ESS: %.0f\n\n", max_rhat_mh, min_ess_mh))

# Save MH summary
summ_mh$max_rhat <- max_rhat_mh
summ_mh$min_ess <- min_ess_mh
summ_mh$total_time <- mh_time

dir_mh_summary <- file.path(results_base, "mh", "summaries")
dir.create(dir_mh_summary, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  summ_mh,
  file.path(dir_mh_summary, paste0(file_stem, "_summary.rds")),
  compress = "xz"
)

# Save MH diagnostics
dir_mh_diag <- file.path(results_base, "mh", "diagnostics")
dir.create(dir_mh_diag, recursive = TRUE, showWarnings = FALSE)
diag_mh <- data.frame(
  max_rhat = max_rhat_mh,
  min_ess = min_ess_mh,
  mean_ess_per_sec = mean(summ_mh$ess, na.rm = TRUE) / mh_time,
  total_time_sec = mh_time,
  n_chains = 4,
  n_iter = 5000,
  n_adapt = 1000,
  n_burnin = 1000
)
saveRDS(diag_mh, file.path(dir_mh_diag, paste0(file_stem, "_diag.rds")))

# ============================================================================
# Summary
# ============================================================================
cat("========================================\n")
cat("COMPLETED:", file_stem, "\n")
cat(sprintf(
  "Total time: %.1f seconds (HMC: %.1f, MH: %.1f)\n",
  hmc_time + mh_time,
  hmc_time,
  mh_time
))
cat("========================================\n")

# Return success
quit(status = 0)
