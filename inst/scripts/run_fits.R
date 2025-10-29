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

get_env_int <- function(name, default, min_value = -Inf, max_value = Inf) {
  val <- suppressWarnings(as.integer(Sys.getenv(name, NA_character_)))
  if (is.na(val)) {
    default
  } else {
    val <- max(val, min_value)
    val <- min(val, max_value)
    val
  }
}

get_env_char <- function(name, default) {
  val <- Sys.getenv(name, "")
  if (nzchar(val)) val else default
}

parse_save_components <- function(spec) {
  defaults <- c("summary", "diagnostics")
  if (!nzchar(spec)) {
    return(defaults)
  }
  pieces <- strsplit(spec, "[, ]+")[[1]]
  valid <- intersect(
    unique(tolower(trimws(pieces))),
    c("summary", "diagnostics", "draws", "fits")
  )
  if (length(valid) == 0) defaults else valid
}

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
results_root <- get_env_char("RUN_FITS_RESULTS_DIR", "mcmc_outputs")
results_base <- file.path(base_dir, results_root, paste0("n", sample_size))
stan_model_file <- file.path(base_dir, "loglogistic_interval.stan")
jags_model_file <- file.path(base_dir, "loglogistic_interval.jags")

hmc_save <- parse_save_components(get_env_char("RUN_FITS_HMC_SAVE", "summary,diagnostics"))
mh_save <- parse_save_components(get_env_char("RUN_FITS_MH_SAVE", "summary,diagnostics"))

hmc_config <- list(
  chains = get_env_int("RUN_FITS_HMC_CHAINS", 4L, min_value = 1L),
  parallel_chains = NA_integer_,  # filled later with available_cores
  warmup = get_env_int("RUN_FITS_HMC_WARMUP", 1000L, min_value = 1L),
  iter = get_env_int("RUN_FITS_HMC_ITER", 5000L, min_value = 1L),
  seed = get_env_int("RUN_FITS_HMC_SEED", 2025L, min_value = 1L),
  draws_compress = get_env_char("RUN_FITS_HMC_DRAWS_COMPRESS", "xz"),
  save = hmc_save
)

mh_config <- list(
  chains = get_env_int("RUN_FITS_MH_CHAINS", 4L, min_value = 1L),
  adapt = get_env_int("RUN_FITS_MH_ADAPT", 1000L, min_value = 0L),
  burnin = get_env_int("RUN_FITS_MH_BURNIN", 1000L, min_value = 0L),
  iter = get_env_int("RUN_FITS_MH_ITER", 5000L, min_value = 1L),
  base_seed = get_env_int("RUN_FITS_MH_SEED_BASE", 2025L, min_value = 1L),
  draws_compress = get_env_char("RUN_FITS_MH_DRAWS_COMPRESS", "xz"),
  save = mh_save
)
mh_config$cores <- max(1L, min(mh_config$chains, available_cores))
hmc_config$parallel_chains <- max(1L, min(hmc_config$chains, available_cores))

build_output_paths <- function(method, results_base, file_stem, components) {
  method_root <- file.path(results_base, method)
  file_map <- list(
    summary = file.path(method_root, "summaries", paste0(file_stem, "_summary.rds")),
    diagnostics = file.path(method_root, "diagnostics", paste0(file_stem, "_diag.rds")),
    draws = file.path(method_root, "draws", paste0(file_stem, "_draws.rds")),
    fits = file.path(method_root, "fits", paste0(file_stem, "_fit.rds"))
  )
  file_map[names(file_map) %in% components]
}

ensure_parent_dirs <- function(paths) {
  dirs <- unique(dirname(unlist(paths, use.names = FALSE)))
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

log_saved_paths <- function(paths, method_label) {
  if (length(paths) == 0) return(invisible(NULL))
  cat("Saved", method_label, "outputs:\n")
  invisible(lapply(names(paths), function(name) {
    cat("  -", sprintf("%s: %s\n", name, paths[[name]]))
  }))
  cat("\n")
}

run_hmc_fit <- function(stan_data, mod_stan, config, expected_paths) {
  cat("Starting HMC fit (Stan)...\n")
  cat("Chains:", config$chains, " | Parallel chains:", config$parallel_chains, "\n")
  ensure_parent_dirs(expected_paths)

  hmc_start <- Sys.time()
  fit_hmc <- mod_stan$sample(
    data = stan_data,
    chains = config$chains,
    parallel_chains = config$parallel_chains,
    iter_warmup = config$warmup,
    iter_sampling = config$iter,
    seed = config$seed,
    refresh = 0,
    show_messages = FALSE
  )

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

  max_rhat <- suppressWarnings(max(summ_hmc$rhat, na.rm = TRUE))
  min_ess <- suppressWarnings(min(summ_hmc$ess, na.rm = TRUE))
  min_ess_tail <- if ("ess_tail" %in% names(summ_hmc)) {
    suppressWarnings(min(summ_hmc$ess_tail, na.rm = TRUE))
  } else {
    NA_real_
  }
  mean_ess <- if ("ess" %in% names(summ_hmc)) {
    suppressWarnings(mean(summ_hmc$ess, na.rm = TRUE))
  } else {
    NA_real_
  }

  diag_arr <- tryCatch(fit_hmc$sampler_diagnostics(), error = function(e) NULL)
  divs <- if (!is.null(diag_arr) && "divergent__" %in% dimnames(diag_arr)[[3]]) {
    sum(diag_arr[, , "divergent__"])
  } else {
    NA_integer_
  }
  meta <- fit_hmc$metadata()
  max_td <- meta$max_treedepth %||% 10L
  treedepth_hits <- if (!is.null(diag_arr) && "treedepth__" %in% dimnames(diag_arr)[[3]]) {
    sum(diag_arr[, , "treedepth__"] >= max_td)
  } else {
    NA_integer_
  }

  hmc_end <- Sys.time()
  elapsed <- as.numeric(difftime(hmc_end, hmc_start, units = "secs"))

  cat(sprintf("HMC completed in %.1f seconds\n", elapsed))
  cat(sprintf("  Max Rhat: %.4f, Min ESS: %.0f\n", max_rhat, min_ess))
  cat(sprintf("  Divergences: %s, Max treedepth hits: %s\n\n",
              ifelse(is.na(divs), "NA", divs),
              ifelse(is.na(treedepth_hits), "NA", treedepth_hits)))

  saved_paths <- list()

  if ("summary" %in% config$save) {
    summ_hmc$max_rhat <- max_rhat
    summ_hmc$min_ess <- min_ess
    summ_hmc$total_time <- elapsed

    path <- expected_paths[["summary"]]
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
      path,
      compress = config$draws_compress
    )
    saved_paths$summary <- path
  }

  if ("diagnostics" %in% config$save) {
    diag_summary <- data.frame(
      max_rhat = max_rhat,
      min_ess = min_ess,
      min_ess_tail = min_ess_tail,
      mean_ess_per_sec = mean_ess / elapsed,
      divergences = divs,
      max_treedepth_hit = treedepth_hits,
      total_time_sec = elapsed,
      n_chains = config$chains,
      iter_warmup = config$warmup,
      iter_sampling = config$iter
    )
    path <- expected_paths[["diagnostics"]]
    saveRDS(diag_summary, path)
    saved_paths$diagnostics <- path
  }

  if ("draws" %in% config$save) {
    draws_df <- posterior::as_draws_df(draws_hmc)
    path <- expected_paths[["draws"]]
    saveRDS(draws_df, path, compress = config$draws_compress)
    saved_paths$draws <- path
  }

  if ("fits" %in% config$save) {
    path <- expected_paths[["fits"]]
    suppressMessages(fit_hmc$save_object(path))
    saved_paths$fits <- path
  }

  rm(fit_hmc, draws_hmc)
  gc()

  log_saved_paths(saved_paths, "HMC")

  list(
    total_time = elapsed,
    max_rhat = max_rhat,
    min_ess = min_ess,
    saved_paths = saved_paths
  )
}

run_mh_fit <- function(jags_model_string, jags_data, dataset_idx, config, expected_paths) {
  cat("Starting MH fit (JAGS)...\n")
  cat("Chains:", config$chains, " | Parallel chains (requested):", config$cores, "\n")
  ensure_parent_dirs(expected_paths)

  mh_start <- Sys.time()
  base_seed <- config$base_seed + dataset_idx

  run_chain <- function(chain_id) {
    chain_seed <- base_seed + chain_id
    model_connection <- textConnection(jags_model_string)
    on.exit(close(model_connection), add = TRUE)

    chain_model <- jags.model(
      model_connection,
      data = jags_data,
      n.chains = 1,
      n.adapt = config$adapt,
      inits = list(
        .RNG.name = "base::Wichmann-Hill",
        .RNG.seed = chain_seed
      ),
      quiet = TRUE
    )

    if (config$burnin > 0) {
      update(chain_model, n.iter = config$burnin, progress.bar = "none")
    }

    chain_samples <- coda.samples(
      chain_model,
      variable.names = c("alpha", "beta", "gamma"),
      n.iter = config$iter,
      progress.bar = "none"
    )

    chain_samples[[1]]
  }

  use_mclapply <- (config$cores > 1L) && (.Platform$OS.type != "windows")
  chain_ids <- seq_len(config$chains)
  chain_list <- if (use_mclapply) {
    parallel::mclapply(
      chain_ids,
      run_chain,
      mc.cores = config$cores,
      mc.preschedule = FALSE
    )
  } else {
    lapply(chain_ids, run_chain)
  }

  samples_mh <- as.mcmc.list(chain_list)
  mh_end <- Sys.time()
  elapsed <- as.numeric(difftime(mh_end, mh_start, units = "secs"))

  summ_obj <- summary(samples_mh)
  ess <- coda::effectiveSize(samples_mh)
  rhat <- coda::gelman.diag(samples_mh, multivariate = FALSE)$psrf

  as_matrix <- as.matrix(samples_mh)
  param_names <- colnames(as_matrix)
  summary_df <- data.frame(
    variable = param_names,
    mean = summ_obj$statistics[, "Mean"],
    median = apply(as_matrix, 2, median),
    sd = summ_obj$statistics[, "SD"],
    q2.5 = summ_obj$quantiles[, "2.5%"],
    q97.5 = summ_obj$quantiles[, "97.5%"],
    rhat = if (is.matrix(rhat)) rhat[, "Point est."] else rhat,
    ess = ess,
    stringsAsFactors = FALSE
  )

  max_rhat <- suppressWarnings(max(summary_df$rhat, na.rm = TRUE))
  min_ess <- suppressWarnings(min(summary_df$ess, na.rm = TRUE))

  cat(sprintf("MH completed in %.1f seconds\n", elapsed))
  cat(sprintf("  Max Rhat: %.4f, Min ESS: %.0f\n\n", max_rhat, min_ess))

  saved_paths <- list()

  if ("summary" %in% config$save) {
    summary_df$max_rhat <- max_rhat
    summary_df$min_ess <- min_ess
    summary_df$total_time <- elapsed

    path <- expected_paths[["summary"]]
    saveRDS(summary_df, path, compress = config$draws_compress)
    saved_paths$summary <- path
  }

  if ("diagnostics" %in% config$save) {
    diag_summary <- data.frame(
      max_rhat = max_rhat,
      min_ess = min_ess,
      mean_ess_per_sec = suppressWarnings(mean(summary_df$ess, na.rm = TRUE) / elapsed),
      total_time_sec = elapsed,
      n_chains = config$chains,
      n_iter = config$iter,
      n_adapt = config$adapt,
      n_burnin = config$burnin
    )
    path <- expected_paths[["diagnostics"]]
    saveRDS(diag_summary, path)
    saved_paths$diagnostics <- path
  }

  if ("fits" %in% config$save) {
    path <- expected_paths[["fits"]]
    saveRDS(samples_mh, path, compress = config$draws_compress)
    saved_paths$fits <- path
  }

  if ("draws" %in% config$save) {
    chain_lengths <- vapply(samples_mh, nrow, integer(1))
    draws_df <- as.data.frame(as_matrix)
    draws_df$.chain <- rep(seq_along(chain_lengths), chain_lengths)
    draws_df$.iteration <- sequence(chain_lengths)
    draws_df$.draw <- seq_len(nrow(draws_df))

    path <- expected_paths[["draws"]]
    saveRDS(draws_df, path, compress = config$draws_compress)
    saved_paths$draws <- path
  }

  log_saved_paths(saved_paths, "MH")

  list(
    total_time = elapsed,
    max_rhat = max_rhat,
    min_ess = min_ess,
    saved_paths = saved_paths
  )
}

# Get list of datasets and select the one for this task
# Cache the directory listing to reduce repeated metadata lookups across jobs
index_cache <- file.path(sim_dir, ".file_index.rds")
sim_files <- NULL
if (file.exists(index_cache)) {
  sim_files <- tryCatch(readRDS(index_cache), error = function(e) NULL)
  if (!is.null(sim_files)) {
    cat("Using cached dataset index:", index_cache, "\n")
  }
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
    if (success) {
      cat("Cached dataset index at:", index_cache, "\n")
    }
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

# Skip work if configured outputs already exist -------------------------------
hmc_expected <- build_output_paths("hmc", results_base, file_stem, hmc_config$save)
mh_expected <- build_output_paths("mh", results_base, file_stem, mh_config$save)
expected_files <- unlist(c(hmc_expected, mh_expected), use.names = FALSE)

if (length(expected_files) > 0 && all(file.exists(expected_files))) {
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

compiled_dir <- file.path(base_dir, "compiled_models")
model_info_file <- file.path(compiled_dir, "model_info.rds")

if (file.exists(model_info_file)) {
  model_info <- readRDS(model_info_file)
  cat("Using pre-compiled model:", model_info$exe_file, "\n")
  mod_stan <- cmdstan_model(exe_file = model_info$exe_file)
} else {
  cat("Pre-compiled model not found, compiling now...\n")
  cat("(Run compile_stan.R first to avoid this overhead)\n")
  mod_stan <- cmdstan_model(stan_model_file)
}

hmc_result <- run_hmc_fit(stan_data, mod_stan, hmc_config, hmc_expected)

# ============================================================================
# MH (JAGS) Fit - PARALLELIZED
# ============================================================================

# Load JAGS model
jags_model_string <- paste(readLines(jags_model_file), collapse = "\n")

mh_result <- run_mh_fit(jags_model_string, jags_data, dataset_idx, mh_config, mh_expected)

# ============================================================================
# Summary
# ============================================================================
cat("========================================\n")
cat("COMPLETED:", file_stem, "\n")
cat(sprintf(
  "Total time: %.1f seconds (HMC: %.1f, MH: %.1f)\n",
  hmc_result$total_time + mh_result$total_time,
  hmc_result$total_time,
  mh_result$total_time
))
cat("========================================\n")

# Return success
quit(status = 0)
