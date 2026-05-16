#!/usr/bin/env Rscript
# Task G: HMC 1,000-rep rerun at two central cells
# Usage:
#   Rscript inst/scripts/09_hmc_1000_rerun.R n2000   # ~3h on 8-core laptop
#   Rscript inst/scripts/09_hmc_1000_rerun.R n10000  # ~25h on 8-core laptop
#
# Requires cmdstanr.

args <- commandArgs(trailingOnly = TRUE)
cell <- if (length(args) > 0) args[1] else "n2000"
if (!cell %in% c("n2000", "n10000")) {
  stop("First argument must be one of: n2000, n10000. Got: ", cell,
    call. = FALSE
  )
}

suppressPackageStartupMessages(devtools::load_all())

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop("Package 'cmdstanr' is required. Install from https://mc-stan.org/r-packages/",
    call. = FALSE
  )
}

cfg <- list(
  n2000 = list(
    n_obs = 2000L,
    data_dir = "mcmc_outputs/hmc1000/n2000_c0.3_whigh/sim_data",
    out_dir = "mcmc_outputs/hmc1000/n2000_c0.3_whigh/hmc"
  ),
  n10000 = list(
    n_obs = 10000L,
    data_dir = "mcmc_outputs/hmc1000/n10000_c0.3_whigh/sim_data",
    out_dir = "mcmc_outputs/hmc1000/n10000_c0.3_whigh/hmc"
  )
)[[cell]]

dir.create(cfg$data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

# Generate 1,000 replicate datasets at this cell (only HMC will fit them)
run_simulations(
  out_dir = cfg$data_dir,
  n_obs_vec = cfg$n_obs,
  censoring_props = 0.3,
  weight_types = "high",
  n_replicates = 1000L,
  n_replicates_hmc = 1000L,
  samplers = "hmc"
)

cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
t0 <- Sys.time()
fit_logistic_hmc(
  sim_dir = cfg$data_dir,
  results_dir = cfg$out_dir,
  save = c("summary", "diagnostics"),
  workers = cores
)
message(sprintf(
  "Cell %s done in %.1f hours",
  cell,
  as.numeric(difftime(Sys.time(), t0, units = "hours"))
))
