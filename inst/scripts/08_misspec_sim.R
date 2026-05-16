#!/usr/bin/env Rscript
# Task F: Misspecification simulation, central cell
# Usage:
#   Rscript inst/scripts/08_misspec_sim.R hmc    # HMC only (~1.5h)
#   Rscript inst/scripts/08_misspec_sim.R mh     # MH only (~8-25h)
#   Rscript inst/scripts/08_misspec_sim.R both
#
# WARNING: Requires cmdstanr (for hmc) and rjags (for mh).

args <- commandArgs(trailingOnly = TRUE)
which_sampler <- if (length(args) > 0) args[1] else "both"
if (!which_sampler %in% c("hmc", "mh", "both")) {
  stop("First argument must be one of: hmc, mh, both. Got: ", which_sampler,
    call. = FALSE
  )
}

suppressPackageStartupMessages(devtools::load_all())

run_misspec_simulation(
  n = 2000,
  target_censoring_prop = 0.3,
  weight_type = "high",
  n_replicates = 200L,
  do_hmc = which_sampler %in% c("hmc", "both"),
  do_mh = which_sampler %in% c("mh", "both")
)

message("Misspec simulation done.")
