#!/usr/bin/env Rscript
# inst/scripts/00_run_all_deferred.R
#
# Master orchestrator for the BMC substantive-revision deferred work.
# Each step calls one of the existing runner scripts (or builds a post-fit
# summary). Steps are isolated by sourcing in `new.env()` so namespace
# pollution doesn't leak.
#
# CALL FROM THE PACKAGE ROOT:
#   Rscript inst/scripts/00_run_all_deferred.R               # run every step in order
#   Rscript inst/scripts/00_run_all_deferred.R quick         # only the < 5-minute steps
#   Rscript inst/scripts/00_run_all_deferred.R <step_name>   # run one step
#
# AVAILABLE STEPS (rough timing on an 8-core laptop):
#   preflight        ~10s    cmdstanr installed; required input files on disk
#   multivariable    ~5 min  Task B HMC fit (sex + urban/rural + age band)
#   cohort           ~6 min  Task C — three cohort HMC fits + forest plot
#   incidence        ~5 sec  Task D post-processing (already ran in this round; idempotent)
#   design_variance  ~2.4 h  Task E — 100 HMC refits across ZIMPHIA replicate weights
#   misspec_hmc      ~1.5 h  Task F HMC leg on Weibull-truth data
#   misspec_mh       ~25 h   Task F MH leg — kick off OVERNIGHT (nohup recommended)
#   hmc1000_n2000    ~3 h    Task G n=2,000 cell (1,000 HMC reps)
#   hmc1000_n10000   ~25 h   Task G n=10,000 cell (1,000 HMC reps) — kick off OVERNIGHT
#   summaries        ~1 min  Build all post-fit comparison tables and plots
#
# RECOMMENDED CALENDAR (matches tasks/revision_plan_substantive.md §4):
#   Day 1 morning   : Rscript inst/scripts/00_run_all_deferred.R quick
#                       (preflight + multivariable + cohort + incidence + misspec_hmc + summaries-so-far)
#   Day 1 evening   : nohup Rscript inst/scripts/00_run_all_deferred.R misspec_mh >> logs/.../misspec_mh.log 2>&1 &
#   Day 2 morning   : Rscript inst/scripts/00_run_all_deferred.R design_variance   (~2.4 h)
#   Day 2 evening   : nohup Rscript inst/scripts/00_run_all_deferred.R hmc1000_n2000 >> .../hmc1000_n2000.log 2>&1 &
#   Day 3 morning   : nohup Rscript inst/scripts/00_run_all_deferred.R hmc1000_n10000 >> .../hmc1000_n10000.log 2>&1 &
#   Day 4 morning   : Rscript inst/scripts/00_run_all_deferred.R summaries
#
# Do NOT run misspec_mh and hmc1000_n10000 on the same overnight slot — they will fight for cores.

# ---- Bootstrap -----------------------------------------------------------

if (!file.exists("DESCRIPTION") ||
  !grepl("^Package:\\s*bayesianICSimulations", readLines("DESCRIPTION", n = 1L))) {
  stop("Run this script from the bayesianICSimulations package root.", call. = FALSE)
}

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(dplyr)
  library(readr)
  library(ggplot2)
})

log_dir <- "logs/substantive_revision"
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

run_step <- function(name, fn) {
  banner <- strrep("-", 72)
  message("\n", banner)
  message(sprintf(
    "[%s] STEP: %s  START",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), name
  ))
  message(banner)
  t0 <- Sys.time()
  status <- tryCatch(
    {
      fn()
      "OK"
    },
    error = function(e) {
      message("[", format(Sys.time(), "%H:%M:%S"), "] ERROR in ", name, ": ", e$message)
      "ERROR"
    }
  )
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf(
    "[%s] STEP: %s  %s in %.1f s (%.2f h)",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    name, status, dt, dt / 3600
  ))
  invisible(status)
}

source_isolated <- function(script_path) {
  # Run the script in its own environment to keep its globals out of ours.
  env <- new.env(parent = globalenv())
  sys.source(script_path, envir = env)
  invisible(env)
}

# ---- STEP DEFINITIONS ----------------------------------------------------

step_preflight <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr is not installed. Install with:\n",
      '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))\n',
      "Then run `cmdstanr::install_cmdstan()` once (~10-15 min compile).",
      call. = FALSE
    )
  }
  cs_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
  if (!nzchar(cs_path) || !dir.exists(cs_path)) {
    stop("CmdStan is not installed. Run cmdstanr::install_cmdstan() first.",
      call. = FALSE
    )
  }
  message("cmdstanr OK. cmdstan path: ", cs_path)

  required <- c(
    "mcmc_outputs/zimphia/zimphia_prepared_data.rds",
    "mcmc_outputs/zimphia/hmc/draws/zimphia_hmc_draws.rds",
    "mcmc_outputs/zimphia/hmc/summaries/zimphia_hmc_summary.csv",
    "ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)/zimphia2020adultind.csv",
    "ZIMPHIA/ZIMPHIA 2020 Intermediary Weights (CSV)/zimphia2020indintermediarywts.csv",
    "inst/models/loglogistic_interval.stan",
    "inst/models/loglogistic_interval_multivariable.stan"
  )
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    stop("Missing required inputs:\n  ", paste(missing, collapse = "\n  "),
      call. = FALSE
    )
  }
  message("All required input files present.")
}

step_multivariable <- function() {
  source_isolated("inst/scripts/04_zimphia_multivariable.R")
}

step_cohort <- function() {
  source_isolated("inst/scripts/05_zimphia_cohort.R")
}

step_incidence <- function() {
  # Idempotent — already ran in the revision round. Re-running regenerates the
  # figure and table from the saved draws if the inputs haven't changed.
  source_isolated("inst/scripts/06_zimphia_incidence.R")
}

step_design_variance <- function() {
  message("This step refits the ZIMPHIA HMC model 100 times. ETA ~2.4 hours.")
  source_isolated("inst/scripts/07_zimphia_design_variance.R")
}

step_misspec_hmc <- function() {
  message("This step runs 200 HMC fits at the misspec central cell. ETA ~1.5 hours.")
  args_backup <- commandArgs(trailingOnly = FALSE)
  # The 08_misspec_sim.R script reads commandArgs(); fake "hmc" for it.
  assign("commandArgs", function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) "hmc" else args_backup
  }, envir = globalenv())
  on.exit(rm("commandArgs", envir = globalenv()), add = TRUE)
  source_isolated("inst/scripts/08_misspec_sim.R")
}

step_misspec_mh <- function() {
  message("This step runs 200 MH fits at the misspec central cell. ETA ~25 hours.")
  message("Strongly recommended: kick off via `nohup ... &` instead of foreground.")
  args_backup <- commandArgs(trailingOnly = FALSE)
  assign("commandArgs", function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) "mh" else args_backup
  }, envir = globalenv())
  on.exit(rm("commandArgs", envir = globalenv()), add = TRUE)
  source_isolated("inst/scripts/08_misspec_sim.R")
}

step_hmc1000_n2000 <- function() {
  message("This step runs 1,000 HMC fits at n=2,000 central cell. ETA ~3 hours.")
  args_backup <- commandArgs(trailingOnly = FALSE)
  assign("commandArgs", function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) "n2000" else args_backup
  }, envir = globalenv())
  on.exit(rm("commandArgs", envir = globalenv()), add = TRUE)
  source_isolated("inst/scripts/09_hmc_1000_rerun.R")
}

step_hmc1000_n10000 <- function() {
  message("This step runs 1,000 HMC fits at n=10,000 central cell. ETA ~25 hours.")
  message("Strongly recommended: kick off via `nohup ... &`.")
  args_backup <- commandArgs(trailingOnly = FALSE)
  assign("commandArgs", function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) "n10000" else args_backup
  }, envir = globalenv())
  on.exit(rm("commandArgs", envir = globalenv()), add = TRUE)
  source_isolated("inst/scripts/09_hmc_1000_rerun.R")
}

# ---- POST-FIT SUMMARY BUILDERS ------------------------------------------
# Each of these is safe to call multiple times; each guards against missing
# inputs and skips the corresponding artefact if the prerequisite fit hasn't
# run yet.

build_multivariable_summary <- function() {
  univ_file <- "mcmc_outputs/zimphia/hmc/summaries/zimphia_hmc_summary.csv"
  mult_file <- "mcmc_outputs/zimphia_multivariable/summaries/summary.csv"
  if (!file.exists(mult_file)) {
    message("Skipping multivariable summary — `", mult_file, "` not found.")
    return(invisible(NULL))
  }
  univ <- read_csv(univ_file, show_col_types = FALSE)
  mult <- read_csv(mult_file, show_col_types = FALSE)
  out <- tibble::tibble(
    parameter = "beta_sex",
    univariate_median = univ$median[univ$variable == "beta"],
    univariate_ci = sprintf(
      "(%.3f, %.3f)",
      univ$q2.5[univ$variable == "beta"],
      univ$q97.5[univ$variable == "beta"]
    ),
    multivariable_median = mult$median[mult$covariate == "sex"],
    multivariable_ci = sprintf(
      "(%.3f, %.3f)",
      mult$q2.5[mult$covariate == "sex"],
      mult$q97.5[mult$covariate == "sex"]
    )
  )
  dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
  write_csv(out, "outputs/tables/tab_multivariable_compare.csv")
  message("Wrote outputs/tables/tab_multivariable_compare.csv")
  print(out)
}

build_cohort_forest <- function() {
  rds <- "mcmc_outputs/zimphia_cohort/combined_summary.rds"
  if (!file.exists(rds)) {
    message("Skipping cohort forest — `", rds, "` not found.")
    return(invisible(NULL))
  }
  combined <- readRDS(rds)
  p <- combined |>
    filter(variable %in% c("alpha", "gamma")) |>
    ggplot(aes(y = cohort, x = median, xmin = q2.5, xmax = q97.5)) +
    geom_pointrange() +
    facet_wrap(~variable, scales = "free_x") +
    labs(
      x = "Posterior median (95% CrI)", y = NULL,
      title = "ZIMPHIA posteriors by birth cohort"
    ) +
    theme_bw()
  dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
  ggsave("outputs/figures/figC1_cohort_forest.png", p,
    width = 8, height = 4, dpi = 320
  )
  message("Wrote outputs/figures/figC1_cohort_forest.png")
}

build_design_variance_forest <- function() {
  rds <- "mcmc_outputs/zimphia_design_replicates/all_replicates.csv"
  if (!file.exists(rds)) {
    message("Skipping design-variance forest — `", rds, "` not found.")
    return(invisible(NULL))
  }
  combined <- read_csv(rds, show_col_types = FALSE)
  p <- combined |>
    filter(variable == "beta") |>
    ggplot(aes(x = median, y = factor(replicate))) +
    geom_pointrange(aes(xmin = q2.5, xmax = q97.5), size = 0.2) +
    geom_vline(xintercept = -0.156, linetype = "dashed") +
    scale_y_discrete(breaks = c("1", "25", "50", "75", "100")) +
    labs(
      x = "Posterior median (95% CrI) for beta_sex",
      y = "Replicate weight index",
      title = "Design-based variance: 100 ZIMPHIA replicate-weight refits"
    ) +
    theme_bw()
  dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
  ggsave("outputs/figures/fig9_design_variance_forest.png", p,
    width = 7, height = 9, dpi = 320
  )
  message("Wrote outputs/figures/fig9_design_variance_forest.png")
}

build_misspec_summary <- function() {
  hmc_dir <- "mcmc_outputs/misspec/n2000_c0.3_whigh/hmc/summaries"
  mh_dir <- "mcmc_outputs/misspec/n2000_c0.3_whigh/mh/summaries"
  hmc_files <- if (dir.exists(hmc_dir)) {
    list.files(hmc_dir, pattern = "_summary\\.rds$", full.names = TRUE)
  } else {
    character(0)
  }
  mh_files <- if (dir.exists(mh_dir)) {
    list.files(mh_dir, pattern = "_summary\\.rds$", full.names = TRUE)
  } else {
    character(0)
  }
  if (length(hmc_files) == 0L && length(mh_files) == 0L) {
    message("Skipping misspec summary — no fit output found.")
    return(invisible(NULL))
  }

  agg <- function(files, sampler) {
    if (length(files) == 0L) {
      return(NULL)
    }
    purrr::map_dfr(files, \(f) {
      readRDS(f) |> mutate(sampler = sampler, file = basename(f))
    })
  }
  all_summ <- bind_rows(agg(hmc_files, "HMC"), agg(mh_files, "MH"))
  truth_beta <- -0.5

  out <- all_summ |>
    filter(variable == "beta") |>
    group_by(sampler) |>
    summarise(
      bias = mean(median - truth_beta),
      rmse = sqrt(mean((median - truth_beta)^2)),
      coverage = mean(q2.5 <= truth_beta & truth_beta <= q97.5),
      mean_total_time = mean(total_time, na.rm = TRUE),
      mean_ess = mean(min_ess, na.rm = TRUE),
      ess_per_sec = mean_ess / mean_total_time,
      n_replicates = n(),
      .groups = "drop"
    )
  dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
  write_csv(out, "outputs/tables/tab_misspec_summary.csv")
  message("Wrote outputs/tables/tab_misspec_summary.csv")
  print(out)

  if (nrow(out) >= 1L) {
    p <- out |>
      tidyr::pivot_longer(c(bias, rmse, coverage, ess_per_sec)) |>
      ggplot(aes(x = sampler, y = value, fill = sampler)) +
      geom_col() +
      facet_wrap(~name, scales = "free_y") +
      labs(
        title = "Misspecified DGM (Weibull): central cell only",
        subtitle = "n=2000, censoring=0.3, weight CV ~1.0"
      ) +
      theme_bw() +
      theme(legend.position = "none")
    ggsave("outputs/figures/fig10_misspec_compare.png", p,
      width = 9, height = 6, dpi = 320
    )
    message("Wrote outputs/figures/fig10_misspec_compare.png")
  }
}

build_hmc1000_verification <- function() {
  cells <- list(
    n2000  = "mcmc_outputs/hmc1000/n2000_c0.3_whigh/hmc/summaries",
    n10000 = "mcmc_outputs/hmc1000/n10000_c0.3_whigh/hmc/summaries"
  )
  rows <- list()
  for (nm in names(cells)) {
    d <- cells[[nm]]
    if (!dir.exists(d)) {
      message("Skipping ", nm, " verification — no summaries directory.")
      next
    }
    files <- list.files(d, pattern = "_summary\\.rds$", full.names = TRUE)
    if (length(files) == 0L) {
      message("Skipping ", nm, " verification — zero summaries.")
      next
    }
    rep_summ <- purrr::map_dfr(files, readRDS) |>
      filter(variable == "beta") |>
      summarise(
        cell = nm,
        n_replicates = n(),
        bias = mean(median - (-0.5)),
        rmse = sqrt(mean((median - (-0.5))^2)),
        mc_se_rmse = rmse / sqrt(2 * n_replicates),
        mean_total_time = mean(total_time, na.rm = TRUE)
      )
    rows[[nm]] <- rep_summ
  }
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  out <- bind_rows(rows)
  dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
  write_csv(out, "outputs/tables/tab_hmc1000_central_cells.csv")
  message("Wrote outputs/tables/tab_hmc1000_central_cells.csv")
  print(out)
}

step_summaries <- function() {
  build_multivariable_summary()
  build_cohort_forest()
  build_design_variance_forest()
  build_misspec_summary()
  build_hmc1000_verification()
  message("\nAll post-fit summaries that have inputs available have been written.")
  message("Output directories:")
  message("  outputs/tables/")
  message("  outputs/figures/")
}

# ---- DISPATCH ------------------------------------------------------------

steps <- list(
  preflight       = step_preflight,
  multivariable   = step_multivariable,
  cohort          = step_cohort,
  incidence       = step_incidence,
  design_variance = step_design_variance,
  misspec_hmc     = step_misspec_hmc,
  misspec_mh      = step_misspec_mh,
  hmc1000_n2000   = step_hmc1000_n2000,
  hmc1000_n10000  = step_hmc1000_n10000,
  summaries       = step_summaries
)

quick_order <- c(
  "preflight", "multivariable", "cohort", "incidence",
  "misspec_hmc", "summaries"
)

all_order <- c(
  "preflight", "multivariable", "cohort", "incidence",
  "design_variance", "misspec_hmc", "misspec_mh",
  "hmc1000_n2000", "hmc1000_n10000", "summaries"
)

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1L) args[1] else "all"

if (mode == "all") {
  message("Mode: all (every step in canonical order; expect many hours of compute)")
  for (s in all_order) run_step(s, steps[[s]])
} else if (mode == "quick") {
  message("Mode: quick (only the < 5-minute steps; finishes in < 2 hours total)")
  for (s in quick_order) run_step(s, steps[[s]])
} else if (mode %in% names(steps)) {
  run_step(mode, steps[[mode]])
} else {
  stop("Unknown step: '", mode, "'. Valid: ",
    paste(c("all", "quick", names(steps)), collapse = ", "),
    call. = FALSE
  )
}

message("\nDone.")
