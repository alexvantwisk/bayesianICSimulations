#' Fit a log-logistic interval-censoring AFT model on pilot data 
#' via Hamiltonian Monte Carlo
#'
#' Runs CmdStanR on each simulated dataset (.rds) found in sim_dir,
#' writing results to results_dir. Progress is reported via \pkg{progressr}
#' and execution is parallelized with \pkg{future}.
#'
#' @section Parallelization:
#' Uses future.batchtools to submit each dataset as a separate PBS job on the
#' HPC cluster. Within each job, MCMC chains run in parallel using all available
#' cores. The future plan is restored to sequential on exit.
#'
#' @importFrom cmdstanr cmdstan_model
#' @importFrom future plan sequential tweak
#' @importFrom future.batchtools batchtools_pbs
#' @importFrom progressr with_progress progressor
#' @importFrom future.apply future_lapply
#' @importFrom posterior as_draws_df
#'
#' @param sim_dir Directory containing simulated .rds datasets.
#' @param results_dir Root directory where outputs will be written. Subfolders
#'   are created as needed.
#' @param model_file Path to the Stan model file. Defaults to the AFT model
#'   "models/loglogistic_interval.stan" in bayesianSimBasis.
#' @param save Character vector controlling what to persist for each fit.
#'   Any of c("summary","fit","draws","diagnostics"). Default saves both
#'   the compact summary and the full fit.
#' @param draws_variables Optional character vector of parameter names to export
#'   when save includes "draws". If NULL, all monitored parameters are exported.
#' @param draws_compress Compression for saved draws RDS.
#' @param workers Number of parallel PBS jobs to submit (one per dataset).
#'   Default is NULL, which submits all datasets as separate jobs.
#'
#' @return Invisibly returns a tibble indexing input files and written outputs.
#' @export
fit_logistic_hmc <- function(
    sim_dir = "data/sim_data",
    results_dir = "results/hmc_summary",
    model_file = "inst/models/loglogistic_interval.stan",
    save = c("summary","fit","draws","diagnostics"),
    draws_variables = NULL,
    draws_compress = "xz",
    workers = NULL
) {
  
  save <- match.arg(save, c("summary","fit","draws","diagnostics"), several.ok = TRUE)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  # output dirs
  dir_summary     <- file.path(results_dir, "summaries")
  dir_fit         <- file.path(results_dir, "fits")
  dir_draws       <- file.path(results_dir, "draws")
  dir_diagnostics <- file.path(results_dir, "diagnostics")
  for (d in c(dir_summary, dir_fit, dir_draws, dir_diagnostics)) if (!dir.exists(d)) dir.create(d, TRUE)
  
  # inputs
  sim_files <- list.files(path = sim_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(sim_files) == 0) {
    warning("No simulation files found in ", sim_dir)
    return(invisible(NULL))
  }
  
  # model path fallback to installed package copy if local file missing
  if (!file.exists(model_file)) {
    alt <- try(system.file("models", "loglogistic_interval.stan", package = "bayesianSimBasis"), silent = TRUE)
    if (!inherits(alt, "try-error") && nzchar(alt)) model_file <- alt
  }
  if (!file.exists(model_file)) stop("Stan model file not found: ", model_file)
  
  mod_aft <- cmdstanr::cmdstan_model(model_file)

  # HPC parallelism via PBS
  # Each dataset gets its own PBS job; if workers is NULL, submit all
  if (is.null(workers)) workers <- length(sim_files)

  # Configure PBS resources for batchtools
  pbs_resources <- list(
    ncpus = 4,              # 4 cores for parallel chains
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
  
  fit_one <- function(file_path) {
    dat <- readRDS(file_path)
    
    # NOTE: If your Stan model requires an indicator for right-censoring,
    # compute it here and replace Inf in R with a large sentinel consistently
    # with the Stan code. The line below passes R as-is.
    stan_data <- list(
      N = nrow(dat),
      L = pmax(dat$L, 1e-10),
      R = dat$R,
      X = as.numeric(dat$X1),
      w = if (!is.null(dat$weight)) as.numeric(dat$weight) else rep(1, nrow(dat))
    )
    
    # sample with modestly robust defaults (adjust as needed)
    start_time <- Sys.time()
    fit <- mod_aft$sample(
      data            = stan_data,
      chains          = 4,
      parallel_chains = 4,      # use all 4 cores for parallel chains
      iter_warmup     = 1000,
      iter_sampling   = 5000,
      seed            = 2025,
      refresh         = 0
    )
    end_time   <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # summarize with 95% CI (q2.5, q97.5)
    # get draws once (no warmup)
    dr <- fit$draws(inc_warmup = FALSE)
    
    # summarise explicitly; use formula so .x is provided by summarise_draws()
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
    
    # standardise column names to match your MH summary
    if ("ess_bulk" %in% names(summ)) summ$ess <- summ$ess_bulk
    
    # add runtime and aggregate diagnostics for parity with MH
    max_rhat <- suppressWarnings(max(summ$rhat, na.rm = TRUE))
    min_ess  <- suppressWarnings(min(summ$ess,  na.rm = TRUE))
    
    summ$max_rhat   <- max_rhat
    summ$min_ess    <- min_ess
    summ$total_time <- total_time
    
    
    # file stems and bookkeeping
    stem    <- basename(sub("\\.rds$", "", file_path))
    written <- list(input = file_path)
    
    # SUMMARY ---------------------------------------------------------------
    if ("summary" %in% save) {
      summ$max_rhat   <- max_rhat
      summ$min_ess    <- min_ess
      summ$total_time <- total_time
      
      path <- file.path(dir_summary, paste0(stem, "_summary.rds"))
      saveRDS(summ[, c("variable","mean","median","sd","q2.5","q97.5","rhat","ess","max_rhat","min_ess","total_time")],
              path, compress = "xz")
      written$summary <- path
    }
    
    # FIT -------------------------------------------------------------------
    if ("fit" %in% save) {
      path <- file.path(dir_fit, paste0(stem, "_fit.rds"))
      suppressMessages(fit$save_object(path))
      written$fit <- path
    }
    
    # DRAWS -----------------------------------------------------------------
    if ("draws" %in% save) {
      dr    <- fit$draws(variables = draws_variables, inc_warmup = FALSE)
      dr_df <- posterior::as_draws_df(dr)  # includes .chain, .iteration, .draw
      path  <- file.path(dir_draws, paste0(stem, "_draws.rds"))
      saveRDS(dr_df, path, compress = draws_compress)
      written$draws <- path
    }
    
    # DIAGNOSTICS -----------------------------------------------------------
    if ("diagnostics" %in% save) {
      diag_arr <- fit$sampler_diagnostics()
      # divergences and treedepth
      divs <- tryCatch(sum(diag_arr[, , "divergent__"]), error = function(e) NA_integer_)
      mtd  <- tryCatch(fit$metadata()$max_treedepth, error = function(e) NA_integer_)
      tdh  <- tryCatch(sum(diag_arr[, , "treedepth__"] >= mtd), error = function(e) NA_integer_)
      mean_ess_per_sec <- suppressWarnings(mean(summ$ess, na.rm = TRUE) / total_time)
      
      diag_summary <- data.frame(
        max_rhat         = max_rhat,
        min_ess          = min_ess,
        min_ess_tail     = if ("ess_tail" %in% names(summ)) min(summ$ess_tail, na.rm = TRUE) else NA_real_,
        mean_ess_per_sec = mean_ess_per_sec,
        divergences      = divs,
        max_treedepth_hit = tdh,
        total_time_sec   = total_time,
        n_chains         = fit$metadata()$chains %||% 4,
        iter_warmup      = fit$metadata()$iter_warmup %||% 1000,
        iter_sampling    = fit$metadata()$iter_sampling %||% 5000
      )
      
      path <- file.path(dir_diagnostics, paste0(stem, "_diag.rds"))
      saveRDS(diag_summary, path)
      written$diagnostics <- path
    }
    
    list(
      input = file_path,
      outputs = written,
      timing = list(total = total_time, sampling = total_time),
      diagnostics = list(max_rhat = max_rhat, min_ess = min_ess)
    )
  }
  
  res <- NULL
  progressr::with_progress({
    p <- progressr::progressor(along = sim_files)
    res <- future.apply::future_lapply(
      X = sim_files,
      FUN = function(fp) { p(sprintf("Running: %s", basename(fp))); fit_one(fp) },
      future.seed = TRUE
    )
  })
  
  future::plan(future::sequential)
  
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
