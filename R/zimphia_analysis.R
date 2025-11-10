#' Run the ZIMPHIA 2020 Interval-Censored Analysis
#'
#' Fits the log-logistic accelerated failure-time model to the ZIMPHIA 2020 HIV
#' seroconversion data using both Hamiltonian Monte Carlo (CmdStanR) and
#' Metropolis-Hastings (JAGS). The workflow mirrors the simulation study:
#' prepare weighted interval-censored data, run both samplers, store summaries,
#' diagnostics, draws, and runtime comparisons, and return a structured summary.
#'
#' @param data_dir Directory that contains the raw ZIMPHIA CSV files
#'   (`zimphia2020adultbio.csv`, `zimphia2020adultind.csv`). Defaults to
#'   `"ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)"`.
#' @param output_dir Directory where prepared data, sampler outputs, and summary
#'   files will be stored. Defaults to `"mcmc_outputs/zimphia"`.
#' @param stan_model_file Path to the CmdStanR model file. Defaults to
#'   `"inst/models/loglogistic_interval.stan"`.
#' @param jags_model_file Path to the JAGS model file. Defaults to
#'   `"inst/models/loglogistic_interval.jags"`.
#' @param hmc_settings Named list overriding the default CmdStanR settings
#'   (`n_chains`, `n_warmup`, `n_sampling`, `parallel_chains`, `seed`,
#'   `refresh`). Missing elements fall back to defaults.
#' @param mh_settings Named list overriding the default JAGS settings
#'   (`n_chains`, `n_adapt`, `n_burnin`, `n_iter`, `seed`). Missing elements fall
#'   back to defaults.
#' @param lower_bound_floor Minimum value applied to the left interval bound `L`
#'   to avoid zero-length intervals (default `1e-10`).
#' @param interval_min_width Minimum positive width enforced when `R <= L`
#'   (default `1e-6`).
#' @param right_censor_placeholder Large finite value passed to JAGS in place of
#'   `Inf` for right-censored observations (default `1e11`).
#' @param verbose Logical; print progress messages and sampler output. Default
#'   `TRUE`.
#'
#' @return Invisibly returns a list with components:
#'   \item{analysis_data}{Final prepared tibble ready for modeling}
#'   \item{hmc_summary}{CmdStanR summary statistics for alpha/beta/gamma}
#'   \item{mh_summary}{JAGS summary statistics for alpha/beta/gamma}
#'   \item{comparison}{Side-by-side comparison tibble of both samplers}
#'   \item{runtime}{Runtime comparison tibble}
#'   \item{diagnostics}{List with `hmc` and `mh` diagnostic tibbles}
#'   \item{files}{Named list of key output file paths}
#'
#' @details
#' The analysis enforces the thesis convergence rules (no missing gender/weights,
#' R-hat < 1.01, bulk ESS > 400, zero HMC divergences) and keeps the simulation
#' parameterization (weights normalized to sample size, gender coded 0/1,
#' log-logistic AFT with alpha, beta, gamma). All outputs are written under
#' `output_dir` for reproducibility and downstream analysis scripts.
#'
#' @examples
#' \dontrun{
#' run_zimphia_analysis()
#'
#' # Custom locations if running from an installed package
#' run_zimphia_analysis(
#'   data_dir = system.file("extdata/zimphia", package = "bayesianICSimulations"),
#'   output_dir = tempfile("zimphia_outputs")
#' )
#' }
#'
#' @seealso [perform_statistical_analysis()] for downstream ADEMP summaries.
#'
#' @importFrom dplyr bind_rows case_when filter if_else left_join mutate select arrange
#' @importFrom readr read_csv write_csv
#' @importFrom tibble tibble
#' @export
run_zimphia_analysis <- function(
    data_dir = file.path("ZIMPHIA", "ZIMPHIA 2020 Datasets (CSV)"),
    output_dir = file.path("mcmc_outputs", "zimphia"),
    stan_model_file = file.path("inst", "models", "loglogistic_interval.stan"),
    jags_model_file = file.path("inst", "models", "loglogistic_interval.jags"),
    hmc_settings = NULL,
    mh_settings = NULL,
    lower_bound_floor = 1e-10,
    interval_min_width = 1e-6,
    right_censor_placeholder = 1e11,
    verbose = TRUE) {
  msg <- function(...) {
    if (isTRUE(verbose)) {
      cat(...)
    }
    invisible(NULL)
  }

  default_hmc <- list(
    n_chains = 4,
    n_warmup = 1000,
    n_sampling = 5000,
    parallel_chains = 4,
    seed = 2025,
    refresh = 500
  )
  if (is.null(hmc_settings)) {
    hmc_settings <- default_hmc
  } else {
    hmc_settings <- utils::modifyList(default_hmc, hmc_settings)
  }

  default_mh <- list(
    n_chains = 4,
    n_adapt = 1000,
    n_burnin = 1000,
    n_iter = 5000,
    seed = 2025
  )
  if (is.null(mh_settings)) {
    mh_settings <- default_mh
  } else {
    mh_settings <- utils::modifyList(default_mh, mh_settings)
  }

  required_suggests <- c("cmdstanr", "rjags", "posterior", "coda")
  missing_suggests <- required_suggests[!vapply(
    required_suggests,
    requireNamespace,
    USE.NAMES = FALSE,
    quietly = TRUE
  )]
  if (length(missing_suggests) > 0) {
    stop(
      "The following packages are required for run_zimphia_analysis(): ",
      paste(missing_suggests, collapse = ", "),
      ". Install them before running the analysis.",
      call. = FALSE
    )
  }

  adultbio_file <- file.path(data_dir, "zimphia2020adultbio.csv")
  adultind_file <- file.path(data_dir, "zimphia2020adultind.csv")

  if (!file.exists(adultbio_file)) {
    stop("Adult biomarker file not found: ", adultbio_file)
  }
  if (!file.exists(adultind_file)) {
    stop("Adult individual file not found: ", adultind_file)
  }
  if (!file.exists(stan_model_file)) {
    stop("Stan model file not found: ", stan_model_file)
  }
  if (!file.exists(jags_model_file)) {
    stop("JAGS model file not found: ", jags_model_file)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  msg("\n=== ZIMPHIA 2020 Bayesian Interval-Censored Survival Analysis ===\n")
  msg("Applying HMC (Stan) and MH (JAGS) to the ZIMPHIA 2020 dataset\n\n")
  msg("Step 1: Loading ZIMPHIA data files...\n")

  adultbio <- readr::read_csv(
    adultbio_file,
    show_col_types = FALSE,
    col_select = c(personid, hivstatusfinal, btwt0, bt_status, age, gender)
  )
  adultind <- readr::read_csv(
    adultind_file,
    show_col_types = FALSE,
    col_select = c(personid, firstsxage, sexever, hivtfposy, gender)
  )

  msg("  Merging datasets on personid...\n")
  zimphia <- adultbio %>%
    left_join(
      adultind %>% select(personid, firstsxage, sexever, hivtfposy),
      by = "personid"
    )
  msg(sprintf("  Merged dataset: %d observations\n", nrow(zimphia)))

  msg("\nStep 2: Applying inclusion/exclusion criteria...\n")
  n_start <- nrow(zimphia)

  zimphia <- zimphia %>% filter(bt_status == 1)
  msg(sprintf(
    "  After bt_status == 1: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))

  zimphia <- zimphia %>% filter(age >= 15)
  msg(sprintf(
    "  After age >= 15: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))

  zimphia <- zimphia %>% filter(sexever == 1)
  msg(sprintf(
    "  After sexever == 1: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))

  zimphia <- zimphia %>% filter(!is.na(firstsxage))
  msg(sprintf(
    "  After non-missing firstsxage: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))

  zimphia <- zimphia %>% filter(!is.na(gender))
  msg(sprintf(
    "  After non-missing gender: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))

  n_hivpos_before <- sum(zimphia$hivstatusfinal == 1, na.rm = TRUE)
  zimphia <- zimphia %>%
    filter(!(hivstatusfinal == 1 & is.na(hivtfposy)))
  n_hivpos_after <- sum(zimphia$hivstatusfinal == 1, na.rm = TRUE)
  msg(sprintf(
    "  After excluding HIV+ with missing hivtfposy: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))
  msg(sprintf(
    "    HIV+ excluded: %d (missing first positive test date)\n",
    n_hivpos_before - n_hivpos_after
  ))

  zimphia <- zimphia %>% filter(!is.na(btwt0) & btwt0 > 0)
  msg(sprintf(
    "  After non-missing/positive weight: %d observations (%.1f%% retained)\n",
    nrow(zimphia), 100 * nrow(zimphia) / n_start
  ))

  msg(sprintf("\nFinal analytical sample: %d observations\n", nrow(zimphia)))
  msg(sprintf(
    "  HIV+: %d (%.1f%%)\n",
    sum(zimphia$hivstatusfinal == 1, na.rm = TRUE),
    100 * mean(zimphia$hivstatusfinal == 1, na.rm = TRUE)
  ))
  msg(sprintf(
    "  HIV-: %d (%.1f%%)\n",
    sum(zimphia$hivstatusfinal == 2, na.rm = TRUE),
    100 * mean(zimphia$hivstatusfinal == 2, na.rm = TRUE)
  ))

  msg("\nStep 3: Constructing interval-censored survival data...\n")
  zimphia <- zimphia %>%
    mutate(
      L = firstsxage,
      R = case_when(
        hivstatusfinal == 2 ~ Inf,
        hivstatusfinal == 1 ~ age,
        TRUE ~ NA_real_
      ),
      L = pmax(L, lower_bound_floor)
    )

  msg("  Interval construction checks:\n")
  msg(sprintf("    Min L: %.1f years\n", min(zimphia$L, na.rm = TRUE)))
  msg(sprintf("    Max L: %.1f years\n", max(zimphia$L, na.rm = TRUE)))
  msg(sprintf(
    "    Min R (HIV+): %.1f years\n",
    min(zimphia$R[zimphia$hivstatusfinal == 1], na.rm = TRUE)
  ))
  msg(sprintf(
    "    Max R (HIV+): %.1f years\n",
    max(zimphia$R[zimphia$hivstatusfinal == 1], na.rm = TRUE)
  ))
  msg(sprintf("    Right-censored (HIV-): %d\n", sum(is.infinite(zimphia$R))))
  msg("  Applied floor to L to avoid zero\n")

  n_nonpos_width <- zimphia %>%
    filter(!is.infinite(R) & R <= L) %>%
    nrow()
  if (n_nonpos_width > 0) {
    zimphia <- zimphia %>%
      mutate(
        R = if_else(
          is.infinite(R) | R > L,
          R,
          L + interval_min_width
        )
      )
    msg(sprintf(
      "  Adjusted %d intervals with non-positive width (added %.0e to R)\n",
      n_nonpos_width, interval_min_width
    ))
  }

  msg("\nStep 4: Preparing covariates and normalizing weights...\n")
  zimphia <- zimphia %>%
    mutate(
      X1 = case_when(
        gender == 1 ~ 0,
        gender == 2 ~ 1,
        TRUE ~ NA_real_
      )
    )

  msg("  Gender distribution:\n")
  msg(sprintf(
    "    Male (X1=0): %d (%.1f%%)\n",
    sum(zimphia$X1 == 0, na.rm = TRUE),
    100 * mean(zimphia$X1 == 0, na.rm = TRUE)
  ))
  msg(sprintf(
    "    Female (X1=1): %d (%.1f%%)\n",
    sum(zimphia$X1 == 1, na.rm = TRUE),
    100 * mean(zimphia$X1 == 1, na.rm = TRUE)
  ))

  N <- nrow(zimphia)
  sum_weights <- sum(zimphia$btwt0)
  if (sum_weights <= 0) {
    stop("Sum of sampling weights must be positive.")
  }
  zimphia <- zimphia %>% mutate(weight = btwt0 * N / sum_weights)

  msg("\n  Weight normalization:\n")
  msg(sprintf("    Original sum(btwt0): %.1f\n", sum_weights))
  msg(sprintf("    Normalized sum(weight): %.1f (expected N=%d)\n", sum(zimphia$weight), N))
  msg(sprintf("    Min weight: %.4f\n", min(zimphia$weight)))
  msg(sprintf("    Max weight: %.4f\n", max(zimphia$weight)))
  msg(sprintf("    Mean weight: %.4f\n", mean(zimphia$weight)))

  msg("\nStep 5: Preparing data for MCMC fitting...\n")
  analysis_data <- zimphia %>%
    select(
      personid,
      L,
      R,
      X1,
      weight,
      hivstatusfinal,
      age,
      gender,
      firstsxage
    ) %>%
    arrange(personid)

  msg(sprintf(
    "  Analysis dataset prepared: %d observations, %d variables\n",
    nrow(analysis_data), ncol(analysis_data)
  ))

  prepared_data_file <- file.path(output_dir, "zimphia_prepared_data.rds")
  saveRDS(analysis_data, prepared_data_file)
  msg(sprintf("  Saved prepared data to: %s\n", prepared_data_file))

  data_summary <- tibble(
    n_total = nrow(analysis_data),
    n_hivpos = sum(analysis_data$hivstatusfinal == 1),
    n_hivneg = sum(analysis_data$hivstatusfinal == 2),
    n_male = sum(analysis_data$X1 == 0),
    n_female = sum(analysis_data$X1 == 1),
    mean_age = mean(analysis_data$age),
    mean_firstsxage = mean(analysis_data$firstsxage),
    median_weight = median(analysis_data$weight),
    mean_L = mean(analysis_data$L),
    mean_R_hivpos = mean(analysis_data$R[analysis_data$hivstatusfinal == 1])
  )

  summary_file <- file.path(output_dir, "zimphia_data_summary.rds")
  saveRDS(data_summary, summary_file)
  readr::write_csv(data_summary, file.path(output_dir, "zimphia_data_summary.csv"))
  msg(sprintf("  Saved data summary to: %s\n", summary_file))

  hmc_dir <- file.path(output_dir, "hmc")
  mh_dir <- file.path(output_dir, "mh")
  for (subdir in c("summaries", "fits", "draws", "diagnostics")) {
    dir.create(file.path(hmc_dir, subdir), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(mh_dir, subdir), recursive = TRUE, showWarnings = FALSE)
  }

  msg("\n=== Starting HMC (Stan) Fitting ===\n")
  stan_data <- list(
    N = nrow(analysis_data),
    L = analysis_data$L,
    R = analysis_data$R,
    X = analysis_data$X1,
    w = analysis_data$weight
  )
  msg(sprintf("  Stan data prepared: N=%d\n", stan_data$N))

  stan_model <- cmdstanr::cmdstan_model(stan_model_file)
  msg(sprintf("  Compiled Stan model: %s\n", stan_model_file))

  msg("  Running HMC sampling...\n")
  msg(sprintf("    Chains: %d\n", hmc_settings$n_chains))
  msg(sprintf("    Warmup: %d iterations\n", hmc_settings$n_warmup))
  msg(sprintf("    Sampling: %d iterations\n", hmc_settings$n_sampling))
  msg(sprintf("    Seed: %d\n", hmc_settings$seed))

  hmc_start_time <- Sys.time()
  hmc_fit <- stan_model$sample(
    data = stan_data,
    chains = hmc_settings$n_chains,
    parallel_chains = hmc_settings$parallel_chains,
    iter_warmup = hmc_settings$n_warmup,
    iter_sampling = hmc_settings$n_sampling,
    seed = hmc_settings$seed,
    refresh = if (isTRUE(verbose)) hmc_settings$refresh else 0,
    show_messages = isTRUE(verbose)
  )
  hmc_end_time <- Sys.time()
  hmc_runtime <- as.numeric(difftime(hmc_end_time, hmc_start_time, units = "secs"))
  msg(sprintf(
    "  HMC sampling completed in %.1f seconds (%.1f minutes)\n",
    hmc_runtime, hmc_runtime / 60
  ))

  msg("  Extracting HMC results...\n")
  hmc_summary <- hmc_fit$summary(
    variables = c("alpha", "beta", "gamma"),
    "mean",
    "median",
    "sd",
    ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
    "rhat",
    "ess_bulk",
    "ess_tail"
  )
  hmc_summary_file <- file.path(hmc_dir, "summaries", "zimphia_hmc_summary.rds")
  saveRDS(hmc_summary, hmc_summary_file)
  readr::write_csv(
    hmc_summary,
    file.path(hmc_dir, "summaries", "zimphia_hmc_summary.csv")
  )

  hmc_draws <- hmc_fit$draws(
    variables = c("alpha", "beta", "gamma"),
    format = "df"
  )
  hmc_draws_file <- file.path(hmc_dir, "draws", "zimphia_hmc_draws.rds")
  saveRDS(hmc_draws, hmc_draws_file)

  hmc_fit_file <- file.path(hmc_dir, "fits", "zimphia_hmc_fit.rds")
  hmc_fit$save_object(hmc_fit_file)

  sampler_diag <- hmc_fit$sampler_diagnostics(format = "df")
  hmc_diagnostics <- tibble(
    method = "hmc",
    dataset = "zimphia",
    n_obs = stan_data$N,
    max_rhat = max(hmc_summary$rhat, na.rm = TRUE),
    min_ess_bulk = min(hmc_summary$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(hmc_summary$ess_tail, na.rm = TRUE),
    n_divergences = sum(sampler_diag$divergent__),
    runtime_secs = hmc_runtime,
    n_chains = hmc_settings$n_chains,
    n_warmup = hmc_settings$n_warmup,
    n_sampling = hmc_settings$n_sampling
  )
  hmc_diag_file <- file.path(hmc_dir, "diagnostics", "zimphia_hmc_diagnostics.rds")
  saveRDS(hmc_diagnostics, hmc_diag_file)
  readr::write_csv(
    hmc_diagnostics,
    file.path(hmc_dir, "diagnostics", "zimphia_hmc_diagnostics.csv")
  )
  if (isTRUE(verbose)) {
    msg("\n  HMC Results:\n")
    print(hmc_summary)
    msg("\n  HMC Diagnostics:\n")
    print(hmc_diagnostics)
  }

  msg("\n=== Starting MH (JAGS) Fitting ===\n")
  jags_data <- list(
    N = nrow(analysis_data),
    L = analysis_data$L,
    R = ifelse(is.infinite(analysis_data$R), right_censor_placeholder, analysis_data$R),
    X = analysis_data$X1,
    w = analysis_data$weight,
    zeros = rep(0, nrow(analysis_data))
  )
  msg(sprintf("  JAGS data prepared: N=%d\n", jags_data$N))

  jags_model_string <- readLines(jags_model_file)
  set.seed(mh_settings$seed)
  msg("  Initializing JAGS model...\n")
  jags_model <- rjags::jags.model(
    file = textConnection(jags_model_string),
    data = jags_data,
    n.chains = mh_settings$n_chains,
    n.adapt = mh_settings$n_adapt,
    quiet = !isTRUE(verbose)
  )

  msg(sprintf(
    "  Running burn-in: %d iterations per chain...\n",
    mh_settings$n_burnin
  ))
  rjags::update(
    jags_model,
    n.iter = mh_settings$n_burnin,
    progress.bar = if (isTRUE(verbose)) "text" else "none"
  )

  msg(sprintf(
    "  Running MCMC sampling: %d iterations per chain...\n",
    mh_settings$n_iter
  ))
  mh_start_time <- Sys.time()
  mh_samples <- rjags::coda.samples(
    model = jags_model,
    variable.names = c("alpha", "beta", "gamma"),
    n.iter = mh_settings$n_iter,
    progress.bar = if (isTRUE(verbose)) "text" else "none"
  )
  mh_end_time <- Sys.time()
  mh_runtime <- as.numeric(difftime(mh_end_time, mh_start_time, units = "secs"))
  msg(sprintf(
    "  MH sampling completed in %.1f seconds (%.1f minutes)\n",
    mh_runtime, mh_runtime / 60
  ))

  msg("  Extracting MH results...\n")
  mh_summary_stats <- summary(mh_samples)
  mh_summary <- tibble(
    variable = rownames(mh_summary_stats$statistics),
    mean = mh_summary_stats$statistics[, "Mean"],
    sd = mh_summary_stats$statistics[, "SD"],
    q2.5 = mh_summary_stats$quantiles[, "2.5%"],
    median = mh_summary_stats$quantiles[, "50%"],
    q97.5 = mh_summary_stats$quantiles[, "97.5%"]
  )
  gelman_diag <- coda::gelman.diag(mh_samples, multivariate = FALSE)
  ess <- coda::effectiveSize(mh_samples)
  mh_summary <- mh_summary %>%
    mutate(
      rhat = gelman_diag$psrf[variable, "Point est."],
      ess = ess[variable]
    )

  mh_summary_file <- file.path(mh_dir, "summaries", "zimphia_mh_summary.rds")
  saveRDS(mh_summary, mh_summary_file)
  readr::write_csv(
    mh_summary,
    file.path(mh_dir, "summaries", "zimphia_mh_summary.csv")
  )

  mh_draws <- as.data.frame(as.matrix(mh_samples))
  mh_draws <- mh_draws %>%
    mutate(
      .chain = rep(seq_len(mh_settings$n_chains), each = mh_settings$n_iter),
      .iteration = rep(seq_len(mh_settings$n_iter), times = mh_settings$n_chains),
      .draw = seq_len(n())
    )
  mh_draws_file <- file.path(mh_dir, "draws", "zimphia_mh_draws.rds")
  saveRDS(mh_draws, mh_draws_file)

  mh_fit_file <- file.path(mh_dir, "fits", "zimphia_mh_fit.rds")
  saveRDS(mh_samples, mh_fit_file)

  mh_diagnostics <- tibble(
    method = "mh",
    dataset = "zimphia",
    n_obs = jags_data$N,
    max_rhat = max(mh_summary$rhat, na.rm = TRUE),
    min_ess = min(mh_summary$ess, na.rm = TRUE),
    runtime_secs = mh_runtime,
    n_chains = mh_settings$n_chains,
    n_adapt = mh_settings$n_adapt,
    n_burnin = mh_settings$n_burnin,
    n_iter = mh_settings$n_iter
  )
  mh_diag_file <- file.path(mh_dir, "diagnostics", "zimphia_mh_diagnostics.rds")
  saveRDS(mh_diagnostics, mh_diag_file)
  readr::write_csv(
    mh_diagnostics,
    file.path(mh_dir, "diagnostics", "zimphia_mh_diagnostics.csv")
  )
  if (isTRUE(verbose)) {
    msg("\n  MH Results:\n")
    print(mh_summary)
    msg("\n  MH Diagnostics:\n")
    print(mh_diagnostics)
  }

  msg("\n=== Method Comparison Summary ===\n")
  comparison <- bind_rows(
    hmc_summary %>%
      select(
        variable,
        mean,
        sd,
        q2.5,
        median = mean,
        q97.5,
        rhat,
        ess = ess_bulk
      ) %>%
      mutate(method = "HMC"),
    mh_summary %>%
      select(variable, mean, sd, q2.5, median, q97.5, rhat, ess) %>%
      mutate(method = "MH")
  ) %>%
    arrange(variable, method)
  if (isTRUE(verbose)) {
    print(comparison)
  }

  comparison_file <- file.path(output_dir, "zimphia_method_comparison.csv")
  readr::write_csv(comparison, comparison_file)
  msg(sprintf("\nSaved method comparison to: %s\n", comparison_file))

  runtime_comparison <- tibble(
    method = c("HMC", "MH"),
    runtime_secs = c(hmc_runtime, mh_runtime),
    runtime_mins = runtime_secs / 60,
    n_chains = c(hmc_settings$n_chains, mh_settings$n_chains),
    total_iterations = c(
      hmc_settings$n_chains *
        (hmc_settings$n_warmup + hmc_settings$n_sampling),
      mh_settings$n_chains *
        (mh_settings$n_adapt + mh_settings$n_burnin + mh_settings$n_iter)
    )
  )
  if (isTRUE(verbose)) {
    print(runtime_comparison)
  }

  runtime_file <- file.path(output_dir, "zimphia_runtime_comparison.csv")
  readr::write_csv(runtime_comparison, runtime_file)
  msg(sprintf("Saved runtime comparison to: %s\n", runtime_file))

  msg("\n=== Analysis Complete ===\n")
  msg(sprintf("Sample size: %d observations\n", nrow(analysis_data)))
  msg(sprintf(
    "HIV+: %d, HIV-: %d\n",
    sum(analysis_data$hivstatusfinal == 1),
    sum(analysis_data$hivstatusfinal == 2)
  ))
  msg("\nOutput directories:\n")
  msg(sprintf("  HMC: %s\n", hmc_dir))
  msg(sprintf("  MH: %s\n", mh_dir))
  msg("\nKey output files:\n")
  msg(sprintf("  Prepared data: %s\n", prepared_data_file))
  msg(sprintf("  HMC summary: %s\n", hmc_summary_file))
  msg(sprintf("  MH summary: %s\n", mh_summary_file))
  msg(sprintf("  Method comparison: %s\n", comparison_file))
  msg(sprintf("  Runtime comparison: %s\n", runtime_file))
  msg("\n=== ZIMPHIA Analysis Complete ===\n\n")

  invisible(list(
    analysis_data = analysis_data,
    hmc_summary = hmc_summary,
    mh_summary = mh_summary,
    comparison = comparison,
    runtime = runtime_comparison,
    diagnostics = list(hmc = hmc_diagnostics, mh = mh_diagnostics),
    files = list(
      prepared_data = prepared_data_file,
      data_summary = summary_file,
      hmc_summary = hmc_summary_file,
      mh_summary = mh_summary_file,
      comparison = comparison_file,
      runtime = runtime_file,
      hmc_diagnostics = hmc_diag_file,
      mh_diagnostics = mh_diag_file,
      hmc_draws = hmc_draws_file,
      mh_draws = mh_draws_file,
      hmc_fit = hmc_fit_file,
      mh_fit = mh_fit_file
    )
  ))
}
