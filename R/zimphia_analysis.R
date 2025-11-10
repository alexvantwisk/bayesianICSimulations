#!/usr/bin/env Rscript
# ZIMPHIA 2020 Real-World Analysis Script
#
# This script applies the HMC and MH methods validated in the simulation study
# to real-world interval-censored HIV seroconversion data from ZIMPHIA 2020.
#
# Usage: Rscript R/zimphia_analysis.R

# ==============================================================================
# Setup and Libraries
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(cmdstanr)
  library(rjags)
  library(posterior)
  library(coda)
  library(tibble)
})

cat("\n=== ZIMPHIA 2020 Bayesian Interval-Censored Survival Analysis ===\n")
cat(
  "Applying HMC (Stan) and MH (JAGS) to real-world HIV seroconversion data\n\n"
)

# ==============================================================================
# Configuration
# ==============================================================================

# Data paths
DATA_DIR <- "ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)"
ADULTBIO_FILE <- file.path(DATA_DIR, "zimphia2020adultbio.csv")
ADULTIND_FILE <- file.path(DATA_DIR, "zimphia2020adultind.csv")

# Model files
STAN_MODEL_FILE <- "inst/models/loglogistic_interval.stan"
JAGS_MODEL_FILE <- "inst/models/loglogistic_interval.jags"

# Output directories
OUTPUT_BASE <- "mcmc_outputs/zimphia"
HMC_OUTPUT_DIR <- file.path(OUTPUT_BASE, "hmc")
MH_OUTPUT_DIR <- file.path(OUTPUT_BASE, "mh")

# MCMC settings (matching simulation study)
MCMC_SETTINGS <- list(
  hmc = list(
    n_chains = 4,
    n_warmup = 1000,
    n_sampling = 5000,
    parallel_chains = 4,
    seed = 2025,
    refresh = 500
  ),
  mh = list(
    n_chains = 4,
    n_adapt = 1000,
    n_burnin = 1000,
    n_iter = 5000,
    seed = 2025
  )
)

# ==============================================================================
# 1. Data Loading and Merging
# ==============================================================================

cat("Step 1: Loading ZIMPHIA data files...\n")

# Check file existence
if (!file.exists(ADULTBIO_FILE)) {
  stop("Adult biomarker file not found: ", ADULTBIO_FILE)
}
if (!file.exists(ADULTIND_FILE)) {
  stop("Adult individual file not found: ", ADULTIND_FILE)
}

# Load datasets
cat("  Loading adult biomarker data...\n")
adultbio <- read_csv(
  ADULTBIO_FILE,
  show_col_types = FALSE,
  col_select = c(personid, hivstatusfinal, btwt0, bt_status, age, gender)
)

cat("  Loading adult individual data...\n")
adultind <- read_csv(
  ADULTIND_FILE,
  show_col_types = FALSE,
  col_select = c(personid, firstsxage, sexever, hivtfposy, gender)
)

# Merge datasets
cat("  Merging datasets on personid...\n")
zimphia <- adultbio %>%
  left_join(
    adultind %>% select(personid, firstsxage, sexever, hivtfposy),
    by = "personid"
  )

cat(sprintf("  Merged dataset: %d observations\n", nrow(zimphia)))

# ==============================================================================
# 2. Data Filtering and Cleaning
# ==============================================================================

cat("\nStep 2: Applying inclusion/exclusion criteria...\n")

# Track sample sizes through filtering
n_start <- nrow(zimphia)

# Filter 1: Valid biomarker result
zimphia <- zimphia %>% filter(bt_status == 1)
cat(sprintf(
  "  After bt_status == 1: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))

# Filter 2: Age >= 15
zimphia <- zimphia %>% filter(age >= 15)
cat(sprintf(
  "  After age >= 15: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))

# Filter 3: Ever had sex
zimphia <- zimphia %>% filter(sexever == 1)
cat(sprintf(
  "  After sexever == 1: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))

# Filter 4: Non-missing firstsxage
zimphia <- zimphia %>% filter(!is.na(firstsxage))
cat(sprintf(
  "  After non-missing firstsxage: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))

# Filter 5: Non-missing gender
zimphia <- zimphia %>% filter(!is.na(gender))
cat(sprintf(
  "  After non-missing gender: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))

# Filter 6: HIV+ individuals must have non-missing hivtfposy
n_before <- nrow(zimphia)
n_hivpos_before <- sum(zimphia$hivstatusfinal == 1, na.rm = TRUE)
zimphia <- zimphia %>%
  filter(!(hivstatusfinal == 1 & is.na(hivtfposy)))
n_hivpos_after <- sum(zimphia$hivstatusfinal == 1, na.rm = TRUE)
cat(sprintf(
  "  After excluding HIV+ with missing hivtfposy: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))
cat(sprintf(
  "    HIV+ excluded: %d (had missing first positive test date)\n",
  n_hivpos_before - n_hivpos_after
))

# Filter 7: Non-missing weight
zimphia <- zimphia %>% filter(!is.na(btwt0) & btwt0 > 0)
cat(sprintf(
  "  After non-missing/positive weight: %d observations (%.1f%% retained)\n",
  nrow(zimphia),
  100 * nrow(zimphia) / n_start
))

cat(sprintf("\nFinal analytical sample: %d observations\n", nrow(zimphia)))
cat(sprintf(
  "  HIV+: %d (%.1f%%)\n",
  sum(zimphia$hivstatusfinal == 1, na.rm = TRUE),
  100 * mean(zimphia$hivstatusfinal == 1, na.rm = TRUE)
))
cat(sprintf(
  "  HIV-: %d (%.1f%%)\n",
  sum(zimphia$hivstatusfinal == 2, na.rm = TRUE),
  100 * mean(zimphia$hivstatusfinal == 2, na.rm = TRUE)
))

# ==============================================================================
# 3. Interval-Censored Variable Construction
# ==============================================================================

cat("\nStep 3: Constructing interval-censored survival data...\n")

# Approach A: Simple interval construction
# L_i = firstsxage (age at sexual debut)
# R_i = age (current age at survey)
# For HIV-: R_i = Inf (right-censored)

zimphia <- zimphia %>%
  mutate(
    L = firstsxage,
    R = case_when(
      hivstatusfinal == 2 ~ Inf, # HIV-negative (coded as 2): right-censored
      hivstatusfinal == 1 ~ age, # HIV-positive (coded as 1): interval (firstsxage, age)
      TRUE ~ NA_real_
    )
  )

# Data quality checks
cat("  Interval construction checks:\n")
cat(sprintf("    Min L: %.1f years\n", min(zimphia$L, na.rm = TRUE)))
cat(sprintf("    Max L: %.1f years\n", max(zimphia$L, na.rm = TRUE)))
cat(sprintf(
  "    Min R (HIV+): %.1f years\n",
  min(zimphia$R[zimphia$hivstatusfinal == 1], na.rm = TRUE)
))
cat(sprintf(
  "    Max R (HIV+): %.1f years\n",
  max(zimphia$R[zimphia$hivstatusfinal == 1], na.rm = TRUE)
))
cat(sprintf("    Right-censored (HIV-): %d\n", sum(is.infinite(zimphia$R))))

# Apply floor to L to avoid zero (as in simulation: 1e-10)
zimphia <- zimphia %>%
  mutate(L = pmax(L, 1e-10))

cat("  Applied floor to L (max(L, 1e-10)) to avoid zero\n")

# Ensure finite intervals have positive width for Stan log_diff_exp
interval_min_width <- 1e-6
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
  cat(sprintf(
    "  Adjusted %d intervals with non-positive width (added %.0e to R)\n",
    n_nonpos_width,
    interval_min_width
  ))
}

# ==============================================================================
# 4. Covariate Preparation and Weight Normalization
# ==============================================================================

cat("\nStep 4: Preparing covariates and normalizing weights...\n")

# Convert gender to binary (0/1) matching simulation
# ZIMPHIA: 1=Male, 2=Female
# Model: 0=Male, 1=Female (as in simulation with 55% female)
zimphia <- zimphia %>%
  mutate(
    X1 = case_when(
      gender == 1 ~ 0, # Male
      gender == 2 ~ 1, # Female
      TRUE ~ NA_real_
    )
  )

cat(sprintf("  Gender distribution:\n"))
cat(sprintf(
  "    Male (X1=0): %d (%.1f%%)\n",
  sum(zimphia$X1 == 0, na.rm = TRUE),
  100 * mean(zimphia$X1 == 0, na.rm = TRUE)
))
cat(sprintf(
  "    Female (X1=1): %d (%.1f%%)\n",
  sum(zimphia$X1 == 1, na.rm = TRUE),
  100 * mean(zimphia$X1 == 1, na.rm = TRUE)
))

# Normalize weights to sum to N (as in simulation)
N <- nrow(zimphia)
sum_weights <- sum(zimphia$btwt0)
zimphia <- zimphia %>%
  mutate(weight = btwt0 * N / sum_weights)

cat(sprintf("\n  Weight normalization:\n"))
cat(sprintf("    Original sum(btwt0): %.1f\n", sum_weights))
cat(sprintf(
  "    Normalized sum(weight): %.1f (should equal N=%d)\n",
  sum(zimphia$weight),
  N
))
cat(sprintf("    Min weight: %.4f\n", min(zimphia$weight)))
cat(sprintf("    Max weight: %.4f\n", max(zimphia$weight)))
cat(sprintf("    Mean weight: %.4f\n", mean(zimphia$weight)))

# ==============================================================================
# 5. Prepare Data for MCMC
# ==============================================================================

cat("\nStep 5: Preparing data for MCMC fitting...\n")

# Extract final analysis dataset
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

cat(sprintf(
  "  Analysis dataset prepared: %d observations, %d variables\n",
  nrow(analysis_data),
  ncol(analysis_data)
))

# Save prepared data for reference
prepared_data_file <- file.path(OUTPUT_BASE, "zimphia_prepared_data.rds")
dir.create(OUTPUT_BASE, recursive = TRUE, showWarnings = FALSE)
saveRDS(analysis_data, prepared_data_file)
cat(sprintf("  Saved prepared data to: %s\n", prepared_data_file))

# Create data summary
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

summary_file <- file.path(OUTPUT_BASE, "zimphia_data_summary.rds")
saveRDS(data_summary, summary_file)
write_csv(data_summary, file.path(OUTPUT_BASE, "zimphia_data_summary.csv"))
cat(sprintf("  Saved data summary to: %s\n", summary_file))

# ==============================================================================
# 6. HMC Fitting (Stan/CmdStanR)
# ==============================================================================

cat("\n=== Starting HMC (Stan) Fitting ===\n")

# Create output directories
for (subdir in c("summaries", "fits", "draws", "diagnostics")) {
  dir.create(
    file.path(HMC_OUTPUT_DIR, subdir),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# Prepare Stan data
stan_data <- list(
  N = nrow(analysis_data),
  L = analysis_data$L,
  R = analysis_data$R,
  X = analysis_data$X1, # Vector for single covariate
  w = analysis_data$weight
)

cat(sprintf("  Stan data prepared: N=%d\n", stan_data$N))

# Compile Stan model
cat(sprintf("  Compiling Stan model: %s\n", STAN_MODEL_FILE))
if (!file.exists(STAN_MODEL_FILE)) {
  stop("Stan model file not found: ", STAN_MODEL_FILE)
}

stan_model <- cmdstan_model(STAN_MODEL_FILE)

# Run HMC
cat("  Running HMC sampling...\n")
cat(sprintf("    Chains: %d\n", MCMC_SETTINGS$hmc$n_chains))
cat(sprintf("    Warmup: %d iterations\n", MCMC_SETTINGS$hmc$n_warmup))
cat(sprintf("    Sampling: %d iterations\n", MCMC_SETTINGS$hmc$n_sampling))
cat(sprintf("    Seed: %d\n", MCMC_SETTINGS$hmc$seed))

hmc_start_time <- Sys.time()

hmc_fit <- stan_model$sample(
  data = stan_data,
  chains = MCMC_SETTINGS$hmc$n_chains,
  parallel_chains = MCMC_SETTINGS$hmc$parallel_chains,
  iter_warmup = MCMC_SETTINGS$hmc$n_warmup,
  iter_sampling = MCMC_SETTINGS$hmc$n_sampling,
  seed = MCMC_SETTINGS$hmc$seed,
  refresh = MCMC_SETTINGS$hmc$refresh,
  show_messages = FALSE
)

hmc_end_time <- Sys.time()
hmc_runtime <- as.numeric(difftime(
  hmc_end_time,
  hmc_start_time,
  units = "secs"
))

cat(sprintf(
  "  HMC sampling completed in %.1f seconds (%.1f minutes)\n",
  hmc_runtime,
  hmc_runtime / 60
))

# Extract and save HMC results
cat("  Extracting HMC results...\n")

# Summary
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
hmc_summary_file <- file.path(
  HMC_OUTPUT_DIR,
  "summaries",
  "zimphia_hmc_summary.rds"
)
saveRDS(hmc_summary, hmc_summary_file)
write_csv(
  hmc_summary,
  file.path(HMC_OUTPUT_DIR, "summaries", "zimphia_hmc_summary.csv")
)

# Posterior draws
hmc_draws <- hmc_fit$draws(
  variables = c("alpha", "beta", "gamma"),
  format = "df"
)
hmc_draws_file <- file.path(HMC_OUTPUT_DIR, "draws", "zimphia_hmc_draws.rds")
saveRDS(hmc_draws, hmc_draws_file)

# Fit object
hmc_fit_file <- file.path(HMC_OUTPUT_DIR, "fits", "zimphia_hmc_fit.rds")
hmc_fit$save_object(hmc_fit_file)

# Diagnostics
hmc_diagnostics <- tibble(
  method = "hmc",
  dataset = "zimphia",
  n_obs = stan_data$N,
  max_rhat = max(hmc_summary$rhat, na.rm = TRUE),
  min_ess_bulk = min(hmc_summary$ess_bulk, na.rm = TRUE),
  min_ess_tail = min(hmc_summary$ess_tail, na.rm = TRUE),
  n_divergences = sum(hmc_fit$sampler_diagnostics(format = "df")$divergent__),
  runtime_secs = hmc_runtime,
  n_chains = MCMC_SETTINGS$hmc$n_chains,
  n_warmup = MCMC_SETTINGS$hmc$n_warmup,
  n_sampling = MCMC_SETTINGS$hmc$n_sampling
)

hmc_diag_file <- file.path(
  HMC_OUTPUT_DIR,
  "diagnostics",
  "zimphia_hmc_diagnostics.rds"
)
saveRDS(hmc_diagnostics, hmc_diag_file)
write_csv(
  hmc_diagnostics,
  file.path(HMC_OUTPUT_DIR, "diagnostics", "zimphia_hmc_diagnostics.csv")
)

cat("\n  HMC Results:\n")
print(hmc_summary)
cat("\n  HMC Diagnostics:\n")
print(hmc_diagnostics)

# ==============================================================================
# 7. MH Fitting (JAGS)
# ==============================================================================

cat("\n=== Starting MH (JAGS) Fitting ===\n")

# Create output directories
for (subdir in c("summaries", "fits", "draws", "diagnostics")) {
  dir.create(
    file.path(MH_OUTPUT_DIR, subdir),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# Prepare JAGS data
jags_data <- list(
  N = nrow(analysis_data),
  L = analysis_data$L,
  R = ifelse(is.infinite(analysis_data$R), 1e11, analysis_data$R), # Replace Inf with sentinel
  X = analysis_data$X1,
  w = analysis_data$weight,
  zeros = rep(0, nrow(analysis_data))
)

cat(sprintf("  JAGS data prepared: N=%d\n", jags_data$N))

# Load JAGS model
cat(sprintf("  Loading JAGS model: %s\n", JAGS_MODEL_FILE))
if (!file.exists(JAGS_MODEL_FILE)) {
  stop("JAGS model file not found: ", JAGS_MODEL_FILE)
}

jags_model_string <- readLines(JAGS_MODEL_FILE)

# Initialize JAGS model
cat("  Initializing JAGS model...\n")
jags_model <- jags.model(
  file = textConnection(jags_model_string),
  data = jags_data,
  n.chains = MCMC_SETTINGS$mh$n_chains,
  n.adapt = MCMC_SETTINGS$mh$n_adapt,
  quiet = FALSE
)

# Burn-in
cat(sprintf(
  "  Running burn-in: %d iterations per chain...\n",
  MCMC_SETTINGS$mh$n_burnin
))
update(jags_model, n.iter = MCMC_SETTINGS$mh$n_burnin, progress.bar = "text")

# Sampling
cat(sprintf(
  "  Running MCMC sampling: %d iterations per chain...\n",
  MCMC_SETTINGS$mh$n_iter
))
mh_start_time <- Sys.time()

mh_samples <- coda.samples(
  model = jags_model,
  variable.names = c("alpha", "beta", "gamma"),
  n.iter = MCMC_SETTINGS$mh$n_iter,
  progress.bar = "text"
)

mh_end_time <- Sys.time()
mh_runtime <- as.numeric(difftime(mh_end_time, mh_start_time, units = "secs"))

cat(sprintf(
  "  MH sampling completed in %.1f seconds (%.1f minutes)\n",
  mh_runtime,
  mh_runtime / 60
))

# Extract and save MH results
cat("  Extracting MH results...\n")

# Summary
mh_summary_stats <- summary(mh_samples)
mh_summary <- tibble(
  variable = rownames(mh_summary_stats$statistics),
  mean = mh_summary_stats$statistics[, "Mean"],
  sd = mh_summary_stats$statistics[, "SD"],
  q2.5 = mh_summary_stats$quantiles[, "2.5%"],
  median = mh_summary_stats$quantiles[, "50%"],
  q97.5 = mh_summary_stats$quantiles[, "97.5%"]
)

# Add Rhat and ESS from gelman.diag and effectiveSize
gelman_diag <- gelman.diag(mh_samples, multivariate = FALSE)
ess <- effectiveSize(mh_samples)

mh_summary <- mh_summary %>%
  mutate(
    rhat = gelman_diag$psrf[variable, "Point est."],
    ess = ess[variable]
  )

mh_summary_file <- file.path(
  MH_OUTPUT_DIR,
  "summaries",
  "zimphia_mh_summary.rds"
)
saveRDS(mh_summary, mh_summary_file)
write_csv(
  mh_summary,
  file.path(MH_OUTPUT_DIR, "summaries", "zimphia_mh_summary.csv")
)

# Posterior draws
mh_draws <- as.data.frame(as.matrix(mh_samples))
mh_draws <- mh_draws %>%
  mutate(
    .chain = rep(1:MCMC_SETTINGS$mh$n_chains, each = MCMC_SETTINGS$mh$n_iter),
    .iteration = rep(
      1:MCMC_SETTINGS$mh$n_iter,
      times = MCMC_SETTINGS$mh$n_chains
    ),
    .draw = 1:n()
  )

mh_draws_file <- file.path(MH_OUTPUT_DIR, "draws", "zimphia_mh_draws.rds")
saveRDS(mh_draws, mh_draws_file)

# Fit object (mcmc.list)
mh_fit_file <- file.path(MH_OUTPUT_DIR, "fits", "zimphia_mh_fit.rds")
saveRDS(mh_samples, mh_fit_file)

# Diagnostics
mh_diagnostics <- tibble(
  method = "mh",
  dataset = "zimphia",
  n_obs = jags_data$N,
  max_rhat = max(mh_summary$rhat, na.rm = TRUE),
  min_ess = min(mh_summary$ess, na.rm = TRUE),
  runtime_secs = mh_runtime,
  n_chains = MCMC_SETTINGS$mh$n_chains,
  n_adapt = MCMC_SETTINGS$mh$n_adapt,
  n_burnin = MCMC_SETTINGS$mh$n_burnin,
  n_iter = MCMC_SETTINGS$mh$n_iter
)

mh_diag_file <- file.path(
  MH_OUTPUT_DIR,
  "diagnostics",
  "zimphia_mh_diagnostics.rds"
)
saveRDS(mh_diagnostics, mh_diag_file)
write_csv(
  mh_diagnostics,
  file.path(MH_OUTPUT_DIR, "diagnostics", "zimphia_mh_diagnostics.csv")
)

cat("\n  MH Results:\n")
print(mh_summary)
cat("\n  MH Diagnostics:\n")
print(mh_diagnostics)

# ==============================================================================
# 8. Comparison Summary
# ==============================================================================

cat("\n=== Method Comparison Summary ===\n")

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

print(comparison)

comparison_file <- file.path(OUTPUT_BASE, "zimphia_method_comparison.csv")
write_csv(comparison, comparison_file)
cat(sprintf("\nSaved method comparison to: %s\n", comparison_file))

# Runtime comparison
runtime_comparison <- tibble(
  method = c("HMC", "MH"),
  runtime_secs = c(hmc_runtime, mh_runtime),
  runtime_mins = runtime_secs / 60,
  n_chains = c(MCMC_SETTINGS$hmc$n_chains, MCMC_SETTINGS$mh$n_chains),
  total_iterations = c(
    MCMC_SETTINGS$hmc$n_chains *
      (MCMC_SETTINGS$hmc$n_warmup + MCMC_SETTINGS$hmc$n_sampling),
    MCMC_SETTINGS$mh$n_chains *
      (MCMC_SETTINGS$mh$n_adapt +
        MCMC_SETTINGS$mh$n_burnin +
        MCMC_SETTINGS$mh$n_iter)
  )
)

print(runtime_comparison)

runtime_file <- file.path(OUTPUT_BASE, "zimphia_runtime_comparison.csv")
write_csv(runtime_comparison, runtime_file)
cat(sprintf("Saved runtime comparison to: %s\n", runtime_file))

# ==============================================================================
# 9. Final Summary
# ==============================================================================

cat("\n=== Analysis Complete ===\n")
cat(sprintf("Sample size: %d observations\n", nrow(analysis_data)))
cat(sprintf(
  "HIV+: %d, HIV-: %d\n",
  sum(analysis_data$hivstatusfinal == 1),
  sum(analysis_data$hivstatusfinal == 2)
))
cat(sprintf("\nOutput directories:\n"))
cat(sprintf("  HMC: %s\n", HMC_OUTPUT_DIR))
cat(sprintf("  MH: %s\n", MH_OUTPUT_DIR))
cat(sprintf("\nKey output files:\n"))
cat(sprintf("  Prepared data: %s\n", prepared_data_file))
cat(sprintf("  HMC summary: %s\n", hmc_summary_file))
cat(sprintf("  MH summary: %s\n", mh_summary_file))
cat(sprintf("  Method comparison: %s\n", comparison_file))
cat(sprintf("  Runtime comparison: %s\n", runtime_file))

cat("\n=== ZIMPHIA Analysis Complete ===\n\n")
