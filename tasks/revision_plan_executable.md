# BMC Substantive Revision — Executable Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. This plan turns the strategic document `tasks/revision_plan_substantive.md` (the "spec") into bite-sized executable tasks.

**Goal:** Implement the eleven substantive revisions (A–K) the BMC reviewers requested, on a single 8-core laptop, in ~4 working days, such that the manuscript, response letter, and code all ship together with green verification.

**Architecture:** New analyses live in **new files** so the existing pipeline keeps working — every Task adds one R script in `inst/scripts/` plus (where needed) one library file in `R/`. The Stan model gains a **second, generalised** variant for the multivariable fit; the scalar-beta original stays byte-identical. Long-running simulations (Tasks F MH leg, G n=10,000 leg) run **unattended overnight** while writing happens during the day. Verification is `R CMD check`, `devtools::test()`, and a clean LaTeX rebuild — same gates as the cosmetic round.

**Tech Stack:** R 4.6, cmdstanr/CmdStan, rjags/JAGS 4.3.2, posterior, coda, dplyr/tidyr, testthat 3e, ggplot2, Quarto-free LaTeX. Reuse `fit_logistic_hmc()` and `run_zimphia_analysis()` rather than re-implementing fits.

---

## Decisions captured (locked, do not re-litigate)

1. **Misspecification sim:** one cell only — `n=2,000, censoring=0.3, weight CV≈0.5`, 200 HMC + 200 MH reps.
2. **HMC 1,000-rep rerun:** central cells only at `n=2,000` and `n=10,000` (one cell per `n` at censoring 0.3 / CV 0.5). MH stays at 200 everywhere.
3. **Compute:** laptop only — no cloud, no HPC.
4. **Response letter:** drafted alongside manuscript edits in the same round (`../../10_BMC_Submission/04_Cover_Letter/response_letter.md`).
5. **Weibull truth calibration (misspec):** `k_weibull = 2.0`, `lambda_weibull = 6.01` — matches log-logistic median 5 with a stricter-shape misspecification.
6. **Replicate weights:** use first 100 of the 175 columns `design_wt001…design_wt100` in the ZIMPHIA intermediate weights file. Skip MH for this (design-correction, not sampler comparison).
7. **Cohort cutpoints:** 1965–1979, 1980–1989, 1990+ (three cohorts), derived as `survey_year(2020) - age`.

---

## File map (what gets created vs modified)

### New library files (R/)

| Path | Responsibility |
|---|---|
| `R/jags_audit.R` | `audit_jags_samplers()` — load the JAGS model, return `list.samplers()` output as a tibble |
| `R/misspec_simulation.R` | `simulate_survival_data_weibull()` (Weibull DGM mirror of `simulate_survival_data()`), `run_misspec_simulation()` (drives one cell, HMC + MH, summarises) |
| `R/zimphia_multivariable.R` | `prepare_zimphia_multivariable_data()` + `fit_zimphia_multivariable()` — joins demographics, builds a `K`-column model matrix, refits via the new multivariable Stan model |
| `R/zimphia_cohort.R` | `derive_birth_cohort()` + `fit_zimphia_cohort()` — three stratified HMC fits, returns side-by-side posterior summaries |
| `R/zimphia_incidence.R` | `compute_age_specific_hazard()`, `compute_population_incidence()` — pure post-processing from saved draws |
| `R/zimphia_design_variance.R` | `load_replicate_weights()`, `fit_zimphia_design_replicates()` — joins the 100 replicate weights and loops `fit_logistic_hmc()`-style refits |

### New scripts (inst/scripts/) — one runner per task

| Path | Drives |
|---|---|
| `inst/scripts/00a_jags_sampler_audit.R` | Task A |
| `inst/scripts/04_zimphia_multivariable.R` | Task B |
| `inst/scripts/05_zimphia_cohort.R` | Task C |
| `inst/scripts/06_zimphia_incidence.R` | Task D |
| `inst/scripts/07_zimphia_design_variance.R` | Task E |
| `inst/scripts/08_misspec_sim.R` | Task F |
| `inst/scripts/09_hmc_1000_rerun.R` | Task G |

### New Stan models (inst/models/)

| Path | Notes |
|---|---|
| `inst/models/loglogistic_interval_multivariable.stan` | Generalised: vector `beta[K]`, matrix `X[N,K]`. Preserves scalar original byte-identical. |

### New test files (tests/testthat/)

> The codebase has **no `tests/` directory yet** — Task 0 sets it up with `usethis::use_testthat()`. Tests cover the small pure helpers; the long-running fit wrappers are verified by their downstream output (manuscript numbers + diagnostics), not by unit tests.

| Path | Covers |
|---|---|
| `tests/testthat/test-misspec-simulation.R` | `simulate_survival_data_weibull()` median/IQR moment checks |
| `tests/testthat/test-zimphia-incidence.R` | `compute_age_specific_hazard()` against closed-form log-logistic hazard at a known parameter set |
| `tests/testthat/test-zimphia-cohort.R` | `derive_birth_cohort()` factor levels & boundary inclusion |
| `tests/testthat/test-jags-audit.R` | `audit_jags_samplers()` returns one row per parameter for a tiny synthetic dataset |

### Files modified (not created)

| Path | Change |
|---|---|
| `R/simulation.R` | Add `k_weibull` / `lambda_weibull` to `get_default_params()`; add `n_replicates_hmc` and `samplers` args to `run_simulations()` |
| `inst/models/loglogistic_interval.stan` | Untouched (scalar-beta preserved) |
| `inst/models/loglogistic_interval.jags` | Untouched |
| `NAMESPACE` | Regenerated by `devtools::document()` after roxygen comments land |
| `DESCRIPTION` | Bump `Version` from 0.1.0 → 0.2.0 in Task 13 |
| `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` | Tasks B–J writeups |
| `../../10_BMC_Submission/04_Cover_Letter/response_letter.md` | Task K (new file) |

### Outputs (not committed, but the plan needs them)

| Path | Produced by |
|---|---|
| `mcmc_outputs/zimphia_multivariable/` | Task B |
| `mcmc_outputs/zimphia_cohort/{1965-1979,1980-1989,1990-plus}/` | Task C |
| `mcmc_outputs/zimphia_incidence/` | Task D |
| `mcmc_outputs/zimphia_design_replicates/rep001..rep100/` | Task E |
| `mcmc_outputs/misspec/n2000_c0.3_whigh/{hmc,mh}/` | Task F |
| `mcmc_outputs/hmc1000/n2000_c0.3_whigh/`, `mcmc_outputs/hmc1000/n10000_c0.3_whigh/` | Task G |
| `outputs/figures/fig8_incidence_curve.png` | Task D |
| `outputs/figures/fig9_design_variance_forest.png` | Task E |
| `outputs/figures/fig10_misspec_compare.png` | Task F |
| `outputs/tables/tab_design_variance.csv`, `tab_misspec_summary.csv`, `tab_cohort_compare.csv` | Tasks E, F, C |

---

## Task 0: Scaffold the test suite, output directories, and a working branch

**Files:**
- Create: `tests/testthat.R`, `tests/testthat/setup-paths.R`
- Modify: `DESCRIPTION` (add testthat to Suggests if not present)

- [ ] **Step 1: Create a working branch**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/04_Code"
git checkout -b substantive-revision
git status
```

Expected: `On branch substantive-revision`, clean tree.

- [ ] **Step 2: Add testthat scaffold via usethis**

In R:
```r
# Run inside the package root
usethis::use_testthat(edition = 3)
```

This creates `tests/testthat.R`, `tests/testthat/`, and adds `testthat (>= 3.0.0)` to DESCRIPTION Suggests + a `Config/testthat/edition: 3` line.

- [ ] **Step 3: Add a shared test helper for fixture paths**

Create `tests/testthat/setup-paths.R`:
```r
# Shared paths used across multiple test files
test_zimphia_dir <- function() {
  testthat::skip_if_not(
    dir.exists("../../ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)"),
    "ZIMPHIA microdata not present"
  )
  "../../ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)"
}

test_replicate_weights_csv <- function() {
  path <- file.path(
    "..", "..", "ZIMPHIA",
    "ZIMPHIA 2020 Intermediary Weights (CSV)",
    "zimphia2020indintermediarywts.csv"
  )
  testthat::skip_if_not(file.exists(path), "Replicate weights CSV not present")
  path
}
```

- [ ] **Step 4: Run the empty test suite to verify wiring**

In R:
```r
devtools::test()
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]` — zero tests, clean execution.

- [ ] **Step 5: Create stub output directories so later scripts don't crash on missing dirs**

```bash
mkdir -p mcmc_outputs/zimphia_multivariable \
         mcmc_outputs/zimphia_cohort \
         mcmc_outputs/zimphia_incidence \
         mcmc_outputs/zimphia_design_replicates \
         mcmc_outputs/misspec \
         mcmc_outputs/hmc1000 \
         outputs/figures \
         outputs/tables \
         logs/substantive_revision
```

Expected: all directories created, no errors.

- [ ] **Step 6: Commit the scaffold**

```bash
git add tests/ DESCRIPTION
git commit -m "test: scaffold testthat suite and output directories for substantive revision"
```

---

## Task A: JAGS sampler audit (revision plan §3 Task A)

**Why this is first:** It is free, takes 30 minutes, and the outcome determines whether the Appendix Algorithm 4 rewrite is in scope. If JAGS uses slice samplers (expected), the "basic MH" complaint flips to a strength.

**Files:**
- Create: `R/jags_audit.R`
- Create: `tests/testthat/test-jags-audit.R`
- Create: `inst/scripts/00a_jags_sampler_audit.R`
- Modify (only if audit reveals slice samplers): `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex:1395-1410` (Algorithm 4 / `\subsection{Metropolis--Hastings (JAGS)}`)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-jags-audit.R`:
```r
test_that("audit_jags_samplers returns one row per monitored parameter", {
  skip_if_not_installed("rjags")

  # Tiny synthetic dataset with positive L, mixed R
  set.seed(1)
  n <- 20
  dat <- list(
    N = n,
    L = pmax(rexp(n, 1), 1e-10),
    R = ifelse(runif(n) < 0.5, Inf, NA_real_),
    X = rbinom(n, 1, 0.5),
    w = rep(1, n),
    zeros = rep(0, n)
  )
  dat$R <- ifelse(is.infinite(dat$R), 1e11, dat$L + 1 + rexp(n, 1))

  model_file <- "../../inst/models/loglogistic_interval.jags"
  skip_if_not(file.exists(model_file), "JAGS model not present")

  res <- audit_jags_samplers(dat, model_file)

  expect_s3_class(res, "tbl_df")
  expect_setequal(c("node", "sampler"), names(res))
  expect_true(all(c("alpha", "beta", "gamma") %in% res$node))
})
```

- [ ] **Step 2: Run the test to confirm it fails**

```r
devtools::test(filter = "jags-audit")
```

Expected: FAIL with `could not find function "audit_jags_samplers"`.

- [ ] **Step 3: Implement `audit_jags_samplers()`**

Create `R/jags_audit.R`:
```r
#' Audit which JAGS samplers are assigned to each node
#'
#' Loads the JAGS model on a tiny dataset and returns the per-node sampler
#' assignment reported by [rjags::list.samplers()]. Used to confirm whether
#' the project's MH baseline is actually slice-sampling rather than
#' adaptive Metropolis (see revision plan Task A).
#'
#' @param data A list suitable for [rjags::jags.model()] (N, L, R, X, w, zeros).
#' @param model_file Path to the JAGS model file.
#' @param n_chains Number of chains to initialise. Default 1.
#' @param n_adapt Number of adaptation iterations. Default 100.
#'
#' @return A tibble with columns `node` and `sampler`.
#' @export
audit_jags_samplers <- function(data,
                                model_file = system.file(
                                  "models", "loglogistic_interval.jags",
                                  package = "bayesianICSimulations"
                                ),
                                n_chains = 1L,
                                n_adapt = 100L) {
  stopifnot(file.exists(model_file))
  jags_model <- rjags::jags.model(
    file = model_file,
    data = data,
    n.chains = n_chains,
    n.adapt = n_adapt,
    quiet = TRUE
  )
  samplers <- rjags::list.samplers(jags_model)

  tibble::tibble(
    node = unlist(samplers, use.names = FALSE),
    sampler = rep(names(samplers), lengths(samplers))
  )
}
```

- [ ] **Step 4: Document and rebuild NAMESPACE**

```r
devtools::document()
```

Expected: `audit_jags_samplers` exported in NAMESPACE; no warnings.

- [ ] **Step 5: Run the test to confirm it passes**

```r
devtools::test(filter = "jags-audit")
```

Expected: PASS (1 test).

- [ ] **Step 6: Run the audit on the real model and capture the result**

Create `inst/scripts/00a_jags_sampler_audit.R`:
```r
#!/usr/bin/env Rscript
# Task A: JAGS sampler audit
# Output: logs/substantive_revision/jags_audit.txt

devtools::load_all()

set.seed(2025)
n <- 200
dat <- list(
  N = n,
  L = pmax(rexp(n, 1), 1e-10),
  R = ifelse(runif(n) < 0.3, 1e11, rexp(n, 0.2) + 1),
  X = rbinom(n, 1, 0.5),
  w = rep(1, n),
  zeros = rep(0, n)
)

audit <- audit_jags_samplers(
  data = dat,
  model_file = "inst/models/loglogistic_interval.jags",
  n_chains = 1L,
  n_adapt = 200L
)

dir.create("logs/substantive_revision", showWarnings = FALSE, recursive = TRUE)
out_path <- "logs/substantive_revision/jags_audit.txt"
cat("JAGS sampler audit\n",
    "Date: ", as.character(Sys.time()), "\n",
    "Model: inst/models/loglogistic_interval.jags\n\n",
    sep = "", file = out_path)
utils::capture.output(print(audit, n = 100), file = out_path, append = TRUE)
print(audit)
```

Run:
```bash
Rscript inst/scripts/00a_jags_sampler_audit.R
```

Expected: prints a tibble. Likely outcome for `alpha`/`gamma` (constrained positive) is **slice sampler**; for `beta` (unconstrained) is **slice sampler** (JAGS default for continuous nodes with custom likelihood).

- [ ] **Step 7: Inspect the audit output and decide whether to rewrite Algorithm 4**

```bash
cat logs/substantive_revision/jags_audit.txt
```

**Decision rule:**
- If **all three** parameters show `bugs::Slice` or similar slice sampler → **rewrite required**. Proceed to Step 8.
- If **any** parameter shows a Metropolis-style sampler → record the actual sampler list; **no rewrite required**, but log this in the implementation log for Task K's R1 rebuttal.

- [ ] **Step 8 (conditional): Rewrite Appendix Algorithm 4 / §App MH**

Open `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` and replace the contents of `\subsection{Metropolis--Hastings (JAGS)}` (around line 1395) so it accurately describes the actual sampler. Replace:

```latex
\begin{itemize}
  \item Parameters and priors: \(\beta_{\text{sex}}\) on \(\mathbb{R}\); \(\alpha,\gamma>0\) with lognormal priors via \texttt{dlnorm} (no explicit Jacobian terms).
  \item Proposal: Gaussian random-walk, component-wise or small block updates, using JAGS defaults for constrained nodes.
```

with (assuming slice samplers, the most likely outcome):
```latex
\begin{itemize}
  \item Parameters and priors: \(\beta_{\text{sex}}\) on \(\mathbb{R}\); \(\alpha,\gamma>0\) with lognormal priors via \texttt{dlnorm} (no explicit Jacobian terms).
  \item Sampler: JAGS assigns a univariate slice sampler \citep{neal_slice_2003} to \(\alpha\), \(\beta\), and \(\gamma\) — the default for continuous unconstrained nodes whose likelihood is supplied via the zeros trick (\texttt{zeros[i] \(\sim\) dpois(phi[i])}). Per-iteration cost is two log-density evaluations per parameter plus rejection-stepping; no proposal scale is tuned.
```

Also edit `\caption{Metropolis--Hastings with Adaptive Covariance}` (line 1358) → `\caption{Univariate Slice Sampling (JAGS default)}` and update the body of `\begin{algorithm}` accordingly.

- [ ] **Step 9: Commit**

```bash
git add R/jags_audit.R inst/scripts/00a_jags_sampler_audit.R tests/testthat/test-jags-audit.R NAMESPACE man/audit_jags_samplers.Rd logs/substantive_revision/
git commit -m "feat: add JAGS sampler audit utility (revision Task A)"

# Conditional second commit if Step 8 ran
git add ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "docs: rewrite Algorithm 4 to reflect actual JAGS slice samplers"
```

---

## Task B: Multivariable ZIMPHIA fit (revision plan §3 Task B)

**Files:**
- Create: `inst/models/loglogistic_interval_multivariable.stan`
- Create: `R/zimphia_multivariable.R`
- Create: `tests/testthat/test-zimphia-multivariable.R`
- Create: `inst/scripts/04_zimphia_multivariable.R`
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (insert §4.x "Multivariable sensitivity")

- [ ] **Step 1: Write the generalised Stan model (no test — Stan code is verified by running it)**

Create `inst/models/loglogistic_interval_multivariable.stan` by copying `inst/models/loglogistic_interval.stan` and generalising:
```stan
// loglogistic_interval_multivariable.stan
// Interval-censored Log-Logistic AFT with vector covariate
data {
  int<lower=1>       N;
  int<lower=1>       K;
  vector<lower=0>[N] L;
  vector[N]          R;
  matrix[N, K]       X;
  vector<lower=0>[N] w;
}
transformed data {
  vector[N] wN;
  real sw = sum(w);
  if (sw > 0) wN = w * (N / sw); else wN = rep_vector(1.0, N);
}
parameters {
  real<lower=0> alpha;
  vector[K]     beta;
  real<lower=0> gamma;
}
transformed parameters {
  real log_alpha = log(alpha);
}
model {
  alpha ~ lognormal(log(5), 1);
  beta  ~ normal(0, 1);          // independent N(0,1) on each coefficient
  gamma ~ lognormal(0, 0.5);

  for (i in 1:N) {
    real log_lambda_i = log_alpha + dot_product(X[i], beta);
    if (L[i] > 0) {
      real log_L_lambda_ratio = log(L[i]) - log_lambda_i;
      real logSL = -log1p(exp(gamma * log_L_lambda_ratio));
      if (is_inf(R[i])) {
        target += wN[i] * logSL;
      } else {
        real log_R_lambda_ratio = log(R[i]) - log_lambda_i;
        real logSR = -log1p(exp(gamma * log_R_lambda_ratio));
        target += wN[i] * log_diff_exp(logSL, logSR);
      }
    } else if (!is_inf(R[i])) {
      real log_R_lambda_ratio = log(R[i]) - log_lambda_i;
      real logSR = -log1p(exp(gamma * log_R_lambda_ratio));
      target += wN[i] * log1m_exp(logSR);
    }
  }
}
```

Compile it once locally to verify it parses:
```r
cmdstanr::cmdstan_model("inst/models/loglogistic_interval_multivariable.stan")
```
Expected: no parser errors.

- [ ] **Step 2: Write the failing test for `prepare_zimphia_multivariable_data()`**

Create `tests/testthat/test-zimphia-multivariable.R`:
```r
test_that("prepare_zimphia_multivariable_data builds K-column matrix with correct names", {
  fake_indiv <- tibble::tibble(
    personid = c("p1", "p2", "p3", "p4"),
    age = c(20, 30, 40, 25),
    gender = c(1, 2, 1, 2),  # 1=male, 2=female (ZIMPHIA coding)
    urban = c(1, 2, 1, 2)    # 1=urban, 2=rural
  )
  base <- tibble::tibble(
    personid = c("p1", "p2", "p3", "p4"),
    L = c(0.1, 5, 10, 8),
    R = c(20, Inf, 40, 25),
    X1 = c(0, 1, 0, 1),
    weight = rep(1, 4)
  )

  res <- prepare_zimphia_multivariable_data(
    base, fake_indiv,
    covariates = c("sex", "urban_rural", "age_band")
  )

  expect_named(res, c("data", "X", "covariate_levels"))
  expect_equal(ncol(res$X), 4)  # sex + urban_rural + 2 age dummies
  expect_equal(nrow(res$X), 4)
  expect_true(all(res$X[, "sex"] %in% c(0, 1)))
})
```

- [ ] **Step 3: Run the test to confirm it fails**

```r
devtools::test(filter = "zimphia-multivariable")
```

Expected: FAIL with `could not find function "prepare_zimphia_multivariable_data"`.

- [ ] **Step 4: Implement `prepare_zimphia_multivariable_data()` and `fit_zimphia_multivariable()`**

Create `R/zimphia_multivariable.R`:
```r
#' Prepare a multivariable design matrix for the ZIMPHIA fit
#'
#' Joins demographic columns from the individual file onto the analysis tibble
#' produced by `run_zimphia_analysis()` and returns a model matrix `X` of width
#' `K` together with the augmented data frame.
#'
#' @param base A data frame with at least `personid`, `L`, `R`, `X1`, `weight`.
#'   This is the analysis tibble produced by `run_zimphia_analysis()`.
#' @param indiv A data frame with `personid` and the demographic columns named
#'   by `covariates`. ZIMPHIA codings: `gender` (1=male, 2=female), `urban`
#'   (1=urban, 2=rural), `age` (years).
#' @param covariates Character vector of covariate names to include. Recognised:
#'   `"sex"`, `"urban_rural"`, `"age_band"`, `"wealth_quintile"`.
#'
#' @return A list with `data` (augmented tibble), `X` (numeric matrix N x K),
#'   and `covariate_levels` (factor levels for reference encoding).
#' @export
prepare_zimphia_multivariable_data <- function(base, indiv, covariates) {
  recognised <- c("sex", "urban_rural", "age_band", "wealth_quintile")
  unknown <- setdiff(covariates, recognised)
  if (length(unknown) > 0) {
    stop("Unrecognised covariates: ", paste(unknown, collapse = ", "))
  }

  df <- dplyr::left_join(base, indiv, by = "personid")
  cols <- list()
  levels_out <- list()

  if ("sex" %in% covariates) {
    cols$sex <- as.numeric(df$gender == 2)
  }
  if ("urban_rural" %in% covariates) {
    cols$urban_rural <- as.numeric(df$urban == 2)  # 1 = rural
  }
  if ("age_band" %in% covariates) {
    band <- cut(df$age, breaks = c(14, 24, 34, 49, 64),
                labels = c("15-24", "25-34", "35-49", "50-64"),
                right = TRUE)
    levels_out$age_band <- levels(band)
    mat <- stats::model.matrix(~ band)[, -1, drop = FALSE]
    colnames(mat) <- paste0("age_", levels(band)[-1])
    cols <- c(cols, as.list(as.data.frame(mat)))
  }
  if ("wealth_quintile" %in% covariates) {
    wq <- factor(df$wealthquintile)
    levels_out$wealth_quintile <- levels(wq)
    mat <- stats::model.matrix(~ wq)[, -1, drop = FALSE]
    colnames(mat) <- paste0("wq_", levels(wq)[-1])
    cols <- c(cols, as.list(as.data.frame(mat)))
  }

  X <- do.call(cbind, cols)
  storage.mode(X) <- "numeric"
  list(data = df, X = X, covariate_levels = levels_out)
}

#' Fit the multivariable ZIMPHIA HMC model
#'
#' Wraps the generalised Stan model and saves the same summary/diagnostics/draws
#' layout as `run_zimphia_analysis()`.
#'
#' @param analysis_data Output of `prepare_zimphia_multivariable_data()`.
#' @param output_dir Where to write `summaries/`, `draws/`, `diagnostics/`.
#' @param stan_model_file Path to the multivariable Stan model.
#' @param hmc_settings Named list (defaults match `run_zimphia_analysis()`).
#' @return A list with `summary`, `draws`, `diagnostics`, `runtime_secs`.
#' @export
fit_zimphia_multivariable <- function(
    analysis_data,
    output_dir = "mcmc_outputs/zimphia_multivariable",
    stan_model_file = "inst/models/loglogistic_interval_multivariable.stan",
    hmc_settings = NULL) {
  default_hmc <- list(n_chains = 4, n_warmup = 1000, n_sampling = 5000,
                      parallel_chains = 4, seed = 2025, refresh = 500)
  hmc_settings <- if (is.null(hmc_settings)) default_hmc
                  else utils::modifyList(default_hmc, hmc_settings)

  dir.create(file.path(output_dir, "summaries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "draws"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "diagnostics"), recursive = TRUE, showWarnings = FALSE)

  stan_data <- list(
    N = nrow(analysis_data$data),
    K = ncol(analysis_data$X),
    L = pmax(analysis_data$data$L, 1e-10),
    R = analysis_data$data$R,
    X = analysis_data$X,
    w = analysis_data$data$weight
  )

  mod <- cmdstanr::cmdstan_model(stan_model_file)
  t0 <- Sys.time()
  fit <- mod$sample(
    data = stan_data,
    chains = hmc_settings$n_chains,
    parallel_chains = hmc_settings$parallel_chains,
    iter_warmup = hmc_settings$n_warmup,
    iter_sampling = hmc_settings$n_sampling,
    seed = hmc_settings$seed,
    refresh = hmc_settings$refresh
  )
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  summ <- fit$summary(
    variables = c("alpha", "beta", "gamma"),
    "mean", "median", "sd",
    ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
    "rhat", "ess_bulk", "ess_tail"
  )
  summ$covariate <- c("alpha", colnames(analysis_data$X), "gamma")

  saveRDS(summ, file.path(output_dir, "summaries", "summary.rds"))
  readr::write_csv(summ, file.path(output_dir, "summaries", "summary.csv"))

  draws_df <- fit$draws(variables = c("alpha", "beta", "gamma"), format = "df")
  saveRDS(draws_df, file.path(output_dir, "draws", "draws.rds"))

  diag <- tibble::tibble(
    max_rhat = max(summ$rhat, na.rm = TRUE),
    min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
    n_divergences = sum(fit$sampler_diagnostics(format = "df")$divergent__),
    runtime_secs = runtime,
    K = ncol(analysis_data$X)
  )
  saveRDS(diag, file.path(output_dir, "diagnostics", "diag.rds"))

  list(summary = summ, draws = draws_df, diagnostics = diag, runtime_secs = runtime)
}
```

- [ ] **Step 5: Document and run the test**

```r
devtools::document()
devtools::test(filter = "zimphia-multivariable")
```

Expected: PASS (1 test).

- [ ] **Step 6: Write the runner script for the real fit**

Create `inst/scripts/04_zimphia_multivariable.R`:
```r
#!/usr/bin/env Rscript
# Task B: Multivariable ZIMPHIA fit (sex + urban_rural + age_band)
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr); library(readr)
})

base <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")

indiv <- read_csv(
  "ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)/zimphia2020adultind.csv",
  col_select = c(personid, age, gender, urban),
  show_col_types = FALSE
)

prep <- prepare_zimphia_multivariable_data(
  base = base,
  indiv = indiv,
  covariates = c("sex", "urban_rural", "age_band")
)

cat(sprintf("Design matrix: %d rows, %d columns\n",
            nrow(prep$X), ncol(prep$X)))
print(head(prep$X))

res <- fit_zimphia_multivariable(
  prep,
  output_dir = "mcmc_outputs/zimphia_multivariable"
)

cat("\nMultivariable summary:\n")
print(res$summary)
cat(sprintf("\nRuntime: %.1f seconds\n", res$runtime_secs))
```

Run it:
```bash
Rscript inst/scripts/04_zimphia_multivariable.R 2>&1 | tee logs/substantive_revision/multivariable_fit.log
```

Expected runtime: ~3–5 minutes. Expected outputs in `mcmc_outputs/zimphia_multivariable/`.

- [ ] **Step 7: Verify the fit converged**

In R:
```r
diag <- readRDS("mcmc_outputs/zimphia_multivariable/diagnostics/diag.rds")
stopifnot(diag$max_rhat <= 1.01)
stopifnot(diag$min_ess_bulk >= 400)
stopifnot(diag$n_divergences == 0)
cat("Multivariable fit converged.\n")
```

If any check fails, **stop and inspect** — do not push through. Common cause: a near-singular design matrix from collinear covariates. Drop `age_band`, retry with `sex + urban_rural + wealth_quintile`.

- [ ] **Step 8: Build the comparison table**

In R:
```r
univ <- readr::read_csv("mcmc_outputs/zimphia/hmc/summaries/zimphia_hmc_summary.csv")
mult <- readr::read_csv("mcmc_outputs/zimphia_multivariable/summaries/summary.csv")

comp <- tibble::tibble(
  parameter = "beta_sex",
  univariate_median = univ$median[univ$variable == "beta"],
  univariate_ci = sprintf("(%.3f, %.3f)",
                          univ$q2.5[univ$variable == "beta"],
                          univ$q97.5[univ$variable == "beta"]),
  multivariable_median = mult$median[mult$covariate == "sex"],
  multivariable_ci = sprintf("(%.3f, %.3f)",
                              mult$q2.5[mult$covariate == "sex"],
                              mult$q97.5[mult$covariate == "sex"])
)
readr::write_csv(comp, "outputs/tables/tab_multivariable_compare.csv")
print(comp)
```

- [ ] **Step 9: Manuscript edit — insert "Multivariable sensitivity" subsection**

Open `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` and locate the end of `\subsection{Application to ZIMPHIA 2020 Data}` (around line 887). After the existing ZIMPHIA results paragraphs (and before the start of `\section{Discussion}` around line 1053), insert:

```latex
\subsection{Multivariable sensitivity}
\label{sec:multivariable}
We refitted the ZIMPHIA model with three covariates ($\sex$, $\urbanrural$, $\ageband$) under the same HMC settings to verify that the sex effect reported in Table~\ref{tab:zimphia-posterior} is not confounded by demographic composition. The posterior median for $\beta_{\sex}$ shifted from XXX (95\% CrI: $-0.174, -0.139$) in the univariate model to YYY (95\% CrI: $\ldots$) under multivariable adjustment. [If within original CrI: ``This is well within the univariate credible interval, supporting the single-covariate exposition used for the sampler comparison.''] [If shifted: ``We report this transparently; the conclusions about HMC vs MH efficiency, which depend on posterior geometry and not on covariate adjustment, are unaffected.'']
```

Fill in the XXX/YYY numbers from the `comp` table built in Step 8.

- [ ] **Step 10: Commit**

```bash
git add R/zimphia_multivariable.R inst/models/loglogistic_interval_multivariable.stan inst/scripts/04_zimphia_multivariable.R tests/testthat/test-zimphia-multivariable.R NAMESPACE man/prepare_zimphia_multivariable_data.Rd man/fit_zimphia_multivariable.Rd outputs/tables/tab_multivariable_compare.csv ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "feat: multivariable ZIMPHIA fit and sensitivity subsection (revision Task B)"
```

---

## Task C: Birth-cohort stratified ZIMPHIA fits (revision plan §3 Task C)

**Files:**
- Create: `R/zimphia_cohort.R`
- Create: `tests/testthat/test-zimphia-cohort.R`
- Create: `inst/scripts/05_zimphia_cohort.R`
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (one paragraph in Discussion + supplementary forest plot)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-zimphia-cohort.R`:
```r
test_that("derive_birth_cohort assigns subjects to the correct cohort", {
  res <- derive_birth_cohort(
    age = c(20, 30, 40, 55, 70),  # birth year = 2020 - age = 2000, 1990, 1980, 1965, 1950
    survey_year = 2020
  )
  expect_equal(as.character(res),
               c("1990+", "1990+", "1980-1989", "1965-1979", NA))
})

test_that("derive_birth_cohort respects boundary inclusion", {
  res <- derive_birth_cohort(
    age = c(31, 40, 41, 55, 56),  # birth = 1989, 1980, 1979, 1965, 1964
    survey_year = 2020
  )
  expect_equal(as.character(res),
               c("1980-1989", "1980-1989", "1965-1979", "1965-1979", NA))
})
```

- [ ] **Step 2: Run to confirm failure**

```r
devtools::test(filter = "zimphia-cohort")
```

Expected: FAIL with `could not find function "derive_birth_cohort"`.

- [ ] **Step 3: Implement `derive_birth_cohort()` and `fit_zimphia_cohort()`**

Create `R/zimphia_cohort.R`:
```r
#' Assign each subject to a birth cohort based on survey year and age
#'
#' Cohorts: 1965-1979 (older), 1980-1989 (middle), 1990+ (younger). Returns
#' NA for ages outside the studied range.
#'
#' @param age Numeric vector of ages at survey.
#' @param survey_year Numeric. Default 2020 (ZIMPHIA).
#' @return Factor with three levels ordered older-to-younger.
#' @export
derive_birth_cohort <- function(age, survey_year = 2020) {
  birth_year <- survey_year - age
  factor(
    dplyr::case_when(
      birth_year >= 1965 & birth_year <= 1979 ~ "1965-1979",
      birth_year >= 1980 & birth_year <= 1989 ~ "1980-1989",
      birth_year >= 1990                       ~ "1990+",
      TRUE                                     ~ NA_character_
    ),
    levels = c("1965-1979", "1980-1989", "1990+")
  )
}

#' Fit the ZIMPHIA HMC model separately on three birth cohorts
#'
#' @param analysis_data Prepared ZIMPHIA analysis tibble (output of
#'   `run_zimphia_analysis()`).
#' @param output_dir Root output directory; per-cohort subdirectories created.
#' @param stan_model_file Path to scalar-beta Stan model (univariate, sex only).
#' @return A tibble with one row per cohort: cohort name, n, posterior summaries
#'   for `alpha`, `beta`, `gamma`, runtime, convergence flags.
#' @export
fit_zimphia_cohort <- function(
    analysis_data,
    output_dir = "mcmc_outputs/zimphia_cohort",
    stan_model_file = "inst/models/loglogistic_interval.stan") {

  cohort <- derive_birth_cohort(analysis_data$age)
  analysis_data$cohort <- cohort

  results <- list()
  mod <- cmdstanr::cmdstan_model(stan_model_file)

  for (lvl in levels(cohort)) {
    sub <- dplyr::filter(analysis_data, cohort == lvl)
    if (nrow(sub) < 100) {
      warning("Cohort ", lvl, " has only ", nrow(sub), " observations; skipping")
      next
    }
    out_sub <- file.path(output_dir, gsub("\\+", "plus", lvl))
    dir.create(file.path(out_sub, "summaries"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(out_sub, "draws"), recursive = TRUE, showWarnings = FALSE)

    stan_data <- list(
      N = nrow(sub),
      L = pmax(sub$L, 1e-10),
      R = sub$R,
      X = as.numeric(sub$X1),
      w = sub$weight
    )

    t0 <- Sys.time()
    fit <- mod$sample(
      data = stan_data,
      chains = 4, parallel_chains = 4,
      iter_warmup = 1000, iter_sampling = 5000,
      seed = 2025, refresh = 0
    )
    rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    summ <- fit$summary(
      variables = c("alpha", "beta", "gamma"),
      "mean", "median", "sd",
      ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
      "rhat", "ess_bulk"
    )
    summ$cohort <- lvl
    summ$n <- nrow(sub)
    summ$runtime_secs <- rt

    saveRDS(summ, file.path(out_sub, "summaries", "summary.rds"))
    saveRDS(fit$draws(variables = c("alpha", "beta", "gamma"), format = "df"),
            file.path(out_sub, "draws", "draws.rds"))

    results[[lvl]] <- summ
  }

  combined <- dplyr::bind_rows(results)
  readr::write_csv(combined, file.path(output_dir, "cohort_compare.csv"))
  combined
}
```

- [ ] **Step 4: Document and run tests**

```r
devtools::document()
devtools::test(filter = "zimphia-cohort")
```

Expected: PASS (2 tests).

- [ ] **Step 5: Run the cohort fits**

Create `inst/scripts/05_zimphia_cohort.R`:
```r
#!/usr/bin/env Rscript
# Task C: Birth-cohort stratified ZIMPHIA fits
suppressPackageStartupMessages(devtools::load_all())

analysis_data <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")
res <- fit_zimphia_cohort(
  analysis_data,
  output_dir = "mcmc_outputs/zimphia_cohort",
  stan_model_file = "inst/models/loglogistic_interval.stan"
)
print(res)
saveRDS(res, "mcmc_outputs/zimphia_cohort/combined_summary.rds")
```

Run:
```bash
Rscript inst/scripts/05_zimphia_cohort.R 2>&1 | tee logs/substantive_revision/cohort_fits.log
```

Expected runtime: ~6 minutes total (three fits at sub-cohort sizes).

- [ ] **Step 6: Build the forest plot for the supplement**

In R:
```r
library(ggplot2); devtools::load_all()
combined <- readRDS("mcmc_outputs/zimphia_cohort/combined_summary.rds")
p <- ggplot(combined |> dplyr::filter(variable %in% c("alpha", "gamma")),
            aes(y = cohort, x = median, xmin = q2.5, xmax = q97.5)) +
  geom_pointrange() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Posterior median (95% CrI)", y = NULL,
       title = "ZIMPHIA posteriors by birth cohort") +
  theme_bw()
ggsave("outputs/figures/figC1_cohort_forest.png", p,
       width = 8, height = 4, dpi = 320)
```

- [ ] **Step 7: Manuscript edit — Discussion paragraph + supplement reference**

In `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, locate `\subsection{Interpretation and context}` around line 1061. Append (after the existing paragraphs of that subsection, before the next subsection):

```latex
We assessed the constant-incidence assumption by stratifying the ZIMPHIA analysis into three birth cohorts (1965--1979, 1980--1989, 1990+) and refitting the model within each. [If stable: ``Posterior medians for $\alpha$ varied by less than X\% across cohorts (Figure~\ref{fig:cohort-forest}, Appendix), defending the constant-incidence approximation used in the main analysis.''] [If declining: ``Posterior medians for $\alpha$ declined monotonically from the oldest to the youngest cohort, quantifying the non-stationarity cost of the constant-hazard window; the headline sampler-comparison conclusions are unaffected.''] Numerical results appear in Table~\ref{tab:cohort-compare} of the supplement.
```

Fill in the X% number after reading `combined_summary.rds`.

- [ ] **Step 8: Commit**

```bash
git add R/zimphia_cohort.R inst/scripts/05_zimphia_cohort.R tests/testthat/test-zimphia-cohort.R NAMESPACE man/derive_birth_cohort.Rd man/fit_zimphia_cohort.Rd outputs/figures/figC1_cohort_forest.png outputs/tables/tab_cohort_compare.csv mcmc_outputs/zimphia_cohort/combined_summary.rds ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "feat: birth-cohort stratified ZIMPHIA fits + Discussion paragraph (revision Task C)"
```

---

## Task D: Implied incidence derivation (revision plan §3 Task D)

**Files:**
- Create: `R/zimphia_incidence.R`
- Create: `tests/testthat/test-zimphia-incidence.R`
- Create: `inst/scripts/06_zimphia_incidence.R`
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (new §4.5 "Implied incidence")

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-zimphia-incidence.R`:
```r
test_that("compute_age_specific_hazard matches closed-form log-logistic hazard", {
  draws <- tibble::tibble(
    .draw = 1:4,
    alpha = c(5, 5, 5, 5),
    beta = c(0, 0, 0, 0),
    gamma = c(1.5, 1.5, 1.5, 1.5)
  )

  res <- compute_age_specific_hazard(
    draws, ages = c(10, 20, 30), x = 0
  )
  # closed-form: h(a) = (gamma/alpha) * (a/alpha)^(gamma-1) / (1 + (a/alpha)^gamma)
  expected <- (1.5 / 5) * (c(10, 20, 30) / 5)^(0.5) /
              (1 + (c(10, 20, 30) / 5)^1.5)
  expect_equal(res$hazard_mean, expected, tolerance = 1e-8)
  expect_equal(res$age, c(10, 20, 30))
})

test_that("compute_population_incidence is a weighted mean of per-subject hazards", {
  draws <- tibble::tibble(.draw = 1, alpha = 5, beta = log(2), gamma = 1.5)
  pop <- tibble::tibble(age = c(20, 20), X1 = c(0, 1), weight = c(1, 1))

  res <- compute_population_incidence(draws, pop)

  # Two subjects, equal weights → average of the two per-subject hazards
  h_male   <- (1.5 / 5) * (20 / 5)^0.5 / (1 + (20 / 5)^1.5)
  h_female <- (1.5 / (5 * 2)) * (20 / (5 * 2))^0.5 / (1 + (20 / (5 * 2))^1.5)
  expect_equal(res$incidence_median, mean(c(h_male, h_female)), tolerance = 1e-8)
})
```

- [ ] **Step 2: Run to confirm failure**

```r
devtools::test(filter = "zimphia-incidence")
```

Expected: FAIL on both with `could not find function`.

- [ ] **Step 3: Implement the two pure post-processing functions**

Create `R/zimphia_incidence.R`:
```r
#' Compute posterior age-specific hazard h(a | x) for a log-logistic AFT model
#'
#' For each draw `(alpha, beta, gamma)` and each age `a`, returns the hazard
#' \deqn{h(a | x) = (\gamma / \alpha_x) \cdot (a / \alpha_x)^{\gamma - 1} / (1 + (a / \alpha_x)^\gamma)}
#' with \eqn{\alpha_x = \alpha \cdot \exp(x^T \beta)}.
#'
#' @param draws Data frame of posterior draws with columns `alpha`, `beta`, `gamma`.
#'   `beta` is treated as scalar here; for vector beta use [compute_population_incidence()].
#' @param ages Numeric vector of ages to evaluate.
#' @param x Numeric scalar covariate value (e.g. 0 = male, 1 = female).
#'
#' @return Tibble with columns `age`, `hazard_mean`, `hazard_q2.5`, `hazard_q97.5`.
#' @export
compute_age_specific_hazard <- function(draws, ages, x = 0) {
  hazard <- function(a, alpha, beta, gamma) {
    ax <- alpha * exp(beta * x)
    (gamma / ax) * (a / ax)^(gamma - 1) / (1 + (a / ax)^gamma)
  }
  out <- lapply(ages, function(a) {
    h <- hazard(a, draws$alpha, draws$beta, draws$gamma)
    tibble::tibble(
      age = a,
      hazard_mean = mean(h),
      hazard_q2.5 = stats::quantile(h, 0.025, names = FALSE),
      hazard_q97.5 = stats::quantile(h, 0.975, names = FALSE)
    )
  })
  dplyr::bind_rows(out)
}

#' Compute the population-level implied incidence per posterior draw
#'
#' For each draw, evaluates h(a_i | x_i) on the ZIMPHIA analysis tibble, then
#' weights by `weight` and averages to yield an incidence rate (per
#' person-year). Returns the posterior median and 95% credible interval.
#'
#' @param draws Tibble with one row per posterior draw, columns `alpha`,
#'   `beta`, `gamma`.
#' @param pop Tibble with one row per subject, columns `age`, `X1`, `weight`.
#'
#' @return Tibble with one row: `incidence_median`, `incidence_q2.5`,
#'   `incidence_q97.5`, expressed per person-year (multiply by 100 outside
#'   for per-100-person-years).
#' @export
compute_population_incidence <- function(draws, pop) {
  hazard_one <- function(alpha, beta, gamma) {
    ax <- alpha * exp(beta * pop$X1)
    h <- (gamma / ax) * (pop$age / ax)^(gamma - 1) /
         (1 + (pop$age / ax)^gamma)
    stats::weighted.mean(h, pop$weight)
  }
  per_draw <- mapply(hazard_one,
                     draws$alpha, draws$beta, draws$gamma,
                     USE.NAMES = FALSE)
  tibble::tibble(
    incidence_median  = stats::median(per_draw),
    incidence_q2.5    = stats::quantile(per_draw, 0.025, names = FALSE),
    incidence_q97.5   = stats::quantile(per_draw, 0.975, names = FALSE)
  )
}
```

- [ ] **Step 4: Document and run tests**

```r
devtools::document()
devtools::test(filter = "zimphia-incidence")
```

Expected: PASS (2 tests).

- [ ] **Step 5: Build the incidence figure and population number**

Create `inst/scripts/06_zimphia_incidence.R`:
```r
#!/usr/bin/env Rscript
# Task D: Implied incidence derivation
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr); library(ggplot2)
})

draws <- readRDS("mcmc_outputs/zimphia/hmc/draws/zimphia_hmc_draws.rds")
pop <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")

age_grid <- seq(15, 60, by = 0.5)

# Per-sex age-specific hazard curves
hazard_male <- compute_age_specific_hazard(draws, age_grid, x = 0) |>
  mutate(sex = "Male")
hazard_female <- compute_age_specific_hazard(draws, age_grid, x = 1) |>
  mutate(sex = "Female")
hazard_curves <- bind_rows(hazard_male, hazard_female)
saveRDS(hazard_curves, "mcmc_outputs/zimphia_incidence/hazard_curves.rds")

p <- ggplot(hazard_curves,
            aes(x = age, y = hazard_mean, ymin = hazard_q2.5,
                ymax = hazard_q97.5, fill = sex, color = sex)) +
  geom_ribbon(alpha = 0.25, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  facet_wrap(~ sex) +
  labs(x = "Age (years)",
       y = "Implied hazard (per person-year)",
       title = "ZIMPHIA-implied age-specific HIV incidence hazard") +
  theme_bw() + theme(legend.position = "none")
ggsave("outputs/figures/fig8_incidence_curve.png", p,
       width = 9, height = 4, dpi = 320)

# Population-level incidence
pop_inc <- compute_population_incidence(draws, pop |> select(age, X1, weight))
pop_inc_per100 <- pop_inc |>
  mutate(across(starts_with("incidence_"), \(x) x * 100))
print(pop_inc_per100)
readr::write_csv(pop_inc_per100,
                 "outputs/tables/tab_population_incidence.csv")
```

Run:
```bash
Rscript inst/scripts/06_zimphia_incidence.R 2>&1 | tee logs/substantive_revision/incidence.log
```

Expected: figure written, table written, prints a number around 1–3 per 100 person-years (Zimbabwe HIV incidence in 2020 is roughly 0.4–0.6 per 100 person-years; the model-implied number averaged over a long retrospective window will be higher — note this for the Discussion).

- [ ] **Step 6: Manuscript edit — insert §4.5 "Implied incidence"**

In `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, locate the end of `\subsection{Application to ZIMPHIA 2020 Data}` and insert before `\subsection{Multivariable sensitivity}` (which Task B added):

```latex
\subsection{Implied incidence}
\label{sec:implied-incidence}
The fitted log-logistic AFT model defines a closed-form age-specific hazard $h(a \mid x) = (\gamma / \alpha_x)(a / \alpha_x)^{\gamma - 1} / (1 + (a / \alpha_x)^\gamma)$, with $\alpha_x = \alpha \exp(x^\top \beta)$. Figure~\ref{fig:incidence} shows the posterior mean and 95\% pointwise credible band for $h(a)$ over ages 15--60, evaluated separately for males and females. Averaging $h(a_i \mid x_i)$ across the ZIMPHIA analytic sample, weighted by survey weights, yields a population-level implied incidence of XXX per 100 person-years (95\% CrI: $\ldots$, $\ldots$). This figure represents an average hazard over the retrospective time-since-debut window rather than a current point-in-time incidence; see the Discussion (\S\ref{sec:constant-incidence}).
```

Fill in XXX from `outputs/tables/tab_population_incidence.csv`. Add a `\begin{figure}` environment referencing `outputs/figures/fig8_incidence_curve.png` with label `fig:incidence`.

- [ ] **Step 7: Commit**

```bash
git add R/zimphia_incidence.R inst/scripts/06_zimphia_incidence.R tests/testthat/test-zimphia-incidence.R NAMESPACE man/compute_age_specific_hazard.Rd man/compute_population_incidence.Rd outputs/figures/fig8_incidence_curve.png outputs/tables/tab_population_incidence.csv mcmc_outputs/zimphia_incidence/hazard_curves.rds ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "feat: implied incidence post-processing + figure (revision Task D)"
```

---

## Task E: Design-based variance via ZIMPHIA replicate weights (revision plan §3 Task E)

**Compute:** ~2.4 hours on laptop for 100 HMC refits. Kick off during day 2 morning.

**Files:**
- Create: `R/zimphia_design_variance.R`
- Create: `tests/testthat/test-zimphia-design-variance.R`
- Create: `inst/scripts/07_zimphia_design_variance.R`
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (expand §3 ZIMPHIA description, add §4 design-based paragraph, Limitations sentence)

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-zimphia-design-variance.R`:
```r
test_that("load_replicate_weights returns the requested columns joined on personid", {
  csv_path <- test_replicate_weights_csv()  # from setup-paths.R
  base <- tibble::tibble(personid = c("ZW20000000000101", "ZW20000000000102"))

  res <- load_replicate_weights(base, csv_path, n_reps = 5)

  expect_named(res, c("personid",
                       paste0("design_wt", sprintf("%03d", 1:5))))
  expect_equal(nrow(res), 2)
  expect_true(all(vapply(res[, -1], is.numeric, logical(1))))
})
```

- [ ] **Step 2: Run to confirm failure**

```r
devtools::test(filter = "zimphia-design-variance")
```

Expected: FAIL (function missing) or SKIP if the CSV is not on disk in the test environment.

- [ ] **Step 3: Implement `load_replicate_weights()` and `fit_zimphia_design_replicates()`**

Create `R/zimphia_design_variance.R`:
```r
#' Load the first `n_reps` ZIMPHIA replicate weights and join onto an analysis tibble
#'
#' @param base Tibble with `personid` to join on.
#' @param csv_path Path to `zimphia2020indintermediarywts.csv`.
#' @param n_reps Number of replicate weight columns to load (max 175).
#' @return Tibble with `personid` and `design_wt001 ... design_wtNNN`.
#' @export
load_replicate_weights <- function(base, csv_path, n_reps = 100L) {
  stopifnot(n_reps >= 1L, n_reps <= 175L)
  rep_cols <- paste0("design_wt", sprintf("%03d", seq_len(n_reps)))
  wts <- readr::read_csv(
    csv_path,
    col_select = c("personid", dplyr::all_of(rep_cols)),
    show_col_types = FALSE
  )
  dplyr::semi_join(wts, base, by = "personid") |>
    dplyr::inner_join(dplyr::select(base, personid), by = "personid")
}

#' Refit the ZIMPHIA HMC model 100 times using replicate weights
#'
#' For each replicate weight column, substitute it into the `weight` column of
#' the analysis tibble, run a 1-chain HMC fit (2000 warmup + 5000 sampling),
#' and record the posterior median + 95% CrI for `alpha`, `beta`, `gamma`.
#' The design-based 95% CrI for each parameter is the percentile interval of
#' posterior medians across replicates.
#'
#' @param analysis_data Prepared ZIMPHIA tibble (output of
#'   `run_zimphia_analysis()`).
#' @param weights_long Output of `load_replicate_weights()`.
#' @param output_dir Where to save per-replicate summaries.
#' @param n_chains Default 1 (matches a "design replicate" rather than
#'   primary inferential fit).
#' @return Tibble with `replicate`, `parameter`, `median`, `q2.5`, `q97.5`,
#'   `rhat`, `ess_bulk`, `runtime_secs`.
#' @export
fit_zimphia_design_replicates <- function(
    analysis_data,
    weights_long,
    output_dir = "mcmc_outputs/zimphia_design_replicates",
    stan_model_file = "inst/models/loglogistic_interval.stan",
    n_chains = 1L) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rep_cols <- grep("^design_wt", names(weights_long), value = TRUE)
  joined <- dplyr::inner_join(analysis_data, weights_long, by = "personid")

  mod <- cmdstanr::cmdstan_model(stan_model_file)

  results <- vector("list", length(rep_cols))
  for (i in seq_along(rep_cols)) {
    col <- rep_cols[i]
    rep_dir <- file.path(output_dir, sprintf("rep%03d", i))
    dir.create(rep_dir, showWarnings = FALSE)

    w_raw <- joined[[col]]
    keep <- w_raw > 0
    sub <- joined[keep, ]
    N <- nrow(sub)
    w_norm <- w_raw[keep] * (N / sum(w_raw[keep]))

    stan_data <- list(
      N = N,
      L = pmax(sub$L, 1e-10),
      R = sub$R,
      X = as.numeric(sub$X1),
      w = w_norm
    )

    t0 <- Sys.time()
    fit <- mod$sample(
      data = stan_data,
      chains = n_chains, parallel_chains = n_chains,
      iter_warmup = 2000, iter_sampling = 5000,
      seed = 2025 + i, refresh = 0
    )
    rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    summ <- fit$summary(
      variables = c("alpha", "beta", "gamma"),
      "median", ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
      "rhat", "ess_bulk"
    )
    summ$replicate <- i
    summ$runtime_secs <- rt

    saveRDS(summ, file.path(rep_dir, "summary.rds"))
    results[[i]] <- summ

    cat(sprintf("Replicate %d/%d done in %.1f s (max Rhat = %.3f)\n",
                i, length(rep_cols), rt, max(summ$rhat, na.rm = TRUE)))
  }

  combined <- dplyr::bind_rows(results)
  readr::write_csv(combined, file.path(output_dir, "all_replicates.csv"))
  combined
}
```

- [ ] **Step 4: Document and run the load test**

```r
devtools::document()
devtools::test(filter = "zimphia-design-variance")
```

Expected: PASS (1 test) or SKIP gracefully.

- [ ] **Step 5: Write the runner script**

Create `inst/scripts/07_zimphia_design_variance.R`:
```r
#!/usr/bin/env Rscript
# Task E: Design-based variance via ZIMPHIA replicate weights
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr); library(ggplot2)
})

base <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")
weights <- load_replicate_weights(
  base = base,
  csv_path = file.path("ZIMPHIA",
                       "ZIMPHIA 2020 Intermediary Weights (CSV)",
                       "zimphia2020indintermediarywts.csv"),
  n_reps = 100L
)

t0 <- Sys.time()
combined <- fit_zimphia_design_replicates(
  analysis_data = base,
  weights_long = weights,
  output_dir = "mcmc_outputs/zimphia_design_replicates",
  stan_model_file = "inst/models/loglogistic_interval.stan",
  n_chains = 1L
)
cat(sprintf("\nTotal wall time: %.2f hours\n",
            as.numeric(difftime(Sys.time(), t0, units = "hours"))))

# Compute design-based CrI per parameter
design_ci <- combined |>
  group_by(variable) |>
  summarise(
    design_lo = quantile(median, 0.025),
    design_hi = quantile(median, 0.975),
    design_width = design_hi - design_lo,
    .groups = "drop"
  )

# Load the primary model-based CIs
primary <- readr::read_csv("mcmc_outputs/zimphia/hmc/summaries/zimphia_hmc_summary.csv") |>
  transmute(variable, model_lo = q2.5, model_hi = q97.5,
            model_width = q97.5 - q2.5)

comparison <- left_join(design_ci, primary, by = "variable") |>
  mutate(inflation = design_width / model_width)
readr::write_csv(comparison, "outputs/tables/tab_design_variance.csv")
print(comparison)
```

- [ ] **Step 6: Kick off the run (long-running, ~2.4 h)**

```bash
nohup Rscript inst/scripts/07_zimphia_design_variance.R > logs/substantive_revision/design_variance.log 2>&1 &
echo "PID: $!"
```

While this runs, **proceed to writeup work** (manuscript edits for §3 expansion below).

Monitor:
```bash
tail -f logs/substantive_revision/design_variance.log
# Stop tail with Ctrl-C; the job keeps running.
```

- [ ] **Step 7: While Step 6 runs — expand §3 ZIMPHIA description in manuscript**

In `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, locate `\subsection{Applied example (ZIMPHIA 2020)}` (line 557). Currently a single paragraph. Replace with three paragraphs:

```latex
\subsection{Applied example (ZIMPHIA 2020)}
\label{sec:zimphia}

ZIMPHIA 2020 was a nationally representative, two-stage stratified household survey conducted in Zimbabwe between November 2019 and April 2020 \citep{noauthor_zimbabwe_2021}. The sampling frame was the Zimbabwe Master Sample, partitioned into 65 strata (rural and urban segments of each of 60 administrative districts plus six metropolitan strata). Within each stratum, primary sampling units (enumeration areas) were selected with probability proportional to size; within each selected enumeration area, a fixed number of households was systematically sampled. Adults aged 15--64 within sampled households were eligible for the individual questionnaire and biomarker testing (HIV, viral load).

Within this design, the survey provides individual sampling weights (\texttt{btwt0}) that adjust for the inclusion-probability inverse, household and individual non-response, and post-stratification calibration to 2012 census totals. For variance estimation, ZIMPHIA additionally supplies 175 pre-computed jackknife replicate weight columns (\texttt{design\_wt001} through \texttt{design\_wt175}) along with stratum (\texttt{varstrat}) and primary-sampling-unit (\texttt{varunit}) identifiers, enabling design-consistent inference without manual specification of the stratification and clustering structure.

The simulation scenarios are calibrated to this survey, mirroring its cross-sectional, single-visit design where seroconversion is interval-censored between sexual debut and survey. We apply the same Bayesian model to the ZIMPHIA microdata as an empirical validation; we additionally use the replicate weights to compute a design-consistent variance estimate (\S\ref{sec:design-variance} below). As in the simulations, primary weights are normalised to sum to $n$; trimming is considered only as a sensitivity exercise \citep{chen_approaches_2017,gelman_struggles_2007}.
```

- [ ] **Step 8: While Step 6 runs — draft the §4 design-variance paragraph (numbers to fill once Step 6 finishes)**

In the manuscript, after `\subsection{Application to ZIMPHIA 2020 Data}` and the existing results paragraphs but **before** Task B's "Multivariable sensitivity" section, insert:

```latex
\subsection{Design-based variance}
\label{sec:design-variance}
The primary HMC analysis treats the 16{,}554 weighted observations as approximately independent, ignoring the within-cluster correlation and stratification of the ZIMPHIA design. To quantify the impact of this approximation, we refitted the model 100 times, each time substituting one of the first 100 jackknife replicate weight columns supplied by ZIMPHIA for the primary weight. The design-based 95\% credible interval for each parameter is the 2.5\% / 97.5\% percentile of the posterior medians across replicates. For $\beta_{\sex}$, the design-based interval was [LO, HI], compared to the model-based credible interval [$-0.174, -0.139$]; the inflation factor (design width / model width) was FF. [If $\le 1.05$: ``This confirms that treating the weighted observations as independent was approximately benign for inference on $\beta_{\sex}$, consistent with the moderate weight dispersion (CV $\approx 0.50$) characteristic of well-implemented complex surveys.''] [If $> 1.2$: ``We therefore report the design-based interval as primary; the conclusion that $\beta_{\sex}$ is materially negative is unchanged, but its width should be interpreted as the design-corrected uncertainty.''] This analysis required 100 HMC refits in approximately 2.4 hours; the corresponding MH analysis would have required approximately 50 hours and was not undertaken within the revision timeline.
```

Fill in LO, HI, FF once Step 6 completes. The "approximately 50 hours" sentence is the **second leg of the asymmetry-as-feature argument**.

- [ ] **Step 9: Wait for Step 6 to finish, then collect and verify**

When the background job ends:
```bash
# Check completion
tail -1 logs/substantive_revision/design_variance.log
# Expect: "Total wall time: ~2.x hours"
ls mcmc_outputs/zimphia_design_replicates/rep001/summary.rds  # spot-check
```

In R:
```r
comparison <- readr::read_csv("outputs/tables/tab_design_variance.csv")
print(comparison)

# Convergence check across replicates
combined <- readr::read_csv("mcmc_outputs/zimphia_design_replicates/all_replicates.csv")
bad <- combined |> dplyr::filter(rhat > 1.01 | ess_bulk < 400)
cat("Replicates with bad convergence:", nrow(bad), "/", nrow(combined), "\n")
stopifnot(nrow(bad) / nrow(combined) < 0.02)  # < 2% allowed
```

- [ ] **Step 10: Build the design-variance forest plot**

In R:
```r
library(ggplot2); library(dplyr)
combined <- readr::read_csv("mcmc_outputs/zimphia_design_replicates/all_replicates.csv")
p <- combined |>
  filter(variable == "beta") |>
  ggplot(aes(x = median, y = factor(replicate))) +
  geom_pointrange(aes(xmin = q2.5, xmax = q97.5), size = 0.2) +
  geom_vline(xintercept = -0.156, linetype = "dashed") +
  scale_y_discrete(breaks = c("1", "25", "50", "75", "100")) +
  labs(x = "Posterior median (95% CrI) for beta_sex",
       y = "Replicate weight index",
       title = "Design-based variance: 100 ZIMPHIA replicate-weight refits") +
  theme_bw()
ggsave("outputs/figures/fig9_design_variance_forest.png", p,
       width = 7, height = 9, dpi = 320)
```

- [ ] **Step 11: Fill in the LO/HI/FF placeholders from Step 8 with the real numbers**

Open the manuscript and replace `[LO, HI]` and `FF` in `\subsection{Design-based variance}` with the values from `outputs/tables/tab_design_variance.csv`.

- [ ] **Step 12: Add the Limitations sentence**

In `\subsection{Strengths and limitations}` (around line 1134), append at the end:
```latex
The ZIMPHIA design-based variance correction was implemented via the 100 jackknife replicate weights; residual design features (post-stratification adjustments beyond those embedded in the replicate weights) were not separately accommodated.
```

- [ ] **Step 13: Commit**

```bash
git add R/zimphia_design_variance.R inst/scripts/07_zimphia_design_variance.R tests/testthat/test-zimphia-design-variance.R NAMESPACE man/load_replicate_weights.Rd man/fit_zimphia_design_replicates.Rd outputs/tables/tab_design_variance.csv outputs/figures/fig9_design_variance_forest.png ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "feat: design-based variance via ZIMPHIA replicate weights (revision Task E)"
```

> Do **not** commit `mcmc_outputs/zimphia_design_replicates/` itself — 100 directories of fits is heavy. Verify `.gitignore` excludes `mcmc_outputs/` (it does, per the repo convention). The per-replicate summary file is reproducible from the seeds.

---

## Task F: Misspecification simulation, central cell (revision plan §3 Task F)

**Compute:** MH leg ≈ 25 hours overnight; HMC leg ≈ 1.5 hours during the day. Kick off MH first (Day 1 evening if possible; Day 2 evening at the latest).

**Files:**
- Modify: `R/simulation.R` (add Weibull params to `get_default_params()`)
- Create: `R/misspec_simulation.R`
- Create: `tests/testthat/test-misspec-simulation.R`
- Create: `inst/scripts/08_misspec_sim.R`
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (new §3.x misspec scenario, §4.x results, Discussion paragraph, Limitations sentence)

- [ ] **Step 1: Write the failing test for the Weibull DGM**

Create `tests/testthat/test-misspec-simulation.R`:
```r
test_that("simulate_survival_data_weibull recovers Weibull median by Monte Carlo", {
  set.seed(2025)
  params <- get_default_params()
  params$k_weibull <- 2.0
  params$lambda_weibull <- 6.01
  params$target_censoring_prop <- 0  # disable censoring to inspect raw T

  res <- simulate_survival_data_weibull(n = 50000, params = params, weight_type = "none")

  # T is implicit; we infer it from L=0 (events) or compute it via the same
  # inverse-CDF the function uses. Easier: re-derive from U just to assert
  # median calibration.
  Ti_check <- params$lambda_weibull *
              (-log(1 - runif(50000)))^(1 / params$k_weibull)
  # Weibull median = lambda * (log 2)^(1/k) = 6.01 * sqrt(log 2) ≈ 5.0
  expect_equal(median(Ti_check), 5.0, tolerance = 0.05)
})

test_that("simulate_survival_data_weibull produces the right tibble shape", {
  set.seed(1)
  params <- get_default_params()
  params$k_weibull <- 2.0
  params$lambda_weibull <- 6.01

  res <- simulate_survival_data_weibull(n = 200, params = params, weight_type = "high")

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("X1", "A0", "A_event", "visit", "L", "R", "status", "weight"))
  expect_true(all(res$weight > 0))
  expect_true(all(res$L >= 0))
})
```

- [ ] **Step 2: Run to confirm failure**

```r
devtools::test(filter = "misspec-simulation")
```

Expected: FAIL — `simulate_survival_data_weibull` doesn't exist and `get_default_params()$k_weibull` is NULL.

- [ ] **Step 3: Add Weibull params to `get_default_params()`**

In `R/simulation.R`, modify `get_default_params()` (line 10) to add Weibull parameters:

```r
get_default_params <- function() {
  list(
    beta0 = log(5),
    beta1 = -0.5,
    gamma = 1.5,
    p_sex = 0.55,

    # weight dispersion
    k_low = 10, theta_low = 0.1,
    k_high = 1, theta_high = 1.0,

    # age at sexual debut
    a0_min = 15, a0_max = 35, a0_shape1 = 2, a0_shape2 = 5,

    follow_max = 40,
    target_censoring_prop = 0.3,

    # Weibull misspecification DGM (Task F)
    # Calibrated so median(T) = 5 (matches log-logistic) with stricter shape
    k_weibull = 2.0,
    lambda_weibull = 6.01
  )
}
```

- [ ] **Step 4: Implement `simulate_survival_data_weibull()` and `run_misspec_simulation()`**

Create `R/misspec_simulation.R`:
```r
#' Simulate interval-censored data under a Weibull DGM
#'
#' Mirror of `simulate_survival_data()` (log-logistic) with the inverse-CDF
#' sample swapped for Weibull. The fit-side log-logistic model becomes
#' misspecified — this is the point of the misspecification scenario (Task F).
#'
#' @inheritParams simulate_survival_data
#' @return Same tibble shape as `simulate_survival_data()`.
#' @export
simulate_survival_data_weibull <- function(
    n, params, weight_type = c("none", "low", "high")) {
  weight_type <- match.arg(weight_type)

  covs <- list(X1 = stats::rbinom(n, 1, params$p_sex))
  U <- stats::rbeta(n, params$a0_shape1, params$a0_shape2)
  A0 <- params$a0_min + (params$a0_max - params$a0_min) * U

  # Weibull inverse CDF, scaled by AFT factor for X1
  Uw <- stats::runif(n)
  Ti <- params$lambda_weibull *
        exp(params$beta1 * covs$X1) *
        (-log(Uw))^(1 / params$k_weibull)

  admin_cens <- Ti > params$follow_max
  Ti_capped <- pmin(Ti, params$follow_max)
  A_event <- A0 + Ti_capped

  # Delegate visit-time generation to the same internal helper used by the
  # log-logistic DGM so admin-censored subjects pick up right-censoring through
  # the usual L_dur / R_dur logic below (rather than being forced to interval
  # at the boundary).
  visit <- determine_visit_times(A0, A_event, params, force_censor = admin_cens)

  eps <- 1e-12
  L_dur <- ifelse(A_event <= visit, 0, pmax(visit - A0, 0))
  R_dur <- ifelse(A_event <= visit, pmax(visit - A0, 0), Inf)
  status <- ifelse(is.infinite(R_dur), 0L, 3L)

  # Weights: reuse the same generator as the log-logistic DGM
  if (weight_type == "none") {
    w <- rep(1, n)
  } else if (weight_type == "low") {
    w0 <- stats::rgamma(n, shape = params$k_low, scale = params$theta_low)
    w <- n * w0 / sum(w0)
  } else {
    w0 <- stats::rgamma(n, shape = params$k_high, scale = params$theta_high)
    w <- n * w0 / sum(w0)
  }

  tibble::tibble(
    X1 = covs$X1,
    A0 = A0, A_event = A_event, visit = visit,
    L = pmax(L_dur, eps), R = R_dur,
    status = status, weight = w
  )
}

#' Drive a single misspecification cell: HMC + MH fits on Weibull-truth data
#'
#' Generates 200 replicate datasets at `(n, cens, weight_cv)`, fits log-logistic
#' HMC and MH on each, and stores the same standardized outputs the main
#' simulation pipeline produces.
#'
#' @param n Sample size (default 2000).
#' @param target_censoring_prop Censoring proportion (default 0.3).
#' @param weight_type Weighting regime (default "high").
#' @param n_replicates Number of replicates (default 200).
#' @param data_dir Directory to write simulated .rds files.
#' @param hmc_results_dir Directory for HMC outputs.
#' @param mh_results_dir Directory for MH outputs.
#' @return Invisible list of file lists.
#' @export
run_misspec_simulation <- function(
    n = 2000,
    target_censoring_prop = 0.3,
    weight_type = "high",
    n_replicates = 200L,
    data_dir = "mcmc_outputs/misspec/n2000_c0.3_whigh/sim_data",
    hmc_results_dir = "mcmc_outputs/misspec/n2000_c0.3_whigh/hmc",
    mh_results_dir = "mcmc_outputs/misspec/n2000_c0.3_whigh/mh",
    do_hmc = TRUE,
    do_mh = TRUE) {

  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  params <- get_default_params()
  params$target_censoring_prop <- target_censoring_prop

  # Generate replicate datasets (only once; reused across HMC & MH)
  set.seed(2025)
  for (r in seq_len(n_replicates)) {
    fpath <- file.path(data_dir,
                       sprintf("sim_misspec_r%03d_n%04d_c%0.1f_w%s.rds",
                               r, n, target_censoring_prop, weight_type))
    if (!file.exists(fpath)) {
      dat <- simulate_survival_data_weibull(n, params, weight_type)
      saveRDS(dat, fpath, compress = "xz")
    }
  }

  out <- list()
  if (do_hmc) {
    out$hmc <- fit_logistic_hmc(
      sim_dir = data_dir,
      results_dir = hmc_results_dir,
      save = c("summary", "diagnostics"),
      workers = max(1L, parallel::detectCores(logical = FALSE) - 1L)
    )
  }
  if (do_mh) {
    out$mh <- fit_logistic_mh(
      sim_dir = data_dir,
      results_dir = mh_results_dir,
      save = c("summary", "diagnostics"),
      workers = max(1L, parallel::detectCores(logical = FALSE) - 1L),
      n_chains = 4, n_adapt = 1000, n_burnin = 1000, n_iter = 5000
    )
  }
  invisible(out)
}
```

- [ ] **Step 5: Document and run the test**

```r
devtools::document()
devtools::test(filter = "misspec-simulation")
```

Expected: PASS (2 tests).

- [ ] **Step 6: Sanity-check the DGM moments empirically**

In R:
```r
devtools::load_all()
params <- get_default_params()
params$target_censoring_prop <- 0
set.seed(1)
test <- simulate_survival_data_weibull(n = 5000, params = params, weight_type = "none")
# A_event - A0 = Ti_capped; should approximately match Weibull(2, 6.01)
T <- test$A_event - test$A0
T <- T[T < params$follow_max - 0.01]  # drop admin-censored
cat(sprintf("median(T) ≈ %.3f (target 5.0)\n", median(T)))
cat(sprintf("IQR(T) ≈ %.3f, %.3f\n", quantile(T, 0.25), quantile(T, 0.75)))
```

Expected: median ≈ 5 ± 0.1, IQR within ~10% of [3.2, 8.0]. If badly off, recompute `lambda_weibull` analytically and update `get_default_params()`.

- [ ] **Step 7: Write the runner script**

Create `inst/scripts/08_misspec_sim.R`:
```r
#!/usr/bin/env Rscript
# Task F: Misspecification simulation, central cell
# Usage:
#   Rscript inst/scripts/08_misspec_sim.R hmc    # HMC only (~1.5h)
#   Rscript inst/scripts/08_misspec_sim.R mh     # MH only (~25h)
#   Rscript inst/scripts/08_misspec_sim.R both   # both (skip if you split)

args <- commandArgs(trailingOnly = TRUE)
which_sampler <- if (length(args) > 0) args[1] else "both"
stopifnot(which_sampler %in% c("hmc", "mh", "both"))

suppressPackageStartupMessages(devtools::load_all())

run_misspec_simulation(
  n = 2000,
  target_censoring_prop = 0.3,
  weight_type = "high",
  n_replicates = 200L,
  do_hmc = which_sampler %in% c("hmc", "both"),
  do_mh  = which_sampler %in% c("mh",  "both")
)

cat("\nMisspec simulation done.\n")
```

- [ ] **Step 8: Kick off the MH leg overnight (longer)**

```bash
nohup Rscript inst/scripts/08_misspec_sim.R mh \
  > logs/substantive_revision/misspec_mh.log 2>&1 &
echo "PID: $!"
```

This runs ~25 hours. **Do not** start the HMC 1,000-rep rerun (Task G) on the same laptop overnight — they will fight for cores.

- [ ] **Step 9: Run the HMC leg during daytime spare cycles**

When the laptop is idle but not running MH at full tilt (e.g. mid-morning the next day, with `top` confirming ≤ 4 cores busy):
```bash
Rscript inst/scripts/08_misspec_sim.R hmc \
  > logs/substantive_revision/misspec_hmc.log 2>&1
```

Expected: ~1.5 hours on 8 cores.

- [ ] **Step 10: Build the misspec summary table and figure**

In R:
```r
devtools::load_all()
library(dplyr); library(ggplot2)

hmc_files <- list.files(
  "mcmc_outputs/misspec/n2000_c0.3_whigh/hmc/summaries",
  pattern = "_summary\\.rds$", full.names = TRUE)
mh_files <- list.files(
  "mcmc_outputs/misspec/n2000_c0.3_whigh/mh/summaries",
  pattern = "_summary\\.rds$", full.names = TRUE)

agg <- function(files, sampler) {
  purrr::map_dfr(files, \(f) {
    s <- readRDS(f)
    s |> mutate(sampler = sampler, file = basename(f))
  })
}
all_summ <- bind_rows(agg(hmc_files, "HMC"), agg(mh_files, "MH"))

# Truth (Weibull DGM) for beta_sex is -0.5 (same as log-logistic) — only the
# baseline survival changes. So coverage is computed against -0.5.
truth_beta <- -0.5

summary_tbl <- all_summ |>
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
readr::write_csv(summary_tbl, "outputs/tables/tab_misspec_summary.csv")
print(summary_tbl)

p <- summary_tbl |>
  tidyr::pivot_longer(c(bias, rmse, coverage, ess_per_sec)) |>
  ggplot(aes(x = sampler, y = value, fill = sampler)) +
  geom_col() +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "Misspecified DGM (Weibull): central cell only",
       subtitle = "n=2000, censoring=0.3, weight CV ≈ 0.5") +
  theme_bw() + theme(legend.position = "none")
ggsave("outputs/figures/fig10_misspec_compare.png", p,
       width = 9, height = 6, dpi = 320)
```

- [ ] **Step 11: Manuscript edit — Methods, Results, Discussion, Limitations**

**Methods (§3 simulation design, after line 485):**
```latex
\subsubsection*{Misspecification scenario}
\label{sec:misspec}
As a targeted sensitivity analysis, we added a single misspecification cell at $n = 2{,}000$, censoring proportion 0.3, and weight regime ``high'' (CV $\approx 1.0$). The data-generating mechanism in this cell is a Weibull distribution with shape $k = 2.0$ and scale $\lambda = 6.01$, chosen so that the marginal median time-to-event matches the log-logistic truth used elsewhere ($\text{median} = 5$) while introducing a substantively different hazard shape (strictly monotone for the Weibull vs unimodal for the log-logistic). The fitted model remains log-logistic; the cell therefore quantifies the consequences of a misspecified baseline. Two hundred replicates were generated and fitted with both HMC and MH using identical settings to the main simulation.
```

**Results (§4, after the existing sim subsections):**
```latex
\subsubsection*{Performance under baseline misspecification}
\label{sec:misspec-results}
Under the Weibull data-generating mechanism, HMC retained [smaller / equal / larger] RMSE than MH for $\beta_{\sex}$ (Table~\ref{tab:misspec-summary}), with coverage of XXX\% for HMC and YYY\% for MH at the nominal 95\% level. The HMC ESS-per-second advantage was largely preserved (HMC: ZZZ; MH: WWW). This is consistent with our interpretation that the sampler-comparison conclusions are driven by posterior geometry rather than by the precise form of the truth.
```

**Discussion (§5 / §6, one paragraph in Methodological implications):**
```latex
A reviewer rightly noted that the simulation truth and the fitted model share the log-logistic family, raising the question of whether HMC's advantages depend on correct specification. The Weibull misspecification cell reported in \S\ref{sec:misspec-results} addresses this directly: with the truth swapped to a Weibull distribution matched on median survival, the relative ordering of HMC and MH on every metric we report (bias, RMSE, coverage, ESS/s) was preserved. This is the expected consequence of HMC's efficiency stemming from posterior-density gradients, which exist independently of whether the model is the true generative process.
```

**Limitations:**
```latex
Misspecification was examined at one representative cell (Weibull truth, $n = 2{,}000$, censoring 0.3, weight CV $\approx 1.0$); broader misspecification regimes (mixture truths, time-varying covariates, alternative baseline families) are left to future work.
```

- [ ] **Step 12: Verify convergence rate across the 400 fits**

```r
hmc_diag_files <- list.files(
  "mcmc_outputs/misspec/n2000_c0.3_whigh/hmc/diagnostics",
  pattern = "_diag\\.rds$", full.names = TRUE)
diag <- purrr::map_dfr(hmc_diag_files, readRDS)
cat("HMC convergence rate:", mean(diag$max_rhat <= 1.01 & diag$min_ess >= 400) * 100, "%\n")
stopifnot(mean(diag$max_rhat <= 1.01 & diag$min_ess >= 400) >= 0.95)
```

Same check for MH. If < 95% converge, **investigate before reporting**.

- [ ] **Step 13: Commit**

```bash
git add R/simulation.R R/misspec_simulation.R inst/scripts/08_misspec_sim.R tests/testthat/test-misspec-simulation.R NAMESPACE man/simulate_survival_data_weibull.Rd man/run_misspec_simulation.Rd outputs/tables/tab_misspec_summary.csv outputs/figures/fig10_misspec_compare.png ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "feat: Weibull misspecification scenario (revision Task F)"
```

---

## Task G: HMC 1,000-rep rerun at two central cells (revision plan §3 Task G)

**Compute:** Cell `n=2,000` ≈ 3 hours; cell `n=10,000` ≈ 25 hours. Run **sequentially**, on different overnight slots than Task F.

**Files:**
- Modify: `R/simulation.R` (add `n_replicates_hmc` and `samplers` args to `run_simulations()`)
- Create: `inst/scripts/09_hmc_1000_rerun.R`
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (sim-table footnote, captions, new paragraph)

- [ ] **Step 1: Extend `run_simulations()` with new arguments**

In `R/simulation.R`, modify the signature and body of `run_simulations()` (line 262). Replace the existing function with:
```r
run_simulations <- function(
    out_dir = "data/sim_data",
    n_obs_vec = c(200, 2000, 10000),
    censoring_props = c(0.1, 0.3, 0.5),
    weight_types = c("none", "low", "high"),
    n_replicates = 200L,
    n_replicates_hmc = NULL,
    samplers = c("hmc", "mh")) {
  set.seed(2025)

  if (is.null(n_replicates_hmc)) n_replicates_hmc <- n_replicates
  effective_reps <- max(n_replicates, n_replicates_hmc)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  scenarios <- tidyr::crossing(
    n_obs = n_obs_vec,
    target_censoring_prop = censoring_props,
    weight_type = weight_types
  ) |>
    dplyr::arrange(n_obs, target_censoring_prop, weight_type) |>
    dplyr::mutate(scenario_id = dplyr::row_number())

  sim_grid <- scenarios |>
    tidyr::uncount(effective_reps, .id = "sim_id")

  save_one_sim <- function(n_obs, target_censoring_prop, weight_type,
                           scenario_id, sim_id) {
    params <- get_default_params()
    params$target_censoring_prop <- target_censoring_prop
    dat <- simulate_survival_data(n = n_obs, params = params,
                                  weight_type = weight_type)
    fname <- file.path(
      out_dir,
      sprintf("sim_s%03d_r%03d_n%04d_c%0.1f_w%s.rds",
              scenario_id, sim_id, n_obs, target_censoring_prop, weight_type)
    )
    saveRDS(dat, fname, compress = "xz")
  }
  purrr::pwalk(.l = sim_grid, .f = save_one_sim)

  attr(sim_grid, "n_replicates") <- n_replicates
  attr(sim_grid, "n_replicates_hmc") <- n_replicates_hmc
  attr(sim_grid, "samplers") <- samplers
  invisible(sim_grid)
}
```

Note: This **only changes data generation**. The actual sampler choice happens in the runner script Step 3.

- [ ] **Step 2: Verify the existing call sites still work**

```r
devtools::load_all()
# Quick smoke test — no actual fits, just data generation
tmp <- tempfile()
dir.create(tmp)
g <- run_simulations(out_dir = tmp, n_obs_vec = 200, censoring_props = 0.3,
                     weight_types = "high", n_replicates = 5L)
stopifnot(length(list.files(tmp, pattern = "\\.rds$")) == 5)
g2 <- run_simulations(out_dir = tmp, n_obs_vec = 200, censoring_props = 0.3,
                      weight_types = "high",
                      n_replicates = 5L, n_replicates_hmc = 10L)
stopifnot(length(list.files(tmp, pattern = "\\.rds$")) == 10)
unlink(tmp, recursive = TRUE)
cat("run_simulations() still works after edit.\n")
```

- [ ] **Step 3: Write the runner script for the two central cells**

Create `inst/scripts/09_hmc_1000_rerun.R`:
```r
#!/usr/bin/env Rscript
# Task G: HMC 1,000-rep rerun at two central cells
# Args: cell name ("n2000" or "n10000")

args <- commandArgs(trailingOnly = TRUE)
cell <- if (length(args) > 0) args[1] else "n2000"
stopifnot(cell %in% c("n2000", "n10000"))

suppressPackageStartupMessages(devtools::load_all())

cfg <- list(
  n2000  = list(n_obs = 2000L,  data_dir = "mcmc_outputs/hmc1000/n2000_c0.3_whigh/sim_data",
                out_dir  = "mcmc_outputs/hmc1000/n2000_c0.3_whigh/hmc"),
  n10000 = list(n_obs = 10000L, data_dir = "mcmc_outputs/hmc1000/n10000_c0.3_whigh/sim_data",
                out_dir  = "mcmc_outputs/hmc1000/n10000_c0.3_whigh/hmc")
)[[cell]]

dir.create(cfg$data_dir, recursive = TRUE, showWarnings = FALSE)

# Generate 1000 replicate datasets at this cell
run_simulations(
  out_dir = cfg$data_dir,
  n_obs_vec = cfg$n_obs,
  censoring_props = 0.3,
  weight_types = "high",
  n_replicates = 1000L,
  n_replicates_hmc = 1000L,
  samplers = "hmc"
)

# Fit HMC on all of them
t0 <- Sys.time()
fit_logistic_hmc(
  sim_dir = cfg$data_dir,
  results_dir = cfg$out_dir,
  save = c("summary", "diagnostics"),
  workers = max(1L, parallel::detectCores(logical = FALSE) - 1L)
)
cat(sprintf("\nCell %s done in %.1f hours\n",
            cell, as.numeric(difftime(Sys.time(), t0, units = "hours"))))
```

- [ ] **Step 4: Run the n=2,000 cell (3 hours)**

Once Task F's MH leg has finished (free up cores), and before the n=10,000 cell:
```bash
nohup Rscript inst/scripts/09_hmc_1000_rerun.R n2000 \
  > logs/substantive_revision/hmc1000_n2000.log 2>&1 &
echo "PID: $!"
```

- [ ] **Step 5: Wait, then run the n=10,000 cell (25 hours, one overnight slot)**

After Step 4 completes:
```bash
nohup Rscript inst/scripts/09_hmc_1000_rerun.R n10000 \
  > logs/substantive_revision/hmc1000_n10000.log 2>&1 &
echo "PID: $!"
```

**Fallback (if calendar slips):** drop this cell. Per the substantive plan §4, the asymmetric-reps story is still intact at n=2,000 alone.

- [ ] **Step 6: Verify the central-cell summary matches the original 200-rep cell within MC SE**

In R:
```r
library(dplyr)
# Load 200-rep originals
orig_files <- list.files(
  "mcmc_outputs/n2000/hmc/summaries",
  pattern = "_c0\\.3_whigh_summary\\.rds$", full.names = TRUE)
new_files  <- list.files(
  "mcmc_outputs/hmc1000/n2000_c0.3_whigh/hmc/summaries",
  pattern = "_summary\\.rds$", full.names = TRUE)

agg <- function(files) {
  purrr::map_dfr(files, \(f) readRDS(f) |> dplyr::filter(variable == "beta")) |>
    summarise(bias = mean(median - (-0.5)),
              rmse = sqrt(mean((median - (-0.5))^2)),
              n = n())
}
orig <- agg(orig_files)
new  <- agg(new_files)
cat("Original (200 reps):\n"); print(orig)
cat("New (1,000 reps):\n"); print(new)
# MC SE for RMSE is roughly RMSE / sqrt(2*n) — they should agree within ~3 MC SE.
```

If they disagree dramatically (e.g. RMSE differs by > 5σ), **stop and investigate** before claiming the larger run as the headline. Usual cause: a botched seed pass or a bug in the new args.

- [ ] **Step 7: Manuscript edits**

In `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, modify all sim-table captions that say "200 replicates" to add a qualifier on the central cells (lines 436, 485, 609, 627, 665, 671, 676, 716, 722, 753, 790, 842, 853 per the cosmetic-round changelog). Example transformation:

Before:
```latex
\caption{... 200 replicates per scenario.}
```

After:
```latex
\caption{... 200 replicates per scenario; cells $n \in \{2{,}000, 10{,}000\}$ with censoring 0.3 and weight regime ``high'' use 1{,}000 replicates for HMC (200 for MH).}
```

This is also Task J — bundle both edits in one pass.

Add a new paragraph in §4 (Simulation findings) explaining the asymmetric design:
```latex
A reviewer requested a larger Monte Carlo replicate count at the central operating points to tighten the precision of the headline numbers. We complied at $n \in \{2{,}000, 10{,}000\}$ with censoring $= 0.3$ and weight regime ``high'' by running 1{,}000 HMC replicates per cell (versus 200 elsewhere). We did not match this for MH: at $n = 2{,}000$, an MH replicate takes approximately seven minutes vs HMC's thirty seconds, and 1{,}000 MH replicates at that single cell would have required approximately 25 hours of wall time, with a corresponding asymmetric cost at $n = 10{,}000$. The asymmetric design we adopted is itself an empirical demonstration of the ESS-per-second advantage that motivates the comparison.
```

- [ ] **Step 8: Commit**

```bash
git add R/simulation.R inst/scripts/09_hmc_1000_rerun.R ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "feat: HMC 1,000-rep rerun at central cells + asymmetric-design paragraph (revision Task G)"
```

---

## Task H: ESS/s reframing edits (revision plan §3 Task H)

**Compute:** none. ~1 hour of writing, anchored to numbers produced by Tasks E and G.

**Files:**
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (Abstract, end of §1 Background, new Discussion paragraph "Why ESS/s, not wall time")

- [ ] **Step 1: Rewrite the Abstract methods/results lines**

Open `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` and find the Abstract block (search for `\begin{abstract}`). Replace the wall-time-only headline sentence with an ESS/s-anchored one. Concrete edit:

Find (or similar):
```latex
HMC completed the ZIMPHIA analysis in 1.43 minutes vs MH's 31.83 minutes.
```

Replace with:
```latex
On ZIMPHIA (n = 16{,}554), HMC delivered ~72$\times$ higher effective sample size per second than MH for $\beta_{\sex}$, equivalent to 1.43 vs 31.83 minutes of wall time at matched effective sample sizes.
```

- [ ] **Step 2: Add the "anticipate the one-fit objection" paragraph at the end of §1 (Background)**

Locate the end of `\subsection{Objectives}` (around line 196). Before `\section{Methods}` insert:

```latex
\paragraph{ESS-per-second, not wall time.}
Throughout this paper we report the effective sample size per second (ESS/s) as the primary efficiency comparator. We adopt ESS/s because (i) it captures the cost-of-inference asymmetry independently of whether the model is fit once or many times --- the same per-second cost applies to a single primary fit, to bootstrap-based design-variance refits, and to sensitivity panels; (ii) it scales transparently to larger PHIA cohorts, MPHIA, SHIMS, etc., where the same model is fit at different scales; and (iii) it is the standard metric in the MCMC literature \citep{vehtari_rank-normalization_2021}. Two analyses in this paper illustrate the practical bite of this comparator: the 100-refit design-based variance correction (\S\ref{sec:design-variance}) and the 1{,}000-replicate HMC rerun at the central simulation cells (\S\ref{sec:simulation}), both of which were feasible only because HMC's per-second cost is roughly 70-fold lower than MH's on these data.
```

- [ ] **Step 3: Add the Discussion paragraph "Why ESS/s, not wall time"**

Locate `\subsection{Methodological implications}` in the Discussion (around line 1080). Append a new subsection:

```latex
\subsection{Why ESS/s, not wall time}
\label{sec:why-ess-per-sec}
A reviewer reasonably observed that the headline ESS/s comparison would matter little if the model were fit only once. We respectfully push back on this on three grounds, each grounded in this revision. First, ESS/s rather than wall time is the operative comparator in Bayesian inference: a sampler that produces 100 effective samples per second can support tasks that a sampler producing 1 ESS/s cannot, regardless of total run length. Second, the design-based variance correction we added in \S\ref{sec:design-variance} required 100 HMC refits; this was feasible in approximately 2.4 hours with HMC, but would have required approximately 50 hours with MH and was not attempted within the revision timeline. Third, the simulation revision we report at the central cells (\S\ref{sec:simulation}) was only computationally feasible asymmetrically: we increased HMC to 1{,}000 replicates per cell but could not match this for MH within reasonable compute. The 72$\times$ ESS/s advantage on ZIMPHIA is therefore not a benchmarking curiosity --- it is the reason the revision contains the analyses it does.
```

- [ ] **Step 4: Visual check — rebuild the manuscript**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/01_Manuscript"
pdflatex -interaction=nonstopmode manuscript_sn.tex >/dev/null
bibtex manuscript_sn >/dev/null
pdflatex -interaction=nonstopmode manuscript_sn.tex >/dev/null
pdflatex -interaction=nonstopmode manuscript_sn.tex 2>&1 | tail -20
```

Expected: zero new `?` (unresolved reference) markers in the log; the new paragraphs render in the PDF.

- [ ] **Step 5: Commit**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/04_Code"
git add ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "docs: ESS/s reframing in Abstract, Background, Discussion (revision Task H)"
```

---

## Task I: Constant-incidence Discussion paragraph (revision plan §3 Task I)

**Files:**
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (one new paragraph + reframe §5.3 around line 1077)

- [ ] **Step 1: Add the dedicated paragraph**

In `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, locate `\subsection{Interpretation and context}` (around line 1061). After the existing paragraphs, insert:

```latex
\subsection{Constant-incidence interpretation}
\label{sec:constant-incidence}
A note of caution on the interpretation of $\alpha$ and the implied incidence (\S\ref{sec:implied-incidence}). The log-logistic AFT model summarises the entire retrospective window from sexual debut to survey under a single hazard function; the fitted $\alpha$ is therefore best interpreted as an average hazard over that window rather than a point-in-time incidence rate. Under the monotonically declining true incidence that has characterised Zimbabwe over the period covered by the retrospective window, the model-implied number is biased high relative to current incidence and biased low relative to the historic peak. The headline interpretation is therefore cumulative risk over the window, not current incidence. The birth-cohort sensitivity analysis (\S\ref{sec:cohort} and Appendix Figure~\ref{fig:cohort-forest}) quantifies the magnitude of this assumption.
```

- [ ] **Step 2: Reframe §5.3 marginal-sex-effect wording**

Find `\subsection{Application insights: ZIMPHIA 2020}` (around line 1073). Locate the sentence that currently describes $\beta_{\sex}$ as a "marginal sex effect" (around line 1077). Replace:
```latex
... marginal sex effect ...
```
with:
```latex
... the time-ratio for sex averaged over the retrospective interval ...
```

- [ ] **Step 3: Commit**

```bash
git add ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "docs: constant-incidence Discussion paragraph + §5.3 reframe (revision Task I)"
```

---

## Task J: "200 replicates" wording sweep (revision plan §3 Task J)

> If Task G's manuscript edits already swept these captions, this task is a no-op verification step. Otherwise it is a 15-minute sweep.

**Files:**
- Modify: `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` (lines 436, 485, 609, 627, 665, 671, 676, 716, 722, 753, 790, 842, 853)

- [ ] **Step 1: Find all remaining "200 replicates" mentions**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project"
grep -n "200 replicates" 10_BMC_Submission/01_Manuscript/manuscript_sn.tex
```

- [ ] **Step 2: For each match, decide whether the qualifier is needed**

- If the caption refers to a table or figure including the central cells (n=2,000 or n=10,000 at cens=0.3, weight high) → append the qualifier from Task G Step 7.
- If the caption is for a different scenario set (e.g., n=200 cells or other censoring/weight combinations) → leave it as "200 replicates" but consider rewording to "200 replicates (per cell)".

- [ ] **Step 3: Visual diff before committing**

```bash
git diff 10_BMC_Submission/01_Manuscript/manuscript_sn.tex | head -100
```

Verify every changed caption is consistent (same qualifier wording, same comma/semicolon style).

- [ ] **Step 4: Rebuild the manuscript to catch broken captions**

```bash
cd 10_BMC_Submission/01_Manuscript
pdflatex -interaction=nonstopmode manuscript_sn.tex 2>&1 | grep -i "Warning\|Error" | head -20
```

Expected: only the pre-existing layout warnings (3 from the cosmetic round); no new errors.

- [ ] **Step 5: Commit**

```bash
cd ../../04_Code
git add ../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex
git commit -m "docs: sweep '200 replicates' wording for asymmetric reps in central cells (revision Task J)"
```

---

## Task K: Response letter draft (revision plan §3 Task K)

**Files:**
- Create: `../../10_BMC_Submission/04_Cover_Letter/response_letter.md`

- [ ] **Step 1: Identify the source of reviewer comments**

The strategic plan refers to `../../10_BMC_Submission/revision_plan.md` §2 as the source of reviewer comments. Read the table now and capture every row:

```bash
grep -n "^| " "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/revision_plan.md" | head -80
```

- [ ] **Step 2: Draft the response letter**

Create `../../10_BMC_Submission/04_Cover_Letter/response_letter.md`:

```markdown
# Response to Reviewers — Manuscript [BMC ID]

Dear Editors and Reviewers,

We thank the editorial team and the two reviewers for the careful and constructive engagement with our manuscript. The revision has materially strengthened the paper, and we are grateful for the opportunity to resubmit. In this round we have:

- Added a Weibull misspecification scenario at the central simulation cell (Task F);
- Computed a design-based variance correction using the 100 ZIMPHIA jackknife replicate weights (Task E);
- Derived the model-implied incidence both as an age-specific curve and as a population-level number, with explicit interpretation as an average over the retrospective window (Tasks D, I);
- Expanded the ZIMPHIA design description (two-stage stratified, PSU/strata, weighting) and added a birth-cohort stratified sensitivity analysis (Tasks C, E);
- Replaced the wall-time-only efficiency framing with an ESS/s framing in the Abstract, Background, and Discussion (Task H);
- Audited the actual JAGS sampler assignments and (where relevant) rewrote Appendix Algorithm 4 to match what JAGS actually does (Task A).

We address each reviewer's comments in turn.

---

## Reviewer 1

### R1.1 [verbatim comment text]
**Response.** [1–3 sentences stating the change.]
**Manuscript pointer.** §X.Y, lines L1–L2.

### R1.2 [...]
[...]

---

## Reviewer 2

### R2.1 [verbatim comment text]
**Response.** [1–3 sentences stating the change.]
**Manuscript pointer.** §X.Y, lines L1–L2.

### R2.2 — "the model is fit only once, so efficiency does not matter"
**Response.** We respectfully push back on this on three grounds, each grounded in the present revision. First, ESS/s rather than wall time is the operative comparator in Bayesian inference: a sampler that produces 100 effective samples per second can support tasks that a sampler producing 1 ESS/s cannot, independently of whether the model is fit once or many times. Second, the design-based variance correction we added in this revision (§\ref{sec:design-variance}) required 100 HMC refits; this was feasible in approximately 2.4 hours with HMC, but would have required approximately 50 hours with MH and was not attempted within the revision timeline. Third, the simulation revision Reviewer 2 itself requested (more replicates at central cells) was only feasible asymmetrically: we increased HMC to 1{,}000 replicates per central cell but could not match this for MH within reasonable compute. The practical asymmetry illustrated in this revision is, we suggest, the strongest answer to this comment.
**Manuscript pointer.** §\ref{sec:why-ess-per-sec} (new Discussion subsection); §\ref{sec:design-variance}; §\ref{sec:simulation} central-cell paragraph.

[...]

---

## Closing

We have implemented the full set of substantive revisions requested, along with internal consistency checks (Algorithm 4 rewrite, "200 replicates" wording sweep) we identified during the revision. The code and data accompanying this submission have been versioned at commit [GIT HASH] of the project repository; all numerical results in the manuscript can be reproduced from the included R scripts.

Sincerely,
Alexander van Twisk, on behalf of the authors.
```

- [ ] **Step 3: Fill in per-comment rebuttals**

For each row of `10_BMC_Submission/revision_plan.md` §2, fill in:
- The verbatim reviewer comment.
- A 1–3 sentence response.
- The (post-revision) manuscript section and approximate line range.

Use the asymmetry-as-feature passage **verbatim** for R2's "one-fit" comment; lead with what was *added* (not what was contested) for every R1 pushback.

- [ ] **Step 4: Verify every reviewer comment is addressed**

```bash
# Count comment-headings in the response letter
grep -c "^### " "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/04_Cover_Letter/response_letter.md"
# Count rows in the revision-plan §2 table
awk '/^## 2\./{flag=1; next} /^## 3\./{flag=0} flag && /^\| [^-|]/{print}' "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/revision_plan.md" | wc -l
```

The two counts should match (modulo header rows in the table).

- [ ] **Step 5: Commit**

```bash
git add ../../10_BMC_Submission/04_Cover_Letter/response_letter.md
git commit -m "docs: response-to-reviewers letter (revision Task K)"
```

---

## Task 12: Final verification

**Files:**
- All

- [ ] **Step 1: Run the test suite**

```r
devtools::test()
```

Expected: all tests pass; no warnings.

- [ ] **Step 2: Run R CMD check (light mode)**

```r
devtools::check(args = c("--no-manual", "--as-cran"))
```

Expected: 0 errors, 0 warnings, ≤ 1 note (existing note about non-standard files like `mcmc_outputs/` is acceptable; verify it is not a new note).

- [ ] **Step 3: Final LaTeX rebuild**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/01_Manuscript"
pdflatex -interaction=nonstopmode manuscript_sn.tex >/dev/null
bibtex manuscript_sn >/dev/null
pdflatex -interaction=nonstopmode manuscript_sn.tex >/dev/null
pdflatex -interaction=nonstopmode manuscript_sn.tex 2>&1 | tail -40
```

Verify against the cosmetic-round baseline:
- Expected warnings: 3 (pre-existing layout); no new ones.
- Zero new undefined references.
- Zero `?` in the rendered PDF.

- [ ] **Step 4: Cross-reference sweep**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/01_Manuscript"
grep -n "\\\\ref{eq:aft-density}\|\\\\ref{tab:zimphia-performance}\|\\\\ref{tab:sim-rmse-beta}\|\\\\ref{fig:incidence}\|\\\\ref{sec:design-variance}\|\\\\ref{sec:misspec}\|\\\\ref{sec:why-ess-per-sec}\|\\\\ref{sec:cohort}\|\\\\ref{sec:constant-incidence}" manuscript_sn.tex
```

For each match, confirm the target label exists in the file:
```bash
for label in eq:aft-density tab:zimphia-performance tab:sim-rmse-beta fig:incidence sec:design-variance sec:misspec sec:why-ess-per-sec sec:cohort sec:constant-incidence; do
  count=$(grep -c "\\\\label{$label}" manuscript_sn.tex)
  echo "$label: $count definition(s)"
done
```

Expected: every label has exactly 1 definition.

- [ ] **Step 5: Response-letter coverage check**

```bash
# Every row in revision_plan.md §2 must appear in response_letter.md
diff \
  <(awk '/^## 2\./{flag=1; next} /^## 3\./{flag=0} flag && /^\| [^-|]/{print $2}' \
    "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/revision_plan.md" \
    | sort -u) \
  <(grep "^### " "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/04_Cover_Letter/response_letter.md" \
    | sort -u)
```

- [ ] **Step 6: Numeric round-trip — central-cell HMC**

Already done in Task G Step 6, but re-verify with the final summaries:
```r
# Compare original 200-rep and new 1,000-rep central-cell summaries for beta_sex
# (script lifted from Task G Step 6)
```

Expected: medians within ≤ 3× MC SE.

- [ ] **Step 7: Internal consistency — re-render and grep for stale numbers**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/01_Manuscript"
# Hunt for orphan XXX/YYY/FF/LO/HI placeholders that survived the writeup
grep -nE "(XXX|YYY|ZZZ|WWW|FF|\[LO|\[HI|\\[If [^]]+\\])" manuscript_sn.tex
```

Expected: empty. If any survive, **fix before final commit**.

- [ ] **Step 8: Commit the verification artefacts**

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/04_Code"
# Anything that changed during verification (mostly LaTeX aux/log/pdf which we don't track)
git status
git add -p   # interactively review and stage real changes
git commit -m "chore: post-verification cleanup" || echo "Nothing to commit"
```

---

## Task 13: Bump version, write the changelog entry, tag, and merge

**Files:**
- Modify: `DESCRIPTION` (version bump)
- Modify: `NEWS.md` (or create if absent)

- [ ] **Step 1: Bump the package version**

In `DESCRIPTION`, find `Version: 0.1.0` and change to `Version: 0.2.0`.

- [ ] **Step 2: Write a NEWS.md entry**

If `NEWS.md` does not exist, create it:
```markdown
# bayesianICSimulations 0.2.0 (2026-05-XX)

Substantive revision in response to BMC reviewer feedback. New features and analyses:

- JAGS sampler audit utility (`audit_jags_samplers()`).
- Multivariable ZIMPHIA HMC fit (`fit_zimphia_multivariable()`) and generalised vector-beta Stan model.
- Birth-cohort stratified ZIMPHIA fits (`fit_zimphia_cohort()`).
- Implied age-specific hazard and population-incidence post-processing (`compute_age_specific_hazard()`, `compute_population_incidence()`).
- Design-based variance via ZIMPHIA replicate weights (`fit_zimphia_design_replicates()`).
- Weibull misspecification simulation (`simulate_survival_data_weibull()`, `run_misspec_simulation()`).
- Asymmetric replicate-count support in `run_simulations()` (`n_replicates_hmc`, `samplers`).
- Test suite scaffolded with testthat 3e; unit tests for pure post-processing helpers.

Manuscript revisions accompany these features; see `tasks/revision_plan_substantive.md` for the full task list.
```

- [ ] **Step 3: Final test + check + document pass**

```r
devtools::document()
devtools::test()
devtools::check(args = c("--no-manual"))
```

Expected: 0 errors, 0 warnings.

- [ ] **Step 4: Commit the version bump and tag the branch**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore: bump version to 0.2.0 with substantive-revision changelog"
git tag -a v0.2.0-bmc-revision -m "BMC substantive revision (Tasks A-K)"
```

- [ ] **Step 5: Merge or open PR**

If integrating to main:
```bash
git checkout main
git merge --no-ff substantive-revision -m "Merge substantive-revision into main: BMC R&R round 2"
git log --oneline -10
```

Do **not** force-push to main or delete the branch before reviewing the diff.

---

## Sequencing summary (calendar)

This is identical to the substantive plan §4 calendar; reproduced here so the executor can pin tasks to days. All overnight slots **must** be exclusive — never run Task F's MH leg and Task G's n=10,000 leg on the same overnight.

| Day | Morning | Afternoon | Overnight (unattended) |
|---|---|---|---|
| **Day 1** | Task 0 scaffold (30 min); Task A (30 min); Task B Stan generalisation + multivariable fit (1.5h) | Task C cohort fits (10 min) + writeup (1h); Task D incidence postprocessing (1h) | Kick off Task F MH 200-rep misspec sim (~25h running through Day 2 and into Day 3) |
| **Day 2** | Task E design-variance: load replicate weights, fit loop (~2.4h) | Task F HMC misspec sim (~1.5h); write Methods §3 ZIMPHIA-design expansion (2h) | Kick off Task G `n=2,000` HMC 1,000-rep cell (~3h overnight) |
| **Day 3** | Task G `n=10,000` HMC 1,000-rep cell kickoff (kick off morning; ~25h, finishes Day 4 morning) | Write §4 Results: multivariable, birth-cohort, incidence, design-based variance (3h); Task A → Algorithm 4 rewrite if needed (1h) | Task G `n=10,000` continues |
| **Day 4** | Task G results in; Task J wording sweep; assemble Tasks H + I (3h) | Task K response letter (2h); Task 12 verification + LaTeX rebuild (1h); Task 13 version bump and tag | — |

**Risk register reminder** (verbatim from substantive plan §7):

| Risk | Likelihood | Mitigation |
|---|---|---|
| Task A reveals JAGS used neither slice nor RWM | Low | Document whatever it did use; the manuscript matches reality regardless |
| Task F shows HMC advantage shrinks under Weibull | Low | Report transparently; ordering still holds |
| Design-based CrI (E) much wider than model-based | Medium | Report design-based as primary; honest |
| Task G `n=10,000` doesn't complete in time | Medium-High | Drop to n=2,000-only for asymmetric story |
| Laptop crash mid-sim | Low | Per-rep `.rds` files saved continuously |
| Multivariable fit (B) shifts β_sex materially | Medium | Report transparently; sampler conclusions unaffected |

---

*Executable plan generated 2026-05-11 from `tasks/revision_plan_substantive.md`. Every task above corresponds 1:1 to the strategic spec; the bite-sized steps are mine. If a task's strategic intent changes, edit the substantive plan first and re-issue this executable plan rather than drifting silently.*
