# bayesianICSimulations

**Comparing Bayesian MCMC methods for interval-censored survival data**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://www.r-project.org/)

An R package for the MSc Biostatistics research project at Stellenbosch University comparing **Hamiltonian Monte Carlo (HMC)** via Stan and **Metropolis-Hastings (MH)** via JAGS for fitting log-logistic AFT models to interval-censored survival data with survey weights.

---

## Research Context

This package implements a simulation study following the **ADEMP framework** (Morris et al., 2019) across:

- **5,400 datasets** (1,800 per sample size: n = 200, 2,000, 10,000)
- **3 scenarios** varying censoring rates and weight distributions
- **2 MCMC methods**: HMC (Stan) and Metropolis-Hastings (JAGS)
- **10,800 total model fits** executed on an HPC cluster (PBS job arrays)

### Statistical Model

Log-logistic AFT model with interval censoring and optional survey weights:

```
log(λᵢ) = log(α) + β × Xᵢ

Priors:
  α ~ Lognormal(log(5), 1)
  β ~ Normal(0, 1)
  γ ~ Lognormal(0, 0.5)
```

---

## Installation

### Prerequisites

- **R** ≥ 4.0.0
- **CmdStan** (for HMC): installed via `cmdstanr::install_cmdstan()`
- **JAGS** (for MH): `brew install jags` / `sudo apt-get install jags`

### Install Package

```r
# From GitHub
devtools::install_github("alexvantwisk/bayesianICSimulations")

# Install CmdStan
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()

# Install rjags
install.packages("rjags")
```

---

## Quick Start

### Fit Models

```r
library(bayesianICSimulations)

# HMC via Stan
fit_logistic_hmc(
  sim_dir    = "sim_data/n200",
  results_dir = "mcmc_outputs/hmc_n200"
)

# Metropolis-Hastings via JAGS
fit_logistic_mh(
  sim_dir    = "sim_data/n200",
  results_dir = "mcmc_outputs/mh_n200"
)
```

### Analysis Pipeline

```r
# 1. Combine all results
results <- combine_results(
  results_dir = "mcmc_outputs",
  data_dir    = "outputs/combined_results"
)

# 2. Statistical analysis (bias, RMSE, coverage, MCSEs)
analysis <- perform_statistical_analysis(
  data_dir   = "outputs/combined_results",
  output_dir = "outputs/analysis"
)

# 3. Generate all 13 publication-ready figures
plots <- save_all_figures(
  data_dir   = "outputs/combined_results",
  output_dir = "outputs/figures",
  dpi        = 320
)
```

Or run as batch scripts:

```bash
Rscript inst/scripts/01_combine_results.R
Rscript inst/scripts/02_analysis.R
Rscript inst/scripts/03_visualization.R
```

---

## HPC Workflow (PBS Cluster)

### One-Time Setup

```bash
bash inst/hpc/setup_hpc.sh   # Installs JAGS from source, CmdStan, R packages
```

### Submit Jobs

```bash
qsub inst/hpc/submit_all.pbs       # All 5,400 datasets
qsub inst/hpc/submit_n200.pbs      # n=200 only (1,800 datasets)
qsub inst/hpc/submit_n2000.pbs     # n=2,000 only
qsub inst/hpc/submit_n10000.pbs    # n=10,000 only
```

### Monitor & Rerun

```bash
qstat -u $USER                               # Check job status
qsub -J 42,105,237 inst/hpc/submit_n200.pbs  # Rerun specific failed tasks
```

---

## Package Structure

```
bayesianICSimulations/
├── R/
│   ├── fitting_hmc.R          # fit_logistic_hmc()
│   ├── fitting_mh.R           # fit_logistic_mh()
│   ├── combine_results.R      # combine_results()
│   ├── analysis_pipeline.R    # perform_statistical_analysis()
│   ├── analysis_mcse.R        # MCSE calculations
│   ├── analysis_stats.R       # Statistical tests
│   ├── figures.R              # 13 figure functions
│   ├── tables.R               # Table generation
│   ├── viz_survival.R         # Survival curve visualisation
│   └── utils.R                # Internal helpers
│
├── inst/
│   ├── models/
│   │   ├── loglogistic_interval.stan   # Stan model
│   │   └── loglogistic_interval.jags   # JAGS model
│   ├── scripts/
│   │   ├── 01_combine_results.R
│   │   ├── 02_analysis.R
│   │   ├── 03_visualization.R
│   │   └── run_fits.R                  # PBS array job entry point
│   └── hpc/
│       ├── setup_hpc.sh
│       ├── submit_all.pbs
│       └── submit_n{200,2000,10000}.pbs
│
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## Key Implementation Details

| Aspect | HMC (Stan) | MH (JAGS) |
|--------|-----------|-----------|
| Likelihood | Direct `log_diff_exp` | Zeros trick (`dpois`) |
| Right-censoring | Native `is_inf(R[i])` | Sentinel `R[i] > 1e10` |
| Chains | 4 × (1000 warmup + 5000 samples) | 4 × (1000 adapt + 1000 burnin + 5000 samples) |
| Seed strategy | Fixed (2025) across all datasets | Unique per dataset (2025 + index) |
| Divergence tracking | Yes | No |

Both methods normalize survey weights to sum to N: `wᵢ* = wᵢ × (N / Σwᵢ)`

---

## Citation

```bibtex
@misc{vantwisk2025,
  author  = {van Twisk, Alexander},
  title   = {{bayesianICSimulations}: Bayesian Interval-Censored Survival
             Data Simulation and Analysis},
  year    = {2025},
  note    = {R package version 1.0.0. MSc Biostatistics Dissertation,
             Stellenbosch University},
  url     = {https://github.com/alexvantwisk/bayesianICSimulations}
}
```

---

## License

MIT — see [LICENSE](LICENSE) for details.

**Author**: Alexander van Twisk, MSc Biostatistics, Stellenbosch University
