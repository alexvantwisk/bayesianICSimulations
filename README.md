# bayesianICSimulations

**Bayesian Interval-Censored Survival Data Simulation and Analysis**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

`bayesianICSimulations` is an R package for comparing Bayesian MCMC methods for fitting log-logistic accelerated failure time (AFT) models to interval-censored survival data with survey weights. Designed for MSc research in Biostatistics, it implements:

- **Hamiltonian Monte Carlo (HMC)** via Stan/CmdStanR
- **Metropolis-Hastings (MH)** via JAGS

The package provides tools for large-scale simulation studies (5,400+ datasets), parallel HPC execution, comprehensive ADEMP-framework analysis, and publication-ready visualization.

## Features

### Core Functionality

- **Model Fitting**: Parallel MCMC fitting with standardized outputs
- **Result Loading**: Parse and combine thousands of simulation results
- **Statistical Analysis**: Monte Carlo standard errors, bias, RMSE, coverage
- **Visualization**: Ghosted spaghetti plots for weighted interval-censored survival curves
- **HPC Support**: PBS job array scripts and batchtools integration

### Statistical Model

Log-logistic AFT model with interval censoring and survey weights:

```
log(λ_i) = log(α) + β × X_i

Priors:
  α ~ Lognormal(log(5), 1)
  β ~ Normal(0, 1)
  γ ~ Lognormal(0, 0.5)
```

## Installation

### Prerequisites

1. **R** (≥ 4.0.0)
2. **CmdStanR** (for HMC) - Install from: https://mc-stan.org/r-packages/
3. **JAGS** (for MH) - Install from: https://mcmc-jags.sourceforge.io/

### Install Package

```r
# Install from local directory
devtools::install()

# Or from GitHub (if hosted)
# devtools::install_github("username/bayesianICSimulations")
```

### Install CmdStanR

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
```

### Install JAGS

- **macOS**: `brew install jags`
- **Linux**: `sudo apt-get install jags` or compile from source
- **Windows**: Download installer from https://mcmc-jags.sourceforge.io/

Then install the R package:

```r
install.packages("rjags")
```

## Quick Start

### Model Fitting

```r
library(bayesianICSimulations)

# Fit HMC models to all datasets in a directory
hmc_results <- fit_logistic_hmc(
  sim_dir = "sim_data/n200",
  results_dir = "mcmc_outputs/hmc_n200"
)

# Fit MH models
mh_results <- fit_logistic_mh(
  sim_dir = "sim_data/n200",
  results_dir = "mcmc_outputs/mh_n200"
)
```

### Load and Combine Results

```r
# Load all results for analysis
data <- combine_results(
  methods = c("hmc", "mh"),
  sample_sizes = c(200, 2000, 10000),
  results_dir = "results"
)

# Access components
summaries <- data$summaries
diagnostics <- data$diagnostics
metadata <- data$scenario_metadata

# Calculate performance metrics
library(dplyr)
performance <- summaries %>%
  group_by(method, variable, n_obs) %>%
  summarise(
    bias = mean(error, na.rm = TRUE),
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    coverage = mean(contains_truth, na.rm = TRUE)
  )
```

### Visualization

```r
# Load simulation replicates
sims <- load_sims_from_dir(
  "sim_data/n200",
  pattern = ".*_c0\\.1_whigh\\.rds$"
)

# Fit parametric models and summarize
results <- summarise_weighted_curves(
  sims,
  weight_metric = "cv",
  include_covariate = TRUE
)

# Create ghosted spaghetti plot
plot_weighted_spaghetti(
  sum_df = results$sum_df,
  rep_df = results$rep_df,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5,
  which = "S",  # S=survival, H=cumulative hazard, h=hazard
  n_ghost = 30
)
```

## HPC Workflow

### Setup (One-Time)

```bash
# On HPC cluster
bash inst/hpc/setup_hpc.sh
```

This installs JAGS from source, CmdStan, and all R dependencies.

### Submit Jobs

```bash
# Submit all 5400 datasets
qsub inst/hpc/submit_all.pbs

# Or submit by sample size
qsub inst/hpc/submit_n200.pbs    # 1800 datasets
qsub inst/hpc/submit_n2000.pbs   # 1800 datasets
qsub inst/hpc/submit_n10000.pbs  # 1800 datasets
```

### Monitor Progress

```bash
# Check job status
qstat -u $USER

# Count completed fits
for size in n200 n2000 n10000; do
  for method in hmc mh; do
    count=$(ls mcmc_outputs/$size/$method/summaries/*.rds 2>/dev/null | wc -l)
    echo "$size/$method: $count summaries"
  done
done
```

### Rerun Failed Jobs

```bash
# Rerun specific tasks
qsub -J 1-10 inst/hpc/submit_n200.pbs          # Tasks 1-10
qsub -J 42,105,237 inst/hpc/submit_n200.pbs   # Specific tasks
```

## Analysis Pipeline

After model fits complete, you can run the analysis in two ways:

### Option 1: Run Scripts (Recommended for batch processing)

```bash
# Step 1: Combine all results
Rscript inst/scripts/01_combine_results.R

# Step 2: Statistical analysis (ADEMP framework)
Rscript inst/scripts/02_analysis.R

# Step 3: Generate publication figures
Rscript inst/scripts/03_visualization.R
```

### Option 2: Use Package Functions (Recommended for interactive analysis)

```r
library(bayesianICSimulations)

# Step 1: Combine results
results <- combine_results(
  results_dir = "mcmc_outputs",
  data_dir = "data",
  verbose = TRUE
)

# Step 2: Perform statistical analysis
analysis <- perform_statistical_analysis(
  data_dir = "data",
  output_dir = "outputs/analysis",
  verbose = TRUE
)

# Step 3: Generate all figures
plots <- save_all_figures(
  data_dir = "data",
  output_dir = "outputs/figures",
  dpi = 320
)

# Or create individual figures
fig2 <- create_figure2_coverage(prepare_plot_data("data"))
fig2 # Display in RStudio
# Customize and save
ggsave("my_custom_coverage.png", fig2, width = 10, height = 6)
```

## Package Structure

```
bayesianICSimulations/
├── R/                          # Package functions
│   ├── fitting_hmc.R           # HMC model fitting (fit_logistic_hmc)
│   ├── fitting_mh.R            # MH model fitting (fit_logistic_mh)
│   ├── combine_results.R       # Data combination pipeline
│   ├── analysis_pipeline.R     # Statistical analysis (perform_statistical_analysis)
│   ├── figures.R               # All 13 figure creation functions
│   ├── data_loading.R          # Result loading and parsing
│   ├── analysis_mcse.R         # MCSE calculations
│   ├── analysis_stats.R        # Statistical tests
│   ├── viz_survival.R          # Survival visualization
│   └── viz_helpers.R           # Internal helpers
│
├── inst/
│   ├── models/                 # Stan and JAGS models
│   │   ├── loglogistic_interval.stan
│   │   └── loglogistic_interval.jags
│   ├── scripts/                # Convenience wrapper scripts
│   │   ├── 01_combine_results.R      # Calls combine_results()
│   │   ├── 02_analysis.R             # Calls perform_statistical_analysis()
│   │   ├── 03_visualization.R        # Calls save_all_figures()
│   │   ├── run_fits.R
│   │   └── example_usage.R
│   ├── hpc/                    # HPC submission scripts
│   │   ├── setup_hpc.sh
│   │   ├── submit_all.pbs
│   │   └── submit_n*.pbs
│   └── templates/              # Configuration templates
│       ├── batchtools.pbs.tmpl
│       └── batchtools.conf.R
│
├── man/                        # Documentation (auto-generated)
├── data-raw/                   # Data generation scripts
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Export declarations (auto-generated)
└── README.md                   # This file
```

## Function Reference

### Model Fitting

- `fit_logistic_hmc()` - Fit log-logistic AFT models using HMC
- `fit_logistic_mh()` - Fit log-logistic AFT models using MH
- `compute_derived_quantities_mh()` - Calculate derived quantities from MH samples

### Data Loading

- `parse_filename()` - Parse metadata from simulation filenames
- `load_summary_file()` - Load single summary file with metadata
- `load_summaries()` - Load all summaries for method/sample size
- `load_diagnostics()` - Load all diagnostics for method/sample size
- `combine_results()` - High-level wrapper to combine all results

### Statistical Analysis

- `mcse_mean()` - Monte Carlo standard error for means
- `mcse_prop()` - Monte Carlo standard error for proportions
- `calc_bias_rmse()` - Calculate bias, RMSE, and their MCSEs
- `cohen_d()` - Calculate Cohen's d effect size

### Visualization

- `load_sims_from_dir()` - Load simulation replicates from directory
- `summarise_weighted_curves()` - Fit parametric models and compute summaries
- `plot_weighted_spaghetti()` - Create ghosted spaghetti plots
- `facet_design_panels()` - Multi-panel faceted visualization
- `compute_true_loglogistic()` - Calculate true survival/hazard curves
- `compute_marginal_survival()` - Calculate marginal survival

## Examples

See `inst/scripts/example_usage.R` for comprehensive examples including:

- Single design cell analysis
- Intercept-only models
- Multi-panel faceted plots
- Weight type comparisons

## Citation

If you use this package in your research, please cite:

```
van Twisk, A. (2025). bayesianICSimulations: Bayesian Interval-Censored
Survival Data Simulation and Analysis. R package version 0.1.0.
MSc Biostatistics Dissertation, Stellenbosch University.
```

## License

MIT License. See LICENSE file for details.

## Author

Alexander van Twisk
MSc Biostatistics, Stellenbosch University

## Acknowledgments

This package was developed as part of an MSc research project comparing MCMC
methods for interval-censored survival data analysis following the ADEMP
simulation framework (Morris et al., 2019).
