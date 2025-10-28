# HPC Execution Guide

This guide explains how to run the Bayesian MCMC simulations (HMC and MH) on the Stellenbosch University HPC cluster.

## Overview

You have 3,600 simulated datasets (1,200 per sample size: n=200, n=2000, n=10000). Each dataset will be processed by both HMC (Stan) and MH (JAGS) models sequentially. The jobs use PBS array jobs to parallelize across datasets.

**Total computational work:**
- 3,600 datasets × 2 models = 7,200 model fits
- Each fit runs 4 MCMC chains with 1,000 warmup + 5,000 sampling iterations

## File Structure

```
HPC/
├── run_fits.R                    # Main R script (processes 1 dataset with both models)
├── setup_hpc.sh                  # One-time environment setup
├── loglogistic_interval.stan     # Stan model definition
├── loglogistic_interval.jags     # JAGS model definition
├── submit_all.pbs                # PBS script for ALL 3600 datasets (Option A - Recommended)
├── submit_n200.pbs               # PBS script for n=200 (1200 tasks, Option B)
├── submit_n2000.pbs              # PBS script for n=2000 (1200 tasks, Option B)
├── submit_n10000.pbs             # PBS script for n=10000 (1200 tasks, Option B)
├── sim_data/
│   ├── n200/                     # 1200 .rds files (8KB each)
│   ├── n2000/                    # 1200 .rds files (72KB each)
│   └── n10000/                   # 1200 .rds files (344KB each)
├── results/                      # Created by jobs
│   ├── n200/{hmc,mh}/{summaries,diagnostics}/
│   ├── n2000/{hmc,mh}/{summaries,diagnostics}/
│   └── n10000/{hmc,mh}/{summaries,diagnostics}/
└── logs/                         # Job output logs
```

## Step-by-Step Instructions

### 1. Upload Files to HPC

Transfer this entire directory to your HPC home directory:

```bash
# From your local machine
scp -r HPC/ 22565361@hpc1.sun.ac.za:~/
```

Or use an SFTP client like FileZilla.

### 2. Connect to HPC

```bash
ssh 22565361@hpc1.sun.ac.za
cd ~/HPC
```

### 3. Run Setup (One Time Only)

```bash
bash setup_hpc.sh
```

This script will:
- Load required modules (R, gcc, JAGS)
- Create results and logs directories
- Install R packages (cmdstanr, rjags, coda, posterior, tibble)
- Install CmdStan (~10 min)
- Pre-compile the Stan model (~2 min)

**Expected time:** 15-20 minutes

### 4. Submit Jobs

You have two options for submitting jobs:

#### Option A: Single Master Job (Simplest - Recommended)

Submit all 3600 datasets as one array job:

```bash
qsub submit_all.pbs
```

This creates one job array with 3600 tasks that automatically routes each dataset to the correct sample size.

#### Option B: Separate Jobs by Sample Size

Submit three separate array jobs:

```bash
qsub submit_n200.pbs    # 1200 tasks for n=200
qsub submit_n2000.pbs   # 1200 tasks for n=2000
qsub submit_n10000.pbs  # 1200 tasks for n=10000
```

This gives you more granular control and monitoring per sample size.

You'll receive email notifications (to 22565361@sun.ac.za) when jobs:
- Start (Begin)
- Complete (End)
- Abort/Error (Abort)

### 5. Monitor Jobs

Check job status:

```bash
# View all your jobs
qstat -u <username>

# View specific job array
qstat -t <job_id>

# Count running/queued tasks
qstat -u <username> | grep "hmc_mh" | wc -l
```

Check logs in real-time:

```bash
# View latest log
tail -f logs/n200_1.out

# Check for errors
grep -i error logs/*.err

# Count completed tasks
ls results/n200/hmc/summaries/*.rds | wc -l
```

### 6. Resource Allocation Summary

#### Individual Job Approach (Option B)

| Sample Size | Tasks | Cores/Task | RAM/Task | Walltime | Est. Completion |
|-------------|-------|------------|----------|----------|-----------------|
| n=200       | 1200  | 4          | 8 GB     | 12 hr    | ~10 min/task    |
| n=2000      | 1200  | 4          | 16 GB    | 48 hr    | ~45 min/task    |
| n=10000     | 1200  | 4          | 32 GB    | 96 hr    | ~2-3 hr/task    |

**Note:** Conservative resource allocation - no practical limits on this HPC

#### Master Job Approach (Option A)

| Job | Tasks | Cores/Task | RAM/Task | Walltime | Est. Completion |
|-----|-------|------------|----------|----------|-----------------|
| All | 3600  | 4          | 32 GB    | 96 hr    | 10 min - 3 hr/task |

**Total core-hours:** ~12,000-16,000 (depends on actual runtime)

**Expected wall-clock time:** If resources are available:
- n=200 tasks: Complete in ~10-20 minutes each
- n=2000 tasks: Complete in ~30-60 minutes each
- n=10000 tasks: Complete in ~2-4 hours each
- **All tasks in parallel:** Can complete in ~3-4 hours total if sufficient nodes available

## Output Files

For each dataset `sim_s001_r001_n0200_c0.1_whigh.rds`, you'll get:

### HMC Outputs
- `results/n200/hmc/summaries/sim_s001_r001_n0200_c0.1_whigh_summary.rds`
  - Posterior summaries: mean, median, sd, q2.5, q97.5, rhat, ess
- `results/n200/hmc/diagnostics/sim_s001_r001_n0200_c0.1_whigh_diag.rds`
  - Diagnostics: max_rhat, min_ess, divergences, max_treedepth_hit, timing

### MH Outputs
- `results/n200/mh/summaries/sim_s001_r001_n0200_c0.1_whigh_summary.rds`
  - Posterior summaries: mean, median, sd, q2.5, q97.5, rhat, ess
- `results/n200/mh/diagnostics/sim_s001_r001_n0200_c0.1_whigh_diag.rds`
  - Diagnostics: max_rhat, min_ess, timing

**Note:** Full fit objects and posterior draws are NOT saved to conserve disk space. Only summaries and diagnostics are retained.

## Troubleshooting

### Job Fails with "Module not found"

Check available modules on your HPC:

```bash
module avail R
module avail gcc
```

**Note:** JAGS is not available as a module on this HPC. The `setup_hpc.sh` script automatically compiles JAGS from source and installs it to `~/local/jags`.

### "Package not found" Error

Ensure setup completed successfully:

```bash
# Check installed packages
Rscript -e '.libPaths(); library(cmdstanr); library(rjags)'
```

If missing, re-run `bash setup_hpc.sh`

### Stan Model Compilation Errors

Stan requires gcc/g++ 11+:

```bash
module load gcc/11.2.0
```

Check CmdStan installation:

```bash
Rscript -e 'library(cmdstanr); cmdstan_version(); cmdstan_path()'
```

### JAGS Not Found or Library Errors

JAGS is automatically compiled from source during `setup_hpc.sh` because it's not available as a module.

**If you see JAGS-related errors:**

1. Check if JAGS was installed correctly:
```bash
ls -la ~/local/jags/
which jags
jags --version
```

2. Verify `rjags` can find JAGS:
```bash
Rscript -e 'Sys.setenv(PATH=paste0(Sys.getenv("HOME"),"/local/jags/bin:",Sys.getenv("PATH"))); Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("HOME"),"/local/jags/lib:",Sys.getenv("LD_LIBRARY_PATH"))); library(rjags); cat(jags.version())'
```

3. If JAGS installation failed during setup, manually compile:
```bash
cd ~/software
wget https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/JAGS-4.3.2.tar.gz
tar -xzf JAGS-4.3.2.tar.gz
cd JAGS-4.3.2
module load app/gcc/gcc-13.2.0
./configure --prefix=$HOME/local/jags
make -j4
make install
```

4. Then reinstall the `rjags` R package:
```bash
export PATH="$HOME/local/jags/bin:$PATH"
export LD_LIBRARY_PATH="$HOME/local/jags/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$HOME/local/jags/lib/pkgconfig:$PKG_CONFIG_PATH"
Rscript -e 'install.packages("rjags", repos="https://cloud.r-project.org", lib="~/R_libs")'
```

### Array Task Failed

Check specific task log:

```bash
cat logs/n200_<task_id>.err
```

Re-run a single dataset locally for debugging:

```bash
Rscript run_fits.R 1 200
```

### Out of Memory Errors

For n=10000, if 16GB is insufficient, increase to 32GB:

```bash
#PBS -l select=1:ncpus=4:mem=32gb
```

### Job Exceeds Walltime

If jobs timeout, increase walltime or reduce MCMC iterations in [run_fits.R](run_fits.R):
- Reduce `iter_sampling` from 5000 to 2500
- Reduce `iter_warmup` from 1000 to 500

## Advanced Usage

### Submit Only a Subset of Tasks

Test with first 10 datasets:

```bash
qsub -J 1-10 submit_n200.pbs
```

### Rerun Failed Tasks Only

After checking logs, rerun specific indices:

```bash
qsub -J 42,105,237 submit_n200.pbs
```

### Monitor Progress Programmatically

Count completed fits:

```bash
#!/bin/bash
for size in 200 2000 10000; do
  hmc_count=$(ls results/n${size}/hmc/summaries/*.rds 2>/dev/null | wc -l)
  mh_count=$(ls results/n${size}/mh/summaries/*.rds 2>/dev/null | wc -l)
  echo "n=${size}: HMC ${hmc_count}/1200, MH ${mh_count}/1200"
done
```

### Aggregate Results After Completion

Download results to local machine:

```bash
# From local machine
scp -r <username>@hpc1.sun.ac.za:~/HPC/results ./
```

Combine summaries in R:

```r
library(tidyverse)

# Load all HMC summaries for n=200
hmc_files <- list.files("results/n200/hmc/summaries", full.names = TRUE)
hmc_summaries <- map_dfr(hmc_files, readRDS, .id = "file_id")

# Load diagnostics
diag_files <- list.files("results/n200/hmc/diagnostics", full.names = TRUE)
hmc_diags <- map_dfr(diag_files, readRDS)

# Check convergence
mean(hmc_diags$max_rhat < 1.01)  # Proportion with good convergence
```

## Estimated Costs

**Total SU (Service Units):** ~48,000 SU
- n=200: 1200 tasks × 4 cores × 0.17 hr = 800 SU
- n=2000: 1200 tasks × 4 cores × 0.75 hr = 3,600 SU
- n=10000: 1200 tasks × 4 cores × 2.5 hr = 12,000 SU

**Total:** ~16,400 SU (conservative; actual usage likely lower)

## Contact

For HPC-specific issues:
- HPC Support: hpc-support@sun.ac.za
- User Guide: https://www0.sun.ac.za/hpc

For code/model issues:
- Email: 22565361@sun.ac.za

## Next Steps After Jobs Complete

1. Download results directory
2. Check convergence diagnostics (Rhat < 1.01, ESS > 400)
3. Aggregate summaries across all datasets
4. Compare HMC vs MH performance (timing, ESS per second, convergence)
5. Analyze posterior estimates for simulation study
