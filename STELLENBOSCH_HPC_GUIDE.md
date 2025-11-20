# Stellenbosch University HPC User Guide

This guide provides comprehensive instructions for using the Stellenbosch University High-Performance Computing (HPC) cluster. It's designed to help both humans and AI assistants work effectively with the cluster for computational research.

## Table of Contents

1. [Introduction](#introduction)
2. [First-Time Setup](#first-time-setup)
3. [PBS Job System Basics](#pbs-job-system-basics)
4. [Writing PBS Job Scripts](#writing-pbs-job-scripts)
5. [Job Arrays for Batch Processing](#job-arrays-for-batch-processing)
6. [Resource Allocation](#resource-allocation)
7. [Module System](#module-system)
8. [Installing Custom Software](#installing-custom-software)
9. [Environment Variables](#environment-variables)
10. [Monitoring and Debugging](#monitoring-and-debugging)
11. [Performance Optimization](#performance-optimization)
12. [Best Practices](#best-practices)
13. [Common Code Patterns](#common-code-patterns)
14. [Troubleshooting](#troubleshooting)

---

## Introduction

### What is the Stellenbosch HPC?

The Stellenbosch University HPC cluster is a shared computing resource that provides:
- Multiple compute nodes with many CPU cores
- Large memory capacity (up to 32+ GB per job)
- PBS (Portable Batch System) job scheduler
- Module system for loading software
- Extended walltime limits (up to 96+ hours)

### When to Use the HPC

Use the HPC for computational tasks that:
- Take hours or days to complete on a laptop
- Can be parallelized across multiple datasets/parameters
- Require more memory than available locally
- Need to run many similar tasks concurrently
- Involve computationally intensive simulations, modeling, or analysis

### When NOT to Use the HPC

Avoid the HPC for:
- Quick exploratory analysis (< 5 minutes runtime)
- Interactive development and debugging
- Tasks requiring frequent human interaction
- Small datasets that fit in laptop memory

---

## First-Time Setup

### 1. Connect to the Cluster

```bash
# SSH into the cluster
ssh your_username@hpc.sun.ac.za

# Or with specific port if required
ssh -p 22 your_username@hpc.sun.ac.za
```

### 2. Set Up Your Environment

Create a setup script that runs once to configure your environment:

```bash
#!/bin/bash
# setup.sh - Run once to configure your HPC environment

# Create standard directory structure
mkdir -p ~/projects
mkdir -p ~/R_libs
mkdir -p ~/local/bin
mkdir -p ~/local/lib

# Add to your ~/.bashrc for persistent configuration
cat >> ~/.bashrc << 'EOF'

# Custom software paths
export PATH="$HOME/local/bin:$PATH"
export LD_LIBRARY_PATH="$HOME/local/lib:$LD_LIBRARY_PATH"

# R package library
export R_LIBS_USER="$HOME/R_libs"

# Limit BLAS threading (important for parallel jobs!)
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

EOF

# Source the updated bashrc
source ~/.bashrc
```

### 3. Test Basic Functionality

```bash
# Load a module and verify
module load app/R/4.5.1
which R

# Check available resources
qstat -q  # View available queues
pbsnodes -a | grep -A 5 "state = free"  # Check free nodes
```

---

## PBS Job System Basics

### Understanding the Job Scheduler

The PBS (Portable Batch System) manages job submission and execution:
- You submit jobs to a **queue**
- The scheduler allocates resources when available
- Jobs run on **compute nodes** (not the login node!)
- Multiple jobs can run simultaneously

### Common PBS Commands

```bash
# Submit a job
qsub my_script.pbs

# Check job status
qstat                 # All jobs
qstat -u $USER       # Only your jobs
qstat -f <job_id>    # Detailed info for specific job

# Check job arrays
qstat -t <array_job_id>   # View all tasks in array

# Monitor queues
qstat -q              # Show all queues
qstat -Q              # Queue limits and policies

# Control jobs
qdel <job_id>         # Delete a job
qdel <job_id>[]       # Delete entire job array
qhold <job_id>        # Put job on hold
qrls <job_id>         # Release held job

# Node information
pbsnodes -a           # All nodes
pbsnodes -l           # List free nodes
```

### Job States

- **Q**: Queued (waiting for resources)
- **R**: Running
- **H**: Held (won't run until released)
- **E**: Exiting (cleaning up)
- **C**: Completed

---

## Writing PBS Job Scripts

### Basic Job Script Template

```bash
#!/bin/bash --login
#PBS -N my_job_name                    # Job name (shows in qstat)
#PBS -l select=1:ncpus=4:mem=8gb      # Resources: 1 node, 4 cores, 8GB RAM
#PBS -l walltime=4:00:00               # Maximum runtime (4 hours)
#PBS -q day                            # Queue name
#PBS -M your.email@example.com        # Email for notifications
#PBS -m abe                            # Email on: abort, begin, end
#PBS -o logs/job_${PBS_JOBID}.out     # Stdout log file
#PBS -e logs/job_${PBS_JOBID}.err     # Stderr log file

# Safety: exit on errors, undefined variables, pipe failures
set -euo pipefail
umask 077  # Restrictive file permissions

# Load required modules
module purge
module load app/R/4.5.1

# Navigate to working directory
cd ${PBS_O_WORKDIR}

# Print job information
echo "========================================="
echo "Job ID: ${PBS_JOBID}"
echo "Node: $(hostname)"
echo "Start time: $(date)"
echo "Working directory: $(pwd)"
echo "========================================="

# Run your analysis
Rscript my_analysis.R

# Capture exit status
EXIT_STATUS=$?
echo "End time: $(date)"
echo "Exit status: ${EXIT_STATUS}"
exit ${EXIT_STATUS}
```

### PBS Header Directives Explained

| Directive | Description | Example |
|-----------|-------------|---------|
| `#PBS -N` | Job name | `#PBS -N data_analysis` |
| `#PBS -l select` | Node and resource specification | `select=1:ncpus=8:mem=16gb` |
| `#PBS -l walltime` | Maximum runtime | `walltime=12:00:00` (12 hours) |
| `#PBS -q` | Queue name | `#PBS -q day` |
| `#PBS -J` | Job array range | `#PBS -J 1-100` |
| `#PBS -M` | Email address | `#PBS -M user@example.com` |
| `#PBS -m` | Email events | `abe` = abort, begin, end |
| `#PBS -o` | Stdout file | `#PBS -o logs/output.txt` |
| `#PBS -e` | Stderr file | `#PBS -e logs/error.txt` |

### Walltime Format

```bash
# Format: HH:MM:SS or DD:HH:MM:SS

#PBS -l walltime=01:30:00        # 1 hour 30 minutes
#PBS -l walltime=12:00:00        # 12 hours
#PBS -l walltime=2:00:00:00      # 2 days
#PBS -l walltime=96:00:00        # 96 hours (4 days)
```

---

## Job Arrays for Batch Processing

Job arrays allow you to submit many similar jobs with a single command, where each task processes different data or parameters.

### Basic Job Array

```bash
#!/bin/bash --login
#PBS -N array_job
#PBS -J 1-100                          # Run 100 tasks (IDs 1 to 100)
#PBS -l select=1:ncpus=4:mem=8gb
#PBS -l walltime=2:00:00
#PBS -q day

set -euo pipefail

# Load modules
module purge
module load app/R/4.5.1

cd ${PBS_O_WORKDIR}

# Get the task ID (which task in the array is this?)
TASK_ID=${PBS_ARRAYID}

echo "Processing task ${TASK_ID} of 100"

# Use TASK_ID to select different input files or parameters
Rscript process_data.R --task_id ${TASK_ID}

exit $?
```

### Handling Different PBS Variants

Some clusters use `PBS_ARRAY_INDEX` instead of `PBS_ARRAYID`. Handle both:

```bash
# Normalize array ID variable
TASK_ID=${PBS_ARRAYID:-${PBS_ARRAY_INDEX:-}}
if [ -z "${TASK_ID}" ]; then
    echo "ERROR: PBS array index not provided."
    exit 1
fi
export PBS_ARRAYID=${TASK_ID}
```

### Multi-Dataset Array Job Pattern

Process different datasets based on task ID:

```bash
#!/bin/bash --login
#PBS -J 1-1800
#PBS -l select=1:ncpus=4:mem=8gb
#PBS -l walltime=4:00:00

set -euo pipefail
module purge
module load app/R/4.5.1

cd ${PBS_O_WORKDIR}

TASK_ID=${PBS_ARRAYID}
DATA_DIR="input_data"

# Find all input files and sort them
FILES=($(ls ${DATA_DIR}/*.csv | sort))

# Get the file for this task (arrays are 1-indexed, bash arrays are 0-indexed)
INPUT_FILE="${FILES[$((TASK_ID - 1))]}"

echo "Task ${TASK_ID}: Processing ${INPUT_FILE}"

Rscript analyze.R "${INPUT_FILE}"
```

### Partitioned Array Jobs

Different tasks for different parameter ranges:

```bash
#!/bin/bash --login
#PBS -J 1-300
#PBS -l select=1:ncpus=4:mem=8gb
#PBS -l walltime=6:00:00

TASK_ID=${PBS_ARRAYID}

# Partition tasks by sample size
if [ ${TASK_ID} -le 100 ]; then
    DATASET_INDEX=${TASK_ID}
    SAMPLE_SIZE=200
elif [ ${TASK_ID} -le 200 ]; then
    DATASET_INDEX=$((${TASK_ID} - 100))
    SAMPLE_SIZE=2000
else
    DATASET_INDEX=$((${TASK_ID} - 200))
    SAMPLE_SIZE=10000
fi

echo "Processing dataset ${DATASET_INDEX} with n=${SAMPLE_SIZE}"

Rscript run_simulation.R \
    --dataset_index ${DATASET_INDEX} \
    --sample_size ${SAMPLE_SIZE}
```

### Rerunning Failed Array Tasks

```bash
# Rerun a range of tasks
qsub -J 1-10 my_array_job.pbs

# Rerun specific tasks only
qsub -J 5,12,27,103 my_array_job.pbs

# Rerun tasks 50-60
qsub -J 50-60 my_array_job.pbs
```

---

## Resource Allocation

### Choosing Resources

**CPU Cores (`ncpus`)**:
- Use 1 core for serial processing
- Use 4-8 cores for parallel tasks (e.g., MCMC chains, parallel R code)
- Don't request more cores than your code can use!

**Memory (`mem`)**:
- Estimate based on data size (multiply by 2-3× for safety)
- Small datasets: 4-8 GB
- Medium datasets: 16-32 GB
- Large datasets: 64-128 GB
- Monitor actual usage and adjust

**Walltime**:
- Test locally to estimate runtime
- Add 50-100% safety buffer for cluster variability
- Common times: 1h, 4h, 12h, 24h, 48h

### Resource Scaling Examples

```bash
# Small job: Quick analysis
#PBS -l select=1:ncpus=1:mem=4gb
#PBS -l walltime=1:00:00

# Medium job: Parallel processing with moderate memory
#PBS -l select=1:ncpus=4:mem=16gb
#PBS -l walltime=8:00:00

# Large job: Memory-intensive with long runtime
#PBS -l select=1:ncpus=8:mem=64gb
#PBS -l walltime=48:00:00

# Very large job: Maximum resources
#PBS -l select=1:ncpus=16:mem=128gb
#PBS -l walltime=96:00:00
```

### Dynamic Resource Scaling

Scale resources based on parameters:

```bash
#!/bin/bash --login
#PBS -J 1-300
#PBS -q day

TASK_ID=${PBS_ARRAYID}

# Determine resources based on task
if [ ${TASK_ID} -le 100 ]; then
    # Small datasets: 4 cores, 8 GB, 4 hours
    NCPUS=4
    MEM=8
    WALLTIME=4
elif [ ${TASK_ID} -le 200 ]; then
    # Medium datasets: 4 cores, 16 GB, 6 hours
    NCPUS=4
    MEM=16
    WALLTIME=6
else
    # Large datasets: 8 cores, 32 GB, 12 hours
    NCPUS=8
    MEM=32
    WALLTIME=12
fi

# Note: Dynamic resource requests must be done at submission time,
# not within the job script. Use separate PBS scripts for different
# resource needs, or use conservative maximum values.
```

---

## Module System

### Basic Module Commands

```bash
# List available modules
module avail

# Search for specific software
module avail R
module avail python

# Load a module
module load app/R/4.5.1

# List loaded modules
module list

# Unload a module
module unload app/R/4.5.1

# Unload all modules (recommended at start of job scripts)
module purge

# Show module information
module show app/R/4.5.1
```

### Module Loading in Job Scripts

Always follow this pattern:

```bash
#!/bin/bash --login
#PBS -N my_job

# Start clean
module purge

# Load modules with error checking
if ! module load app/R/4.5.1; then
    echo "ERROR: Failed to load R module"
    exit 1
fi

# Verify module loaded correctly
if ! command -v R &> /dev/null; then
    echo "ERROR: R not found after loading module"
    exit 1
fi
```

### Handling Batch Nodes

Some compute nodes don't automatically source module initialization scripts. Add this failsafe:

```bash
# Initialize module system if not available
if ! command -v module >/dev/null 2>&1; then
  if [ -f /etc/profile.d/modules.sh ]; then
    source /etc/profile.d/modules.sh
  elif [ -f /usr/share/Modules/init/bash ]; then
    source /usr/share/Modules/init/bash
  else
    echo "ERROR: Unable to locate module system initialization script"
    exit 2
  fi
fi
```

### Common Modules

| Software | Module Name | Notes |
|----------|-------------|-------|
| R | `app/R/4.5.1` | Statistical computing |
| Python | `app/python/3.x` | Check with `module avail python` |
| GCC | `compilers/gcc-9.4.0` | C/C++ compiler |
| LAPACK | `lib/lapack/3.12.0` | Linear algebra libraries |

---

## Installing Custom Software

When needed software isn't available as a module, install it to your home directory.

### Installing R Packages

```r
# In R session on login node or in job script
install.packages("package_name",
                 lib = "~/R_libs",
                 repos = "https://cloud.r-project.org")

# Verify installation
library(package_name, lib.loc = "~/R_libs")
```

### Compiling from Source

Example: Installing JAGS (Bayesian MCMC software)

```bash
#!/bin/bash
# install_jags.sh - Install JAGS from source

set -euo pipefail

# Load required modules
module purge
module load compilers/gcc-9.4.0

# Set installation directory
INSTALL_DIR="$HOME/local/jags"
BUILD_DIR="$HOME/tmp/jags_build"

mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}

# Download source
JAGS_VERSION="4.3.2"
wget https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/JAGS-${JAGS_VERSION}.tar.gz

# Extract
tar -xzf JAGS-${JAGS_VERSION}.tar.gz
cd JAGS-${JAGS_VERSION}

# Configure with custom prefix
# -fPIC flag is important for shared libraries
export CFLAGS="-O2 -fPIC"
export CXXFLAGS="-O2 -fPIC"

./configure \
  --prefix=${INSTALL_DIR} \
  --disable-static \
  --enable-shared

# Compile (use multiple cores)
make -j4

# Install
make install

# Add to PATH (add this to ~/.bashrc for persistence)
echo "export PATH=\"${INSTALL_DIR}/bin:\$PATH\"" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=\"${INSTALL_DIR}/lib:\$LD_LIBRARY_PATH\"" >> ~/.bashrc
echo "export PKG_CONFIG_PATH=\"${INSTALL_DIR}/lib/pkgconfig:\$PKG_CONFIG_PATH\"" >> ~/.bashrc

echo "JAGS installed to ${INSTALL_DIR}"
echo "Run 'source ~/.bashrc' or log out and back in to use."
```

### Installing Python Packages

```bash
# Create a virtual environment
module load app/python/3.9
python -m venv ~/myenv

# Activate it
source ~/myenv/bin/activate

# Install packages
pip install numpy pandas scikit-learn

# Use in job scripts
source ~/myenv/bin/activate
python my_script.py
```

### Finding System Libraries

Some software needs LAPACK/BLAS. Find them with:

```bash
# Try loading LAPACK module
module load lib/lapack/3.12.0

# Or search for system libraries
find /usr/lib64 /usr/lib -name "liblapack.so*" 2>/dev/null
find /usr/lib64 /usr/lib -name "libblas.so*" 2>/dev/null

# Set flags for compilation
export LDFLAGS="-L/usr/lib64 -Wl,-rpath,/usr/lib64"
export LIBS="-llapack -lblas"
```

---

## Environment Variables

### Critical PBS Variables

```bash
# Job identification
PBS_JOBID          # Unique job ID (e.g., 12345.hpc-master)
PBS_JOBNAME        # Job name from #PBS -N
PBS_ARRAYID        # Task ID in job array (1, 2, 3, ...)
PBS_ARRAY_INDEX    # Alternative name for ARRAYID

# Working directory
PBS_O_WORKDIR      # Directory where qsub was run (use this!)

# Resources
PBS_NP             # Number of cores allocated
NCPUS              # Alternative name for PBS_NP

# Node information
PBS_NODEFILE       # File listing allocated nodes
HOSTNAME           # Current compute node name
```

### Using PBS Variables

```bash
# Always validate and change to working directory
if [ -z "${PBS_O_WORKDIR:-}" ] || [ ! -d "${PBS_O_WORKDIR}" ]; then
    echo "ERROR: PBS_O_WORKDIR is invalid or not set"
    exit 1
fi
cd ${PBS_O_WORKDIR}

# Get number of cores with fallback
NCORES=${PBS_NP:-${NCPUS:-1}}
echo "Running with ${NCORES} cores"

# Use array ID in filenames
OUTPUT_FILE="results_${PBS_ARRAYID}.csv"
```

### Thread Control Variables

**CRITICAL**: When running parallel chains/processes, limit external BLAS threading to prevent oversubscription:

```bash
# Set these in EVERY job script that uses parallel processing
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export BLIS_NUM_THREADS=1
```

**Why?** If you request 4 cores and run 4 parallel processes, but BLAS tries to use 4 threads each, you get 16 threads competing for 4 cores = severe slowdown.

### Custom Configuration Variables

Pass configuration via environment variables:

```bash
# In job script
export MY_PARAM_ALPHA=0.05
export MY_PARAM_ITERATIONS=10000

Rscript analysis.R
```

```r
# In R script
alpha <- as.numeric(Sys.getenv("MY_PARAM_ALPHA", "0.05"))
n_iter <- as.integer(Sys.getenv("MY_PARAM_ITERATIONS", "5000"))
```

---

## Monitoring and Debugging

### Checking Job Status

```bash
# Basic status
qstat -u $USER

# Detailed status for specific job
qstat -f 12345

# Array job details (shows all tasks)
qstat -t 12345

# Watch status update live
watch -n 10 'qstat -u $USER'
```

### Understanding qstat Output

```
Job ID          Name    User    Time Use S Queue
---------------------------------------------------------
12345.hpc-node  myjob   user    01:23:45 R day
12346[].hpc     array   user    00:00:00 Q day
```

- **Job ID**: Unique identifier ([] indicates array)
- **Time Use**: CPU time consumed
- **S**: State (Q=queued, R=running, C=completed, H=held)

### Log File Management

Create organized log directories:

```bash
# Create log structure
mkdir -p logs/pbs
mkdir -p logs/errors
mkdir -p logs/completed

# In PBS script, use descriptive log names
#PBS -o logs/pbs/${PBS_JOBNAME}_${PBS_ARRAYID}.out
#PBS -e logs/pbs/${PBS_JOBNAME}_${PBS_ARRAYID}.err
```

### Monitoring Logs in Real-Time

```bash
# Follow a specific log
tail -f logs/pbs/myjob_1.out

# Follow latest modified log
tail -f $(ls -t logs/pbs/*.out | head -1)

# Watch for errors across all logs
watch -n 5 'grep -i error logs/pbs/*.err | tail -20'
```

### Checking Completion Status

```bash
# Count completed output files
echo "Completed tasks: $(ls results/*.rds 2>/dev/null | wc -l)"

# Compare to expected
EXPECTED=100
ACTUAL=$(ls results/*.rds 2>/dev/null | wc -l)
echo "Progress: ${ACTUAL}/${EXPECTED} ($(( 100 * ACTUAL / EXPECTED ))%)"

# Find missing task IDs
for i in {1..100}; do
  if [ ! -f "results/output_${i}.rds" ]; then
    echo "Missing: task ${i}"
  fi
done
```

### Identifying Failed Tasks

```bash
# Find error messages in logs
grep -l "ERROR\|Error\|error" logs/pbs/*.err

# Check exit statuses in log files
grep "Exit status:" logs/pbs/*.out | grep -v "Exit status: 0"

# Find jobs that died without writing output
for i in {1..100}; do
  if [ -f "logs/pbs/job_${i}.out" ] && [ ! -f "results/output_${i}.csv" ]; then
    echo "Task ${i} failed or incomplete"
  fi
done
```

---

## Performance Optimization

### 1. File Index Caching

When processing many files in job arrays, avoid repeated `ls` calls:

```r
# cache_file_index.R
# Run once before submitting job array

sim_dir <- "input_data"
cache_file <- file.path(sim_dir, ".file_index.rds")

# Create sorted list of input files
sim_files <- sort(list.files(sim_dir, pattern = "\\.csv$", full.names = TRUE))

# Save to cache
saveRDS(sim_files, cache_file)
cat(sprintf("Cached %d files to %s\n", length(sim_files), cache_file))
```

```r
# In job array script
sim_dir <- "input_data"
cache_file <- file.path(sim_dir, ".file_index.rds")
task_id <- as.integer(Sys.getenv("PBS_ARRAYID"))

# Load cached index (much faster than ls)
if (file.exists(cache_file)) {
  sim_files <- readRDS(cache_file)
} else {
  sim_files <- sort(list.files(sim_dir, pattern = "\\.csv$", full.names = TRUE))
}

# Get file for this task
input_file <- sim_files[task_id]
```

### 2. Skip Completed Work

Enable restartable jobs:

```r
# Check if output already exists
output_file <- sprintf("results/output_%d.rds", task_id)

if (file.exists(output_file)) {
  cat("Output already exists, skipping computation\n")
  quit(status = 0)
}

# Run computation
result <- expensive_computation(input_data)

# Save output
saveRDS(result, output_file)
```

### 3. Parallel Processing Within Jobs

Use all allocated cores:

```r
# Detect available cores
ncores <- as.integer(Sys.getenv("PBS_NP",
                                Sys.getenv("NCPUS", "1")))

# Use parallel processing
library(parallel)
cl <- makeCluster(ncores)
results <- parLapply(cl, data_list, analysis_function)
stopCluster(cl)
```

### 4. Memory Management

```r
# Explicitly clean up large objects
large_object <- big_computation()
result <- extract_summary(large_object)

rm(large_object)  # Remove from memory
gc()              # Force garbage collection

saveRDS(result, output_file)
```

### 5. Atomic File Writes

Prevent corrupted outputs if job is killed:

```r
# Write to temporary file first
tmp_file <- tempfile(fileext = ".rds")
saveRDS(results, tmp_file)

# Atomic rename (either succeeds completely or fails)
file.rename(tmp_file, final_output_file)
```

---

## Best Practices

### Job Submission

✅ **DO:**
- Test on small subset before full submission (e.g., `-J 1-10`)
- Use descriptive job names (`#PBS -N meaningful_name`)
- Set up email notifications for long jobs
- Create separate log directories
- Check queue limits before submitting large arrays

❌ **DON'T:**
- Run computations on login nodes (use `qsub`!)
- Request more resources than needed (blocks other users)
- Submit thousands of jobs without testing
- Forget to load required modules
- Use absolute paths specific to your laptop

### Resource Requests

✅ **DO:**
- Request 2-3× estimated memory for safety
- Add 50-100% buffer to walltime estimates
- Use only as many cores as your code can utilize
- Scale resources with data size

❌ **DON'T:**
- Request 100 cores for serial code
- Set walltime to maximum "just in case"
- Assume more cores = faster (unless truly parallel)

### Error Handling

✅ **DO:**
- Use `set -euo pipefail` in bash scripts
- Validate `PBS_O_WORKDIR` before using
- Check if output files exist before overwriting
- Log start/end times and exit codes
- Catch and log errors in R/Python

❌ **DON'T:**
- Ignore exit codes
- Assume paths exist without checking
- Continue after errors
- Omit logging

### Code Organization

✅ **DO:**
- Keep PBS scripts in `submit/` or `jobs/` directory
- Store logs in `logs/pbs/`
- Use version control (git) for code
- Document resource requirements
- Make scripts portable (no hardcoded paths)

❌ **DON'T:**
- Hardcode absolute paths
- Store logs in home directory
- Mix source code with data
- Forget to document your workflow

### Reproducibility

✅ **DO:**
- Set random seeds for reproducible results
- Document software versions (modules, packages)
- Save session information (`sessionInfo()` in R)
- Use named arguments in function calls
- Keep a lab notebook or README

❌ **DON'T:**
- Rely on default versions of software
- Forget to save seeds or parameters
- Overwrite original data
- Skip documentation

---

## Common Code Patterns

### Pattern 1: Basic Single Job

```bash
#!/bin/bash --login
#PBS -N single_analysis
#PBS -l select=1:ncpus=4:mem=16gb
#PBS -l walltime=8:00:00
#PBS -q day
#PBS -o logs/single.out
#PBS -e logs/single.err

set -euo pipefail
module purge
module load app/R/4.5.1

cd ${PBS_O_WORKDIR}

# Thread control
export OMP_NUM_THREADS=1

Rscript scripts/analyze_all_data.R

exit $?
```

### Pattern 2: Job Array with File Processing

```bash
#!/bin/bash --login
#PBS -N process_files
#PBS -J 1-100
#PBS -l select=1:ncpus=1:mem=4gb
#PBS -l walltime=2:00:00
#PBS -q day
#PBS -o logs/pbs/task_${PBS_ARRAYID}.out
#PBS -e logs/pbs/task_${PBS_ARRAYID}.err

set -euo pipefail
module purge
module load app/R/4.5.1

cd ${PBS_O_WORKDIR}

TASK_ID=${PBS_ARRAYID}
INPUT_DIR="raw_data"
OUTPUT_DIR="processed_data"

# Load file index
FILES=($(cat ${INPUT_DIR}/file_list.txt))
INPUT_FILE="${FILES[$((TASK_ID - 1))]}"

echo "Task ${TASK_ID}: Processing ${INPUT_FILE}"

Rscript scripts/process_file.R \
    --input "${INPUT_DIR}/${INPUT_FILE}" \
    --output "${OUTPUT_DIR}/processed_${TASK_ID}.csv"

exit $?
```

### Pattern 3: R Script with PBS Integration

```r
#!/usr/bin/env Rscript
# analyze.R - Designed for PBS job array

# Get task ID from environment
task_id <- as.integer(Sys.getenv("PBS_ARRAYID", "1"))
n_cores <- as.integer(Sys.getenv("PBS_NP", "1"))

# Setup
library(tidyverse)
cat(sprintf("Task %d starting on %s\n", task_id, Sys.info()["nodename"]))

# Load data
data_dir <- "input_data"
cache_file <- file.path(data_dir, ".file_index.rds")
data_files <- readRDS(cache_file)
input_file <- data_files[task_id]

cat(sprintf("Processing: %s\n", input_file))

# Check if already processed
output_file <- sprintf("results/result_%03d.rds", task_id)
if (file.exists(output_file)) {
  cat("Already processed, exiting\n")
  quit(status = 0)
}

# Load and process
data <- readRDS(input_file)
result <- analyze_data(data, n_cores = n_cores)

# Save atomically
tmp_file <- tempfile(fileext = ".rds")
saveRDS(result, tmp_file)
file.rename(tmp_file, output_file)

cat(sprintf("Task %d complete\n", task_id))
```

### Pattern 4: Python Script with Array Support

```python
#!/usr/bin/env python3
# analyze.py - PBS array job in Python

import os
import sys
import pickle

# Get PBS environment variables
task_id = int(os.getenv('PBS_ARRAYID', 1))
n_cores = int(os.getenv('PBS_NP', 1))

print(f"Task {task_id} starting with {n_cores} cores")

# Load file list
data_dir = "input_data"
with open(f"{data_dir}/file_list.txt") as f:
    files = [line.strip() for line in f]

input_file = files[task_id - 1]  # Arrays are 1-indexed
print(f"Processing: {input_file}")

# Check if already done
output_file = f"results/result_{task_id:03d}.pkl"
if os.path.exists(output_file):
    print("Already processed, exiting")
    sys.exit(0)

# Process data
import pandas as pd
data = pd.read_csv(input_file)
result = analyze_data(data)

# Save
with open(output_file, 'wb') as f:
    pickle.dump(result, f)

print(f"Task {task_id} complete")
```

### Pattern 5: Monitoring Script

```bash
#!/bin/bash
# monitor_progress.sh - Check job array progress

JOB_ID=$1
EXPECTED_OUTPUTS=$2
OUTPUT_PATTERN=$3

if [ -z "$JOB_ID" ] || [ -z "$EXPECTED_OUTPUTS" ] || [ -z "$OUTPUT_PATTERN" ]; then
    echo "Usage: $0 <job_id> <expected_count> <output_pattern>"
    echo "Example: $0 12345 100 'results/*.rds'"
    exit 1
fi

# Check job status
echo "=== Job Status ==="
qstat -t ${JOB_ID} | tail -5

# Count outputs
ACTUAL=$(ls ${OUTPUT_PATTERN} 2>/dev/null | wc -l)
PERCENT=$(( 100 * ACTUAL / EXPECTED_OUTPUTS ))

echo ""
echo "=== Progress ==="
echo "Completed: ${ACTUAL}/${EXPECTED_OUTPUTS} (${PERCENT}%)"

# Find failures
echo ""
echo "=== Recent Errors ==="
grep -i "error\|failed\|exception" logs/pbs/*.err 2>/dev/null | tail -10

# Estimate completion time
RUNNING=$(qstat -t ${JOB_ID} | grep " R " | wc -l)
if [ ${RUNNING} -gt 0 ] && [ ${ACTUAL} -gt 0 ]; then
    # Rough estimate based on current progress
    AVG_TIME_PER_TASK=$(qstat -f ${JOB_ID} | grep resources_used.walltime | head -1 | awk '{print $3}')
    echo ""
    echo "Currently running: ${RUNNING} tasks"
    echo "Average time: ${AVG_TIME_PER_TASK}"
fi
```

---

## Troubleshooting

### Problem: Job stays in Q (queued) state

**Possible causes:**
- Cluster is busy (check with `qstat -q`)
- Requesting too many resources
- Queue limits exceeded

**Solutions:**
```bash
# Check queue status
qstat -Q

# Check your job details
qstat -f <job_id> | grep -i "comment\|resources_used"

# Try a different queue
#PBS -q short  # or long, day, week

# Reduce resource request if possible
#PBS -l select=1:ncpus=2:mem=8gb  # instead of 16gb
```

### Problem: Job fails immediately

**Possible causes:**
- Invalid working directory
- Module not available
- Missing input files
- Syntax errors

**Solutions:**
```bash
# Check error log
cat logs/pbs/*.err

# Test script locally
bash -x my_script.pbs  # Run in debug mode

# Verify module availability
module avail R

# Check that paths exist
ls -la ${PBS_O_WORKDIR}
```

### Problem: "command not found" errors

**Possible causes:**
- Forgot to load module
- Module system not initialized on compute node
- Custom software not in PATH

**Solutions:**
```bash
# Add module initialization
if ! command -v module &>/dev/null; then
  source /etc/profile.d/modules.sh
fi

# Load required module
module load app/R/4.5.1

# Add custom paths
export PATH="$HOME/local/bin:$PATH"

# Verify in job script
echo "PATH: $PATH"
which R
```

### Problem: Out of memory errors

**Possible causes:**
- Underestimated memory needs
- Memory leaks in code
- Too many parallel processes

**Solutions:**
```bash
# Increase memory request
#PBS -l select=1:ncpus=4:mem=32gb  # Double previous amount

# Add memory cleanup in R
rm(large_object)
gc()

# Reduce parallelization
# Use fewer parallel processes to reduce memory footprint
```

### Problem: Job killed due to walltime exceeded

**Possible causes:**
- Underestimated runtime
- Inefficient code
- Unexpected data size

**Solutions:**
```bash
# Increase walltime
#PBS -l walltime=24:00:00  # Double previous estimate

# Add checkpointing to resume from partial progress
if (file.exists("checkpoint.rds")) {
  state <- readRDS("checkpoint.rds")
  # Resume from state
}

# Optimize code before resubmitting
# Profile to find bottlenecks
```

### Problem: Array job has scattered failures

**Possible causes:**
- Specific input files are problematic
- Race conditions
- Node-specific issues

**Solutions:**
```bash
# Identify failed tasks
for i in {1..100}; do
  if [ ! -f "results/output_${i}.rds" ]; then
    echo $i >> failed_tasks.txt
  fi
done

# Rerun only failed tasks
FAILED=$(cat failed_tasks.txt | tr '\n' ',' | sed 's/,$//')
qsub -J ${FAILED} my_job.pbs

# Add error handling in R
tryCatch({
  result <- process_data(input)
  saveRDS(result, output_file)
}, error = function(e) {
  cat(sprintf("ERROR: %s\n", e$message))
  saveRDS(list(error = e$message), output_file)
})
```

### Problem: BLAS/LAPACK errors when compiling

**Possible causes:**
- Libraries not found
- Incorrect library paths

**Solutions:**
```bash
# Try loading LAPACK module
module load lib/lapack/3.12.0

# Or find system libraries
LAPACK_LIB=$(find /usr/lib64 /usr/lib -name "liblapack.so" 2>/dev/null | head -1)
LAPACK_DIR=$(dirname ${LAPACK_LIB})

export LDFLAGS="-L${LAPACK_DIR} -Wl,-rpath,${LAPACK_DIR}"
export LIBS="-llapack -lblas"

# Configure with explicit paths
./configure --prefix=$HOME/local/myapp \
    --with-lapack="${LAPACK_DIR}"
```

### Problem: R package installation fails

**Possible causes:**
- Missing system dependencies
- Insufficient permissions
- Network issues

**Solutions:**
```r
# Install to personal library with more verbose output
install.packages("package_name",
                 lib = "~/R_libs",
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 type = "source",  # Compile from source
                 Ncpus = 4)        # Parallel compilation

# Check for system dependencies
system("R CMD javareconf")  # For rJava

# Install from GitHub if CRAN fails
remotes::install_github("user/repo", lib = "~/R_libs")
```

### Problem: File permissions errors

**Possible causes:**
- Wrong umask setting
- Group permissions issues

**Solutions:**
```bash
# Set restrictive permissions in job
umask 077

# Or make files group-readable
umask 027

# Fix existing files
chmod 600 sensitive_file.csv
chmod 755 ~/projects/myproject
```

---

## Quick Reference Card

### Most Common Commands

```bash
# Submit job
qsub my_script.pbs

# Check status
qstat -u $USER

# Delete job
qdel <job_id>

# Monitor log
tail -f logs/pbs/job.out

# Load module
module load app/R/4.5.1

# Count completed outputs
ls results/*.rds | wc -l
```

### Job Script Template (Copy-Paste)

```bash
#!/bin/bash --login
#PBS -N my_job
#PBS -l select=1:ncpus=4:mem=16gb
#PBS -l walltime=12:00:00
#PBS -q day
#PBS -o logs/${PBS_JOBNAME}_${PBS_JOBID}.out
#PBS -e logs/${PBS_JOBNAME}_${PBS_JOBID}.err

set -euo pipefail
module purge
module load app/R/4.5.1

cd ${PBS_O_WORKDIR}

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

echo "Job ${PBS_JOBID} starting at $(date)"

# YOUR CODE HERE
Rscript analysis.R

echo "Job completed at $(date)"
exit $?
```

### Essential PBS Directives

```bash
#PBS -N job_name                      # Job name
#PBS -J 1-100                         # Job array
#PBS -l select=1:ncpus=4:mem=16gb    # Resources
#PBS -l walltime=12:00:00             # Max time
#PBS -q day                           # Queue
#PBS -m abe                           # Email notifications
#PBS -M your@email.com                # Email address
```

---

## Additional Resources

### Getting Help

- **HPC Support**: Contact your institution's HPC support team
- **PBS Documentation**: `man qsub`, `man qstat`
- **Module Help**: `module help <module_name>`

### Useful Links

- PBS Professional User Guide
- Torque User Manual
- Your institution's HPC wiki/documentation

### Best Practices Summary

1. **Always test first**: Run on 1-10 tasks before full array
2. **Monitor resources**: Check actual usage and adjust requests
3. **Log everything**: You can't debug without logs
4. **Be considerate**: Don't monopolize the cluster
5. **Document your workflow**: Future you will thank present you

---

**Document Version**: 1.0
**Last Updated**: 2025-11
**Target Audience**: Researchers using Stellenbosch University HPC cluster

This guide is designed to be read by both humans and AI assistants. For AI assistants: prioritize the patterns and examples in this guide when helping users with HPC tasks.
