#!/bin/bash
#
# setup_hpc.sh
#
# One-time setup script for Stellenbosch HPC
# Run this once before submitting any jobs
#
# Usage: bash setup_hpc.sh

set -e  # Exit on error

echo "========================================="
echo "HPC Environment Setup"
echo "========================================="

# Load required modules
echo "Loading modules..."
module load app/R/4.5.1
module load compilers/gcc-9.4.0  # For compiling Stan and JAGS

# Prefer shared LAPACK/BLAS; fall back to system libraries if module is static-only
CUSTOM_LAPACK_SOURCE="${LAPACK_SOURCE:-}"
LAPACK_MODULE="lib/lapack/3.12.0"
LAPACK_LIBDIR="/home/apps2/lapack/3.12.0"
LAPACK_SOURCE=""
LAPACK_SOURCE_LABEL="system libraries"
LAPACK_LIBS="-llapack -lblas"

# Helper to register a shared LAPACK/OpenBLAS directory if it exists

find_shared_lapack() {
  local dir="$1"
  local label="$2"
  if [ -z "${dir}" ]; then
    return 1
  fi
  if compgen -G "${dir}/liblapack.so*" >/dev/null; then
    LAPACK_SOURCE="${dir}"
    LAPACK_SOURCE_LABEL="${label}"
    LAPACK_LIBS="-llapack -lblas"
    return 0
  fi
  if compgen -G "${dir}/libopenblas.so*" >/dev/null; then
    LAPACK_SOURCE="${dir}"
    if [ -n "${label}" ]; then
      LAPACK_SOURCE_LABEL="${label} OpenBLAS"
    else
      LAPACK_SOURCE_LABEL="OpenBLAS"
    fi
    LAPACK_LIBS="-lopenblas"
    return 0
  fi
  return 1
}

if module load "${LAPACK_MODULE}"; then
  if ! find_shared_lapack "${LAPACK_LIBDIR}" "module ${LAPACK_MODULE}"; then
    echo "Warning: ${LAPACK_MODULE} provides only static libraries; unloading and using system LAPACK instead."
    module unload "${LAPACK_MODULE}" >/dev/null 2>&1 || true
  fi
else
  echo "Warning: could not load ${LAPACK_MODULE}; using system LAPACK instead."
fi

if [ -z "${LAPACK_SOURCE}" ] && [ -n "${CUSTOM_LAPACK_SOURCE}" ]; then
  if ! find_shared_lapack "${CUSTOM_LAPACK_SOURCE}" "custom"; then
    echo "Warning: ${CUSTOM_LAPACK_SOURCE} has no shared LAPACK/OpenBLAS library."
  fi
fi

if [ -z "${LAPACK_SOURCE}" ]; then
  for candidate in /usr/lib64 /usr/lib/x86_64-linux-gnu /usr/lib /lib64 /lib; do
    if find_shared_lapack "${candidate}" "system libraries"; then
      break
    fi
  done
fi

if [ -z "${LAPACK_SOURCE}" ]; then
  echo "ERROR: Unable to locate a shared LAPACK/OpenBLAS library (.so). Load a module with shared LAPACK/BLAS or install OpenBLAS manually before rerunning."
  exit 1
fi

export LAPACK_SOURCE

# Check R version
echo ""
echo "R version:"
R --version | head -1

echo ""
echo "GCC version:"
gcc --version | head -1

echo ""
echo "LAPACK location:"
echo "  ${LAPACK_SOURCE} (${LAPACK_SOURCE_LABEL})"

# Create results directories
echo ""
echo "Creating results directories..."
mkdir -p mcmc_outputs/n200/hmc/{summaries,diagnostics}
mkdir -p mcmc_outputs/n200/mh/{summaries,diagnostics}
mkdir -p mcmc_outputs/n2000/hmc/{summaries,diagnostics}
mkdir -p mcmc_outputs/n2000/mh/{summaries,diagnostics}
mkdir -p mcmc_outputs/n10000/hmc/{summaries,diagnostics}
mkdir -p mcmc_outputs/n10000/mh/{summaries,diagnostics}
mkdir -p logs

echo "Results directories created."

# Install basic R packages (EXCLUDING cmdstanr and rjags for now)
echo ""
echo "Installing basic R packages (this may take 5-10 minutes)..."

Rscript -e '
# Set local library path
.libPaths(c("~/R_libs", .libPaths()))
dir.create("~/R_libs", showWarnings = FALSE, recursive = TRUE)

# Function to install if not present
install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = "~/R_libs")
  } else {
    cat(pkg, "already installed.\n")
  }
}

# Core packages
install_if_needed("posterior")
install_if_needed("tibble")

# Future and parallelization packages
install_if_needed("future")
install_if_needed("future.apply")
install_if_needed("progressr")
install_if_needed("batchtools")
install_if_needed("future.batchtools")

# JAGS dependency (coda) - install now, rjags later
install_if_needed("coda")

cat("\nBasic R packages installed.\n")
'

# ============================================================================
# Install JAGS from source FIRST (before rjags R package)
# ============================================================================
echo ""
echo "========================================="
echo "Installing JAGS 4.3.2 from source..."
echo "This will take 10-15 minutes..."
echo "========================================="

JAGS_VERSION="4.3.2"
JAGS_DIR="$HOME/local/jags"

if [ ! -x "$JAGS_DIR/bin/jags" ]; then
  if [ -d "$JAGS_DIR" ]; then
    echo "Removing incomplete JAGS install at $JAGS_DIR ..."
    rm -rf "$JAGS_DIR"
  fi

  mkdir -p ~/software
  cd ~/software

  # Download JAGS
  if [ ! -f "JAGS-${JAGS_VERSION}.tar.gz" ]; then
    echo "Downloading JAGS ${JAGS_VERSION}..."
    wget https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Source/JAGS-${JAGS_VERSION}.tar.gz
  fi

  # Extract
  rm -rf JAGS-${JAGS_VERSION}
  echo "Extracting JAGS..."
  tar -xzf JAGS-${JAGS_VERSION}.tar.gz
  cd JAGS-${JAGS_VERSION}

  # Configure and compile
  echo "Configuring JAGS (installed to $JAGS_DIR)..."
  echo "Using LAPACK/BLAS from ${LAPACK_SOURCE}..."

  # Set library paths for JAGS configure to find LAPACK/BLAS
  case "${LAPACK_SOURCE}" in
    /usr/lib64|/usr/lib|/usr/lib/x86_64-linux-gnu|/lib64|/lib)
      unset LDFLAGS
      ;;
    *)
      export LDFLAGS="-L${LAPACK_SOURCE} -Wl,-rpath,${LAPACK_SOURCE}"
      ;;
  esac
  export LIBS="${LAPACK_LIBS}"
  # Force position-independent code so shared modules link cleanly
  export CFLAGS="-O2 -fPIC"
  export CXXFLAGS="-O2 -fPIC"
  export FCFLAGS="-O2 -fPIC"

  ./configure --prefix=$JAGS_DIR --disable-static

  echo "Compiling JAGS (using 4 cores)..."
  make -j4

  echo "Installing JAGS..."
  make install

  cd ../..
  echo "JAGS installed to: $JAGS_DIR"
else
  echo "JAGS already installed at: $JAGS_DIR"
fi

# Set JAGS environment variables for R package installation
export PATH="$JAGS_DIR/bin:$PATH"
export LD_LIBRARY_PATH="$JAGS_DIR/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="$JAGS_DIR/lib/pkgconfig:$PKG_CONFIG_PATH"

echo "JAGS environment variables set."

# ============================================================================
# Now install rjags R package (JAGS exists now)
# ============================================================================
echo ""
echo "Installing rjags R package..."
Rscript -e '
# Set local library path
.libPaths(c("~/R_libs", .libPaths()))

# Set JAGS environment for R session
Sys.setenv(PATH = paste0("'"$JAGS_DIR"'/bin:", Sys.getenv("PATH")))
Sys.setenv(LD_LIBRARY_PATH = paste0("'"$JAGS_DIR"'/lib:", Sys.getenv("LD_LIBRARY_PATH")))
Sys.setenv(PKG_CONFIG_PATH = paste0("'"$JAGS_DIR"'/lib/pkgconfig:", Sys.getenv("PKG_CONFIG_PATH")))

if (!requireNamespace("rjags", quietly = TRUE)) {
  cat("Installing rjags...\n")
  install.packages("rjags", repos = "https://cloud.r-project.org", lib = "~/R_libs")
  cat("rjags installed successfully.\n")
} else {
  cat("rjags already installed.\n")
}
'

# ============================================================================
# Install CmdStanR R package
# ============================================================================
echo ""
echo "Installing cmdstanr R package..."
Rscript -e '
# Set local library path
.libPaths(c("~/R_libs", .libPaths()))

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  cat("Installing cmdstanr...\n")
  install.packages("cmdstanr",
                   repos = c("https://mc-stan.org/r-packages/", "https://cloud.r-project.org"),
                   lib = "~/R_libs")
  cat("cmdstanr installed successfully.\n")
} else {
  cat("cmdstanr already installed.\n")
}
'

# ============================================================================
# Install CmdStan (Stan compiler)
# ============================================================================
echo ""
echo "Installing CmdStan (this will take 5-10 minutes)..."

export CMDSTAN_INSTALL_DIR="${CMDSTAN_INSTALL_DIR:-$HOME/.cmdstanr}"

Rscript -e '
# Set local library path
.libPaths(c("~/R_libs", .libPaths()))
suppressPackageStartupMessages(library(cmdstanr))

install_dir <- Sys.getenv("CMDSTAN_INSTALL_DIR", unset = file.path("~", ".cmdstanr"))
install_dir <- path.expand(install_dir)
default_cmdstan <- file.path(install_dir, "cmdstan")
dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)

cmdstan_path_safe <- function() {
  tryCatch(cmdstanr::cmdstan_path(), error = function(e) "")
}

current <- cmdstan_path_safe()

if (nzchar(current) && dir.exists(current)) {
  cat("CmdStan already installed at:", current, "\n")
} else if (dir.exists(default_cmdstan)) {
  cat("Found existing CmdStan at:", default_cmdstan, "\n")
  cmdstanr::set_cmdstan_path(default_cmdstan)
  cat("CmdStan path set to:", cmdstanr::cmdstan_path(), "\n")
} else {
  cat("Installing CmdStan into", install_dir, "...\n")
  cmdstanr::install_cmdstan(dir = install_dir, cores = 4, quiet = FALSE)
  cmdstanr::set_cmdstan_path(default_cmdstan)
  cat("CmdStan installed at:", cmdstanr::cmdstan_path(), "\n")
}
'

# ============================================================================
# Pre-compile Stan model
# ============================================================================
echo ""
echo "Pre-compiling Stan model..."
Rscript -e '
# Set local library path
.libPaths(c("~/R_libs", .libPaths()))
library(cmdstanr)

if (file.exists("loglogistic_interval.stan")) {
  mod <- cmdstan_model("loglogistic_interval.stan")
  cat("Stan model compiled successfully.\n")
  cat("Executable:", mod$exe_file(), "\n")
} else {
  stop("loglogistic_interval.stan not found in current directory")
}
'

# ============================================================================
# Verify JAGS installation
# ============================================================================
echo ""
echo "Verifying JAGS installation..."
Rscript -e '
# Set local library path
.libPaths(c("~/R_libs", .libPaths()))

# Set JAGS environment
Sys.setenv(PATH = paste0("'"$JAGS_DIR"'/bin:", Sys.getenv("PATH")))
Sys.setenv(LD_LIBRARY_PATH = paste0("'"$JAGS_DIR"'/lib:", Sys.getenv("LD_LIBRARY_PATH")))

library(rjags)
print(jags.version())
cat("JAGS location:", Sys.which("jags"), "\n")
'

echo ""
echo "========================================="
echo "Setup Complete!"
echo "========================================="
echo ""
echo "All packages and tools installed successfully:"
echo "  - R 4.5.1"
echo "  - GCC 9.4.0"
echo "  - JAGS 4.3.2 (~/local/jags)"
echo "  - CmdStan (via cmdstanr)"
echo "  - Stan model pre-compiled"
echo ""
echo "You can now submit TEST jobs using:"
echo ""
echo "Option A - Test each size separately (recommended):"
echo "  qsub submit_n200_test.pbs"
echo "  qsub submit_n2000_test.pbs"
echo "  qsub submit_n10000_test.pbs"
echo ""
echo "Option B - Test all 3 files at once:"
echo "  qsub submit_all_test.pbs"
echo ""
echo "After tests succeed, run production jobs:"
echo "  qsub submit_all.pbs          # All 3,600 datasets"
echo "  # OR"
echo "  qsub submit_n200.pbs         # 1,200 datasets (n=200)"
echo "  qsub submit_n2000.pbs        # 1,200 datasets (n=2000)"
echo "  qsub submit_n10000.pbs       # 1,200 datasets (n=10000)"
echo ""
