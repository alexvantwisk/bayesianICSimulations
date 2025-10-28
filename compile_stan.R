#!/usr/bin/env Rscript
#
# compile_stan.R
#
# Pre-compiles the Stan model to avoid recompilation overhead in array jobs.
# Run this ONCE before submitting job arrays.
#
# Usage: Rscript compile_stan.R

suppressPackageStartupMessages({
  library(cmdstanr)
})

cat("========================================\n")
cat("Stan Model Pre-Compilation\n")
cat("========================================\n\n")

# Get paths
base_dir <- getwd()
stan_model_file <- file.path(base_dir, "loglogistic_interval.stan")
compiled_dir <- file.path(base_dir, "compiled_models")

# Check if model exists
if (!file.exists(stan_model_file)) {
  stop("Stan model file not found: ", stan_model_file)
}

cat("Model file:", stan_model_file, "\n")
cat("Output directory:", compiled_dir, "\n\n")

# Create output directory
dir.create(compiled_dir, recursive = TRUE, showWarnings = FALSE)

# Compile model
cat("Compiling Stan model...\n")
compile_start <- Sys.time()

mod <- cmdstan_model(
  stan_model_file,
  dir = compiled_dir,
  compile = TRUE,
  force_recompile = TRUE
)

compile_end <- Sys.time()
compile_time <- as.numeric(difftime(compile_end, compile_start, units = "secs"))

cat(sprintf("\nCompilation completed in %.1f seconds\n", compile_time))
cat("Executable location:", mod$exe_file(), "\n")

# Save model path for reference
model_info <- list(
  stan_file = stan_model_file,
  exe_file = mod$exe_file(),
  compiled_at = Sys.time(),
  compile_time_sec = compile_time
)

saveRDS(model_info, file.path(compiled_dir, "model_info.rds"))

cat("\n========================================\n")
cat("Pre-compilation successful!\n")
cat("Use the compiled model in run_fits.R\n")
cat("========================================\n")
