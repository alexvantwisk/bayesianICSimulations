## batchtools configuration for Stellenbosch HPC (PBS)
##
## This file configures batchtools to submit jobs to the PBS scheduler.
## Place this file in your project root directory.
##
## For more info: https://mllg.github.io/batchtools/

# Use PBS cluster functions
cluster.functions = batchtools::makeClusterFunctionsPBS(
  template = "batchtools.pbs.tmpl"
)

# Default resources for PBS jobs
# These can be overridden in future.batchtools::tweak()
default.resources = list(
  ncpus = 4,            # 4 cores for parallel MCMC chains
  memory = 8192,        # 8 GB RAM (in MB)
  walltime = "12:00:00", # 12 hours
  queue = "day"         # Queue name (adjust to your HPC)
)

# Job registry path (stores job metadata and results)
# Uses a temporary directory by default; jobs clean up automatically
# Uncomment to use a persistent directory for debugging:
# work.dir = "~/.batchtools"

# Raise resources for PBS communication
raise.warnings = FALSE

# Email notifications (set in PBS template, not here)
# mail.start = "none"
# mail.done = "none"
# mail.error = "none"

# Logging
# Logs are written to the registry directory
# Set to TRUE for verbose output (useful for debugging)
debug = FALSE
