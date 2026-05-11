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
  sep = "", file = out_path
)
utils::capture.output(print(audit, n = 100), file = out_path, append = TRUE)
print(audit)
