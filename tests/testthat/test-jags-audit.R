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
