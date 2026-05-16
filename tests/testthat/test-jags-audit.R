test_that("audit_jags_samplers returns one row per monitored parameter", {
  skip_if_not_installed("rjags")

  # Tiny synthetic dataset with positive L, mixed R
  set.seed(1)
  n <- 20
  dat <- list(
    N = n,
    L = pmax(rexp(n, 1), 1e-10),
    X = rbinom(n, 1, 0.5),
    w = rep(1, n),
    zeros = rep(0, n)
  )
  R_finite <- dat$L + 1 + rexp(n, 1)
  dat$R <- ifelse(runif(n) < 0.5, 1e11, R_finite)

  model_file <- system.file("models", "loglogistic_interval.jags",
    package = "bayesianICSimulations"
  )
  if (!nzchar(model_file)) {
    # dev-mode fallback when the package isn't installed
    model_file <- testthat::test_path("..", "..", "inst", "models", "loglogistic_interval.jags")
  }
  skip_if_not(file.exists(model_file), "JAGS model not present")

  res <- audit_jags_samplers(dat, model_file)

  expect_s3_class(res, "tbl_df")
  expect_setequal(c("node", "sampler"), names(res))
  expect_true(all(c("alpha", "beta", "gamma") %in% res$node))
  expect_gt(nrow(res), 0L)
  expect_true(all(nzchar(res$sampler)))
})
