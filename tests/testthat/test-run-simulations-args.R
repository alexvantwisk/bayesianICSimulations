test_that("run_simulations() effective_reps is max(n_replicates, n_replicates_hmc)", {
  skip_if_not_installed("tidyr")
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  run_simulations(
    out_dir = tmp,
    n_obs_vec = 200, censoring_props = 0.3, weight_types = "high",
    n_replicates = 3L
  )
  expect_equal(length(list.files(tmp, pattern = "\\.rds$")), 3L)

  unlink(list.files(tmp, full.names = TRUE))

  run_simulations(
    out_dir = tmp,
    n_obs_vec = 200, censoring_props = 0.3, weight_types = "high",
    n_replicates = 3L, n_replicates_hmc = 5L
  )
  expect_equal(length(list.files(tmp, pattern = "\\.rds$")), 5L)
})

test_that("run_simulations() rejects bad replicate args", {
  expect_error(
    run_simulations(out_dir = tempfile(), n_replicates = 0L),
    "n_replicates must be"
  )
  expect_error(
    run_simulations(
      out_dir = tempfile(),
      n_replicates_hmc = -1L
    ),
    "n_replicates_hmc must be"
  )
})
