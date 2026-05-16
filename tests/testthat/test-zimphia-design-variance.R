test_that("load_replicate_weights returns the requested columns joined on personid", {
  csv_path <- test_replicate_weights_csv() # from setup-paths.R
  base <- tibble::tibble(personid = c("ZW20000000000101", "ZW20000000000102"))

  res <- load_replicate_weights(base, csv_path, n_reps = 5)

  expect_named(res, c(
    "personid",
    paste0("design_wt", sprintf("%03d", 1:5))
  ))
  expect_equal(nrow(res), 2)
  expect_true(all(vapply(res[, -1], is.numeric, logical(1))))
})

test_that("load_replicate_weights rejects out-of-range n_reps", {
  base <- tibble::tibble(personid = "ZW20000000000101")
  expect_error(
    load_replicate_weights(base, csv_path = "nonexistent.csv", n_reps = 200L),
    "n_reps"
  )
})

test_that("load_replicate_weights rejects non-integer n_reps", {
  base <- tibble::tibble(personid = "ZW20000000000101")
  expect_error(
    load_replicate_weights(base, csv_path = "nonexistent.csv", n_reps = 1.5),
    "whole-number"
  )
})

test_that("combine_design_replicates returns T >= W_bar (Rubin sanity)", {
  set.seed(2025)
  fake <- tibble::tibble(
    variable  = rep(c("alpha", "beta", "gamma"), each = 10),
    mean      = rnorm(30),
    sd        = runif(30, 0.1, 0.5),
    replicate = rep(seq_len(10), 3)
  )
  out <- combine_design_replicates(fake)
  expect_true(all(out$t_total >= out$w_bar))
  expect_true(all(out$ci_upper > out$ci_lower))
  expect_equal(nrow(out), 3L)
  expect_setequal(out$variable, c("alpha", "beta", "gamma"))
})

test_that("combine_design_replicates errors on missing columns", {
  expect_error(
    combine_design_replicates(tibble::tibble(variable = "alpha", mean = 0)),
    "missing columns"
  )
})

test_that("combine_design_replicates correctly recovers known T", {
  toy <- tibble::tibble(
    variable  = rep("alpha", 5),
    mean      = c(-0.16, -0.15, -0.17, -0.14, -0.16),
    sd        = rep(0.009, 5),
    replicate = seq_len(5)
  )
  out <- combine_design_replicates(toy)
  expect_equal(out$w_bar, 0.009^2, tolerance = 1e-9)
  expect_equal(out$b, stats::var(toy$mean), tolerance = 1e-9)
  expected_t <- 0.009^2 + (1 + 1 / 5) * stats::var(toy$mean)
  expect_equal(out$t_total, expected_t, tolerance = 1e-9)
})
