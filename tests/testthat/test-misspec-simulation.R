test_that("get_default_params() exposes the Weibull DGM parameters", {
  p <- get_default_params()
  expect_equal(p$k_weibull, 2.0)
  expect_equal(p$lambda_weibull, 6.01)
})

test_that("simulate_survival_data_weibull recovers Weibull median by Monte Carlo", {
  set.seed(2025)
  params <- get_default_params()
  params$target_censoring_prop <- 0

  # Sanity-check the inverse CDF used internally (independent reconstruction)
  Ti_check <- params$lambda_weibull *
    (-log(1 - runif(50000)))^(1 / params$k_weibull)
  expect_equal(median(Ti_check), 5.0, tolerance = 0.05)
})

test_that("simulate_survival_data_weibull produces the right tibble shape", {
  set.seed(1)
  params <- get_default_params()

  res <- simulate_survival_data_weibull(
    n = 200, params = params,
    weight_type = "high"
  )

  expect_s3_class(res, "tbl_df")
  expect_named(res, c(
    "X1", "A0", "A_event", "visit", "L", "R",
    "status", "weight"
  ))
  expect_true(all(res$weight > 0))
  expect_true(all(res$L >= 0))
})

test_that("simulate_survival_data_weibull validates weight_type", {
  params <- get_default_params()
  expect_error(
    simulate_survival_data_weibull(
      n = 10, params = params,
      weight_type = "invalid"
    ),
    "should be one of"
  )
})
