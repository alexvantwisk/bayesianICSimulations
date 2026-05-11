test_that("compute_age_specific_hazard matches closed-form log-logistic hazard", {
  draws <- tibble::tibble(
    .draw = 1:4,
    alpha = c(5, 5, 5, 5),
    beta = c(0, 0, 0, 0),
    gamma = c(1.5, 1.5, 1.5, 1.5)
  )

  res <- compute_age_specific_hazard(
    draws,
    ages = c(10, 20, 30), x = 0
  )
  expected <- (1.5 / 5) * (c(10, 20, 30) / 5)^(0.5) /
    (1 + (c(10, 20, 30) / 5)^1.5)
  expect_equal(res$hazard_mean, expected, tolerance = 1e-8)
  expect_equal(res$age, c(10, 20, 30))
})

test_that("compute_population_incidence is a weighted mean of per-subject hazards", {
  draws <- tibble::tibble(.draw = 1, alpha = 5, beta = log(2), gamma = 1.5)
  pop <- tibble::tibble(age = c(20, 20), X1 = c(0, 1), weight = c(1, 1))

  res <- compute_population_incidence(draws, pop)

  h_male <- (1.5 / 5) * (20 / 5)^0.5 / (1 + (20 / 5)^1.5)
  h_female <- (1.5 / (5 * 2)) * (20 / (5 * 2))^0.5 / (1 + (20 / (5 * 2))^1.5)
  expect_equal(res$incidence_median, mean(c(h_male, h_female)), tolerance = 1e-8)
})
