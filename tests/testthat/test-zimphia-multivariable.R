test_that("prepare_zimphia_multivariable_data builds K-column matrix with correct names", {
  fake_indiv <- tibble::tibble(
    personid = c("p1", "p2", "p3", "p4"),
    age = c(20, 30, 40, 25),
    gender = c(1, 2, 1, 2), # 1=male, 2=female (ZIMPHIA coding)
    urban = c(1, 2, 1, 2) # 1=urban, 2=rural
  )
  base <- tibble::tibble(
    personid = c("p1", "p2", "p3", "p4"),
    L = c(0.1, 5, 10, 8),
    R = c(20, Inf, 40, 25),
    X1 = c(0, 1, 0, 1),
    weight = rep(1, 4)
  )

  res <- prepare_zimphia_multivariable_data(
    base, fake_indiv,
    covariates = c("sex", "urban_rural", "age_band")
  )

  expect_named(res, c("data", "X", "covariate_levels"))
  expect_equal(ncol(res$X), 4) # sex + urban_rural + 2 age dummies (after dropping youngest baseline)
  expect_equal(nrow(res$X), 4)
  expect_true(all(res$X[, "sex"] %in% c(0, 1)))
})
