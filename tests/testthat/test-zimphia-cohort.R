test_that("derive_birth_cohort assigns subjects to the correct cohort", {
  res <- derive_birth_cohort(
    age = c(20, 30, 40, 55, 70), # birth year = 2020 - age = 2000, 1990, 1980, 1965, 1950
    survey_year = 2020
  )
  expect_equal(
    as.character(res),
    c("1990+", "1990+", "1980-1989", "1965-1979", NA)
  )
})

test_that("derive_birth_cohort respects boundary inclusion", {
  res <- derive_birth_cohort(
    age = c(31, 40, 41, 55, 56), # birth = 1989, 1980, 1979, 1965, 1964
    survey_year = 2020
  )
  expect_equal(
    as.character(res),
    c("1980-1989", "1980-1989", "1965-1979", "1965-1979", NA)
  )
})
