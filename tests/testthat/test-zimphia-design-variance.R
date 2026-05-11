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
