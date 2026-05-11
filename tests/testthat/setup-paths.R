# Shared paths used across multiple test files
test_zimphia_dir <- function() {
  testthat::skip_if_not(
    dir.exists("../../ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)"),
    "ZIMPHIA microdata not present"
  )
  "../../ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)"
}

test_replicate_weights_csv <- function() {
  path <- file.path(
    "..", "..", "ZIMPHIA",
    "ZIMPHIA 2020 Intermediary Weights (CSV)",
    "zimphia2020indintermediarywts.csv"
  )
  testthat::skip_if_not(file.exists(path), "Replicate weights CSV not present")
  path
}
