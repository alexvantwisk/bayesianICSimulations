library(tidyverse)
library(gt)
#' Mean Runtime Table
#'
#' Creates a gt table showing mean runtime by method, sample size, censoring,
#' and weight type.
#'
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object with runtime statistics
#'
#' @examples
#' \dontrun{
#' mean_runtime()
#' }
mean_runtime <- function(data) {
  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Prepare data: calculate mean total_time by method, n_obs, censoring, weight_type
  df <- df %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      total_time
    ) %>%
    dplyr::distinct(total_time, .keep_all = TRUE) %>%
    # Set weight_type factor order to None, Low, High
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      mean_time = mean(total_time, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to wide format: columns for weight_type, rows for censoring within n_obs
  table_data <- df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = mean_time,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, censoring)

  # Create gt table
  runtime_table <- table_data %>%
    gt() %>%
    # Add column spanners for MH and HMC
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    # Row grouping by sample size
    tab_row_group(
      label = "n = 10,000",
      rows = n_obs == 10000
    ) %>%
    tab_row_group(
      label = "n = 2,000",
      rows = n_obs == 2000
    ) %>%
    tab_row_group(
      label = "n = 200",
      rows = n_obs == 200
    ) %>%
    # Rename columns to be more readable
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    # Hide the n_obs column since we're using row groups
    cols_hide(n_obs) %>%
    # Format numbers to 2 decimal places
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 2
    ) %>%
    # Add table title
    tab_header(
      title = "Mean Runtime by Method, Sample Size, Censoring, and Weight Type"
    ) %>%
    # Style the table
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  # Display the table
  runtime_table %>% as_latex() %>% as.character()
}

#' Mean ESS per Second Table
#'
#' Creates a gt table showing mean effective sample size per second by method,
#' sample size, censoring, and weight type.
#'
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object with ESS per second statistics
#'
#' @examples
#' \dontrun{
#' mean_ess_per_sec()
#' }
mean_ess_per_sec <- function(data) {
  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Prepare data: calculate mean ess_per_sec by method, n_obs, censoring, weight_type
  df <- df %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      ess,
      total_time
    ) %>%
    # Calculate ESS per second
    dplyr::mutate(
      ess_per_sec = ess / total_time
    ) %>%
    dplyr::distinct(ess_per_sec, .keep_all = TRUE) %>%
    # Set weight_type factor order to None, Low, High
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      mean_ess_per_sec = mean(ess_per_sec, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to wide format: columns for weight_type, rows for censoring within n_obs
  table_data <- df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = mean_ess_per_sec,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, censoring)

  # Create gt table
  ess_table <- table_data %>%
    gt() %>%
    # Add column spanners for MH and HMC
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    # Row grouping by sample size
    tab_row_group(
      label = "n = 10,000",
      rows = n_obs == 10000
    ) %>%
    tab_row_group(
      label = "n = 2,000",
      rows = n_obs == 2000
    ) %>%
    tab_row_group(
      label = "n = 200",
      rows = n_obs == 200
    ) %>%
    # Rename columns to be more readable
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    # Hide the n_obs column since we're using row groups
    cols_hide(n_obs) %>%
    # Format numbers to 2 decimal places
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 2
    ) %>%
    # Add table title
    tab_header(
      title = "Mean ESS per Second by Method, Sample Size, Censoring, and Weight Type"
    ) %>%
    # Style the table
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  # Display the table
  ess_table %>% as_latex() %>% as.character()
}

#' Bias Table
#'
#' Creates a gt table showing mean bias of parameter estimates by method,
#' sample size, censoring, and weight type.
#'
#' @param parameter Character vector of parameter names. Default is
#'   c("alpha", "beta", "gamma"), which generates tables for all three
#'   parameters and returns a named list.
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object (single parameter) or named list of gt tables
#'   (all three parameters)
#'
#' @examples
#' \dontrun{
#' # Generate all three parameter tables
#' bias_table()
#'
#' # Generate table for a single parameter
#' bias_table("alpha")
#' }
bias_table <- function(parameter = c("alpha", "beta", "gamma"), data) {
  # If parameter is vector of all three, generate tables for all
  if (
    length(parameter) == 3 && all(parameter %in% c("alpha", "beta", "gamma"))
  ) {
    if (missing(data)) {
      return(
        purrr::map(parameter, ~ bias_table(.x)) %>% purrr::set_names(parameter)
      )
    } else {
      return(
        purrr::map(parameter, ~ bias_table(.x, data)) %>%
          purrr::set_names(parameter)
      )
    }
  }

  # Validate parameter
  if (!parameter %in% c("alpha", "beta", "gamma")) {
    stop("parameter must be one of: 'alpha', 'beta', 'gamma'")
  }

  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Filter to specific parameter and calculate mean bias
  df <- df %>%
    dplyr::filter(variable == parameter) %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      error
    ) %>%
    # Set factor orders
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      mean_bias = mean(error, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to wide format
  table_data <- df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = mean_bias,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, censoring)

  # Create gt table
  bias_tbl <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    tab_row_group(
      label = "n = 10,000",
      rows = n_obs == 10000
    ) %>%
    tab_row_group(
      label = "n = 2,000",
      rows = n_obs == 2000
    ) %>%
    tab_row_group(
      label = "n = 200",
      rows = n_obs == 200
    ) %>%
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    cols_hide(n_obs) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 4
    ) %>%
    tab_header(
      title = paste0("Bias: ", parameter)
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  bias_tbl %>% as_latex() %>% as.character()
}

#' RMSE Table
#'
#' Creates a gt table showing root mean squared error (RMSE) of parameter
#' estimates by method, sample size, censoring, and weight type.
#'
#' @param parameter Character vector of parameter names. Default is
#'   c("alpha", "beta", "gamma"), which generates tables for all three
#'   parameters and returns a named list.
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object (single parameter) or named list of gt tables
#'   (all three parameters)
#'
#' @examples
#' \dontrun{
#' # Generate all three parameter tables
#' rmse_table()
#'
#' # Generate table for a single parameter
#' rmse_table("beta")
#' }
rmse_table <- function(parameter = c("alpha", "beta", "gamma"), data) {
  # If parameter is vector of all three, generate tables for all
  if (
    length(parameter) == 3 && all(parameter %in% c("alpha", "beta", "gamma"))
  ) {
    if (missing(data)) {
      return(
        purrr::map(parameter, ~ rmse_table(.x)) %>% purrr::set_names(parameter)
      )
    } else {
      return(
        purrr::map(parameter, ~ rmse_table(.x, data)) %>%
          purrr::set_names(parameter)
      )
    }
  }

  # Validate parameter
  if (!parameter %in% c("alpha", "beta", "gamma")) {
    stop("parameter must be one of: 'alpha', 'beta', 'gamma'")
  }

  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Filter to specific parameter and calculate RMSE
  df <- df %>%
    dplyr::filter(variable == parameter) %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      squared_error
    ) %>%
    # Set factor orders
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      rmse = sqrt(mean(squared_error, na.rm = TRUE)),
      .groups = "drop"
    )

  # Pivot to wide format
  table_data <- df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = rmse,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, censoring)

  # Create gt table
  rmse_tbl <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    tab_row_group(
      label = "n = 10,000",
      rows = n_obs == 10000
    ) %>%
    tab_row_group(
      label = "n = 2,000",
      rows = n_obs == 2000
    ) %>%
    tab_row_group(
      label = "n = 200",
      rows = n_obs == 200
    ) %>%
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    cols_hide(n_obs) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 3
    ) %>%
    tab_header(
      title = paste0("RMSE: ", parameter)
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  rmse_tbl %>% as_latex() %>% as.character()
}

#' MCSE Table
#'
#' Creates a gt table showing Monte Carlo standard error (MCSE) of posterior
#' means by method, sample size, censoring, and weight type.
#'
#' @param parameter Character vector of parameter names. Default is
#'   c("alpha", "beta", "gamma"), which generates tables for all three
#'   parameters and returns a named list.
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object (single parameter) or named list of gt tables
#'   (all three parameters)
#'
#' @examples
#' \dontrun{
#' # Generate all three parameter tables
#' mcse_table()
#'
#' # Generate table for a single parameter
#' mcse_table("gamma")
#' }
mcse_table <- function(parameter = c("alpha", "beta", "gamma"), data) {
  # If parameter is vector of all three, generate tables for all
  if (
    length(parameter) == 3 && all(parameter %in% c("alpha", "beta", "gamma"))
  ) {
    if (missing(data)) {
      return(
        purrr::map(parameter, ~ mcse_table(.x)) %>% purrr::set_names(parameter)
      )
    } else {
      return(
        purrr::map(parameter, ~ mcse_table(.x, data)) %>%
          purrr::set_names(parameter)
      )
    }
  }

  # Validate parameter
  if (!parameter %in% c("alpha", "beta", "gamma")) {
    stop("parameter must be one of: 'alpha', 'beta', 'gamma'")
  }

  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Filter to specific parameter and calculate MCSE
  df <- df %>%
    dplyr::filter(variable == parameter) %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      mean
    ) %>%
    # Set factor orders
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      mcse = sd(mean, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Pivot to wide format
  table_data <- df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = mcse,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, censoring)

  # Create gt table
  mcse_tbl <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    tab_row_group(
      label = "n = 10,000",
      rows = n_obs == 10000
    ) %>%
    tab_row_group(
      label = "n = 2,000",
      rows = n_obs == 2000
    ) %>%
    tab_row_group(
      label = "n = 200",
      rows = n_obs == 200
    ) %>%
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    cols_hide(n_obs) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 4
    ) %>%
    tab_header(
      title = paste0("MCSE (of posterior mean): ", parameter)
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  mcse_tbl %>% as_latex() %>% as.character()
}

#' Standard Deviation Table
#'
#' Creates a gt table showing standard deviation of parameter estimates across
#' replicates by method, sample size, censoring, and weight type.
#'
#' @param parameter Character vector of parameter names. Default is
#'   c("alpha", "beta", "gamma"), which generates tables for all three
#'   parameters and returns a named list.
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object (single parameter) or named list of gt tables
#'   (all three parameters)
#'
#' @examples
#' \dontrun{
#' # Generate all three parameter tables
#' sd_table()
#'
#' # Generate table for a single parameter
#' sd_table("alpha")
#' }
sd_table <- function(parameter = c("alpha", "beta", "gamma"), data) {
  # If parameter is vector of all three, generate tables for all
  if (
    length(parameter) == 3 && all(parameter %in% c("alpha", "beta", "gamma"))
  ) {
    if (missing(data)) {
      return(
        purrr::map(parameter, ~ sd_table(.x)) %>% purrr::set_names(parameter)
      )
    } else {
      return(
        purrr::map(parameter, ~ sd_table(.x, data)) %>%
          purrr::set_names(parameter)
      )
    }
  }

  # Validate parameter
  if (!parameter %in% c("alpha", "beta", "gamma")) {
    stop("parameter must be one of: 'alpha', 'beta', 'gamma'")
  }

  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Filter to specific parameter and calculate SD across replicates
  df <- df %>%
    dplyr::filter(variable == parameter) %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      mean
    ) %>%
    # Set factor orders
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      sd_across_reps = sd(mean, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to wide format
  table_data <- df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = sd_across_reps,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, censoring)

  # Create gt table
  sd_tbl <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    tab_row_group(
      label = "n = 10,000",
      rows = n_obs == 10000
    ) %>%
    tab_row_group(
      label = "n = 2,000",
      rows = n_obs == 2000
    ) %>%
    tab_row_group(
      label = "n = 200",
      rows = n_obs == 200
    ) %>%
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    cols_hide(n_obs) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 3
    ) %>%
    tab_header(
      title = paste0("SD across replicates: ", parameter)
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  sd_tbl %>% as_latex() %>% as.character()
}

#' CI Width and Coverage Table
#'
#' Creates a gt table showing both credible interval width and coverage
#' percentage by method, sample size, censoring, and weight type.
#'
#' @param parameter Character vector of parameter names. Default is
#'   c("alpha", "beta", "gamma"), which generates tables for all three
#'   parameters and returns a named list.
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object (single parameter) or named list of gt tables
#'   (all three parameters)
#'
#' @examples
#' \dontrun{
#' # Generate all three parameter tables
#' ci_coverage_table()
#'
#' # Generate table for a single parameter
#' ci_coverage_table("beta")
#' }
ci_coverage_table <- function(parameter = c("alpha", "beta", "gamma"), data) {
  # If parameter is vector of all three, generate tables for all
  if (
    length(parameter) == 3 && all(parameter %in% c("alpha", "beta", "gamma"))
  ) {
    if (missing(data)) {
      return(
        purrr::map(parameter, ~ ci_coverage_table(.x)) %>%
          purrr::set_names(parameter)
      )
    } else {
      return(
        purrr::map(parameter, ~ ci_coverage_table(.x, data)) %>%
          purrr::set_names(parameter)
      )
    }
  }

  # Validate parameter
  if (!parameter %in% c("alpha", "beta", "gamma")) {
    stop("parameter must be one of: 'alpha', 'beta', 'gamma'")
  }

  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Filter to specific parameter and calculate CI width
  ci_width_df <- df %>%
    dplyr::filter(variable == parameter) %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      ci_width
    ) %>%
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      mean_ci_width = mean(ci_width, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(metric = "CI Width")

  # Calculate coverage percentage
  coverage_df <- df %>%
    dplyr::filter(variable == parameter) %>%
    dplyr::select(
      method,
      n_obs,
      censoring,
      weight_type,
      contains_truth
    ) %>%
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    ) %>%
    dplyr::group_by(
      method,
      n_obs,
      censoring,
      weight_type
    ) %>%
    dplyr::summarise(
      coverage_pct = mean(contains_truth, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    dplyr::mutate(metric = "Coverage (%)")

  # Combine both metrics
  combined_df <- dplyr::bind_rows(
    ci_width_df %>% dplyr::rename(value = mean_ci_width),
    coverage_df %>% dplyr::rename(value = coverage_pct)
  )

  # Pivot to wide format
  table_data <- combined_df %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = value,
      names_sep = "_"
    ) %>%
    dplyr::arrange(n_obs, metric, censoring)

  # Create gt table
  ci_cov_tbl <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    # Row groups for each sample size and metric combination
    tab_row_group(
      label = "Coverage (%) - n = 10,000",
      rows = n_obs == 10000 & metric == "Coverage (%)"
    ) %>%
    tab_row_group(
      label = "CI Width - n = 10,000",
      rows = n_obs == 10000 & metric == "CI Width"
    ) %>%
    tab_row_group(
      label = "Coverage (%) - n = 2,000",
      rows = n_obs == 2000 & metric == "Coverage (%)"
    ) %>%
    tab_row_group(
      label = "CI Width - n = 2,000",
      rows = n_obs == 2000 & metric == "CI Width"
    ) %>%
    tab_row_group(
      label = "Coverage (%) - n = 200",
      rows = n_obs == 200 & metric == "Coverage (%)"
    ) %>%
    tab_row_group(
      label = "CI Width - n = 200",
      rows = n_obs == 200 & metric == "CI Width"
    ) %>%
    cols_label(
      censoring = "Censoring",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    cols_hide(c(n_obs, metric)) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      rows = metric == "CI Width",
      decimals = 2
    ) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      rows = metric == "Coverage (%)",
      decimals = 1
    ) %>%
    tab_header(
      title = paste0("CI Width & Coverage: ", parameter)
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  ci_cov_tbl %>% as_latex() %>% as.character()
}

#' Sample Size Scaling Table
#'
#' Creates a gt table showing scaling ratios across sample sizes for runtime
#' or ESS per second metrics by method, censoring, and weight type.
#'
#' @param metric Character specifying the metric to analyze. Either "runtime"
#'   or "ess_per_sec". Default is c("runtime", "ess_per_sec").
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#'
#' @return A gt table object showing 10x (n=2000/200) and 5x (n=10000/2000)
#'   scaling ratios
#'
#' @examples
#' \dontrun{
#' # Runtime scaling table
#' scaling_table("runtime")
#'
#' # ESS per second scaling table
#' scaling_table("ess_per_sec")
#' }
scaling_table <- function(metric = c("runtime", "ess_per_sec"), data) {
  # Validate metric
  metric <- match.arg(metric)

  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Calculate the appropriate metric
  if (metric == "runtime") {
    metric_data <- df %>%
      dplyr::select(
        method,
        n_obs,
        censoring,
        weight_type,
        total_time
      ) %>%
      dplyr::distinct(total_time, .keep_all = TRUE) %>%
      dplyr::group_by(
        method,
        n_obs,
        censoring,
        weight_type
      ) %>%
      dplyr::summarise(
        metric_value = mean(total_time, na.rm = TRUE),
        .groups = "drop"
      )
    metric_label <- "Runtime"
  } else {
    metric_data <- df %>%
      dplyr::select(
        method,
        n_obs,
        censoring,
        weight_type,
        ess,
        total_time
      ) %>%
      dplyr::mutate(
        ess_per_sec = ess / total_time
      ) %>%
      dplyr::distinct(ess_per_sec, .keep_all = TRUE) %>%
      dplyr::group_by(
        method,
        n_obs,
        censoring,
        weight_type
      ) %>%
      dplyr::summarise(
        metric_value = mean(ess_per_sec, na.rm = TRUE),
        .groups = "drop"
      )
    metric_label <- "ESS per Second"
  }

  # Set factor orders
  metric_data <- metric_data %>%
    dplyr::mutate(
      weight_type = factor(weight_type, levels = c("none", "low", "high")),
      method = factor(method, levels = c("mh", "hmc"))
    )

  # Pivot to wide format by sample size to calculate ratios
  wide_data <- metric_data %>%
    tidyr::pivot_wider(
      names_from = n_obs,
      values_from = metric_value,
      names_prefix = "n"
    )

  # Calculate ratios
  ratio_data <- wide_data %>%
    dplyr::mutate(
      ratio_10x = n2000 / n200,
      ratio_5x = n10000 / n2000
    ) %>%
    dplyr::select(method, censoring, weight_type, ratio_10x, ratio_5x)

  # Reshape to long format with ratio type
  ratio_long <- ratio_data %>%
    tidyr::pivot_longer(
      cols = c(ratio_10x, ratio_5x),
      names_to = "ratio_type",
      values_to = "ratio_value"
    ) %>%
    dplyr::mutate(
      ratio_label = dplyr::case_when(
        ratio_type == "ratio_10x" ~ "10x (n=2000/200)",
        ratio_type == "ratio_5x" ~ "5x (n=10000/2000)"
      )
    )

  # Pivot to final wide format with method_weight columns
  table_data <- ratio_long %>%
    tidyr::pivot_wider(
      names_from = c(method, weight_type),
      values_from = ratio_value,
      names_sep = "_"
    ) %>%
    dplyr::arrange(censoring, ratio_type)

  # Create gt table
  scaling_tbl <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "MH",
      columns = starts_with("mh_")
    ) %>%
    tab_spanner(
      label = "HMC",
      columns = starts_with("hmc_")
    ) %>%
    # Row grouping by censoring level
    tab_row_group(
      label = "c = 0.5",
      rows = censoring == 0.5
    ) %>%
    tab_row_group(
      label = "c = 0.3",
      rows = censoring == 0.3
    ) %>%
    tab_row_group(
      label = "c = 0.1",
      rows = censoring == 0.1
    ) %>%
    cols_label(
      ratio_label = "Ratio",
      mh_none = "None",
      mh_low = "Low",
      mh_high = "High",
      hmc_none = "None",
      hmc_low = "Low",
      hmc_high = "High"
    ) %>%
    cols_hide(c(censoring, ratio_type)) %>%
    fmt_number(
      columns = starts_with(c("mh_", "hmc_")),
      decimals = 2
    ) %>%
    tab_header(
      title = paste0("Scaling with Sample Size: ", metric_label)
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  scaling_tbl %>% as_latex() %>% as.character()
}

#' Save All Tables as LaTeX
#'
#' Generates all table functions and saves their LaTeX output to a text file.
#' Tables are separated by blank lines in the output file.
#'
#' @param data Optional data frame. If not provided, loads from
#'   outputs/combined_results/combined_summaries.rds
#' @param output_file Path to output file. Default is
#'   "outputs/tables/latex_tables.txt"
#'
#' @return Character string with path to the saved file
#'
#' @examples
#' \dontrun{
#' # Generate all tables and save to default location
#' save_all_tables_latex()
#'
#' # Use custom data and output location
#' save_all_tables_latex(my_data, "custom/path/tables.txt")
#' }
save_all_tables_latex <- function(data, output_file = "outputs/tables/latex_tables.txt") {
  # Load data if not provided
  if (missing(data)) {
    df <- readRDS("outputs/combined_results/combined_summaries.rds")
  } else {
    df <- data
  }

  # Initialize list to store all table outputs
  latex_outputs <- list()

  # Generate runtime table
  message("Generating mean runtime table...")
  latex_outputs[["runtime"]] <- mean_runtime(df)

  # Generate ESS per second table
  message("Generating mean ESS per second table...")
  latex_outputs[["ess_per_sec"]] <- mean_ess_per_sec(df)

  # Generate bias tables for all parameters
  message("Generating bias tables...")
  bias_tables <- bias_table(c("alpha", "beta", "gamma"), df)
  latex_outputs[["bias_alpha"]] <- bias_tables$alpha
  latex_outputs[["bias_beta"]] <- bias_tables$beta
  latex_outputs[["bias_gamma"]] <- bias_tables$gamma

  # Generate RMSE tables for all parameters
  message("Generating RMSE tables...")
  rmse_tables <- rmse_table(c("alpha", "beta", "gamma"), df)
  latex_outputs[["rmse_alpha"]] <- rmse_tables$alpha
  latex_outputs[["rmse_beta"]] <- rmse_tables$beta
  latex_outputs[["rmse_gamma"]] <- rmse_tables$gamma

  # Generate MCSE tables for all parameters
  message("Generating MCSE tables...")
  mcse_tables <- mcse_table(c("alpha", "beta", "gamma"), df)
  latex_outputs[["mcse_alpha"]] <- mcse_tables$alpha
  latex_outputs[["mcse_beta"]] <- mcse_tables$beta
  latex_outputs[["mcse_gamma"]] <- mcse_tables$gamma

  # Generate SD tables for all parameters
  message("Generating SD tables...")
  sd_tables <- sd_table(c("alpha", "beta", "gamma"), df)
  latex_outputs[["sd_alpha"]] <- sd_tables$alpha
  latex_outputs[["sd_beta"]] <- sd_tables$beta
  latex_outputs[["sd_gamma"]] <- sd_tables$gamma

  # Generate CI coverage tables for all parameters
  message("Generating CI width & coverage tables...")
  ci_tables <- ci_coverage_table(c("alpha", "beta", "gamma"), df)
  latex_outputs[["ci_coverage_alpha"]] <- ci_tables$alpha
  latex_outputs[["ci_coverage_beta"]] <- ci_tables$beta
  latex_outputs[["ci_coverage_gamma"]] <- ci_tables$gamma

  # Generate scaling tables
  message("Generating scaling tables...")
  latex_outputs[["scaling_runtime"]] <- scaling_table("runtime", df)
  latex_outputs[["scaling_ess_per_sec"]] <- scaling_table("ess_per_sec", df)

  # Combine all outputs with blank lines between tables
  combined_latex <- paste(latex_outputs, collapse = "\n\n")

  # Create output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created directory: ", output_dir)
  }

  # Write to file
  writeLines(combined_latex, output_file)

  message("Successfully saved ", length(latex_outputs), " tables to: ", output_file)

  return(invisible(output_file))
}
