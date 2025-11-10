#' Convert gt table to LaTeX with standard float positioning
#'
#' Ensures every exported table opts into the `!htbp` placement directive so
#' tables do not float unexpectedly in the manuscript.
#'
#' @param tbl A `gt_tbl` object.
#'
#' @return Character vector containing the rendered LaTeX fragment.
#' @keywords internal
gt_to_latex <- function(tbl) {
  tbl %>%
    tab_options(latex.tbl.pos = "!htbp") %>%
    as_latex() %>%
    as.character()
}

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
  runtime_table %>% gt_to_latex()
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
  ess_table %>% gt_to_latex()
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

  bias_tbl %>% gt_to_latex()
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

  rmse_tbl %>% gt_to_latex()
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

  mcse_tbl %>% gt_to_latex()
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

  sd_tbl %>% gt_to_latex()
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

  ci_cov_tbl %>% gt_to_latex()
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

  scaling_tbl %>% gt_to_latex()
}

# ZIMPHIA Tables --------------------------------------------------------------

#' Get canonical ZIMPHIA output paths
#' @keywords internal
get_zimphia_paths <- function(base_dir = "mcmc_outputs/zimphia") {
  list(
    prepared = file.path(base_dir, "zimphia_prepared_data.rds"),
    method_comparison = file.path(base_dir, "zimphia_method_comparison.csv"),
    runtime_comparison = file.path(base_dir, "zimphia_runtime_comparison.csv"),
    hmc_summary = file.path(
      base_dir,
      "hmc",
      "summaries",
      "zimphia_hmc_summary.csv"
    ),
    mh_summary = file.path(
      base_dir,
      "mh",
      "summaries",
      "zimphia_mh_summary.csv"
    ),
    hmc_diag = file.path(
      base_dir,
      "hmc",
      "diagnostics",
      "zimphia_hmc_diagnostics.csv"
    ),
    mh_diag = file.path(
      base_dir,
      "mh",
      "diagnostics",
      "zimphia_mh_diagnostics.csv"
    ),
    hmc_draws = file.path(base_dir, "hmc", "draws", "zimphia_hmc_draws.rds"),
    mh_draws = file.path(base_dir, "mh", "draws", "zimphia_mh_draws.rds"),
    mh_fit = file.path(base_dir, "mh", "fits", "zimphia_mh_fit.rds")
  )
}

#' Load analytic ZIMPHIA dataset
#' @keywords internal
load_zimphia_prepared_data <- function(base_dir = "mcmc_outputs/zimphia") {
  paths <- get_zimphia_paths(base_dir)
  if (!file.exists(paths$prepared)) {
    stop("Prepared ZIMPHIA data not found at: ", paths$prepared)
  }
  readRDS(paths$prepared)
}

#' Load ZIMPHIA summary tables
#' @keywords internal
load_zimphia_summaries <- function(base_dir = "mcmc_outputs/zimphia") {
  paths <- get_zimphia_paths(base_dir)
  for (pth in c(paths$hmc_summary, paths$mh_summary)) {
    if (!file.exists(pth)) {
      stop("Summary file not found: ", pth)
    }
  }
  list(
    hmc = readr::read_csv(paths$hmc_summary, show_col_types = FALSE),
    mh = readr::read_csv(paths$mh_summary, show_col_types = FALSE)
  )
}

#' Load ZIMPHIA diagnostic tables
#' @keywords internal
load_zimphia_diagnostics <- function(base_dir = "mcmc_outputs/zimphia") {
  paths <- get_zimphia_paths(base_dir)
  for (pth in c(paths$hmc_diag, paths$mh_diag)) {
    if (!file.exists(pth)) {
      stop("Diagnostic file not found: ", pth)
    }
  }
  list(
    hmc = readr::read_csv(paths$hmc_diag, show_col_types = FALSE),
    mh = readr::read_csv(paths$mh_diag, show_col_types = FALSE)
  )
}

#' Load MH fit object for acceptance statistics
#' @keywords internal
load_zimphia_mh_fit <- function(base_dir = "mcmc_outputs/zimphia") {
  paths <- get_zimphia_paths(base_dir)
  if (!file.exists(paths$mh_fit)) {
    stop("MH fit object not found: ", paths$mh_fit)
  }
  readRDS(paths$mh_fit)
}

#' Summarise ZIMPHIA missingness flow across inclusion criteria
#' @keywords internal
summarise_zimphia_missing_flow <- function(
  adultbio_file = file.path(
    "ZIMPHIA",
    "ZIMPHIA 2020 Datasets (CSV)",
    "zimphia2020adultbio.csv"
  ),
  adultind_file = file.path(
    "ZIMPHIA",
    "ZIMPHIA 2020 Datasets (CSV)",
    "zimphia2020adultind.csv"
  )
) {
  required_files <- c(adultbio_file, adultind_file)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    stop(
      "Missing ZIMPHIA source files: ",
      paste(missing_files, collapse = ", ")
    )
  }

  adultbio <- readr::read_csv(
    adultbio_file,
    show_col_types = FALSE,
    col_select = c(
      personid,
      hivstatusfinal,
      btwt0,
      bt_status,
      age,
      gender
    )
  )

  adultind <- readr::read_csv(
    adultind_file,
    show_col_types = FALSE,
    col_select = c(personid, firstsxage, sexever, hivtfposy)
  )

  joined <- adultbio %>%
    dplyr::left_join(adultind, by = "personid")

  records <- list(
    tibble::tibble(
      step = "Merged adultbio + adultind",
      remaining = nrow(joined),
      removed = NA_integer_,
      detail = "Baseline joined sample"
    )
  )

  apply_step <- function(data, expr, label, detail_fmt = NULL) {
    before <- nrow(data)
    filtered <- dplyr::filter(data, {{ expr }})
    removed <- before - nrow(filtered)
    detail <- if (is.null(detail_fmt)) {
      sprintf("Removed %s participants", scales::comma(removed))
    } else {
      detail_fmt(before, removed, filtered)
    }
    records <<- append(
      records,
      list(
        tibble::tibble(
          step = label,
          remaining = nrow(filtered),
          removed = removed,
          detail = detail
        )
      )
    )
    filtered
  }

  flow <- joined %>%
    apply_step(bt_status == 1, "Valid biomarker result") %>%
    apply_step(age >= 15, "Age ≥ 15 years") %>%
    apply_step(sexever == 1, "Ever sexually active") %>%
    apply_step(!is.na(firstsxage), "Non-missing age at debut") %>%
    apply_step(!is.na(gender), "Recorded gender") %>%
    apply_step(
      !(hivstatusfinal == 1 & is.na(hivtfposy)),
      "HIV+ with documented first positive test",
      detail_fmt = function(before, removed, filtered) {
        sprintf(
          "Removed %s HIV+ lacking first-positive date",
          scales::comma(removed)
        )
      }
    ) %>%
    apply_step(!is.na(btwt0) & btwt0 > 0, "Positive survey weight")

  flow_summary <- dplyr::bind_rows(records) %>%
    dplyr::mutate(
      pct_remaining = 100 * remaining / remaining[1]
    )

  list(
    final_data = flow,
    flow = flow_summary
  )
}

#' Table 3.6 — ZIMPHIA 2020 Analysis Sample Characteristics
#'
#' Summarises analytic sample composition, interval structure, weight dispersion,
#' and missing-data attrition counts for the ZIMPHIA 2020 analysis dataset.
#'
#' @param base_dir Path to ZIMPHIA output directory (default
#'   "mcmc_outputs/zimphia").
#' @param adultbio_file Path to raw adult biomarker CSV.
#' @param adultind_file Path to raw adult individual CSV.
#'
#' @return Character representation of the gt table in LaTeX format.
#' @export
table_3_6_sample_characteristics <- function(
  base_dir = "mcmc_outputs/zimphia",
  adultbio_file = file.path(
    "ZIMPHIA",
    "ZIMPHIA 2020 Datasets (CSV)",
    "zimphia2020adultbio.csv"
  ),
  adultind_file = file.path(
    "ZIMPHIA",
    "ZIMPHIA 2020 Datasets (CSV)",
    "zimphia2020adultind.csv"
  )
) {
  dat <- load_zimphia_prepared_data(base_dir)
  flow_info <- summarise_zimphia_missing_flow(adultbio_file, adultind_file)$flow

  n_total <- nrow(dat)
  hiv_counts <- dat %>%
    dplyr::mutate(
      hiv_status = dplyr::case_when(
        hivstatusfinal == 1 ~ "HIV-positive",
        hivstatusfinal == 2 ~ "HIV-negative",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::count(hiv_status) %>%
    dplyr::mutate(pct = n / n_total)

  gender_counts <- dat %>%
    dplyr::mutate(
      gender_label = dplyr::case_when(
        X1 == 1 ~ "Female (X1 = 1)",
        X1 == 0 ~ "Male (X1 = 0)",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::count(gender_label) %>%
    dplyr::mutate(pct = n / n_total)

  interval_df <- dat %>%
    dplyr::mutate(
      interval_type = dplyr::if_else(
        is.infinite(R),
        "Right-censored (HIV−)",
        "Interval-censored (HIV+)"
      ),
      width = dplyr::if_else(is.infinite(R), NA_real_, pmax(R - L, 0))
    )

  width_stats <- interval_df %>%
    dplyr::filter(!is.na(width)) %>%
    dplyr::summarise(
      median_width = stats::median(width),
      p25 = stats::quantile(width, 0.25),
      p75 = stats::quantile(width, 0.75),
      mean_width = mean(width),
      .groups = "drop"
    )

  weight_stats <- dat %>%
    dplyr::summarise(
      median = stats::median(weight),
      p25 = stats::quantile(weight, 0.25),
      p75 = stats::quantile(weight, 0.75),
      min_w = min(weight),
      max_w = max(weight),
      cv = stats::sd(weight) / mean(weight),
      kish_ess = (sum(weight)^2) / sum(weight^2),
      .groups = "drop"
    )

  format_count <- function(n, pct = NULL, digits = 1) {
    out <- scales::comma(n)
    if (!is.null(pct)) {
      out <- sprintf(
        "%s (%s)",
        out,
        scales::percent(pct, accuracy = digits)
      )
    }
    out
  }

  section_sample <- tibble::tibble(
    section = "Sample size and composition",
    metric = c(
      "Analytic sample (unweighted)",
      hiv_counts$hiv_status,
      gender_counts$gender_label
    ),
    estimate = c(
      scales::comma(n_total),
      purrr::map2_chr(hiv_counts$n, hiv_counts$pct, format_count),
      purrr::map2_chr(gender_counts$n, gender_counts$pct, format_count)
    ),
    detail = c(
      "Final dataset after all filters",
      rep(
        "",
        length(hiv_counts$hiv_status) + length(gender_counts$gender_label)
      )
    )
  )

  section_interval <- tibble::tibble(
    section = "Interval censoring distribution",
    metric = c(
      "Right-censored share (HIV−)",
      "Median interval width (HIV+)",
      "IQR of interval width",
      "Mean lower bound L",
      "Mean upper bound R (HIV+)"
    ),
    estimate = c(
      scales::percent(
        mean(is.infinite(dat$R)),
        accuracy = 0.1
      ),
      scales::number(width_stats$median_width, accuracy = 0.1),
      sprintf(
        "%s – %s",
        scales::number(width_stats$p25, accuracy = 0.1),
        scales::number(width_stats$p75, accuracy = 0.1)
      ),
      scales::number(mean(dat$L), accuracy = 0.1),
      scales::number(
        mean(dat$R[is.finite(dat$R)]),
        accuracy = 0.1
      )
    ),
    detail = c(
      "Proportion with R = ∞ (still HIV− at survey)",
      "Years between debut and first positive test",
      "25th–75th percentile of widths",
      "Average age at sexual debut",
      "Average age at first HIV+ evidence"
    )
  )

  section_weights <- tibble::tibble(
    section = "Survey weight dispersion",
    metric = c(
      "Median weight",
      "Weight IQR",
      "Min / Max weight",
      "Coefficient of variation",
      "Kish effective sample size"
    ),
    estimate = c(
      scales::number(weight_stats$median, accuracy = 0.01),
      sprintf(
        "%s – %s",
        scales::number(weight_stats$p25, accuracy = 0.01),
        scales::number(weight_stats$p75, accuracy = 0.01)
      ),
      sprintf(
        "%s / %s",
        scales::number(weight_stats$min_w, accuracy = 0.001),
        scales::number(weight_stats$max_w, accuracy = 0.001)
      ),
      scales::number(weight_stats$cv, accuracy = 0.01),
      scales::comma(weight_stats$kish_ess)
    ),
    detail = c(
      "Weights normalised to sum to N",
      "Inter-quartile range",
      "Range of respondent weights",
      "sd(weight) / mean(weight)",
      " (∑w)^2 / ∑w^2"
    )
  )

  section_missing <- flow_info %>%
    dplyr::filter(!is.na(removed)) %>%
    dplyr::mutate(
      section = "Missing-data and quality filters",
      metric = step,
      estimate = scales::comma(removed),
      detail = sprintf(
        "%s remaining (%.1f%% of merged sample)",
        scales::comma(remaining),
        pct_remaining
      )
    ) %>%
    dplyr::select(section, metric, estimate, detail)

  table_df <- dplyr::bind_rows(
    section_sample,
    section_interval,
    section_weights,
    section_missing
  )

  tbl <- table_df %>%
    gt(groupname_col = "section", rowname_col = "metric") %>%
    cols_label(
      metric = "",
      estimate = "Value",
      detail = "Detail"
    ) %>%
    tab_header(
      title = "ZIMPHIA 2020 analysis sample characteristics"
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold"
    )

  tbl %>% gt_to_latex()
}

#' Table 3.7 — Sampler performance on ZIMPHIA data
#'
#' Compares runtime, ESS/s, R-hat violations, divergences, and MH acceptance
#' rate between HMC and MH fits on the ZIMPHIA dataset.
#'
#' @param base_dir ZIMPHIA results directory. Default "mcmc_outputs/zimphia".
#'
#' @return Character representation of the gt table in LaTeX format.
#' @export
table_3_7_sampler_performance <- function(
  base_dir = "mcmc_outputs/zimphia"
) {
  summaries <- load_zimphia_summaries(base_dir)
  diagnostics <- load_zimphia_diagnostics(base_dir)

  hmc_runtime <- diagnostics$hmc$runtime_secs[1]
  mh_runtime <- diagnostics$mh$runtime_secs[1]

  hmc_alpha <- summaries$hmc %>% dplyr::filter(variable == "alpha")
  hmc_beta <- summaries$hmc %>% dplyr::filter(variable == "beta")
  mh_alpha <- summaries$mh %>% dplyr::filter(variable == "alpha")
  mh_beta <- summaries$mh %>% dplyr::filter(variable == "beta")

  ess_alpha_hmc <- hmc_alpha$ess_bulk[1]
  ess_beta_hmc <- hmc_beta$ess_bulk[1]
  ess_alpha_mh <- mh_alpha$ess[1]
  ess_beta_mh <- mh_beta$ess[1]

  rhat_violations_hmc <- sum(summaries$hmc$rhat > 1.01, na.rm = TRUE)
  rhat_violations_mh <- sum(summaries$mh$rhat > 1.01, na.rm = TRUE)

  hmc_divergences <- diagnostics$hmc$n_divergences[1]

  if (!requireNamespace("coda", quietly = TRUE)) {
    stop("Package 'coda' required for MH acceptance diagnostics.")
  }
  mh_fit <- load_zimphia_mh_fit(base_dir)
  rejection <- coda::rejectionRate(mh_fit)
  mh_acceptance <- 1 - mean(rejection, na.rm = TRUE)

  tbl_df <- tibble::tibble(
    metric = c(
      "Runtime (minutes)",
      "ESS/s (α)",
      "ESS/s (β)",
      "Split R̂ > 1.01",
      "HMC divergences",
      "MH acceptance rate",
      "HMC:MH ratio"
    ),
    hmc = c(
      hmc_runtime / 60,
      ess_alpha_hmc / hmc_runtime,
      ess_beta_hmc / hmc_runtime,
      rhat_violations_hmc,
      hmc_divergences,
      NA,
      (hmc_runtime / 60) / (mh_runtime / 60)
    ),
    mh = c(
      mh_runtime / 60,
      ess_alpha_mh / mh_runtime,
      ess_beta_mh / mh_runtime,
      rhat_violations_mh,
      0,
      mh_acceptance,
      NA
    ),
    ratio = c(
      hmc_runtime / mh_runtime,
      (ess_alpha_hmc / hmc_runtime) / (ess_alpha_mh / mh_runtime),
      (ess_beta_hmc / hmc_runtime) / (ess_beta_mh / mh_runtime),
      NA,
      NA,
      NA,
      NA
    )
  )

  fmt_cols <- tbl_df %>%
    gt() %>%
    fmt_number(
      columns = c(hmc, mh, ratio),
      rows = metric %in%
        c("Runtime (minutes)", "ESS/s (α)", "ESS/s (β)", "HMC:MH ratio"),
      decimals = 2
    ) %>%
    fmt_number(
      columns = c(hmc, mh),
      rows = metric == "MH acceptance rate",
      decimals = 3
    ) %>%
    fmt_number(
      columns = c(hmc, mh),
      rows = metric == "Split R̂ > 1.01",
      decimals = 0
    ) %>%
    cols_label(
      hmc = "HMC",
      mh = "MH",
      ratio = "HMC ÷ MH"
    ) %>%
    tab_header(
      title = "Sampler performance on ZIMPHIA 2020 data"
    ) %>%
    tab_options(
      heading.align = "left"
    )

  fmt_cols %>% gt_to_latex()
}

#' Table 3.8 — Parameter estimates from ZIMPHIA application
#'
#' Displays posterior medians and 95% CrIs for core parameters with MH and
#' HMC side-by-side.
#'
#' @param base_dir ZIMPHIA results directory. Default "mcmc_outputs/zimphia".
#'
#' @return Character representation of the gt table in LaTeX format.
#' @export
table_3_8_parameter_estimates <- function(
  base_dir = "mcmc_outputs/zimphia"
) {
  summaries <- load_zimphia_summaries(base_dir)

  param_map <- tibble::tibble(
    variable = c("alpha", "beta", "gamma"),
    label = c(
      "Baseline median (α)",
      "Gender effect (β, female vs male)",
      "Shape (γ)"
    ),
    group = c("Baseline", "Covariate", "Shape"),
    digits = c(2, 3, 2)
  )

  fmt_interval <- function(median, lo, hi, digits) {
    sprintf(
      "%s (%s, %s)",
      scales::number(median, accuracy = 10^-digits),
      scales::number(lo, accuracy = 10^-digits),
      scales::number(hi, accuracy = 10^-digits)
    )
  }

  tidy_summary <- function(df, method_label) {
    df %>%
      dplyr::select(variable, median, q2.5, q97.5) %>%
      dplyr::mutate(method = method_label)
  }

  combined <- dplyr::bind_rows(
    tidy_summary(summaries$hmc, "HMC"),
    tidy_summary(summaries$mh, "MH")
  )

  table_df <- param_map %>%
    dplyr::left_join(
      combined %>%
        dplyr::filter(method == "HMC") %>%
        dplyr::rename(
          hmc_median = median,
          hmc_lo = q2.5,
          hmc_hi = q97.5
        ),
      by = "variable"
    ) %>%
    dplyr::left_join(
      combined %>%
        dplyr::filter(method == "MH") %>%
        dplyr::rename(
          mh_median = median,
          mh_lo = q2.5,
          mh_hi = q97.5
        ),
      by = "variable"
    ) %>%
    dplyr::mutate(
      hmc_text = fmt_interval(hmc_median, hmc_lo, hmc_hi, digits),
      mh_text = fmt_interval(mh_median, mh_lo, mh_hi, digits)
    )

  tbl <- table_df %>%
    dplyr::select(label, hmc_text, mh_text, group) %>%
    gt(groupname_col = "group", rowname_col = "label") %>%
    cols_label(
      label = "Parameter",
      hmc_text = "HMC median (95% CrI)",
      mh_text = "MH median (95% CrI)"
    ) %>%
    tab_header(
      title = "Posterior estimates for ZIMPHIA application"
    ) %>%
    tab_options(
      row_group.font.weight = "bold",
      heading.align = "left"
    )

  tbl %>% gt_to_latex()
}

#' Table 3.9 — ZIMPHIA vs simulation scalability check
#'
#' Compares predicted runtime and ESS/s from simulation scaling laws against the
#' observed ZIMPHIA fits, including observed/predicted ratios.
#'
#' @param base_dir Path to ZIMPHIA results directory.
#' @param efficiency_file Combined simulation efficiency summary file.
#' @param target_n Sample size used in ZIMPHIA application. If NULL, inferred
#'   from prepared data.
#'
#' @return Character representation of the gt table in LaTeX format.
#' @export
table_3_9_scalability_validation <- function(
  base_dir = "mcmc_outputs/zimphia",
  efficiency_file = "outputs/analysis/efficiency_comparisons.csv",
  target_n = NULL
) {
  summaries <- load_zimphia_summaries(base_dir)
  diagnostics <- load_zimphia_diagnostics(base_dir)

  if (is.null(target_n)) {
    target_n <- nrow(load_zimphia_prepared_data(base_dir))
  }

  observed_runtime <- tibble::tibble(
    method = c("HMC", "MH"),
    runtime = c(
      diagnostics$hmc$runtime_secs[1],
      diagnostics$mh$runtime_secs[1]
    )
  )

  observed_ess_per_sec <- tibble::tibble(
    method = c("HMC", "MH"),
    ess_per_sec = c(
      mean(summaries$hmc$ess_bulk, na.rm = TRUE) /
        diagnostics$hmc$runtime_secs[1],
      mean(summaries$mh$ess, na.rm = TRUE) /
        diagnostics$mh$runtime_secs[1]
    )
  )

  efficiency <- readr::read_csv(efficiency_file, show_col_types = FALSE)

  runtime_long <- efficiency %>%
    tidyr::pivot_longer(
      cols = c(mean_runtime_hmc, mean_runtime_mh),
      names_to = "method",
      values_to = "runtime"
    ) %>%
    dplyr::mutate(
      method = stringr::str_to_upper(
        stringr::str_replace(method, "mean_runtime_", "")
      )
    ) %>%
    dplyr::filter(!is.na(runtime), runtime > 0)

  ess_long <- efficiency %>%
    tidyr::pivot_longer(
      cols = c(mean_ess_per_sec_hmc, mean_ess_per_sec_mh),
      names_to = "method",
      values_to = "ess_per_sec"
    ) %>%
    dplyr::mutate(
      method = stringr::str_to_upper(
        stringr::str_replace(method, "mean_ess_per_sec_", "")
      )
    ) %>%
    dplyr::filter(!is.na(ess_per_sec), ess_per_sec > 0)

  predict_scaling <- function(df, value_col) {
    split <- df %>% dplyr::group_split(method)
    purrr::map_dfr(split, function(chunk) {
      if (nrow(chunk) < 2) {
        return(tibble::tibble(
          method = unique(chunk$method),
          predicted = NA_real_
        ))
      }
      fit <- stats::lm(
        stats::as.formula(paste0("log(", value_col, ") ~ log(n_obs)")),
        data = chunk
      )
      pred <- exp(stats::predict(
        fit,
        newdata = data.frame(n_obs = target_n)
      ))
      tibble::tibble(method = unique(chunk$method), predicted = pred)
    })
  }

  runtime_pred <- runtime_long %>%
    dplyr::group_by(method, n_obs) %>%
    dplyr::summarise(
      runtime = stats::median(runtime, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    predict_scaling("runtime")

  ess_pred <- ess_long %>%
    dplyr::group_by(method, n_obs) %>%
    dplyr::summarise(
      ess_per_sec = stats::median(ess_per_sec, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    predict_scaling("ess_per_sec")

  table_df <- observed_runtime %>%
    dplyr::left_join(
      runtime_pred,
      by = "method",
      suffix = c("_obs", "_pred")
    ) %>%
    dplyr::rename(runtime_obs = runtime, runtime_pred = predicted) %>%
    dplyr::left_join(
      observed_ess_per_sec %>%
        dplyr::rename(ess_per_sec_obs = ess_per_sec),
      by = "method"
    ) %>%
    dplyr::left_join(
      ess_pred %>%
        dplyr::rename(ess_per_sec_pred = predicted),
      by = "method"
    ) %>%
    dplyr::mutate(
      runtime_ratio = runtime_obs / runtime_pred,
      ess_ratio = ess_per_sec_obs / ess_per_sec_pred
    )

  tidy_rows <- tibble::tibble(
    metric = c("Runtime (minutes)", "Mean ESS/s"),
    hmc_pred = c(
      table_df$runtime_pred[table_df$method == "HMC"] / 60,
      table_df$ess_per_sec_pred[table_df$method == "HMC"]
    ),
    mh_pred = c(
      table_df$runtime_pred[table_df$method == "MH"] / 60,
      table_df$ess_per_sec_pred[table_df$method == "MH"]
    ),
    hmc_obs = c(
      table_df$runtime_obs[table_df$method == "HMC"] / 60,
      table_df$ess_per_sec_obs[table_df$method == "HMC"]
    ),
    mh_obs = c(
      table_df$runtime_obs[table_df$method == "MH"] / 60,
      table_df$ess_per_sec_obs[table_df$method == "MH"]
    ),
    hmc_ratio = c(
      table_df$runtime_ratio[table_df$method == "HMC"],
      table_df$ess_ratio[table_df$method == "HMC"]
    ),
    mh_ratio = c(
      table_df$runtime_ratio[table_df$method == "MH"],
      table_df$ess_ratio[table_df$method == "MH"]
    ),
    observed_hmc_mh = c(
      (table_df$runtime_obs[table_df$method == "HMC"] / 60) /
        (table_df$runtime_obs[table_df$method == "MH"] / 60),
      table_df$ess_per_sec_obs[table_df$method == "HMC"] /
        table_df$ess_per_sec_obs[table_df$method == "MH"]
    )
  )

  tbl <- tidy_rows %>%
    gt() %>%
    cols_label(
      metric = "",
      hmc_pred = "Predicted HMC",
      mh_pred = "Predicted MH",
      hmc_obs = "Observed HMC",
      mh_obs = "Observed MH",
      hmc_ratio = "Obs/Pred HMC",
      mh_ratio = "Obs/Pred MH",
      observed_hmc_mh = "Observed HMC ÷ MH"
    ) %>%
    fmt_number(
      columns = c(hmc_pred, mh_pred, hmc_obs, mh_obs),
      decimals = 2
    ) %>%
    fmt_number(
      columns = c(hmc_ratio, mh_ratio, observed_hmc_mh),
      decimals = 2
    ) %>%
    tab_header(
      title = sprintf(
        "ZIMPHIA vs simulation scalability (n = %s)",
        scales::comma(target_n)
      )
    ) %>%
    tab_options(
      heading.align = "left"
    )

  tbl %>% gt_to_latex()
}

#' Save ZIMPHIA tables to disk
#'
#' Convenience wrapper that generates Tables 3.6–3.9 and writes each to
#' outputs/tables as individual LaTeX fragments.
#'
#' @param output_dir Directory where tables should be written.
#' @param ... Additional arguments passed to individual table functions.
#'
#' @return Invisibly returns named vector of output paths.
#' @export
save_zimphia_tables <- function(
  output_dir = "outputs/tables",
  ...
) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  files <- c(
    table3_6 = file.path(output_dir, "table3_6_sample_characteristics.tex"),
    table3_7 = file.path(output_dir, "table3_7_sampler_performance.tex"),
    table3_8 = file.path(output_dir, "table3_8_parameter_estimates.tex"),
    table3_9 = file.path(output_dir, "table3_9_scalability_validation.tex")
  )

  writeLines(
    table_3_6_sample_characteristics(...),
    files["table3_6"]
  )
  writeLines(
    table_3_7_sampler_performance(...),
    files["table3_7"]
  )
  writeLines(
    table_3_8_parameter_estimates(...),
    files["table3_8"]
  )
  writeLines(
    table_3_9_scalability_validation(...),
    files["table3_9"]
  )

  invisible(files)
}

#' Save All Tables as LaTeX
#'
#' Generates all simulation and ZIMPHIA table functions and saves their
#' LaTeX output to a text file. Tables are separated by blank lines in the
#' output file.
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
save_all_tables_latex <- function(
  data,
  output_file = "outputs/tables/latex_tables.txt"
) {
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

  # Append ZIMPHIA application tables
  message("Generating ZIMPHIA tables...")
  latex_outputs[["zimphia_table_3_6"]] <- table_3_6_sample_characteristics()
  latex_outputs[["zimphia_table_3_7"]] <- table_3_7_sampler_performance()
  latex_outputs[["zimphia_table_3_8"]] <- table_3_8_parameter_estimates()
  latex_outputs[["zimphia_table_3_9"]] <- table_3_9_scalability_validation()

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

  message(
    "Successfully saved ",
    length(latex_outputs),
    " tables to: ",
    output_file
  )

  return(invisible(output_file))
}
