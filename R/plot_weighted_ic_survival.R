# ==============================================================================
# plot_weighted_ic_survival.R
#
# Production-quality R code for thesis-ready visualization of weighted
# interval-censored survival estimates compared to true log-logistic curves.
# Uses a "ghosted spaghetti + bright average + true" visualization style.
#
# Author: Generated for MSc Biostatistics Dissertation
# Purpose: Compare MH vs HMC for interval-censored data with survey weights
# ==============================================================================

#' @title Weighted Interval-Censored Survival Visualization
#' @description Functions to visualize parametric log-logistic survival
#'   estimates from interval-censored data with survey weights, comparing
#'   fitted curves across simulation replicates to true population curves.
#' @keywords internal
"_PACKAGE"

# Dependencies -----------------------------------------------------------------
# Required packages:
# - Base R (stats, utils, graphics)
# - tidyverse (dplyr, tidyr, purrr, ggplot2, stringr)
#   Note: ggplot2 provides viridis color scales via scale_color_viridis_c()
# - survival (for Surv() function)
# - icenReg (for parametric interval-censored regression)

# Helper Functions (Not Exported) ----------------------------------------------

#' Safely compute coefficient of variation
#'
#' @param w Numeric vector of weights
#' @return Scalar CV, or NA if mean is near zero
#' @keywords internal
safe_cv <- function(w) {
  w <- w[is.finite(w)]
  if (length(w) == 0) return(NA_real_)

  mu <- mean(w)
  if (abs(mu) < 1e-10) {
    warning("Mean weight near zero; CV undefined")
    return(NA_real_)
  }

  sd(w) / mu
}

#' Validate simulation data
#'
#' @param df Data.frame to validate
#' @param require_cols Character vector of required column names
#' @return Logical, TRUE if valid
#' @keywords internal
validate_sim_data <- function(df, require_cols = c("L", "R", "weight")) {
  if (!is.data.frame(df)) {
    warning("Input is not a data.frame")
    return(FALSE)
  }

  missing_cols <- setdiff(require_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }

  # Check for valid interval bounds
  if (any(is.na(df$L)) || any(is.na(df$R))) {
    warning("NA values in L or R")
    return(FALSE)
  }

  if (any(df$L < 0, na.rm = TRUE)) {
    warning("Negative L values detected")
    return(FALSE)
  }

  if (any(df$L > df$R & is.finite(df$R), na.rm = TRUE)) {
    warning("Invalid intervals: L > R")
    return(FALSE)
  }

  TRUE
}

#' Compute true log-logistic survival/hazard functions
#'
#' @param t Numeric vector of time points
#' @param alpha Baseline scale parameter
#' @param gamma Shape parameter
#' @param beta AFT coefficient (default 0)
#' @param X1 Covariate value (default 0)
#' @param type Character: "S" (survival), "H" (cumulative hazard), "h" (hazard)
#' @return Numeric vector of function values at t
#' @keywords internal
compute_true_loglogistic <- function(t, alpha, gamma, beta = 0, X1 = 0,
                                      type = c("S", "H", "h")) {
  type <- match.arg(type)

  # AFT parameterization: log(alpha_i) = log(alpha) + beta * X1
  alpha_i <- alpha * exp(beta * X1)

  # Log-logistic survival function
  S_t <- 1 / (1 + (t / alpha_i)^gamma)

  switch(type,
    "S" = S_t,
    "H" = -log(S_t),
    "h" = {
      # h(t) = (gamma/alpha_i) * (t/alpha_i)^(gamma-1) / (1 + (t/alpha_i)^gamma)
      (gamma / alpha_i) * (t / alpha_i)^(gamma - 1) / (1 + (t / alpha_i)^gamma)
    }
  )
}

#' Compute marginal survival averaged over covariate distribution
#'
#' @param t Numeric vector of time points
#' @param alpha Baseline scale parameter
#' @param gamma Shape parameter
#' @param beta AFT coefficient
#' @param X1_dist Numeric vector of length 2: c(P(X1=0), P(X1=1))
#' @param type Character: "S", "H", or "h"
#' @return Numeric vector of marginal function values
#' @keywords internal
compute_marginal_survival <- function(t, alpha, gamma, beta,
                                       X1_dist = c(0.5, 0.5),
                                       type = c("S", "H", "h")) {
  type <- match.arg(type)

  if (length(X1_dist) != 2 || abs(sum(X1_dist) - 1) > 1e-6) {
    stop("X1_dist must be a length-2 vector summing to 1")
  }

  # Compute conditional curves
  S0 <- compute_true_loglogistic(t, alpha, gamma, beta, X1 = 0, type = "S")
  S1 <- compute_true_loglogistic(t, alpha, gamma, beta, X1 = 1, type = "S")

  # Marginal survival
  S_marginal <- X1_dist[1] * S0 + X1_dist[2] * S1

  switch(type,
    "S" = S_marginal,
    "H" = -log(S_marginal),
    "h" = {
      # h(t) = -d/dt log(S(t)) ≈ finite difference
      # For marginal, we need to be careful; use H(t) derivative
      H_marginal <- -log(S_marginal)
      dt <- c(diff(t), diff(t)[length(t) - 1])
      dH <- c(diff(H_marginal), diff(H_marginal)[length(t) - 1])
      pmax(dH / dt, 0)  # Ensure non-negative
    }
  )
}

# Main Exported Functions ------------------------------------------------------

#' Load simulation data from directory
#'
#' Loads existing simulation replicates from disk, parsing filenames to extract
#' design metadata (scenario, replicate, sample size, censoring, weight type).
#'
#' @param path Character, path to data directory (e.g., "sim_data/n200")
#' @param pattern Character, regex pattern for files (default matches rep_*.rds/RDS/csv)
#' @param require_cols Character vector of required column names (default: L, R, weight)
#' @param sample_size_filter Integer, optional filter for specific sample size
#'
#' @return List of data.frames, each representing one replicate. Each data.frame
#'   has attributes: scenario, replicate, n_obs, censoring, weight_type, filepath.
#'   The list also has attribute "skipped" with names of files that failed validation.
#'
#' @examples
#' \dontrun{
#' sims <- load_sims_from_dir(
#'   path = "sim_data/n200",
#'   pattern = "sim_s001.*_whigh\\.rds$",
#'   require_cols = c("L", "R", "weight", "X1")
#' )
#' }
#'
#' @export
load_sims_from_dir <- function(path,
                                pattern = "sim_s.*_r.*\\.rds$",
                                require_cols = c("L", "R", "weight"),
                                sample_size_filter = NULL) {

  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }

  # List all matching files
  all_files <- list.files(path, pattern = pattern, full.names = TRUE,
                          ignore.case = TRUE)

  if (length(all_files) == 0) {
    warning("No files matched pattern '", pattern, "' in ", path)
    return(list())
  }

  # Filename parsing regex
  # Pattern: sim_s{scenario}_r{replicate}_n{size}_c{censoring}_w{weight}.rds
  fname_pattern <- "sim_s(\\d+)_r(\\d+)_n(\\d+)_c([0-9.]+)_w([a-z]+)"

  results <- list()
  skipped <- character(0)

  for (fpath in all_files) {
    fname <- basename(fpath)

    # Parse filename
    matches <- stringr::str_match(fname, fname_pattern)

    if (is.na(matches[1])) {
      warning("Could not parse filename: ", fname)
      skipped <- c(skipped, fpath)
      next
    }

    # Extract metadata
    scenario_id <- as.integer(matches[2])
    replicate <- as.integer(matches[3])
    n_obs <- as.integer(matches[4])
    censoring <- as.numeric(matches[5])
    weight_type <- matches[6]

    # Filter by sample size if specified
    if (!is.null(sample_size_filter) && n_obs != sample_size_filter) {
      next
    }

    # Load file
    dat <- tryCatch({
      if (grepl("\\.rds$", fname, ignore.case = TRUE)) {
        readRDS(fpath)
      } else if (grepl("\\.csv$", fname, ignore.case = TRUE)) {
        # For CSV, read only required columns
        read.csv(fpath, stringsAsFactors = FALSE)[, require_cols, drop = FALSE]
      } else {
        stop("Unsupported file format")
      }
    }, error = function(e) {
      warning("Failed to load ", fname, ": ", e$message)
      NULL
    })

    if (is.null(dat)) {
      skipped <- c(skipped, fpath)
      next
    }

    # Validate
    if (!validate_sim_data(dat, require_cols)) {
      skipped <- c(skipped, fpath)
      next
    }

    # Attach metadata as attributes
    attr(dat, "scenario") <- scenario_id
    attr(dat, "replicate") <- replicate
    attr(dat, "n_obs") <- n_obs
    attr(dat, "censoring") <- censoring
    attr(dat, "weight_type") <- weight_type
    attr(dat, "filepath") <- fpath

    # Add to results with meaningful name
    result_name <- sprintf("s%03d_r%03d_n%d_c%.1f_w%s",
                           scenario_id, replicate, n_obs, censoring, weight_type)
    results[[result_name]] <- dat
  }

  # Attach skipped files as attribute
  attr(results, "skipped") <- skipped

  if (length(skipped) > 0) {
    message("Loaded ", length(results), " files; skipped ", length(skipped))
  } else {
    message("Successfully loaded ", length(results), " files")
  }

  results
}

#' Create design data.frame from files
#'
#' Auto-generates a design_df by scanning files and extracting unique
#' combinations of design factors.
#'
#' @param path Character, path to data directory
#' @param true_alpha Numeric, true alpha parameter (default 5.0)
#' @param true_gamma Numeric, true gamma parameter (default 1.5)
#' @param true_beta Numeric, true beta parameter (default -0.5)
#'
#' @return Data.frame with columns: cell_id, n, censoring, weight_type,
#'   true_alpha, true_gamma, true_beta
#'
#' @export
create_design_df_from_files <- function(path,
                                         true_alpha = 5.0,
                                         true_gamma = 1.5,
                                         true_beta = -0.5) {

  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }

  all_files <- list.files(path, pattern = "sim_s.*\\.rds$", full.names = FALSE)

  fname_pattern <- "sim_s(\\d+)_r(\\d+)_n(\\d+)_c([0-9.]+)_w([a-z]+)"

  meta_list <- lapply(all_files, function(fname) {
    matches <- stringr::str_match(fname, fname_pattern)
    if (is.na(matches[1])) return(NULL)

    data.frame(
      n = as.integer(matches[4]),
      censoring = as.numeric(matches[5]),
      weight_type = matches[6],
      stringsAsFactors = FALSE
    )
  })

  meta_df <- dplyr::bind_rows(meta_list)

  design_df <- meta_df %>%
    dplyr::distinct(n, censoring, weight_type) %>%
    dplyr::arrange(n, censoring, weight_type) %>%
    dplyr::mutate(
      cell_id = sprintf("n%d_c%.1f_w%s", n, censoring, weight_type),
      true_alpha = true_alpha,
      true_gamma = true_gamma,
      true_beta = true_beta
    )

  design_df
}

#' Summarise weighted survival curves across replicates
#'
#' Fits parametric log-logistic AFT models to each replicate using icenReg,
#' predicts survival curves, and computes pointwise summaries (mean, median,
#' quantiles) across replicates. Also computes weight inequality metrics.
#'
#' @param sims List of data.frames from load_sims_from_dir(), all in same design cell
#' @param grid Numeric vector of time points for prediction (default NULL = auto)
#' @param estimator Character, estimation method (currently only "parametric")
#' @param weight_metric Character, either "cv" (coefficient of variation) or
#'   "kish_ess" (Kish effective sample size)
#' @param include_covariate Logical, if TRUE fit ~ X1, else fit ~ 1 (default TRUE)
#'
#' @return List with components:
#'   \describe{
#'     \item{rep_df}{Long data.frame with columns: rep_id, t, S, weight_metric_value,
#'       X1 (if include_covariate=TRUE)}
#'     \item{sum_df}{Data.frame with pointwise summaries: t, S_mean, S_med,
#'       S_q50l, S_q50u, S_q95l, S_q95u, X1 (if stratified)}
#'     \item{meta}{List with n_total, n_converged, dropped_reps, grid}
#'   }
#'
#' @examples
#' \dontrun{
#' results <- summarise_weighted_curves(
#'   sims = sims,
#'   weight_metric = "cv",
#'   include_covariate = TRUE
#' )
#' }
#'
#' @export
summarise_weighted_curves <- function(sims,
                                       grid = NULL,
                                       estimator = c("parametric"),
                                       weight_metric = c("cv", "kish_ess"),
                                       include_covariate = TRUE) {

  estimator <- match.arg(estimator)
  weight_metric <- match.arg(weight_metric)

  if (!requireNamespace("icenReg", quietly = TRUE)) {
    stop("Package 'icenReg' required but not installed. ",
         "Install with: install.packages('icenReg')")
  }

  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' required but not installed. ",
         "Install with: install.packages('survival')")
  }

  if (length(sims) == 0) {
    stop("Empty sims list")
  }

  # Auto-generate grid if not provided
  if (is.null(grid)) {
    # Collect all L and finite R values
    all_L <- unlist(lapply(sims, function(x) x$L))
    all_R <- unlist(lapply(sims, function(x) x$R[is.finite(x$R)]))

    t_min <- max(min(all_L, na.rm = TRUE), 1e-6)
    t_max <- quantile(all_R, 0.99, na.rm = TRUE)

    grid <- seq(t_min, t_max, length.out = 100)
  }

  # Storage
  rep_list <- list()
  dropped_reps <- character(0)
  error_messages <- character(0)

  # Test first replicate to catch common errors early
  if (length(sims) > 0) {
    test_dat <- sims[[1]]
    test_result <- tryCatch({
      if (include_covariate && "X1" %in% names(test_dat)) {
        icenReg::ic_par(
          formula = survival::Surv(L, R, type = "interval2") ~ X1,
          data = test_dat,
          dist = "loglogistic",
          model = "aft",
          weights = test_dat$weight
        )
      } else {
        icenReg::ic_par(
          formula = survival::Surv(L, R, type = "interval2") ~ 1,
          data = test_dat,
          dist = "loglogistic",
          model = "aft",
          weights = test_dat$weight
        )
      }
      "success"
    }, error = function(e) {
      message("\nDiagnostic: First replicate failed with error:")
      message("  Error: ", e$message)
      message("  Data dimensions: ", nrow(test_dat), " rows x ", ncol(test_dat), " cols")
      message("  Columns: ", paste(names(test_dat), collapse = ", "))
      message("  L range: ", min(test_dat$L, na.rm=TRUE), " to ", max(test_dat$L, na.rm=TRUE))
      message("  R range: ", min(test_dat$R[is.finite(test_dat$R)], na.rm=TRUE),
              " to ", max(test_dat$R[is.finite(test_dat$R)], na.rm=TRUE))
      message("  Weight range: ", min(test_dat$weight, na.rm=TRUE), " to ",
              max(test_dat$weight, na.rm=TRUE))
      if ("X1" %in% names(test_dat)) {
        message("  X1 values: ", paste(unique(test_dat$X1), collapse = ", "))
      }
      e$message
    })

    if (test_result != "success") {
      stop("Initial test fit failed. See diagnostic messages above. ",
           "Common issues:\n",
           "  1. survival package not installed: install.packages('survival')\n",
           "  2. icenReg package issue: Try ic_par() manually on one dataset\n",
           "  3. Data format issue: Check L, R, weight columns")
    }
  }

  for (i in seq_along(sims)) {
    rep_name <- names(sims)[i]
    dat <- sims[[i]]

    # Compute weight metric
    w_metric <- switch(weight_metric,
      "cv" = safe_cv(dat$weight),
      "kish_ess" = {
        cv <- safe_cv(dat$weight)
        nrow(dat) / (1 + cv^2)
      }
    )

    # Fit model
    fit <- tryCatch({
      if (include_covariate && "X1" %in% names(dat)) {
        icenReg::ic_par(
          formula = survival::Surv(L, R, type = "interval2") ~ X1,
          data = dat,
          dist = "loglogistic",
          model = "aft",
          weights = dat$weight
        )
      } else {
        icenReg::ic_par(
          formula = survival::Surv(L, R, type = "interval2") ~ 1,
          data = dat,
          dist = "loglogistic",
          model = "aft",
          weights = dat$weight
        )
      }
    }, error = function(e) {
      error_messages <<- c(error_messages, paste(rep_name, ":", e$message))
      NULL
    }, warning = function(w) {
      # Suppress convergence warnings but continue
      suppressWarnings(
        if (include_covariate && "X1" %in% names(dat)) {
          icenReg::ic_par(
            formula = survival::Surv(L, R, type = "interval2") ~ X1,
            data = dat,
            dist = "loglogistic",
            model = "aft",
            weights = dat$weight
          )
        } else {
          icenReg::ic_par(
            formula = survival::Surv(L, R, type = "interval2") ~ 1,
            data = dat,
            dist = "loglogistic",
            model = "aft",
            weights = dat$weight
          )
        }
      )
    })

    if (is.null(fit)) {
      dropped_reps <- c(dropped_reps, rep_name)
      next
    }

    # Predict survival by computing manually from fitted parameters
    # icenReg ic_par returns: log(scale), log(shape), beta1, beta2, ...
    coefs <- coef(fit)
    log_scale_base <- coefs[1]
    log_shape <- coefs[2]

    if (include_covariate && "X1" %in% names(dat)) {
      # With covariate: log(scale_i) = log(scale) + beta * X1
      beta_X1 <- coefs[3]

      # Compute survival for X1=0 and X1=1
      # AFT model: log(scale_i) = log_scale_base + beta_X1 * X1
      # Survival: S(t) = 1 / (1 + (t / exp(log_scale_i))^exp(log_shape))

      scale_0 <- exp(log_scale_base)  # X1=0
      scale_1 <- exp(log_scale_base + beta_X1)  # X1=1
      shape <- exp(log_shape)

      S0 <- 1 / (1 + (grid / scale_0)^shape)
      S1 <- 1 / (1 + (grid / scale_1)^shape)

      # Store both curves
      rep_list[[paste0(rep_name, "_X0")]] <- data.frame(
        rep_id = rep_name,
        t = grid,
        S = S0,
        weight_metric_value = w_metric,
        X1 = 0,
        stringsAsFactors = FALSE
      )

      rep_list[[paste0(rep_name, "_X1")]] <- data.frame(
        rep_id = rep_name,
        t = grid,
        S = S1,
        weight_metric_value = w_metric,
        X1 = 1,
        stringsAsFactors = FALSE
      )

    } else {
      # Intercept-only model
      scale <- exp(log_scale_base)
      shape <- exp(log_shape)

      S_pred <- 1 / (1 + (grid / scale)^shape)

      rep_list[[rep_name]] <- data.frame(
        rep_id = rep_name,
        t = grid,
        S = S_pred,
        weight_metric_value = w_metric,
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine replicates
  if (length(rep_list) == 0) {
    msg <- paste("All", length(sims), "replicates failed to fit.")
    if (length(error_messages) > 0) {
      msg <- paste0(msg, "\n\nFirst few errors:\n  ",
                    paste(head(error_messages, 3), collapse = "\n  "))
    }
    msg <- paste0(msg, "\n\nTroubleshooting:\n",
                  "  1. Check that 'survival' package is installed\n",
                  "  2. Verify data has valid L, R, weight columns\n",
                  "  3. Try fitting one dataset manually with ic_par()")
    stop(msg)
  }

  rep_df <- dplyr::bind_rows(rep_list)

  # Compute pointwise summaries
  if (include_covariate && "X1" %in% names(rep_df)) {
    sum_df <- rep_df %>%
      dplyr::group_by(t, X1) %>%
      dplyr::summarise(
        S_mean = mean(S, na.rm = TRUE),
        S_med = median(S, na.rm = TRUE),
        S_q50l = quantile(S, 0.25, na.rm = TRUE),
        S_q50u = quantile(S, 0.75, na.rm = TRUE),
        S_q95l = quantile(S, 0.025, na.rm = TRUE),
        S_q95u = quantile(S, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    sum_df <- rep_df %>%
      dplyr::group_by(t) %>%
      dplyr::summarise(
        S_mean = mean(S, na.rm = TRUE),
        S_med = median(S, na.rm = TRUE),
        S_q50l = quantile(S, 0.25, na.rm = TRUE),
        S_q50u = quantile(S, 0.75, na.rm = TRUE),
        S_q95l = quantile(S, 0.025, na.rm = TRUE),
        S_q95u = quantile(S, 0.975, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Metadata
  meta <- list(
    n_total = length(sims),
    n_converged = length(unique(rep_df$rep_id)),
    dropped_reps = dropped_reps,
    grid = grid
  )

  list(
    rep_df = rep_df,
    sum_df = sum_df,
    meta = meta
  )
}

#' Plot ghosted spaghetti survival curves
#'
#' Creates a publication-ready plot with ghosted replicate curves, pointwise
#' summary ribbons, mean curve, and true population curve overlay.
#'
#' @param sum_df Summary data.frame from summarise_weighted_curves()
#' @param rep_df Replicate-level data.frame from summarise_weighted_curves()
#' @param true_alpha Numeric, true alpha parameter (default 5.0)
#' @param true_gamma Numeric, true gamma parameter (default 1.5)
#' @param true_beta Numeric, true beta parameter (default -0.5)
#' @param X1_dist Numeric vector c(P(X1=0), P(X1=1)) for marginal curve (default c(0.5, 0.5))
#' @param which Character, "S" (survival), "H" (cumulative hazard), or "h" (hazard)
#' @param n_ghost Integer, number of ghost replicates to plot (default 30)
#' @param seed Integer, random seed for ghost sampling (default 1)
#' @param palette Character, viridis palette name (default "viridis")
#' @param title Character, plot title (default NULL = auto)
#' @param show_covariate_curves Logical, if TRUE and data has X1, show separate
#'   true curves for X1=0 and X1=1 (default FALSE)
#'
#' @return ggplot object
#'
#' @export
plot_weighted_spaghetti <- function(sum_df,
                                     rep_df,
                                     true_alpha = 5.0,
                                     true_gamma = 1.5,
                                     true_beta = -0.5,
                                     X1_dist = c(0.5, 0.5),
                                     which = c("S", "H", "h"),
                                     n_ghost = 30,
                                     seed = 1,
                                     palette = "viridis",
                                     title = NULL,
                                     show_covariate_curves = FALSE) {

  which <- match.arg(which)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  if (!requireNamespace("viridis", quietly = TRUE)) {
    stop("Package 'viridis' required")
  }

  # Transform data if needed
  if (which %in% c("H", "h")) {
    sum_df <- sum_df %>%
      dplyr::mutate(
        S_mean = -log(S_mean),
        S_med = -log(S_med),
        S_q50l = -log(S_q50l),
        S_q50u = -log(S_q50u),
        S_q95l = -log(S_q95l),
        S_q95u = -log(S_q95u)
      )

    rep_df <- rep_df %>%
      dplyr::mutate(S = -log(S))
  }

  if (which == "h") {
    # Hazard from finite differences (smooth H(t))
    sum_df <- sum_df %>%
      dplyr::group_by(if ("X1" %in% names(.)) X1 else NULL) %>%
      dplyr::arrange(t) %>%
      dplyr::mutate(
        dt = c(diff(t), diff(t)[dplyr::n() - 1]),
        S_mean = pmax(c(diff(S_mean), 0) / dt, 0),
        S_med = pmax(c(diff(S_med), 0) / dt, 0),
        S_q50l = pmax(c(diff(S_q50l), 0) / dt, 0),
        S_q50u = pmax(c(diff(S_q50u), 0) / dt, 0),
        S_q95l = pmax(c(diff(S_q95l), 0) / dt, 0),
        S_q95u = pmax(c(diff(S_q95u), 0) / dt, 0)
      ) %>%
      dplyr::select(-dt) %>%
      dplyr::ungroup()

    rep_df <- rep_df %>%
      dplyr::group_by(rep_id, if ("X1" %in% names(.)) X1 else NULL) %>%
      dplyr::arrange(t) %>%
      dplyr::mutate(
        dt = c(diff(t), diff(t)[dplyr::n() - 1]),
        S = pmax(c(diff(S), 0) / dt, 0)
      ) %>%
      dplyr::select(-dt) %>%
      dplyr::ungroup()
  }

  # Sample ghost replicates
  set.seed(seed)
  unique_reps <- unique(rep_df$rep_id)
  n_available <- length(unique_reps)
  n_sample <- min(n_ghost, n_available)

  ghost_reps <- sample(unique_reps, n_sample, replace = FALSE)
  ghost_df <- rep_df %>%
    dplyr::filter(rep_id %in% ghost_reps)

  # Compute true curves
  t_grid <- sort(unique(sum_df$t))
  true_marginal <- compute_marginal_survival(
    t = t_grid,
    alpha = true_alpha,
    gamma = true_gamma,
    beta = true_beta,
    X1_dist = X1_dist,
    type = which
  )

  true_df <- data.frame(t = t_grid, S_true = true_marginal)

  # Optional: separate curves for X1 levels
  if (show_covariate_curves && "X1" %in% names(rep_df)) {
    true_X0 <- compute_true_loglogistic(
      t = t_grid, alpha = true_alpha, gamma = true_gamma,
      beta = true_beta, X1 = 0, type = which
    )
    true_X1 <- compute_true_loglogistic(
      t = t_grid, alpha = true_alpha, gamma = true_gamma,
      beta = true_beta, X1 = 1, type = which
    )

    true_df_X0 <- data.frame(t = t_grid, S_true = true_X0, X1 = 0)
    true_df_X1 <- data.frame(t = t_grid, S_true = true_X1, X1 = 1)
  }

  # Build plot
  y_lab <- switch(which,
    "S" = "Survival S(t)",
    "H" = "Cumulative Hazard H(t)",
    "h" = "Hazard h(t)"
  )

  if (is.null(title)) {
    title <- switch(which,
      "S" = "Weighted Log-Logistic Survival Estimates",
      "H" = "Weighted Log-Logistic Cumulative Hazard Estimates",
      "h" = "Weighted Log-Logistic Hazard Estimates (smoothed)"
    )
  }

  p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = t)) +
    # 95% ribbon
    ggplot2::geom_ribbon(ggplot2::aes(ymin = S_q95l, ymax = S_q95u),
                         fill = "grey70", alpha = 0.2) +
    # 50% ribbon
    ggplot2::geom_ribbon(ggplot2::aes(ymin = S_q50l, ymax = S_q50u),
                         fill = "grey50", alpha = 0.3) +
    # Ghost lines
    ggplot2::geom_line(
      data = ghost_df,
      ggplot2::aes(y = S, group = rep_id, color = weight_metric_value),
      alpha = 0.15,
      linewidth = 0.3
    ) +
    # Mean curve
    ggplot2::geom_line(ggplot2::aes(y = S_mean),
                       color = "grey30", linewidth = 1.0) +
    # True marginal curve
    ggplot2::geom_line(data = true_df, ggplot2::aes(x = t, y = S_true),
                       color = "black", linewidth = 1.2) +
    # Color scale
    ggplot2::scale_color_viridis_c(
      option = palette,
      name = "CV(weights)",
      guide = ggplot2::guide_colorbar(barwidth = 10, barheight = 0.5)
    ) +
    # Theme
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "Time",
      y = y_lab,
      title = title,
      subtitle = if (which == "h") {
        "Hazard smoothed from cumulative hazard; tails less reliable"
      } else NULL
    )

  # Add covariate-specific true curves if requested
  if (show_covariate_curves && "X1" %in% names(rep_df)) {
    p <- p +
      ggplot2::geom_line(data = true_df_X0,
                         ggplot2::aes(x = t, y = S_true),
                         color = "black", linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_line(data = true_df_X1,
                         ggplot2::aes(x = t, y = S_true),
                         color = "black", linetype = "dotted", linewidth = 0.8)
  }

  p
}

#' Create faceted design panels
#'
#' Generates a multi-panel plot with one panel per design cell, faceted by
#' design factors (e.g., censoring level and sample size).
#'
#' @param results_list Named list where each element is the output of
#'   summarise_weighted_curves() for one design cell. Names must match
#'   design_df$cell_id.
#' @param design_df Data.frame with columns: cell_id, n, censoring, weight_type,
#'   true_alpha, true_gamma, true_beta
#' @param facet_rows Character, column name for facet rows (default "censoring")
#' @param facet_cols Character, column name for facet columns (default "n")
#' @param which Character, "S", "H", or "h"
#' @param ... Additional arguments passed to plot_weighted_spaghetti()
#'
#' @return ggplot object with facets
#'
#' @export
facet_design_panels <- function(results_list,
                                 design_df,
                                 facet_rows = "censoring",
                                 facet_cols = "n",
                                 which = "S",
                                 ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }

  # Combine all rep_df and sum_df with cell_id
  all_rep <- list()
  all_sum <- list()

  for (cell_id in names(results_list)) {
    res <- results_list[[cell_id]]

    # Add cell_id to both dataframes
    rep_with_id <- res$rep_df %>%
      dplyr::mutate(cell_id = cell_id)

    sum_with_id <- res$sum_df %>%
      dplyr::mutate(cell_id = cell_id)

    all_rep[[cell_id]] <- rep_with_id
    all_sum[[cell_id]] <- sum_with_id
  }

  combined_rep <- dplyr::bind_rows(all_rep)
  combined_sum <- dplyr::bind_rows(all_sum)

  # Join with design_df
  combined_rep <- combined_rep %>%
    dplyr::left_join(design_df, by = "cell_id")

  combined_sum <- combined_sum %>%
    dplyr::left_join(design_df, by = "cell_id")

  # Transform if needed
  if (which %in% c("H", "h")) {
    combined_sum <- combined_sum %>%
      dplyr::mutate(
        S_mean = -log(S_mean),
        S_med = -log(S_med),
        S_q50l = -log(S_q50l),
        S_q50u = -log(S_q50u),
        S_q95l = -log(S_q95l),
        S_q95u = -log(S_q95u)
      )

    combined_rep <- combined_rep %>%
      dplyr::mutate(S = -log(S))
  }

  if (which == "h") {
    # Hazard via finite differences
    combined_sum <- combined_sum %>%
      dplyr::group_by(cell_id, if ("X1" %in% names(.)) X1 else NULL) %>%
      dplyr::arrange(t) %>%
      dplyr::mutate(
        dt = c(diff(t), diff(t)[dplyr::n() - 1]),
        S_mean = pmax(c(diff(S_mean), 0) / dt, 0),
        S_q50l = pmax(c(diff(S_q50l), 0) / dt, 0),
        S_q50u = pmax(c(diff(S_q50u), 0) / dt, 0),
        S_q95l = pmax(c(diff(S_q95l), 0) / dt, 0),
        S_q95u = pmax(c(diff(S_q95u), 0) / dt, 0)
      ) %>%
      dplyr::select(-dt) %>%
      dplyr::ungroup()

    combined_rep <- combined_rep %>%
      dplyr::group_by(cell_id, rep_id, if ("X1" %in% names(.)) X1 else NULL) %>%
      dplyr::arrange(t) %>%
      dplyr::mutate(
        dt = c(diff(t), diff(t)[dplyr::n() - 1]),
        S = pmax(c(diff(S), 0) / dt, 0)
      ) %>%
      dplyr::select(-dt) %>%
      dplyr::ungroup()
  }

  # Sample ghosts per cell
  # Compute unique reps per cell, then sample up to 30
  ghost_df <- combined_rep %>%
    dplyr::group_by(cell_id) %>%
    dplyr::filter(rep_id %in% {
      unique_reps <- unique(rep_id)
      sample(unique_reps, min(30, length(unique_reps)), replace = FALSE)
    }) %>%
    dplyr::ungroup()

  # Compute true curves per cell
  true_list <- lapply(unique(combined_sum$cell_id), function(cid) {
    cell_meta <- design_df %>% dplyr::filter(cell_id == cid)

    t_grid <- combined_sum %>%
      dplyr::filter(cell_id == cid) %>%
      dplyr::pull(t) %>%
      unique() %>%
      sort()

    true_vals <- compute_marginal_survival(
      t = t_grid,
      alpha = cell_meta$true_alpha[1],
      gamma = cell_meta$true_gamma[1],
      beta = cell_meta$true_beta[1],
      X1_dist = c(0.5, 0.5),
      type = which
    )

    data.frame(
      cell_id = cid,
      t = t_grid,
      S_true = true_vals
    ) %>%
      dplyr::left_join(design_df, by = "cell_id")
  })

  true_df <- dplyr::bind_rows(true_list)

  # Build faceted plot
  y_lab <- switch(which,
    "S" = "Survival S(t)",
    "H" = "Cumulative Hazard H(t)",
    "h" = "Hazard h(t)"
  )

  # Create facet formula
  facet_formula <- as.formula(paste(facet_rows, "~", facet_cols))

  p <- ggplot2::ggplot(combined_sum, ggplot2::aes(x = t)) +
    # 95% ribbon
    ggplot2::geom_ribbon(ggplot2::aes(ymin = S_q95l, ymax = S_q95u),
                         fill = "grey70", alpha = 0.2) +
    # 50% ribbon
    ggplot2::geom_ribbon(ggplot2::aes(ymin = S_q50l, ymax = S_q50u),
                         fill = "grey50", alpha = 0.3) +
    # Ghost lines
    ggplot2::geom_line(
      data = ghost_df,
      ggplot2::aes(y = S, group = rep_id, color = weight_metric_value),
      alpha = 0.15,
      linewidth = 0.3
    ) +
    # Mean curve
    ggplot2::geom_line(ggplot2::aes(y = S_mean),
                       color = "grey30", linewidth = 0.8) +
    # True curve
    ggplot2::geom_line(data = true_df,
                       ggplot2::aes(x = t, y = S_true),
                       color = "black", linewidth = 1.0) +
    # Facets
    ggplot2::facet_grid(facet_formula, scales = "free") +
    # Color scale
    ggplot2::scale_color_viridis_c(
      name = "CV(weights)",
      guide = ggplot2::guide_colorbar(barwidth = 10, barheight = 0.5)
    ) +
    # Theme
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90"),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      x = "Time",
      y = y_lab,
      title = "Weighted Survival Estimates by Design Cell"
    )

  p
}

#' Get caption text for plots
#'
#' Returns standardized caption text explaining the plot elements.
#'
#' @return Character string
#' @export
get_caption_text <- function() {
  paste(
    "Ghost lines: weighted parametric log-logistic fits across replicates,",
    "coloured by CV of weights (higher = more unequal). Bright line: pointwise",
    "mean. Ribbons: 50% and 95% intervals. Bold black: true marginal survival.",
    "Hazards smoothed from cumulative hazard; tails less reliable."
  )
}
