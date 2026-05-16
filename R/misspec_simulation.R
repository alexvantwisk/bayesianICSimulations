#' Simulate interval-censored data under a Weibull DGM
#'
#' Mirror of `simulate_survival_data()` (log-logistic) with the inverse-CDF
#' sample swapped for Weibull. The fit-side log-logistic model becomes
#' misspecified — this is the point of the misspecification scenario (Task F).
#'
#' @inheritParams simulate_survival_data
#' @return Same tibble shape as `simulate_survival_data()`: columns X1, A0,
#'   A_event, visit, L, R, status, weight.
#' @examples
#' set.seed(1)
#' params <- get_default_params()
#' simulate_survival_data_weibull(n = 50, params = params, weight_type = "low")
#' @export
simulate_survival_data_weibull <- function(
  n, params, weight_type = c("none", "low", "high")
) {
  weight_type <- match.arg(weight_type)

  covs <- list(X1 = stats::rbinom(n, 1, params$p_sex))
  U <- stats::rbeta(n, params$a0_shape1, params$a0_shape2)
  A0 <- params$a0_min + (params$a0_max - params$a0_min) * U

  Uw <- stats::runif(n)
  Ti <- params$lambda_weibull *
    exp(params$beta1 * covs$X1) *
    (-log(Uw))^(1 / params$k_weibull)

  admin_cens <- Ti > params$follow_max
  Ti_capped <- pmin(Ti, params$follow_max)
  A_event <- A0 + Ti_capped

  # Delegate visit-time generation to the same internal helper used by the
  # log-logistic DGM so admin-censored subjects pick up right-censoring
  # through the L_dur / R_dur logic below.
  visit <- determine_visit_times(A0, A_event, params, force_censor = admin_cens)

  eps <- 1e-12
  L_dur <- ifelse(A_event <= visit, 0, pmax(visit - A0, 0))
  R_dur <- ifelse(A_event <= visit, pmax(visit - A0, 0), Inf)
  status <- ifelse(is.infinite(R_dur), 0L, 3L)

  # Reuse the same weight generator as the log-logistic DGM (inlined here for
  # symmetry — generate_weights is internal to simulation.R but visible
  # in-package after devtools::load_all()).
  w <- generate_weights(n, weight_type, params)

  tibble::tibble(
    X1 = covs$X1,
    A0 = A0, A_event = A_event, visit = visit,
    L = pmax(L_dur, eps), R = R_dur,
    status = status, weight = w
  )
}

#' Drive a single misspecification cell: HMC + MH fits on Weibull-truth data
#'
#' Generates `n_replicates` datasets at `(n, target_censoring_prop, weight_type)`,
#' fits log-logistic HMC and/or MH on each via the standard fit functions, and
#' writes the standardised summary/diagnostics outputs.
#'
#' @param n Sample size (default 2000).
#' @param target_censoring_prop Censoring proportion (default 0.3).
#' @param weight_type Weighting regime (default "high").
#' @param n_replicates Number of replicates (default 200).
#' @param data_dir Directory to write simulated .rds files.
#' @param hmc_results_dir Directory for HMC outputs.
#' @param mh_results_dir Directory for MH outputs.
#' @param do_hmc Whether to run the HMC leg (default TRUE).
#' @param do_mh Whether to run the MH leg (default TRUE).
#' @return Invisible list of fit-result lists.
#' @examples
#' \dontrun{
#' # ~1.5 hours for HMC, ~8-25 hours for MH on 8-core laptop
#' run_misspec_simulation(do_hmc = TRUE, do_mh = TRUE)
#' }
#' @export
run_misspec_simulation <- function(
  n = 2000,
  target_censoring_prop = 0.3,
  weight_type = c("high", "low", "none"),
  n_replicates = 200L,
  data_dir = "mcmc_outputs/misspec/n2000_c0.3_whigh/sim_data",
  hmc_results_dir = "mcmc_outputs/misspec/n2000_c0.3_whigh/hmc",
  mh_results_dir = "mcmc_outputs/misspec/n2000_c0.3_whigh/mh",
  do_hmc = TRUE,
  do_mh = TRUE
) {
  weight_type <- match.arg(weight_type)
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  params <- get_default_params()
  params$target_censoring_prop <- target_censoring_prop

  set.seed(2025)
  for (r in seq_len(n_replicates)) {
    fpath <- file.path(
      data_dir,
      sprintf(
        "sim_misspec_r%03d_n%04d_c%0.1f_w%s.rds",
        r, n, target_censoring_prop, weight_type
      )
    )
    if (!file.exists(fpath)) {
      dat <- simulate_survival_data_weibull(n, params, weight_type)
      saveRDS(dat, fpath, compress = "xz")
    }
  }

  cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

  out <- list()
  if (isTRUE(do_hmc)) {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      stop(
        "Package 'cmdstanr' is required for the HMC leg.\n",
        "Install from https://mc-stan.org/r-packages/",
        call. = FALSE
      )
    }
    out$hmc <- fit_logistic_hmc(
      sim_dir = data_dir,
      results_dir = hmc_results_dir,
      save = c("summary", "diagnostics"),
      workers = cores
    )
  }
  if (isTRUE(do_mh)) {
    if (!requireNamespace("rjags", quietly = TRUE)) {
      stop(
        "Package 'rjags' is required for the MH leg.\n",
        "Install JAGS from https://mcmc-jags.sourceforge.io/",
        call. = FALSE
      )
    }
    out$mh <- fit_logistic_mh(
      sim_dir = data_dir,
      results_dir = mh_results_dir,
      save = c("summary", "diagnostics"),
      workers = cores,
      n_chains = 4, n_adapt = 1000, n_burnin = 1000, n_iter = 5000
    )
  }
  invisible(out)
}
