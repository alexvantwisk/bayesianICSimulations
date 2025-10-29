#' Simulation utilities and orchestration
#'
#' @importFrom tibble tibble
#' @importFrom tidyr crossing uncount
#' @importFrom dplyr arrange mutate row_number %>%
#' @importFrom purrr pwalk
#'
#' @return A list of default parameter values
#' @export
get_default_params <- function() {
  list(
    beta0 = log(5), # baseline median time-to-HIV-infection ≈5 years (alpha = exp(beta0))
    beta1 = -0.5, # AFT coefficient on sex (negative = females have shorter time to infection)
    gamma = 1.5, # log-logistic shape
    p_sex = 0.55, # P(X1 = 1) = P(female)

    # weight dispersion
    k_low = 10,
    theta_low = 0.1,
    k_high = 1,
    theta_high = 1.0,

    # age at sexual debut A0 ∼ scaled Beta (absolute age at first sex)
    a0_min = 15,
    a0_max = 35,
    a0_shape1 = 2,
    a0_shape2 = 5,

    # maximum follow-up (years from sexual debut to survey)
    follow_max = 40,

    # proportion of HIV-negative at survey (right censoring)
    target_censoring_prop = 0.3
  )
}

# internal helpers -----------------------------------------------------------

#' @keywords internal
#' @noRd
generate_covariates <- function(n, params) {
  list(X1 = rbinom(n, 1, params$p_sex))
}

#' @keywords internal
#' @noRd
generate_a0 <- function(n, params) {
  U <- rbeta(n, params$a0_shape1, params$a0_shape2)
  params$a0_min + (params$a0_max - params$a0_min) * U
}

#' @keywords internal
#' @noRd
generate_weights <- function(n, wt, params) {
  if (wt == "none") {
    return(rep(1, n))
  }
  if (wt == "low") {
    k <- params$k_low
    theta <- params$theta_low
  } else {
    k <- params$k_high
    theta <- params$theta_high
  }
  w0 <- rgamma(n, shape = k, scale = theta)
  n * w0 / sum(w0)
}

#' @keywords internal
#' @noRd
#' @param force_censor Logical vector (length n) of subjects to be right-censored
#'   irrespective of random censoring (e.g., administrative censoring at follow_max).
determine_visit_times <- function(A0, A_event, params, force_censor = NULL) {
  n <- length(A0)
  p <- params$target_censoring_prop

  if (is.null(force_censor)) {
    force_censor <- rep(FALSE, n)
  }

  n_cens <- round(n * p)

  # random censoring among those not forced
  pool <- which(!force_censor)
  n_pool <- length(pool)
  cens_idx <- if (n_pool > 0) {
    sample(pool, size = min(n_cens, n_pool))
  } else {
    integer(0)
  }
  cens_flag <- force_censor
  cens_flag[cens_idx] <- TRUE

  visit <- numeric(n)
  for (i in seq_len(n)) {
    # Simple approach: Survey age is independent of everything else
    # Survey happens at various ages (realistic for ZIMPHIA)
    min_survey_age <- max(A0[i] + 2, 20) # At least 2 years after debut, age 20+
    max_survey_age <- min(A0[i] + 40, 65) # Up to 40 years later, max age 65

    survey_age <- runif(1, min_survey_age, max_survey_age)
    visit[i] <- survey_age
  }
  visit
}

simulate_survival_data <- function(
  n,
  params,
  weight_type = c("none", "low", "high")
) {
  weight_type <- match.arg(weight_type)

  covs <- generate_covariates(n, params)
  A0 <- generate_a0(n, params)

  alpha <- exp(params$beta0)
  U <- runif(n)

  # Generate HIV infection times (covariate-dependent)
  Ti_infection <- alpha *
    exp(params$beta1 * covs$X1) *
    (U / (1 - U))^(1 / params$gamma)
  A_infection <- A0 + Ti_infection

  # Generate survey times (independent of infection timing!)
  survey_age <- determine_visit_times(
    A0,
    A_infection,
    params,
    force_censor = NULL
  )

  # Simple interval structure:
  # - HIV+ at survey: interval is [0, survey_age - A0] (time from debut to survey)
  # - HIV- at survey: right-censored at survey_age - A0

  # Determine HIV status at survey
  infected_by_survey <- A_infection <= survey_age

  # Apply target censoring proportion to those who could be positive
  n_potential_pos <- sum(infected_by_survey)
  n_to_censor <- round(params$target_censoring_prop * n)

  if (n_to_censor > 0 && n_potential_pos > 0) {
    censor_idx <- sample(
      which(infected_by_survey),
      size = min(n_to_censor, n_potential_pos)
    )
    infected_by_survey[censor_idx] <- FALSE
  }

  # Build intervals
  eps <- 1e-12
  L_dur <- rep(eps, n) # Always start at sexual debut
  R_dur <- ifelse(
    infected_by_survey,
    survey_age - A0, # HIV+: interval from debut to survey
    Inf
  ) # HIV-: right-censored

  status <- ifelse(is.infinite(R_dur), 0L, 3L)
  w <- generate_weights(n, weight_type, params)

  tibble::tibble(
    X1 = covs$X1,
    A0 = A0, # age at sexual debut
    A_infection = A_infection, # age at HIV infection (reference)
    survey_age = survey_age, # age at survey
    L = pmax(L_dur, eps), # time from debut (always ~0)
    R = R_dur, # time from debut to survey (HIV+) or Inf (HIV-)
    status = status,
    weight = w
  )
}

#' Simulate interval-censored survival data
#'
#' @param n Sample size.
#' @param params List of parameters as produced by get_default_params().
#' @param weight_type Weighting scheme. One of "none", "low", or "high".
#'
#' @return A tibble with one row per subject containing:
#'   X1 (covariate), A0 (entry age), A_event (absolute event age or admin limit),
#'   visit (absolute visit age), interval bounds L and R (DURATIONS since entry),
#'   status (0 right-censored, 3 interval), and analysis weight.
#' @examples
#' params <- get_default_params()
#' set.seed(1)
#' dat <- simulate_survival_data(50, params, "low")
#' head(dat)
#' @export
simulate_survival_data <- function(
  n,
  params,
  weight_type = c("none", "low", "high")
) {
  weight_type <- match.arg(weight_type)

  covs <- generate_covariates(n, params)
  A0 <- generate_a0(n, params)

  alpha <- exp(params$beta0)
  U <- runif(n)
  # Standard AFT parameterization: log(lambda_i) = log(alpha) + beta1 * X1
  # Individual scale parameter: lambda_i = alpha * exp(beta1 * X1)
  # Log-logistic quantile function: Q(u) = lambda_i * (u/(1-u))^(1/gamma)
  Ti <- alpha * exp(params$beta1 * covs$X1) * (U / (1 - U))^(1 / params$gamma)

  # Administrative censoring at follow_max: mark and DO NOT treat as events
  admin_cens <- Ti > params$follow_max
  Ti_capped <- pmin(Ti, params$follow_max)

  A_event <- A0 + Ti_capped

  # Determine visit times; force right-censoring for administrative cases
  visit <- determine_visit_times(A0, A_event, params, force_censor = admin_cens)

  # Build interval bounds as DURATIONS since entry (time-on-study)
  # Observed event: (0, visit - A0]; Right-censored: (visit - A0, Inf)
  # Guard against tiny negative due to floating arithmetic
  eps <- 1e-12
  L_dur <- ifelse(A_event <= visit, 0, pmax(visit - A0, 0))
  R_dur <- ifelse(A_event <= visit, pmax(visit - A0, 0), Inf)

  status <- ifelse(is.infinite(R_dur), 0L, 3L)

  w <- generate_weights(n, weight_type, params)

  tibble::tibble(
    X1 = covs$X1,
    A0 = A0, # absolute entry time (for reference)
    A_event = A_event, # absolute event/admin-censoring time (for reference)
    visit = visit, # absolute last visit time (for reference)
    L = pmax(L_dur, eps),
    R = R_dur,
    status = status,
    weight = w
  )
}

#' Run multiple simulation scenarios and save to disk
#'
#' @param out_dir Output directory for simulated datasets.
#' @param n_obs_vec Vector of sample sizes.
#' @param censoring_props Vector of target censoring proportions.
#' @param weight_types Vector of weight types.
#' @param n_replicates Number of Monte Carlo replicates per scenario.
#'
#' @return Invisibly returns a data frame describing the generated files.
#' @examples
#' \dontrun{
#' run_simulations_hmc(
#'   out_dir = tempdir(),
#'   n_obs_vec = c(200, 1000),
#'   censoring_props = c(0.1, 0.3),
#'   weight_types = c("none", "high"),
#'   n_replicates = 100
#' )
#' }
#' @export
run_simulations <- function(
  out_dir = "data/sim_data",
  n_obs_vec = c(200, 2000, 10000),
  censoring_props = c(0.1, 0.3, 0.5),
  weight_types = c("none", "low", "high"),
  n_replicates = 200
) {
  set.seed(2025)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  scenarios <- tidyr::crossing(
    n_obs = n_obs_vec,
    target_censoring_prop = censoring_props,
    weight_type = weight_types
  ) %>%
    dplyr::arrange(n_obs, target_censoring_prop, weight_type) %>%
    dplyr::mutate(scenario_id = dplyr::row_number())

  sim_grid <- scenarios %>%
    tidyr::uncount(n_replicates, .id = "sim_id")

  save_one_sim <- function(
    n_obs,
    target_censoring_prop,
    weight_type,
    scenario_id,
    sim_id
  ) {
    params <- get_default_params()
    params$target_censoring_prop <- target_censoring_prop

    dat <- simulate_survival_data(
      n = n_obs,
      params = params,
      weight_type = weight_type
    )

    fname <- file.path(
      out_dir,
      sprintf(
        "sim_s%03d_r%03d_n%04d_c%0.1f_w%s.rds",
        scenario_id,
        sim_id,
        n_obs,
        target_censoring_prop,
        weight_type
      )
    )
    saveRDS(dat, fname, compress = "xz")
  }

  purrr::pwalk(.l = sim_grid, .f = save_one_sim)

  invisible(sim_grid)
}
