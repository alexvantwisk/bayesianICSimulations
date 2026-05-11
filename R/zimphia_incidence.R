#' Compute posterior age-specific hazard h(a | x) for a log-logistic AFT model
#'
#' For each draw `(alpha, beta, gamma)` and each age `a`, returns the hazard
#' \deqn{h(a | x) = (\gamma / \alpha_x) \cdot (a / \alpha_x)^{\gamma - 1} / (1 + (a / \alpha_x)^\gamma)}
#' with \eqn{\alpha_x = \alpha \cdot \exp(x^T \beta)}.
#'
#' @param draws Data frame of posterior draws with columns `alpha`, `beta`, `gamma`.
#' @param ages Numeric vector of ages to evaluate.
#' @param x Numeric scalar covariate value (e.g. 0 = male, 1 = female).
#' @return Tibble with columns `age`, `hazard_mean`, `hazard_q2.5`, `hazard_q97.5`.
#' @examples
#' draws <- tibble::tibble(alpha = c(5, 5), beta = c(0, 0), gamma = c(1.5, 1.5))
#' compute_age_specific_hazard(draws, ages = c(20, 30, 40))
#' @export
compute_age_specific_hazard <- function(draws, ages, x = 0) {
  required_cols <- c("alpha", "beta", "gamma")
  missing_cols <- setdiff(required_cols, names(draws))
  if (length(missing_cols) > 0L) {
    stop("draws is missing columns: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  hazard <- function(a, alpha, beta, gamma) {
    ax <- alpha * exp(beta * x)
    (gamma / ax) * (a / ax)^(gamma - 1) / (1 + (a / ax)^gamma)
  }
  out <- lapply(ages, function(a) {
    h <- hazard(a, draws$alpha, draws$beta, draws$gamma)
    tibble::tibble(
      age = a,
      hazard_mean = mean(h),
      hazard_q2.5 = stats::quantile(h, 0.025, names = FALSE),
      hazard_q97.5 = stats::quantile(h, 0.975, names = FALSE)
    )
  })
  dplyr::bind_rows(out)
}

#' Compute the population-level implied incidence per posterior draw
#'
#' For each draw, evaluates h(a_i | x_i) on the ZIMPHIA analysis tibble, then
#' weights by `weight` and averages to yield an incidence rate (per
#' person-year). Returns the posterior median and 95% credible interval.
#'
#' @param draws Tibble with one row per posterior draw, columns `alpha`,
#'   `beta`, `gamma`.
#' @param pop Tibble with one row per subject, columns `age`, `X1`, `weight`.
#' @return Tibble with one row: `incidence_median`, `incidence_q2.5`,
#'   `incidence_q97.5`, expressed per person-year.
#' @examples
#' draws <- tibble::tibble(alpha = 5, beta = 0, gamma = 1.5)
#' pop <- tibble::tibble(age = c(20, 30), X1 = c(0, 1), weight = c(1, 1))
#' compute_population_incidence(draws, pop)
#' @export
compute_population_incidence <- function(draws, pop) {
  required_draws <- c("alpha", "beta", "gamma")
  missing_draws <- setdiff(required_draws, names(draws))
  if (length(missing_draws) > 0L) {
    stop("draws is missing columns: ", paste(missing_draws, collapse = ", "),
      call. = FALSE
    )
  }
  required_pop <- c("age", "X1", "weight")
  missing_pop <- setdiff(required_pop, names(pop))
  if (length(missing_pop) > 0L) {
    stop("pop is missing columns: ", paste(missing_pop, collapse = ", "),
      call. = FALSE
    )
  }
  hazard_one <- function(alpha, beta, gamma) {
    ax <- alpha * exp(beta * pop$X1)
    h <- (gamma / ax) * (pop$age / ax)^(gamma - 1) /
      (1 + (pop$age / ax)^gamma)
    stats::weighted.mean(h, pop$weight)
  }
  per_draw <- mapply(hazard_one,
    draws$alpha, draws$beta, draws$gamma,
    USE.NAMES = FALSE
  )
  tibble::tibble(
    incidence_median = stats::median(per_draw),
    incidence_q2.5   = stats::quantile(per_draw, 0.025, names = FALSE),
    incidence_q97.5  = stats::quantile(per_draw, 0.975, names = FALSE)
  )
}
