#' Internal helper functions for survival visualization
#'
#' @keywords internal
#' @noRd
NULL

#' Safely compute coefficient of variation
#'
#' @param w Numeric vector of weights
#' @return Scalar CV, or NA if mean is near zero
#' @keywords internal
safe_cv <- function(w) {
  w <- w[is.finite(w)]
  if (length(w) == 0) {
    return(NA_real_)
  }

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
#' Calculates the true survival, cumulative hazard, or hazard function for a
#' log-logistic distribution with AFT parameterization.
#'
#' @param t Numeric vector of time points
#' @param alpha Baseline scale parameter
#' @param gamma Shape parameter
#' @param beta AFT coefficient (default 0)
#' @param X1 Covariate value (default 0)
#' @param type Character: "S" (survival), "H" (cumulative hazard), "h" (hazard)
#' @return Numeric vector of function values at t
#'
#' @section Log-Logistic Formulas:
#' \itemize{
#'   \item AFT parameterization: \code{alpha_i = alpha * exp(beta * X1)}
#'   \item Survival: \code{S(t) = 1 / (1 + (t/alpha_i)^gamma)}
#'   \item Cumulative hazard: \code{H(t) = -log(S(t))}
#'   \item Hazard: \code{h(t) = (gamma/alpha_i) * (t/alpha_i)^(gamma-1) / (1 + (t/alpha_i)^gamma)}
#' }
#'
#' @keywords internal
#' @export
compute_true_loglogistic <- function(
  t,
  alpha,
  gamma,
  beta = 0,
  X1 = 0,
  type = c("S", "H", "h")
) {
  type <- match.arg(type)

  # AFT parameterization: log(alpha_i) = log(alpha) + beta * X1
  alpha_i <- alpha * exp(beta * X1)

  # Log-logistic survival function
  S_t <- 1 / (1 + (t / alpha_i)^gamma)

  switch(type, "S" = S_t, "H" = -log(S_t), "h" = {
    # h(t) = (gamma/alpha_i) * (t/alpha_i)^(gamma-1) / (1 + (t/alpha_i)^gamma)
    (gamma / alpha_i) * (t / alpha_i)^(gamma - 1) / (1 + (t / alpha_i)^gamma)
  })
}


#' Compute marginal survival averaged over covariate distribution
#'
#' Calculates the marginal (population-averaged) survival, cumulative hazard,
#' or hazard function by averaging over a binary covariate distribution.
#'
#' @param t Numeric vector of time points
#' @param alpha Baseline scale parameter
#' @param gamma Shape parameter
#' @param beta AFT coefficient
#' @param X1_dist Numeric vector of length 2: c(P(X1=0), P(X1=1)). Must sum to 1.
#' @param type Character: "S", "H", or "h"
#' @return Numeric vector of marginal function values
#'
#' @section Computation:
#' For survival: \code{S_marginal(t) = P(X1=0)*S(t|X1=0) + P(X1=1)*S(t|X1=1)}
#'
#' @keywords internal
#' @export
compute_marginal_survival <- function(
  t,
  alpha,
  gamma,
  beta,
  X1_dist = c(0.5, 0.5),
  type = c("S", "H", "h")
) {
  type <- match.arg(type)

  if (length(X1_dist) != 2 || abs(sum(X1_dist) - 1) > 1e-6) {
    stop("X1_dist must be a length-2 vector summing to 1")
  }

  # Compute conditional curves
  S0 <- compute_true_loglogistic(t, alpha, gamma, beta, X1 = 0, type = "S")
  S1 <- compute_true_loglogistic(t, alpha, gamma, beta, X1 = 1, type = "S")

  # Marginal survival
  S_marginal <- X1_dist[1] * S0 + X1_dist[2] * S1

  switch(type, "S" = S_marginal, "H" = -log(S_marginal), "h" = {
    # h(t) = -d/dt log(S(t)) using finite differences
    H_marginal <- -log(S_marginal)
    dt <- c(diff(t), diff(t)[length(t) - 1])
    dH <- c(diff(H_marginal), diff(H_marginal)[length(t) - 1])
    pmax(dH / dt, 0) # Ensure non-negative
  })
}
