#' Calculate Monte Carlo Standard Error for mean/bias estimates
#'
#' Computes the MCSE for bias estimates following the standard formula:
#' \deqn{MCSE_{bias} = SD(estimates) / \sqrt{n}}
#' where n is the number of Monte Carlo replicates.
#'
#' @param x Numeric vector of parameter estimates across replications.
#'
#' @return Numeric value representing the MCSE. Returns \code{NA} if fewer than
#'   2 finite values are present.
#'
#' @section Reference:
#' Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
#' studies to evaluate statistical methods. Statistics in Medicine, 38(11),
#' 2074-2102.
#'
#' @examples
#' \dontrun{
#' # Simulate 200 estimates
#' estimates <- rnorm(200, mean = 5.1, sd = 0.3)
#' mcse_mean(estimates)  # ~ 0.021
#' }
#'
#' @export
mcse_mean <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n <= 1) {
    return(NA_real_)
  }
  sd(x) / sqrt(n)
}


#' Calculate Monte Carlo Standard Error for proportion estimates
#'
#' Computes the MCSE for binary outcomes (e.g., coverage indicators) using
#' the binomial standard error formula:
#' \deqn{MCSE_{prop} = \sqrt{p(1-p) / n}}
#' where p is the observed proportion and n is the number of replicates.
#'
#' @param x Numeric or logical vector of binary outcomes (0/1 or TRUE/FALSE).
#'
#' @return Numeric value representing the MCSE. Returns \code{NA} if no finite
#'   values are present.
#'
#' @section Usage:
#' Commonly used to assess Monte Carlo uncertainty in coverage probability
#' estimates. For 200 replicates with 95% coverage, MCSE ≈ 0.015.
#'
#' @examples
#' \dontrun{
#' # Coverage indicators for 200 replicates
#' coverage <- rbinom(200, 1, prob = 0.95)
#' mcse_prop(coverage)  # ~ 0.015
#' }
#'
#' @export
mcse_prop <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }
  p <- mean(x)
  sqrt(p * (1 - p) / n)
}


#' Calculate bias, RMSE, and their Monte Carlo Standard Errors
#'
#' Computes bias and root mean squared error (RMSE) from a set of parameter
#' estimates, along with their Monte Carlo standard errors. This is the primary
#' function for evaluating estimation performance in simulation studies following
#' the ADEMP framework.
#'
#' @param estimates Numeric vector of parameter estimates across Monte Carlo
#'   replicates.
#' @param truth Numeric scalar. The true parameter value used in data generation.
#'
#' @return A tibble with one row and four columns:
#'   \itemize{
#'     \item \code{bias} - Mean estimation error (mean(estimates) - truth)
#'     \item \code{bias_mcse} - Monte Carlo SE for bias
#'     \item \code{rmse} - Root mean squared error
#'     \item \code{rmse_mcse} - Monte Carlo SE for RMSE (approximate)
#'   }
#'   Returns tibble with \code{NA} values if estimates vector is empty or all
#'   non-finite.
#'
#' @section MCSE Formulas:
#' \itemize{
#'   \item Bias MCSE: \code{SD(errors) / sqrt(n)}
#'   \item RMSE MCSE: Approximate using \code{SD(squared_errors) / (2 * RMSE * sqrt(n))}
#' }
#'
#' @section Interpretation:
#' \itemize{
#'   \item Bias close to 0 indicates unbiased estimation
#'   \item RMSE quantifies overall estimation accuracy
#'   \item MCSEs quantify Monte Carlo uncertainty (should be small relative to estimates)
#' }
#'
#' @examples
#' \dontrun{
#' # True value: alpha = 5
#' estimates <- rnorm(200, mean = 5.05, sd = 0.30)
#' calc_bias_rmse(estimates, truth = 5.0)
#' # Returns tibble with bias ≈ 0.05, rmse ≈ 0.30, and their MCSEs
#' }
#'
#' @references
#' Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
#' studies to evaluate statistical methods. Statistics in Medicine, 38(11),
#' 2074-2102. \doi{10.1002/sim.8086}
#'
#' @export
calc_bias_rmse <- function(estimates, truth) {
  estimates <- estimates[is.finite(estimates)]
  n <- length(estimates)

  if (n == 0) {
    return(tibble::tibble(
      bias      = NA_real_,
      bias_mcse = NA_real_,
      rmse      = NA_real_,
      rmse_mcse = NA_real_
    ))
  }

  errors <- estimates - truth
  bias   <- mean(errors)
  rmse   <- sqrt(mean(errors^2))

  # MCSE for bias
  bias_mcse <- sd(errors) / sqrt(n)

  # MCSE for RMSE (approximation using delta method)
  # Var(RMSE) ≈ Var(errors^2) / (4 * RMSE^2)
  rmse_mcse <- sd((errors^2 - rmse^2)) / (2 * rmse * sqrt(n))

  tibble::tibble(
    bias      = bias,
    bias_mcse = bias_mcse,
    rmse      = rmse,
    rmse_mcse = rmse_mcse
  )
}
