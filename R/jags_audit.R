#' Audit which JAGS samplers are assigned to each node
#'
#' Loads the JAGS model on a tiny dataset and returns the per-node sampler
#' assignment reported by [rjags::list.samplers()]. Used to confirm whether
#' the project's MH baseline is actually slice-sampling rather than
#' adaptive Metropolis (see revision plan Task A).
#'
#' @param data A list suitable for [rjags::jags.model()] (N, L, R, X, w, zeros).
#' @param model_file Path to the JAGS model file.
#' @param n_chains Number of chains to initialise. Default 1.
#' @param n_adapt Number of adaptation iterations. Default 100.
#'
#' @return A tibble with columns `node` and `sampler`.
#' @examples
#' \dontrun{
#' dat <- list(
#'   N = 10, L = rexp(10, 1) + 1e-10, R = rep(1e11, 10),
#'   X = rbinom(10, 1, 0.5), w = rep(1, 10), zeros = rep(0, 10)
#' )
#' audit_jags_samplers(dat)
#' }
#' @export
audit_jags_samplers <- function(data,
                                model_file = system.file(
                                  "models", "loglogistic_interval.jags",
                                  package = "bayesianICSimulations"
                                ),
                                n_chains = 1L,
                                n_adapt = 100L) {
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop(
      "Package 'rjags' is required but not installed.\n",
      "Install JAGS from: https://mcmc-jags.sourceforge.io/",
      call. = FALSE
    )
  }
  if (!nzchar(model_file)) {
    stop("JAGS model file path is empty; was the package installed?", call. = FALSE)
  }
  if (!file.exists(model_file)) {
    stop("JAGS model file not found: ", model_file, call. = FALSE)
  }
  jags_model <- rjags::jags.model(
    file = model_file,
    data = data,
    n.chains = n_chains,
    n.adapt = n_adapt,
    quiet = TRUE
  )
  samplers <- rjags::list.samplers(jags_model)

  tibble::tibble(
    node = unlist(samplers, use.names = FALSE),
    sampler = rep(names(samplers), lengths(samplers))
  )
}
