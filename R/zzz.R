#' Package startup message
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "bayesianICSimulations v", utils::packageVersion("bayesianICSimulations"), "\n",
    "Bayesian Interval-Censored Survival Data Simulation and Analysis\n",
    "For HMC: Install cmdstanr from https://mc-stan.org/r-packages/\n",
    "For MH: Install JAGS from https://mcmc-jags.sourceforge.io/"
  )
}
