#' Prepare a multivariable design matrix for the ZIMPHIA fit
#'
#' Joins demographic columns from the individual file onto the analysis tibble
#' produced by `run_zimphia_analysis()` and returns a model matrix `X` of width
#' `K` together with the augmented data frame.
#'
#' @param base A data frame with at least `personid`, `L`, `R`, `X1`, `weight`.
#'   This is the analysis tibble produced by `run_zimphia_analysis()`.
#' @param indiv A data frame with `personid` and the demographic columns named
#'   by `covariates`. ZIMPHIA codings: `gender` (1=male, 2=female), `urban`
#'   (1=urban, 2=rural), `age` (years).
#' @param covariates Character vector of covariate names to include. Recognised:
#'   `"sex"`, `"urban_rural"`, `"age_band"`, `"wealth_quintile"`.
#'
#' @return A list with `data` (augmented tibble), `X` (numeric matrix N x K),
#'   and `covariate_levels` (factor levels for reference encoding).
#' @importFrom dplyr left_join
#' @importFrom stats model.matrix
#' @importFrom utils modifyList
#' @export
prepare_zimphia_multivariable_data <- function(base, indiv, covariates) {
  recognised <- c("sex", "urban_rural", "age_band", "wealth_quintile")
  unknown <- setdiff(covariates, recognised)
  if (length(unknown) > 0) {
    stop("Unrecognised covariates: ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  df <- dplyr::left_join(base, indiv, by = "personid")
  cols <- list()
  levels_out <- list()

  if ("sex" %in% covariates) {
    cols$sex <- as.numeric(df$gender == 2)
  }
  if ("urban_rural" %in% covariates) {
    cols$urban_rural <- as.numeric(df$urban == 2)
  }
  if ("age_band" %in% covariates) {
    band <- cut(df$age,
      breaks = c(14, 24, 34, 49, 64),
      labels = c("15-24", "25-34", "35-49", "50-64"),
      right = TRUE
    )
    # Drop unused levels so the model matrix only contains bands present in data
    band <- droplevels(band)
    levels_out$age_band <- levels(band)
    mat <- stats::model.matrix(~band)[, -1, drop = FALSE]
    colnames(mat) <- paste0("age_", levels(band)[-1])
    cols <- c(cols, as.list(as.data.frame(mat)))
  }
  if ("wealth_quintile" %in% covariates) {
    wq <- factor(df$wealthquintile)
    levels_out$wealth_quintile <- levels(wq)
    mat <- stats::model.matrix(~wq)[, -1, drop = FALSE]
    colnames(mat) <- paste0("wq_", levels(wq)[-1])
    cols <- c(cols, as.list(as.data.frame(mat)))
  }

  X <- do.call(cbind, cols)
  storage.mode(X) <- "numeric"
  list(data = df, X = X, covariate_levels = levels_out)
}

#' Fit the multivariable ZIMPHIA HMC model
#'
#' Wraps the generalised Stan model and saves the same summary/diagnostics/draws
#' layout as `run_zimphia_analysis()`.
#'
#' @param analysis_data Output of `prepare_zimphia_multivariable_data()`.
#' @param output_dir Where to write `summaries/`, `draws/`, `diagnostics/`.
#' @param stan_model_file Path to the multivariable Stan model.
#' @param hmc_settings Named list (defaults match `run_zimphia_analysis()`).
#' @return A list with `summary`, `draws`, `diagnostics`, `runtime_secs`.
#' @importFrom tibble tibble
#' @importFrom readr write_csv
#' @export
fit_zimphia_multivariable <- function(
  analysis_data,
  output_dir = "mcmc_outputs/zimphia_multivariable",
  stan_model_file = "inst/models/loglogistic_interval_multivariable.stan",
  hmc_settings = NULL
) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      "Package 'cmdstanr' is required but not installed.\n",
      "Install from https://mc-stan.org/r-packages/",
      call. = FALSE
    )
  }
  default_hmc <- list(
    n_chains = 4, n_warmup = 1000, n_sampling = 5000,
    parallel_chains = 4, seed = 2025, refresh = 500
  )
  hmc_settings <- if (is.null(hmc_settings)) {
    default_hmc
  } else {
    utils::modifyList(default_hmc, hmc_settings)
  }

  dir.create(file.path(output_dir, "summaries"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "draws"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "diagnostics"), recursive = TRUE, showWarnings = FALSE)

  stan_data <- list(
    N = nrow(analysis_data$data),
    K = ncol(analysis_data$X),
    L = pmax(analysis_data$data$L, 1e-10),
    R = analysis_data$data$R,
    X = analysis_data$X,
    w = analysis_data$data$weight
  )

  mod <- cmdstanr::cmdstan_model(stan_model_file)
  t0 <- Sys.time()
  fit <- mod$sample(
    data = stan_data,
    chains = hmc_settings$n_chains,
    parallel_chains = hmc_settings$parallel_chains,
    iter_warmup = hmc_settings$n_warmup,
    iter_sampling = hmc_settings$n_sampling,
    seed = hmc_settings$seed,
    refresh = hmc_settings$refresh
  )
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  summ <- fit$summary(
    variables = c("alpha", "beta", "gamma"),
    "mean", "median", "sd",
    ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
    "rhat", "ess_bulk", "ess_tail"
  )
  summ$covariate <- c("alpha", colnames(analysis_data$X), "gamma")

  saveRDS(summ, file.path(output_dir, "summaries", "summary.rds"))
  readr::write_csv(summ, file.path(output_dir, "summaries", "summary.csv"))

  draws_df <- fit$draws(variables = c("alpha", "beta", "gamma"), format = "df")
  saveRDS(draws_df, file.path(output_dir, "draws", "draws.rds"))

  diag <- tibble::tibble(
    max_rhat = max(summ$rhat, na.rm = TRUE),
    min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
    n_divergences = sum(fit$sampler_diagnostics(format = "df")$divergent__),
    runtime_secs = runtime,
    K = ncol(analysis_data$X)
  )
  saveRDS(diag, file.path(output_dir, "diagnostics", "diag.rds"))

  list(summary = summ, draws = draws_df, diagnostics = diag, runtime_secs = runtime)
}
