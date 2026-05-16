#' Assign each subject to a birth cohort based on survey year and age
#'
#' Cohorts: 1965-1979 (older), 1980-1989 (middle), 1990+ (younger). Returns
#' NA for ages outside the studied range.
#'
#' @param age Numeric vector of ages at survey.
#' @param survey_year Numeric. Default 2020 (ZIMPHIA).
#' @return Factor with three levels ordered older-to-younger.
#' @examples
#' derive_birth_cohort(age = c(25, 35, 50), survey_year = 2020)
#' @export
derive_birth_cohort <- function(age, survey_year = 2020) {
  birth_year <- survey_year - age
  factor(
    dplyr::case_when(
      birth_year >= 1965 & birth_year <= 1979 ~ "1965-1979",
      birth_year >= 1980 & birth_year <= 1989 ~ "1980-1989",
      birth_year >= 1990 ~ "1990+",
      TRUE ~ NA_character_
    ),
    levels = c("1965-1979", "1980-1989", "1990+")
  )
}

#' Fit the ZIMPHIA HMC model separately on three birth cohorts
#'
#' @param analysis_data Prepared ZIMPHIA analysis tibble (output of
#'   `run_zimphia_analysis()`).
#' @param output_dir Root output directory; per-cohort subdirectories created.
#' @param stan_model_file Path to scalar-beta Stan model (univariate, sex only).
#' @return A tibble with one row per cohort: cohort name, n, posterior summaries
#'   for `alpha`, `beta`, `gamma`, runtime, convergence flags.
#' @examples
#' \dontrun{
#' analysis_data <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")
#' res <- fit_zimphia_cohort(analysis_data)
#' }
#' @export
fit_zimphia_cohort <- function(
  analysis_data,
  output_dir = "mcmc_outputs/zimphia_cohort",
  stan_model_file = "inst/models/loglogistic_interval.stan"
) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required but not installed.\n",
      "Install from https://mc-stan.org/r-packages/",
      call. = FALSE
    )
  }
  required_cols <- c("age", "L", "R", "X1", "weight")
  missing_cols <- setdiff(required_cols, names(analysis_data))
  if (length(missing_cols) > 0L) {
    stop("analysis_data is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!file.exists(stan_model_file)) {
    stop("Stan model not found: ", stan_model_file, call. = FALSE)
  }

  work_data <- dplyr::mutate(analysis_data, cohort = derive_birth_cohort(age))
  cohort_levels <- levels(work_data$cohort)

  results <- list()
  mod <- cmdstanr::cmdstan_model(stan_model_file)

  for (lvl in cohort_levels) {
    sub <- dplyr::filter(work_data, .data$cohort == lvl)
    if (nrow(sub) < 100) {
      warning("Cohort ", lvl, " has only ", nrow(sub),
        " observations; skipping",
        call. = FALSE
      )
      next
    }
    out_sub <- file.path(output_dir, gsub("\\+", "plus", lvl))
    dir.create(file.path(out_sub, "summaries"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(out_sub, "draws"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(out_sub, "diagnostics"), recursive = TRUE, showWarnings = FALSE)

    stan_data <- list(
      N = nrow(sub),
      L = pmax(sub$L, 1e-10),
      R = sub$R,
      X = as.numeric(sub$X1),
      w = sub$weight
    )

    t0 <- Sys.time()
    fit <- mod$sample(
      data = stan_data,
      chains = 4, parallel_chains = 4,
      iter_warmup = 1000, iter_sampling = 5000,
      seed = 2025, refresh = 0
    )
    rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    summ <- fit$summary(
      variables = c("alpha", "beta", "gamma"),
      "mean", "median", "sd",
      ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
      "rhat", "ess_bulk"
    )
    summ <- summ |>
      dplyr::mutate(
        cohort = lvl,
        n = nrow(sub),
        runtime_secs = rt,
        converged = rhat <= 1.01 & ess_bulk >= 400
      )

    diag <- tibble::tibble(
      cohort = lvl,
      n = nrow(sub),
      max_rhat = max(summ$rhat, na.rm = TRUE),
      min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
      n_divergences = sum(fit$sampler_diagnostics(format = "df")$divergent__),
      runtime_secs = rt
    )

    saveRDS(summ, file.path(out_sub, "summaries", "summary.rds"))
    saveRDS(
      fit$draws(variables = c("alpha", "beta", "gamma"), format = "df"),
      file.path(out_sub, "draws", "draws.rds")
    )
    saveRDS(diag, file.path(out_sub, "diagnostics", "diag.rds"))

    results[[lvl]] <- summ
  }

  if (length(results) == 0L) {
    warning("All cohorts were skipped (n < 100); returning empty tibble.",
      call. = FALSE
    )
  }

  combined <- dplyr::bind_rows(results)
  readr::write_csv(combined, file.path(output_dir, "cohort_compare.csv"))
  combined
}
