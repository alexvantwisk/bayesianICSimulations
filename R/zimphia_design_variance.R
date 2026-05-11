#' Load the first `n_reps` ZIMPHIA replicate weights and join onto an analysis tibble
#'
#' @param base Tibble with `personid` to join on.
#' @param csv_path Path to `zimphia2020indintermediarywts.csv`.
#' @param n_reps Number of replicate weight columns to load (max 175).
#' @return Tibble with `personid` and `design_wt001 ... design_wtNNN`.
#' @examples
#' \dontrun{
#' base <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")
#' weights <- load_replicate_weights(
#'   base,
#'   "ZIMPHIA/ZIMPHIA 2020 Intermediary Weights (CSV)/zimphia2020indintermediarywts.csv",
#'   n_reps = 100L
#' )
#' }
#' @export
load_replicate_weights <- function(base, csv_path, n_reps = 100L) {
  if (!is.numeric(n_reps) || length(n_reps) != 1L ||
    n_reps != round(n_reps) || n_reps < 1L || n_reps > 175L) {
    stop("n_reps must be a whole-number scalar between 1 and 175; got ", n_reps,
      call. = FALSE
    )
  }
  if (!file.exists(csv_path)) {
    stop("Replicate weights file not found: ", csv_path, call. = FALSE)
  }
  if (!"personid" %in% names(base)) {
    stop("`base` must contain a `personid` column.", call. = FALSE)
  }
  rep_cols <- paste0("design_wt", sprintf("%03d", seq_len(n_reps)))
  wts <- readr::read_csv(
    csv_path,
    col_select = c("personid", dplyr::all_of(rep_cols)),
    show_col_types = FALSE
  )
  # Filter to only personids present in base
  dplyr::semi_join(wts, base, by = "personid")
}

#' Refit the ZIMPHIA HMC model using ZIMPHIA replicate weights
#'
#' For each replicate weight column, substitute it into the `weight` column of
#' the analysis tibble, run a single-chain HMC fit, and record the posterior
#' median + 95% CrI for `alpha`, `beta`, `gamma`. The design-based 95% CrI for
#' each parameter is the percentile interval of posterior medians across
#' replicates.
#'
#' @param analysis_data Prepared ZIMPHIA tibble (output of
#'   `run_zimphia_analysis()`).
#' @param weights_long Output of `load_replicate_weights()`.
#' @param output_dir Where to save per-replicate summaries.
#' @param stan_model_file Path to scalar-beta Stan model.
#' @param n_chains Default 1 (this is a design replicate, not a primary fit).
#' @return Tibble with `replicate`, `variable`, `median`, `q2.5`, `q97.5`,
#'   `rhat`, `ess_bulk`, `runtime_secs`.
#' @examples
#' \dontrun{
#' base <- readRDS("mcmc_outputs/zimphia/zimphia_prepared_data.rds")
#' weights <- load_replicate_weights(base, "ZIMPHIA/.../zimphia2020indintermediarywts.csv")
#' res <- fit_zimphia_design_replicates(base, weights)
#' }
#' @export
fit_zimphia_design_replicates <- function(
  analysis_data,
  weights_long,
  output_dir = "mcmc_outputs/zimphia_design_replicates",
  stan_model_file = "inst/models/loglogistic_interval.stan",
  n_chains = 1L
) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Package 'cmdstanr' is required but not installed.\n",
      "Install from https://mc-stan.org/r-packages/",
      call. = FALSE
    )
  }
  if (!file.exists(stan_model_file)) {
    stop("Stan model not found: ", stan_model_file, call. = FALSE)
  }
  required_cols <- c("personid", "L", "R", "X1", "weight")
  missing_cols <- setdiff(required_cols, names(analysis_data))
  if (length(missing_cols) > 0L) {
    stop("analysis_data is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(analysis_data$personid) > 0) {
    stop("analysis_data contains duplicate personid values.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rep_cols <- grep("^design_wt", names(weights_long), value = TRUE)
  if (length(rep_cols) == 0L) {
    stop("`weights_long` has no design_wt* columns.", call. = FALSE)
  }
  if (anyDuplicated(weights_long$personid) > 0) {
    stop("weights_long contains duplicate personid values; cannot join safely.",
      call. = FALSE
    )
  }
  joined <- dplyr::inner_join(analysis_data, weights_long, by = "personid")
  n_dropped <- nrow(analysis_data) - nrow(joined)
  if (n_dropped > 0L) {
    message(n_dropped, " rows in analysis_data had no match in weights_long and were dropped.")
  }
  mod <- cmdstanr::cmdstan_model(stan_model_file)

  results <- vector("list", length(rep_cols))
  for (i in seq_along(rep_cols)) {
    col <- rep_cols[i]
    rep_dir <- file.path(output_dir, sprintf("rep%03d", i))
    dir.create(rep_dir, showWarnings = FALSE, recursive = TRUE)

    w_raw <- joined[[col]]
    keep <- w_raw > 0
    sub <- joined[keep, ]
    N <- nrow(sub)
    if (N < 100) {
      warning("Replicate ", i, " has only ", N,
        " positive-weight rows; skipping.",
        call. = FALSE
      )
      next
    }
    w_norm <- w_raw[keep] * (N / sum(w_raw[keep]))
    if (!all(is.finite(w_norm))) {
      warning("Replicate ", i, ": weight normalisation produced non-finite values; skipping.",
        call. = FALSE
      )
      next
    }

    stan_data <- list(
      N = N,
      L = pmax(sub$L, 1e-10),
      R = sub$R,
      X = as.numeric(sub$X1),
      w = w_norm
    )

    t0 <- Sys.time()
    fit <- mod$sample(
      data = stan_data,
      chains = n_chains, parallel_chains = n_chains,
      iter_warmup = 2000, iter_sampling = 5000,
      seed = 2025 + i, refresh = 0
    )
    rt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    summ <- fit$summary(
      variables = c("alpha", "beta", "gamma"),
      "median", ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
      "rhat", "ess_bulk"
    ) |>
      dplyr::mutate(replicate = i, runtime_secs = rt)

    saveRDS(summ, file.path(rep_dir, "summary.rds"))
    results[[i]] <- summ

    message(sprintf(
      "Replicate %d/%d done in %.1f s (max Rhat = %.3f)",
      i, length(rep_cols), rt, max(summ$rhat, na.rm = TRUE)
    ))
  }

  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0L) {
    warning("All replicates were skipped; returning empty tibble.", call. = FALSE)
  }
  combined <- dplyr::bind_rows(results)
  readr::write_csv(combined, file.path(output_dir, "all_replicates.csv"))
  combined
}
