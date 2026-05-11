#!/usr/bin/env Rscript
# Task D: Implied incidence derivation
#
# Uses the saved ZIMPHIA HMC draws from the primary fit (no cmdstanr needed).
suppressPackageStartupMessages({
  devtools::load_all()
  library(dplyr)
  library(ggplot2)
})

draws_file <- "mcmc_outputs/zimphia/hmc/draws/zimphia_hmc_draws.rds"
prepared_file <- "mcmc_outputs/zimphia/zimphia_prepared_data.rds"
if (!file.exists(draws_file)) {
  stop("Posterior draws not found: ", draws_file, call. = FALSE)
}
if (!file.exists(prepared_file)) {
  stop("Prepared data not found: ", prepared_file, call. = FALSE)
}

draws <- readRDS(draws_file)
pop <- readRDS(prepared_file)

dir.create("mcmc_outputs/zimphia_incidence", showWarnings = FALSE, recursive = TRUE)
age_grid <- seq(15, 60, by = 0.5)

hazard_male <- compute_age_specific_hazard(draws, age_grid, x = 0) |>
  mutate(sex = "Male")
hazard_female <- compute_age_specific_hazard(draws, age_grid, x = 1) |>
  mutate(sex = "Female")
hazard_curves <- bind_rows(hazard_male, hazard_female)
saveRDS(hazard_curves, "mcmc_outputs/zimphia_incidence/hazard_curves.rds")

p <- ggplot(
  hazard_curves,
  aes(
    x = age, y = hazard_mean, ymin = hazard_q2.5,
    ymax = hazard_q97.5, fill = sex, color = sex
  )
) +
  geom_ribbon(alpha = 0.25, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  facet_wrap(~sex) +
  labs(
    x = "Age (years)",
    y = "Implied hazard (per person-year)",
    title = "ZIMPHIA-implied age-specific HIV incidence hazard"
  ) +
  theme_bw() +
  theme(legend.position = "none")
ggsave("outputs/figures/fig8_incidence_curve.png", p,
  width = 9, height = 4, dpi = 320
)

pop_inc <- compute_population_incidence(draws, pop |> select(age, X1, weight))
pop_inc_per100 <- pop_inc |>
  mutate(across(starts_with("incidence_"), \(x) x * 100))
print(pop_inc_per100)
readr::write_csv(
  pop_inc_per100,
  "outputs/tables/tab_population_incidence.csv"
)
