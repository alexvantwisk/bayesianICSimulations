# ZIMPHIA Application Summary

## 1. Data Preparation

-   Input files and locations
    -   `ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)/zimphia2020adultbio.csv` (CSV) read with selected columns `personid, hivstatusfinal, btwt0, bt_status, age, gender` (R/zimphia_analysis.R:127)
    -   `ZIMPHIA/ZIMPHIA 2020 Datasets (CSV)/zimphia2020adultind.csv` (CSV) read with selected columns `personid, firstsxage, sexever, hivtfposy, gender` (R/zimphia_analysis.R:128)
    -   Merge on `personid` via left join (R/zimphia_analysis.R:161)
-   Variable mapping to simulation equivalents
    -   Interval bounds: `L = firstsxage` (floored by `lower_bound_floor`, default 1e-10) (R/zimphia_analysis.R:233)
    -   Right bound: `R = age` if HIV-positive (`hivstatusfinal == 1`); `R = Inf` if HIV-negative (`hivstatusfinal == 2`) (R/zimphia_analysis.R:233)
    -   Non-positive widths: if finite `R <= L`, set `R = L + interval_min_width` (default 1e-6) (R/zimphia_analysis.R:258)
    -   Covariate: `X1` recode of gender — male (`gender == 1`) → 0; female (`gender == 2`) → 1 (R/zimphia_analysis.R:277)
    -   Model inputs: Stan/JAGS receive `N, L, R, X = X1, w = weight` (R/zimphia_analysis.R:318)
-   Handling of missing data, exclusions, recoding
    -   Keep `bt_status == 1` (valid biomarker) (R/zimphia_analysis.R:171)
    -   Keep `age >= 15` (R/zimphia_analysis.R:177)
    -   Keep `sexever == 1` (R/zimphia_analysis.R:183)
    -   Drop missing `firstsxage` (R/zimphia_analysis.R:189)
    -   Drop missing `gender` (R/zimphia_analysis.R:195)
    -   Exclude HIV+ with missing first positive test date `hivtfposy` (R/zimphia_analysis.R:202)
    -   Keep positive survey weight `btwt0 > 0` and non-missing (R/zimphia_analysis.R:214)
    -   Gender recode to `X1` as above (R/zimphia_analysis.R:277)
-   Weighting and normalization
    -   Normalization rule: `weight = btwt0 * N / sum(btwt0)` so that `sum(weight) = N` (R/zimphia_analysis.R:298)
    -   Both Stan and JAGS re-normalize internally (`wN = w * N / sum(w)`) before applying weighted likelihood (inst/models/loglogistic_interval.stan:14; inst/models/loglogistic_interval.jags:28)
-   Survey design features (PSU, strata)
    -   ⚠️ Information not found in repository. No PSU/strata variables are used in the application scripts.

## 2. Model Implementation

-   Model family and files
    -   Log-logistic AFT model with interval censoring, common parameterization across Stan/JAGS using `alpha` (scale), `beta` (covariate effect), `gamma` (shape) (inst/models/loglogistic_interval.stan:1; inst/models/loglogistic_interval.jags:1)
    -   Stan: `inst/models/loglogistic_interval.stan` with weighted log-likelihood and `wN` normalization (inst/models/loglogistic_interval.stan:14)
    -   JAGS: `inst/models/loglogistic_interval.jags` using zeros-trick and `wN` weights with `R > 1e10` as right-censor placeholder (inst/models/loglogistic_interval.jags:1)
-   Priors
    -   `alpha ~ lognormal(log(5), 1)`; `beta ~ normal(0, 1)`; `gamma ~ lognormal(0, 0.5)` (inst/models/loglogistic_interval.stan:32; inst/models/loglogistic_interval.jags:40)
-   Sampler configuration
    -   HMC (CmdStanR): `chains=4`, `warmup=1000`, `sampling=5000`, `parallel_chains=4`, `seed=2025`, `refresh=500` (R/zimphia_analysis.R:98)
    -   MH (JAGS): `chains=4`, `adapt=1000`, `burnin=1000`, `iter=5000`, `seed=2025` (R/zimphia_analysis.R:113)
    -   Right-censoring in JAGS: finite placeholder `1e11` passed for `Inf` (R/zimphia_analysis.R:452)
-   Software and versions
    -   R 4.5.1; GCC 9.4.0; JAGS 4.3.2; CmdStan installed via cmdstanr on HPC (inst/hpc/setup_hpc.sh:15,149,334,268)
    -   Exact CmdStan version: ⚠️ Information not found in repository.
    -   R packages used in application: `cmdstanr`, `rjags`, `posterior`, `coda`, plus `dplyr`, `readr`, `tibble` for preparation (R/zimphia_analysis.R:111,145)

## 3. Analysis Protocol

-   Subsetting logic
    -   Analytic subset after sequential filters: valid biomarker, age ≥ 15, sexually active, non-missing debut age, recorded gender, HIV+ must have documented first-positive date, and positive weight (R/zimphia_analysis.R:171)
-   Interval construction
    -   `L = firstsxage` floored at small positive value; `R = age` for HIV+, `R = Inf` for HIV−; ensure positive width (`R > L`) or set `R = L + interval_min_width` (R/zimphia_analysis.R:233,258)
-   Diagnostics thresholds (gates)
    -   Split-R̂ \< 1.01, bulk ESS \> 400, zero HMC divergences — stated and used for interpretation; diagnostics recorded in CSV/RDS outputs (R/zimphia_analysis.R:38; R/zimphia_analysis.R:379)
-   Weight trimming or sensitivity analyses
    -   ⚠️ Information not found in repository. No trimming/sensitivity scripts detected.
-   Random seeds and reproducibility
    -   Seeds set via defaults: HMC `seed = 2025`; MH `seed = 2025` (R/zimphia_analysis.R:98,113)
    -   Prepared analysis dataset and all outputs persisted under `mcmc_outputs/zimphia` for reproducibility (R/zimphia_analysis.R:208)

## 4. Tables and Figures

-   Tables generated (saved under `outputs/tables/`)
    -   Table 3.6: ZIMPHIA 2020 analysis sample characteristics
        -   Function: `table_3_6_sample_characteristics()` (R/tables.R:1603)
        -   File: `outputs/tables/table3_6_sample_characteristics.tex`
        -   Columns: sectioned rows with “Value” and “Detail”; includes composition, interval structure, weight dispersion, and missingness flow.
        -   Underlying data: `mcmc_outputs/zimphia/zimphia_prepared_data.rds`; missing-data flow constructed from raw CSVs (R/tables.R:1464)
    -   Table 3.7: Sampler performance on ZIMPHIA 2020 data
        -   Function: `table_3_7_sampler_performance()` (R/tables.R:1565)
        -   File: `outputs/tables/table3_7_sampler_performance.tex`
        -   Columns: `metric`, `HMC`, `MH`, `HMC ÷ MH`; metrics include runtime (minutes), ESS/s (α, β), split-R̂ \> 1.01, divergences, MH acceptance rate.
        -   Inputs: summaries and diagnostics from `mcmc_outputs/zimphia/...` (R/tables.R:1569)
    -   Table 3.8: Posterior estimates for ZIMPHIA application
        -   Function: `table_3_8_parameter_estimates()` (R/tables.R:1677)
        -   File: `outputs/tables/table3_8_parameter_estimates.tex`
        -   Columns: `HMC median (95% CrI)`, `MH median (95% CrI)` for α (baseline median), β (gender effect), γ (shape).
    -   Table 3.9: ZIMPHIA vs simulation scalability (n ≈ 16,554)
        -   Function: `table_3_9_scalability_validation()` (R/tables.R:1770)
        -   File: `outputs/tables/table3_9_scalability_validation.tex`
        -   Columns: Predicted vs Observed (runtime, ESS/s) for HMC and MH, plus Obs/Pred ratios and HMC ÷ MH.
        -   Simulation inputs: `outputs/analysis/efficiency_comparisons.csv` (outputs/analysis/efficiency_comparisons.csv:1)
-   Figures produced for thesis (saved under `outputs/figures/` via `save_zimphia_figures()`)
    -   Figure 3.6: `fig3_6_zimphia_interval_patterns.png`
        -   Function: `create_figure3_6_zimphia_interval_patterns()` (R/figures.R:2515)
        -   Visualizes: Interval width distributions (R−L) overall and by gender; ZIMPHIA (weighted hist/density) vs sampled simulation replicates; dashed medians; y-axis log scale for counts (R/figures.R:2552)
        -   Inputs: `mcmc_outputs/zimphia/zimphia_prepared_data.rds`; simulation widths collected from `data/` (R/figures.R:2527)
    -   Figure 3.7: `fig3_7_zimphia_convergence.png`
        -   Function: `create_figure3_7_zimphia_convergence_traces()` (R/figures.R:2699)
        -   Visualizes: Trace plots for α and β by sampler and chain with overlays of posterior medians; annotations include split-R̂, bulk ESS, and HMC divergences (R/figures.R:2886)
        -   Inputs: Draws `mcmc_outputs/zimphia/hmc/draws/zimphia_hmc_draws.rds`, `mcmc_outputs/zimphia/mh/draws/zimphia_mh_draws.rds`; summaries CSVs (R/figures.R:2710)
    -   Figure 3.8: `fig3_8_zimphia_posterior_forest.png`
        -   Function: `create_figure3_8_zimphia_posterior_forest()` (R/figures.R:2837)
        -   Visualizes: Posterior medians and 95% CrIs for α, β, γ side-by-side by sampler; overlays simulation “truth” (dotted) and 95% bands for the nearest simulation scenario in n, censoring, and weight CV (R/figures.R:3031)
        -   Inputs: ZIMPHIA summaries; simulation `outputs/combined_results/combined_summaries.rds` (R/figures.R:2889)
-   Table/figure crosswalk
    -   Table 3.7 complements Figure 3.7 (sampler performance vs trace behavior).
    -   Table 3.8 aligns with Figure 3.8 (posterior summaries vs visual intervals).
    -   Table 3.6 stands alone (sample characteristics informing Figure 3.6 context).

## 5. Comparison with Simulation

-   Runtime and efficiency scaling
    -   Table 3.9 fits log–log scaling `log(metric) ~ log(n_obs)` per method from simulation and predicts for ZIMPHIA sample size; compares observed vs predicted (R/tables.R:1811)
    -   Simulation source file for scaling: `outputs/analysis/efficiency_comparisons.csv` (outputs/analysis/efficiency_comparisons.csv:1)
-   Posterior comparison
    -   Figure 3.8 selects the closest simulation scenario in n, censoring rate, and weight CV to provide a reference band and truth overlay (R/figures.R:2851)
    -   Programmatic guards warn on mismatch tolerances for n and censoring (R/figures.R:2913)

## 6. Reproducibility Metadata

-   Prepared data and outputs (ZIMPHIA application)
    -   Prepared dataset: `mcmc_outputs/zimphia/zimphia_prepared_data.rds` (R/zimphia_analysis.R:208)
    -   Summaries: `mcmc_outputs/zimphia/hmc/summaries/zimphia_hmc_summary.csv`, `mcmc_outputs/zimphia/mh/summaries/zimphia_mh_summary.csv` (R/zimphia_analysis.R:361)
    -   Diagnostics: `mcmc_outputs/zimphia/hmc/diagnostics/zimphia_hmc_diagnostics.csv`, `mcmc_outputs/zimphia/mh/diagnostics/zimphia_mh_diagnostics.csv` (R/zimphia_analysis.R:388)
    -   Draws: `mcmc_outputs/zimphia/hmc/draws/zimphia_hmc_draws.rds`, `mcmc_outputs/zimphia/mh/draws/zimphia_mh_draws.rds` (R/zimphia_analysis.R:347)
    -   Fits: `mcmc_outputs/zimphia/hmc/fits/zimphia_hmc_fit.rds`, `mcmc_outputs/zimphia/mh/fits/zimphia_mh_fit.rds` (R/zimphia_analysis.R:354)
-   Seeds
    -   HMC `seed = 2025`, MH `seed = 2025` (R/zimphia_analysis.R:98,113)
-   Software/versions
    -   R 4.5.1; GCC 9.4.0; JAGS 4.3.2; CmdStan via cmdstanr (exact version not recorded) (inst/hpc/setup_hpc.sh:15,149,334,268)
-   Session info
    -   ⚠️ Information not found in repository.
-   Git commit
    -   Current HEAD: `db98ab9f63de57cb040991f68a7563b577410bbb`