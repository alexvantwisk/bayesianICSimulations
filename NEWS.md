# bayesianICSimulations 0.2.0 (2026-05-11)

Substantive revision in response to BMC reviewer feedback (R&R round 2).
New features and analyses:

- **JAGS sampler audit** (`audit_jags_samplers()`). Confirms that the project's
  MH baseline is actually JAGS univariate slice sampling — Appendix Algorithm 4
  rewritten to match.
- **Multivariable ZIMPHIA fit** (`fit_zimphia_multivariable()`,
  `prepare_zimphia_multivariable_data()`) + a generalised vector-beta Stan
  model.
- **Birth-cohort stratified fits** (`fit_zimphia_cohort()`,
  `derive_birth_cohort()`).
- **Implied age-specific hazard + population incidence post-processing**
  (`compute_age_specific_hazard()`, `compute_population_incidence()`).
  fig8_incidence_curve.png and outputs/tables/tab_population_incidence.csv.
- **Design-based variance via ZIMPHIA replicate weights**
  (`fit_zimphia_design_replicates()`, `load_replicate_weights()`).
- **Weibull misspecification simulation** (`simulate_survival_data_weibull()`,
  `run_misspec_simulation()`) plus new `k_weibull` / `lambda_weibull`
  parameters in `get_default_params()`.
- **Asymmetric replicate-count support** in `run_simulations()` via the new
  `n_replicates_hmc` and `samplers` arguments.

Other:

- Tests scaffolded with testthat 3e under `tests/testthat/`. 37+ expectations.
- Manuscript edits applied directly to `../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`.
- Response letter drafted at `../../10_BMC_Submission/04_Cover_Letter/response_letter.md`.
- Long-running simulation runs (Task E ~2.4h, Task F MH ~25h, Task G n=10,000 ~25h) deferred to user execution after cmdstanr install.

See `tasks/revision_plan_substantive.md` and `tasks/revision_plan_executable.md`
for the full plan.
