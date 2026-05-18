# Substantive-Revision Plan — BMC Submission

**Companion to:** [`../../10_BMC_Submission/revision_plan.md`](../../10_BMC_Submission/revision_plan.md) (cosmetic items applied 2026-05-11, see §10 changelog).

**Paths in this file are relative to the code-project root (`04_Code/`).**

**Scope:** All revision items left open after the cosmetic round — substantive simulations, additional ZIMPHIA analyses, manuscript section rewrites, and the response letter draft.

---

## 0. Decisions captured (from user, 2026-05-11)

1. **Misspecification simulation:** one cell only — `n=2,000, censoring=0.3, weight CV≈0.5`, 200 HMC + 200 MH reps.
2. **HMC 1,000-rep headline rerun:** central cells only at `n=2,000` and `n=10,000` (one cell per `n` at censoring 0.3 / CV 0.5).
3. **Compute:** laptop only — no cloud, no HPC.
4. **Response letter:** drafted alongside manuscript edits in this same round (artefact: `../../10_BMC_Submission/04_Cover_Letter/response_letter.md`).

---

## 1. Strategic frame for this round

The narrative payoff (R2 + R1 implicit) is the **asymmetry-as-feature** argument:

> Reviewer 2 raised the question of whether HMC's efficiency advantage translates to a meaningful practical benefit given that the model needs to be fit only once. We respectfully disagree on three grounds. First, ESS/s — not wall time — is the operative comparator, and HMC's ~72× advantage on ZIMPHIA holds independently of whether the model is fit once or many times. Second, the design-based variance correction added in this revision required 100 bootstrap refits — feasible in 2 hours with HMC, but ~50 hours with MH. Third, the simulation revision Reviewer 2 itself requested (increased replicates) was only computationally feasible asymmetrically: we increased HMC to 1,000 replicates per cell but could not match this for MH within reasonable compute time. The practical asymmetry illustrated in this revision is, we suggest, the strongest answer to the reviewer's question.

Every substantive task in this plan exists to *populate* that argument with evidence. Keep that in mind when choosing scope: the cheapest credible answer is also the strategically strongest, because it preserves the asymmetry.

---

## 2. Repository orientation (key files, paths relative to `04_Code/`)

These are the files the implementation will touch or call into:

| Purpose | Path |
|---|---|
| Log-logistic DGM + simulation runner | [`R/simulation.R`](../R/simulation.R) — `simulate_survival_data` (line 193), `run_simulations` (line 262) |
| HMC fitting | [`R/fitting_hmc.R`](../R/fitting_hmc.R) — `fit_logistic_hmc` (line 105) |
| MH (JAGS) fitting | [`R/fitting_mh.R`](../R/fitting_mh.R) — `fit_logistic_mh` (line 114) |
| ZIMPHIA analysis | [`R/zimphia_analysis.R`](../R/zimphia_analysis.R) — `run_zimphia_analysis` (line 66) |
| Analysis pipeline | [`R/analysis_pipeline.R`](../R/analysis_pipeline.R) |
| Combine + tabulate | [`R/combine_results.R`](../R/combine_results.R), [`R/tables.R`](../R/tables.R) |
| Figures | [`R/figures.R`](../R/figures.R), [`R/viz_survival.R`](../R/viz_survival.R) |
| Stan + JAGS models | [`inst/models/loglogistic_interval.stan`](../inst/models/loglogistic_interval.stan), [`inst/models/loglogistic_interval.jags`](../inst/models/loglogistic_interval.jags) |
| Headline scripts | [`inst/scripts/run_fits.R`](../inst/scripts/run_fits.R), [`inst/scripts/02_analysis.R`](../inst/scripts/02_analysis.R) |
| ZIMPHIA replicate weights | [`ZIMPHIA/ZIMPHIA 2020 Intermediary Weights (CSV)/zimphia2020indintermediarywts.csv`](../ZIMPHIA/ZIMPHIA%202020%20Intermediary%20Weights%20%28CSV%29/zimphia2020indintermediarywts.csv) — 175 jackknife/BRR replicate weights `design_wt001…design_wt175` plus `varstrat` (strata) and `varunit` (PSU) columns. **This means no bootstrap needs to be coded from scratch — refit per replicate weight column.** |
| Manuscript | [`../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex`](../../10_BMC_Submission/01_Manuscript/manuscript_sn.tex) |

---

## 3. Workflow — cheapest first, long sims overnight

The order is chosen so that the manuscript can be drafted while long sims run unattended. Free/cheap wins land first; long compute is overlaid on writing days.

### Task A — JAGS sampler audit (revision_plan §3.2)  *— 30 min, no compute*

R1's "MH is basic" comment hinges on assumed adaptive-RWM with a tuned proposal. The JAGS model at [`inst/models/loglogistic_interval.jags`](../inst/models/loglogistic_interval.jags) uses the **zeros trick** (`zeros[i] ~ dpois(phi[i])`) to inject a custom likelihood. JAGS then chooses samplers per-node from its module library; for continuous unconstrained parameters with custom likelihoods, the default is **slice sampling** (Neal 2003) — *not* adaptive Metropolis.

**Action:** Confirm the actual sampler used at runtime by `list.samplers()`-style introspection in `rjags`:

```r
library(rjags)
m <- jags.model("inst/models/loglogistic_interval.jags", data = ..., n.chains = 1)
list.samplers(m)
```

Record the output. If (as expected) slice samplers are reported for `alpha`, `beta`, `gamma`, the manuscript's Algorithm 4 description (current "adaptive RWM with Gaussian proposal") is wrong and needs replacing with a slice-sampler description. Critically: **slice sampling is a stronger baseline than R1 assumed**, which converts R1's "basic MH" complaint into a paper strength rather than a weakness.

**Deliverable:** A one-line entry in the implementation log of which samplers JAGS used, and a note on whether Algorithm 4 in Appendix needs rewriting.

---

### Task B — Multivariable ZIMPHIA fit (revision_plan §3.4)  *— ~2 min compute + 1h writeup*

Run **one** additional HMC fit on ZIMPHIA with covariates: `sex + urban_rural + age_band` (or `sex + urban_rural + wealth_quintile` if `age_band` is collinear with the outcome). Report posterior for `β_sex` alongside the original single-covariate fit.

**Implementation:** Extend [`R/zimphia_analysis.R`](../R/zimphia_analysis.R) (or write a thin wrapper in `inst/scripts/`) that:
1. Joins demographic columns from the ZIMPHIA individual file onto the existing analysis tibble.
2. Builds a model matrix `X` instead of the current single column.
3. Calls `fit_logistic_hmc()` with the new `X`.

The Stan model at [`inst/models/loglogistic_interval.stan`](../inst/models/loglogistic_interval.stan) currently hardcodes a scalar `beta` — confirm and, if needed, generalise to vector `beta` of length `K`. (This is a small Stan-side edit; preserve the original scalar model as `loglogistic_interval_univariate.stan` for backwards compatibility.)

**Writeup:** One new subsection in §4 (Results) — "Multivariable sensitivity" — with a single table comparing `β_sex` under univariate vs multivariable fits. If the estimate stays within the original CrI, defend the single-covariate exposition. If it shifts, report transparently and note that the headline sampler-comparison conclusions are unaffected.

---

### Task C — Birth-cohort stratified ZIMPHIA fits (revision_plan §3.5)  *— ~6 min compute + 1h writeup*

Three HMC fits on ZIMPHIA, stratified by birth cohort:
- Cohort 1: 1965–1979 (older)
- Cohort 2: 1980–1989 (middle)
- Cohort 3: 1990+ (younger — narrowest retrospective window)

Compare posterior `γ` and `α` across cohorts. If stable → defends the constant-incidence assumption. If `α` decreases monotonically with younger cohort → quantifies the non-stationarity cost and supports R2's concern transparently. **Either outcome strengthens the response.**

**Implementation:** Reuse `run_zimphia_analysis()`; pass a `cohort_filter` argument that subsets the analysis tibble on a derived `birth_cohort` column (`survey_year - age`).

**Writeup:** One paragraph in §6 (Discussion), with one supplementary forest plot comparing the three posteriors.

---

### Task D — Implied incidence derivation (revision_plan §3.6)  *— ~10 min, post-processing only*

Post-processing of existing posterior draws from the main ZIMPHIA HMC fit. From [`R/zimphia_analysis.R`](../R/zimphia_analysis.R), the saved `cmdstanr` draws contain `alpha`, `beta`, `gamma`. The log-logistic hazard at age `a` for covariates `x` is (already Eq. 7 in the manuscript at line 305):

```
h(a | x) = (γ / α_x) · (a/α_x)^(γ-1) / (1 + (a/α_x)^γ)
α_x = α · exp(xᵀβ)
```

**Implementation:** Write a new function `compute_age_specific_hazard()` in [`R/viz_survival.R`](../R/viz_survival.R):
1. Read posterior draws.
2. Evaluate `h(a)` on a grid `a ∈ {15, 15.5, …, 60}` for each draw, separately for males/females.
3. Summarise as posterior mean + 95% pointwise CrI.
4. Plot two curves (one panel per sex) → `fig8_incidence_curve.png`.

For the population-level number:
```
incidence_pop = Σᵢ wᵢ · h(aᵢ | xᵢ) / Σᵢ wᵢ
```
evaluated on the ZIMPHIA analysis tibble (per-posterior-draw). Report posterior median + 95% CrI as "X.Y per 100 person-years".

**Writeup:** New §4.5 "Implied incidence" subsection in the manuscript, with one figure and one paragraph interpreting both the age-curve and the population number.

---

### Task E — Design-based variance via ZIMPHIA replicate weights (revision_plan §3.3)  *— ~2.4 hours compute*

**Major scope change vs original plan:** ZIMPHIA's intermediate weights file already provides 175 pre-computed jackknife/BRR replicate weights. **No Rao-Wu bootstrap needs to be coded from scratch.** Just refit per replicate weight column.

**Implementation:**

1. Load [`ZIMPHIA/ZIMPHIA 2020 Intermediary Weights (CSV)/zimphia2020indintermediarywts.csv`](../ZIMPHIA/ZIMPHIA%202020%20Intermediary%20Weights%20%28CSV%29/zimphia2020indintermediarywts.csv). Join the 175 columns `design_wt001…design_wt175` onto the analysis tibble by `personid`. Also keep `varstrat` and `varunit` columns for a one-line description of the design.

2. Pick R = 100 of the 175 replicate weights (use the first 100 — they're exchangeable by construction).

3. For each `r ∈ {1, …, 100}`: substitute `weight ← design_wt00r` and refit with HMC (single chain, 2,000 warmup + 5,000 sampling — same as primary fit). Per fit ≈ 1.43 min → total ≈ 2.4 hours. **Skip MH entirely** (this is a design-correction analysis, not a sampler comparison; rerunning MH would take ~50 hours and produces no new information).

4. Collect posterior medians and 95% CrIs per replicate. The design-based CrI for `β` is the percentile interval of posterior medians across replicates.

5. Compare to the model-based CrI from the original fit. Quantify the inflation factor: `width(design-based) / width(model-based)`. If small (< 1.05), report as confirmation that ignoring clustering/stratification was approximately benign. If large (> 1.2), report the design-based interval as primary and discuss.

**Asymmetry payoff:** *"This analysis required 100 HMC refits in approximately 2.4 hours. The corresponding MH-only analysis would have required approximately 50 hours, which was not feasible within the revision timeline."* — this is the second leg of the asymmetry-as-feature argument.

**Writeup:**
- Expand §3 (ZIMPHIA description) to ~3 paragraphs (two-stage stratified design, PSU/strata, weighting), drawing on the PHIA "Sampling and Weighting Technical Report" PDF already in the project.
- One paragraph in §4 reporting the design-based CrI and the inflation factor.
- One sentence in Limitations: residual design features not accommodated.

---

### Task F — Misspecification simulation, central cell (revision_plan §3.1)  *— ~25 hours compute (overnight + weekend)*

**Scope (confirmed):** one cell at `n = 2,000, censoring = 0.3, weight CV ≈ 0.5`, 200 HMC reps + 200 MH reps. Weibull DGM.

**Weibull truth calibration:** Match the median and an IQR-like spread of the log-logistic truth (`α = 5, γ = 1.5`). Log-logistic median = 5; IQR via quantile function ≈ [3.15, 7.94]. Pick Weibull `(shape k, scale λ)` such that:
- `λ · (log 2)^(1/k) = 5` (median)
- IQR matches (an additional one-parameter match)

A workable choice: `k = 2.0`, `λ = 5 / (log 2)^(1/2) ≈ 6.01`. This gives a strictly-increasing Weibull hazard (vs log-logistic's unimodal hazard), so it's a substantive misspecification — exactly what R1 wants.

**Implementation:**

1. Add a new DGM function in [`R/simulation.R`](../R/simulation.R) — `simulate_survival_data_weibull(n, params, weight_type)` — mirroring the existing `simulate_survival_data` (line 193) but with the inverse-CDF sample swapped:
   ```r
   Ti <- params$lambda_weibull * (-log(U))^(1 / params$k_weibull)
   ```
   Wire `k_weibull` and `lambda_weibull` into `get_default_params()` with `k_weibull = 2.0, lambda_weibull = 6.01`.

2. Add a small wrapper `run_misspec_simulation()` that calls the Weibull DGM at the central cell and feeds the data through the **unchanged log-logistic** fits (HMC + MH). The point of misspecification is that the *fit* is log-logistic; only the truth changes.

3. Reuse the existing analysis pipeline ([`R/analysis_pipeline.R`](../R/analysis_pipeline.R)) for bias / RMSE / coverage / ESS-per-second summaries.

**Compute budget on laptop (8 cores assumed):**
- HMC: 200 reps × ~30s per fit (n=2,000, single cell) parallel-8 ≈ 1.5 hours
- MH: 200 reps × ~7 min per fit (n=2,000) parallel-8 ≈ 25 hours → overnight + weekend

Kick off MH on day 1 evening; HMC fits opportunistically in spare cycles or on day 2 morning.

**Writeup:**
- New §3.x (Methods) misspecification scenario paragraph: DGM choice, parameter calibration, scope, why one cell.
- New §4.x (Results) subsection: bias / RMSE / coverage / ESS-per-second under correct vs misspecified DGM. Likely finding: the **HMC/MH ordering is preserved** (geometry-driven, not DGM-driven). Frame this as evidence that the sampler comparison is robust.
- One paragraph in §5 (Discussion) tying it to R1's concern.
- One sentence in Limitations: "Misspecification was examined at one representative cell; broader misspecification regimes are left to future work."

---

### Task G — HMC 1,000-rep rerun at headline cells (revision_plan row 10)  *— ~28 hours compute (one weekend)*

**Scope (confirmed):** two cells only:
- Cell 1: `n=2,000, censoring=0.3, CV≈0.5` — ~3 hours on laptop
- Cell 2: `n=10,000, censoring=0.3, CV≈0.5` — ~25 hours on laptop

Don't rerun MH. Keep MH at 200 reps everywhere. The asymmetry is the headline finding.

**Implementation:**

1. Add an `n_replicates_hmc` argument to `run_simulations()` (line 262 of [`R/simulation.R`](../R/simulation.R)), separate from the existing replicate count. Default to existing 200; allow override to 1,000.

2. Run `run_simulations(n_obs_vec = c(2000), censoring_props = c(0.3), weight_types = c("high"), n_replicates_hmc = 1000)` and similarly for `n=10000`. The MH reruns are skipped by a new `samplers = c("hmc")` argument; the existing MH results from the original simulation remain authoritative.

3. Recompute summary tables (`tab:sim-rmse-beta`, etc.) for the two affected cells with the larger HMC rep count. Mark the affected cells in the manuscript with a footnote.

**Writeup:**
- Tables: in the central cells, HMC summaries now report MC SE with √5× tighter precision; MH summaries unchanged.
- Table captions: replace "200 replicates" with "200 replicates (MH); 1,000 replicates (HMC, central cells only)".
- New paragraph in §4 results explaining the asymmetric design and citing R2.
- Sentence in §5/§6 framing the asymmetry as illustrative of the ESS/s comparator (Task H below).

---

### Task H — ESS/s reframing (revision_plan §3.7)  *— ~1 hour writing, no compute*

The strategic frame in §1 of this plan goes into three places:

1. **Abstract.** Replace the "1.43 min vs 31.83 min" headline with the ESS/s framing.
2. **End of §1 (Background).** Add the "anticipate the one-fit objection" paragraph: ESS/s is the operative comparator, supports repeated fits (bootstrap, sensitivity panels), scales to larger PHIA cohorts.
3. **§5/§6 (Discussion).** New paragraph titled *"Why ESS/s, not wall time"*, anchored to (i) Task E's 100-refit bootstrap, (ii) Task G's asymmetric reps, (iii) the original 72× ZIMPHIA gain.

These edits are written *after* Tasks E and G land, so the new paragraph can quote concrete numbers from this revision.

---

### Task I — Constant-incidence Discussion paragraph (revision_plan §3.5)  *— 30 min writing*

A dedicated paragraph in §6 (Discussion). Three sentences:

1. The model recovers an *average hazard over the retrospective interval*, not a point-in-time incidence rate.
2. Under monotonically declining true incidence (as has held in Zimbabwe), the estimate is biased high relative to current incidence and low relative to historic peak.
3. The headline interpretation is therefore cumulative risk over the window, not current incidence; Task C (birth-cohort sensitivity) quantifies the cost of this assumption empirically.

Also: reframe §5.3 (line 1077 of the manuscript) — change "marginal sex effect" to acknowledge that β is the log time-ratio averaged over the cohort's retrospective window.

---

### Task J — "200 replicates" wording sweep  *— 15 min, after Task G lands*

Per the cosmetic-round changelog, this was deferred to follow Task G. Once Task G adds the asymmetric rep count for the central cells, sweep every "200 replicates per scenario" caption in the manuscript and replace with the appropriate qualifier ("200 replicates (MH); 1,000 replicates (HMC, central cells only)") where it applies.

Locations to revisit (from the earlier exploration of `01_Manuscript/manuscript_sn.tex`):
- Lines 436, 485, 609, 627, 665, 671, 676, 716, 722, 753, 790, 842, 853.

---

### Task K — Response letter draft  *— 2 hours writing*

Produce `../../10_BMC_Submission/04_Cover_Letter/response_letter.md` with comment-by-comment rebuttals.

**Structure:**

1. **Opening paragraph.** A four-bullet summary of the most consequential revisions: misspecification scenario (Task F), design-based variance via ZIMPHIA replicate weights (Task E), incidence derivation (Task D), ZIMPHIA design expansion + birth-cohort sensitivity (Tasks C, E, I). This anchors the editor's read.

2. **Per-comment rebuttals.** Use the §2 table of [`../../10_BMC_Submission/revision_plan.md`](../../10_BMC_Submission/revision_plan.md) as the index. For each row, write:
   - *Reviewer comment* — quote verbatim.
   - *Response* — 1–3 sentences stating the change.
   - *Manuscript pointer* — section + (post-revision) line number.

3. **Pushback wording (strategic):**
   - **R2 "one-fit"**: use the asymmetry-as-feature passage in §1 of this plan, *verbatim if it survives review*, otherwise lightly edited.
   - **R1 "basic MH"**: lead with what Task A reveals (slice samplers, not RWM), state the bounded scope of the comparison, defer adaptive Metropolis variants to future work.

4. **Tone:** "We thank the reviewer for this observation" — not "the reviewer raises a valid point" (backhanded). For pushback, lead with what was *added*, then state bounded scope.

---

## 4. Sequencing (calendar — laptop-only, 4 working days)

| Day | Morning | Afternoon | Overnight (unattended) |
|---|---|---|---|
| **Day 1** | Task A (JAGS audit, 30 min); Task B Stan-model generalisation + multivariable fit (1.5h) | Task C birth-cohort fits (10 min) + writeup (1h); Task D incidence-curve postprocessing (1h) | Kick off Task F MH 200-rep misspec sim (~25h running through Day 2 and into Day 3) |
| **Day 2** | Task E design-based variance: load replicate weights, fit loop, ~2.4h | Task F HMC misspec sim (~1.5h); write Methods §3 ZIMPHIA-design expansion (2h) | Kick off Task G n=2,000 HMC 1,000-rep cell (~3h overnight) |
| **Day 3** | Task G n=10,000 HMC 1,000-rep cell kickoff (kick off morning; ~25h, finishes Day 4 morning) | Write §4 Results: multivariable, birth-cohort, incidence, design-based variance (3h); Task A → Algorithm 4 rewrite if needed (1h) | Task G n=10,000 continues |
| **Day 4** | Task G results in; Task J wording sweep; assemble Tasks H (ESS/s reframing) + I (constant-incidence paragraph) (3h) | Task K response letter (2h); verification + LaTeX rebuild (1h) | — |

**Risk on this calendar:** Task F's MH leg (25h MH parallel-8) and Task G's `n=10,000` leg (25h HMC parallel-8) cannot both occupy the same overnight slots. Sequenced as above: Task F runs Day-1-evening → Day-3-morning, Task G `n=10,000` runs Day-3-morning → Day-4-morning. If the laptop is needed for active work on Day 2, the simulations slow proportionally. If timeline slips beyond Day 4, the user has authorised dropping Task G's `n=10,000` cell as the cheapest sacrifice (it still leaves the asymmetric-reps story intact at `n=2,000`).

---

## 5. Manuscript edits required (summary)

After Tasks A–K land, the manuscript will need the following structural updates. These are *not* line-by-line edits in this plan — they will be planned in a third pass once results are in hand. This list exists so nothing is forgotten.

### New / rewritten sections
- **Abstract** — reframe around ESS/s and the design-based variance result (Task H).
- **§1 (Background)** — append the "anticipate the one-fit objection" paragraph (Task H).
- **§3 (Methods → ZIMPHIA description)** — expand from one paragraph to three (Task E).
- **§3.x (Methods)** — misspecification scenario description (Task F).
- **§3.2 / Appendix Algorithm 4** — rewrite to match actual JAGS sampler (Task A).
- **§4.x (Results)** — new subsections: multivariable sensitivity (Task B), birth-cohort (Task C), implied incidence (Task D), design-based variance (Task E), misspecification (Task F), HMC 1,000-rep central-cell results (Task G).
- **§5 / §6 (Discussion)** — "Why ESS/s, not wall time" (Task H); constant-incidence interpretation (Task I); broader limitations (design-based, MH variants, stationarity, single-PHIA scope).

### Edits to existing sections
- §5.3 (line 1077 of pre-cosmetic-round manuscript) — reframe "marginal sex effect" wording.
- All sim-table captions — Task J ("200 replicates" wording sweep).

### Limitations paragraph additions
- Strata/clusters not in the likelihood; design-based bootstrap was used as the principled correction (Task E).
- Misspecification examined at one representative cell (Task F).
- MH baseline is JAGS slice samplers; more aggressively tuned variants (adaptive Metropolis, DRAM, parallel tempering) left to future work (Task A).
- Constant-incidence assumption: estimate is an *average over the retrospective interval*; birth-cohort sensitivity quantifies impact (Tasks C, I).
- Single PHIA dataset (ZIMPHIA 2020); generalisation to MPHIA, SHIMS, etc. is future work.

---

## 6. Verification

End-to-end checks before submission:

1. **Code unit tests.** Existing test suite under `tests/testthat/` (if present) must pass after the Stan/JAGS-model and `simulation.R` changes. Run `devtools::test()`.
2. **LaTeX rebuild.** `pdflatex → bibtex → pdflatex → pdflatex` clean, zero new errors, zero new undefined references. Compare warning count to the post-cosmetic baseline (3 warnings, all pre-existing layout).
3. **Cross-reference sweep.** `grep` for `\ref{eq:aft-density}`, `\ref{tab:zimphia-performance}`, `\ref{tab:sim-rmse-beta}` and the new figures/tables — all should resolve.
4. **Numeric round-trip.** The central-cell HMC 1,000-rep rerun (Task G) must reproduce — within MC SE — the original `n=2,000, cens=0.3, CV=0.5` cell at 200 reps. If not, investigate before claiming the larger run is the headline.
5. **Response-letter coverage check.** Every row in [`../../10_BMC_Submission/revision_plan.md`](../../10_BMC_Submission/revision_plan.md) §2 must appear in `response_letter.md` with a manuscript pointer. Walk the table top-to-bottom.
6. **Internal consistency.** After all renumbering, equation `\ref{}`s, table `\ref{}`s, and section `\ref{}`s all resolve to the right targets. Run `pdflatex` with `-interaction=nonstopmode` and confirm no `?` left in the PDF for unresolved references.

---

## 7. Risk register

| Risk | Likelihood | Mitigation |
|---|---|---|
| Task A reveals JAGS used *neither* slice nor RWM (e.g., conjugate sampler) | Low | Document whatever it did use; the manuscript needs to match reality regardless |
| Misspecification (Task F) reveals HMC advantage *shrinks* under Weibull DGM | Low | Report transparently; the ordering will still hold, and that's the headline |
| Design-based CrI (Task E) is *much* wider than model-based (> 1.5×) | Medium | Report design-based as primary; rewrite §4 around the design-corrected number. This is honest and strengthens R1's response |
| Task G `n=10,000` doesn't complete in time | Medium-High | Drop to `n=2,000`-only for the asymmetric-reps story; still strong |
| Laptop crash / power loss mid-sim | Low | Save intermediate results every 25 reps; the simulation harness already writes per-rep `.rds` files |
| Reviewer 3 added on resubmission | Possible | This plan's four-task substantive backbone covers most plausible additional concerns |
| Multivariable fit (Task B) shifts `β_sex` materially | Medium | Report transparently; sampler-comparison conclusions are unaffected. Note in Discussion |

---

## 8. Open questions for execution time

These are *not* blocking for the plan; flag them when the execution pass starts:

1. **Birth-cohort cutpoints (Task C):** confirm three cohorts is the right granularity, or whether five smaller cohorts would expose more structure.
2. **Multivariable covariate choice (Task B):** `age_band` may be collinear with the outcome scale (we're modelling time-to-event indexed in age). Consider `sex + urban_rural + wealth_quintile` as a safer alternative.
3. **Design-based variance: 100 or 175 replicates?** ZIMPHIA ships 175; the plan calls for 100 to control time. If the loop is faster than estimated (~1.43 min/fit may be conservative for laptops with 8 cores), use all 175.
4. **Response-letter destination:** is `../../10_BMC_Submission/04_Cover_Letter/response_letter.md` the right path, or does BMC expect a specific filename / format (cover letter vs response-to-reviewers)?

---

## 9. Out of scope (deliberately deferred)

- Re-running the full 5,400-dataset grid at 1,000 reps. Not feasible on laptop; not strategically necessary.
- Adaptive Metropolis / DRAM / parallel tempering variants of MH. Acknowledged as future work in Limitations.
- Generalisation to MPHIA / SHIMS / other PHIA datasets. Future work.
- Bayesian model averaging across log-logistic vs Weibull vs log-normal baselines. Out of scope for a sampler-comparison paper.

---

*Plan prepared 2026-05-11. Compute estimates assume an 8-core laptop running unattended overnight, no thermal throttling. All four user-confirmed decisions captured in §0. Mirror copy of [`../../10_BMC_Submission/revision_plan_substantive.md`](../../10_BMC_Submission/revision_plan_substantive.md) with paths rebased to the code-project root.*
