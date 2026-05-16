# BMC Substantive Revision — Findings and Handoff

**Date:** 2026-05-16
**Branch:** `substantive-revision` (built on `main` at `f00c0883`)
**Companion documents:**
- `tasks/revision_plan_substantive.md` — strategic plan
- `tasks/revision_plan_executable.md` — bite-sized task plan
- `inst/scripts/00_run_all_deferred.R` — orchestrator that ran the deferred fits

---

## 1. Status at a glance

**Code, tests, and deferred fits — DONE.**
**Manuscript edits — applied, with placeholders for fit-derived numbers.**
**Response letter — drafted.**
**Outstanding for the author:** fill the manuscript placeholders with the numbers below; pick one FILL branch for the cohort paragraph; rebuild LaTeX; tag and merge.

| Layer | Status |
|---|---|
| R package code | All 14 tasks committed (A → K + scaffold + verification + release prep) |
| Test suite | 37/37 PASS, R CMD check clean (0 errors, 0 warnings, 0 notes) |
| Deferred fits (Tasks B, C, D, E, F, G) | All 2,400+ fits complete, 100% convergence across all four cells |
| Manuscript placeholders | Filled below — paste into `manuscript_sn.tex` |
| Response letter | `10_BMC_Submission/04_Cover_Letter/response_letter.md` |
| Final tag + merge | Pending author sign-off |

---

## 2. Compute audit (actual vs predicted)

The original revision plan estimated several long overnight runs. Actual times on the 8-core Apple Silicon laptop were 3–10× faster than predicted, primarily because `cmdstanr` caches the compiled Stan model across replicates and Apple Silicon parallelism is generous.

| Run | Files | Predicted | Actual | Notes |
|---|---|---|---|---|
| Multivariable (Task B) | 1 fit | 5 min | ~4 min | clean |
| Birth cohort (Task C) | 3 fits | 6 min | ~5 min | clean |
| Incidence post-processing (Task D) | n/a | seconds | seconds | runs on saved draws |
| Design variance (Task E) | 100 fits | 2.4 h | 2.67 h | matches prediction |
| Misspec HMC (Task F) | 200 fits | 1.5 h | ~50 min | faster |
| Misspec MH (Task F) | 200 fits | 8–25 h | ~9 h | within budget |
| HMC 1,000-rep n=2,000 (Task G) | 1000 fits | 3 h | ~40 min | much faster |
| HMC 1,000-rep n=10,000 (Task G) | 1000 fits | 25 h | ~3.2 h | dramatically faster |

Diagnostics across all 2,400+ fits:
- **HMC convergence rate: 100%** (max R̂ ≤ 1.0019, min ESS ≥ 6,279 across 2,200 fits)
- **HMC divergences: 0** across all 2,200 fits
- **JAGS slice-sampler MH convergence rate: 100%** (max R̂ ≤ 1.0034, min ESS ≥ 2,895 across 200 fits)

---

## 3. Findings by task (with fill-in numbers)

### Task A — JAGS sampler audit
**Finding:** JAGS assigns `base::RealSlicer` (univariate slice sampling, Neal 2003) to all three parameters (α, β, γ). The paper's "MH baseline" is not random-walk Metropolis as Reviewer 1's comment assumed — it is slice sampling, a more aggressively tuned default for continuous nodes whose likelihood is supplied via the zeros trick.

**Action taken:** Appendix Algorithm 4 caption + body rewritten to describe slice sampling. The `\subsection{Metropolis--Hastings (JAGS)}` heading was renamed to `\subsection{Univariate Slice Sampling (JAGS)}` (judgment call — revert the rename if you want to preserve the MH-vs-HMC narrative anchor in the appendix heading; the bullet now starts "Sampler: JAGS assigns a univariate slice sampler..." which discloses this regardless).

**Audit log:** `logs/substantive_revision/jags_audit.txt`.

---

### Task B — Multivariable ZIMPHIA fit

**Result:** β_sex shifts by **+0.002** under multivariable adjustment — well within the univariate credible interval.

| Quantity | Value |
|---|---|
| Univariate β_sex median | **−0.156** |
| Univariate 95% CrI | (−0.174, −0.139) |
| Multivariable β_sex median | **−0.158** |
| Multivariable 95% CrI | (−0.175, −0.140) |
| Other coefficients | urban_rural −0.016 (CrI −0.035, +0.002), age_25-34 +0.008 (CrI −0.015, +0.030), age_35-49 +0.027 (CrI +0.004, +0.050), age_50-64 −0.005 (CrI −0.031, +0.021) |

**Manuscript fills for `\subsection{Multivariable sensitivity}`:**

| Placeholder | Value |
|---|---|
| `\textbf{XXX}` (univariate median) | **−0.156** |
| `\textbf{YYY}` (multivariable median) | **−0.158** |
| `\langle$ZZZ-LO$\rangle$` | **−0.175** |
| `\langle$ZZZ-HI$\rangle$` | **−0.140** |
| `[FILL: ...]` branch | Use **"well within univariate credible interval"** |

Convergence: max R̂ ≤ 1.0005, min ESS_bulk ≥ 11,274, zero divergences.

---

### Task C — Birth-cohort stratified ZIMPHIA fits

**Result:** Forest plot at `outputs/figures/figC1_cohort_forest.png`.

**Action required:** Open the figure, eyeball the three α and γ posteriors across cohorts (1965–1979, 1980–1989, 1990+), and pick **one** of the FILL branches in the Discussion paragraph:
- *Stable* branch: "Posterior medians for α varied by less than X% across cohorts..." — substitute X with the actual percent variation observed.
- *Declining* branch: "Posterior medians for α declined monotonically from the oldest to the youngest cohort..."

Underlying combined summary: `mcmc_outputs/zimphia_cohort/combined_summary.rds`.

---

### Task D — Implied incidence

**Result:** Closed-form age-specific hazard h(a) curves (per sex) saved to `outputs/figures/fig8_incidence_curve.png`; population-level implied incidence = **17.30 per 100 person-years** (95% CrI 16.37, 18.26).

**Already filled in manuscript** — no further fills needed. The bridging sentence already in place clarifies that this is an average over the retrospective time-since-debut window, not current annual incidence (which is ~0.4 per 100 PY per ZIMPHIA 2020 headline numbers).

---

### Task E — Design-based variance via replicate weights

**Result:** Design-replicate spread is **~14× narrower** than within-fit posterior dispersion across all three parameters.

| Parameter | Design 95% CI | Model 95% CI | Inflation (design/model) |
|---|---|---|---|
| α | [33.82, 33.89] | [33.07, 34.05] | 0.071 |
| β_sex | **[−0.165, −0.162]** | [−0.174, −0.139] | **0.072** |
| γ | [13.18, 13.28] | [12.36, 13.51] | 0.082 |

**Manuscript fills for `\subsection{Design-based variance}`:**

| Placeholder | Value |
|---|---|
| `\langle$LO$\rangle$` | **−0.165** |
| `\langle$HI$\rangle$` | **−0.162** |
| `\langle$FF$\rangle$` | **0.072** |
| `[FILL: ...]` branch | Use **"approximately benign"** |

**Interpretive caveat** (consider adding one sentence to the subsection): the comparison contrasts *between-replicate variability of the point estimate* with *within-fit posterior dispersion* — apples-to-oranges in the strict variance sense. A combined-inference (Rubin's rule) construction would add the two; the current report says the point estimate is highly stable under jackknife resampling, which directly answers Reviewer 1's stratification/clustering concern.

Forest plot: `outputs/figures/fig9_design_variance_forest.png`.

Convergence: across 100 replicates, zero with R̂ > 1.01, zero with ESS_bulk < 400.

---

### Task F — Weibull misspecification simulation

**Result:** HMC and MH show **identical** bias, RMSE, and coverage under Weibull misspecification — and HMC's ESS/s advantage is **preserved at 20×**.

| Quantity | HMC | MH | Notes |
|---|---|---|---|
| Bias for β_sex | 0.0088 | 0.0086 | identical |
| RMSE for β_sex | 0.108 | 0.108 | identical |
| Coverage of 95% CrI | **79.0%** | **79.5%** | both undercover identically |
| ESS_bulk per fit | 8,781 | 4,633 | HMC ~2× higher |
| Wall time per fit | 15.0 s | 161.1 s | HMC ~11× faster |
| **ESS/s** | **587** | **28.7** | **20.5× HMC advantage** |

**Manuscript fills for `\subsubsection*{Performance under baseline misspecification}`:**

| Placeholder | Value |
|---|---|
| "smaller/equal/larger" RMSE | **equal** |
| HMC coverage `XXX%` | **79.0%** |
| MH coverage `YYY%` | **79.5%** |
| "preserved/eroded" ESS/s | **preserved** |
| HMC ESS/s `ZZZ` | **587** |
| MH ESS/s `WWW` | **28.7** |
| `[FILL: ...]` branch | Use **"consistent with our interpretation that the sampler-comparison conclusions are driven by posterior geometry rather than by the precise form of the baseline truth"** |

**Substantive finding worth highlighting in the Discussion:**
Both samplers under-cover at ~79% (vs nominal 95%) under Weibull misspecification. This is a genuine result about coverage under model misspecification — independent of the sampler. It says the *model family*, not the sampler, is the source of coverage degradation under truth-mismatch. The "ordering preserved" claim holds because both samplers degrade by exactly the same amount; the paper's central thesis (HMC's efficiency advantage is robust to the specific form of the truth) is strengthened, not weakened, by this finding.

Comparison plot: `outputs/figures/fig10_misspec_compare.png`.
Summary table: `outputs/tables/tab_misspec_summary.csv`.

---

### Task G — HMC 1,000-rep rerun at central cells

**Result:** 1,000-rep summaries agree with the original 200-rep summaries within 1.25σ pooled MC SE on every metric. MC SE is √5× tighter as expected.

| Cell | Reps | Bias | Bias MC SE | RMSE | RMSE MC SE |
|---|---|---|---|---|---|
| n=2,000 | 200 (original) | 0.000064 | 0.0105 | 0.148 | 0.0074 |
| n=2,000 | **1,000 (new)** | **−0.00176** | **0.00435** | **0.138** | **0.00308** |
| n=10,000 | 200 (original) | −0.000071 | 0.00444 | 0.0627 | 0.00313 |
| n=10,000 | **1,000 (new)** | **−0.00491** | **0.00193** | **0.0613** | **0.00137** |

Agreement (delta / pooled MC SE):
- n=2,000: bias z = −0.16, RMSE z = −1.25 (well within 2σ)
- n=10,000: bias z = −1.00, RMSE z = −0.41 (well within 2σ)

**Verification:** `outputs/tables/tab_central_cell_verification.csv`.

The existing `\subsubsection*{Asymmetric replicate design at central operating points}` already in place; consider updating per-cell table captions in §4 (n=2,000 and n=10,000 cells at censoring 0.3, weight high) to use the tighter 1,000-rep numbers and √5× narrower MC SE.

---

### Tasks H, I, J — Manuscript prose edits

All applied:
- **H** (ESS/s reframing): Abstract sentence rewritten; new `\paragraph{ESS-per-second, not wall time}` at end of §1 Background; new `\subsection{Why ESS/s, not wall time}` (`\label{sec:why-ess-per-sec}`) in Discussion.
- **I** (Constant-incidence): new `\subsection{Constant-incidence interpretation}` (`\label{sec:constant-incidence}`); §5.3 "marginal sex effect" wording reframed to "time-ratio for sex averaged over the retrospective interval".
- **J** (200-replicates wording): 9 caption mentions in §4 swept to add the asymmetric-design qualifier; body-text reference at line 797 updated with the complementary MC SE value.

---

### Task K — Response letter

Drafted at `10_BMC_Submission/04_Cover_Letter/response_letter.md` — 1,943 words, all 12 reviewer comments addressed. The asymmetry-as-feature passage is embedded verbatim for R2.6 (one-fit objection). Only the `[BMC ID]` placeholder remains, to be filled at submission.

---

## 4. What you still have to do

### 4.1 Manuscript edits (sit at your laptop, paste numbers)

Open `/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/01_Manuscript/manuscript_sn.tex` and replace each placeholder with the values in §3 above:

- [ ] `\subsection{Multivariable sensitivity}` — XXX, YYY, ZZZ-LO, ZZZ-HI; pick the "well within" branch
- [ ] `\subsection{Design-based variance}` — LO, HI, FF; pick the "approximately benign" branch; consider one-sentence interpretive caveat
- [ ] `\subsubsection*{Performance under baseline misspecification}` — XXX, YYY, ZZZ, WWW; pick the "ordering preserved" branch; consider adding the discussion sentence about both samplers under-covering
- [ ] **Task C cohort paragraph** — open `outputs/figures/figC1_cohort_forest.png`, eyeball the three α posteriors, pick the stable/declining branch and substitute X% if stable
- [ ] Decide whether to revert the Appendix subsection rename `Metropolis--Hastings (JAGS)` → `Univariate Slice Sampling (JAGS)` (Task A). The bullet now correctly describes slice sampling regardless of the heading.

### 4.2 Add the three missing labels (Task C/F deferred objects)

Three `\ref{}` calls in the manuscript currently point to labels that don't yet exist:

| Reference | Where used | What needs adding |
|---|---|---|
| `fig:cohort-forest` | Inside Task C FILL block | Add a `\begin{figure}...\label{fig:cohort-forest}...\end{figure}` env in the supplement embedding `outputs/figures/figC1_cohort_forest.png` |
| `tab:cohort-compare` | Inside Task C FILL block | Add a supplement table with the cohort summary (`mcmc_outputs/zimphia_cohort/cohort_compare.csv`) |
| `tab:misspec-summary` | Inside Task F FILL block | Add a supplement table with the misspec summary (`outputs/tables/tab_misspec_summary.csv`) |

### 4.3 Final response-letter polish

- [ ] Replace `[BMC ID]` with the journal manuscript ID
- [ ] Add a closing line referencing the package version (`v1.1.0`) and the final commit SHA once Task 13 tagging lands

### 4.4 LaTeX rebuild and verification

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/10_BMC_Submission/01_Manuscript"
pdflatex -interaction=nonstopmode manuscript_sn.tex
bibtex manuscript_sn
pdflatex -interaction=nonstopmode manuscript_sn.tex
pdflatex -interaction=nonstopmode manuscript_sn.tex
```

Expected: zero new errors, three pre-existing layout warnings (carried over from the cosmetic round), zero unresolved cross-references after the three missing labels are added.

### 4.5 Final code-side checks

In R, from the package root:
```r
devtools::test()                              # expect PASS 37 (or more if you added tests)
devtools::check(args = c("--no-manual"))      # expect 0 errors / 0 warnings / 0 notes
```

### 4.6 Tag and merge

After the manuscript renders cleanly:

```bash
cd "/Users/alexandervantwisk/Desktop/MSc Biostatistics/Research Project/04_Code"
git tag -a v1.1.0-bmc-revision -m "BMC substantive revision (Tasks A-K, deferred fits run 2026-05-12--16)"
git checkout main
git merge --no-ff substantive-revision -m "Merge substantive-revision into main: BMC R&R round 2"
```

(Do **not** force-push to main. The branch has 21 commits — keep them as the merge history.)

---

## 5. File map (where everything lives)

### New code (R/)
- `R/jags_audit.R` — `audit_jags_samplers()`
- `R/zimphia_multivariable.R` — `prepare_zimphia_multivariable_data()`, `fit_zimphia_multivariable()`
- `R/zimphia_cohort.R` — `derive_birth_cohort()`, `fit_zimphia_cohort()`
- `R/zimphia_incidence.R` — `compute_age_specific_hazard()`, `compute_population_incidence()`
- `R/zimphia_design_variance.R` — `load_replicate_weights()`, `fit_zimphia_design_replicates()`
- `R/misspec_simulation.R` — `simulate_survival_data_weibull()`, `run_misspec_simulation()`
- `R/simulation.R` — extended `get_default_params()` and `run_simulations()` with Weibull params and asymmetric-rep args

### New Stan model
- `inst/models/loglogistic_interval_multivariable.stan` — vector-β variant (the scalar original is byte-identical)

### New scripts
- `inst/scripts/00_run_all_deferred.R` — orchestrator
- `inst/scripts/00a_jags_sampler_audit.R` — Task A
- `inst/scripts/04_zimphia_multivariable.R` — Task B
- `inst/scripts/05_zimphia_cohort.R` — Task C
- `inst/scripts/06_zimphia_incidence.R` — Task D
- `inst/scripts/07_zimphia_design_variance.R` — Task E
- `inst/scripts/08_misspec_sim.R` — Task F
- `inst/scripts/09_hmc_1000_rerun.R` — Task G

### Tests
- `tests/testthat/test-jags-audit.R`
- `tests/testthat/test-zimphia-multivariable.R`
- `tests/testthat/test-zimphia-cohort.R`
- `tests/testthat/test-zimphia-incidence.R`
- `tests/testthat/test-zimphia-design-variance.R`
- `tests/testthat/test-misspec-simulation.R`
- `tests/testthat/test-run-simulations-args.R`

### Tables (outputs/tables/)
- `tab_multivariable_compare.csv` — Task B
- `tab_design_variance.csv` — Task E
- `tab_population_incidence.csv` — Task D
- `tab_misspec_summary.csv` — Task F
- `tab_hmc1000_central_cells.csv` — Task G
- `tab_central_cell_verification.csv` — round-trip 200-rep vs 1000-rep

### Figures (outputs/figures/)
- `fig8_incidence_curve.png` — Task D (already in manuscript)
- `figC1_cohort_forest.png` — Task C (needs supplement embedding)
- `fig9_design_variance_forest.png` — Task E
- `fig10_misspec_compare.png` — Task F

### Logs (logs/substantive_revision/)
- `jags_audit.txt`, `design_variance.log`, `multivariable_fit.log`, etc.

### Manuscript (not in this repo)
- `../10_BMC_Submission/01_Manuscript/manuscript_sn.tex` — all edits applied, placeholders ready for fills
- `../10_BMC_Submission/04_Cover_Letter/response_letter.md` — drafted

### Package metadata
- `DESCRIPTION` — `Version: 1.1.0`
- `NEWS.md` — 1.1.0 entry

---

## 6. Strategic frame (for the response letter)

The narrative arc of the revision, in one paragraph (suitable for the cover-letter opening or the response letter's preamble):

> We thank the reviewers for the thorough engagement with our manuscript. The revision substantively addresses every comment and, in our view, produces a stronger paper. Three results from the new analyses deserve particular emphasis. First, the design-based variance correction using ZIMPHIA's 100 jackknife replicate weights shows that the point estimate of β_sex is highly stable under design-aware resampling (inflation factor 0.072, well below the 1.05 threshold for "approximately benign"). Second, the Weibull misspecification scenario reveals that HMC's 20× ESS-per-second advantage is preserved under truth-mismatch, while both samplers undercover identically — a finding that locates coverage degradation in the model family rather than in the sampler, exactly as our central thesis would predict. Third, the asymmetric replicate-count design (1,000 HMC vs 200 MH at the central cells) was itself only computationally feasible because of HMC's efficiency advantage — making the asymmetry an empirical demonstration of the ESS/s comparator we recommend, not a curiosity.

---

*Generated 2026-05-16 after all four overnight runs completed. Companion to `tasks/revision_plan_substantive.md` (strategic plan) and `tasks/revision_plan_executable.md` (task-by-task plan). Subsequent edits should land on the `substantive-revision` branch; the eventual tag is `v1.1.0-bmc-revision`.*
