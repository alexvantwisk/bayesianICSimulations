# Revision Corrections — Handoff for Claude Code

**Context.** The substantive-revision branch is functionally complete (37/37 tests pass, R CMD check clean, 2,400+ fits at 100% convergence). An independent statistical audit identified four items that need correction before submission. One of them is a true statistical error; the other three are framing/interpretation fixes. Author has approved scope: re-running simulations is acceptable if it produces a materially better artefact, but unnecessary re-runs should be avoided.

Read `tasks/revision_findings_and_handoff.md` first if you need the broader context. This document supersedes the "What you still have to do" section of that file for the items below.

---

## Issue summary

| # | Task | Severity | Fix type | Re-run needed? |
|---|---|---|---|---|
| 1 | E — Design variance | **Blocking** (statistical error) | Recompute Rubin total; rewrite paragraph | Yes, ~2.4 h (re-fit with draws saved) |
| 2 | C — Cohort FILL branch | High (wrong branch chosen) | Edit text, add caveat | No |
| 3 | D — Implied incidence framing | High (overclaim risk) | Rename + optional recompute on HIV− subset | Optional, seconds |
| 4 | A — JAGS subsection rename | Medium (narrative continuity) | Revert rename, sharpen body text | No |
| 5 | F — Weibull calibration caveat | Low (one sentence) | Edit text | No |

Items not on this list (Tasks B, G, H, I, J, K) are correct as currently implemented. Do not touch.

---

## Issue #1 — Task E: Design-based variance (BLOCKING)

### The statistical problem

`R/zimphia_design_variance.R` (lines 142–155) saves per-replicate posterior **median + 2.5%/97.5% quantiles** only, then `inst/scripts/07_zimphia_design_variance.R` reports the **2.5%/97.5% percentile of the 100 replicate medians** as the "design 95% CI."

This is only the *between-replicate* variance component B in a Rubin-style combination. The within-replicate posterior variance W̄ is never computed. Reporting `inflation_factor = design_CI_width / model_CI_width = 0.072` is therefore comparing B^(1/2) to W̄^(1/2) — apples to oranges. A proper design-aware total variance must be ≥ the model variance by construction.

The conclusion ("approximately benign") is almost certainly correct, but the **calculation as currently reported is wrong** and any survey-statistician reviewer will flag it.

### The fix

Re-run the 100 design replicates **saving full posterior draws per replicate** (currently only summaries are saved). Then compute the proper Rubin combination:

```
T = W̄ + (1 + 1/m) · B
```

where for each parameter:

- `θ̂_r` = posterior median (or mean) in replicate r
- `s²_r` = posterior variance in replicate r
- `W̄ = mean(s²_r)` across replicates
- `B = var(θ̂_r)` across replicates (sample variance, denominator m−1)
- `T = W̄ + (1 + 1/m) · B`
- Approximate 95% CI: `mean(θ̂_r) ± 1.96 · √T`

### Concrete code changes

#### 1. Update `R/zimphia_design_variance.R`

Change the `fit$summary()` call (line 150) to also save `mean` and `sd`, and additionally save full draws:

```r
summ <- fit$summary(
  variables = c("alpha", "beta", "gamma"),
  "mean", "median", "sd",
  ~ posterior::quantile2(.x, probs = c(0.025, 0.975)),
  "rhat", "ess_bulk"
) |>
  dplyr::mutate(replicate = i, runtime_secs = rt)

# Save full draws for proper Rubin combination
saveRDS(
  fit$draws(variables = c("alpha", "beta", "gamma"), format = "df"),
  file.path(rep_dir, "draws.rds")
)
```

#### 2. Add new function `R/zimphia_design_variance.R` (append at bottom)

```r
#' Combine per-replicate posteriors via Rubin's rules
#'
#' For each parameter, compute the proper design-aware total variance
#' \deqn{T = \bar{W} + (1 + 1/m) B}
#' where \eqn{\bar{W}} is the mean within-replicate posterior variance
#' and \eqn{B} is the between-replicate variance of posterior means.
#'
#' @param replicate_summaries Tibble combining per-replicate `summary.rds`
#'   files (must include columns `variable`, `mean`, `sd`, `replicate`).
#' @return Tibble with one row per parameter: variable, m, q_bar (combined
#'   point estimate), w_bar, b, t_total, se_total, ci_lower, ci_upper.
#' @export
combine_design_replicates <- function(replicate_summaries) {
  required <- c("variable", "mean", "sd", "replicate")
  missing <- setdiff(required, names(replicate_summaries))
  if (length(missing) > 0L) {
    stop("replicate_summaries missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  replicate_summaries |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(
      m       = dplyr::n(),
      q_bar   = mean(.data$mean),
      w_bar   = mean(.data$sd^2),
      b       = stats::var(.data$mean),
      t_total = .data$w_bar + (1 + 1 / .data$m) * .data$b,
      se_total = sqrt(.data$t_total),
      ci_lower = .data$q_bar - 1.96 * .data$se_total,
      ci_upper = .data$q_bar + 1.96 * .data$se_total,
      .groups = "drop"
    )
}
```

#### 3. Add a test in `tests/testthat/test-zimphia-design-variance.R`

```r
test_that("combine_design_replicates produces T >= W_bar", {
  fake <- tibble::tibble(
    variable  = rep(c("alpha", "beta", "gamma"), each = 10),
    mean      = rnorm(30),
    sd        = runif(30, 0.1, 0.5),
    replicate = rep(seq_len(10), 3)
  )
  out <- combine_design_replicates(fake)
  expect_true(all(out$t_total >= out$w_bar))
  expect_true(all(out$ci_upper > out$ci_lower))
})

test_that("combine_design_replicates errors on missing columns", {
  expect_error(
    combine_design_replicates(tibble::tibble(variable = "alpha", mean = 0)),
    "missing columns"
  )
})
```

#### 4. Rerun the design replicates

Execute `inst/scripts/07_zimphia_design_variance.R`. Budget ~2.4 hours unattended. Confirm 100 replicates complete with zero R̂ > 1.01 (matches previous run).

#### 5. Post-process and write new summary

Add a small wrapper (could go in `inst/scripts/07_zimphia_design_variance.R` at the end, or a new `inst/scripts/07b_design_variance_combine.R`):

```r
library(bayesianICSimulations)
library(dplyr)

reps <- list.files(
  "mcmc_outputs/zimphia_design_replicates",
  pattern = "summary\\.rds$", recursive = TRUE, full.names = TRUE
)
all_summ <- purrr::map_dfr(reps, readRDS)
combined <- combine_design_replicates(all_summ)
print(combined)
readr::write_csv(combined, "outputs/tables/tab_design_variance_rubin.csv")
```

#### 6. Manuscript changes — `10_BMC_Submission/01_Manuscript/manuscript_sn.tex`

Locate `\subsection{Design-based variance}` (around line 1072) and rewrite. **Rename the subsection** and **rewrite the body** as follows (paste-ready):

```latex
\subsection{Sensitivity to design-based resampling}
\label{sec:design-variance}
The primary HMC analysis treats the 16{,}554 weighted observations as
approximately independent, ignoring the within-cluster correlation and
stratification of the ZIMPHIA design. To quantify the impact of this
approximation, we refitted the model 100 times, each time substituting one of
the first 100 jackknife replicate weight columns supplied by ZIMPHIA for the
primary weight. For each parameter we then combined the within-replicate
posterior variance $\bar{W}$ and the between-replicate variance $B$ of the
posterior means via Rubin's rules
\citep{rubin_multiple_1987,beaumont_bayesian_2008}:
$T = \bar{W} + (1 + 1/m)\,B$,
giving a design-aware total variance and an approximate 95\% credible
interval $\bar{Q} \pm 1.96\sqrt{T}$.

For $\beta_{\text{sex}}$, the model-based 95\% credible interval was
$[-0.174,\,-0.139]$ (width $\approx 0.035$). Across the 100 replicates the
between-replicate component $B$ was negligible relative to within-replicate
posterior dispersion $\bar{W}$, yielding a design-aware Rubin total interval
of $\langle$\textbf{LO}$\rangle$, $\langle$\textbf{HI}$\rangle$
(width $\langle$\textbf{W}$\rangle$, inflation factor
$\langle$\textbf{FF}$\rangle$ relative to the model-based interval).
We conclude that the model-based credible interval is approximately
correct under the ZIMPHIA design: the between-replicate variability of
the point estimate is small relative to the within-fit posterior dispersion,
so treating the weighted observations as conditionally independent does
not materially understate uncertainty for $\beta_{\text{sex}}$. The
design-aware interval is therefore reported as a sensitivity finding
rather than a correction.

This analysis required 100 HMC refits in approximately 2.4 hours; the
corresponding MH analysis would have required approximately 50 hours and
was not undertaken within the revision timeline (see
\S\ref{sec:why-ess-per-sec}).
```

Then fill `LO`, `HI`, `W`, `FF` from `outputs/tables/tab_design_variance_rubin.csv`:

- `LO`, `HI` = `ci_lower`, `ci_upper` (3 decimal places)
- `W` = `ci_upper - ci_lower` (3 decimals)
- `FF` = `W / 0.035` (2 decimals)

Expected values based on my hand-calculation: `FF` should land near 1.005–1.05. If you get something dramatically different (e.g., FF > 1.5 or FF < 1.0), stop and investigate — the calculation is wrong somewhere.

#### 7. Response letter changes — `10_BMC_Submission/04_Cover_Letter/response_letter.md`

Search for the paragraph addressing the stratification/clustering comment (around line 60–64). Update:

> Before: "design-corrected credible intervals and a design-effect inflation factor are now reported alongside the original model-based estimates"
>
> After: "we combine within- and between-replicate variance via Rubin's rules to give a design-aware total credible interval; the resulting inflation relative to the model-based interval is approximately benign (<value>%)"

#### 8. Add citation to `sn-bibliography.bib`

If not already present:

```bibtex
@article{beaumont_bayesian_2008,
  title   = {A {Bayesian} approach to using replicate weights for variance estimation},
  author  = {Beaumont, Jean-Fran{\c{c}}ois and Bocci, Cynthia},
  journal = {Survey Methodology},
  volume  = {34},
  number  = {2},
  pages   = {229--236},
  year    = {2008}
}

@book{rubin_multiple_1987,
  title     = {Multiple Imputation for Nonresponse in Surveys},
  author    = {Rubin, Donald B.},
  publisher = {Wiley},
  year      = {1987}
}
```

### Acceptance criteria for Issue #1

- [ ] `combine_design_replicates()` exists with passing tests
- [ ] 100 replicate fits complete with full draws saved (or summaries with `sd` column)
- [ ] `tab_design_variance_rubin.csv` exists with one row per parameter and includes `q_bar`, `w_bar`, `b`, `t_total`, `ci_lower`, `ci_upper`
- [ ] For each parameter, `t_total >= w_bar` (sanity check)
- [ ] Manuscript subsection renamed and body rewritten
- [ ] Response letter paragraph updated
- [ ] Two new bibliography entries

---

## Issue #2 — Task C: Birth-cohort FILL branch (HIGH)

### The problem

The author's previous handoff (`tasks/revision_findings_and_handoff.md`, §3 Task C) instructs to "eyeball the three α posteriors and pick stable vs declining." The cohort_compare.csv shows substantial variation that warrants the **non-stationary** branch, not the stable branch — but with a right-censoring caveat for the youngest cohort.

Numbers from `mcmc_outputs/zimphia_cohort/cohort_compare.csv`:

| Cohort | n | α median | β median | γ median |
|---|---|---|---|---|
| 1965–1979 | 3,511 | 38.16 | −0.039 | 42.47 |
| 1980–1989 | 3,826 | 32.23 | −0.068 | 29.02 |
| 1990+ | 5,940 | 28.85 | −0.126 | 13.86 |

α declines 24% across cohorts; |β| triples; γ falls 3×. This is non-stationary. But the 1990+ cohort is right-truncated (max age at survey = 30), so part of α's decline is mechanical, not epidemiological.

### The fix

In `10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, locate the cohort FILL block (around line 1111, the paragraph that begins "We assessed the constant-incidence assumption..."). Replace the `[FILL: ...]` content with the **declining** branch and add the censoring caveat. Paste-ready:

```latex
We assessed the constant-incidence assumption by stratifying the ZIMPHIA
analysis into three birth cohorts (1965--1979, 1980--1989, 1990+) and
refitting the model within each. Posterior medians for $\alpha$ declined
monotonically from the oldest cohort to the youngest
(38.2, 32.2, 28.9 years), and the magnitude of the sex effect
$|\beta_{\text{sex}}|$ grew correspondingly (0.04, 0.07, 0.13;
Figure~\ref{fig:cohort-forest}, supplement). This is consistent with a
declining HIV incidence trend over the period covered by the retrospective
window, in line with the Zimbabwean epidemic's well-documented secular
decline since the early 2000s. We note that the apparent shortening of
$\alpha$ in the 1990+ cohort is partly mechanical: members of this cohort
were at most 30 years old at the 2020 survey, so the upper tail of the
seroconversion-age distribution is structurally unobservable in that
stratum. The growth in $|\beta_{\text{sex}}|$ across cohorts, by contrast,
is not a censoring artefact. The headline sampler-comparison conclusions
are unaffected by this finding; the cohort-stratified results are
reported as evidence that the constant-incidence assumption operates as
a smoothing approximation over a non-stationary process.
Numerical results appear in Appendix Table~\ref{tab:cohort-compare}.
```

### Acceptance criteria for Issue #2

- [ ] FILL block replaced with the non-stationary branch
- [ ] Right-censoring caveat for α in the 1990+ cohort included
- [ ] Explicit note that β trend is *not* a censoring artefact

---

## Issue #3 — Task D: Implied incidence framing (HIGH)

### The problem

`R/zimphia_incidence.R::compute_population_incidence()` evaluates `h(age_i | sex_i)` at each subject's current age and weight-averages. This includes already-HIV+ subjects (whose hazard at survey age is being computed as if they were still susceptible), which is mathematically defined but epidemiologically loose. The 17.30/100 PY result is then 43× larger than ZIMPHIA's published incidence (~0.4/100 PY).

The current manuscript paragraph (around line 1078, `\subsection{Implied incidence}`) **does** include a bridging sentence distinguishing this from PHIA published incidence — which saves it from being outright wrong — but the subsection title and the term "implied incidence" overclaim what the number represents.

### The fix — pick one of two options

#### Option A (recommended): rename and reframe; no recompute

Change all references from "implied incidence" to "model-implied hazard." Specifically:

In `10_BMC_Submission/01_Manuscript/manuscript_sn.tex`:

1. Subsection title (line ~1076): `\subsection{Model-implied age-specific hazard}` (replace `\subsection{Implied incidence}`)
2. Label (line ~1077): `\label{sec:implied-hazard}` (update any `\ref{sec:implied-incidence}` references accordingly — there's at least one in the Discussion)
3. Subsection body: change "population-level implied incidence of \textbf{17.30} per 100 person-years" to "population-averaged model-implied hazard of \textbf{17.30} per 100 person-years"
4. Existing bridging sentence (good as-is, keep): "This figure represents an average hazard over the retrospective time-since-debut window rather than a current point-in-time incidence... current annual HIV incidence in Zimbabwe (which is approximately 0.4 per 100 person-years per the 2020 survey)."
5. Add at end of subsection: "We report this quantity to demonstrate that the fitted model yields a closed-form age-specific hazard, in response to Reviewer 2's request; it is not intended as an alternative estimator of HIV incidence in Zimbabwe."
6. Figure caption (line ~1083): change "age-specific HIV incidence hazard" to "age-specific model-implied hazard"

Also update `R/zimphia_incidence.R` function names? Optional — function names don't appear in the manuscript. Skip unless it bothers you.

#### Option B (alternative): restrict to HIV− subset; recompute scalar; keep "incidence" framing

Modify `compute_population_incidence()` to take an at-risk mask. ~30 seconds of recompute on existing draws.

```r
compute_population_incidence <- function(draws, pop, at_risk = NULL) {
  # ... existing argument checks ...
  if (is.null(at_risk)) at_risk <- rep(TRUE, nrow(pop))
  if (length(at_risk) != nrow(pop) || !is.logical(at_risk)) {
    stop("`at_risk` must be a logical vector of length nrow(pop).",
      call. = FALSE
    )
  }
  pop_at_risk <- pop[at_risk, , drop = FALSE]
  hazard_one <- function(alpha, beta, gamma) {
    ax <- alpha * exp(beta * pop_at_risk$X1)
    h <- (gamma / ax) * (pop_at_risk$age / ax)^(gamma - 1) /
      (1 + (pop_at_risk$age / ax)^gamma)
    stats::weighted.mean(h, pop_at_risk$weight)
  }
  per_draw <- mapply(hazard_one,
    draws$alpha, draws$beta, draws$gamma, USE.NAMES = FALSE
  )
  tibble::tibble(
    incidence_median = stats::median(per_draw),
    incidence_q2.5   = stats::quantile(per_draw, 0.025, names = FALSE),
    incidence_q97.5  = stats::quantile(per_draw, 0.975, names = FALSE)
  )
}
```

Call with `at_risk = pop$hiv_status == 0`. The new number will likely be larger (HIV− subjects skew younger and their hazard at peak ages is higher) — but it'll still be ~10–20× the PHIA published number because the underlying issue is the difference between hazard-at-observed-age and recent-infection incidence. The framing problem isn't fully resolved by this.

**Recommendation: do Option A.** It's faster, cleaner, and the manuscript already does most of the disambiguation work.

### Acceptance criteria for Issue #3

- [ ] If Option A: subsection retitled, body and figure caption updated, cross-references checked
- [ ] If Option B: function signature updated, test added for `at_risk` argument, scalar recomputed
- [ ] Abstract (if it currently uses "incidence" to refer to the 17.30 number) updated to match. Check.

---

## Issue #4 — Task A: JAGS subsection rename (MEDIUM)

### The problem

The previous handoff renamed Appendix `\subsection{Metropolis--Hastings (JAGS)}` → `\subsection{Univariate Slice Sampling (JAGS)}`. This is technically accurate (`rjags::list.samplers()` confirms `base::RealSlicer` for all three parameters) but creates an internal inconsistency: the title, abstract, all section headings, and every figure caption refer to "MH" or "Metropolis–Hastings." A reviewer might ask whether the comparison is HMC-vs-MH or HMC-vs-slice.

### The fix

Revert the heading rename. In whichever appendix file is affected (likely `10_BMC_Submission/01_Manuscript/manuscript_sn.tex`, somewhere in `\section*{Appendix Samplers}` or similar):

```latex
\subsection{Metropolis--Hastings (JAGS)}
```

Keep the **body** disclosure of slice sampling intact. The opening sentence of the subsection should read approximately:

```latex
The MH baseline is implemented in JAGS. JAGS assigns
\texttt{base::RealSlicer} (univariate slice sampling, \citealp{neal_slice_2003})
to all three continuous parameters via the zeros trick — a more aggressively
tuned default than the random-walk Metropolis algorithm described in
Algorithm~\ref{alg:mh}. We retain the ``MH'' label throughout the paper for
continuity with the framing in the title and abstract, but readers should note
that JAGS' actual sampler is slice sampling rather than random-walk Metropolis.
```

Algorithm 4 (the RWM-with-adaptive-cov pseudo-code) should now carry a footnote or caption note:

```latex
\caption{Metropolis--Hastings with Adaptive Covariance \emph{(idealised pseudo-code; the JAGS implementation used in this paper substitutes univariate slice sampling, see above)}}
```

### Citation needed (if not already present)

```bibtex
@article{neal_slice_2003,
  title   = {Slice sampling},
  author  = {Neal, Radford M.},
  journal = {Annals of Statistics},
  volume  = {31},
  number  = {3},
  pages   = {705--767},
  year    = {2003}
}
```

### Acceptance criteria for Issue #4

- [ ] Subsection heading reverted to "Metropolis--Hastings (JAGS)"
- [ ] Opening sentence discloses slice sampling
- [ ] Algorithm 4 caption notes the implementation substitution
- [ ] `neal_slice_2003` in bibliography
- [ ] Response letter R1.MH-implementation paragraph foregrounds the slice-sampler finding

---

## Issue #5 — Task F: Weibull calibration caveat (LOW)

### The problem

`R/simulation.R` line 36–38 sets `k_weibull = 2.0`, `lambda_weibull = 6.01`, calibrated so the Weibull DGM matches the log-logistic median of 5 years. Only the median is matched; the IQR and tails differ between the two distributions. This is a minor limitation worth one sentence in the misspecification subsection.

### The fix

Locate the misspecification results subsection in the manuscript (search for "Performance under baseline misspecification" or the Weibull/misspec wording in §4 or §5). Add at the end of the methodology paragraph:

```latex
The Weibull data-generating mechanism was calibrated to match the
log-logistic truth's median time-to-event (median \(T = 5\) years);
higher-order moments differ between the two distributions. Calibrations
matching multiple moments simultaneously could be explored as a more
stringent misspecification scenario, but the present calibration is
sufficient to demonstrate that the qualitative sampler ordering
(HMC \(>\) MH on ESS/s) is preserved under shape misspecification.
```

### Acceptance criteria for Issue #5

- [ ] One sentence added to the misspec methodology paragraph

---

## Out of scope (do NOT touch)

These items were reviewed and found correct. Changes risk introducing regressions:

- Task B (multivariable sensitivity): FILL values correct, "well within univariate CrI" branch correct.
- Task G (HMC 1,000-rep verification): MC SE agreement within 2σ; numbers are right.
- Task H (ESS/s reframing in abstract / §1 / Discussion): correct framing.
- Task I (constant-incidence Discussion subsection): correct text.
- Task J (caption qualifiers for asymmetric design): correct.
- JAGS audit code (`R/jags_audit.R`, `logs/substantive_revision/jags_audit.txt`): correct.
- Misspec simulation code (`R/misspec_simulation.R`): correct.
- All test files: tests passing.

---

## Verification checklist (after all fixes)

Run from `04_Code/`:

```r
devtools::test()                              # expect ≥ 39 tests PASS (37 existing + 2 new for combine_design_replicates)
devtools::check(args = c("--no-manual"))      # expect 0 errors / 0 warnings / 0 notes
```

Run from `10_BMC_Submission/01_Manuscript/`:

```bash
pdflatex -interaction=nonstopmode manuscript_sn.tex
bibtex manuscript_sn
pdflatex -interaction=nonstopmode manuscript_sn.tex
pdflatex -interaction=nonstopmode manuscript_sn.tex
```

Expected: zero new errors; cross-reference to `sec:implied-hazard` resolves if Option A on Issue #3 was taken; `beaumont_bayesian_2008`, `rubin_multiple_1987`, `neal_slice_2003` resolve in bibliography.

Spot-check by hand:

- [ ] §sec:design-variance reports a Rubin total credible interval; the inflation factor reported is ≥ 1 (sanity check)
- [ ] Birth-cohort paragraph uses the non-stationary branch with right-censoring caveat
- [ ] Subsection on hazard/incidence consistently uses "model-implied hazard" if Option A, or restricts to at-risk if Option B
- [ ] Appendix subsection on JAGS retains the "Metropolis--Hastings (JAGS)" heading but discloses slice sampling in the body
- [ ] Misspec methodology paragraph has the calibration caveat sentence

---

## Time budget

| Step | Estimate |
|---|---|
| Issue #1 code changes (function + test + script tweak) | 30 min |
| Issue #1 re-run 100 replicate fits (saving draws) | 2.4 h unattended |
| Issue #1 post-process + manuscript rewrite | 45 min |
| Issue #2 manuscript edits | 10 min |
| Issue #3 manuscript edits (Option A) | 15 min |
| Issue #4 manuscript edits + bib | 15 min |
| Issue #5 one-sentence add | 5 min |
| Verification + LaTeX rebuild | 30 min |
| **Total wall-clock** | **~4.5 hours (mostly unattended)** |

The only re-run needed is Issue #1, and it's worth doing because the proper Rubin combination requires per-replicate posterior dispersion data that the original run didn't save. Backing this out from quantiles via normal approximation would work in a pinch (≈ width/3.92), but a full re-run saving draws is more defensible and only adds ~2.4 hours unattended.

Once these fixes land, the substantive revision is genuinely ready for submission.
