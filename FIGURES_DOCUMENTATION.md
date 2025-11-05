# Figure Reference Guide

A quick reference for all 16 figures comparing HMC and MH methods for interval-censored survival analysis.

---

## Figure 1a: R-hat Convergence Diagnostics
**File**: `fig1a_rhat_ecdf.png` | **Size**: 9" × 3.2"

Shows cumulative distribution of R-hat convergence diagnostics across sample sizes. Assesses what proportion of model fits achieved good convergence (R-hat ≤ 1.01). Separate curves for HMC vs MH.

---

## Figure 1b: Effective Sample Size Distributions
**File**: `fig1b_ess_ridges.png` | **Size**: 9" × 3.2"

Ridge plots showing ESS (effective sample size) distributions across all simulation scenarios. Compares sampling efficiency between methods. Higher ESS indicates more independent posterior samples.

---

## Figure 2: Coverage of 95% Credible Intervals
**File**: `fig2_coverage.png` | **Size**: 10" × 6"

Empirical coverage rates of 95% credible intervals for the treatment effect parameter β₁. Shows whether intervals contain the true value 95% of the time as intended. Grey band indicates Monte Carlo uncertainty.

---

## Figure 3a: Bias of Treatment Effect Estimates
**File**: `fig3a_bias.png` | **Size**: 9" × 6"

Mean bias of β₁ estimates across all design cells (censoring levels, weight regimes, sample sizes). Zero bias is ideal. Points show mean with error bars indicating Monte Carlo standard errors.

---

## Figure 3b: Root Mean Squared Error
**File**: `fig3b_rmse.png` | **Size**: 9" × 6"

RMSE of β₁ estimates measuring overall estimation accuracy (combines bias and variance). Lower values indicate better performance. Text annotations show relative performance between methods.

---

## Figure 3c: Bias–RMSE Trade-off
**File**: `fig3c_bias_rmse_tradeoff.png` | **Size**: 9" × 6"

Two-dimensional view of bias (x-axis) vs RMSE (y-axis). Point size indicates sample size. Horizontal bars show credible interval widths. Bottom-left corner represents optimal performance (low bias, low RMSE).

---

## Figure 4: Computational Efficiency
**File**: `fig4_ess_per_sec.png` | **Size**: 9" × 3.2"

Violin plots showing effective samples per second of computation time. Measures cost-adjusted efficiency. Black dots mark median values.

---

## Figure 5b: Runtime Speed-up Heatmap
**File**: `fig5b_speedup_heatmap.png` | **Size**: 8" × 6"

Heatmap showing how much faster one method is than the other (MH ÷ HMC). Blue tiles indicate HMC is faster, orange indicates MH is faster. Numbers show speed-up factors (e.g., "2.3×").

---

## Figure 5c: Paired Runtime Comparison
**File**: `fig5c_runtime_slope.png` | **Size**: 9" × 6"

Slope plot connecting HMC and MH runtimes for the same design cell. Line color indicates which method was faster. Steeper slopes show larger performance differences. Point shapes indicate weighting schemes.

---

## Figure 6: Survival Curve Estimates
**File**: `fig6_survival_cells.png` | **Size**: 12" × 16"

Large multi-panel plot showing estimated survival curves across all 27 design cells. Ghost curves show individual replicates, ribbons show uncertainty bands, and the red line shows the true population curve. Visualizes how well methods recover the truth.

---

## Figure 7: Agreement Between Methods
**File**: `fig7_means_agreement.png` | **Size**: 9" × 6"

Scatter plots comparing posterior means from HMC (x-axis) vs MH (y-axis) for all three parameters. Points should cluster along the 1:1 diagonal line if methods agree. Includes correlation and regression statistics.

---

## Figure A1: Credible Intervals vs Truth
**File**: `figA1_cis_vs_truth.png` | **Size**: 11" × 7"

Forest plot showing 95% credible intervals for all parameters across scenarios. Vertical dashed line marks the true value. Opacity indicates whether the interval captured the truth (bright = yes, faded = no).

---

## Figure A2: Credible Interval Matrix
**File**: `figA2_ci_matrix.png` | **Size**: 10" × 18"

Comprehensive matrix showing median credible intervals organized by parameter, sample size, and method. Filled circles indicate adequate coverage (≥95%), open circles indicate under-coverage.

---

## Figure B1: Precision–Coverage Trade-off
**File**: `figB1_precision_tradeoff.png` | **Size**: 10" × 6"

Shows relationship between interval width (precision) and coverage rate. Grey lines connect HMC-MH pairs for the same scenario. Ideal performance is top-left (high coverage with narrow intervals).

---

## Figure B2: Coverage Bias Heatmap
**File**: `figB2_coverage_bias_heatmap.png` | **Size**: 10" × 5"

Heatmap showing deviations from nominal 95% coverage. Red indicates under-coverage (anti-conservative), blue indicates over-coverage (conservative). Numbers show percentage point deviations.

---

## Figure B3: Credible Interval Width Distributions
**File**: `figB3_ci_width_violin.png` | **Size**: 9" × 6"

Violin plots showing the distribution of 95% CI widths. Narrower distributions with lower medians indicate more precise and consistent uncertainty quantification. Black lines mark interquartile ranges.

---

## Quick Legend

- **HMC**: Blue
- **MH**: Orange
- **Sample sizes**: n = 200, 2000, 10000
- **Censoring levels**: 10%, 30%, 50%
- **Weight types**: none, low dispersion, high dispersion
- **Parameters**: α (scale), β (treatment effect), γ (shape)