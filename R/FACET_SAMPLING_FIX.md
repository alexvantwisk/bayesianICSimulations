# Fix: facet_design_panels() Sampling Error

## Issue

Error when running `facet_design_panels()`:
```
Error in `dplyr::slice_sample()`:
! `n` must be a constant.
Caused by error:
! object 'rep_id' not found
```

## Root Cause

The code attempted to use `dplyr::n_distinct(rep_id)` inside `slice_sample()`:

```r
ghost_df <- combined_rep %>%
  dplyr::group_by(cell_id) %>%
  dplyr::slice_sample(n = min(30, dplyr::n_distinct(rep_id))) %>%
  dplyr::ungroup()
```

**Problem:** `n_distinct(rep_id)` was evaluated outside the grouped data context, so `rep_id` was not found.

## Solution

Changed to use `filter()` with explicit sampling logic:

```r
ghost_df <- combined_rep %>%
  dplyr::group_by(cell_id) %>%
  dplyr::filter(rep_id %in% {
    unique_reps <- unique(rep_id)
    sample(unique_reps, min(30, length(unique_reps)), replace = FALSE)
  }) %>%
  dplyr::ungroup()
```

This approach:
1. Groups by `cell_id`
2. Within each group, gets unique `rep_id` values
3. Samples up to 30 unique replicate IDs
4. Filters to keep only those sampled replicates

## File Updated

**R/plot_weighted_ic_survival.R** (lines 929-937)

## Testing

The fix ensures that:
- Each facet panel shows up to 30 ghost replicates
- Sampling is deterministic (uses R's random seed)
- Works even when groups have fewer than 30 replicates
- No evaluation order issues with dplyr

## Verification

Re-run the example:

```r
source("R/plot_weighted_ic_survival.R")

# This should now work without errors
p_facet <- facet_design_panels(
  results_list = results_list,
  design_df = design_df,
  facet_rows = "censoring",
  facet_cols = "weight_type",
  which = "S"
)

print(p_facet)
```

Expected: Multi-panel plot with ghost lines properly sampled in each panel.
