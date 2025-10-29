# ==============================================================================
# example_usage.R
#
# Smoke test and usage examples for plot_weighted_ic_survival.R
# Demonstrates loading simulation data and creating thesis-ready visualizations
#
# Author: Generated for MSc Biostatistics Dissertation
# Date: 2025
# ==============================================================================

# Load Required Functions ------------------------------------------------------
source("R/plot_weighted_ic_survival.R")

# Load Required Packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(viridis)

# NOTE: icenReg must be installed first
if (!requireNamespace("icenReg", quietly = TRUE)) {
  message("Installing icenReg package...")
  install.packages("icenReg")
}

library(icenReg)

# ==============================================================================
# Example 1: Single Design Cell Analysis
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("Example 1: Single Cell Analysis (n=200, c=0.1, whigh)")
message(paste(rep("=", 80), collapse = ""))

# Load simulation data from one design cell
# Pattern matches: scenario 1, any replicate, n=200, censoring=0.1, weight=high
sims_single <- load_sims_from_dir(
  path = "sim_data/n200",
  pattern = "sim_s001_r.*_n0200_c0\\.1_whigh\\.rds$",
  require_cols = c("L", "R", "weight", "X1"),
  sample_size_filter = 200
)

message("Loaded ", length(sims_single), " replicates for single cell")

# If no files found, try a broader pattern (for demonstration)
if (length(sims_single) == 0) {
  message("No files found with strict pattern; trying broader search...")
  sims_single <- load_sims_from_dir(
    path = "sim_data/n200",
    pattern = "sim_s.*_n0200_c0\\.1_whigh\\.rds$",
    require_cols = c("L", "R", "weight", "X1"),
    sample_size_filter = 200
  )
}

# Limit to first 50 replicates for speed
if (length(sims_single) > 50) {
  message("Using first 50 replicates for speed")
  sims_single <- sims_single[1:50]
}

# Summarise survival curves with covariate
results_single <- summarise_weighted_curves(
  sims = sims_single,
  weight_metric = "cv",
  include_covariate = TRUE
)

message("Successfully fit ", results_single$meta$n_converged, " / ",
        results_single$meta$n_total, " replicates")

# Plot survival S(t)
p_surv <- plot_weighted_spaghetti(
  sum_df = results_single$sum_df,
  rep_df = results_single$rep_df,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5,
  X1_dist = c(0.5, 0.5),  # 50/50 distribution of X1
  which = "S",
  n_ghost = 30,
  seed = 1,
  palette = "viridis",
  title = "Survival: n=200, c=0.1, whigh"
)

print(p_surv)

# Save plot
ggsave(
  filename = "results/figures/example_survival_single_cell.png",
  plot = p_surv,
  width = 10,
  height = 6,
  dpi = 300
)

message("Saved survival plot to: results/figures/example_survival_single_cell.png")

# Plot cumulative hazard H(t)
p_chaz <- plot_weighted_spaghetti(
  sum_df = results_single$sum_df,
  rep_df = results_single$rep_df,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5,
  which = "H",
  n_ghost = 30,
  seed = 1,
  title = "Cumulative Hazard: n=200, c=0.1, whigh"
)

print(p_chaz)

# Plot hazard h(t) (with smoothing)
p_haz <- plot_weighted_spaghetti(
  sum_df = results_single$sum_df,
  rep_df = results_single$rep_df,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5,
  which = "h",
  n_ghost = 20,  # Fewer ghosts for clearer hazard plot
  seed = 1,
  title = "Hazard: n=200, c=0.1, whigh"
)

print(p_haz)

# Print caption
message("\nPlot caption:")
message(get_caption_text())

# ==============================================================================
# Example 2: Intercept-Only Model (no covariate)
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("Example 2: Intercept-Only Model")
message(paste(rep("=", 80), collapse = ""))

# Fit without covariate
results_intercept <- summarise_weighted_curves(
  sims = sims_single,
  weight_metric = "cv",
  include_covariate = FALSE  # Fit ~ 1
)

message("Intercept-only: ", results_intercept$meta$n_converged, " / ",
        results_intercept$meta$n_total, " converged")

p_intercept <- plot_weighted_spaghetti(
  sum_df = results_intercept$sum_df,
  rep_df = results_intercept$rep_df,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5,  # Still used for true marginal curve
  which = "S",
  n_ghost = 30,
  title = "Intercept-Only Model"
)

print(p_intercept)

# ==============================================================================
# Example 3: Kish Effective Sample Size Metric
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("Example 3: Using Kish Effective Sample Size")
message(paste(rep("=", 80), collapse = ""))

results_kish <- summarise_weighted_curves(
  sims = sims_single,
  weight_metric = "kish_ess",
  include_covariate = TRUE
)

p_kish <- plot_weighted_spaghetti(
  sum_df = results_kish$sum_df,
  rep_df = results_kish$rep_df,
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5,
  which = "S",
  n_ghost = 30,
  palette = "plasma",
  title = "Colored by Kish Effective Sample Size"
)

# Update legend label
p_kish <- p_kish +
  ggplot2::scale_color_viridis_c(
    option = "plasma",
    name = "Kish n_eff",
    guide = ggplot2::guide_colorbar(barwidth = 10, barheight = 0.5)
  )

print(p_kish)

# ==============================================================================
# Example 4: Multi-Panel Faceted Plot
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("Example 4: Faceted Multi-Panel Plot")
message(paste(rep("=", 80), collapse = ""))

# Auto-generate design_df from files
design_df <- create_design_df_from_files(
  path = "sim_data/n200",
  true_alpha = 5.0,
  true_gamma = 1.5,
  true_beta = -0.5
)

message("Found ", nrow(design_df), " unique design cells in sim_data/n200")
print(design_df)

# Load and analyze multiple cells
# For demonstration, limit to 3 cells to keep runtime reasonable
selected_cells <- head(design_df$cell_id, 3)

results_list <- list()

for (cell_id in selected_cells) {
  message("\nProcessing cell: ", cell_id)

  # Extract design factors
  cell_meta <- design_df %>% filter(cell_id == !!cell_id)

  # Build file pattern
  pattern <- sprintf(
    "sim_s.*_n%04d_c%.1f_w%s\\.rds$",
    cell_meta$n,
    cell_meta$censoring,
    cell_meta$weight_type
  )

  # Load data
  sims <- load_sims_from_dir(
    path = sprintf("sim_data/n%d", cell_meta$n),
    pattern = pattern,
    require_cols = c("L", "R", "weight", "X1")
  )

  # Limit to 30 replicates for speed
  if (length(sims) > 30) {
    sims <- sims[1:30]
  }

  if (length(sims) == 0) {
    message("  No data found for ", cell_id)
    next
  }

  # Fit models
  results <- tryCatch({
    summarise_weighted_curves(
      sims = sims,
      weight_metric = "cv",
      include_covariate = TRUE
    )
  }, error = function(e) {
    message("  Error fitting ", cell_id, ": ", e$message)
    NULL
  })

  if (!is.null(results)) {
    results_list[[cell_id]] <- results
    message("  Converged: ", results$meta$n_converged, " / ",
            results$meta$n_total)
  }
}

# Create faceted plot if we have results
if (length(results_list) > 0) {
  # Subset design_df to cells we actually fit
  design_df_subset <- design_df %>%
    filter(cell_id %in% names(results_list))

  p_facet <- facet_design_panels(
    results_list = results_list,
    design_df = design_df_subset,
    facet_rows = "censoring",
    facet_cols = "weight_type",  # Or "n" for sample size
    which = "S"
  )

  print(p_facet)

  # Save faceted plot
  ggsave(
    filename = "results/figures/example_faceted_panels.png",
    plot = p_facet,
    width = 12,
    height = 8,
    dpi = 300
  )

  message("\nSaved faceted plot to: results/figures/example_faceted_panels.png")
} else {
  message("\nNo results to plot (all cells failed)")
}

# ==============================================================================
# Example 5: Comparing Weight Types in Same Panel
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("Example 5: Comparing Different Weight Types")
message(paste(rep("=", 80), collapse = ""))

# Load three weight types: high, low, none (all n=200, c=0.1)
weight_types <- c("high", "low", "none")
results_by_weight <- list()

for (wt in weight_types) {
  pattern <- sprintf("sim_s.*_n0200_c0\\.1_w%s\\.rds$", wt)

  sims_wt <- load_sims_from_dir(
    path = "sim_data/n200",
    pattern = pattern,
    require_cols = c("L", "R", "weight", "X1")
  )

  # Limit to 30 for speed
  if (length(sims_wt) > 30) {
    sims_wt <- sims_wt[1:30]
  }

  if (length(sims_wt) > 0) {
    results_by_weight[[wt]] <- summarise_weighted_curves(
      sims = sims_wt,
      weight_metric = "cv",
      include_covariate = FALSE  # Simpler for comparison
    )

    message("Weight type '", wt, "': ", results_by_weight[[wt]]$meta$n_converged,
            " / ", results_by_weight[[wt]]$meta$n_total, " converged")
  }
}

# Create side-by-side comparison
if (length(results_by_weight) > 0) {
  library(patchwork)  # For combining plots

  plots_by_weight <- lapply(names(results_by_weight), function(wt) {
    plot_weighted_spaghetti(
      sum_df = results_by_weight[[wt]]$sum_df,
      rep_df = results_by_weight[[wt]]$rep_df,
      true_alpha = 5.0,
      true_gamma = 1.5,
      true_beta = -0.5,
      which = "S",
      n_ghost = 20,
      title = paste("Weight type:", wt)
    )
  })

  # Combine plots
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_combined <- plots_by_weight[[1]] + plots_by_weight[[2]] + plots_by_weight[[3]] +
      patchwork::plot_layout(ncol = 3, guides = "collect") &
      theme(legend.position = "bottom")

    print(p_combined)

    ggsave(
      filename = "results/figures/example_weight_comparison.png",
      plot = p_combined,
      width = 15,
      height = 5,
      dpi = 300
    )

    message("Saved weight comparison to: results/figures/example_weight_comparison.png")
  }
}

# ==============================================================================
# Summary
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("All examples completed successfully!")
message(paste(rep("=", 80), collapse = ""))

message("\nGenerated plots:")
message("  1. results/figures/example_survival_single_cell.png")
message("  2. results/figures/example_faceted_panels.png")
message("  3. results/figures/example_weight_comparison.png")

message("\nKey functions demonstrated:")
message("  - load_sims_from_dir()         : Load simulation replicates")
message("  - create_design_df_from_files(): Auto-generate design metadata")
message("  - summarise_weighted_curves()  : Fit models and compute summaries")
message("  - plot_weighted_spaghetti()    : Create ghosted spaghetti plots")
message("  - facet_design_panels()        : Multi-panel faceted visualization")
message("  - get_caption_text()           : Standardized caption")

message("\nWeight metrics available:")
message("  - 'cv'       : Coefficient of variation (default)")
message("  - 'kish_ess' : Kish effective sample size")

message("\nPlot types available:")
message("  - which='S' : Survival S(t)")
message("  - which='H' : Cumulative hazard H(t)")
message("  - which='h' : Hazard h(t) (smoothed, use with caution)")

message("\nFor thesis use:")
message("  ", get_caption_text())
