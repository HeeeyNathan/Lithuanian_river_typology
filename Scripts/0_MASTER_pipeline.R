# ===============================================================================
# LITHUANIAN RIVER TYPOLOGY - MASTER PIPELINE
# ===============================================================================
#
# Purpose: Execute complete analytical pipeline for exploring Lithuanian river
#          typology based on abiotic (physico-chemical) characteristics
#
# Author: Freshwater Ecologist
# Date: 2025-01-04
#
# This master script orchestrates the entire analytical workflow, from data
# preparation through spatial interpolation, classification, and visualization
#
# ===============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                        ║\n")
cat("║          LITHUANIAN RIVER TYPOLOGY - MASTER PIPELINE                  ║\n")
cat("║          Abiotic-Based River Classification Explorer                  ║\n")
cat("║                                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ===============================================================================
# PIPELINE CONFIGURATION
# ===============================================================================

cat("Configuration\n")
cat("═════════════\n\n")

# Define which scripts to run
config <- list(
  run_data_prep = TRUE,           # Script 1-2: Data preparation
  run_tprs = TRUE,                # Script 6: TPRS spatial interpolation
  run_typology = TRUE,            # Script 7: Advanced typology exploration
  run_mapping = TRUE,             # Script 8: Spatial cluster mapping
  run_previous_analyses = FALSE   # Scripts 3-5: Previous exploratory analyses
)

# Display configuration
cat("Pipeline steps to execute:\n")
cat(paste0("  [", ifelse(config$run_data_prep, "✓", " "), "] Data preparation (Scripts 1-2)\n"))
cat(paste0("  [", ifelse(config$run_tprs, "✓", " "), "] TPRS spatial interpolation (Script 6)\n"))
cat(paste0("  [", ifelse(config$run_typology, "✓", " "), "] Advanced typology exploration (Script 7)\n"))
cat(paste0("  [", ifelse(config$run_mapping, "✓", " "), "] Spatial cluster mapping (Script 8)\n"))
cat(paste0("  [", ifelse(config$run_previous_analyses, "✓", " "), "] Previous exploratory analyses (Scripts 3-5)\n"))
cat("\n")

# Timing
start_time <- Sys.time()

# ===============================================================================
# ERROR HANDLING WRAPPER
# ===============================================================================

safe_run <- function(script_name, script_path) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════════\n")
  cat(paste0("EXECUTING: ", script_name, "\n"))
  cat("═══════════════════════════════════════════════════════════════════════\n\n")

  script_start <- Sys.time()

  tryCatch({
    source(script_path, echo = FALSE)
    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "secs")

    cat("\n")
    cat(paste0("✓ ", script_name, " completed successfully!\n"))
    cat(paste0("  Time elapsed: ", round(elapsed, 2), " seconds\n"))

    return(list(success = TRUE, time = elapsed))

  }, error = function(e) {
    script_end <- Sys.time()
    elapsed <- difftime(script_end, script_start, units = "secs")

    cat("\n")
    cat(paste0("✗ ERROR in ", script_name, ":\n"))
    cat(paste0("  ", e$message, "\n"))
    cat(paste0("  Time elapsed: ", round(elapsed, 2), " seconds\n"))

    return(list(success = FALSE, time = elapsed, error = e$message))
  })
}

# ===============================================================================
# PIPELINE EXECUTION
# ===============================================================================

results <- list()

# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: DATA PREPARATION
# ─────────────────────────────────────────────────────────────────────────────

if(config$run_data_prep) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PHASE 1: DATA PREPARATION                                 ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  # Script 1: Extract elevation data
  results$elevation <- safe_run(
    "1. Elevation Extraction",
    "Scripts/1_extracting_elevation.R"
  )

  # Script 2: Calculate seasonal means
  results$seasonal_means <- safe_run(
    "2. Seasonal Means Calculation",
    "Scripts/2_calculated_seasonal_means.R"
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: SPATIAL INTERPOLATION (TPRS)
# ─────────────────────────────────────────────────────────────────────────────

if(config$run_tprs) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PHASE 2: SPATIAL INTERPOLATION (TPRS)                     ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  results$tprs <- safe_run(
    "6. TPRS Spatial Interpolation",
    "Scripts/6_TPRS_spatial_interpolation.R"
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: ADVANCED TYPOLOGY EXPLORATION
# ─────────────────────────────────────────────────────────────────────────────

if(config$run_typology) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PHASE 3: ADVANCED TYPOLOGY EXPLORATION                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  results$typology <- safe_run(
    "7. Advanced Typology Exploration",
    "Scripts/7_advanced_typology_exploration.R"
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: SPATIAL CLUSTER MAPPING
# ─────────────────────────────────────────────────────────────────────────────

if(config$run_mapping) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PHASE 4: SPATIAL CLUSTER MAPPING                          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  results$mapping <- safe_run(
    "8. Spatial Cluster Mapping",
    "Scripts/8_spatial_cluster_mapping.R"
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# OPTIONAL: PREVIOUS EXPLORATORY ANALYSES
# ─────────────────────────────────────────────────────────────────────────────

if(config$run_previous_analyses) {

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PHASE 5: PREVIOUS EXPLORATORY ANALYSES                    ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")

  results$covariate_plot <- safe_run(
    "3. Covariate Panel Plot",
    "Scripts/3_creating_covariate_panel_plot.R"
  )

  results$rda <- safe_run(
    "4. Initial RDA",
    "Scripts/4_initial_RDA.R"
  )

  results$cluster <- safe_run(
    "5. Initial Cluster Analysis",
    "Scripts/5_initial_cluster_analysis.R"
  )
}

# ===============================================================================
# PIPELINE SUMMARY
# ===============================================================================

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

cat("\n\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                        ║\n")
cat("║                      PIPELINE EXECUTION COMPLETE                       ║\n")
cat("║                                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("Execution Summary\n")
cat("═════════════════\n\n")

# Count successes and failures
n_success <- sum(sapply(results, function(x) x$success))
n_total <- length(results)

cat(paste0("Total scripts executed: ", n_total, "\n"))
cat(paste0("Successful: ", n_success, " (", round(n_success/n_total*100, 1), "%)\n"))
cat(paste0("Failed: ", n_total - n_success, "\n"))
cat(paste0("Total time: ", round(total_time, 2), " minutes\n\n"))

# Detailed results
cat("Detailed Results:\n")
cat("─────────────────\n")
for(step_name in names(results)) {
  result <- results[[step_name]]
  status <- ifelse(result$success, "✓ SUCCESS", "✗ FAILED")
  cat(sprintf("  %-25s: %s (%.2f sec)\n", step_name, status, result$time))
  if(!result$success) {
    cat(sprintf("    Error: %s\n", result$error))
  }
}

cat("\n")

# ===============================================================================
# OUTPUT SUMMARY
# ===============================================================================

cat("\nGenerated Outputs\n")
cat("═════════════════\n\n")

cat("Data files (Output data/):\n")
cat("  1. site_factors_2009-2023_wElevation.xlsx\n")
cat("  2. annual_environmental_means_2009-2023.xlsx\n")
cat("  3. seasonal_annual_environmental_means_2009-2023.xlsx\n")
cat("  4. TPRS_interpolated_grid.csv\n")
cat("  5. site_data_with_interpolations.csv\n")
cat("  6. TPRS_model_summaries.csv\n")
cat("  7. classification_scheme_comparison.csv\n")
cat("  8. best_cluster_assignments.csv\n")
cat("  9. cluster_profiles.csv\n")
cat("  10. temporal_stability.csv\n\n")

cat("Figures (Plots/):\n")
cat("  • Figure 1: Sampling sites map\n")
cat("  • Figure 2: Land cover\n")
cat("  • Figure 3: Covariate panel plots\n")
cat("  • Figure 4: TPRS typology variable isoline maps\n")
cat("  • Figure 5: TPRS nutrient variable isoline maps\n")
cat("  • Figure 6: Spatial environmental gradients\n")
cat("  • Figure 7: Classification dendrograms\n")
cat("  • Figure 8: RDA ordination (best scheme)\n")
cat("  • Figure 9: Typology comparison maps\n")
cat("  • Figure 10: Environmental gradient panels\n\n")

# ===============================================================================
# NEXT STEPS
# ===============================================================================

cat("Recommended Next Steps\n")
cat("══════════════════════\n\n")

if(n_success == n_total) {
  cat("✓ All analyses completed successfully!\n\n")
  cat("Next steps for ecological interpretation:\n\n")
  cat("1. BIOLOGICAL VALIDATION\n")
  cat("   → Compare clusters with macroinvertebrate community data\n")
  cat("   → Test if abiotic clusters better predict biological metrics\n")
  cat("   → Develop cluster-specific biological indices\n\n")
  cat("2. TEMPORAL ANALYSIS\n")
  cat("   → Assess year-to-year stability of cluster assignments\n")
  cat("   → Identify sites switching between clusters\n")
  cat("   → Explore seasonal vs annual classification\n\n")
  cat("3. RIVER FEEDING REGIME\n")
  cat("   → Infer groundwater vs surface water dominance\n")
  cat("   → Use temperature/conductivity ratios\n")
  cat("   → Identify spring-fed systems\n\n")
  cat("4. MANUSCRIPT PREPARATION\n")
  cat("   → Synthesize findings into coherent narrative\n")
  cat("   → Compare with European WFD typology approaches\n")
  cat("   → Highlight ecological relevance of new typology\n\n")
  cat("5. MANAGEMENT IMPLICATIONS\n")
  cat("   → Identify reference sites for each cluster\n")
  cat("   → Develop cluster-specific ecological targets\n")
  cat("   → Inform WFD implementation in Lithuania\n\n")
} else {
  cat("⚠ Some analyses failed. Review error messages above.\n\n")
  cat("Troubleshooting:\n")
  cat("  → Check that all required R packages are installed\n")
  cat("  → Verify input data files exist in 'Input data/'\n")
  cat("  → Ensure output directories exist\n")
  cat("  → Review individual script error messages\n\n")
}

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("For questions or issues, review individual scripts in Scripts/ folder\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Save pipeline summary
summary_df <- data.frame(
  script = names(results),
  success = sapply(results, function(x) x$success),
  time_seconds = sapply(results, function(x) x$time),
  error = sapply(results, function(x) ifelse(!x$success, x$error, NA))
)

write.csv(summary_df,
          "Output data/pipeline_execution_summary.csv",
          row.names = FALSE)

cat("Pipeline summary saved to: Output data/pipeline_execution_summary.csv\n\n")
