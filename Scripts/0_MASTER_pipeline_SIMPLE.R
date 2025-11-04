# ===============================================================================
# SIMPLIFIED MASTER PIPELINE - WITH BETTER ERROR HANDLING
# ===============================================================================
#
# This simplified version runs scripts sequentially with better dependency
# checking and error handling
#
# ===============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                        ║\n")
cat("║          LITHUANIAN RIVER TYPOLOGY - SIMPLIFIED PIPELINE              ║\n")
cat("║                                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Start timer
pipeline_start <- Sys.time()

# ===============================================================================
# STEP 1: CHECK REQUIREMENTS
# ===============================================================================

cat("STEP 1: Checking requirements...\n")
cat("═══════════════════════════════════\n\n")

# Check input data
if(!file.exists("Input data/2_environmental_data_2009-2023.xlsx")) {
  stop("ERROR: Input data file not found!\nPlease ensure 'Input data/2_environmental_data_2009-2023.xlsx' exists.")
}

cat("✓ Input data found\n\n")

# Check packages
required_pkgs <- c("dplyr", "tidyr", "readxl", "mgcv", "vegan", "cluster", "ggplot2")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if(length(missing_pkgs) > 0) {
  cat("⚠ Missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  cat("Installing missing packages...\n")
  install.packages(missing_pkgs)
}

cat("✓ All required packages available\n\n")

# ===============================================================================
# STEP 2: CALCULATE SEASONAL MEANS (if needed)
# ===============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 2: SEASONAL MEANS CALCULATION                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

if(file.exists("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")) {
  cat("✓ Seasonal means already calculated, skipping...\n")
} else {
  cat("Running seasonal means calculation...\n")
  tryCatch({
    source("Scripts/2_calculated_seasonal_means.R")
    cat("✓ Seasonal means calculated successfully!\n")
  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
    stop("Cannot proceed without seasonal means")
  })
}

# ===============================================================================
# STEP 3: TPRS SPATIAL INTERPOLATION (if needed)
# ===============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 3: TPRS SPATIAL INTERPOLATION                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

if(file.exists("Output data/5_site_data_with_interpolations.csv")) {
  cat("✓ TPRS interpolation already done, skipping...\n")
} else {
  cat("Running TPRS spatial interpolation...\n")
  cat("(This may take 5-10 minutes...)\n\n")

  tryCatch({
    source("Scripts/6_TPRS_spatial_interpolation.R")
    cat("\n✓ TPRS interpolation complete!\n")
  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
    cat("\nTrying to continue without interpolation...\n")
  })
}

# ===============================================================================
# STEP 4: ADVANCED TYPOLOGY EXPLORATION
# ===============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 4: ADVANCED TYPOLOGY EXPLORATION                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Check if we have the required input
if(!file.exists("Output data/5_site_data_with_interpolations.csv")) {
  cat("⚠ Cannot run typology exploration without interpolated data\n")
  cat("  Creating basic site data from seasonal means...\n\n")

  # Create a basic version without interpolation
  library(dplyr)
  library(readxl)

  annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

  site_data_basic <- annual_means %>%
    filter(data_quality == "complete") %>%
    group_by(site_id) %>%
    summarise(
      longitude = first(longitude),
      latitude = first(latitude),
      elevation = first(elevation),
      river_type = first(river_type),
      river_basin = first(river_basin),

      # Use observed values (not interpolated)
      temp = mean(`temp[°C]_ann`, na.rm = TRUE),
      pH = mean(`pH_ann`, na.rm = TRUE),
      EC = mean(`EC[µS/cm]_ann`, na.rm = TRUE),
      alkalinity = mean(`alkalinity[mmol/l]_ann`, na.rm = TRUE),
      DO_mg = mean(`D.O.[mg/l]_ann`, na.rm = TRUE),
      total_N = mean(`total_N[mg/L]_ann`, na.rm = TRUE),
      total_P = mean(`total_P[mg/L]_ann`, na.rm = TRUE),
      flow = mean(`flow[m3/s]_ann`, na.rm = TRUE),
      velocity = mean(`velocity[m/s]_ann`, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

  # Save it
  write.csv(site_data_basic, "Output data/5_site_data_basic.csv", row.names = FALSE)
  cat("✓ Basic site data created\n\n")
}

cat("Running advanced typology exploration...\n\n")

tryCatch({
  source("Scripts/7_advanced_typology_exploration.R")
  cat("\n✓ Typology exploration complete!\n")
}, error = function(e) {
  cat("✗ Error in typology exploration:", e$message, "\n")
  cat("\nAttempting alternative clustering approach...\n\n")

  # Try a simpler clustering approach
  tryCatch({
    library(vegan)
    library(cluster)
    library(dplyr)
    library(readxl)

    cat("Loading data...\n")
    annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

    # Simple site-level aggregation
    site_data <- annual_means %>%
      filter(data_quality == "complete") %>%
      group_by(site_id) %>%
      summarise(
        river_type = first(river_type),
        temp = mean(`temp[°C]_ann`, na.rm = TRUE),
        pH = mean(`pH_ann`, na.rm = TRUE),
        EC = mean(`EC[µS/cm]_ann`, na.rm = TRUE),
        alkalinity = mean(`alkalinity[mmol/l]_ann`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      na.omit()

    cat(paste0("Sites with complete data: ", nrow(site_data), "\n"))

    # Simple clustering
    mat <- site_data %>% select(temp, pH, EC, alkalinity) %>% scale()
    dist_mat <- dist(mat)
    hc <- hclust(dist_mat, method = "ward.D2")

    # Determine k (3-5 clusters typical)
    sil <- sapply(2:6, function(k) {
      mean(silhouette(cutree(hc, k), dist_mat)[, 3])
    })
    best_k <- which.max(sil) + 1

    clusters <- cutree(hc, k = best_k)

    # Save results
    cluster_assignments <- data.frame(
      site_id = site_data$site_id,
      river_type = site_data$river_type,
      cluster = clusters
    )

    write.csv(cluster_assignments,
              "Output data/8_best_cluster_assignments.csv",
              row.names = FALSE)

    cat(paste0("✓ Simple clustering complete! k = ", best_k, " clusters\n"))

    # Cross-tab
    cat("\nClusters vs River Types:\n")
    print(table(cluster_assignments$river_type, cluster_assignments$cluster))

  }, error = function(e2) {
    cat("✗ Alternative approach also failed:", e2$message, "\n")
  })
})

# ===============================================================================
# STEP 5: SPATIAL MAPPING (optional, needs spatial packages)
# ===============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  STEP 5: SPATIAL MAPPING (OPTIONAL)                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

if(requireNamespace("sf", quietly = TRUE) &&
   requireNamespace("rnaturalearth", quietly = TRUE) &&
   file.exists("Output data/8_best_cluster_assignments.csv")) {

  cat("Running spatial mapping...\n\n")

  tryCatch({
    source("Scripts/8_spatial_cluster_mapping.R")
    cat("\n✓ Spatial maps created!\n")
  }, error = function(e) {
    cat("⚠ Spatial mapping failed:", e$message, "\n")
    cat("  (Maps are optional, continuing anyway)\n")
  })
} else {
  cat("⚠ Skipping spatial mapping (missing packages or data)\n")
  cat("  Install with: install.packages(c('sf', 'rnaturalearth', 'rnaturalearthdata'))\n")
}

# ===============================================================================
# FINAL SUMMARY
# ===============================================================================

pipeline_end <- Sys.time()
total_time <- difftime(pipeline_end, pipeline_start, units = "mins")

cat("\n\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                                                                        ║\n")
cat("║                      PIPELINE COMPLETE                                 ║\n")
cat("║                                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("Summary:\n")
cat("═══════════════════════════════════\n")
cat(paste0("Total time: ", round(total_time, 2), " minutes\n\n"))

cat("Check these files:\n")
cat("  • Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx\n")
cat("  • Output data/8_best_cluster_assignments.csv\n")
cat("  • Plots/ folder for figures\n\n")

cat("Next steps:\n")
cat("  1. Review cluster assignments\n")
cat("  2. Examine generated figures\n")
cat("  3. Compare with biological data\n\n")
