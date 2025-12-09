# ===============================================================================
# LITHUANIAN RIVER TYPOLOGY - ADVANCED ABIOTIC-BASED CLASSIFICATION
# ===============================================================================
#
# Purpose: Explore alternative river classification schemes based on abiotic
#          parameters, integrating spatial patterns, temporal stability, and
#          ecological gradients
#
# Author: Freshwater Ecologist
# Date: 2025-01-04
#
# This script explores alternative river typologies beyond catchment size and
# slope, using comprehensive abiotic data to identify natural groupings that
# may better reflect ecological conditions and biological communities
#
# ===============================================================================

# Load required libraries
library(vegan)         # Multivariate analysis
library(cluster)       # Clustering algorithms
library(dplyr)         # Data manipulation
library(tidyr)         # Data tidying
library(readxl)        # Read Excel files
library(ggplot2)       # Plotting
library(ggdendro)      # Dendrogram plotting
library(factoextra)    # Cluster visualization
library(NbClust)       # Optimal cluster number
library(patchwork)     # Combine plots
library(viridis)       # Color palettes
library(corrplot)      # Correlation plots

# ===============================================================================
# 1. LOAD DATA
# ===============================================================================

cat("Loading data...\n")

# Load site data with interpolated values (from TPRS script)
site_data <- read.csv("Output data/5_site_data_with_interpolations.csv")

# Load original annual means for temporal analysis
annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

cat(paste0("Sites loaded: ", nrow(site_data), "\n"))
cat(paste0("Years covered: ", min(annual_means$year), "-", max(annual_means$year), "\n\n"))

# ===============================================================================
# 2. DEFINE ALTERNATIVE CLASSIFICATION SCHEMES
# ===============================================================================

cat("========================================\n")
cat("DEFINING CLASSIFICATION SCHEMES\n")
cat("========================================\n\n")

# Scheme 1: Geographic/Topographic
geographic_vars <- c("latitude", "longitude", "elevation")

# Scheme 2: Hydrochemical (WFD-relevant, less pollution-sensitive)
hydrochemical_vars <- c("temp_interpolated", "pH_interpolated",
                        "EC_interpolated", "alkalinity_interpolated",
                        "DO_mg_interpolated")

# Scheme 3: Nutrient signature
nutrient_vars <- c("total_N_interpolated", "total_P_interpolated",
                   "NO3_N_interpolated", "PO4_P_interpolated")

# Scheme 4: Hydrological regime
hydro_vars <- c("flow_interpolated", "velocity_interpolated")

# Scheme 5: Combined typology approach (geographic + hydrochemical)
combined_vars <- c(geographic_vars, hydrochemical_vars)

# Scheme 6: Full abiotic suite
all_abiotic_vars <- grep("_interpolated$", names(site_data), value = TRUE)

cat("Classification schemes defined:\n")
cat("  1. Geographic/Topographic (3 variables)\n")
cat("  2. Hydrochemical (5 variables)\n")
cat("  3. Nutrient signature (4 variables)\n")
cat("  4. Hydrological regime (2 variables)\n")
cat("  5. Combined typology (8 variables)\n")
cat("  6. Full abiotic suite (", length(all_abiotic_vars), " variables)\n\n")

# ===============================================================================
# 3. CLUSTERING FUNCTION
# ===============================================================================

#' Perform hierarchical clustering and determine optimal k
#'
#' @param data Data frame
#' @param vars Variable names to use
#' @param scheme_name Name of classification scheme
#' @param max_k Maximum number of clusters to test
#' @return List with clustering results
perform_clustering <- function(data, vars, scheme_name, max_k = 8) {

  cat(paste0("Processing: ", scheme_name, "\n"))

  # Select variables and remove NAs
  cluster_data <- data %>%
    select(site_id, river_type, all_of(vars)) %>%
    na.omit()

  cat(paste0("  Sites with complete data: ", nrow(cluster_data), "\n"))

  if(nrow(cluster_data) < 20) {
    cat("  ⚠ Insufficient data, skipping\n\n")
    return(NULL)
  }

  # Extract matrix
  mat <- cluster_data %>%
    select(all_of(vars)) %>%
    as.matrix()

  # Scale
  mat_scaled <- scale(mat)

  # Distance matrix
  dist_mat <- dist(mat_scaled, method = "euclidean")

  # Hierarchical clustering (Ward's method)
  hc <- hclust(dist_mat, method = "ward.D2")

  # Determine optimal k using multiple methods
  wss <- sapply(2:max_k, function(k) {
    kmeans(mat_scaled, centers = k, nstart = 25)$tot.withinss
  })

  sil <- sapply(2:max_k, function(k) {
    km <- kmeans(mat_scaled, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist_mat)
    mean(ss[, 3])
  })

  # Gap statistic (time-consuming, so use fewer iterations)
  gap_stat <- clusGap(mat_scaled, FUN = kmeans, nstart = 25,
                      K.max = max_k, B = 50)

  optimal_k_sil <- which.max(sil) + 1
  optimal_k_gap <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method = "Tibs2001SEmax")

  cat(paste0("  Optimal k (silhouette): ", optimal_k_sil, "\n"))
  cat(paste0("  Optimal k (gap statistic): ", optimal_k_gap, "\n"))

  # Use silhouette method
  k_final <- optimal_k_sil
  clusters <- cutree(hc, k = k_final)

  # Add clusters to data
  cluster_data$cluster <- clusters

  # Calculate cluster quality metrics
  cluster_stats <- cluster_data %>%
    group_by(cluster) %>%
    summarise(
      n_sites = n(),
      n_river_types = n_distinct(river_type),
      .groups = "drop"
    )

  # PERMANOVA: test if clusters explain environmental variation
  permanova_clusters <- adonis2(mat_scaled ~ cluster, data = cluster_data, permutations = 999)

  # PERMANOVA: test if river types explain environmental variation
  permanova_types <- adonis2(mat_scaled ~ river_type, data = cluster_data, permutations = 999)

  cat(paste0("  PERMANOVA R² (clusters): ", round(permanova_clusters$R2[1], 3), "\n"))
  cat(paste0("  PERMANOVA R² (river types): ", round(permanova_types$R2[1], 3), "\n"))

  # Adjusted Rand Index: agreement between clusters and river types
  ari <- adjustedRandIndex(cluster_data$cluster, cluster_data$river_type)
  cat(paste0("  Adjusted Rand Index: ", round(ari, 3), "\n\n"))

  # Return results
  list(
    scheme_name = scheme_name,
    variables = vars,
    n_sites = nrow(cluster_data),
    hclust = hc,
    distance = dist_mat,
    optimal_k = k_final,
    clusters = clusters,
    cluster_data = cluster_data,
    cluster_stats = cluster_stats,
    wss = wss,
    silhouette = sil,
    gap_stat = gap_stat,
    permanova_clusters = permanova_clusters,
    permanova_types = permanova_types,
    ari = ari,
    scaled_matrix = mat_scaled
  )
}

# ===============================================================================
# 4. APPLY ALL CLASSIFICATION SCHEMES
# ===============================================================================

cat("\n========================================\n")
cat("APPLYING CLASSIFICATION SCHEMES\n")
cat("========================================\n\n")

# Apply clustering to all schemes
schemes <- list(
  Geographic = list(vars = geographic_vars, name = "Geographic/Topographic"),
  Hydrochemical = list(vars = hydrochemical_vars, name = "Hydrochemical"),
  Nutrient = list(vars = nutrient_vars, name = "Nutrient Signature"),
  Hydrological = list(vars = hydro_vars, name = "Hydrological Regime"),
  Combined = list(vars = combined_vars, name = "Combined Typology"),
  Full = list(vars = all_abiotic_vars, name = "Full Abiotic Suite")
)

results <- list()

for(scheme_id in names(schemes)) {
  scheme <- schemes[[scheme_id]]
  results[[scheme_id]] <- perform_clustering(
    site_data,
    scheme$vars,
    scheme$name,
    max_k = 8
  )
}

# ===============================================================================
# 5. COMPARE CLASSIFICATION SCHEMES
# ===============================================================================

cat("\n========================================\n")
cat("COMPARING CLASSIFICATION SCHEMES\n")
cat("========================================\n\n")

# Create comparison table
comparison_df <- data.frame(
  Scheme = sapply(results, function(x) if(!is.null(x)) x$scheme_name else NA),
  N_Sites = sapply(results, function(x) if(!is.null(x)) x$n_sites else NA),
  N_Vars = sapply(results, function(x) if(!is.null(x)) length(x$variables) else NA),
  Optimal_K = sapply(results, function(x) if(!is.null(x)) x$optimal_k else NA),
  R2_Clusters = sapply(results, function(x) if(!is.null(x)) x$permanova_clusters$R2[1] else NA),
  R2_RiverTypes = sapply(results, function(x) if(!is.null(x)) x$permanova_types$R2[1] else NA),
  ARI = sapply(results, function(x) if(!is.null(x)) x$ari else NA)
) %>%
  filter(!is.na(Scheme)) %>%
  mutate(
    R2_Improvement = R2_Clusters - R2_RiverTypes,
    R2_Ratio = R2_Clusters / R2_RiverTypes
  )

print(comparison_df)
cat("\n")

# Find best performing scheme
best_scheme <- comparison_df %>%
  arrange(desc(R2_Clusters)) %>%
  slice(1)

cat(paste0("Best performing scheme: ", best_scheme$Scheme, "\n"))
cat(paste0("  R² = ", round(best_scheme$R2_Clusters, 3), "\n"))
cat(paste0("  Optimal k = ", best_scheme$Optimal_K, "\n\n"))

# ===============================================================================
# 6. VISUALIZE DENDROGRAMS
# ===============================================================================

cat("Creating dendrogram visualizations...\n")

# Function to create nice dendrogram
plot_dendrogram <- function(result) {
  if(is.null(result)) return(NULL)

  # Create dendrogram data
  dend_data <- dendro_data(result$hclust, type = "rectangle")

  # Add river type colors
  labels_df <- dend_data$labels
  labels_df$site_index <- as.numeric(labels_df$label)
  labels_df$river_type <- result$cluster_data$river_type[labels_df$site_index]

  p <- ggplot() +
    geom_segment(data = dend_data$segments,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = labels_df,
               aes(x = x, y = y, color = factor(river_type)),
               size = 2) +
    scale_color_manual(values = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
                       name = "River Type") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0(result$scheme_name, " Dendrogram"),
      subtitle = paste0("k = ", result$optimal_k, " clusters"),
      x = "Sites",
      y = "Height"
    )

  return(p)
}

# Create dendrograms for all schemes
dendro_plots <- lapply(results, plot_dendrogram)
dendro_plots <- dendro_plots[!sapply(dendro_plots, is.null)]

if(length(dendro_plots) > 0) {
  pdf("Plots/Figure7_dendrograms.pdf", width = 16, height = 12)
  print(wrap_plots(dendro_plots, ncol = 2))
  dev.off()

  png("Plots/Figure7_dendrograms.png", width = 3200, height = 2400, res = 200)
  print(wrap_plots(dendro_plots, ncol = 2))
  dev.off()

  cat("✓ Dendrograms saved\n")
}

# ===============================================================================
# 7. RDA ORDINATION FOR BEST SCHEME
# ===============================================================================

cat("\nCreating RDA ordination for best scheme...\n")

# Get best result
best_result <- results[[which(comparison_df$Scheme == best_scheme$Scheme)]]

if(!is.null(best_result)) {

  # Perform RDA
  rda_result <- rda(best_result$scaled_matrix, scale = FALSE)

  # Extract scores
  site_scores <- scores(rda_result, display = "sites", choices = 1:2) %>%
    as.data.frame() %>%
    mutate(
      site_id = best_result$cluster_data$site_id,
      river_type = best_result$cluster_data$river_type,
      cluster = best_result$cluster_data$cluster
    )

  species_scores <- scores(rda_result, display = "species", choices = 1:2) %>%
    as.data.frame() %>%
    mutate(variable = rownames(.))

  # Variance explained
  var_explained <- summary(rda_result)$cont$importance[2, ] * 100

  # Create RDA plot with clusters
  p_rda_cluster <- ggplot() +
    geom_point(data = site_scores,
               aes(x = PC1, y = PC2, color = factor(cluster)),
               size = 3, alpha = 0.7) +
    geom_segment(data = species_scores,
                 aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "darkred", alpha = 0.7) +
    geom_text(data = species_scores,
              aes(x = PC1 * 3.3, y = PC2 * 3.3, label = variable),
              size = 3, color = "darkred", fontface = "bold") +
    scale_color_viridis_d(name = "Cluster") +
    theme_minimal() +
    labs(
      title = paste0("RDA Ordination: ", best_scheme$Scheme),
      subtitle = "Sites colored by abiotic cluster",
      x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2], 1), "%)")
    )

  # Create RDA plot with river types
  p_rda_type <- ggplot() +
    geom_point(data = site_scores,
               aes(x = PC1, y = PC2, color = factor(river_type)),
               size = 3, alpha = 0.7) +
    geom_segment(data = species_scores,
                 aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "darkred", alpha = 0.7) +
    geom_text(data = species_scores,
              aes(x = PC1 * 3.3, y = PC2 * 3.3, label = variable),
              size = 3, color = "darkred", fontface = "bold") +
    scale_color_manual(values = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
                       name = "River Type") +
    theme_minimal() +
    labs(
      title = paste0("RDA Ordination: ", best_scheme$Scheme),
      subtitle = "Sites colored by current river type",
      x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2], 1), "%)")
    )

  pdf("Plots/Figure8_RDA_best_scheme.pdf", width = 14, height = 6)
  print(p_rda_cluster + p_rda_type)
  dev.off()

  png("Plots/Figure8_RDA_best_scheme.png", width = 2800, height = 1200, res = 200)
  print(p_rda_cluster + p_rda_type)
  dev.off()

  cat("✓ RDA ordination saved\n")
}

# ===============================================================================
# 8. CHARACTERIZE NEW CLUSTERS
# ===============================================================================

cat("\n========================================\n")
cat("CHARACTERIZING NEW CLUSTERS\n")
cat("========================================\n\n")

if(!is.null(best_result)) {

  # Calculate mean values for each cluster
  cluster_profiles <- best_result$cluster_data %>%
    select(cluster, all_of(best_result$variables)) %>%
    group_by(cluster) %>%
    summarise(across(everything(), ~mean(., na.rm = TRUE)), .groups = "drop")

  cat("Cluster profiles (mean values):\n")
  print(round(cluster_profiles, 3))
  cat("\n")

  # Cross-tabulation: clusters vs river types
  cat("Cross-tabulation: Clusters vs River Types\n")
  cross_tab <- table(best_result$cluster_data$cluster, best_result$cluster_data$river_type)
  print(cross_tab)
  cat("\n")

  # Chi-square test
  chi_test <- chisq.test(cross_tab)
  cat(paste0("Chi-square test: χ² = ", round(chi_test$statistic, 2),
             ", p = ", format.pval(chi_test$p.value, digits = 3), "\n\n"))

  # Cluster sizes
  cluster_sizes <- table(best_result$cluster_data$cluster)
  cat("Cluster sizes:\n")
  print(cluster_sizes)
  cat("\n")
}

# ===============================================================================
# 9. TEMPORAL STABILITY ANALYSIS
# ===============================================================================

cat("========================================\n")
cat("TEMPORAL STABILITY ANALYSIS\n")
cat("========================================\n\n")

# For each site, calculate coefficient of variation across years
temporal_stability <- annual_means %>%
  filter(data_quality == "complete") %>%
  group_by(site_id) %>%
  summarise(
    n_years = n(),
    cv_temp = sd(`temp[°C]_ann`, na.rm = TRUE) / mean(`temp[°C]_ann`, na.rm = TRUE),
    cv_pH = sd(`pH_ann`, na.rm = TRUE) / mean(`pH_ann`, na.rm = TRUE),
    cv_EC = sd(`EC[µS/cm]_ann`, na.rm = TRUE) / mean(`EC[µS/cm]_ann`, na.rm = TRUE),
    cv_alkalinity = sd(`alkalinity[mmol/l]_ann`, na.rm = TRUE) / mean(`alkalinity[mmol/l]_ann`, na.rm = TRUE),
    cv_total_N = sd(`total_N[mg/L]_ann`, na.rm = TRUE) / mean(`total_N[mg/L]_ann`, na.rm = TRUE),
    cv_total_P = sd(`total_P[mg/L]_ann`, na.rm = TRUE) / mean(`total_P[mg/L]_ann`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("cv_"), ~ifelse(is.infinite(.) | is.nan(.), NA, .)))

cat("Temporal stability (coefficient of variation):\n")
cat("Mean CV across sites:\n")
temporal_summary <- temporal_stability %>%
  select(starts_with("cv_")) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))
print(round(temporal_summary, 3))
cat("\n")

# Join with cluster assignments
if(!is.null(best_result)) {
  temporal_by_cluster <- temporal_stability %>%
    left_join(best_result$cluster_data %>% select(site_id, cluster), by = "site_id") %>%
    filter(!is.na(cluster))

  cat("Temporal stability by cluster:\n")
  stability_by_cluster <- temporal_by_cluster %>%
    group_by(cluster) %>%
    summarise(across(starts_with("cv_"), ~mean(., na.rm = TRUE)), .groups = "drop")
  print(round(stability_by_cluster, 3))
  cat("\n")
}

# ===============================================================================
# 10. SAVE OUTPUTS
# ===============================================================================

cat("========================================\n")
cat("SAVING OUTPUTS\n")
cat("========================================\n\n")

# Save comparison table
write.csv(comparison_df,
          "Output data/7_classification_scheme_comparison.csv",
          row.names = FALSE)
cat("✓ Classification comparison saved\n")

# Save best cluster assignments
if(!is.null(best_result)) {
  best_clusters <- best_result$cluster_data %>%
    select(site_id, river_type, cluster)

  write.csv(best_clusters,
            "Output data/8_best_cluster_assignments.csv",
            row.names = FALSE)
  cat("✓ Best cluster assignments saved\n")

  write.csv(cluster_profiles,
            "Output data/9_cluster_profiles.csv",
            row.names = FALSE)
  cat("✓ Cluster profiles saved\n")
}

# Save temporal stability
write.csv(temporal_stability,
          "Output data/10_temporal_stability.csv",
          row.names = FALSE)
cat("✓ Temporal stability saved\n")

# ===============================================================================
# 11. FINAL SUMMARY
# ===============================================================================

cat("\n\n")
cat("========================================\n")
cat("ADVANCED TYPOLOGY EXPLORATION COMPLETE!\n")
cat("========================================\n\n")

cat("Key findings:\n")
cat(paste0("  • Best classification scheme: ", best_scheme$Scheme, "\n"))
cat(paste0("  • Optimal number of clusters: ", best_scheme$Optimal_K, "\n"))
cat(paste0("  • R² (clusters): ", round(best_scheme$R2_Clusters, 3), "\n"))
cat(paste0("  • R² (current river types): ", round(best_scheme$R2_RiverTypes, 3), "\n"))
cat(paste0("  • Improvement: ", round(best_scheme$R2_Improvement, 3), "\n"))
cat("\nInterpretation:\n")
if(best_scheme$R2_Clusters > best_scheme$R2_RiverTypes) {
  cat("  ✓ Abiotic clusters explain MORE variation than current river types!\n")
  cat("    → Alternative typology may be more ecologically meaningful\n")
} else {
  cat("  → Current river types perform well\n")
  cat("    → Consider refining rather than replacing typology\n")
}
cat("\nNext steps:\n")
cat("  → Validate clusters with biological data (macroinvertebrates, macrophytes)\n")
cat("  → Test if new typology improves bioassessment indices\n")
cat("  → Explore seasonal patterns within clusters\n")
cat("  → Map clusters spatially to identify biogeographic regions\n\n")
