library(vegan)
library(dplyr)
library(readxl)
library(tidyverse)
library(cluster)

# Load data
annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

# Filter for complete seasonal data
complete_data <- annual_means |> filter(data_quality == "complete")

# Prepare data - aggregate to site level (mean across years) to avoid pseudoreplication
site_data <- complete_data|>
  group_by(site_id)|>
  summarise(
    # Environmental variables (annual means)
    flow = mean(`flow[m3/s]_ann`, na.rm = TRUE),
    velocity = mean(`velocity[m/s]_ann`, na.rm = TRUE),
    suspended_solids = mean(`suspended_solids[mg/l]_ann`, na.rm = TRUE),
    pH = mean(`pH_ann`, na.rm = TRUE),
    temp = mean(`temp[°C]_ann`, na.rm = TRUE),
    DO_mg = mean(`D.O.[mg/l]_ann`, na.rm = TRUE),
    DO_pct = mean(`D.O.[%]_ann`, na.rm = TRUE),
    BOD7 = mean(`BOD7[mg/L]_ann`, na.rm = TRUE),
    ChDS_C = mean(`ChDS_C[mg/L]_ann`, na.rm = TRUE),
    NH4_N = mean(`NH4_N[mg/L]_ann`, na.rm = TRUE),
    NO2_N = mean(`NO2_N[mg/L]_ann`, na.rm = TRUE),
    NO3_N = mean(`NO3_N[mg/L]_ann`, na.rm = TRUE),
    mineral_N = mean(`mineral_N[mg/L]_ann`, na.rm = TRUE),
    total_N = mean(`total_N[mg/L]_ann`, na.rm = TRUE),
    PO4_P = mean(`PO4_P[mg/L]_ann`, na.rm = TRUE),
    total_P = mean(`total_P[mg/L]_ann`, na.rm = TRUE),
    EC = mean(`EC[µS/cm]_ann`, na.rm = TRUE),
    alkalinity = mean(`alkalinity[mmol/l]_ann`, na.rm = TRUE),

    # Geographic and topographic variables
    latitude = last(latitude),
    longitude = last(longitude),
    elevation = last(elevation),

    # Site metadata
    river_type = last(river_type),
    river_basin = last(river_basin),
    modification_state = last(modification_state),
    .groups = "drop"
  ) |>
  # Convert NaN values to NA
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# Extract environmental matrix (including geographic variables)
env_matrix <- site_data|>
  select(c(flow:elevation))|>
  as.matrix()

# Keep only complete cases
complete_cases <- complete.cases(env_matrix)
env_matrix_clean <- env_matrix[complete_cases, ]
site_data_clean <- site_data[complete_cases, ]

# Extract ONLY typology-relevant environmental variables
typology_vars <- c(
  # Geographic/topographic - fundamental drivers
  "latitude", "longitude", "elevation",
  # Physical flow characteristics
  "flow", "velocity",
  # Basic water chemistry (less influenced by pollution)
  "temp", "pH", "DO_mg", "EC", "alkalinity"
)

# Create typology-focused environmental matrix
env_matrix_typology <- site_data_clean %>%
  select(all_of(typology_vars)) %>%
  as.matrix()

# Check for complete cases
complete_typology <- complete.cases(env_matrix_typology)
env_matrix_typ_clean <- env_matrix_typology[complete_typology, ]
site_data_typ_clean <- site_data_clean[complete_typology, ]

# =============================================================================
# CLUSTER ANALYSIS - TYPOLOGY VARIABLES ONLY
# =============================================================================

# Scale the data for clustering
env_typ_scaled <- scale(env_matrix_typ_clean)

# Calculate distance matrix
env_typ_dist <- dist(env_typ_scaled, method = "euclidean")

# Hierarchical clustering
hclust_typ_result <- hclust(env_typ_dist, method = "ward.D2")

# Determine optimal number of clusters
wss_typ <- sapply(1:8, function(k) {
  kmeans(env_typ_scaled, centers = k, nstart = 10)$tot.withinss
})

sil_width_typ <- sapply(2:8, function(k) {
  km <- kmeans(env_typ_scaled, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, env_typ_dist)
  mean(ss[, 3])
})

# Plot cluster diagnostics
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(hclust_typ_result, main = "Hierarchical Clustering\n(Typology Variables Only)",
     xlab = "Sites", sub = "", cex = 0.6)

plot(1:8, wss_typ, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K", ylab = "Total WSS",
     main = "Elbow Method - Typology Variables")

plot(2:8, sil_width_typ, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K", ylab = "Average silhouette width",
     main = "Silhouette Method - Typology Variables")

# Cut dendrogram
best_k_typ <- which.max(sil_width_typ) + 1
clusters_typ <- cutree(hclust_typ_result, k = best_k_typ)
site_data_typ_clean$cluster_typ <- clusters_typ

par(mfrow = c(1, 1))

# =============================================================================
# RDA WITH TYPOLOGY VARIABLES
# =============================================================================
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
# Perform RDA with typology variables only
rda_typ_result <- rda(env_matrix_typ_clean, scale = TRUE)

# Plot comparison
plot(rda_typ_result, type = "n", main = "RDA - Typology Variables\nRiver Types (color) vs Clusters (shape)")
colors_type <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")[site_data_typ_clean$river_type]
shapes_cluster <- c(15, 16, 17, 18, 19, 8, 9, 10)[site_data_typ_clean$cluster_typ]
points(rda_typ_result, display = "sites", col = colors_type, pch = shapes_cluster, cex = 1.2)

# Add environmental vectors
species_scores_typ <- scores(rda_typ_result, display = "species", choices = 1:2)
geo_vars <- c("latitude", "longitude", "elevation")
geo_idx <- rownames(species_scores_typ) %in% geo_vars

# Geographic vectors in blue
arrows(0, 0, species_scores_typ[geo_idx, 1], species_scores_typ[geo_idx, 2],
       length = 0.1, col = "blue", lwd = 2)
text(species_scores_typ[geo_idx, ] * 1.1, labels = rownames(species_scores_typ)[geo_idx],
     col = "blue", cex = 0.8, font = 2)

# Physical/chemical vectors in red
arrows(0, 0, species_scores_typ[!geo_idx, 1], species_scores_typ[!geo_idx, 2],
       length = 0.1, col = "red", lwd = 1.5)
text(species_scores_typ[!geo_idx, ] * 1.1, labels = rownames(species_scores_typ)[!geo_idx],
     col = "red", cex = 0.8)

legend("topright", legend = paste("Type", 1:5),
       col = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
       pch = 19, cex = 0.6, title = "River Types")
legend("topleft", legend = paste("Cluster", 1:best_k_typ),
       pch = c(15, 16, 17, 18, 19, 8, 9, 10)[1:best_k_typ],
       col = "black", cex = 0.6, title = "Typology Clusters")

par(mfrow = c(1, 1))

# =============================================================================
# ANALYSIS RESULTS
# =============================================================================

print("=== TYPOLOGY-FOCUSED CLUSTER ANALYSIS ===")
print(paste("Optimal number of clusters (typology variables):", best_k_typ))
print(paste("Variables used:", paste(typology_vars, collapse = ", ")))

# Compare river types vs typology clusters
print("\nRiver Types vs Typology Clusters:")
print(table(site_data_typ_clean$river_type, site_data_typ_clean$cluster_typ))

# Characterize clusters
cluster_typ_means <- site_data_typ_clean %>%
  select(all_of(typology_vars), cluster_typ) %>%
  group_by(cluster_typ) %>%
  summarise(across(all_of(typology_vars), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

print("\nTypology cluster characterization:")
print(round(cluster_typ_means, 3))

# Test statistical significance
print("\nPERMANOVA - typology clusters:")
adonis_typ_clusters <- adonis2(env_matrix_typ_clean ~ cluster_typ, data = site_data_typ_clean, permutations = 999)
print(adonis_typ_clusters)

print("\nPERMANOVA - river types (typology variables):")
adonis_typ_types <- adonis2(env_matrix_typ_clean ~ river_type, data = site_data_typ_clean, permutations = 999)
print(adonis_typ_types)

print("\nRDA Summary - Typology Variables:")
summary(rda_typ_result)

# =============================================================================
# TEST MULTIPLE CLUSTER SOLUTIONS
# =============================================================================

# Test cluster solutions from 2 to 8 clusters
cluster_range <- 2:8
cluster_solutions <- list()

# Generate all cluster solutions
for(k in cluster_range) {
  clusters_k <- cutree(hclust_typ_result, k = k)
  site_data_typ_clean[[paste0("cluster_", k)]] <- clusters_k
  cluster_solutions[[paste0("k", k)]] <- clusters_k
}

# =============================================================================
# VISUAL COMPARISON OF CLUSTER SOLUTIONS
# =============================================================================

# Create a large plot comparing all cluster solutions
par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))

# Plot 1: River types (reference)
plot(rda_typ_result, type = "n", main = "Current River Types")
colors_type <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")[site_data_typ_clean$river_type]
points(rda_typ_result, display = "sites", col = colors_type, pch = 19, cex = 0.8)
legend("topright", legend = paste("Type", 1:5),
       col = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
       pch = 19, cex = 0.5)

# Plot 2-8: Different cluster solutions
for(k in cluster_range) {
  plot(rda_typ_result, type = "n", main = paste(k, "Clusters"))
  colors_k <- rainbow(k)[site_data_typ_clean[[paste0("cluster_", k)]]]
  points(rda_typ_result, display = "sites", col = colors_k, pch = 19, cex = 0.8)
  legend("topright", legend = paste("C", 1:k), col = rainbow(k), pch = 19, cex = 0.5)
}

# Plot diagnostic metrics
plot(1:8, wss_typ, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters", ylab = "Total WSS", main = "Elbow Plot")
abline(v = cluster_range, col = "gray", lty = 2)

plot(2:8, sil_width_typ, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters", ylab = "Silhouette Width", main = "Silhouette Plot")
abline(v = cluster_range, col = "gray", lty = 2)
abline(v = which.max(sil_width_typ) + 1, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# =============================================================================
# DETAILED COMPARISON TABLE
# =============================================================================

print("=== CLUSTER SOLUTION COMPARISON ===")

# Cross-tabulation of river types vs different cluster numbers
for(k in cluster_range) {
  cat("\n", k, "CLUSTERS vs RIVER TYPES:\n")
  cross_tab <- table(site_data_typ_clean$river_type, site_data_typ_clean[[paste0("cluster_", k)]])
  print(cross_tab)

  # Calculate how well clusters separate river types (adjusted rand index)
  ari <- adjustedRandIndex(site_data_typ_clean$river_type, site_data_typ_clean[[paste0("cluster_", k)]])
  cat("Adjusted Rand Index with river types:", round(ari, 3), "\n")
}

# =============================================================================
# PERMANOVA COMPARISON
# =============================================================================

print("\n=== PERMANOVA COMPARISON ===")

# Test explanatory power of different cluster solutions
permanova_results <- data.frame()
