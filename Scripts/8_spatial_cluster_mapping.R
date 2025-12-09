# ===============================================================================
# LITHUANIAN RIVER TYPOLOGY - SPATIAL CLUSTER MAPPING
# ===============================================================================
#
# Purpose: Create comprehensive maps showing cluster assignments and river types
#          across Lithuania's geographic extent
#
# Author: Freshwater Ecologist
# Date: 2025-01-04
#
# This script creates publication-quality maps visualizing the spatial
# distribution of river clusters and comparing them with current river types
#
# ===============================================================================

# Load required libraries
library(ggplot2)       # Plotting
library(dplyr)         # Data manipulation
library(sf)            # Spatial data
library(rnaturalearth) # Country boundaries
library(rnaturalearthdata)
library(viridis)       # Color palettes
library(patchwork)     # Combine plots
library(ggspatial)     # Scale bars and north arrows
library(readxl)        # Read Excel files

# ===============================================================================
# 1. LOAD DATA
# ===============================================================================

cat("Loading data...\n")

# Load site data with spatial coordinates
site_data <- read.csv("Output data/5_site_data_with_interpolations.csv")

# Load cluster assignments
cluster_assignments <- read.csv("Output data/8_best_cluster_assignments.csv")

# Load interpolated grid for background
tprs_grid <- read.csv("Output data/4_TPRS_interpolated_grid.csv")

# Merge
map_data <- site_data %>%
  left_join(cluster_assignments, by = c("site_id", "river_type"))

cat(paste0("Sites loaded: ", nrow(map_data), "\n"))
cat(paste0("Clusters: ", n_distinct(map_data$cluster, na.rm = TRUE), "\n\n"))

# ===============================================================================
# 2. GET LITHUANIA BOUNDARY
# ===============================================================================

cat("Downloading Lithuania boundary...\n")

# Get Lithuania boundary from Natural Earth
lithuania <- ne_countries(scale = "medium", country = "Lithuania", returnclass = "sf")

# Get neighboring countries for context
neighbors <- ne_countries(scale = "medium",
                          country = c("Latvia", "Belarus", "Poland", "Russia"),
                          returnclass = "sf")

cat("✓ Boundaries loaded\n\n")

# ===============================================================================
# 3. PREPARE SPATIAL DATA
# ===============================================================================

# Convert site data to sf object
sites_sf <- st_as_sf(map_data,
                     coords = c("longitude", "latitude"),
                     crs = 4326)  # WGS84

# Get bounding box for Lithuania
bbox <- st_bbox(lithuania)

# Extend bbox slightly
bbox_extended <- c(
  xmin = bbox["xmin"] - 0.2,
  xmax = bbox["xmax"] + 0.2,
  ymin = bbox["ymin"] - 0.1,
  ymax = bbox["ymax"] + 0.1
)

# ===============================================================================
# 4. CREATE BASE MAP THEME
# ===============================================================================

theme_map <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ===============================================================================
# 5. MAP 1: CURRENT RIVER TYPES
# ===============================================================================

cat("Creating map 1: Current river types...\n")

# Define colors for river types
river_type_colors <- c(
  "1" = "#E31A1C",  # Red
  "2" = "#1F78B4",  # Blue
  "3" = "#33A02C",  # Green
  "4" = "#FF7F00",  # Orange
  "5" = "#6A3D9A"   # Purple
)

map_river_types <- ggplot() +
  # Lithuania boundary
  geom_sf(data = lithuania, fill = "gray95", color = "black", linewidth = 0.5) +
  # Neighboring countries
  geom_sf(data = neighbors, fill = "gray98", color = "gray70", linewidth = 0.3) +
  # Site points colored by river type
  geom_sf(data = sites_sf, aes(color = factor(river_type)), size = 3, alpha = 0.8) +
  scale_color_manual(
    values = river_type_colors,
    name = "River Type",
    labels = c("Type 1 (small)", "Type 2 (medium, steep)", "Type 3 (medium, flat)",
               "Type 4 (large, steep)", "Type 5 (large, flat)"),
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(bbox_extended["xmin"], bbox_extended["xmax"]),
           ylim = c(bbox_extended["ymin"], bbox_extended["ymax"])) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  labs(
    title = "Current Lithuanian River Typology",
    subtitle = "Based on catchment size and slope",
    x = "Longitude",
    y = "Latitude"
  )

cat("✓ Map 1 complete\n")

# ===============================================================================
# 6. MAP 2: NEW ABIOTIC CLUSTERS
# ===============================================================================

cat("Creating map 2: Abiotic clusters...\n")

# Count clusters
n_clusters <- n_distinct(sites_sf$cluster, na.rm = TRUE)

map_clusters <- ggplot() +
  # Lithuania boundary
  geom_sf(data = lithuania, fill = "gray95", color = "black", linewidth = 0.5) +
  # Neighboring countries
  geom_sf(data = neighbors, fill = "gray98", color = "gray70", linewidth = 0.3) +
  # Site points colored by cluster
  geom_sf(data = sites_sf %>% filter(!is.na(cluster)),
          aes(color = factor(cluster)), size = 3, alpha = 0.8) +
  scale_color_viridis_d(name = "Abiotic\nCluster", option = "turbo") +
  coord_sf(xlim = c(bbox_extended["xmin"], bbox_extended["xmax"]),
           ylim = c(bbox_extended["ymin"], bbox_extended["ymax"])) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  labs(
    title = "Alternative River Typology",
    subtitle = "Based on abiotic characteristics (hydrochemical + geographic)",
    x = "Longitude",
    y = "Latitude"
  )

cat("✓ Map 2 complete\n")

# ===============================================================================
# 7. MAP 3: BACKGROUND ENVIRONMENTAL GRADIENT (e.g., TEMPERATURE)
# ===============================================================================

cat("Creating map 3: Environmental gradient map...\n")

# Get temperature interpolated surface
temp_surface <- tprs_grid %>%
  filter(variable == "temp")

map_temp_gradient <- ggplot() +
  # Interpolated temperature surface
  geom_raster(data = temp_surface, aes(x = longitude, y = latitude, fill = predicted),
              interpolate = TRUE, alpha = 0.7) +
  # Lithuania boundary
  geom_sf(data = lithuania, fill = NA, color = "black", linewidth = 0.7) +
  # Site points
  geom_sf(data = sites_sf %>% filter(!is.na(cluster)),
          aes(shape = factor(cluster)), size = 2.5, color = "white", stroke = 0.8) +
  scale_fill_viridis_c(name = "Temperature\n(°C)", option = "plasma") +
  scale_shape_manual(name = "Abiotic\nCluster", values = c(21, 22, 23, 24, 25, 3, 4, 8)[1:n_clusters]) +
  coord_sf(xlim = c(bbox_extended["xmin"], bbox_extended["xmax"]),
           ylim = c(bbox_extended["ymin"], bbox_extended["ymax"])) +
  annotation_scale(location = "bl", width_hint = 0.2, text_col = "white", line_col = "white") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  labs(
    title = "Temperature Gradient with Cluster Overlay",
    subtitle = "TPRS interpolated temperature surface",
    x = "Longitude",
    y = "Latitude"
  )

cat("✓ Map 3 complete\n")

# ===============================================================================
# 8. MAP 4: ALKALINITY GRADIENT (KEY TYPOLOGY VARIABLE)
# ===============================================================================

cat("Creating map 4: Alkalinity gradient map...\n")

# Get alkalinity interpolated surface
alkalinity_surface <- tprs_grid %>%
  filter(variable == "alkalinity")

if(nrow(alkalinity_surface) > 0) {
  map_alkalinity_gradient <- ggplot() +
    # Interpolated alkalinity surface
    geom_raster(data = alkalinity_surface, aes(x = longitude, y = latitude, fill = predicted),
                interpolate = TRUE, alpha = 0.7) +
    # Lithuania boundary
    geom_sf(data = lithuania, fill = NA, color = "black", linewidth = 0.7) +
    # Site points
    geom_sf(data = sites_sf %>% filter(!is.na(cluster)),
            aes(shape = factor(cluster)), size = 2.5, color = "white", stroke = 0.8) +
    scale_fill_viridis_c(name = "Alkalinity\n(mmol/L)", option = "mako", direction = -1) +
    scale_shape_manual(name = "Abiotic\nCluster", values = c(21, 22, 23, 24, 25, 3, 4, 8)[1:n_clusters]) +
    coord_sf(xlim = c(bbox_extended["xmin"], bbox_extended["xmax"]),
             ylim = c(bbox_extended["ymin"], bbox_extended["ymax"])) +
    annotation_scale(location = "bl", width_hint = 0.2, text_col = "white", line_col = "white") +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    theme_map() +
    labs(
      title = "Alkalinity Gradient with Cluster Overlay",
      subtitle = "TPRS interpolated alkalinity surface",
      x = "Longitude",
      y = "Latitude"
    )

  cat("✓ Map 4 complete\n")
} else {
  map_alkalinity_gradient <- NULL
  cat("⚠ Alkalinity data not available\n")
}

# ===============================================================================
# 9. MAP 5: CLUSTER SIZE REPRESENTATION
# ===============================================================================

cat("Creating map 5: Cluster composition...\n")

# Calculate cluster sizes
cluster_sizes <- sites_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    n_sites = n(),
    mean_lat = mean(latitude, na.rm = TRUE),
    mean_lon = mean(longitude, na.rm = TRUE)
  )

# Create bubble map showing cluster centroids
map_cluster_size <- ggplot() +
  # Lithuania boundary
  geom_sf(data = lithuania, fill = "gray95", color = "black", linewidth = 0.5) +
  # All sites (small)
  geom_sf(data = sites_sf %>% filter(!is.na(cluster)),
          aes(color = factor(cluster)), size = 1.5, alpha = 0.3) +
  # Cluster centroids (large)
  geom_point(data = cluster_sizes,
             aes(x = mean_lon, y = mean_lat, size = n_sites, fill = factor(cluster)),
             shape = 21, color = "black", stroke = 1, alpha = 0.7) +
  scale_color_viridis_d(name = "Cluster", option = "turbo", guide = "none") +
  scale_fill_viridis_d(name = "Cluster", option = "turbo") +
  scale_size_continuous(name = "Number\nof Sites", range = c(5, 20)) +
  coord_sf(xlim = c(bbox_extended["xmin"], bbox_extended["xmax"]),
           ylim = c(bbox_extended["ymin"], bbox_extended["ymax"])) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  labs(
    title = "Cluster Centroids and Sizes",
    subtitle = "Bubble size represents number of sites per cluster",
    x = "Longitude",
    y = "Latitude"
  )

cat("✓ Map 5 complete\n")

# ===============================================================================
# 10. MAP 6: SPATIAL OUTLIERS
# ===============================================================================

cat("Creating map 6: Spatial outliers...\n")

# Identify sites with spatial outlier status
sites_outliers <- sites_sf %>%
  mutate(outlier_status = ifelse(spatial_outlier, "Outlier", "Typical"))

map_outliers <- ggplot() +
  # Lithuania boundary
  geom_sf(data = lithuania, fill = "gray95", color = "black", linewidth = 0.5) +
  # All sites
  geom_sf(data = sites_outliers %>% filter(outlier_status == "Typical"),
          color = "gray70", size = 2, alpha = 0.5) +
  # Outlier sites
  geom_sf(data = sites_outliers %>% filter(outlier_status == "Outlier"),
          aes(color = factor(river_type)), size = 4, shape = 17) +
  scale_color_manual(
    values = river_type_colors,
    name = "River Type",
    na.translate = FALSE
  ) +
  coord_sf(xlim = c(bbox_extended["xmin"], bbox_extended["xmax"]),
           ylim = c(bbox_extended["ymin"], bbox_extended["ymax"])) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_map() +
  labs(
    title = "Spatial Outliers",
    subtitle = "Sites deviating from expected spatial patterns (triangles)",
    x = "Longitude",
    y = "Latitude"
  )

cat("✓ Map 6 complete\n")

# ===============================================================================
# 11. COMBINE AND SAVE MAPS
# ===============================================================================

cat("\nSaving maps...\n")

# Save individual maps
ggsave("Plots/Figure9a_current_river_types.pdf", map_river_types, width = 10, height = 8)
ggsave("Plots/Figure9a_current_river_types.png", map_river_types, width = 10, height = 8, dpi = 300)

ggsave("Plots/Figure9b_abiotic_clusters.pdf", map_clusters, width = 10, height = 8)
ggsave("Plots/Figure9b_abiotic_clusters.png", map_clusters, width = 10, height = 8, dpi = 300)

ggsave("Plots/Figure9c_temp_gradient.pdf", map_temp_gradient, width = 10, height = 8)
ggsave("Plots/Figure9c_temp_gradient.png", map_temp_gradient, width = 10, height = 8, dpi = 300)

if(!is.null(map_alkalinity_gradient)) {
  ggsave("Plots/Figure9d_alkalinity_gradient.pdf", map_alkalinity_gradient, width = 10, height = 8)
  ggsave("Plots/Figure9d_alkalinity_gradient.png", map_alkalinity_gradient, width = 10, height = 8, dpi = 300)
}

ggsave("Plots/Figure9e_cluster_sizes.pdf", map_cluster_size, width = 10, height = 8)
ggsave("Plots/Figure9e_cluster_sizes.png", map_cluster_size, width = 10, height = 8, dpi = 300)

ggsave("Plots/Figure9f_spatial_outliers.pdf", map_outliers, width = 10, height = 8)
ggsave("Plots/Figure9f_spatial_outliers.png", map_outliers, width = 10, height = 8, dpi = 300)

cat("✓ Individual maps saved\n")

# Create combined comparison figure
combined_comparison <- map_river_types + map_clusters +
  plot_annotation(
    title = "Comparison: Current vs. Alternative River Typology",
    subtitle = "Lithuanian rivers classified by (A) current system vs (B) abiotic characteristics",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("Plots/Figure9_typology_comparison.pdf", combined_comparison, width = 18, height = 8)
ggsave("Plots/Figure9_typology_comparison.png", combined_comparison, width = 18, height = 8, dpi = 300)

cat("✓ Combined comparison saved\n")

# Create multi-panel gradient figure
if(!is.null(map_alkalinity_gradient)) {
  gradient_panel <- map_temp_gradient + map_alkalinity_gradient +
    plot_annotation(
      title = "Environmental Gradients across Lithuania",
      subtitle = "Key abiotic variables with cluster overlay",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )

  ggsave("Plots/Figure10_environmental_gradients.pdf", gradient_panel, width = 18, height = 8)
  ggsave("Plots/Figure10_environmental_gradients.png", gradient_panel, width = 18, height = 8, dpi = 300)

  cat("✓ Gradient panel saved\n")
}

# ===============================================================================
# 12. SUMMARY STATISTICS BY REGION
# ===============================================================================

cat("\n========================================\n")
cat("SPATIAL PATTERN SUMMARY\n")
cat("========================================\n\n")

# Calculate regional statistics
if(!is.na(sites_sf$cluster[1])) {
  regional_stats <- sites_sf %>%
    st_drop_geometry() %>%
    filter(!is.na(cluster)) %>%
    group_by(cluster) %>%
    summarise(
      n_sites = n(),
      mean_longitude = mean(longitude, na.rm = TRUE),
      mean_latitude = mean(latitude, na.rm = TRUE),
      sd_longitude = sd(longitude, na.rm = TRUE),
      sd_latitude = sd(latitude, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      spatial_spread = sqrt(sd_longitude^2 + sd_latitude^2),
      region = case_when(
        mean_longitude < 24 ~ "Western",
        mean_longitude >= 24 & mean_longitude < 25 ~ "Central",
        TRUE ~ "Eastern"
      )
    )

  cat("Cluster spatial characteristics:\n")
  print(regional_stats %>% select(cluster, n_sites, region, spatial_spread))
  cat("\n")

  # Distribution of clusters across regions
  cat("Cluster distribution by geographic region:\n")
  print(table(regional_stats$cluster, regional_stats$region))
  cat("\n")
}

cat("========================================\n")
cat("SPATIAL MAPPING COMPLETE!\n")
cat("========================================\n\n")

cat("Maps generated:\n")
cat("  • Figure 9a: Current river types\n")
cat("  • Figure 9b: Abiotic clusters\n")
cat("  • Figure 9c: Temperature gradient\n")
cat("  • Figure 9d: Alkalinity gradient\n")
cat("  • Figure 9e: Cluster sizes\n")
cat("  • Figure 9f: Spatial outliers\n")
cat("  • Figure 9: Combined comparison\n")
cat("  • Figure 10: Environmental gradients panel\n\n")
