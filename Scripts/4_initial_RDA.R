library(vegan)
library(dplyr)
library(readxl)
library(tidyverse)


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

# Perform RDA (unconstrained = PCA-like)
rda_result <- rda(env_matrix_clean, scale = TRUE)

# Create ordination plot
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Plot 1: Sites colored by river type
plot(rda_result, type = "n", main = "RDA - Sites by River Type\n(Including Geographic Variables)")
colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")[site_data$river_type]
points(rda_result, display = "sites", col = colors, pch = 19, cex = 1.2)

# Add environmental vectors - color code them
species_scores <- scores(rda_result, display = "species", choices = 1:2)
# Geographic variables in blue
geo_vars <- c("latitude", "longitude", "elevation")
geo_idx <- rownames(species_scores) %in% geo_vars
text(species_scores[geo_idx, ], labels = rownames(species_scores)[geo_idx],
     col = "blue", cex = 0.7, font = 2)
# Chemical/physical variables in red
text(species_scores[!geo_idx, ], labels = rownames(species_scores)[!geo_idx],
     col = "red", cex = 0.7)

legend("topright", legend = paste("Type", 1:5),
       col = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
       pch = 19, cex = 0.8)

# Plot 2: Environmental vectors only
plot(rda_result, type = "n", main = "Environmental Vectors\n(Blue = Geographic, Red = Chemical)")
# Geographic vectors in blue
arrows(0, 0,
       species_scores[geo_idx, 1], species_scores[geo_idx, 2],
       length = 0.1, col = "blue", lwd = 2)
text(species_scores[geo_idx, ] * 1.1, labels = rownames(species_scores)[geo_idx],
     col = "blue", cex = 0.8, font = 2)

# Chemical/physical vectors in red
arrows(0, 0,
       species_scores[!geo_idx, 1], species_scores[!geo_idx, 2],
       length = 0.1, col = "red", lwd = 1.5)
text(species_scores[!geo_idx, ] * 1.1, labels = rownames(species_scores)[!geo_idx],
     col = "red", cex = 0.7)

par(mfrow = c(1, 1))

# Summary
summary(rda_result)

# Sites per river type
table(site_data$river_type)

# Check correlations between geographic and environmental variables
cor_matrix <- cor(env_matrix[, c("latitude", "longitude", "elevation", "temp", "flow", "total_N", "total_P")])
print(round(cor_matrix, 3))

