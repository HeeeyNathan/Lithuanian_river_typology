library(vegan)
library(dplyr)
library(readxl)
library(tidyverse)
library(sf)
library(gstat)
library(stars)
library(viridis)
library(tmap)
library(rgeoboundaries)
library(patchwork)
library(sp)

# Load data
annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

# Filter for complete seasonal data
complete_data <- annual_means |> filter(data_quality == "complete")

# Unique sites and their distribution
unique_sites <- complete_data |>
  group_by(site_id, latitude, longitude, elevation) |>
  summarise(
    n_years = n(),
    year_range = paste(min(year), "-", max(year)),
    .groups = "drop"
  )

print(paste("Number of unique monitoring sites:", nrow(unique_sites)))
print(paste("Year range in data:", min(complete_data$year), "-", max(complete_data$year)))

# Check for any missing coordinates
missing_coords <- sum(is.na(unique_sites$latitude) | is.na(unique_sites$longitude))
print(paste("Sites with missing coordinates:", missing_coords))

# Summary of spatial extent
cat("\nSpatial extent of monitoring network:\n")
cat("Latitude range:", range(unique_sites$latitude, na.rm = TRUE), "\n")
cat("Longitude range:", range(unique_sites$longitude, na.rm = TRUE), "\n")
cat("Elevation range:", range(unique_sites$elevation, na.rm = TRUE), "\n")

# Convert to spatial object
sites_sf <- unique_sites |>
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) |>  # WGS84
  st_transform(crs = 3346)  # LKS94 / Lithuania TM - Lithuanian national coordinate system

# Create a simple visualization
library(ggplot2)
ggplot() +
  geom_sf(data = sites_sf, aes(color = n_years), size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Years of\ndata") +
  theme_minimal() +
  labs(title = "Lithuanian River Monitoring Network",
       subtitle = paste("Total sites:", nrow(sites_sf))) +
  theme(legend.position = "right")

# Check spatial clustering
cat("\nSites per number of years sampled:\n")
table(unique_sites$n_years)

# Let's also check the temporal consistency
temporal_coverage <- complete_data |>
  group_by(year) |>
  summarise(
    n_sites = n_distinct(site_id),
    .groups = "drop"
  )
print(temporal_coverage)

# Get Lithuania boundary
lithuania_boundary <- gb_adm0(country = "Lithuania") |>
  st_transform(crs = 3346)  # Transform to LKS94

# Load rivers (if you want to visualize them later)
gdb_path <- "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/Lithuanian data/Geographic data/UETK_2024-05-02.gdb/UETK_2024-05-02.gdb"
rivers <- st_read(gdb_path, layer = "upes_l")

# Prepare temperature data using the existing annual column
temp_data <- complete_data |>
  select(site_id, year, latitude, longitude, elevation, `temp[°C]_ann`) |>
  rename(temp_annual = `temp[°C]_ann`) |>
  filter(!is.na(temp_annual))  # Remove rows with no temperature data

# Check the data
cat("Temperature data summary:\n")
cat("Years with data:", sort(unique(temp_data$year)), "\n")
cat("Sites with temperature data:", n_distinct(temp_data$site_id), "\n")
cat("Total observations:", nrow(temp_data), "\n\n")

# Summary statistics
summary(temp_data$temp_annual)

# Check temporal coverage
temp_coverage <- temp_data|>
  group_by(year)|>
  summarise(
    n_sites = n_distinct(site_id),
    mean_temp = mean(temp_annual, na.rm = TRUE),
    sd_temp = sd(temp_annual, na.rm = TRUE),
    min_temp = min(temp_annual, na.rm = TRUE),
    max_temp = max(temp_annual, na.rm = TRUE),
    .groups = "drop"
  )
print(temp_coverage)

# Visualize spatial distribution of sites with temperature data
temp_sites_sf <- temp_data|>
  group_by(site_id, latitude, longitude)|>
  summarise(
    n_years = n(),
    mean_temp = mean(temp_annual, na.rm = TRUE),
    .groups = "drop"
  )|>
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)|>
  st_transform(crs = 3346)

# Create visualization
ggplot() +
  geom_sf(data = lithuania_boundary, fill = "lightgray", color = "black") +
  geom_sf(data = temp_sites_sf, aes(color = mean_temp), size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Mean Temp\n(°C)") +
  theme_minimal() +
  labs(title = "Monitoring Sites with Temperature Data",
       subtitle = paste("Total sites:", nrow(temp_sites_sf)))

# Create interpolation grid covering Lithuania
# Using a 1km resolution (you can adjust this)
grid_lithuania <- st_make_grid(
  lithuania_boundary,
  cellsize = 1000,  # 1000m grid cells
  what = "centers"
)|>
  st_as_sf()|>
  st_filter(lithuania_boundary)  # Keep only points within Lithuania

cat("Grid points for interpolation:", nrow(grid_lithuania), "\n")

# Function to interpolate temperature for a single year
interpolate_year <- function(year_data, grid, lithuania_boundary) {

  # Convert data to spatial object
  year_sf <- year_data|>
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326)|>
    st_transform(crs = 3346)

  # Extract coordinates for variogram modeling
  coords <- st_coordinates(year_sf)
  year_sf$X <- coords[,1]
  year_sf$Y <- coords[,2]

  # Create and fit variogram
  v_emp <- variogram(temp_annual ~ 1, year_sf,
                     cutoff = 100000,  # 100km cutoff
                     width = 5000)     # 5km bins

  # Fit variogram model (trying multiple models)
  v_models <- list(
    vgm(psill = var(year_sf$temp_annual), "Sph", range = 50000, nugget = 0.1),
    vgm(psill = var(year_sf$temp_annual), "Exp", range = 50000, nugget = 0.1),
    vgm(psill = var(year_sf$temp_annual), "Gau", range = 50000, nugget = 0.1)
  )

  # Fit and select best model based on SSErr
  fits <- lapply(v_models, function(m) fit.variogram(v_emp, m))
  ssErr <- sapply(fits, function(f) attr(f, "SSErr"))
  v_fit <- fits[[which.min(ssErr)]]

  # Add coordinates to grid
  grid_coords <- st_coordinates(grid)
  grid_df <- st_drop_geometry(grid)
  grid_df$X <- grid_coords[,1]
  grid_df$Y <- grid_coords[,2]
  grid_sf <- st_as_sf(grid_df, coords = c("X", "Y"), crs = 3346)

  # Perform kriging
  krig <- krige(temp_annual ~ 1,
                year_sf,
                grid_sf,
                model = v_fit)

  # Add kriged values back to grid
  krig$year <- unique(year_data$year)

  return(list(
    predictions = krig,
    variogram_empirical = v_emp,
    variogram_model = v_fit
  ))
}

# Test with 2023 data (most recent year with good coverage)
temp_2023 <- temp_data|>
  filter(year == 2023)

cat("\nInterpolating temperature for 2023...\n")
result_2023 <- interpolate_year(temp_2023, grid_lithuania, lithuania_boundary)

# Visualize results
# Plot 1: Variogram
var_plot <- plot(result_2023$variogram_empirical,
                 result_2023$variogram_model,
                 main = "Variogram for Temperature (2023)")

# Plot 2: Interpolated surface
interp_plot <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = result_2023$predictions,
          aes(color = var1.pred), size = 0.3) +
  scale_color_viridis_c(name = "Temp (°C)") +
  geom_sf(data = temp_2023|>
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326)|>
            st_transform(3346),
          color = "red", size = 0.5, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Interpolated Temperature 2023",
       subtitle = "Red dots = monitoring sites")

# Plot 3: Prediction variance
var_plot2 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = result_2023$predictions,
          aes(color = var1.var), size = 0.3) +
  scale_color_viridis_c(name = "Variance", option = "magma") +
  theme_minimal() +
  labs(title = "Prediction Variance 2023")

# Display plots
var_plot
interp_plot + var_plot2

# Summary statistics
cat("\nInterpolation results for 2023:\n")
summary(result_2023$predictions$var1.pred)
cat("\nPrediction variance:\n")
summary(result_2023$predictions$var1.var)

# Use the 1km grid we created earlier (or recreate it)
grid_lithuania <- st_make_grid(
  lithuania_boundary,
  cellsize = 1000,  # 1km grid cells
  what = "centers"
) |>
  st_as_sf() |>
  st_filter(lithuania_boundary)

cat("Grid points for interpolation:", nrow(grid_lithuania), "\n\n")

# Interpolation function
interpolate_year_robust <- function(year_data, grid, year_num) {

  cat("  Processing year", year_num, "...")

  # Convert data to spatial object
  year_sf <- year_data |>
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) |>
    st_transform(crs = 3346)

  # Extract coordinates
  coords <- st_coordinates(year_sf)
  year_sf$X <- coords[,1]
  year_sf$Y <- coords[,2]

  # Create empirical variogram with optimized parameters
  v_emp <- variogram(temp_annual ~ 1, year_sf,
                     cutoff = 80000,   # Reduced from 100km to 80km
                     width = 4000)     # Smaller bins (4km instead of 5km)

  # Calculate initial values based on empirical variogram
  max_gamma <- max(v_emp$gamma)
  range_init <- v_emp$dist[which.max(v_emp$gamma > 0.95 * max_gamma)][1]
  if(is.na(range_init)) range_init <- 40000

  # Try simpler models with better initial values
  v_fit <- tryCatch({
    # Try exponential model first (often most stable)
    vgm_exp <- vgm(psill = 0.8 * var(year_sf$temp_annual),
                   "Exp",
                   range = range_init/3,  # Exp uses practical range
                   nugget = 0.2 * var(year_sf$temp_annual))
    fit.variogram(v_emp, vgm_exp)
  }, error = function(e) {
    # If that fails, try spherical with fixed nugget
    vgm_sph <- vgm(psill = var(year_sf$temp_annual),
                   "Sph",
                   range = range_init,
                   nugget = 0.1)
    fit.variogram(v_emp, vgm_sph, fit.sills = FALSE)
  })

  # Add coordinates to grid
  grid_coords <- st_coordinates(grid)
  grid_df <- st_drop_geometry(grid)
  grid_df$X <- grid_coords[,1]
  grid_df$Y <- grid_coords[,2]
  grid_sf <- st_as_sf(grid_df, coords = c("X", "Y"), crs = 3346)

  # Perform kriging
  krig <- krige(temp_annual ~ 1,
                year_sf,
                grid_sf,
                model = v_fit,
                debug.level = 0)

  cat(" Done! (Model:", v_fit$model[2], ")\n")

  # Return predictions
  return(krig$var1.pred)
}

# Re-run the interpolation with the improved function
cat("Re-running interpolation with improved variogram fitting...\n\n")

all_years <- sort(unique(temp_data$year))
temp_predictions <- list()

start_time <- Sys.time()

for(yr in all_years) {
  year_data <- temp_data |> filter(year == yr)
  temp_predictions[[as.character(yr)]] <- interpolate_year_robust(year_data, grid_lithuania, yr)
}

end_time <- Sys.time()
cat("\nTotal processing time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# Continue with the same processing as before
temp_matrix <- do.call(cbind, temp_predictions)
colnames(temp_matrix) <- all_years

# Calculate long-term statistics
cat("\nCalculating long-term statistics...\n")
grid_lithuania$temp_mean <- rowMeans(temp_matrix, na.rm = TRUE)
grid_lithuania$temp_sd <- apply(temp_matrix, 1, sd, na.rm = TRUE)
grid_lithuania$temp_min <- apply(temp_matrix, 1, min, na.rm = TRUE)
grid_lithuania$temp_max <- apply(temp_matrix, 1, max, na.rm = TRUE)
grid_lithuania$temp_cv <- grid_lithuania$temp_sd / grid_lithuania$temp_mean * 100
grid_lithuania$temp_range <- grid_lithuania$temp_max - grid_lithuania$temp_min

# Add individual years
for(yr in all_years) {
  grid_lithuania[[paste0("temp_", yr)]] <- temp_matrix[, as.character(yr)]
}

# Check for any unusual values
cat("\nQuality check of interpolated values:\n")
cat("Mean temperature range:", range(grid_lithuania$temp_mean), "\n")
cat("Any NA values in mean:", sum(is.na(grid_lithuania$temp_mean)), "\n")
cat("Unrealistic values (< 5°C or > 15°C):",
    sum(grid_lithuania$temp_mean < 5 | grid_lithuania$temp_mean > 15, na.rm = TRUE), "\n")

# Create final visualizations
p1 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_lithuania, aes(color = temp_mean), size = 0.3) +
  scale_color_viridis_c(name = "Mean\nTemp (°C)", limits = c(8, 12)) +
  theme_minimal() +
  labs(title = "Long-term Mean Temperature (2009-2023)",
       subtitle = paste("Based on", length(unique(temp_data$site_id)), "monitoring sites"))

p2 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_lithuania, aes(color = temp_cv), size = 0.3) +
  scale_color_viridis_c(name = "CV (%)", option = "magma") +
  theme_minimal() +
  labs(title = "Temperature Variability (CV%)",
       subtitle = "Inter-annual coefficient of variation")

p3 <- p1 + p2

# Save outputs
## save image
ggsave(p3,
       filename = "Plots/Figure4_temperature_interpolation_summary.png",
       width = 12, height = 6, dpi = 450, bg = "white")
## save rds file
saveRDS(grid_lithuania, "Output data/4_temperature_interpolation_2009-2023.rds")
