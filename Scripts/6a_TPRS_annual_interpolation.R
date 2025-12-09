# ===============================================================================
# LITHUANIAN RIVER TYPOLOGY - ANNUAL THIN PLATE REGRESSION SPLIE (TPRS) SPATIAL INTERPOLATION
# ===============================================================================
#
# Purpose: Create annual thin plate regression spline interpolations, then
#          average across years to obtain temporally robust spatial estimates.
#
# Approach:
#   1. For each year with sufficient data:
#      - Fit TPRS model for each environmental variable
#      - Generate predictions on a common spatial grid
#   2. Average annual surfaces to get multi-year mean surface
#   3. Calculate temporal variability (CV, SD) at each grid point
#   4. Extract temporally-averaged values at across the entire grid
#
# Advantages over single aggregated interpolation:
#   - Respects inter-annual variability
#   - Avoids bias towards years with more sampling
#   - Provides uncertainty estimates from temporal variation
#   - More robust spatial estimates
#
# ===============================================================================

# Load required libraries
library(mgcv)          # For GAM and thin plate splines
library(dplyr)         # Data manipulation
library(tidyr)         # Data tidying
library(readxl)        # Read Excel files
library(ggplot2)       # Plotting
library(viridis)       # Color palettes
library(patchwork)     # Combine plots
library(metR)          # Contour/isoline plotting
library(sf)            # Spatial data handling
library(rgeoboundaries) # Country boundaries
library(ggspatial)     # Spatial plotting utilities

# ===============================================================================
# 1. CONFIGURATION
# ===============================================================================

# Minimum sites per year required to fit TPRS
MIN_SITES_PER_YEAR <- 100 # original was 20 obvs/year

# TPRS basis dimension (controls smoothness)
# Higher k = more flexible, lower k = smoother
# check the model diagnostics output (6a_TPRS_annual_model_diagnostics.csv) after running
# If the EDF values are consistently close to k, increase it. If they're much lower, you could even reduce it.
TPRS_K <- 60 # original was 30

# Prediction grid resolution (300x300)
# Lithuania spans roughly 4° longitude (~300 km) and 2° latitude (~220 km)
# With 300 cells: each cell is ~1 km × `~0.73 km
GRID_RESOLUTION <- 300 # original was 100

# Buffer around data extent (proportion)
EXTENT_BUFFER <- 0.05

# Output directory
OUTPUT_DIR <- "Output data"
PLOTS_DIR <- "Plots"

# Path to geodatabase with river network
GDB_PATH <- "Additonal data/UETK_2024-05-02.gdb"

# ===============================================================================
# 2. LOAD AND PREPARE DATA
# ===============================================================================

cat("\n========================================\n")
cat("LOADING DATA\n")
cat("========================================\n\n")

# Load the seasonal/annual means data
annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

# ------------------------------------------------------------------------------
# Load spatial data: country boundary and river network
# ------------------------------------------------------------------------------

cat("Loading spatial data...\n")

# Lithuania boundary
lithuania_sf <- geoboundaries("Lithuania", adm_lvl = 0)
cat("  Lithuania boundary loaded\n")

# Rivers from geodatabase
rivers <- st_read(GDB_PATH, layer = "upes_l", quiet = TRUE) |>
  st_transform(4326)  # Reproject to WGS84
cat(paste0("  Rivers loaded: ", nrow(rivers), " features\n"))

# Define environmental variables for interpolation
env_vars <- c(
  # Physical-chemical
  "temp", "pH", "EC", "alkalinity",
  # "DO_mg",
  # # Nutrients
  # "total_N", "total_P", "NO3_N", "PO4_P",
  # # Pollution indicators
  # "BOD7", "suspended_solids", "NH4_N"
  # Hydrological
  "current", "flow_velocity"
)

# Create standardised variable names mapping
var_mapping <- c(
  "temp" = "temp[°C]_ann",
  "pH" = "pH_ann",
  "EC" = "EC[µS/cm]_ann",
  "alkalinity" = "alkalinity[mmol/l]_ann",
  # "DO_mg" = "D.O.[mg/l]_ann",
  # "total_N" = "total_N[mg/L]_ann",
  # "total_P" = "total_P[mg/L]_ann",
  # "NO3_N" = "NO3_N[mg/L]_ann",
  # "PO4_P" = "PO4_P[mg/L]_ann",
  # "BOD7" = "BOD7[mg/L]_ann",
  # "suspended_solids" = "suspended_solids[mg/l]_ann",
  # "NH4_N" = "NH4_N[mg/L]_ann",
  "current" = "flow[m3/s]_ann",
  "flow_velocity" = "velocity[m/s]_ann"
)

# Filter for complete seasonal coverage and prepare data
site_year_data <- annual_means %>%
  filter(data_quality == "complete") %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  select(
    site_id, year, longitude, latitude,
    river_type, river_basin, river_name, modification_state,
    elevation,
    all_of(unname(var_mapping))
  ) %>%
  # Rename variables to simpler names
  rename(!!!setNames(unname(var_mapping), names(var_mapping)))

cat(paste0("Total site-year observations: ", nrow(site_year_data), "\n"))
cat(paste0("Unique sites: ", n_distinct(site_year_data$site_id), "\n"))
cat(paste0("Years covered: ", paste(sort(unique(site_year_data$year)), collapse = ", "), "\n\n"))

# Check years with sufficient data
year_summary <- site_year_data %>%
  group_by(year) %>%
  summarise(
    n_sites = n_distinct(site_id),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(sufficient = n_sites >= MIN_SITES_PER_YEAR)

cat("Sites per year:\n")
print(year_summary)

valid_years <- year_summary %>%
  filter(sufficient) %>%
  pull(year)

cat(paste0("\nYears with sufficient data (>= ", MIN_SITES_PER_YEAR, " sites): ",
           length(valid_years), "\n"))

# ===============================================================================
# 3. CREATE PREDICTION GRID
# ===============================================================================

cat("\n========================================\n")
cat("CREATING PREDICTION GRID\n")
cat("========================================\n\n")

# Calculate spatial extent with buffer
lon_range <- range(site_year_data$longitude, na.rm = TRUE)
lat_range <- range(site_year_data$latitude, na.rm = TRUE)

lon_buffer <- diff(lon_range) * EXTENT_BUFFER
lat_buffer <- diff(lat_range) * EXTENT_BUFFER

# Create regular prediction grid
pred_grid <- expand.grid(
  longitude = seq(lon_range[1] - lon_buffer, lon_range[2] + lon_buffer,
                  length.out = GRID_RESOLUTION),
  latitude = seq(lat_range[1] - lat_buffer, lat_range[2] + lat_buffer,
                 length.out = GRID_RESOLUTION)
)

cat(paste0("Grid extent:\n"))
cat(paste0("  Longitude: ", round(min(pred_grid$longitude), 3), " to ",
           round(max(pred_grid$longitude), 3), "\n"))
cat(paste0("  Latitude: ", round(min(pred_grid$latitude), 3), " to ",
           round(max(pred_grid$latitude), 3), "\n"))
cat(paste0("  Resolution: ", GRID_RESOLUTION, " x ", GRID_RESOLUTION, "\n"))
cat(paste0("  Total grid points: ", nrow(pred_grid), "\n"))

plot(pred_grid)

# ===============================================================================
# 4. TPRS FITTING FUNCTION
# ===============================================================================

#' Fit Thin Plate Spline for a single variable and year

#' @param data Data frame with longitude, latitude, and response variable
#' @param var_name Name of variable to interpolate
#' @param pred_grid Prediction grid
#' @param k Basis dimension
#' @return List with model, predictions, and diagnostics

fit_tprs_annual <- function(data, var_name, pred_grid, k = TPRS_K) {

  # Prepare data - remove NAs for this variable
  model_data <- data %>%
    select(longitude, latitude, value = !!sym(var_name)) %>%
    filter(!is.na(value))

  n_obs <- nrow(model_data)

  # Check minimum observations
  if (n_obs < 10) {
    return(list(
      success = FALSE,
      message = paste("Insufficient observations:", n_obs),
      n_obs = n_obs
    ))
  }

  # Adjust k if necessary (k must be <= n_obs)
  k_adjusted <- min(k, n_obs - 1)

  # Fit GAM with thin plate spline
  tryCatch({
    gam_model <- gam(
      value ~ s(longitude, latitude, bs = "tp", k = k_adjusted),
      data = model_data,
      method = "REML"
    )

    # Predict on grid
    preds <- predict(gam_model, newdata = pred_grid, se.fit = TRUE)

    # Model diagnostics
    model_summary <- summary(gam_model)

    return(list(
      success = TRUE,
      model = gam_model,
      predictions = preds$fit,
      se = preds$se.fit,
      n_obs = n_obs,
      k_used = k_adjusted,
      deviance_explained = model_summary$dev.expl,
      r_squared = model_summary$r.sq,
      edf = sum(gam_model$edf)
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      message = e$message,
      n_obs = n_obs
    ))
  })
}

# ===============================================================================
# 5. ANNUAL INTERPOLATION LOOP
# ===============================================================================

cat("\n========================================\n")
cat("FITTING ANNUAL TPRS MODELS\n")
cat("========================================\n\n")

# Storage for results
annual_predictions <- list()
model_diagnostics <- data.frame()

# Loop through each variable
for (var in env_vars) {

  cat(paste0("\n--- Processing: ", var, " ---\n"))

  # Storage for this variable's annual surfaces
  var_annual_preds <- list()

  # Loop through valid years
  for (yr in valid_years) {

    # Filter data for this year
    year_data <- site_year_data %>%
      filter(year == yr)

    # Fit TPRS
    result <- fit_tprs_annual(year_data, var, pred_grid, k = TPRS_K)

    if (result$success) {
      # Store predictions
      var_annual_preds[[as.character(yr)]] <- result$predictions

      # Store diagnostics
      model_diagnostics <- bind_rows(model_diagnostics, data.frame(
        variable = var,
        year = yr,
        n_obs = result$n_obs,
        k_used = result$k_used,
        deviance_explained = result$deviance_explained,
        r_squared = result$r_squared,
        edf = result$edf
      ))

      cat(paste0("  ", yr, ": n=", result$n_obs,
                 ", Dev.Expl=", round(result$deviance_explained * 100, 1), "%\n"))

    } else {
      cat(paste0("  ", yr, ": SKIPPED - ", result$message, "\n"))
    }
  }

  # Store all annual predictions for this variable
  if (length(var_annual_preds) > 0) {
    annual_predictions[[var]] <- var_annual_preds
  }
}

cat("\n\nModel fitting complete!\n")
cat(paste0("Variables successfully interpolated: ", length(annual_predictions), "\n"))

# ===============================================================================
# 6. CALCULATE TEMPORAL AVERAGES
# ===============================================================================

cat("\n========================================\n")
cat("CALCULATING TEMPORAL AVERAGES\n")
cat("========================================\n\n")

# Function to calculate temporal statistics
calc_temporal_stats <- function(annual_preds) {

  # Convert list to matrix (rows = grid points, cols = years)
  pred_matrix <- do.call(cbind, annual_preds)

  # Calculate statistics across years
  data.frame(
    mean = rowMeans(pred_matrix, na.rm = TRUE),
    sd = apply(pred_matrix, 1, sd, na.rm = TRUE),
    min = apply(pred_matrix, 1, min, na.rm = TRUE),
    max = apply(pred_matrix, 1, max, na.rm = TRUE),
    n_years = apply(pred_matrix, 1, function(x) sum(!is.na(x))),
    cv = apply(pred_matrix, 1, function(x) {
      m <- mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      if (m != 0) s / abs(m) * 100 else NA
    })
  )
}

# Calculate temporal statistics for each variable
temporal_surfaces <- list()

for (var in names(annual_predictions)) {

  cat(paste0("Calculating temporal statistics for ", var, "...\n"))

  stats <- calc_temporal_stats(annual_predictions[[var]])
  stats$variable <- var
  temporal_surfaces[[var]] <- stats
}

# Combine with grid coordinates
grid_with_temporal_means <- pred_grid

for (var in names(temporal_surfaces)) {
  grid_with_temporal_means[[paste0(var, "_mean")]] <- temporal_surfaces[[var]]$mean
  grid_with_temporal_means[[paste0(var, "_sd")]] <- temporal_surfaces[[var]]$sd
  grid_with_temporal_means[[paste0(var, "_cv")]] <- temporal_surfaces[[var]]$cv
  grid_with_temporal_means[[paste0(var, "_n_years")]] <- temporal_surfaces[[var]]$n_years
}

cat("\nTemporal averaging complete!\n")

# ===============================================================================
# 6b. CLIP GRID TO LITHUANIA BOUNDARY
# ===============================================================================

cat("\n========================================\n")
cat("CLIPPING GRID TO LITHUANIA BOUNDARY\n")
cat("========================================\n\n")

# Convert grid to sf object
grid_sf <- st_as_sf(grid_with_temporal_means,
                    coords = c("longitude", "latitude"),
                    crs = 4326)

# Clip to Lithuania boundary
grid_clipped_sf <- st_intersection(grid_sf, lithuania_sf)

# Convert back to data frame with coordinates
grid_clipped <- grid_clipped_sf %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(longitude, latitude, everything(), -shapeGroup, -shapeType, -shapeName, -shapeID, -shapeISO) %>%
  as.data.frame()

cat(paste0("  Original grid points: ", nrow(grid_with_temporal_means), "\n"))
cat(paste0("  Points within Lithuania: ", nrow(grid_clipped), "\n"))

# ===============================================================================
# 7. EXTRACT VALUES AT SITE LOCATIONS
# ===============================================================================

cat("\n========================================\n")
cat("EXTRACTING VALUES AT SITE LOCATIONS\n")
cat("========================================\n\n")

# Get unique site locations
site_locations <- site_year_data %>%
  group_by(site_id) %>%
  summarise(
    longitude = first(longitude),
    latitude = first(latitude),
    river_type = first(river_type),
    river_basin = first(river_basin),
    river_name = first(river_name),
    modification_state = first(modification_state),
    elevation = first(elevation),
    n_years_data = n_distinct(year),
    .groups = "drop"
  )

# Extract temporally-averaged interpolated values at each site
site_interpolated <- site_locations

for (var in names(annual_predictions)) {

  # For each year, predict at site locations
  site_annual_preds <- list()

  for (yr in names(annual_predictions[[var]])) {

    # Get the year's data to refit model (or use stored predictions)
    year_data <- site_year_data %>% filter(year == as.numeric(yr))

    # Fit model again to predict at site locations
    result <- fit_tprs_annual(year_data, var, site_locations, k = TPRS_K)

    if (result$success) {
      site_annual_preds[[yr]] <- result$predictions
    }
  }

  # Calculate temporal mean at sites
  if (length(site_annual_preds) > 0) {
    pred_matrix <- do.call(cbind, site_annual_preds)
    site_interpolated[[paste0(var, "_interp")]] <- rowMeans(pred_matrix, na.rm = TRUE)
    site_interpolated[[paste0(var, "_interp_sd")]] <- apply(pred_matrix, 1, sd, na.rm = TRUE)
  }
}

# Also add observed mean values for comparison
observed_means <- site_year_data %>%
  group_by(site_id) %>%
  summarise(across(all_of(env_vars), ~mean(., na.rm = TRUE), .names = "{.col}_obs"),
            .groups = "drop")

site_interpolated <- site_interpolated %>%
  left_join(observed_means, by = "site_id")

cat(paste0("Interpolated values extracted for ", nrow(site_interpolated), " sites\n"))

# ===============================================================================
# 8. CREATE VISUALISATIONS
# ===============================================================================

cat("\n========================================\n")
cat("CREATING VISUALISATIONS\n")
cat("========================================\n\n")

#' Create isoline map with temporal mean, country borders, and river network
#' Grid data should be pre-clipped to Lithuania boundary
#'
#' @param grid_data Data frame with grid predictions (clipped to Lithuania) for fill
#' @param full_grid_data Data frame with full grid predictions for contour calculation
#' @param sites Data frame with site locations
#' @param var_name Variable name to plot
#' @param lithuania_boundary sf object with Lithuania boundary
#' @param rivers_sf sf object with river network
#' @param title_suffix Optional title suffix
create_temporal_isoline_map <- function(grid_data, full_grid_data = NULL, sites, var_name,
                                         lithuania_boundary = NULL,
                                         rivers_sf = NULL,
                                         title_suffix = "") {

  mean_col <- paste0(var_name, "_mean")

  # Check if columns exist
  if (!mean_col %in% names(grid_data)) {
    return(NULL)
  }

  # Get clipped data for fill

  plot_data <- grid_data %>%
    select(longitude, latitude, mean_val = !!sym(mean_col))

  # Use full grid for contours if provided, otherwise use clipped
  if (!is.null(full_grid_data)) {
    contour_data <- full_grid_data %>%
      select(longitude, latitude, mean_val = !!sym(mean_col))
  } else {
    contour_data <- plot_data
  }

  # Determine contour breaks based on clipped data range

  breaks <- pretty(plot_data$mean_val, n = 10)

  # Create main plot
  p <- ggplot() +
    # Smooth interpolated surface using tiles (clipped data)
    geom_tile(data = plot_data,
              aes(x = longitude, y = latitude, fill = mean_val)) +
    # Contour lines (from full grid for proper calculation)
    geom_contour(data = contour_data,
                 aes(x = longitude, y = latitude, z = mean_val),
                 breaks = breaks,
                 color = "grey30", alpha = 0.7, linewidth = 0.4) +
    # Labelled contours
    geom_text_contour(data = contour_data,
                      aes(x = longitude, y = latitude, z = mean_val),
                      breaks = breaks,
                      stroke = 0.2, size = 2.5, skip = 0,
                      color = "grey20")

  # Add rivers if provided
  if (!is.null(rivers_sf)) {
    p <- p +
      geom_sf(data = rivers_sf, color = "steelblue",
              linewidth = 0.15, alpha = 0.4, inherit.aes = FALSE)
  }

  # Add country boundary if provided
  if (!is.null(lithuania_boundary)) {
    p <- p +
      geom_sf(data = lithuania_boundary, fill = NA, color = "black",
              linewidth = 0.8, inherit.aes = FALSE)
  }

  # Add finishing touches
  p <- p +
    # Scales
    scale_fill_viridis_c(option = "plasma", name = var_name) +
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0(var_name, " - Temporal Mean", title_suffix),
      subtitle = "Averaged across annual TPRS interpolations",
      x = "Longitude", y = "Latitude"
    ) +
    coord_sf(crs = 4326, expand = FALSE)

  return(p)
}

# Create maps for key variables
cat("Generating isoline maps...\n")

key_vars <- c("temp", "pH", "EC", "alkalinity", "current", "flow_velocity")
maps <- list()

for (var in key_vars) {
  if (paste0(var, "_mean") %in% names(grid_clipped)) {
    maps[[var]] <- create_temporal_isoline_map(
      grid_data = grid_clipped,
      full_grid_data = grid_with_temporal_means,  # Pass full grid for contours
      sites = site_locations,
      var_name = var,
      lithuania_boundary = lithuania_sf,
      rivers_sf = rivers
    )

    # Save individual plot
    ggsave(
      filename = file.path(PLOTS_DIR, paste0("Figure_TPRS_", var, ".png")),
      plot = maps[[var]],
      width = 8, height = 6, dpi = 200, bg = "white"
    )
    cat(paste0("  ", var, " map saved\n"))
  }
}

# Save combined figure
if (length(maps) > 0) {

  combined_maps <- wrap_plots(maps[!sapply(maps, is.null)], ncol = 2)

  ggsave(
    filename = file.path(PLOTS_DIR, "Figure_TPRS_annual_averaged_isolines.png"),
    plot = combined_maps,
    width = 14, height = 12, dpi = 200, bg = "white"
  )

  ggsave(
    filename = file.path(PLOTS_DIR, "Figure_TPRS_annual_averaged_isolines.pdf"),
    plot = combined_maps,
    width = 14, height = 12, bg = "white"
  )

  cat("  Combined isoline maps saved\n")
}

# ===============================================================================
# 9. CREATE TEMPORAL VARIABILITY MAPS
# ===============================================================================

cat("Generating temporal variability maps...\n")

#' Create temporal variability (CV) map for a variable
#' @param grid_data Data frame with clipped grid predictions
#' @param full_grid_data Data frame with full grid predictions for contour calculation
#' @param var_name Variable name
#' @param lithuania_boundary sf object with Lithuania boundary
#' @param rivers_sf sf object with river network
#' @return ggplot object

create_cv_map <- function(grid_data, full_grid_data, var_name,
                          lithuania_boundary = NULL, rivers_sf = NULL) {

  cv_col <- paste0(var_name, "_cv")

  # Check if column exists
  if (!cv_col %in% names(grid_data)) {
    return(NULL)
  }

  # Prepare plot data
  plot_data <- grid_data %>%
    select(longitude, latitude, cv_val = !!sym(cv_col))

    contour_data <- full_grid_data %>%
    select(longitude, latitude, cv_val = !!sym(cv_col))

  # Determine contour breaks
  cv_breaks <- pretty(plot_data$cv_val, n = 8)

  # Variable labels for nicer titles
  var_labels <- c(
    "temp" = "Temperature",
    "pH" = "pH",
    "EC" = "Electrical Conductivity",
    "alkalinity" = "Alkalinity",
    # "DO_mg" = "Dissolved Oxygen",
    # "total_N" = "Total Nitrogen",
    # "total_P" = "Total Phosphorus"
    "current" = "Current",
    "flow_velocity" = "Flow velocity"
  )

  var_label <- ifelse(var_name %in% names(var_labels),
                      var_labels[var_name], var_name)

  p <- ggplot() +
    # Smooth interpolated surface using tiles
    geom_tile(data = plot_data,
              aes(x = longitude, y = latitude, fill = cv_val)) +
    # Contour lines (from full grid)
    geom_contour(data = contour_data,
                 aes(x = longitude, y = latitude, z = cv_val),
                 breaks = cv_breaks,
                 color = "grey30", alpha = 0.7, linewidth = 0.4) +
    # Labelled contours
    geom_text_contour(data = contour_data,
                      aes(x = longitude, y = latitude, z = cv_val),
                      breaks = cv_breaks,
                      stroke = 0.2, size = 2.5, skip = 0,
                      color = "grey20")

  # Add rivers if provided
  if (!is.null(rivers_sf)) {
    p <- p +
      geom_sf(data = rivers_sf, color = "steelblue",
              linewidth = 0.15, alpha = 0.4, inherit.aes = FALSE)
  }

  # Add country boundary if provided
  if (!is.null(lithuania_boundary)) {
    p <- p +
      geom_sf(data = lithuania_boundary, fill = NA, color = "black",
              linewidth = 0.8, inherit.aes = FALSE)
  }

  p <- p +
    scale_fill_viridis_c(option = "magma", name = "CV (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("Temporal Variability: ", var_label),
      subtitle = "Coefficient of Variation across years",
      x = "Longitude", y = "Latitude"
    ) +
    coord_sf(crs = 4326, expand = FALSE)

  return(p)
}

# Variables for temporal variability maps
cv_vars <- c("temp", "pH", "EC", "alkalinity", "current", "flow_velocity")

cv_maps <- list()
for (var in cv_vars) {
  if (paste0(var, "_cv") %in% names(grid_clipped)) {
    cv_maps[[var]] <- create_cv_map(
      grid_data = grid_clipped,
      full_grid_data = grid_with_temporal_means,
      var_name = var,
      lithuania_boundary = lithuania_sf,
      rivers_sf = rivers
    )

    # Save individual CV plot
    ggsave(
      filename = file.path(PLOTS_DIR, paste0("Figure_TPRS_CV_", var, ".png")),
      plot = cv_maps[[var]],
      width = 8, height = 6, dpi = 200, bg = "white"
    )
    cat(paste0("  ", var, " CV map saved\n"))
  }
}

# Save combined CV figure
if (length(cv_maps) > 0) {
  combined_cv_maps <- wrap_plots(cv_maps[!sapply(cv_maps, is.null)], ncol = 2)
  ggsave(
    filename = file.path(PLOTS_DIR, "Figure_TPRS_temporal_variability_all.png"),
    plot = combined_cv_maps,
    width = 14, height = 12, dpi = 200, bg = "white"
  )
  ggsave(
    filename = file.path(PLOTS_DIR, "Figure_TPRS_temporal_variability_all.pdf"),
    plot = combined_cv_maps,
    width = 14, height = 12, bg = "white"
  )
  cat("  Combined temporal variability maps saved\n")
}

# ===============================================================================
# 10. SAVE OUTPUTS
# ===============================================================================

cat("\n========================================\n")
cat("SAVING OUTPUTS\n")
cat("========================================\n\n")

# Save clipped interpolated grid (Lithuania only) - use this for extracting values
write.csv(
  grid_clipped,
  file.path(OUTPUT_DIR, "4a_TPRS_annual_averaged_grid_Lithuania.csv"),
  row.names = FALSE
)
cat("  Clipped grid predictions saved (Lithuania only)\n")
cat(paste0("    -> Use this file to extract values for unmonitored sites\n"))
cat(paste0("    -> ", nrow(grid_clipped), " grid points within Lithuania\n"))

# Save site-level interpolated values
write.csv(
  site_interpolated,
  file.path(OUTPUT_DIR, "5a_site_data_annual_averaged_interpolations.csv"),
  row.names = FALSE
)
cat("  Site interpolations saved\n")

# Save model diagnostics
write.csv(
  model_diagnostics,
  file.path(OUTPUT_DIR, "6a_TPRS_annual_model_diagnostics.csv"),
  row.names = FALSE
)
cat("  Model diagnostics saved\n")

# ===============================================================================
# 11. SUMMARY REPORT
# ===============================================================================

cat("\n\n")
cat("========================================\n")
cat("ANNUAL TPRS INTERPOLATION COMPLETE!\n")
cat("========================================\n\n")

cat("Summary:\n")
cat(paste0("  Sites: ", n_distinct(site_year_data$site_id), "\n"))
cat(paste0("  Years with sufficient data: ", length(valid_years), "\n"))
cat(paste0("  Variables interpolated: ", length(annual_predictions), "\n"))
cat(paste0("  Grid resolution: ", GRID_RESOLUTION, " x ", GRID_RESOLUTION, "\n"))
cat(paste0("  Total grid points: ", nrow(pred_grid), "\n"))

cat("\nModel performance summary:\n")
print(
  model_diagnostics %>%
    group_by(variable) %>%
    summarise(
      n_years_fit = n(),
      mean_n_obs = mean(n_obs),
      mean_dev_expl = mean(deviance_explained) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(mean_dev_expl))
)

cat("\nOutputs generated:\n")
cat("  1. 4a_TPRS_annual_averaged_grid_Lithuania.csv - Grid predictions clipped to Lithuania\n")
cat("     -> Use this to extract interpolated values at any location within Lithuania\n")
cat("  2. 5a_site_data_annual_averaged_interpolations.csv - Site-level interpolations\n")
cat("  3. 6a_TPRS_annual_model_diagnostics.csv - Model diagnostics by year\n")
cat("  4. Figure_TPRS_annual_averaged_isolines.png/pdf - Isoline maps\n")
cat("  5. Figure_TPRS_temporal_variability.png - CV map\n")

cat("\nTo extract values for unmonitored sites:\n")
cat("  1. Load the clipped grid: grid <- read.csv('Output data/4a_TPRS_annual_averaged_grid_Lithuania.csv')\n")
cat("  2. For each unmonitored site, find nearest grid point or use spatial interpolation\n")
cat("  3. Example using nearest neighbour:\n")
cat("     library(FNN)\n")
cat("     nn <- get.knnx(grid[,c('longitude','latitude')], new_sites[,c('lon','lat')], k=1)\n")
cat("     extracted_values <- grid[nn$nn.index, ]\n")

cat("\nNext steps:\n")
cat("  -> Use temporal-averaged surfaces for clustering\n")
cat("  -> Examine temporal variability patterns\n")
cat("  -> Extract values for unmonitored river reaches\n\n")
