# ===============================================================================
# LITHUANIAN RIVER TYPOLOGY - THIN PLATE REGRESSION SPLINES SPATIAL ANALYSIS
# ===============================================================================
#
# Purpose: Use thin plate regression splines (TPRS) to create isoline maps
#          and explore spatial patterns in abiotic variables across Lithuania
#
# Author: Freshwater Ecologist
# Date: 2025-01-04
#
# This script implements a comprehensive spatial interpolation approach using
# thin plate splines to create continuous surfaces of environmental variables,
# similar to kriging or ArcGIS isoline mapping but with superior handling of
# edge effects and complex spatial patterns.
#
# ===============================================================================

# Load required libraries
library(mgcv)          # For GAM and thin plate splines
library(dplyr)         # Data manipulation
library(tidyr)         # Data tidying
library(readxl)        # Read Excel files
library(ggplot2)       # Advanced plotting
library(sf)            # Spatial data handling
library(viridis)       # Color palettes
library(patchwork)     # Combine plots
library(metR)          # Isoline/contour plotting

# ===============================================================================
# 1. LOAD AND PREPARE DATA
# ===============================================================================

cat("Loading environmental data...\n")

# Load the seasonal/annual means data
annual_means <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

# Filter for complete seasonal coverage and aggregate to site level
site_data <- annual_means %>%
  filter(data_quality == "complete") %>%
  group_by(site_id) %>%
  summarise(
    # Coordinates
    longitude = first(longitude),
    latitude = first(latitude),
    elevation = first(elevation),

    # Site metadata
    river_type = first(river_type),
    river_basin = first(river_basin),
    river_name = first(river_name),
    modification_state = first(modification_state),

    # Hydrological variables
    flow = mean(`flow[m3/s]_ann`, na.rm = TRUE),
    velocity = mean(`velocity[m/s]_ann`, na.rm = TRUE),

    # Physical-chemical variables
    temp = mean(`temp[°C]_ann`, na.rm = TRUE),
    pH = mean(`pH_ann`, na.rm = TRUE),
    DO_mg = mean(`D.O.[mg/l]_ann`, na.rm = TRUE),
    DO_pct = mean(`D.O.[%]_ann`, na.rm = TRUE),
    suspended_solids = mean(`suspended_solids[mg/l]_ann`, na.rm = TRUE),

    # Organic pollution indicators
    BOD7 = mean(`BOD7[mg/L]_ann`, na.rm = TRUE),
    ChDS_C = mean(`ChDS_C[mg/L]_ann`, na.rm = TRUE),

    # Nutrient variables
    NH4_N = mean(`NH4_N[mg/L]_ann`, na.rm = TRUE),
    NO2_N = mean(`NO2_N[mg/L]_ann`, na.rm = TRUE),
    NO3_N = mean(`NO3_N[mg/L]_ann`, na.rm = TRUE),
    mineral_N = mean(`mineral_N[mg/L]_ann`, na.rm = TRUE),
    total_N = mean(`total_N[mg/L]_ann`, na.rm = TRUE),
    PO4_P = mean(`PO4_P[mg/L]_ann`, na.rm = TRUE),
    total_P = mean(`total_P[mg/L]_ann`, na.rm = TRUE),

    # Ionic composition
    EC = mean(`EC[µS/cm]_ann`, na.rm = TRUE),
    alkalinity = mean(`alkalinity[mmol/l]_ann`, na.rm = TRUE),

    # Sample size
    n_years = n(),

    .groups = "drop"
  ) %>%
  # Convert NaN to NA
  mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

# Remove rows with missing coordinates
site_data <- site_data %>%
  filter(!is.na(longitude) & !is.na(latitude))

cat("Data loaded successfully!\n")
cat(paste0("Number of sites: ", nrow(site_data), "\n"))
cat(paste0("Spatial extent:\n"))
cat(paste0("  Longitude: ", round(min(site_data$longitude, na.rm=TRUE), 3), " to ",
           round(max(site_data$longitude, na.rm=TRUE), 3), "\n"))
cat(paste0("  Latitude: ", round(min(site_data$latitude, na.rm=TRUE), 3), " to ",
           round(max(site_data$latitude, na.rm=TRUE), 3), "\n"))

# ===============================================================================
# 2. DEFINE KEY VARIABLES FOR SPATIAL INTERPOLATION
# ===============================================================================

# Define variable groups based on ecological significance
typology_vars <- c("temp", "pH", "EC", "alkalinity", "DO_mg")
nutrient_vars <- c("total_N", "total_P", "NO3_N", "PO4_P")
hydro_vars <- c("flow", "velocity")
pollution_vars <- c("BOD7", "suspended_solids", "NH4_N")

# Combine all variables of interest
all_vars <- c(typology_vars, nutrient_vars, hydro_vars, pollution_vars)

# ===============================================================================
# 3. CREATE PREDICTION GRID FOR LITHUANIA
# ===============================================================================

cat("\nCreating prediction grid for spatial interpolation...\n")

# Create a fine-resolution grid covering Lithuania's extent
# Add buffer to ensure coverage
lon_range <- range(site_data$longitude, na.rm = TRUE)
lat_range <- range(site_data$latitude, na.rm = TRUE)

# Add 5% buffer
lon_buffer <- diff(lon_range) * 0.05
lat_buffer <- diff(lat_range) * 0.05

# Create grid (100x100 points - can be adjusted for resolution)
pred_grid <- expand.grid(
  longitude = seq(lon_range[1] - lon_buffer, lon_range[2] + lon_buffer, length.out = 100),
  latitude = seq(lat_range[1] - lat_buffer, lat_range[2] + lat_buffer, length.out = 100)
)

cat(paste0("Prediction grid created: ", nrow(pred_grid), " points\n"))

# ===============================================================================
# 4. THIN PLATE REGRESSION SPLINE INTERPOLATION FUNCTION
# ===============================================================================

#' Fit Thin Plate Spline and Predict
#'
#' @param data Data frame with longitude, latitude, and response variable
#' @param var_name Name of variable to interpolate
#' @param pred_grid Prediction grid
#' @param k Dimension of the basis (complexity of spline)
#' @return Data frame with predictions and standard errors
fit_tprs <- function(data, var_name, pred_grid, k = 30) {

  # Prepare data - remove NAs for this variable
  model_data <- data %>%
    select(longitude, latitude, !!sym(var_name)) %>%
    filter(!is.na(!!sym(var_name)))

  # Check if we have enough data
  if(nrow(model_data) < 10) {
    warning(paste("Insufficient data for", var_name, "- skipping"))
    return(NULL)
  }

  # Fit GAM with thin plate spline
  # bs = "tp" specifies thin plate regression spline
  # k controls the basis dimension (higher = more flexible)
  formula_str <- paste0(var_name, " ~ s(longitude, latitude, bs = 'tp', k = ", k, ")")

  gam_model <- gam(
    as.formula(formula_str),
    data = model_data,
    method = "REML"  # REML for better smoothness parameter estimation
  )

  # Predict on grid
  predictions <- predict(gam_model, newdata = pred_grid, se.fit = TRUE)

  # Combine with grid
  result <- pred_grid %>%
    mutate(
      variable = var_name,
      predicted = predictions$fit,
      se = predictions$se.fit,
      lower_ci = predicted - 1.96 * se,
      upper_ci = predicted + 1.96 * se
    )

  # Return model and predictions
  list(
    model = gam_model,
    predictions = result,
    n_obs = nrow(model_data),
    deviance_explained = summary(gam_model)$dev.expl
  )
}

# ===============================================================================
# 5. INTERPOLATE ALL VARIABLES
# ===============================================================================

cat("\n========================================\n")
cat("FITTING THIN PLATE SPLINES...\n")
cat("========================================\n\n")

# Store results
tprs_results <- list()
tprs_models <- list()

# Fit TPRS for each variable
for(var in all_vars) {
  cat(paste0("Processing: ", var, "...\n"))

  tryCatch({
    result <- fit_tprs(site_data, var, pred_grid, k = 30)

    if(!is.null(result)) {
      tprs_results[[var]] <- result$predictions
      tprs_models[[var]] <- result$model

      cat(paste0("  ✓ Success! N = ", result$n_obs,
                 ", Deviance explained = ", round(result$deviance_explained * 100, 1), "%\n"))
    }
  }, error = function(e) {
    cat(paste0("  ✗ Error: ", e$message, "\n"))
  })

  cat("\n")
}

# Combine all predictions
all_predictions <- bind_rows(tprs_results)

cat("Spatial interpolation complete!\n")
cat(paste0("Successfully interpolated ", length(tprs_results), " variables\n"))

# ===============================================================================
# 6. CREATE ISOLINE MAPS
# ===============================================================================

cat("\nCreating isoline maps...\n")

#' Create beautiful isoline map for a variable
#'
#' @param predictions Prediction data frame
#' @param sites Site data for overlay
#' @param var_name Variable name
#' @param n_breaks Number of contour breaks
create_isoline_map <- function(predictions, sites, var_name, n_breaks = 10) {

  # Filter predictions for this variable
  pred_data <- predictions %>%
    filter(variable == var_name)

  # Get site data for this variable
  site_values <- sites %>%
    select(longitude, latitude, river_type, value = !!sym(var_name)) %>%
    filter(!is.na(value))

  # Determine breaks
  breaks <- pretty(pred_data$predicted, n = n_breaks)

  # Create base plot with continuous surface
  p <- ggplot() +
    # Interpolated surface
    geom_raster(data = pred_data,
                aes(x = longitude, y = latitude, fill = predicted),
                interpolate = TRUE) +
    # Contour lines (isolines)
    geom_contour(data = pred_data,
                 aes(x = longitude, y = latitude, z = predicted),
                 breaks = breaks,
                 color = "white",
                 alpha = 0.4,
                 linewidth = 0.3) +
    # Labeled contours
    geom_text_contour(data = pred_data,
                      aes(x = longitude, y = latitude, z = predicted),
                      breaks = breaks,
                      stroke = 0.15,
                      size = 2.5,
                      skip = 0) +
    # Overlay actual site points
    geom_point(data = site_values,
               aes(x = longitude, y = latitude, color = value),
               size = 2,
               shape = 21,
               fill = "white",
               stroke = 1) +
    # Scales
    scale_fill_viridis_c(option = "plasma", name = var_name) +
    scale_color_viridis_c(option = "plasma", name = "Observed") +
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("Spatial Distribution: ", var_name),
      subtitle = "Thin Plate Regression Spline Interpolation",
      x = "Longitude",
      y = "Latitude"
    ) +
    coord_fixed(ratio = 1.5)  # Adjust aspect ratio for Lithuania

  return(p)
}

# ===============================================================================
# 7. GENERATE ISOLINE MAPS FOR KEY VARIABLES
# ===============================================================================

# Create maps for typology variables
cat("Generating typology variable maps...\n")

maps_typology <- lapply(typology_vars[typology_vars %in% names(tprs_results)], function(var) {
  create_isoline_map(all_predictions, site_data, var, n_breaks = 12)
})

# Create maps for nutrient variables
cat("Generating nutrient variable maps...\n")

maps_nutrients <- lapply(nutrient_vars[nutrient_vars %in% names(tprs_results)], function(var) {
  create_isoline_map(all_predictions, site_data, var, n_breaks = 10)
})

# ===============================================================================
# 8. SAVE ISOLINE MAPS
# ===============================================================================

cat("\nSaving isoline maps...\n")

# Save individual typology maps
if(length(maps_typology) > 0) {
  pdf("Plots/Figure4_TPRS_typology_variables.pdf", width = 14, height = 10)
  print(wrap_plots(maps_typology, ncol = 2))
  dev.off()

  png("Plots/Figure4_TPRS_typology_variables.png", width = 2800, height = 2000, res = 200)
  print(wrap_plots(maps_typology, ncol = 2))
  dev.off()

  cat("  ✓ Typology maps saved\n")
}

# Save individual nutrient maps
if(length(maps_nutrients) > 0) {
  pdf("Plots/Figure5_TPRS_nutrient_variables.pdf", width = 14, height = 10)
  print(wrap_plots(maps_nutrients, ncol = 2))
  dev.off()

  png("Plots/Figure5_TPRS_nutrient_variables.png", width = 2800, height = 2000, res = 200)
  print(wrap_plots(maps_nutrients, ncol = 2))
  dev.off()

  cat("  ✓ Nutrient maps saved\n")
}

# ===============================================================================
# 9. EXTRACT SPATIAL GRADIENTS (PCA ON INTERPOLATED SURFACES)
# ===============================================================================

cat("\n========================================\n")
cat("SPATIAL GRADIENT ANALYSIS\n")
cat("========================================\n\n")

# Create a matrix of all interpolated variables at each grid point
spatial_matrix <- all_predictions %>%
  select(longitude, latitude, variable, predicted) %>%
  pivot_wider(names_from = variable, values_from = predicted) %>%
  select(-longitude, -latitude)

# Remove rows with any NA
spatial_matrix_complete <- spatial_matrix[complete.cases(spatial_matrix), ]
grid_complete <- all_predictions %>%
  select(longitude, latitude) %>%
  distinct() %>%
  slice(which(complete.cases(spatial_matrix)))

cat(paste0("Complete cases for gradient analysis: ", nrow(spatial_matrix_complete), "\n\n"))

# Perform PCA on interpolated surfaces
if(nrow(spatial_matrix_complete) > 10) {

  pca_spatial <- prcomp(spatial_matrix_complete, scale. = TRUE, center = TRUE)

  # Add PC scores to grid
  grid_with_pcs <- grid_complete %>%
    bind_cols(as.data.frame(pca_spatial$x[, 1:4]))

  # Variance explained
  var_explained <- summary(pca_spatial)$importance[2, 1:4] * 100

  cat("PCA of Interpolated Surfaces:\n")
  cat(paste0("  PC1 explains: ", round(var_explained[1], 1), "% of variance\n"))
  cat(paste0("  PC2 explains: ", round(var_explained[2], 1), "% of variance\n"))
  cat(paste0("  PC3 explains: ", round(var_explained[3], 1), "% of variance\n"))
  cat(paste0("  PC4 explains: ", round(var_explained[4], 1), "% of variance\n\n"))

  # Variable loadings
  cat("Variable loadings on PC1 and PC2:\n")
  loadings_df <- as.data.frame(pca_spatial$rotation[, 1:2])
  loadings_df$variable <- rownames(loadings_df)
  print(loadings_df %>% arrange(desc(abs(PC1))))

  # Create spatial gradient maps
  p_pc1 <- ggplot(grid_with_pcs, aes(x = longitude, y = latitude, fill = PC1)) +
    geom_raster(interpolate = TRUE) +
    geom_contour(aes(z = PC1), color = "white", alpha = 0.3, linewidth = 0.3) +
    geom_point(data = site_data, aes(x = longitude, y = latitude),
               color = "black", size = 0.8, inherit.aes = FALSE) +
    scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
    theme_minimal() +
    labs(title = paste0("Principal Component 1 (", round(var_explained[1], 1), "% variance)"),
         subtitle = "Dominant environmental gradient across Lithuania") +
    coord_fixed(ratio = 1.5)

  p_pc2 <- ggplot(grid_with_pcs, aes(x = longitude, y = latitude, fill = PC2)) +
    geom_raster(interpolate = TRUE) +
    geom_contour(aes(z = PC2), color = "white", alpha = 0.3, linewidth = 0.3) +
    geom_point(data = site_data, aes(x = longitude, y = latitude),
               color = "black", size = 0.8, inherit.aes = FALSE) +
    scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
    theme_minimal() +
    labs(title = paste0("Principal Component 2 (", round(var_explained[2], 1), "% variance)"),
         subtitle = "Secondary environmental gradient") +
    coord_fixed(ratio = 1.5)

  # Save gradient maps
  pdf("Plots/Figure6_spatial_gradients.pdf", width = 14, height = 6)
  print(p_pc1 + p_pc2)
  dev.off()

  png("Plots/Figure6_spatial_gradients.png", width = 2800, height = 1200, res = 200)
  print(p_pc1 + p_pc2)
  dev.off()

  cat("\n✓ Spatial gradient maps saved\n")
}

# ===============================================================================
# 10. EXTRACT INTERPOLATED VALUES AT SITE LOCATIONS
# ===============================================================================

cat("\n========================================\n")
cat("EXTRACTING INTERPOLATED VALUES AT SITES\n")
cat("========================================\n\n")

# For each site, extract the interpolated value from each TPRS model
site_interpolated <- site_data %>%
  select(site_id, longitude, latitude, river_type, river_basin)

for(var in names(tprs_models)) {

  # Predict at site locations
  site_coords <- site_data %>%
    select(longitude, latitude)

  predictions_at_sites <- predict(tprs_models[[var]], newdata = site_coords, se.fit = TRUE)

  # Add to site data
  site_interpolated[[paste0(var, "_interpolated")]] <- predictions_at_sites$fit
  site_interpolated[[paste0(var, "_observed")]] <- site_data[[var]]
}

# Calculate residuals (observed - interpolated)
# Large residuals indicate sites that deviate from spatial expectations
for(var in names(tprs_models)) {
  site_interpolated[[paste0(var, "_residual")]] <-
    site_interpolated[[paste0(var, "_observed")]] -
    site_interpolated[[paste0(var, "_interpolated")]]
}

cat("Interpolated values extracted for all sites\n")
cat(paste0("Total variables: ", length(tprs_models), "\n"))

# ===============================================================================
# 11. IDENTIFY SPATIAL OUTLIERS AND UNIQUE SITES
# ===============================================================================

cat("\n========================================\n")
cat("IDENTIFYING SPATIAL OUTLIERS\n")
cat("========================================\n\n")

# Calculate multivariate distance from spatial expectation
residual_vars <- grep("_residual$", names(site_interpolated), value = TRUE)

# Standardize residuals
site_interpolated_scaled <- site_interpolated %>%
  mutate(across(all_of(residual_vars), ~scale(.) %>% as.vector(), .names = "{.col}_scaled"))

# Calculate Mahalanobis distance
residual_vars_scaled <- grep("_residual_scaled$", names(site_interpolated_scaled), value = TRUE)
residual_matrix <- site_interpolated_scaled %>%
  select(all_of(residual_vars_scaled)) %>%
  as.matrix()

# Remove rows with NA
complete_rows <- complete.cases(residual_matrix)
residual_matrix_complete <- residual_matrix[complete_rows, ]

if(nrow(residual_matrix_complete) > 10 && ncol(residual_matrix_complete) > 1) {

  # Calculate Mahalanobis distance
  mahal_dist <- mahalanobis(residual_matrix_complete,
                             center = colMeans(residual_matrix_complete),
                             cov = cov(residual_matrix_complete))

  # Add to data
  site_interpolated_scaled$mahal_distance <- NA
  site_interpolated_scaled$mahal_distance[complete_rows] <- mahal_dist

  # Identify outliers (e.g., > 95th percentile)
  threshold <- quantile(mahal_dist, 0.95, na.rm = TRUE)
  site_interpolated_scaled$spatial_outlier <- site_interpolated_scaled$mahal_distance > threshold

  cat(paste0("Number of spatial outliers identified: ",
             sum(site_interpolated_scaled$spatial_outlier, na.rm = TRUE), "\n"))

  # Show top outliers
  cat("\nTop 10 spatial outliers (sites deviating from spatial patterns):\n")
  top_outliers <- site_interpolated_scaled %>%
    filter(!is.na(mahal_distance)) %>%
    arrange(desc(mahal_distance)) %>%
    select(site_id, river_type, mahal_distance, longitude, latitude) %>%
    head(10)

  print(top_outliers)
}

# ===============================================================================
# 12. SAVE OUTPUTS
# ===============================================================================

cat("\n========================================\n")
cat("SAVING OUTPUTS\n")
cat("========================================\n\n")

# Save interpolated predictions
write.csv(all_predictions,
          "Output data/4_TPRS_interpolated_grid.csv",
          row.names = FALSE)
cat("✓ Interpolated grid saved\n")

# Save site-level data with interpolated values
write.csv(site_interpolated_scaled,
          "Output data/5_site_data_with_interpolations.csv",
          row.names = FALSE)
cat("✓ Site data with interpolations saved\n")

# Save model summaries
model_summaries <- data.frame(
  variable = names(tprs_models),
  n_obs = sapply(tprs_models, function(m) nobs(m)),
  deviance_explained = sapply(tprs_models, function(m) summary(m)$dev.expl),
  edf = sapply(tprs_models, function(m) sum(m$edf)),
  AIC = sapply(tprs_models, AIC)
)

write.csv(model_summaries,
          "Output data/6_TPRS_model_summaries.csv",
          row.names = FALSE)
cat("✓ Model summaries saved\n")

# ===============================================================================
# 13. SUMMARY REPORT
# ===============================================================================

cat("\n\n")
cat("========================================\n")
cat("TPRS SPATIAL ANALYSIS COMPLETE!\n")
cat("========================================\n\n")

cat("Summary:\n")
cat(paste0("  • Sites analyzed: ", nrow(site_data), "\n"))
cat(paste0("  • Variables interpolated: ", length(tprs_models), "\n"))
cat(paste0("  • Prediction grid points: ", nrow(pred_grid), "\n"))
cat(paste0("  • Spatial outliers identified: ", sum(site_interpolated_scaled$spatial_outlier, na.rm = TRUE), "\n"))
cat("\nOutputs generated:\n")
cat("  1. Typology variable isoline maps (Figure 4)\n")
cat("  2. Nutrient variable isoline maps (Figure 5)\n")
cat("  3. Spatial gradient maps (Figure 6)\n")
cat("  4. Interpolated grid data (CSV)\n")
cat("  5. Site data with interpolations (CSV)\n")
cat("  6. Model summaries (CSV)\n")
cat("\nNext steps:\n")
cat("  → Use spatial outliers for refined typology\n")
cat("  → Cluster sites based on interpolated values\n")
cat("  → Compare spatial patterns with current river types\n")
cat("  → Identify biogeographic regions\n\n")
