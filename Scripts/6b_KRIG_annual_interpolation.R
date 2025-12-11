############################################################
# KRIGING WITH COMMON OPTIMIZED GRID - MULTI-VARIABLE, MULTI-YEAR
# Based on Tisseyre et al. 2018 grid optimization
# Uses geoR package for kriging
############################################################

#==================== 1. LOAD PACKAGES ====================

library(geoR)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgeoboundaries)
library(sf)
library(fields)
library(writexl)
library(spdep)

#==================== 2. USER SETTINGS ====================

# 2.1 File paths ----
DATA_FILE   <- "Output data/1_annual_environmental_means_2009-2023.xlsx"
OUTPUT_DIR  <- "Output data"
PLOTS_DIR   <- "Plots/kriging_multi_variable"

# 2.2 Column names ----
x_col    <- "longitude"
y_col    <- "latitude"
year_col <- "year"
site_col <- "site_id"

# 2.3 Variables to interpolate ----
# Toggle variables on/off by commenting/uncommenting
VARIABLES_TO_KRIGE <- c(
  "flow_annual",
  "velocity_annual",
  "pH_annual",
 "temp_annual",
  "EC_annual",
  "alkalinity_annual"
  # "suspended_solids_annual",
  # "DO_mgl_annual",
  # "DO_pct_annual",
  # "BOD7_annual",
  # "ChDS_C_annual",
  # "NH4_N_annual",
  # "NO2_N_annual",
  # "NO3_N_annual",
  # "mineral_N_annual",
  # "total_N_annual",
  # "PO4_P_annual",
  # "total_P_annual"
)

# 2.4 Variable labels and units ----
# Pretty labels for plotting (variable_name -> list(label, unit))
VARIABLE_LABELS <- list(
  flow_annual = list(label = "Current", unit = "m\u00b3/s"),
  velocity_annual = list(label = "Flow velocity", unit = "m/s"),
  pH_annual = list(label = "pH", unit = ""),
  temp_annual = list(label = "Temperature", unit = "\u00b0C"),
  EC_annual = list(label = "Electrical conductivity", unit = "\u00b5S/cm"),
  alkalinity_annual = list(label = "Alkalinity", unit = "mmol/L"),
  suspended_solids_annual = list(label = "Suspended solids", unit = "mg/L"),
  DO_mgl_annual = list(label = "Dissolved oxygen", unit = "mg/L"),
  DO_pct_annual = list(label = "Dissolved oxygen", unit = "%"),
  BOD7_annual = list(label = "BOD\u2087", unit = "mg/L"),
  ChDS_C_annual = list(label = "ChDS-C", unit = "mg/L"),
  NH4_N_annual = list(label = "NH\u2084-N", unit = "mg/L"),
  NO2_N_annual = list(label = "NO\u2082-N", unit = "mg/L"),
  NO3_N_annual = list(label = "NO\u2083-N", unit = "mg/L"),
  mineral_N_annual = list(label = "Mineral N", unit = "mg/L"),
  total_N_annual = list(label = "Total N", unit = "mg/L"),
  PO4_P_annual = list(label = "PO\u2084-P", unit = "mg/L"),
  total_P_annual = list(label = "Total P", unit = "mg/L")
)

# Helper function to get pretty label with unit
get_var_label <- function(var_name, include_unit = TRUE) {
  if (var_name %in% names(VARIABLE_LABELS)) {
    info <- VARIABLE_LABELS[[var_name]]
    if (include_unit && nchar(info$unit) > 0) {
      return(paste0(info$label, " (", info$unit, ")"))
    } else {
      return(info$label)
    }
  }
  return(var_name)  # fallback to original name
}

# Helper function to get just the unit
get_var_unit <- function(var_name) {
  if (var_name %in% names(VARIABLE_LABELS)) {
    return(VARIABLE_LABELS[[var_name]]$unit)
  }
  return("")
}

# 2.5 Grid optimization settings ----
# Variable used for grid optimization (choose one with ~100% coverage)
GRID_OPT_VARIABLE <- "temp_annual"  # Options: "temp_annual", "pH_annual", "EC_annual"

# 2.6 CRS settings ----
crs_wgs84  <- 4326
crs_utm34n <- 32634

# 2.7 Kriging settings ----
EXTENT_BUFFER <- 0.2        # Buffer around data extent for prediction grid
LITHUANIA_BUFFER_KM <- 10   # Buffer around Lithuania border (km) to ensure border coverage

#==================== 3. LOAD DATA ========================

raw_data <- read_xlsx(DATA_FILE)

cat("Data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "cols\n")
cat("Years in data:", paste(sort(unique(raw_data[[year_col]])), collapse = ", "), "\n")

#==================== 4. CREATE AVERAGED DATASET FOR GRID OPTIMIZATION ====

cat("Using variable:", GRID_OPT_VARIABLE, "\n")

# Average the grid optimization variable across all years for each unique site
# This uses ALL sites, not just those with complete records
grid_opt_data <- raw_data |>
  filter(!is.na(.data[[GRID_OPT_VARIABLE]])) |>
  group_by(.data[[site_col]], .data[[x_col]], .data[[y_col]]) |>
  summarise(
    grid_opt_value = mean(.data[[GRID_OPT_VARIABLE]], na.rm = TRUE),
    across(all_of(VARIABLES_TO_KRIGE), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

cat("Grid optimization dataset:", nrow(grid_opt_data), "unique sites\n")

#==================== 5. TRANSFORM COORDINATES TO UTM (km) & CENTRE ====

# Transform grid optimization data
grid_opt_sf <- st_as_sf(
  grid_opt_data,
  coords = c(x_col, y_col),
  crs = crs_wgs84
)
grid_opt_sf <- st_transform(grid_opt_sf, crs = crs_utm34n)
grid_opt_coords_utm <- st_coordinates(grid_opt_sf) / 1000  # Convert to km

# Calculate centroid from ALL raw data (for consistency across years)
all_sf <- st_as_sf(raw_data, coords = c(x_col, y_col), crs = crs_wgs84)
all_sf <- st_transform(all_sf, crs = crs_utm34n)
all_coords_utm <- st_coordinates(all_sf) / 1000

x_centroid <- mean(all_coords_utm[, 1])
y_centroid <- mean(all_coords_utm[, 2])

cat("Centroid (UTM km): x =", round(x_centroid, 3), ", y =", round(y_centroid, 3), "\n")

# Centre coordinates
grid_opt_coords_centred <- data.frame(
  x = grid_opt_coords_utm[, 1] - x_centroid,
  y = grid_opt_coords_utm[, 2] - y_centroid
)

#==================== 6. LOAD LITHUANIA BORDER ====================

lith_sf <- geoboundaries(country = "Lithuania", adm_lvl = 0)
lith_union <- st_union(lith_sf)
lith_poly <- st_cast(lith_union, "POLYGON")
lith_poly <- lith_poly[which.max(st_area(lith_poly)), ]
lith_poly_utm <- st_transform(lith_poly, crs = crs_utm34n)

# Create buffered polygon for grid filtering (buffer in meters)
lith_poly_buffered <- st_buffer(lith_poly_utm, dist = LITHUANIA_BUFFER_KM * 1000)

cat("Lithuania border loaded with", LITHUANIA_BUFFER_KM, "km buffer\n")

# Extract coordinates for plotting (in km)
coords_all <- st_coordinates(lith_poly_utm)
outer_ring <- coords_all[coords_all[, "L2"] == 1, c("X", "Y")]
lith_coords_utm <- as.matrix(outer_ring) / 1000

lith_coords_centred <- lith_coords_utm
lith_coords_centred[, 1] <- lith_coords_centred[, 1] - x_centroid
lith_coords_centred[, 2] <- lith_coords_centred[, 2] - y_centroid

plot(lith_coords_centred)

#==================== 7. CALCULATE OPTIMAL GRID (Tisseyre-style) ====================

x_range <- range(grid_opt_coords_centred$x, na.rm = TRUE)
y_range <- range(grid_opt_coords_centred$y, na.rm = TRUE)

bbox_area <- diff(x_range) * diff(y_range)
n_pts <- nrow(grid_opt_coords_centred)

# Point density
r <- n_pts / bbox_area

# Optimal grid length
L0 <- sqrt(1 / r)

# Calculate max distance for constraints
all_dists <- as.vector(dist(grid_opt_coords_centred))
max_dist <- max(all_dists, na.rm = TRUE)

L_min <- max_dist / 100
L_max <- max_dist / 5

L_opt <- max(L_min, min(L0, L_max))

cat("Point density (r):", round(r, 4), "points/km^2\n")
cat("Optimal grid size (L_opt):", round(L_opt, 3), "km\n")
cat("Constraints: L_min =", round(L_min, 3), ", L_max =", round(L_max, 3), "\n")

#==================== 8. CREATE COMMON PREDICTION GRID ====================

# Use full data extent (all years) for grid bounds
full_x_range <- range(all_coords_utm[, 1] - x_centroid, na.rm = TRUE)
full_y_range <- range(all_coords_utm[, 2] - y_centroid, na.rm = TRUE)

x_buffer <- diff(full_x_range) * EXTENT_BUFFER
y_buffer <- diff(full_y_range) * EXTENT_BUFFER

xlim_grid <- c(full_x_range[1] - x_buffer, full_x_range[2] + x_buffer)
ylim_grid <- c(full_y_range[1] - y_buffer, full_y_range[2] + y_buffer)

# Create initial full grid
prediction_grid_full <- expand.grid(
  x = seq(xlim_grid[1], xlim_grid[2], by = L_opt),
  y = seq(ylim_grid[1], ylim_grid[2], by = L_opt)
)

# Convert to absolute UTM coordinates
prediction_grid_full$x_utm <- prediction_grid_full$x + x_centroid
prediction_grid_full$y_utm <- prediction_grid_full$y + y_centroid

cat("Initial grid:", nrow(prediction_grid_full), "cells\n")

plot(prediction_grid_full$x, prediction_grid_full$y, pch = 20, cex = 0.5,
     xlab = "X (km)", ylab = "Y (km)", main = "Initial Prediction Grid")

# Filter grid to cells within buffered Lithuania border
# Create points in meters (multiply km by 1000)
grid_points_m <- data.frame(
  x = prediction_grid_full$x_utm * 1000,
  y = prediction_grid_full$y_utm * 1000
)
grid_sf <- st_as_sf(grid_points_m, coords = c("x", "y"), crs = crs_utm34n)

# Check which grid cells fall within buffered Lithuania
within_lithuania <- st_intersects(grid_sf, lith_poly_buffered, sparse = FALSE)[, 1]

prediction_grid <- prediction_grid_full[within_lithuania, ]

# Re-assign grid cell IDs after filtering
prediction_grid$grid_cell_id <- seq_len(nrow(prediction_grid))

cat("Grid cells within Lithuania (+", LITHUANIA_BUFFER_KM, "km buffer):",
    nrow(prediction_grid), "cells\n")
cat("Grid resolution:", round(L_opt, 3), "km\n")

plot(prediction_grid$x, prediction_grid$y, pch = 20, cex = 0.5,
     xlab = "X (km)", ylab = "Y (km)", main = "Filtered Prediction Grid (within Lithuania)")

#==================== 9. DATA EXPLORATION ====

# Explore data distributions and check for outliers/skewness

cat("\n--- Data Exploration ---\n")
cat("Checking distributions for each variable...\n\n")

for (var in VARIABLES_TO_KRIGE) {
  var_values <- grid_opt_data[[var]]
  var_values <- var_values[!is.na(var_values)]

  var_label <- get_var_label(var, include_unit = TRUE)

  # Basic statistics
  cat(var_label, ":\n")
  cat("  n =", length(var_values), "\n")
  cat("  Range:", round(min(var_values), 3), "-", round(max(var_values), 3), "\n")
  cat("  Mean:", round(mean(var_values), 3), ", Median:", round(median(var_values), 3), "\n")
  cat("  Skewness:", round((mean(var_values) - median(var_values)) / sd(var_values), 3), "(approx)\n\n")
}

# Create distribution plots for each variable
cat("Creating distribution plots...\n")

for (var in VARIABLES_TO_KRIGE) {
  var_values <- grid_opt_data[[var]]
  var_values <- var_values[!is.na(var_values)]
  var_label <- get_var_label(var, include_unit = TRUE)

  outfile <- file.path(PLOTS_DIR, paste0("distribution_", var, ".png"))
  png(outfile, width = 12, height = 4, units = "in", res = 300)
  par(mfrow = c(1, 3))

  # Histogram
  hist(var_values, breaks = 30, main = paste0("Histogram: ", var_label),
       xlab = var_label, col = "steelblue", border = "white")

  # Q-Q plot
  qqnorm(var_values, main = paste0("Q-Q Plot: ", var_label))
  qqline(var_values, col = "red", lwd = 2)

  # Boxplot
  boxplot(var_values, main = paste0("Boxplot: ", var_label),
          ylab = var_label, col = "steelblue")

  dev.off()
}

cat("Distribution plots saved to:", PLOTS_DIR, "\n")

#==================== 10. FIT VARIOGRAM MODELS (POOLED ACROSS YEARS) ====

# It is important to note that when bin values are specified manually, for better visualization or otherwise,
# it should be ensured that the minimum number of pairs in each bin should not be less than 30.

# Model configurations to test
# - cte: constant mean (stationary)
# - 1st: first-order polynomial trend (linear surface)
# - 2nd: second-order polynomial trend (quadratic surface)
# - fix.nugget TRUE: forces nugget = 0 (pure spatial model, no measurement error)
# - fix.nugget FALSE: estimates nugget from data (allows for measurement error/micro-scale variation)
model_configs <- list(
  # Exponential covariance models
  list(name = "exp_cte", cov = "exp", trend = "cte", fix_nugget = FALSE),
  list(name = "exp_1st", cov = "exp", trend = "1st", fix_nugget = FALSE),
  list(name = "exp_2nd", cov = "exp", trend = "2nd", fix_nugget = FALSE),
  list(name = "exp_cte_nug0", cov = "exp", trend = "cte", fix_nugget = TRUE),
  list(name = "exp_1st_nug0", cov = "exp", trend = "1st", fix_nugget = TRUE),
  list(name = "exp_2nd_nug0", cov = "exp", trend = "2nd", fix_nugget = TRUE),
  # Spherical covariance models
  list(name = "sph_cte", cov = "sph", trend = "cte", fix_nugget = FALSE),
  list(name = "sph_1st", cov = "sph", trend = "1st", fix_nugget = FALSE),
  list(name = "sph_2nd", cov = "sph", trend = "2nd", fix_nugget = FALSE),
  list(name = "sph_cte_nug0", cov = "sph", trend = "cte", fix_nugget = TRUE),
  list(name = "sph_1st_nug0", cov = "sph", trend = "1st", fix_nugget = TRUE),
  list(name = "sph_2nd_nug0", cov = "sph", trend = "2nd", fix_nugget = TRUE)
)

# Function to fit best variogram model for a variable using pooled data
fit_best_variogram <- function(data_df, var_name, coords_centred, max_dist_vario) {

  # Prepare geodata
  geo_df <- data.frame(
    x = coords_centred$x,
    y = coords_centred$y,
    z = data_df[[var_name]]
  ) |> na.omit()

  if (nrow(geo_df) < 30) {
    warning(paste("Insufficient data for", var_name, "- skipping"))
    return(NULL)
  }

  geo_obj <- as.geodata(geo_df, coords.col = c("x", "y"), data.col = "z")

  # Fit all models
  results <- lapply(model_configs, function(cfg) {
    tryCatch({
      vario <- variog(geo_obj, trend = cfg$trend, max.dist = max_dist_vario, messages = FALSE)
      fit <- variofit(vario, cov.model = cfg$cov, fix.nugget = cfg$fix_nugget, messages = FALSE)
      list(
        name = cfg$name,
        trend = cfg$trend,
        fix_nugget = cfg$fix_nugget,
        vario = vario,
        fit = fit,
        SSQ = summary(fit)$sum.of.squares
      )
    }, error = function(e) {
      NULL
    })
  })

  # Remove failed fits
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    warning(paste("All model fits failed for", var_name))
    return(NULL)
  }

  # Select best model
  best_idx <- which.min(sapply(results, `[[`, "SSQ"))
  best <- results[[best_idx]]

  cat("  ", var_name, "-> Best model:", best$name,
      "(SSQ =", round(best$SSQ, 4), ")\n")

  return(list(
    variable = var_name,
    model = best$fit,
    trend = best$trend,
    model_name = best$name,
    fix_nugget = best$fix_nugget
  ))
}

# Calculate max distance for variogram fitting
# Rule of thumb: limit variogram to 50-60% of max distance because:
# 1. Fewer point pairs at large distances = unreliable semivariance estimates
# 2. Edge effects at large distances (points can only pair in limited directions)
# 3. The variogram range (where spatial correlation disappears) is typically much smaller
max_dist_vario <- max_dist * 0.6
cat("Max distance for variogram:", round(max_dist_vario, 2), "km\n")

# Fit models for each variable using the averaged (grid optimization) dataset
variogram_models <- list()

for (var in VARIABLES_TO_KRIGE) {
  cat("Fitting variogram for:", var, "\n")

  model_result <- fit_best_variogram(
    data_df = grid_opt_data,
    var_name = var,
    coords_centred = grid_opt_coords_centred,
    max_dist_vario = max_dist_vario
  )

  if (!is.null(model_result)) {
    variogram_models[[var]] <- model_result
  }
}

cat("\nVariogram models fitted for", length(variogram_models), "variables\n")

#==================== 10a. ANISOTROPY CHECK ====

# Check for directional effects (anisotropy) using variog4
# This computes variograms in 4 directions: 0°, 45°, 90°, 135°
# If directional variograms are similar, isotropy assumption is reasonable
# If they differ substantially, anisotropic kriging may be needed

cat("\n--- Anisotropy Check (Directional Variograms) ---\n")
cat("Creating variog4 plots for each variable...\n")

for (var in names(variogram_models)) {
  cat("Checking anisotropy:", var, "\n")

  # Get variable data
  var_values <- grid_opt_data[[var]]
  valid_idx <- !is.na(var_values)

  geo_df <- data.frame(
    x = grid_opt_coords_centred$x[valid_idx],
    y = grid_opt_coords_centred$y[valid_idx],
    z = var_values[valid_idx]
  )

  geo_obj <- as.geodata(geo_df, coords.col = c("x", "y"), data.col = "z")

  # Get the trend used for this variable's best model
  trend_used <- variogram_models[[var]]$trend

  # Compute directional variograms
  v4 <- variog4(geo_obj, trend = trend_used, max.dist = max_dist_vario)

  # Get pretty label
  var_label <- get_var_label(var, include_unit = TRUE)

  # Save plot
  outfile <- file.path(PLOTS_DIR, paste0("anisotropy_", var, ".png"))
  png(outfile, width = 10, height = 8, units = "in", res = 300)
  plot(v4, omnidirectional = TRUE)
  # Add title manually since plot.variog4 doesn't handle main/sub well
  title(main = paste0("Directional Variograms: ", var_label),
        sub = paste0("Trend: ", trend_used, " | If lines differ substantially, anisotropy may be present"),
        cex.main = 1.2)
  dev.off()
}

cat("Anisotropy plots saved to:", PLOTS_DIR, "\n")

#==================== 10b. SPATIAL AUTOCORRELATION ANALYSIS ====

cat("\n--- Spatial Autocorrelation Analysis ---\n")
cat("Calculating Moran's I and variogram-based metrics for each variable...\n\n")

# Create spatial weights matrix for Moran's I (using k-nearest neighbors)
coords_matrix <- as.matrix(grid_opt_coords_centred)
k_neighbors <- 8  # Use 8 nearest neighbors

# Create k-nearest neighbor weights
knn <- knearneigh(coords_matrix, k = k_neighbors)
knn_nb <- knn2nb(knn)
knn_weights <- nb2listw(knn_nb, style = "W")

# Also create distance-based weights for comparison
# Using inverse distance weighting within a threshold
dist_threshold <- L_opt * 3  # 3x grid cell size as neighborhood
dist_nb <- dnearneigh(coords_matrix, d1 = 0, d2 = dist_threshold)
dist_weights <- nb2listw(dist_nb, style = "W", zero.policy = TRUE)

# Initialize results storage
spatial_autocorr_results <- data.frame(

  variable = character(),
  n_obs = integer(),
  morans_I_knn = numeric(),
  morans_I_pvalue = numeric(),
  morans_I_dist = numeric(),
  effective_range_km = numeric(),
  nugget = numeric(),
  partial_sill = numeric(),
  total_sill = numeric(),
  nugget_sill_ratio = numeric(),
  spatial_dependence_index = numeric(),
  best_model = character(),
  trend = character(),
  stringsAsFactors = FALSE
)

# Store variogram data for plotting
variogram_data_list <- list()

for (var in names(variogram_models)) {
  cat("Analyzing:", var, "\n")

  # Get variable values (remove NAs)
  var_values <- grid_opt_data[[var]]
  valid_idx <- !is.na(var_values)
  var_valid <- var_values[valid_idx]

  # Subset coordinates to match valid values
  coords_valid <- coords_matrix[valid_idx, ]

  if (length(var_valid) < 30) {
    cat("  Insufficient data, skipping\n")
    next
  }

  # Calculate Moran's I with k-nearest neighbors
  # Need to recreate weights for subset
  knn_sub <- knearneigh(coords_valid, k = min(k_neighbors, nrow(coords_valid) - 1))
  knn_nb_sub <- knn2nb(knn_sub)
  knn_weights_sub <- nb2listw(knn_nb_sub, style = "W")

  moran_knn <- tryCatch({
    moran.test(var_valid, knn_weights_sub)
  }, error = function(e) NULL)

  # Calculate Moran's I with distance-based weights
  dist_nb_sub <- dnearneigh(coords_valid, d1 = 0, d2 = dist_threshold)

  moran_dist <- tryCatch({
    if (any(card(dist_nb_sub) > 0)) {
      dist_weights_sub <- nb2listw(dist_nb_sub, style = "W", zero.policy = TRUE)
      moran.test(var_valid, dist_weights_sub, zero.policy = TRUE)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Extract variogram parameters from fitted model
  model_info <- variogram_models[[var]]
  fitted_model <- model_info$model

  # Get variogram parameters
  nugget <- fitted_model$nugget
  partial_sill <- fitted_model$cov.pars[1]  # sigmasq (partial sill)

  # Calculate effective range based on model type
  phi <- fitted_model$cov.pars[2]  # phi parameter
  cov_model <- fitted_model$cov.model

  if (cov_model == "exponential") {
    # For exponential: effective range ≈ 3 * phi (where correlation drops to ~5%)
    effective_range <- 3 * phi
  } else if (cov_model == "spherical") {
    # For spherical: range = phi (hard cutoff)
    effective_range <- phi
  } else {
    effective_range <- phi
  }

  total_sill <- nugget + partial_sill
  nugget_sill_ratio <- ifelse(total_sill > 0, nugget / total_sill, NA)

  # Spatial Dependence Index (SDI): 1 - nugget/sill
  # Higher values indicate stronger spatial structure
  sdi <- ifelse(total_sill > 0, 1 - nugget_sill_ratio, NA)

  # Store empirical variogram for plotting
  geo_df <- data.frame(
    x = coords_valid[, 1],
    y = coords_valid[, 2],
    z = var_valid
  )
  geo_obj <- as.geodata(geo_df, coords.col = c("x", "y"), data.col = "z")
  emp_vario <- variog(geo_obj, trend = model_info$trend, max.dist = max_dist_vario, messages = FALSE)

  variogram_data_list[[var]] <- list(
    empirical = emp_vario,
    fitted = fitted_model,
    model_name = model_info$model_name
  )

  # Add results to table
  spatial_autocorr_results <- rbind(spatial_autocorr_results, data.frame(
    variable = var,
    n_obs = length(var_valid),
    morans_I_knn = ifelse(!is.null(moran_knn), round(moran_knn$estimate["Moran I statistic"], 4), NA),
    morans_I_pvalue = ifelse(!is.null(moran_knn), signif(moran_knn$p.value, 4), NA),
    morans_I_dist = ifelse(!is.null(moran_dist), round(moran_dist$estimate["Moran I statistic"], 4), NA),
    effective_range_km = round(effective_range, 2),
    nugget = round(nugget, 4),
    partial_sill = round(partial_sill, 4),
    total_sill = round(total_sill, 4),
    nugget_sill_ratio = round(nugget_sill_ratio, 4),
    spatial_dependence_index = round(sdi, 4),
    best_model = model_info$model_name,
    trend = model_info$trend
  ))

  cat("  Moran's I (knn):",
      ifelse(!is.null(moran_knn), round(moran_knn$estimate["Moran I statistic"], 3), "NA"),
      "| Effective range:", round(effective_range, 1), "km",
      "| SDI:", round(sdi, 3), "\n")
}

# Print spatial autocorrelation summary
cat("\n========== SPATIAL AUTOCORRELATION SUMMARY ==========\n")
cat("\nMoran's I interpretation:\n")
cat("  > 0: Positive spatial autocorrelation (similar values cluster)\n")
cat("  ≈ 0: Random spatial pattern\n")
cat("  < 0: Negative spatial autocorrelation (dissimilar values cluster)\n")
cat("\nSpatial Dependence Index (SDI) interpretation:\n")
cat("  > 0.75: Strong spatial structure (good for typology)\n")
cat("  0.50-0.75: Moderate spatial structure\n")
cat("  0.25-0.50: Weak spatial structure\n")
cat("  < 0.25: Very weak/random (may not add value to typology)\n")
cat("\n")

# Sort by SDI descending
spatial_autocorr_results <- spatial_autocorr_results[order(-spatial_autocorr_results$spatial_dependence_index), ]

# Print summary table
print_cols <- c("variable", "morans_I_knn", "morans_I_pvalue", "effective_range_km",
                "nugget_sill_ratio", "spatial_dependence_index")
print(spatial_autocorr_results[, print_cols], row.names = FALSE)

# Classification for typology suitability
cat("\n--- Variable Classification for Typology ---\n")
for (i in seq_len(nrow(spatial_autocorr_results))) {
  var <- spatial_autocorr_results$variable[i]
  var_label <- get_var_label(var, include_unit = TRUE)
  sdi <- spatial_autocorr_results$spatial_dependence_index[i]
  moran <- spatial_autocorr_results$morans_I_knn[i]
  pval <- spatial_autocorr_results$morans_I_pvalue[i]
  range_km <- spatial_autocorr_results$effective_range_km[i]

  if (!is.na(sdi) && sdi > 0.75) {
    suitability <- "STRONG - Recommended for typology"
  } else if (!is.na(sdi) && sdi > 0.50) {
    suitability <- "MODERATE - Consider including"
  } else if (!is.na(sdi) && sdi > 0.25) {
    suitability <- "WEAK - Include with caution"
  } else {
    suitability <- "VERY WEAK - May not improve typology"
  }

  sig <- ifelse(!is.na(pval) && pval < 0.001, "***",
                ifelse(!is.na(pval) && pval < 0.01, "**",
                       ifelse(!is.na(pval) && pval < 0.05, "*", "")))

  cat(sprintf("  %-35s SDI=%.3f, Moran's I=%.3f%s, Range=%.0fkm -> %s\n",
              var_label, sdi, moran, sig, range_km, suitability))
}

# Save spatial autocorrelation results
autocorr_outfile <- file.path(OUTPUT_DIR, "kriging_spatial_autocorrelation.xlsx")
write_xlsx(spatial_autocorr_results, autocorr_outfile)
cat("\nSpatial autocorrelation results saved to:", autocorr_outfile, "\n")

# Create variogram comparison plot
cat("\n--- Creating variogram plots ---\n")

# Individual variogram plots
for (var in names(variogram_data_list)) {
  cat("Plotting variogram:", var, "\n")

  vario_data <- variogram_data_list[[var]]
  emp_vario <- vario_data$empirical
  fitted_model <- vario_data$fitted

  # Create plot data
  vario_df <- data.frame(
    distance = emp_vario$u,
    semivariance = emp_vario$v,
    n_pairs = emp_vario$n
  )

  # Generate fitted line
  dist_seq <- seq(0, max(emp_vario$u), length.out = 100)

  # Calculate fitted values manually based on model type
  nugget <- fitted_model$nugget
  psill <- fitted_model$cov.pars[1]
  phi <- fitted_model$cov.pars[2]

  if (fitted_model$cov.model == "exponential") {
    fitted_vals <- nugget + psill * (1 - exp(-dist_seq / phi))
  } else if (fitted_model$cov.model == "spherical") {
    fitted_vals <- ifelse(dist_seq <= phi,
                          nugget + psill * (1.5 * dist_seq / phi - 0.5 * (dist_seq / phi)^3),
                          nugget + psill)
  } else {
    fitted_vals <- nugget + psill * (1 - exp(-dist_seq / phi))
  }

  fitted_df <- data.frame(distance = dist_seq, semivariance = fitted_vals)

  # Get metrics for subtitle
  metrics <- spatial_autocorr_results[spatial_autocorr_results$variable == var, ]

  # Get pretty label for title
 var_label <- get_var_label(var, include_unit = TRUE)

  p <- ggplot() +
    geom_point(data = vario_df, aes(x = distance, y = semivariance, size = n_pairs),
               alpha = 0.7, color = "darkblue") +
    geom_line(data = fitted_df, aes(x = distance, y = semivariance),
              color = "red", linewidth = 1.2) +
    geom_hline(yintercept = nugget + psill, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = nugget, linetype = "dotted", color = "grey50") +
    scale_size_continuous(name = "Point pairs", range = c(2, 8)) +
    labs(
      title = paste0("Empirical & Fitted Variogram: ", var_label),
      subtitle = paste0("Model: ", vario_data$model_name,
                        " | Range: ", metrics$effective_range_km, " km",
                        " | SDI: ", metrics$spatial_dependence_index,
                        " | Moran's I: ", metrics$morans_I_knn),
      x = "Distance (km)",
      y = "Semivariance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )

  outfile <- file.path(PLOTS_DIR, paste0("variogram_", var, ".png"))
  ggsave(outfile, p, width = 10, height = 6, dpi = 300, bg = "white")
}

# Create summary bar plot comparing SDI across variables
cat("Creating spatial autocorrelation summary plot...\n")

sdi_plot_data <- spatial_autocorr_results |>
  mutate(
    var_label = sapply(variable, function(v) get_var_label(v, include_unit = TRUE)),
    suitability = case_when(
      spatial_dependence_index > 0.75 ~ "Strong",
      spatial_dependence_index > 0.50 ~ "Moderate",
      spatial_dependence_index > 0.25 ~ "Weak",
      TRUE ~ "Very Weak"
    ),
    suitability = factor(suitability, levels = c("Strong", "Moderate", "Weak", "Very Weak"))
  )

p_sdi <- ggplot(sdi_plot_data, aes(x = reorder(var_label, spatial_dependence_index),
                                    y = spatial_dependence_index, fill = suitability)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = c(0.25, 0.50, 0.75), linetype = "dashed", color = "grey40", alpha = 0.7) +
  scale_fill_manual(values = c("Strong" = "#2E7D32", "Moderate" = "#FFA000",
                               "Weak" = "#F57C00", "Very Weak" = "#D32F2F"),
                    name = "Spatial\nStructure") +
  coord_flip() +
  labs(
    title = "Spatial Dependence Index (SDI) by Variable",
    subtitle = "Higher SDI indicates stronger spatial structure (better for river typology)",
    x = "",
    y = "Spatial Dependence Index (1 - nugget/sill)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  ) +
  annotate("text", x = 0.5, y = 0.75, label = "Strong", hjust = 0, size = 3, color = "grey30") +
  annotate("text", x = 0.5, y = 0.50, label = "Moderate", hjust = 0, size = 3, color = "grey30") +
  annotate("text", x = 0.5, y = 0.25, label = "Weak", hjust = 0, size = 3, color = "grey30")

outfile <- file.path(PLOTS_DIR, "spatial_autocorrelation_summary.png")
ggsave(outfile, p_sdi, width = 10, height = 6, dpi = 300, bg = "white")

cat("Spatial autocorrelation plots saved to:", PLOTS_DIR, "\n")

#==================== 11. PERFORM KRIGING FOR EACH YEAR AND VARIABLE ====

cat("\n--- Performing kriging for each year and variable ---\n")

years <- sort(unique(raw_data[[year_col]]))
cat("Years to process:", paste(years, collapse = ", "), "\n\n")

# Initialize results storage
all_results <- list()

# Add UTM coordinates to raw data
raw_data$x_utm <- all_coords_utm[, 1]
raw_data$y_utm <- all_coords_utm[, 2]
raw_data$x_centred <- raw_data$x_utm - x_centroid
raw_data$y_centred <- raw_data$y_utm - y_centroid

for (yr in years) {
  cat("Processing year:", yr, "\n")

  year_data <- raw_data |>
    filter(.data[[year_col]] == yr)

  year_results <- data.frame(
    grid_cell_id = prediction_grid$grid_cell_id,
    x_utm = prediction_grid$x_utm,
    y_utm = prediction_grid$y_utm,
    year = yr
  )

  for (var in names(variogram_models)) {
    # Prepare data for this variable
    var_data <- year_data |>
      dplyr::select(x_centred, y_centred, all_of(var)) |>
      rename(x = x_centred, y = y_centred, z = all_of(var)) |>
      na.omit() |>
      group_by(x, y) |>
      summarise(z = mean(z, na.rm = TRUE), .groups = "drop")

    if (nrow(var_data) < 20) {
      cat("  ", var, "- insufficient data (n =", nrow(var_data), "), skipping\n")
      year_results[[var]] <- NA
      next
    }

    # Create geodata object
    geo_obj <- as.geodata(var_data, coords.col = c("x", "y"), data.col = "z")

    # Get the fitted model for this variable
    model_info <- variogram_models[[var]]

    # Perform kriging
    tryCatch({
      krig_result <- krige.conv(
        geo_obj,
        loc = prediction_grid[, c("x", "y")],
        krige = krige.control(
          obj.model = model_info$model,
          trend.d = model_info$trend,
          trend.l = model_info$trend
        )
      )

      year_results[[var]] <- krig_result$predict
      cat("  ", var, "- done (n =", nrow(var_data), "points)\n")

    }, error = function(e) {
      cat("  ", var, "- ERROR:", conditionMessage(e), "\n")
      year_results[[var]] <<- NA
    })
  }

  all_results[[as.character(yr)]] <- year_results
  cat("\n")
}

#==================== 12. COMBINE ANNUAL RESULTS ====

cat("--- Combining results ---\n")

results_df <- bind_rows(all_results)

cat("Combined results:", nrow(results_df), "rows x", ncol(results_df), "cols\n")

#==================== 13. CALCULATE MULTI-YEAR AVERAGED INTERPOLATIONS ====

cat("\n--- Calculating multi-year averages ---\n")

averaged_results <- results_df |>
  group_by(grid_cell_id, x_utm, y_utm) |>
  summarise(
    across(all_of(names(variogram_models)), ~ mean(.x, na.rm = TRUE)),
    n_years = sum(!is.na(.data[[names(variogram_models)[1]]])),
    .groups = "drop"
  )

cat("Averaged results:", nrow(averaged_results), "grid cells\n")

#==================== 14. EXPORT RESULTS ====

cat("\n--- Saving results to Excel ---\n")

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Save annual results
annual_outfile <- file.path(OUTPUT_DIR, "kriging_annual_interpolations.xlsx")
write_xlsx(results_df, annual_outfile)
cat("Annual results saved to:", annual_outfile, "\n")

# Save averaged results
averaged_outfile <- file.path(OUTPUT_DIR, "kriging_averaged_interpolations.xlsx")
write_xlsx(averaged_results, averaged_outfile)
cat("Averaged results saved to:", averaged_outfile, "\n")

# Save grid metadata
grid_metadata <- data.frame(
  parameter = c(
    "grid_resolution_km",
    "x_centroid_utm_km",
    "y_centroid_utm_km",
    "n_grid_cells",
    "n_years",
    "grid_opt_variable",
    "n_sites_for_grid_opt",
    "lithuania_buffer_km",
    "crs_utm"
  ),
  value = c(
    round(L_opt, 4),
    round(x_centroid, 4),
    round(y_centroid, 4),
    nrow(prediction_grid),
    length(years),
    GRID_OPT_VARIABLE,
    nrow(grid_opt_data),
    LITHUANIA_BUFFER_KM,
    crs_utm34n
  )
)

metadata_outfile <- file.path(OUTPUT_DIR, "kriging_grid_metadata.xlsx")
write_xlsx(grid_metadata, metadata_outfile)
cat("Grid metadata saved to:", metadata_outfile, "\n")

#==================== 15. PLOT AVERAGED INTERPOLATIONS ====

cat("\n--- Creating averaged interpolation plots ---\n")

if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)

# Calculate plot limits from Lithuania border (in km)
xlim_plot <- range(lith_coords_utm[, 1]) + c(-10, 10)
ylim_plot <- range(lith_coords_utm[, 2]) + c(-10, 10)

for (var in names(variogram_models)) {
  cat("Plotting:", var, "\n")

  # Get pretty label with unit for legend
  legend_label <- get_var_label(var, include_unit = TRUE)

  # Create plot data (in km for geom_tile)
  plot_data <- data.frame(
    x = averaged_results$x_utm,
    y = averaged_results$y_utm,
    z = averaged_results[[var]]
  )

  # Calculate nice breaks for the legend
  z_range <- range(plot_data$z, na.rm = TRUE)
  z_breaks <- pretty(z_range, n = 4)

  # Create Lithuania border data frame for geom_path (in km)
  lith_border_df <- data.frame(
    x = lith_coords_utm[, 1],
    y = lith_coords_utm[, 2]
  )

  # Create ggplot matching the reference style
  p <- ggplot() +
    geom_tile(
      data = plot_data,
      aes(x = x, y = y, fill = z),
      width = L_opt,
      height = L_opt
    ) +
    geom_path(
      data = lith_border_df,
      aes(x = x, y = y),
      colour = "black",
      linewidth = 0.5
    ) +
    coord_cartesian(
      xlim = xlim_plot,
      ylim = ylim_plot,
      expand = FALSE
    ) +
    scale_fill_viridis_c(
      option = "viridis",
      name = legend_label,
      breaks = z_breaks,
      guide = guide_colorbar(
        barwidth = 20,
        barheight = 0.5,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0.5
      )
    ) +
    labs(x = "Easting (km)", y = "Northing (km)") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
      legend.box.spacing = unit(0.5, "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "black")
    )

  # Save plot
  outfile <- file.path(PLOTS_DIR, paste0("kriging_averaged_", var, ".png"))
  ggsave(outfile, p, width = 10, height = 8, dpi = 450, bg = "white")
}

cat("\nPlots saved to:", PLOTS_DIR, "\n")

#==================== 16. CROSS-VALIDATION ANALYSIS ====

cat("\n--- Cross-validation: Observed vs Interpolated ---\n")

# For each variable, compare observed site averages with interpolated values
# at the nearest grid cell

# Create averaged observed data per site (across all years)
observed_site_avg <- raw_data |>
  group_by(.data[[site_col]], .data[[x_col]], .data[[y_col]]) |>
  summarise(
    across(all_of(names(variogram_models)), ~ mean(.x, na.rm = TRUE)),
    x_utm = first(x_utm),
    y_utm = first(y_utm),
    .groups = "drop"
  )

# For each observed site, find the nearest grid cell and extract interpolated value
# Convert observed sites to sf
obs_sf <- st_as_sf(
  observed_site_avg,
  coords = c("x_utm", "y_utm"),
  crs = crs_utm34n
)
st_geometry(obs_sf) <- st_geometry(obs_sf) * 1000  # Convert to meters

# Convert averaged results grid to sf
grid_avg_sf <- st_as_sf(
  averaged_results,
  coords = c("x_utm", "y_utm"),
  crs = crs_utm34n
)
st_geometry(grid_avg_sf) <- st_geometry(grid_avg_sf) * 1000

# Find nearest grid cell for each observation
nearest_idx <- st_nearest_feature(obs_sf, grid_avg_sf)

# Build cross-validation results
cv_results <- list()
cv_metrics <- data.frame(
  variable = character(),
  n_obs = integer(),
  pearson_r = numeric(),
  spearman_rho = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(variogram_models)) {
  observed_vals <- observed_site_avg[[var]]
  interpolated_vals <- averaged_results[[var]][nearest_idx]

  # Remove NAs
  valid_idx <- !is.na(observed_vals) & !is.na(interpolated_vals)
  obs_valid <- observed_vals[valid_idx]
  int_valid <- interpolated_vals[valid_idx]

  if (length(obs_valid) < 10) {
    cat("  ", var, "- insufficient valid pairs, skipping\n")
    next
  }

  # Calculate metrics
  pearson_r <- cor(obs_valid, int_valid, method = "pearson")
  spearman_rho <- cor(obs_valid, int_valid, method = "spearman")
  rmse <- sqrt(mean((obs_valid - int_valid)^2))
  mae <- mean(abs(obs_valid - int_valid))

  cv_metrics <- rbind(cv_metrics, data.frame(
    variable = var,
    n_obs = length(obs_valid),
    pearson_r = round(pearson_r, 4),
    spearman_rho = round(spearman_rho, 4),
    RMSE = round(rmse, 4),
    MAE = round(mae, 4)
  ))

  # Store for plotting
  cv_results[[var]] <- data.frame(
    observed = obs_valid,
    interpolated = int_valid
  )

  cat("  ", var, ": r =", round(pearson_r, 3),
      ", rho =", round(spearman_rho, 3),
      ", RMSE =", round(rmse, 3), "\n")
}

# Print cross-validation metrics table
cat("\nCross-validation metrics summary:\n")
print(cv_metrics, row.names = FALSE)

# Save cross-validation metrics
cv_outfile <- file.path(OUTPUT_DIR, "kriging_cross_validation_metrics.xlsx")
write_xlsx(cv_metrics, cv_outfile)
cat("\nCross-validation metrics saved to:", cv_outfile, "\n")

# Create cross-validation plots (observed vs interpolated)
cat("\n--- Creating cross-validation plots ---\n")

for (var in names(cv_results)) {
  cat("Plotting CV:", var, "\n")

  # Get pretty labels
  var_label <- get_var_label(var, include_unit = TRUE)

  cv_data <- cv_results[[var]]
  metrics <- cv_metrics[cv_metrics$variable == var, ]

  p <- ggplot(cv_data, aes(x = observed, y = interpolated)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "lm", color = "blue", se = TRUE, alpha = 0.2) +
    labs(
      title = paste0("Cross-validation: ", var_label),
      subtitle = paste0("r = ", metrics$pearson_r,
                        " | rho = ", metrics$spearman_rho,
                        " | RMSE = ", metrics$RMSE,
                        " | n = ", metrics$n_obs),
      x = "Observed (site average across years)",
      y = "Interpolated (grid cell average)"
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )

  outfile <- file.path(PLOTS_DIR, paste0("kriging_cv_", var, ".png"))
  ggsave(outfile, p, width = 8, height = 8, dpi = 450, bg = "white")
}

cat("Cross-validation plots saved to:", PLOTS_DIR, "\n")

#==================== 17. SUMMARY ====

cat("\n==================== SUMMARY ====================\n")
cat("Variables interpolated:", paste(names(variogram_models), collapse = ", "), "\n")
cat("Years processed:", length(years), "(", min(years), "-", max(years), ")\n")
cat("Grid cells:", nrow(prediction_grid), "\n")
cat("Grid resolution:", round(L_opt, 3), "km\n")
cat("\nVariogram models used:\n")
for (var in names(variogram_models)) {
  cat("  ", var, ":", variogram_models[[var]]$model_name, "\n")
}
cat("\nOutput files:\n")
cat("  Annual interpolations:", annual_outfile, "\n")
cat("  Averaged interpolations:", averaged_outfile, "\n")
cat("  Grid metadata:", metadata_outfile, "\n")
cat("  Plots directory:", PLOTS_DIR, "\n")
cat("================================================\n")

