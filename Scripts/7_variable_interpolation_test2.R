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

# Get Lithuania boundary
lithuania_boundary <- gb_adm0(country = "Lithuania") |>
  st_transform(crs = 3346)  # Transform to LKS94

# Load rivers
gdb_path <- "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/Lithuanian data/Geographic data/UETK_2024-05-02.gdb/UETK_2024-05-02.gdb"
rivers <- st_read(gdb_path, layer = "upes_l")

# Create interpolation grid
grid_lithuania <- st_make_grid(
  lithuania_boundary,
  cellsize = 2500,  # 2.5km grid cells
  what = "centers"
) |>
  st_as_sf() |>
  st_filter(lithuania_boundary)

cat("Grid points for interpolation:", nrow(grid_lithuania), "\n\n")

# ========================================
# IMPROVED INTERPOLATION FUNCTION
# ========================================

interpolate_variable <- function(data, variable_name, grid, years = NULL, log_transform = FALSE) {

  cat("\n========================================\n")
  cat("Interpolating:", variable_name, "\n")
  cat("========================================\n")

  # Prepare data for the specified variable
  var_data <- data |>
    select(site_id, year, latitude, longitude, elevation, all_of(variable_name)) |>
    rename(var_value = all_of(variable_name)) |>
    filter(!is.na(var_value))

  # Check data availability
  if(nrow(var_data) == 0) {
    cat("No data available for", variable_name, "\n")
    return(NULL)
  }

  # Apply log transformation if needed
  if(log_transform) {
    cat("Applying log transformation...\n")
    var_data$var_value <- log1p(var_data$var_value)  # log1p handles zeros
  }

  # Use specified years or all available years
  if(is.null(years)) {
    years <- sort(unique(var_data$year))
  }

  cat("Years to process:", paste(years, collapse = ", "), "\n")
  cat("Sites with data:", n_distinct(var_data$site_id), "\n")
  cat("Total observations:", nrow(var_data), "\n")
  cat("Value range:", round(range(var_data$var_value, na.rm = TRUE), 3), "\n\n")

  # Store predictions for each year
  var_predictions <- list()

  for(yr in years) {
    year_data <- var_data |> filter(year == yr)

    if(nrow(year_data) < 10) {
      cat("  Year", yr, "- Insufficient data (n =", nrow(year_data), "), skipping...\n")
      next
    }

    cat("  Processing year", yr, "(n =", nrow(year_data), ")...")

    tryCatch({
      # Convert to spatial object
      year_sf <- year_data |>
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
        st_transform(crs = 3346)

      # Extract coordinates
      coords <- st_coordinates(year_sf)
      year_sf$X <- coords[,1]
      year_sf$Y <- coords[,2]

      # Check variance
      data_var <- var(year_sf$var_value)

      # Prepare grid for interpolation
      grid_coords <- st_coordinates(grid)
      grid_df <- st_drop_geometry(grid)
      grid_df$X <- grid_coords[,1]
      grid_df$Y <- grid_coords[,2]
      grid_sf <- st_as_sf(grid_df, coords = c("X", "Y"), crs = 3346)

      if(data_var == 0 || is.na(data_var) || data_var < 1e-10) {
        cat(" (no variance, using IDW)...")
        idw_result <- idw(var_value ~ 1, year_sf, grid_sf, idp = 2)
        var_predictions[[as.character(yr)]] <- idw_result$var1.pred
      } else {
        # Try kriging
        v_emp <- variogram(var_value ~ 1, year_sf,
                          cutoff = 80000,
                          width = 4000)

        # Calculate initial values
        max_gamma <- max(v_emp$gamma)
        range_init <- v_emp$dist[which.max(v_emp$gamma > 0.95 * max_gamma)][1]
        if(is.na(range_init) || range_init <= 0) range_init <- 40000

        # Ensure positive values
        psill_val <- max(0.8 * data_var, 0.001)
        nugget_val <- max(0.2 * data_var, 0)
        range_val <- max(range_init/3, 1000)

        v_fit <- NULL

        # Try to fit variogram
        v_fit <- tryCatch({
          vgm_exp <- vgm(psill = psill_val,
                        "Exp",
                        range = range_val,
                        nugget = nugget_val)
          fit.variogram(v_emp, vgm_exp)
        }, error = function(e) {
          NULL
        }, warning = function(w) {
          # Try even with warnings
          suppressWarnings(fit.variogram(v_emp, vgm(psill = psill_val, "Exp", range = range_val, nugget = nugget_val)))
        })

        # Check if fit is valid
        if(is.null(v_fit) || any(v_fit$range < 0)) {
          cat(" (invalid variogram, using IDW)...")
          idw_result <- idw(var_value ~ 1, year_sf, grid_sf, idp = 2)
          var_predictions[[as.character(yr)]] <- idw_result$var1.pred
        } else {
          # Perform kriging
          krig <- tryCatch({
            krige(var_value ~ 1,
                  year_sf,
                  grid_sf,
                  model = v_fit,
                  debug.level = 0)
          }, error = function(e) {
            cat(" (kriging failed, using IDW)...")
            idw(var_value ~ 1, year_sf, grid_sf, idp = 2)
          })

          var_predictions[[as.character(yr)]] <- krig$var1.pred
        }
      }
      cat(" Done!\n")

    }, error = function(e) {
      cat(" Failed:", e$message, "\n")
    })
  }

  if(length(var_predictions) == 0) {
    cat("No successful interpolations for", variable_name, "\n")
    return(NULL)
  }

  # Combine predictions into matrix
  var_matrix <- do.call(cbind, var_predictions)
  colnames(var_matrix) <- names(var_predictions)

  # Back-transform if log was used
  if(log_transform) {
    var_matrix <- expm1(var_matrix)  # Inverse of log1p
  }

  # Create result dataframe with statistics
  result <- grid

  # Calculate long-term statistics
  clean_name <- gsub("\\[|\\]|\\.|/|°C|%|µS|cm|mg|L|m3|s", "", variable_name)
  clean_name <- gsub("_ann$", "", clean_name)

  result[[paste0(clean_name, "_mean")]] <- rowMeans(var_matrix, na.rm = TRUE)
  result[[paste0(clean_name, "_sd")]] <- apply(var_matrix, 1, sd, na.rm = TRUE)
  result[[paste0(clean_name, "_min")]] <- apply(var_matrix, 1, min, na.rm = TRUE)
  result[[paste0(clean_name, "_max")]] <- apply(var_matrix, 1, max, na.rm = TRUE)
  result[[paste0(clean_name, "_cv")]] <- (result[[paste0(clean_name, "_sd")]] / result[[paste0(clean_name, "_mean")]]) * 100
  result[[paste0(clean_name, "_range")]] <- result[[paste0(clean_name, "_max")]] - result[[paste0(clean_name, "_min")]]

  # Add individual years
  for(yr in names(var_predictions)) {
    result[[paste0(clean_name, "_", yr)]] <- var_matrix[, yr]
  }

  cat("\nSummary for", variable_name, ":\n")
  cat("  Mean range:", round(range(result[[paste0(clean_name, "_mean")]], na.rm = TRUE), 3), "\n")
  cat("  CV range:", round(range(result[[paste0(clean_name, "_cv")]], na.rm = TRUE), 1), "%\n")

  return(result)
}

# ========================================
# INTERPOLATE ALL VARIABLES
# ========================================

cat("\nStarting interpolation of all variables...\n")
start_time_all <- Sys.time()

# Initialize results with grid
grid_results <- grid_lithuania

# Temperature first
temp_result <- interpolate_variable(complete_data, "temp[°C]_ann", grid_lithuania)
if(!is.null(temp_result)) {
  data_cols <- names(temp_result)[!names(temp_result) %in% c("x", "geometry")]
  for(col in data_cols) {
    grid_results[[col]] <- temp_result[[col]]
  }
}

# List of other variables
variables_to_interpolate <- c(
  "pH_ann",
  "D.O.[mg/l]_ann",
  "D.O.[%]_ann",
  "BOD7[mg/L]_ann",
  "suspended_solids[mg/l]_ann",
  "NH4_N[mg/L]_ann",
  "NO2_N[mg/L]_ann",
  "NO3_N[mg/L]_ann",
  "mineral_N[mg/L]_ann",
  "total_N[mg/L]_ann",
  "PO4_P[mg/L]_ann",
  "total_P[mg/L]_ann",
  "EC[µS/cm]_ann",
  "alkalinity[mmol/l]_ann"
)

# Variables that need log transformation due to high skewness
log_transform_vars <- c(
  "flow[m3/s]_ann",
  "velocity[m/s]_ann")

# Check which variables exist
available_vars <- variables_to_interpolate[variables_to_interpolate %in% names(complete_data)]
available_log_vars <- log_transform_vars[log_transform_vars %in% names(complete_data)]

cat("\nVariables available for interpolation:\n")
cat("Regular:", paste(available_vars, collapse = ", "), "\n")
cat("Log-transformed:", paste(available_log_vars, collapse = ", "), "\n\n")

# Interpolate regular variables
for(var in available_vars) {
  result <- interpolate_variable(complete_data, var, grid_lithuania, log_transform = FALSE)
  if(!is.null(result)) {
    data_cols <- names(result)[!names(result) %in% c("x", "geometry")]
    for(col in data_cols) {
      grid_results[[col]] <- result[[col]]
    }
  }
}

# Interpolate log-transformed variables
for(var in available_log_vars) {
  result <- interpolate_variable(complete_data, var, grid_lithuania, log_transform = TRUE)
  if(!is.null(result)) {
    data_cols <- names(result)[!names(result) %in% c("x", "geometry")]
    for(col in data_cols) {
      grid_results[[col]] <- result[[col]]
    }
  }
}

end_time_all <- Sys.time()
cat("\n\nTotal processing time for all variables:",
    round(difftime(end_time_all, start_time_all, units = "mins"), 1), "minutes\n")

# ========================================
# VISUALIZATIONS
# ========================================

# Create summary plots for key variables
p1 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = temp_mean), size = 0.3) +
  scale_color_viridis_c(name = "Temp (°C)") +
  theme_minimal() +
  labs(title = "Temperature")

p2 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = pH_mean), size = 0.3) +
  scale_color_viridis_c(name = "pH", option = "plasma") +
  theme_minimal() +
  labs(title = "pH")

p3 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = total_N_mean), size = 0.3) +
  scale_color_viridis_c(name = "N (mg/L)", option = "magma") +
  theme_minimal() +
  labs(title = "Total Nitrogen")

p4 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = total_P_mean), size = 0.3) +
  scale_color_viridis_c(name = "P (mg/L)", option = "turbo") +
  theme_minimal() +
  labs(title = "Total Phosphorus")

p5 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = flow_mean), size = 0.3) +
  scale_color_viridis_c(name = "flow (m3/s)", option = "turbo") +
  theme_minimal() +
  labs(title = "Flow")

p6 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = EC_mean), size = 0.3) +
  scale_color_viridis_c(name = "EC (uS/cm)", option = "magma") +
  theme_minimal() +
  labs(title = "Electrical conductivity")

p7 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = velocitym_mean), size = 0.3) +
  scale_color_viridis_c(name = "velocity (m/s)", option = "turbo") +
  theme_minimal() +
  labs(title = "Velocity")

p8 <- ggplot() +
  geom_sf(data = lithuania_boundary, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = grid_results, aes(color = alkalinitymmoll_mean), size = 0.3) +
  scale_color_viridis_c(name = "Alkalinity (mmol)", option = "magma") +
  theme_minimal() +
  labs(title = "Alkalinity")

combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8)

# ========================================
# SAVE OUTPUTS
# ========================================

# Save combined plot
ggsave(combined_plot,
       filename = "Plots/Figure5_all_variables_interpolation_summary.png",
       width = 12, height = 10, dpi = 450, bg = "white")

# Save complete interpolated dataset as RDS
saveRDS(grid_results, "Output data/4_all_variables_interpolation_2009-2023.rds")

# Save as CSV (note: will lose geometry)
grid_csv <- st_drop_geometry(grid_results)
write.csv(grid_csv, "Output data/4_all_variables_interpolation_2009-2023.csv", row.names = FALSE)

# Print summary of what was interpolated
cat("\nFinal dataset contains interpolations for:\n")
interpolated_vars <- unique(gsub("_mean|_sd|_cv|_min|_max|_range|_20[0-9]{2}", "",
                                 names(grid_results)[!names(grid_results) %in% c("x", "geometry")]))
print(interpolated_vars)
