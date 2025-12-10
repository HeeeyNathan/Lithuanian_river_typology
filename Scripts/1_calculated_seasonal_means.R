############################################################
# CALCULATE ANNUAL MEANS FROM RAW ENVIRONMENTAL DATA
# - Input data: Input data/2_environmental_data_2009-2023.xlsx
# - Source: Lithuanian Environmental Protection Agency WFD water quality monitoring
# - Outputs:
#     * Spreadsheet containing calculated annual environmental means for 2009-2023 and various metadata
############################################################

# =============================================================================
# LOAD PACKAGES
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)

# =============================================================================
# DATA LOADING
# =============================================================================

# Define column types for all 35 columns
col_types <- c(
  "text", "text", "text", "numeric", "numeric", "text", "text", "text",
  "text", "text", "numeric", "logical", "date", "numeric", "numeric",
  "numeric", "text", "numeric", "numeric", "numeric", "numeric",
  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
  "numeric", "numeric", "numeric"
)

# Load data
env_data <- read_excel("Input data/2_environmental_data_2009-2023.xlsx", col_types = col_types)

# =============================================================================
# SIMPLE SEASONAL WEIGHTED MEANS
# =============================================================================

calculate_seasonal_annual_means <- function(data) {

  # Define environmental variables
  env_variables <- c(
    "flow[m3/s]", "velocity[m/s]", "suspended_solids[mg/l]", "pH", "temp[°C]",
    "D.O.[mg/l]", "D.O.[%]", "BOD7[mg/L]", "ChDS_C[mg/L]", "NH4_N[mg/L]",
    "NO2_N[mg/L]", "NO3_N[mg/L]", "mineral_N[mg/L]", "total_N[mg/L]",
    "PO4_P[mg/L]", "total_P[mg/L]", "EC[µS/cm]", "alkalinity[mmol/l]"
  )

  # Step 1: Add season classification and calculate seasonal means
  seasonal_data <- data |>
    filter(if_any(all_of(env_variables), ~ !is.na(.))) |>
    mutate(
      season = case_when(
        month %in% c(12, 1, 2) ~ "winter",
        month %in% c(3, 4, 5) ~ "spring",
        month %in% c(6, 7, 8) ~ "summer",
        month %in% c(9, 10, 11) ~ "autumn"
      )
    ) |>
    filter(!is.na(season)) |>
    group_by(site_id, year, season) |>
    summarise(
      # Calculate seasonal means for each variable
      flow_seasonal = mean(`flow[m3/s]`, na.rm = TRUE),
      velocity_seasonal = mean(`velocity[m/s]`, na.rm = TRUE),
      suspended_solids_seasonal = mean(`suspended_solids[mg/l]`, na.rm = TRUE),
      pH_seasonal = mean(pH, na.rm = TRUE),
      temp_seasonal = mean(`temp[°C]`, na.rm = TRUE),
      DO_mgl_seasonal = mean(`D.O.[mg/l]`, na.rm = TRUE),
      DO_pct_seasonal = mean(`D.O.[%]`, na.rm = TRUE),
      BOD7_seasonal = mean(`BOD7[mg/L]`, na.rm = TRUE),
      ChDS_C_seasonal = mean(`ChDS_C[mg/L]`, na.rm = TRUE),
      NH4_N_seasonal = mean(`NH4_N[mg/L]`, na.rm = TRUE),
      NO2_N_seasonal = mean(`NO2_N[mg/L]`, na.rm = TRUE),
      NO3_N_seasonal = mean(`NO3_N[mg/L]`, na.rm = TRUE),
      mineral_N_seasonal = mean(`mineral_N[mg/L]`, na.rm = TRUE),
      total_N_seasonal = mean(`total_N[mg/L]`, na.rm = TRUE),
      PO4_P_seasonal = mean(`PO4_P[mg/L]`, na.rm = TRUE),
      total_P_seasonal = mean(`total_P[mg/L]`, na.rm = TRUE),
      EC_seasonal = mean(`EC[µS/cm]`, na.rm = TRUE),
      alkalinity_seasonal = mean(`alkalinity[mmol/l]`, na.rm = TRUE),

      # Keep metadata
      site_code = first(site_code),
      latitude = first(latitude),
      longitude = first(longitude),
      elevation = first(elevation),
      river_basin = first(river_basin),
      catchment_subcatchment = first(catchment_subcatchment),
      river_name = first(river_name),
      modification_state = first(modification_state),
      modification_group = first(modification_group),
      river_type = first(river_type),
      reference_site = first(reference_site),

      n_samples_in_season = n(),
      .groups = "drop"
    )

  # Step 2: Calculate annual means from seasonal means
  annual_means <- seasonal_data |>
    group_by(site_id, year) |>
    summarise(
      # Site metadata
      site_code = first(site_code),
      latitude = first(latitude),
      longitude = first(longitude),
      elevation = first(elevation),
      river_basin = first(river_basin),
      catchment_subcatchment = first(catchment_subcatchment),
      river_name = first(river_name),
      modification_state = first(modification_state),
      modification_group = first(modification_group),
      river_type = first(river_type),
      reference_site = first(reference_site),

      # Sampling information
      n_seasons_sampled = n(),
      seasons_sampled = paste(sort(unique(season)), collapse = ","),
      total_samples = sum(n_samples_in_season),

      # Annual means (simple average of seasonal means)
      flow_annual = mean(flow_seasonal, na.rm = TRUE),
      velocity_annual = mean(velocity_seasonal, na.rm = TRUE),
      suspended_solids_annual = mean(suspended_solids_seasonal, na.rm = TRUE),
      pH_annual = mean(pH_seasonal, na.rm = TRUE),
      temp_annual = mean(temp_seasonal, na.rm = TRUE),
      DO_mgl_annual = mean(DO_mgl_seasonal, na.rm = TRUE),
      DO_pct_annual = mean(DO_pct_seasonal, na.rm = TRUE),
      BOD7_annual = mean(BOD7_seasonal, na.rm = TRUE),
      ChDS_C_annual = mean(ChDS_C_seasonal, na.rm = TRUE),
      NH4_N_annual = mean(NH4_N_seasonal, na.rm = TRUE),
      NO2_N_annual = mean(NO2_N_seasonal, na.rm = TRUE),
      NO3_N_annual = mean(NO3_N_seasonal, na.rm = TRUE),
      mineral_N_annual = mean(mineral_N_seasonal, na.rm = TRUE),
      total_N_annual = mean(total_N_seasonal, na.rm = TRUE),
      PO4_P_annual = mean(PO4_P_seasonal, na.rm = TRUE),
      total_P_annual = mean(total_P_seasonal, na.rm = TRUE),
      EC_annual = mean(EC_seasonal, na.rm = TRUE),
      alkalinity_annual = mean(alkalinity_seasonal, na.rm = TRUE),

      .groups = "drop"
    ) |>
    # Add data quality assessment
    mutate(
      # Seasonal representation
      has_winter = grepl("winter", seasons_sampled),
      has_spring = grepl("spring", seasons_sampled),
      has_summer = grepl("summer", seasons_sampled),
      has_autumn = grepl("autumn", seasons_sampled),

      # Data quality
      data_quality = case_when(
        n_seasons_sampled == 4 ~ "complete",
        n_seasons_sampled == 3 ~ "good",
        n_seasons_sampled == 2 ~ "partial",
        n_seasons_sampled == 1 ~ "limited",
        TRUE ~ "insufficient"
      )
    )

  return(annual_means)
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

explore_seasonal_patterns <- function(data) {

  summary_data <- data |>
    mutate(
      season = case_when(
        month %in% c(12, 1, 2) ~ "winter",
        month %in% c(3, 4, 5) ~ "spring",
        month %in% c(6, 7, 8) ~ "summer",
        month %in% c(9, 10, 11) ~ "autumn"
      )
    ) |>
    group_by(site_id, year) |>
    summarise(
      n_samples = n(),
      n_seasons = n_distinct(season, na.rm = TRUE),
      seasons = paste(sort(unique(season)), collapse = ","),
      .groups = "drop"
    )

  pattern_overview <- summary_data |>
    count(n_seasons, name = "n_site_years") |>
    mutate(
      prop_site_years = round(n_site_years / sum(n_site_years) * 100, 1)
    )

  cat("Seasonal Sampling Patterns:\n")
  cat("===========================\n")
  print(pattern_overview)

  return(summary_data)
}

validate_seasonal_means <- function(annual_data) {

  validation <- annual_data |>
    summarise(
      total_site_years = n(),
      complete_seasonal = sum(data_quality == "complete"),
      good_seasonal = sum(data_quality == "good"),
      partial_seasonal = sum(data_quality == "partial"),
      limited_seasonal = sum(data_quality == "limited"),

      # Check completeness of key variables
      flow_complete = sum(!is.na(flow_annual)),
      temp_complete = sum(!is.na(temp_annual)),
      nutrients_complete = sum(!is.na(total_N_annual))
    )

  cat("Validation Summary:\n")
  cat("==================\n")
  print(validation)

  return(validation)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# 1. Load data
env_data

# 2. Explore patterns
patterns <- explore_seasonal_patterns(env_data)

# 3. Calculate annual means
annual_means <- calculate_seasonal_annual_means(env_data)

unique(annual_means$data_quality)

# 4. Validate
validation <- validate_seasonal_means(annual_means)

# 5. Filter by quality
complete_data <- annual_means |> filter(data_quality == "complete")

# 6. Save
write_xlsx(annual_means, "Output data/1_annual_environmental_means_2009-2023.xlsx")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
