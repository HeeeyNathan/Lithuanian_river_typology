# Lithuanian River Data Analysis - Simple Seasonal Approach
# Clean, transparent calculation of seasonal weighted annual means

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# =============================================================================
# DATA LOADING
# =============================================================================

# Define column types for all 35 columns
col_types <- c(
  "text",     # site_id
  "text",     # site_code
  "text",     # sample_id
  "numeric",  # latitude
  "numeric",  # longitude
  "text",     # river_basin
  "text",     # catchment_subcatchment
  "text",     # river_name
  "text",     # modification_state
  "text",     # modification_group
  "numeric",  # river_type
  "logical",  # reference_site
  "date",     # serial_date
  "numeric",  # day
  "numeric",  # month
  "numeric",  # year
  "text",     # date
  "numeric",  # elevation
  "numeric",  # flow[m3/s]
  "numeric",  # velocity[m/s]
  "numeric",  # suspended_solids[mg/l]
  "numeric",  # pH
  "numeric",  # temp[°C]
  "numeric",  # D.O.[mg/l]
  "numeric",  # D.O.[%]
  "numeric",  # BOD7[mg/L]
  "numeric",  # ChDS_C[mg/L]
  "numeric",  # NH4_N[mg/L]
  "numeric",  # NO2_N[mg/L]
  "numeric",  # NO3_N[mg/L]
  "numeric",  # mineral_N[mg/L]
  "numeric",  # total_N[mg/L]
  "numeric",  # PO4_P[mg/L]
  "numeric",  # total_P[mg/L]
  "numeric",  # EC[µS/cm]
  "numeric"   # alkalinity[mmol/l]
)

# Load data
env_data <- read_excel("Input data/2_environmental_data_2009-2023.xlsx", col_types = col_types) |>
  arrange(site_id, serial_date) # order the data.frame

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

  # Step 2: Calculate annual means from seasonal means and retain seasonal data
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

      # Seasonal means with original units format
      `flow[m3/s]_win` = ifelse("winter" %in% season, flow_seasonal[season == "winter"], NA)[1],
      `flow[m3/s]_spr` = ifelse("spring" %in% season, flow_seasonal[season == "spring"], NA)[1],
      `flow[m3/s]_sum` = ifelse("summer" %in% season, flow_seasonal[season == "summer"], NA)[1],
      `flow[m3/s]_aut` = ifelse("autumn" %in% season, flow_seasonal[season == "autumn"], NA)[1],

      `velocity[m/s]_win` = ifelse("winter" %in% season, velocity_seasonal[season == "winter"], NA)[1],
      `velocity[m/s]_spr` = ifelse("spring" %in% season, velocity_seasonal[season == "spring"], NA)[1],
      `velocity[m/s]_sum` = ifelse("summer" %in% season, velocity_seasonal[season == "summer"], NA)[1],
      `velocity[m/s]_aut` = ifelse("autumn" %in% season, velocity_seasonal[season == "autumn"], NA)[1],

      `suspended_solids[mg/l]_win` = ifelse("winter" %in% season, suspended_solids_seasonal[season == "winter"], NA)[1],
      `suspended_solids[mg/l]_spr` = ifelse("spring" %in% season, suspended_solids_seasonal[season == "spring"], NA)[1],
      `suspended_solids[mg/l]_sum` = ifelse("summer" %in% season, suspended_solids_seasonal[season == "summer"], NA)[1],
      `suspended_solids[mg/l]_aut` = ifelse("autumn" %in% season, suspended_solids_seasonal[season == "autumn"], NA)[1],

      `pH_win` = ifelse("winter" %in% season, pH_seasonal[season == "winter"], NA)[1],
      `pH_spr` = ifelse("spring" %in% season, pH_seasonal[season == "spring"], NA)[1],
      `pH_sum` = ifelse("summer" %in% season, pH_seasonal[season == "summer"], NA)[1],
      `pH_aut` = ifelse("autumn" %in% season, pH_seasonal[season == "autumn"], NA)[1],

      `temp[°C]_win` = ifelse("winter" %in% season, temp_seasonal[season == "winter"], NA)[1],
      `temp[°C]_spr` = ifelse("spring" %in% season, temp_seasonal[season == "spring"], NA)[1],
      `temp[°C]_sum` = ifelse("summer" %in% season, temp_seasonal[season == "summer"], NA)[1],
      `temp[°C]_aut` = ifelse("autumn" %in% season, temp_seasonal[season == "autumn"], NA)[1],

      `D.O.[mg/l]_win` = ifelse("winter" %in% season, DO_mgl_seasonal[season == "winter"], NA)[1],
      `D.O.[mg/l]_spr` = ifelse("spring" %in% season, DO_mgl_seasonal[season == "spring"], NA)[1],
      `D.O.[mg/l]_sum` = ifelse("summer" %in% season, DO_mgl_seasonal[season == "summer"], NA)[1],
      `D.O.[mg/l]_aut` = ifelse("autumn" %in% season, DO_mgl_seasonal[season == "autumn"], NA)[1],

      `D.O.[%]_win` = ifelse("winter" %in% season, DO_pct_seasonal[season == "winter"], NA)[1],
      `D.O.[%]_spr` = ifelse("spring" %in% season, DO_pct_seasonal[season == "spring"], NA)[1],
      `D.O.[%]_sum` = ifelse("summer" %in% season, DO_pct_seasonal[season == "summer"], NA)[1],
      `D.O.[%]_aut` = ifelse("autumn" %in% season, DO_pct_seasonal[season == "autumn"], NA)[1],

      `BOD7[mg/L]_win` = ifelse("winter" %in% season, BOD7_seasonal[season == "winter"], NA)[1],
      `BOD7[mg/L]_spr` = ifelse("spring" %in% season, BOD7_seasonal[season == "spring"], NA)[1],
      `BOD7[mg/L]_sum` = ifelse("summer" %in% season, BOD7_seasonal[season == "summer"], NA)[1],
      `BOD7[mg/L]_aut` = ifelse("autumn" %in% season, BOD7_seasonal[season == "autumn"], NA)[1],

      `ChDS_C[mg/L]_win` = ifelse("winter" %in% season, ChDS_C_seasonal[season == "winter"], NA)[1],
      `ChDS_C[mg/L]_spr` = ifelse("spring" %in% season, ChDS_C_seasonal[season == "spring"], NA)[1],
      `ChDS_C[mg/L]_sum` = ifelse("summer" %in% season, ChDS_C_seasonal[season == "summer"], NA)[1],
      `ChDS_C[mg/L]_aut` = ifelse("autumn" %in% season, ChDS_C_seasonal[season == "autumn"], NA)[1],

      `NH4_N[mg/L]_win` = ifelse("winter" %in% season, NH4_N_seasonal[season == "winter"], NA)[1],
      `NH4_N[mg/L]_spr` = ifelse("spring" %in% season, NH4_N_seasonal[season == "spring"], NA)[1],
      `NH4_N[mg/L]_sum` = ifelse("summer" %in% season, NH4_N_seasonal[season == "summer"], NA)[1],
      `NH4_N[mg/L]_aut` = ifelse("autumn" %in% season, NH4_N_seasonal[season == "autumn"], NA)[1],

      `NO2_N[mg/L]_win` = ifelse("winter" %in% season, NO2_N_seasonal[season == "winter"], NA)[1],
      `NO2_N[mg/L]_spr` = ifelse("spring" %in% season, NO2_N_seasonal[season == "spring"], NA)[1],
      `NO2_N[mg/L]_sum` = ifelse("summer" %in% season, NO2_N_seasonal[season == "summer"], NA)[1],
      `NO2_N[mg/L]_aut` = ifelse("autumn" %in% season, NO2_N_seasonal[season == "autumn"], NA)[1],

      `NO3_N[mg/L]_win` = ifelse("winter" %in% season, NO3_N_seasonal[season == "winter"], NA)[1],
      `NO3_N[mg/L]_spr` = ifelse("spring" %in% season, NO3_N_seasonal[season == "spring"], NA)[1],
      `NO3_N[mg/L]_sum` = ifelse("summer" %in% season, NO3_N_seasonal[season == "summer"], NA)[1],
      `NO3_N[mg/L]_aut` = ifelse("autumn" %in% season, NO3_N_seasonal[season == "autumn"], NA)[1],

      `mineral_N[mg/L]_win` = ifelse("winter" %in% season, mineral_N_seasonal[season == "winter"], NA)[1],
      `mineral_N[mg/L]_spr` = ifelse("spring" %in% season, mineral_N_seasonal[season == "spring"], NA)[1],
      `mineral_N[mg/L]_sum` = ifelse("summer" %in% season, mineral_N_seasonal[season == "summer"], NA)[1],
      `mineral_N[mg/L]_aut` = ifelse("autumn" %in% season, mineral_N_seasonal[season == "autumn"], NA)[1],

      `total_N[mg/L]_win` = ifelse("winter" %in% season, total_N_seasonal[season == "winter"], NA)[1],
      `total_N[mg/L]_spr` = ifelse("spring" %in% season, total_N_seasonal[season == "spring"], NA)[1],
      `total_N[mg/L]_sum` = ifelse("summer" %in% season, total_N_seasonal[season == "summer"], NA)[1],
      `total_N[mg/L]_aut` = ifelse("autumn" %in% season, total_N_seasonal[season == "autumn"], NA)[1],

      `PO4_P[mg/L]_win` = ifelse("winter" %in% season, PO4_P_seasonal[season == "winter"], NA)[1],
      `PO4_P[mg/L]_spr` = ifelse("spring" %in% season, PO4_P_seasonal[season == "spring"], NA)[1],
      `PO4_P[mg/L]_sum` = ifelse("summer" %in% season, PO4_P_seasonal[season == "summer"], NA)[1],
      `PO4_P[mg/L]_aut` = ifelse("autumn" %in% season, PO4_P_seasonal[season == "autumn"], NA)[1],

      `total_P[mg/L]_win` = ifelse("winter" %in% season, total_P_seasonal[season == "winter"], NA)[1],
      `total_P[mg/L]_spr` = ifelse("spring" %in% season, total_P_seasonal[season == "spring"], NA)[1],
      `total_P[mg/L]_sum` = ifelse("summer" %in% season, total_P_seasonal[season == "summer"], NA)[1],
      `total_P[mg/L]_aut` = ifelse("autumn" %in% season, total_P_seasonal[season == "autumn"], NA)[1],

      `EC[µS/cm]_win` = ifelse("winter" %in% season, EC_seasonal[season == "winter"], NA)[1],
      `EC[µS/cm]_spr` = ifelse("spring" %in% season, EC_seasonal[season == "spring"], NA)[1],
      `EC[µS/cm]_sum` = ifelse("summer" %in% season, EC_seasonal[season == "summer"], NA)[1],
      `EC[µS/cm]_aut` = ifelse("autumn" %in% season, EC_seasonal[season == "autumn"], NA)[1],

      `alkalinity[mmol/l]_win` = ifelse("winter" %in% season, alkalinity_seasonal[season == "winter"], NA)[1],
      `alkalinity[mmol/l]_spr` = ifelse("spring" %in% season, alkalinity_seasonal[season == "spring"], NA)[1],
      `alkalinity[mmol/l]_sum` = ifelse("summer" %in% season, alkalinity_seasonal[season == "summer"], NA)[1],
      `alkalinity[mmol/l]_aut` = ifelse("autumn" %in% season, alkalinity_seasonal[season == "autumn"], NA)[1],

      # Annual means with original units format
      `flow[m3/s]_ann` = mean(c(`flow[m3/s]_win`, `flow[m3/s]_spr`, `flow[m3/s]_sum`, `flow[m3/s]_aut`), na.rm = TRUE),
      `velocity[m/s]_ann` = mean(c(`velocity[m/s]_win`, `velocity[m/s]_spr`, `velocity[m/s]_sum`, `velocity[m/s]_aut`), na.rm = TRUE),
      `suspended_solids[mg/l]_ann` = mean(c(`suspended_solids[mg/l]_win`, `suspended_solids[mg/l]_spr`, `suspended_solids[mg/l]_sum`, `suspended_solids[mg/l]_aut`), na.rm = TRUE),
      `pH_ann` = mean(c(`pH_win`, `pH_spr`, `pH_sum`, `pH_aut`), na.rm = TRUE),
      `temp[°C]_ann` = mean(c(`temp[°C]_win`, `temp[°C]_spr`, `temp[°C]_sum`, `temp[°C]_aut`), na.rm = TRUE),
      `D.O.[mg/l]_ann` = mean(c(`D.O.[mg/l]_win`, `D.O.[mg/l]_spr`, `D.O.[mg/l]_sum`, `D.O.[mg/l]_aut`), na.rm = TRUE),
      `D.O.[%]_ann` = mean(c(`D.O.[%]_win`, `D.O.[%]_spr`, `D.O.[%]_sum`, `D.O.[%]_aut`), na.rm = TRUE),
      `BOD7[mg/L]_ann` = mean(c(`BOD7[mg/L]_win`, `BOD7[mg/L]_spr`, `BOD7[mg/L]_sum`, `BOD7[mg/L]_aut`), na.rm = TRUE),
      `ChDS_C[mg/L]_ann` = mean(c(`ChDS_C[mg/L]_win`, `ChDS_C[mg/L]_spr`, `ChDS_C[mg/L]_sum`, `ChDS_C[mg/L]_aut`), na.rm = TRUE),
      `NH4_N[mg/L]_ann` = mean(c(`NH4_N[mg/L]_win`, `NH4_N[mg/L]_spr`, `NH4_N[mg/L]_sum`, `NH4_N[mg/L]_aut`), na.rm = TRUE),
      `NO2_N[mg/L]_ann` = mean(c(`NO2_N[mg/L]_win`, `NO2_N[mg/L]_spr`, `NO2_N[mg/L]_sum`, `NO2_N[mg/L]_aut`), na.rm = TRUE),
      `NO3_N[mg/L]_ann` = mean(c(`NO3_N[mg/L]_win`, `NO3_N[mg/L]_spr`, `NO3_N[mg/L]_sum`, `NO3_N[mg/L]_aut`), na.rm = TRUE),
      `mineral_N[mg/L]_ann` = mean(c(`mineral_N[mg/L]_win`, `mineral_N[mg/L]_spr`, `mineral_N[mg/L]_sum`, `mineral_N[mg/L]_aut`), na.rm = TRUE),
      `total_N[mg/L]_ann` = mean(c(`total_N[mg/L]_win`, `total_N[mg/L]_spr`, `total_N[mg/L]_sum`, `total_N[mg/L]_aut`), na.rm = TRUE),
      `PO4_P[mg/L]_ann` = mean(c(`PO4_P[mg/L]_win`, `PO4_P[mg/L]_spr`, `PO4_P[mg/L]_sum`, `PO4_P[mg/L]_aut`), na.rm = TRUE),
      `total_P[mg/L]_ann` = mean(c(`total_P[mg/L]_win`, `total_P[mg/L]_spr`, `total_P[mg/L]_sum`, `total_P[mg/L]_aut`), na.rm = TRUE),
      `EC[µS/cm]_ann` = mean(c(`EC[µS/cm]_win`, `EC[µS/cm]_spr`, `EC[µS/cm]_sum`, `EC[µS/cm]_aut`), na.rm = TRUE),
      `alkalinity[mmol/l]_ann` = mean(c(`alkalinity[mmol/l]_win`, `alkalinity[mmol/l]_spr`, `alkalinity[mmol/l]_sum`, `alkalinity[mmol/l]_aut`), na.rm = TRUE),

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

      # Check completeness of key variables - using correct column names
      flow_complete = sum(!is.na(`flow[m3/s]_ann`)),
      temp_complete = sum(!is.na(`temp[°C]_ann`)),
      nutrients_complete = sum(!is.na(`total_N[mg/L]_ann`))
    )

  cat("Validation Summary:\n")
  cat("==================\n")
  print(validation)

  return(validation)
}

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# 1. Explore patterns
patterns <- explore_seasonal_patterns(env_data)

# 2. Calculate annual means
annual_means <- calculate_seasonal_annual_means(env_data)

# 3. Validate
validation <- validate_seasonal_means(annual_means)

# 4. Filter by quality
complete_data <- annual_means |> filter(data_quality == "complete")

# 5. Save
write_xlsx(annual_means, "Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")
