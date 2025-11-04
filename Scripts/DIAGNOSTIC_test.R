# ===============================================================================
# DIAGNOSTIC TEST - Check if data files exist and are loadable
# ===============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════╗\n")
cat("║  LITHUANIAN RIVER TYPOLOGY - DIAGNOSTIC TEST          ║\n")
cat("╚════════════════════════════════════════════════════════╝\n")
cat("\n")

# Check required packages
cat("1. CHECKING REQUIRED PACKAGES...\n")
cat("═══════════════════════════════════\n\n")

required_packages <- c("mgcv", "vegan", "cluster", "dplyr", "tidyr",
                       "readxl", "ggplot2", "viridis", "patchwork")

for(pkg in required_packages) {
  if(requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("  ✓ ", pkg, "\n"))
  } else {
    cat(paste0("  ✗ ", pkg, " - NOT INSTALLED\n"))
    cat(paste0("    Install with: install.packages('", pkg, "')\n"))
  }
}

cat("\n")

# Check input data files
cat("2. CHECKING INPUT DATA FILES...\n")
cat("═══════════════════════════════════\n\n")

input_files <- c(
  "Input data/1_site_factors_2009-2023.xlsx",
  "Input data/2_environmental_data_2009-2023.xlsx"
)

for(file in input_files) {
  if(file.exists(file)) {
    size <- file.info(file)$size / 1024  # KB
    cat(paste0("  ✓ ", file, " (", round(size, 1), " KB)\n"))
  } else {
    cat(paste0("  ✗ ", file, " - NOT FOUND\n"))
  }
}

cat("\n")

# Check output data files
cat("3. CHECKING OUTPUT DATA FILES...\n")
cat("═══════════════════════════════════\n\n")

output_files <- c(
  "Output data/1_site_factors_2009-2023_wElevation.xlsx",
  "Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx",
  "Output data/4_TPRS_interpolated_grid.csv",
  "Output data/5_site_data_with_interpolations.csv"
)

all_outputs_exist <- TRUE

for(file in output_files) {
  if(file.exists(file)) {
    size <- file.info(file)$size / 1024  # KB
    cat(paste0("  ✓ ", file, " (", round(size, 1), " KB)\n"))
  } else {
    cat(paste0("  ✗ ", file, " - NOT FOUND (needs to be generated)\n"))
    all_outputs_exist <- FALSE
  }
}

cat("\n")

# Try loading key data
cat("4. TESTING DATA LOADING...\n")
cat("═══════════════════════════════════\n\n")

if(file.exists("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")) {
  tryCatch({
    library(readxl)
    test_data <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")
    cat(paste0("  ✓ Successfully loaded seasonal means\n"))
    cat(paste0("    Rows: ", nrow(test_data), "\n"))
    cat(paste0("    Columns: ", ncol(test_data), "\n"))
    cat(paste0("    Sites: ", length(unique(test_data$site_id)), "\n"))
    cat(paste0("    Years: ", min(test_data$year, na.rm=T), "-", max(test_data$year, na.rm=T), "\n"))
  }, error = function(e) {
    cat(paste0("  ✗ Error loading data: ", e$message, "\n"))
  })
} else {
  cat("  ⚠ Seasonal means file not found\n")
}

cat("\n")

# Recommendations
cat("5. RECOMMENDATIONS...\n")
cat("═══════════════════════════════════\n\n")

if(!all_outputs_exist) {
  cat("⚠ Some output files are missing. You need to run:\n\n")
  cat("Option A - Run preparation scripts manually:\n")
  cat('  source("Scripts/2_calculated_seasonal_means.R")\n')
  cat('  source("Scripts/6_TPRS_spatial_interpolation.R")\n\n')
  cat("Option B - Run simplified pipeline (see below)\n\n")
} else {
  cat("✓ All required files exist! You can run:\n")
  cat('  source("Scripts/7_advanced_typology_exploration.R")\n')
  cat('  source("Scripts/8_spatial_cluster_mapping.R")\n\n')
}

cat("\n")
cat("═══════════════════════════════════\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("═══════════════════════════════════\n\n")
