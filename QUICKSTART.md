# Quick Start Guide

## Lithuanian River Typology Explorer

Get started with the abiotic-based river classification system in **3 simple steps**!

---

## Prerequisites

### 1. Install R and RStudio

- **R**: Version ≥ 4.0.0 ([Download R](https://cran.r-project.org/))
- **RStudio**: Latest version ([Download RStudio](https://www.rstudio.com/))

### 2. Install Required Packages

Open R/RStudio and run:

```r
# Install packages (only needs to be done once)
install.packages(c(
  # Core analysis
  "mgcv", "vegan", "cluster", "NbClust",
  # Data manipulation
  "dplyr", "tidyr", "readxl", "writexl",
  # Visualization
  "ggplot2", "ggdendro", "factoextra", "patchwork", "viridis", "corrplot",
  # Spatial analysis
  "sf", "rnaturalearth", "rnaturalearthdata", "ggspatial", "metR"
))
```

**Installation time:** ~5-10 minutes

---

## Running the Analysis

### Option 1: Full Pipeline (Recommended)

**Run everything with a single command:**

```r
# Set working directory to project folder
setwd("/path/to/Lithuanian_river_typology")

# Run master pipeline
source("Scripts/0_MASTER_pipeline.R")
```

**Expected outputs:**
- ✓ 10 data files in `Output data/`
- ✓ 10 figures in `Plots/`
- ✓ Pipeline execution summary

**Runtime:** 15-30 minutes (depending on computer)

---

### Option 2: Step-by-Step

**Run individual phases:**

#### Phase 1: Data Preparation

```r
source("Scripts/1_extracting_elevation.R")
source("Scripts/2_calculated_seasonal_means.R")
```

**Outputs:**
- Site factors with elevation
- Seasonal and annual means

**Runtime:** ~2 minutes

---

#### Phase 2: Spatial Interpolation (TPRS)

```r
source("Scripts/6_TPRS_spatial_interpolation.R")
```

**Outputs:**
- Isoline maps (Figure 4-6)
- Interpolated grid data
- Spatial outliers identified

**Runtime:** ~5-10 minutes

---

#### Phase 3: Typology Exploration

```r
source("Scripts/7_advanced_typology_exploration.R")
```

**Outputs:**
- Classification comparison table
- Best cluster assignments
- Dendrograms (Figure 7)
- RDA ordination (Figure 8)

**Runtime:** ~3-5 minutes

---

#### Phase 4: Spatial Mapping

```r
source("Scripts/8_spatial_cluster_mapping.R")
```

**Outputs:**
- Spatial comparison maps (Figure 9)
- Environmental gradient maps (Figure 10)

**Runtime:** ~2-3 minutes

---

## Quick Data Exploration

### Load and Inspect Data

```r
library(readxl)
library(dplyr)

# Load seasonal means
data <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

# Quick summary
glimpse(data)
summary(data)

# Number of sites and years
n_distinct(data$site_id)  # Number of unique sites
range(data$year)           # Time span

# Filter for complete seasonal coverage
complete_data <- data %>% filter(data_quality == "complete")
nrow(complete_data)
```

### View Cluster Assignments

```r
# Load cluster assignments
clusters <- read.csv("Output data/8_best_cluster_assignments.csv")

# Cross-tabulation: clusters vs current river types
table(clusters$river_type, clusters$cluster)

# Number of sites per cluster
table(clusters$cluster)
```

### Quick Visualization

```r
library(ggplot2)

# Load site data with coordinates
sites <- read.csv("Output data/5_site_data_with_interpolations.csv")

# Temperature vs pH by cluster
ggplot(sites, aes(x = pH_interpolated, y = temp_interpolated, color = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_viridis_d(name = "Cluster") +
  theme_minimal() +
  labs(title = "Temperature vs pH by Abiotic Cluster",
       x = "pH", y = "Temperature (°C)")
```

---

## Understanding Your Results

### Key Files to Check

| File | What to Look For |
|------|------------------|
| **`7_classification_scheme_comparison.csv`** | Which typology scheme performs best? (highest R²) |
| **`8_best_cluster_assignments.csv`** | Which cluster is each site assigned to? |
| **`9_cluster_profiles.csv`** | What characterizes each cluster? |
| **`10_temporal_stability.csv`** | Are site conditions stable over time? |

### Key Figures to Review

| Figure | Key Questions |
|--------|---------------|
| **Figure 4-5** (Isoline maps) | Where are the environmental gradients? |
| **Figure 6** (Spatial gradients) | What are the dominant spatial patterns? |
| **Figure 7** (Dendrograms) | How do sites group together? |
| **Figure 8** (RDA) | What environmental variables drive separation? |
| **Figure 9** (Maps) | How do clusters differ from current types? |

---

## Customization

### Adjust TPRS Resolution

**For faster computation** (lower resolution):

Edit `Scripts/6_TPRS_spatial_interpolation.R`, line ~72:

```r
# Change from:
length.out = 100  # 100x100 grid = 10,000 points

# To:
length.out = 50   # 50x50 grid = 2,500 points (4x faster)
```

### Change Clustering Method

**Test different approaches** in `Scripts/7_advanced_typology_exploration.R`:

```r
# Line 92: Change clustering method
hc <- hclust(dist_mat, method = "ward.D2")  # Current (Ward)

# Alternatives:
hc <- hclust(dist_mat, method = "average")  # UPGMA
hc <- hclust(dist_mat, method = "complete") # Complete linkage
```

### Focus on Specific Variables

**Create custom typology** with selected variables:

```r
# Define your own variable set
my_vars <- c("temp_interpolated", "EC_interpolated", "total_N_interpolated")

# Run clustering (in console or script)
my_result <- perform_clustering(site_data, my_vars, "My Custom Scheme")
```

---

## Troubleshooting

### ❌ Error: "Cannot find file"

**Solution:**
```r
# Check your working directory
getwd()

# Set to project folder
setwd("/full/path/to/Lithuanian_river_typology")

# Verify input data exists
list.files("Input data/")
```

### ❌ Error: "Package not found"

**Solution:**
```r
# Install missing package
install.packages("package_name")

# Load it
library(package_name)
```

### ❌ Error: "Insufficient data for TPRS"

**Solution:**
Some variables have too many missing values. This is normal - the script will skip these and continue with available variables.

### ❌ Maps don't display

**Solution (Linux/Mac):**
```bash
# Install spatial dependencies
# Ubuntu/Debian:
sudo apt-get install libgdal-dev libproj-dev

# Mac:
brew install gdal proj
```

**Solution (Windows):**
- Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
- Reinstall packages: `install.packages(c("sf", "rnaturalearth"))`

---

## Next Steps

### 1. Explore Your Results

- Open figures in `Plots/` folder
- Review data files in `Output data/`
- Check pipeline summary: `Output data/pipeline_execution_summary.csv`

### 2. Biological Validation

Add macroinvertebrate or macrophyte data and test if clusters predict biological patterns better than current types.

### 3. Customize Analysis

- Try different variable combinations
- Test alternative clustering methods
- Explore seasonal patterns (winter vs. summer)

### 4. Read Full Documentation

For detailed methodology and interpretation, see:
- **`ANALYSIS_GUIDE.md`**: Comprehensive analysis documentation
- **`README.md`**: Project overview

---

## Example Session

**Complete workflow in RStudio:**

```r
# ========================================
# Lithuanian River Typology Explorer
# Example Analysis Session
# ========================================

# 1. Setup
setwd("~/Lithuanian_river_typology")
library(dplyr)
library(ggplot2)

# 2. Run full pipeline
source("Scripts/0_MASTER_pipeline.R")
# ☕ Get coffee - will take 15-30 min

# 3. Load results
clusters <- read.csv("Output data/8_best_cluster_assignments.csv")
comparison <- read.csv("Output data/7_classification_scheme_comparison.csv")

# 4. Quick analysis
print(comparison)  # Which scheme works best?
table(clusters$cluster)  # Cluster sizes

# 5. View figures
# Navigate to Plots/ folder and open:
# - Figure9_typology_comparison.png
# - Figure8_RDA_best_scheme.png

# 6. Export for manuscript
# All figures are publication-ready (300 dpi PNG + vector PDF)
```

---

## Getting Help

### Documentation

- **Full guide**: `ANALYSIS_GUIDE.md`
- **Method details**: See individual script headers
- **R package help**: `?function_name` in R console

### Common Questions

**Q: How many sites do I need for TPRS?**
A: Minimum ~20 sites, but 50+ is better for smooth surfaces.

**Q: Can I use this with my own data?**
A: Yes! Format your data like `2_environmental_data_2009-2023.xlsx` and run the pipeline.

**Q: What if I don't have all variables?**
A: The pipeline handles missing data. It will skip variables with insufficient data.

**Q: How do I interpret cluster profiles?**
A: See `ANALYSIS_GUIDE.md` section "Interpreting Results" for detailed guidance.

---

## Success Criteria

✅ **Pipeline completed successfully if you see:**

1. ✓ Console message: "PIPELINE EXECUTION COMPLETE!"
2. ✓ 10 data files in `Output data/`
3. ✓ 10+ figures in `Plots/`
4. ✓ No fatal errors in `Output data/pipeline_execution_summary.csv`

---

## Tips for Success

💡 **Best practices:**

1. **Start fresh**: Close R, restart, run pipeline
2. **Check data**: Verify input files are in correct location
3. **Be patient**: TPRS interpolation takes time - this is normal
4. **Save often**: Pipeline saves outputs incrementally
5. **Version control**: Commit results to git after successful run

---

**Ready to explore? Let's go! 🚀**

```r
source("Scripts/0_MASTER_pipeline.R")
```

---

*For detailed methodology and ecological interpretation, see `ANALYSIS_GUIDE.md`*
