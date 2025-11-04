# Lithuanian River Typology Explorer

## Abiotic-Based River Classification System

**Version:** 1.0
**Date:** 2025-01-04
**Author:** Freshwater Ecologist

---

## Table of Contents

1. [Overview](#overview)
2. [Rationale](#rationale)
3. [Data Structure](#data-structure)
4. [Analytical Pipeline](#analytical-pipeline)
5. [Key Methods](#key-methods)
6. [Running the Analysis](#running-the-analysis)
7. [Interpreting Results](#interpreting-results)
8. [Next Steps](#next-steps)

---

## Overview

This project explores alternative river typology schemes for Lithuanian rivers based on comprehensive abiotic (physico-chemical) characteristics. The current Lithuanian river typology (Types 1-5) is based solely on **catchment size** and **slope**, but may not adequately capture ecological variation relevant to biological communities.

### Project Goals

1. **Explore spatial patterns** in abiotic variables using thin plate regression splines (TPRS)
2. **Identify natural groupings** of rivers based on hydrochemistry, geography, and hydrology
3. **Compare alternative typologies** with the current classification system
4. **Provide evidence** for potential refinement of Lithuanian river types

---

## Rationale

### Current River Typology

Lithuanian rivers are classified into 5 types based on:
- **Catchment area**: small (<100 km²), medium (100-1000 km²), large (>1000 km²)
- **Slope**: varying thresholds (0.7 m/km for medium rivers, 0.3 m/km for large rivers)
- All rivers are considered **calcareous waters** of the Baltic Sea ecoregion at altitudes <200m

### Problem Statement

Previous attempts to create type-specific macroinvertebrate indices found **little ecological separation** between current river types. This suggests the classification may not reflect true environmental or biological differences.

### Alternative Approaches

Rivers could be better differentiated by:
- **Hydrochemistry** (pH, alkalinity, conductivity, dissolved oxygen)
- **Feeding regime** (groundwater-fed, rain-fed, snow-fed)
- **Geographic position** (latitude, longitude, elevation)
- **Nutrient status** (nitrogen, phosphorus concentrations)
- **Hydrological regime** (flow patterns, velocity)

---

## Data Structure

### Input Data

Located in `Input data/`:

1. **`1_site_factors_2009-2023.xlsx`** (56 KB)
   - Site identifiers, coordinates
   - River basin, catchment information
   - Modification state
   - Current river type classification

2. **`2_environmental_data_2009-2023.xlsx`** (3.5 MB)
   - Time series: 2009-2023
   - 35 variables including:
     - **Physical**: flow, velocity, suspended solids, temperature
     - **Chemical**: pH, DO, BOD7, COD
     - **Nutrients**: NH4-N, NO2-N, NO3-N, total N, PO4-P, total P
     - **Ionic**: electrical conductivity, alkalinity

### Output Data

Located in `Output data/`:

1. **Processed datasets**:
   - `3_seasonal_annual_environmental_means_2009-2023.xlsx`: Seasonal and annual means
   - `4_TPRS_interpolated_grid.csv`: Spatially interpolated values across Lithuania
   - `5_site_data_with_interpolations.csv`: Site-level data with TPRS predictions

2. **Classification results**:
   - `7_classification_scheme_comparison.csv`: Performance of different typology schemes
   - `8_best_cluster_assignments.csv`: Optimal cluster assignments for each site
   - `9_cluster_profiles.csv`: Characterization of each cluster
   - `10_temporal_stability.csv`: Inter-annual variability metrics

### Figures

Located in `Plots/`:

- **Figure 4**: TPRS isoline maps for typology variables (temp, pH, EC, alkalinity, DO)
- **Figure 5**: TPRS isoline maps for nutrient variables (N, P)
- **Figure 6**: Spatial environmental gradients (PCA of interpolated surfaces)
- **Figure 7**: Dendrograms for different classification schemes
- **Figure 8**: RDA ordination of best classification scheme
- **Figure 9**: Spatial maps comparing current types vs. new clusters
- **Figure 10**: Environmental gradient overlay maps

---

## Analytical Pipeline

### Phase 1: Data Preparation

**Scripts 1-2**

1. **Elevation extraction** (`1_extracting_elevation.R`)
   - Extracts elevation data for each site
   - Uses Google Elevation API or equivalent
   - Adds topographic context

2. **Seasonal means calculation** (`2_calculated_seasonal_means.R`)
   - Calculates seasonal means (winter, spring, summer, autumn)
   - Computes weighted annual means
   - Assesses data quality and temporal coverage

### Phase 2: Spatial Interpolation (TPRS)

**Script 6** (`6_TPRS_spatial_interpolation.R`)

#### What is TPRS?

Thin Plate Regression Splines are a sophisticated spatial interpolation method that:
- Creates **smooth continuous surfaces** from point observations
- Handles **edge effects** better than kriging
- Provides **uncertainty estimates** (standard errors)
- Works well with **irregular sampling designs**

#### Method

For each abiotic variable:

```r
# Fit GAM with thin plate spline
gam_model <- gam(variable ~ s(longitude, latitude, bs = 'tp', k = 30),
                 data = site_data,
                 method = "REML")
```

#### Outputs

1. **Isoline maps**: Continuous spatial surfaces with contour lines (like ArcGIS)
2. **Interpolated values**: Predictions at each site location
3. **Spatial outliers**: Sites deviating from expected spatial patterns
4. **Environmental gradients**: PCA of interpolated surfaces

### Phase 3: Advanced Typology Exploration

**Script 7** (`7_advanced_typology_exploration.R`)

#### Classification Schemes Tested

1. **Geographic/Topographic**: latitude, longitude, elevation
2. **Hydrochemical**: temp, pH, EC, alkalinity, DO
3. **Nutrient signature**: total N, total P, NO3-N, PO4-P
4. **Hydrological regime**: flow, velocity
5. **Combined typology**: geographic + hydrochemical
6. **Full abiotic suite**: all interpolated variables

#### Clustering Approach

- **Method**: Hierarchical clustering (Ward's linkage)
- **Optimization**: Silhouette method and gap statistic
- **Validation**: PERMANOVA to test environmental separation

#### Comparison Metrics

- **R² (PERMANOVA)**: Proportion of variance explained
- **Adjusted Rand Index**: Agreement with current river types
- **Silhouette width**: Cluster quality
- **Temporal stability**: Inter-annual consistency

### Phase 4: Spatial Visualization

**Script 8** (`8_spatial_cluster_mapping.R`)

Creates publication-quality maps:
1. Current river type distribution
2. New cluster distribution
3. Environmental gradients (temperature, alkalinity)
4. Cluster centroids and sizes
5. Spatial outliers
6. Comparative maps (current vs. new)

---

## Key Methods

### 1. Thin Plate Regression Splines (TPRS)

**Why TPRS over other interpolation methods?**

| Method | Pros | Cons |
|--------|------|------|
| **TPRS** | • Smooth surfaces<br>• Handles edges well<br>• Automatic smoothing<br>• Fast computation | • Assumes stationarity |
| Kriging | • Accounts for spatial correlation<br>• Flexible variogram | • Slow<br>• Edge effects<br>• Requires variogram fitting |
| IDW | • Simple<br>• Fast | • No smoothing<br>• Bull's eye effect |

**Key parameters:**
- `bs = "tp"`: Thin plate spline basis
- `k = 30`: Basis dimension (complexity)
- `method = "REML"`: Restricted maximum likelihood for smoothness selection

### 2. Hierarchical Clustering

**Why Ward's method?**
- Minimizes within-cluster variance
- Produces compact, spherical clusters
- Works well with ecological data
- Interpretable dendrograms

**Distance metric:**
- Euclidean distance on scaled data
- All variables standardized (mean = 0, SD = 1)

**Optimal k selection:**
1. **Silhouette method**: Maximizes average silhouette width
2. **Gap statistic**: Compares to null distribution
3. **Elbow method**: Identifies point of diminishing returns

### 3. Ordination (RDA)

**Purpose:**
- Visualize multivariate patterns
- Identify major environmental gradients
- Show relationships between variables and sites

**Method:**
- Redundancy Analysis (RDA) = PCA on environmental matrix
- Unconstrained ordination
- Sites plotted in environmental space

### 4. PERMANOVA

**Purpose:**
- Test if groupings (clusters, river types) explain environmental variation
- Permutation-based, non-parametric
- Provides R² (effect size) and p-value

**Interpretation:**
- **R² > 0.3**: Strong separation
- **R² 0.1-0.3**: Moderate separation
- **R² < 0.1**: Weak separation

---

## Running the Analysis

### Requirements

**R version:** ≥ 4.0.0

**Required packages:**

```r
# Install all required packages
install.packages(c(
  "mgcv",           # GAM and TPRS
  "vegan",          # Multivariate ecology
  "cluster",        # Clustering algorithms
  "NbClust",        # Optimal cluster number
  "dplyr",          # Data manipulation
  "tidyr",          # Data tidying
  "readxl",         # Read Excel files
  "writexl",        # Write Excel files
  "ggplot2",        # Plotting
  "ggdendro",       # Dendrograms
  "factoextra",     # Cluster visualization
  "patchwork",      # Combine plots
  "viridis",        # Color palettes
  "sf",             # Spatial data
  "rnaturalearth",  # Map data
  "rnaturalearthdata",
  "ggspatial",      # Map elements
  "metR",           # Contours
  "corrplot"        # Correlation plots
))
```

### Execution

#### Option 1: Run entire pipeline

```r
# Navigate to project directory
setwd("/path/to/Lithuanian_river_typology")

# Run master script
source("Scripts/0_MASTER_pipeline.R")
```

This will execute all analyses in sequence:
1. Data preparation
2. TPRS interpolation
3. Typology exploration
4. Spatial mapping

**Expected runtime:** 10-30 minutes (depending on computer)

#### Option 2: Run individual scripts

```r
# Run scripts in order:
source("Scripts/1_extracting_elevation.R")
source("Scripts/2_calculated_seasonal_means.R")
source("Scripts/6_TPRS_spatial_interpolation.R")
source("Scripts/7_advanced_typology_exploration.R")
source("Scripts/8_spatial_cluster_mapping.R")
```

### Configuration

Edit `0_MASTER_pipeline.R` to customize:

```r
config <- list(
  run_data_prep = TRUE,           # Scripts 1-2
  run_tprs = TRUE,                # Script 6
  run_typology = TRUE,            # Script 7
  run_mapping = TRUE,             # Script 8
  run_previous_analyses = FALSE   # Scripts 3-5
)
```

---

## Interpreting Results

### 1. TPRS Isoline Maps (Figure 4-5)

**What to look for:**
- **Spatial gradients**: Do variables show clear geographic patterns?
- **Hotspots**: Areas with unusually high/low values
- **Isoline density**: Steep gradients vs. gradual transitions

**Example interpretation:**
- Strong N-S temperature gradient → latitude effect
- EC/alkalinity clustering in certain regions → geology
- Nutrient hotspots → anthropogenic impact

### 2. Classification Comparison (Table 7)

**Key metrics:**

| Metric | Interpretation |
|--------|----------------|
| **R²_Clusters** | Variance explained by abiotic clusters |
| **R²_RiverTypes** | Variance explained by current typology |
| **R²_Improvement** | Additional variance explained by clusters |
| **ARI** | Agreement with current types (-1 to 1) |

**Decision criteria:**
- If **R²_Clusters >> R²_RiverTypes**: Alternative typology is superior
- If **ARI ≈ 0**: Clusters are independent of current types (novel groupings)
- If **ARI > 0.5**: Clusters align with current types (refinement)

### 3. Cluster Profiles (Table 9)

**Questions to ask:**
- What defines each cluster?
- Are clusters ecologically meaningful?
- Do they align with known river characteristics?

**Example:**
```
Cluster 1: Cold, high DO, low nutrients → headwater streams
Cluster 2: Warm, high EC, moderate nutrients → lowland rivers
Cluster 3: Variable temp, very high nutrients → impacted sites
```

### 4. Spatial Maps (Figure 9)

**Compare:**
- Current types: Based on catchment/slope
- New clusters: Based on abiotic reality

**Look for:**
- Geographic coherence (clusters in same regions?)
- Size distribution (balanced clusters?)
- Outliers (sites that don't fit)

### 5. Temporal Stability (Table 10)

**Coefficient of Variation (CV):**
- **CV < 0.2**: Stable
- **CV 0.2-0.5**: Moderate variability
- **CV > 0.5**: High variability

**Interpretation:**
- Stable variables → good for typology
- Variable nutrients → pollution impact
- Variable temperature → microclimatic effects

---

## Next Steps

### 1. Biological Validation

**Critical step:** Test if new typology better predicts biological patterns

#### Approach A: Macroinvertebrate Community

```r
# Compare community composition between:
# 1. Current river types
# 2. Abiotic clusters

# Use PERMANOVA on macroinvertebrate abundance/presence-absence
```

**Expected outcome:**
- If R²_clusters > R²_river_types → new typology is biologically relevant

#### Approach B: Biological Indices

```r
# Test if cluster-specific indices perform better than:
# 1. Generic indices (e.g., ASPT)
# 2. Current type-specific indices
```

### 2. Feeding Regime Classification

**Goal:** Distinguish groundwater-fed vs. surface water-fed rivers

**Indicators:**
- Temperature variability (groundwater = stable)
- EC/alkalinity (groundwater = high)
- Seasonal patterns (groundwater = dampened)

**Analysis:**
```r
# Calculate seasonal amplitude:
temp_amplitude = temp_summer - temp_winter

# High amplitude → surface water
# Low amplitude → groundwater
```

### 3. Macrophyte Integration

**Question:** Do clusters also differentiate macrophyte communities?

**Approach:**
- Overlay macrophyte survey data on clusters
- Test if species composition differs between clusters
- Develop cluster-specific macrophyte indices

### 4. Reference Conditions

**For WFD implementation:**

1. Identify least-impacted sites in each cluster
2. Calculate reference values for biological metrics
3. Define cluster-specific ecological targets
4. Develop pressure-response relationships

### 5. Manuscript Preparation

**Suggested structure:**

1. **Introduction**: Limitations of current typology
2. **Methods**: TPRS interpolation + multivariate classification
3. **Results**:
   - Spatial patterns in abiotic variables
   - Alternative typologies and their performance
   - Biological validation
4. **Discussion**:
   - Ecological interpretation of clusters
   - Comparison with other European countries
   - WFD implications
5. **Conclusions**: Recommendations for Lithuanian river typology

**Potential journals:**
- *River Research and Applications*
- *Hydrobiologia*
- *Ecological Indicators*
- *Science of the Total Environment*

---

## Technical Notes

### TPRS vs. Kriging

**When to use TPRS:**
- Regular to moderately irregular sampling
- Large spatial extent with edge effects
- Need for fast computation
- Continuous environmental gradients

**When to use Kriging:**
- Strong spatial autocorrelation known a priori
- Small spatial extent
- Sufficient data for variogram estimation
- Need for prediction intervals (not just SE)

### Cluster Validation

**Internal validation:**
- Silhouette width
- Within-cluster sum of squares
- Gap statistic

**External validation:**
- PERMANOVA (environmental separation)
- Biological data (community composition)
- Independent variables (e.g., land use)

### Handling Missing Data

Current approach:
1. Calculate site-level means (averages across years)
2. Use interpolated values from TPRS to fill gaps
3. Remove sites with <50% data completeness

Alternative approach:
- Multiple imputation (mice package)
- Weighted means based on data quality

---

## Troubleshooting

### Common Issues

**Error: "Cannot find input data"**
- Ensure working directory is set correctly: `setwd("path/to/project")`
- Check that files exist in `Input data/` folder

**Error: "Package 'X' not found"**
- Install missing package: `install.packages("X")`
- Load library: `library(X)`

**Error: "Not enough data for TPRS"**
- Some variables have too many NAs
- Check data quality: `summary(site_data)`
- Consider excluding sparse variables

**Error: "Memory allocation"**
- TPRS on fine grid may exceed RAM
- Reduce grid resolution in script 6: `length.out = 50` (instead of 100)

**Maps not displaying correctly**
- Install system dependencies for `sf` package
- On Ubuntu: `sudo apt-get install libgdal-dev libproj-dev`

---

## References

### Key Methods Papers

**Thin Plate Splines:**
- Wood, S.N. (2017). *Generalized Additive Models: An Introduction with R*, 2nd ed. CRC Press.

**River Typology:**
- Sandin, L. & Verdonschot, P.F.M. (2006). Stream and river typologies – major results and conclusions from the STAR project. *Hydrobiologia*, 566, 33-37.

**Clustering Ecology:**
- Borcard, D., Gillet, F. & Legendre, P. (2018). *Numerical Ecology with R*, 2nd ed. Springer.

**WFD Typology:**
- Furse, M.T., Hering, D., Brabec, K., et al. (2006). The ecological status of European rivers: evaluation and intercalibration of assessment methods. *Hydrobiologia*, 566, 1-2.

---

## Contact & Citation

**Author:** Freshwater Ecologist
**Institution:** [Your Institution]
**Email:** [Your Email]

**How to cite this pipeline:**

> [Your Name] (2025). Lithuanian River Typology Explorer: Abiotic-Based Classification System. Version 1.0. https://github.com/[yourusername]/Lithuanian_river_typology

---

## License

This analytical pipeline is provided under MIT License.

---

**Last updated:** 2025-01-04
