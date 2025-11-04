# Lithuanian River Typology Explorer

## Abiotic-Based River Classification System

**Exploring alternative river typologies for Lithuania based on comprehensive physico-chemical data**

---

## Overview

This project develops and tests alternative river classification schemes for Lithuanian rivers using abiotic (physico-chemical) characteristics. The current Lithuanian river typology (Types 1-5) is based solely on **catchment size** and **slope**, but may not adequately capture ecological variation relevant to biological communities.

### Key Innovation: Thin Plate Regression Splines (TPRS)

We use **thin plate regression splines** to create continuous spatial surfaces of environmental variables across Lithuania - similar to isoline mapping in ArcGIS, but entirely in R. This allows us to:

- Interpolate environmental variables across space
- Identify natural environmental gradients
- Detect spatial outliers
- Create data-driven river classifications

---

## Quick Start

### 1. Install Requirements

```r
# Install all required R packages
install.packages(c("mgcv", "vegan", "cluster", "dplyr", "tidyr",
                   "ggplot2", "sf", "rnaturalearth", "viridis"))
```

### 2. Run Complete Pipeline

```r
# Set working directory and run
setwd("/path/to/Lithuanian_river_typology")
source("Scripts/0_MASTER_pipeline.R")
```

**That's it!** The pipeline will:
- ✓ Calculate seasonal means
- ✓ Perform TPRS spatial interpolation
- ✓ Test multiple classification schemes
- ✓ Generate publication-ready maps and figures

**Runtime:** ~15-30 minutes

---

## Project Structure

```
Lithuanian_river_typology/
├── Input data/              # Raw data (2009-2023)
├── Output data/             # Processed results & classifications
├── Scripts/                 # R analysis scripts
│   ├── 0_MASTER_pipeline.R           # ⭐ Run this first!
│   ├── 6_TPRS_spatial_interpolation.R
│   ├── 7_advanced_typology_exploration.R
│   └── 8_spatial_cluster_mapping.R
├── Plots/                   # Generated figures
├── QUICKSTART.md           # 🚀 Quick start guide
├── ANALYSIS_GUIDE.md       # 📖 Detailed methodology
└── README.md               # This file
```

---

## Key Features

### 🗺️ Spatial Interpolation
- Thin plate regression splines for smooth spatial surfaces
- Isoline maps for 18+ environmental variables
- Identification of environmental gradients

### 🔬 Multiple Classification Schemes
- **Geographic**: Latitude, longitude, elevation
- **Hydrochemical**: Temperature, pH, conductivity, alkalinity, DO
- **Nutrient**: Total N, total P, NO3, PO4
- **Hydrological**: Flow, velocity
- **Combined**: Integrated approaches

### 📊 Comprehensive Statistics
- Hierarchical clustering (Ward's method)
- Redundancy Analysis (RDA) ordination
- PERMANOVA for group separation
- Temporal stability assessment

### 🎨 Publication-Ready Visualizations
- Spatial maps comparing typologies
- Environmental gradient overlays
- Dendrograms and ordination plots
- High-resolution figures (300 dpi PNG + vector PDF)

---

## Results

The pipeline generates:

**Data outputs:**
- 10+ processed datasets with classifications
- Cluster assignments for each site
- Temporal stability metrics
- Model performance comparisons

**Figures:**
- Figure 4-5: TPRS isoline maps
- Figure 6: Spatial environmental gradients
- Figure 7: Classification dendrograms
- Figure 8: RDA ordination plots
- Figure 9-10: Spatial comparison maps

---

## Documentation

| Document | Purpose |
|----------|---------|
| **[QUICKSTART.md](QUICKSTART.md)** | Get started in 5 minutes |
| **[ANALYSIS_GUIDE.md](ANALYSIS_GUIDE.md)** | Detailed methodology & interpretation |
| **Script headers** | Individual method documentation |

---

## Scientific Background

### Current Lithuanian River Typology

- **Type 1**: Small rivers (catchment <100 km²)
- **Type 2**: Medium rivers, steep (100-1000 km², slope >0.7 m/km)
- **Type 3**: Medium rivers, flat (100-1000 km², slope ≤0.7 m/km)
- **Type 4**: Large rivers, steep (>1000 km², slope >0.3 m/km)
- **Type 5**: Large rivers, flat (>1000 km², slope ≤0.3 m/km)

All rivers are classified as **calcareous waters** of the Baltic Sea ecoregion (<200m altitude).

### Problem

Previous studies found **little ecological separation** between these types when analyzing macroinvertebrate communities. This suggests the current classification may not reflect true environmental or biological differences.

### Alternative Approach

This project explores classifications based on:
- **Hydrochemistry** (pH, alkalinity, conductivity, DO)
- **Geographic position** (latitude, longitude, elevation)
- **Nutrient status** (nitrogen and phosphorus)
- **Hydrological regime** (flow patterns)
- **Feeding regime** (groundwater vs. surface water)

---

## Data

**Time series:** 2009-2023 (15 years)
**Sites:** 100+ monitoring locations
**Variables:** 18 physico-chemical parameters
- Physical: flow, velocity, temperature, suspended solids
- Chemical: pH, DO, BOD, COD
- Nutrients: NH4-N, NO2-N, NO3-N, total N, PO4-P, total P
- Ionic: electrical conductivity, alkalinity

**Data source:** Lithuanian Environmental Agency monitoring network

---

## Methods Highlights

### Thin Plate Regression Splines (TPRS)

```r
# Fit spatial surface for each variable
gam(variable ~ s(longitude, latitude, bs = "tp", k = 30),
    data = sites, method = "REML")
```

**Advantages:**
- Smooth, continuous surfaces
- Superior edge effect handling
- Automatic smoothness selection
- Fast computation

### Hierarchical Clustering

- **Method:** Ward's linkage (minimizes within-cluster variance)
- **Optimization:** Silhouette method + gap statistic
- **Validation:** PERMANOVA on environmental data

### Comparison Metrics

- **R²**: Variance explained by classification
- **Adjusted Rand Index**: Agreement with current types
- **Silhouette width**: Cluster quality
- **Temporal stability**: Inter-annual consistency

---

## Requirements

**Software:**
- R (≥ 4.0.0)
- RStudio (recommended)

**Key R packages:**
- `mgcv` - GAM and thin plate splines
- `vegan` - Multivariate ecology
- `cluster` - Clustering algorithms
- `sf` - Spatial data handling
- `ggplot2` - Visualization

See `QUICKSTART.md` for complete installation instructions.

---

## Usage Examples

### Run Full Analysis

```r
source("Scripts/0_MASTER_pipeline.R")
```

### Quick Data Exploration

```r
library(dplyr)
library(readxl)

# Load data
data <- read_excel("Output data/3_seasonal_annual_environmental_means_2009-2023.xlsx")

# View clusters
clusters <- read.csv("Output data/8_best_cluster_assignments.csv")
table(clusters$cluster, clusters$river_type)
```

### Custom Analysis

```r
# Create your own typology with specific variables
source("Scripts/7_advanced_typology_exploration.R")

my_vars <- c("temp_interpolated", "EC_interpolated", "alkalinity_interpolated")
my_typology <- perform_clustering(site_data, my_vars, "Custom Scheme")
```

---

## Next Steps

### 1. Biological Validation
Test if abiotic clusters better predict:
- Macroinvertebrate community composition
- Macrophyte assemblages
- Biological quality metrics

### 2. WFD Implementation
- Identify reference sites per cluster
- Develop cluster-specific biological indices
- Define ecological quality targets

### 3. Manuscript Preparation
Compare with European WFD typology approaches and assess ecological relevance.

---

## Citation

If you use this pipeline in your research, please cite:

> [Your Name] (2025). Lithuanian River Typology Explorer: Abiotic-Based Classification System.
> GitHub repository: https://github.com/[username]/Lithuanian_river_typology

---

## References

**Key papers:**
- Wood, S.N. (2017). *Generalized Additive Models: An Introduction with R*, 2nd ed.
- Sandin, L. & Verdonschot, P.F.M. (2006). Stream and river typologies – major results from the STAR project. *Hydrobiologia*, 566, 33-37.
- Borcard, D., Gillet, F. & Legendre, P. (2018). *Numerical Ecology with R*, 2nd ed.

---

## License

MIT License - See LICENSE file for details

---

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request

---

## Contact

**Project Lead:** [Your Name]
**Institution:** [Your Institution]
**Email:** [Your Email]

---

## Acknowledgments

- Lithuanian Environmental Agency for monitoring data
- Water Framework Directive community for typology frameworks
- R community for excellent statistical tools

---

**Status:** ✅ Active Development
**Version:** 1.0
**Last Updated:** 2025-01-04

---

## Get Started Now!

```r
# Clone repository
git clone https://github.com/[username]/Lithuanian_river_typology

# Set working directory
setwd("Lithuanian_river_typology")

# Run pipeline
source("Scripts/0_MASTER_pipeline.R")
```

🚀 **Let's explore Lithuanian rivers together!**
