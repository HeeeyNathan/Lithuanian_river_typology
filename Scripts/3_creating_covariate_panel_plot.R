# Panel plot for paper

#==================== Sampling sites ===================
#====== Load packages ======
library(tidyverse)
library(ggplot2)
library(sf)
library(rgeoboundaries)
library(grid)
library(readr)
library(ggspatial)
library(pacman)
library(readxl)
library(sfnetworks)

#====== Load theme for plotting ======
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, linewidth = 1.25),
                  strip.background = element_rect(fill = "white",
                                                  color = "white", linewidth = 1.25),
                  legend.position = "bottom",
                  text = element_text(size = 16))

#====== Load data ======
Unique_sites <- read_excel("Output data/1_site_factors_2009-2023_wElevation.xlsx") |>
  dplyr::select(-id)

# Load and reproject water bodies from geodatabase
gdb_path <- "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/Lithuanian data/Geographic data/UETK_2024-05-02.gdb/UETK_2024-05-02.gdb"
rivers <- st_read(gdb_path, layer = "upes_l") |>
  st_transform(4326) # Reproject to WGS84

rivers_cast <- st_cast(rivers, "LINESTRING")
river_net <- as_sfnetwork(rivers_cast)



lakes <- st_read(gdb_path, layer = "ezerai_tvenkiniai") |>
  st_transform(4326)  # Reproject to WGS84

#====== Get Lithuania boundary ======
lithuania_sf <- geoboundaries("Lithuania", adm_lvl = 0)
lithuania_spat <- terra::vect(lithuania_sf)

#====== Get Europe boundary ======
europe <- geoboundaries(c("Lithuania", "Poland", "Latvia", "Estonia", "Belarus",
                          "Russia", "Ukraine", "Germany", "Denmark", "Sweden", "Finland",
                          "Czech Republic", "Slovakia", "Austria", "Hungary", "Romania", "Bulgaria",
                          "Moldova", "Slovenia", "Croatia", "Bosnia and Herzegovina", "Serbia",
                          "Montenegro", "Albania", "North Macedonia", "Kosovo", "Greece", "Turkey",
                          "Cyprus", "Malta", "Italy", "France", "Spain", "Portugal", "Andorra",
                          "Monaco", "San Marino", "Vatican City", "Switzerland", "Liechtenstein",
                          "Luxembourg", "Belgium", "Netherlands", "Ireland", "United Kingdom",
                          "Norway", "Iceland"))
europe <- st_transform(europe, crs = 4326)

# Create main plot
sampling_sites <- ggplot() +
  # Add rivers layer with much thinner lines
  geom_sf(data = rivers,
          color = "lightblue",
          alpha = 0.25,
          linewidth = 0.5) +
  # Add lakes layer
  # geom_sf(data = lakes,
  #         fill = "lightblue",
  #         color = NA,
  #         alpha = 0.75,
  #         size = 0.01) +
  # Base layer - Lithuania boundary
  geom_sf(data = lithuania_sf, color = "#440154", fill = NA, linewidth = 0.5) +
  # Add sampling points - simplified for river sites only
  geom_point(aes(x = longitude,  # Changed from Long to longitude
                 y = latitude),  # Changed from Lat to latitude
             data = Unique_sites,
             size = 1.5,
             color = "#440154",  # Single color since all are rivers
             shape = 19) +
  coord_sf(xlim = c(20.9, 26.9), ylim = c(53.85, 56.5)) +
  labs(x = "Longitude",
       y = "Latitude") +
  My_theme +
  theme(
    legend.position = "none",  # No legend needed since all points are the same
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black")
  )

# Create inset map with matching style
inset <- ggplot() +
  geom_sf(data = europe, color = "black", fill = "white") +
  geom_sf(data = lithuania_sf, fill = "#440154", alpha = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.border = element_rect(fill = NA, linewidth = 0.5)) +
  coord_sf(xlim = c(10, 39), ylim = c(45, 65))

# Convert inset to grob
inset_grob <- ggplotGrob(inset)

# Add inset to main plot
sampling_sites <- sampling_sites +
  annotation_custom(inset_grob,
                    xmin = 19.99, xmax = 23,
                    ymin = 53.85, ymax = 54.98)

# Add north arrow and scale bar
sampling_sites <- sampling_sites +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.25, "cm"),
    pad_y = unit(0.25, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  )

ggsave("Plots/Figure1_Sampling_sites.png", plot = sampling_sites, width = 10, height = 8, dpi = 450, bg = "white")

#==================== CORINE Land Cover ===================
#====== Load packages ======
library(raster)
library(tidyverse)
library(rgeoboundaries)
library(sf)
library(terra)
library(tidyterra)

# !!!!!! IMPORTANT: YOU WILL NEED TO DOWNLOAD THE CORINE LANDCOVER DATA YOURSELF AND LINK IT TO YOUR OWN WORKING DIRECTORY!!!!!!!!!!!!!!!!!!!

#====== Get Lithuania boundary ======
lithuania_sf <- geoboundaries("Lithuania", adm_lvl = 0)
lithuania_spat_3035 <- terra::project(terra::vect(lithuania_sf), "EPSG:3035")

#====== Load and process CORINE data ======
# Read CORINE raster
corine_raster <- terra::rast("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2018/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
print(cats(corine_raster))

# Load legends
LegendCorine_2012 <- as_tibble(read.table("C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/Lithuanian data/Landcover data/LandUse/Corine2012/clc_legend.csv", h = T, sep = ";")) |>
  select(CLC_CODE, LABEL1, LABEL2)

LegendCorine_2018 <- as_tibble(read.csv("C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/Lithuanian data/Landcover data/LandUse/Corine2018/u2018_clc2018_v2020_20u1_raster100m/Legend/CLC2018_CLC2018_V2018_20_QGIS.txt", h = FALSE, sep = ",")) |>
  rename(CLC_CODE = V1, RED = V2, GREEN = V3, BLUE = V4, ALPHA = V5, LABEL3 = V6) |>
  mutate(GRID_CODE = row_number()) |>
  left_join(LegendCorine_2012, by = "CLC_CODE") |>
  select(GRID_CODE, CLC_CODE, RED, GREEN, BLUE, ALPHA, LABEL1, LABEL2, LABEL3)

# Crop to Lithuania
corine_lt <- terra::crop(corine_raster, lithuania_spat_3035)
corine_lt <- terra::mask(corine_lt, lithuania_spat_3035)

# Project to WGS84 (EPSG:4326)
corine_lt_4326 <- terra::project(corine_lt, "EPSG:4326")

# Create a levels dataframe for LABEL1
levels_df <- data.frame(
  ID = 1:nrow(LegendCorine_2018),
  LABEL1 = LegendCorine_2018$LABEL1
)

# Set the new levels
levels(corine_lt_4326) <- levels_df

# Create color scheme based on LABEL1
unique_labels <- unique(LegendCorine_2018$LABEL1)
label1_colors <- c(
  "Artificial surfaces" = "#ff0000",            # Reddish color for urban/built-up areas
  "Agricultural areas" = "#ffffa8",             # Wheat/crop color for agriculture
  "Forest and semi natural areas" = "#00a600",  # Dark green for forests
  "Wetlands" = "#a6a6ff",                       # Light green-blue for wetlands
  "Water bodies" = "#00ccf2",                   # Dark blue for water
  "NODATA" = "#ffffff"                         # White for no data
)

landcover_plot <- ggplot() +
  geom_spatraster(data = corine_lt_4326) +
  geom_sf(data = lithuania_sf, color = "#440154", fill = NA, linewidth = 0.5) +
  coord_sf(xlim = c(20.9, 26.9), ylim = c(53.85, 56.5)) +
  scale_fill_manual(
    name = "Land Cover Type",
    values = label1_colors,
    breaks = unique_labels,
    na.value = "white"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  My_theme +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    legend.box.spacing = unit(0.5, "lines"),  # Reduced from 1.5 to 0.5 to bring legend closer
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  ) +
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = TRUE
    )
  )

ggsave("Plots/Figure2_Landcover.png", plot = landcover_plot, width = 10, height = 9, dpi = 450, bg = "white")

#==================== Combining plots ===================
#====== Load packages ======
library(patchwork)

# Function to modify legend appearance and remove titles
modify_legend <- function(plot) {
  plot +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.margin = margin(t = 3, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = 3, r = 0, b = 0, l = 0),
      # Remove plot title
      plot.title = element_blank()
    )
}

#====== Create add_plot_label function with top-right positioning ======
add_plot_label <- function(plot, label) {
  plot +
    annotate(
      "text",
      x = -Inf,  # Changed from -Inf to Inf for right side
      y = Inf,
      label = label,
      hjust = -0.4,  # Changed to move label inward from right
      vjust = 1.4,
      size = 7,     # Increased from 5 to 7
      fontface = "bold"
    )
}

plots_list <- list(
  # Plot A (top-left): Show y-axis, hide x-axis and x-label
  add_plot_label(modify_legend(sampling_sites +
    labs(x = NULL, y = NULL, caption = NULL)), "A"),

  # Plot B (top-right): Show x-axis, hide y-axis and y-label
  add_plot_label(modify_legend(landcover_plot +
    labs(x = NULL, y = NULL, caption = NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank())), "B")
)

# Create layout using patchwork
combined_plot <- (
  plots_list[[1]] + plots_list[[2]]
) +
  plot_layout(
    ncol = 2,
    guides = "keep"
  ) +
  plot_annotation(
    theme = theme(
      axis.title.x = element_text(size = 12, margin = margin(t = 20)),
      axis.title.y = element_text(size = 12, margin = margin(r = 20)),
      # Ensure no title in combined plot
      plot.title = element_blank()
    )
  ) &
  xlab("Longitude") &
  ylab("Latitude")

ggsave("Plots/Figure3_panel_plot.png", plot = combined_plot, width = 15, height = 7, dpi = 450, bg = "white")

###############################################################################################################
# CLEAN UP WORKSPACE
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
