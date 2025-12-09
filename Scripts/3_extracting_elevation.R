############################################################
# ELEVATION FROM DEM ONTO COMMON 300x300 GRID
# - Extent based on: Output data/2_annual_environmental_means_2009-2023.xlsx
# - Elevation source: elevatr::get_elev_raster (DEM)
# - Grid: 300x300 lon/lat grid (same style used in other interpolations)
# - Outputs:
#     * Full grid with DEM elevation (for entire grid extent)
#     * Grid clipped to Lithuania boundary
############################################################

#==================== 1. LOAD PACKAGES =====================

library(dplyr)
library(readxl)
library(janitor)
library(sf)
library(rgeoboundaries)
library(elevatr)
library(terra)
library(ggplot2)   # for plotting

#==================== 2. CONFIGURATION =====================

INPUT_ENV_FILE        <- "Output data/1_annual_environmental_means_2009-2023.xlsx"

FULL_GRID_OUT_FILE    <- "Output data/2.Elevation_DEM_grid_Lithuania_300x300_full.csv"
CLIPPED_GRID_OUT_FILE <- "Output data/3.Elevation_DEM_grid_Lithuania_300x300_clipped.csv"

GRID_RESOLUTION  <- 300      # 300 × 300 grid
EXTENT_BUFFER    <- 0.05     # 5% buffer around site extent

PLOTS_DIR        <- "Plots"
if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)

#==================== 3. LOAD ANNUAL MEANS & GET EXTENT =====================

annual_means <- read_excel(INPUT_ENV_FILE)

coords_df <- annual_means |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  select(longitude, latitude) |>
  filter(is.finite(longitude), is.finite(latitude))

if (nrow(coords_df) == 0L) {
  stop("No valid longitude/latitude values found in the annual means file.")
}

lon_range <- range(coords_df$longitude)
lat_range <- range(coords_df$latitude)

lon_buffer <- diff(lon_range) * EXTENT_BUFFER
lat_buffer <- diff(lat_range) * EXTENT_BUFFER

lon_min_ext <- lon_range[1] - lon_buffer
lon_max_ext <- lon_range[2] + lon_buffer
lat_min_ext <- lat_range[1] - lat_buffer
lat_max_ext <- lat_range[2] + lat_buffer

#==================== 4. LITHUANIA BOUNDARY & DEM (BASED ON GRID EXTENT) =====================

# Lithuania boundary (for later clipping ONLY)
lithuania_sf <- geoboundaries("Lithuania", adm_lvl = 0) |>
  st_transform(4326)

# Create an sf polygon for the grid extent (NOT Lithuania)
extent_poly <- st_polygon(list(rbind(
  c(lon_min_ext, lat_min_ext),
  c(lon_max_ext, lat_min_ext),
  c(lon_max_ext, lat_max_ext),
  c(lon_min_ext, lat_max_ext),
  c(lon_min_ext, lat_min_ext)
))) |>
  st_sfc(crs = 4326)

extent_sf <- st_sf(geometry = extent_poly)

# DEM from elevatr, clipped to the GRID EXTENT (NOT Lithuania)
elev_raw <- elevatr::get_elev_raster(
  locations = extent_sf,
  z        = 9,
  clip     = "locations"   # clip to grid extent polygon
)

elev_rast <- terra::rast(elev_raw)

cat("DEM loaded for grid extent\n")

#==================== 5. CREATE 300×300 COMMON GRID =====================

pred_grid <- expand.grid(
  longitude = seq(lon_min_ext, lon_max_ext, length.out = GRID_RESOLUTION),
  latitude  = seq(lat_min_ext, lat_max_ext, length.out = GRID_RESOLUTION)
)

cat(paste0("Grid cells (full grid): ", nrow(pred_grid), "\n"))

#==================== 6. SAMPLE DEM ELEVATION ONTO GRID (ENTIRE GRID) =====================

grid_vect <- terra::vect(
  pred_grid,
  geom = c("longitude", "latitude"),
  crs  = "EPSG:4326"
)

grid_elev_df <- terra::extract(elev_rast, grid_vect)

# 2nd column is DEM elevation at each grid cell
pred_grid$elevation_dem <- grid_elev_df[, 2]

cat(paste0("Cells with NA elevation (outside DEM tiles): ",
           sum(!is.finite(pred_grid$elevation_dem)), "\n"))

#-------------------- 6a. SHARED STYLE SETTINGS FOR BOTH PLOTS --------------------

elev_range <- range(pred_grid$elevation_dem, na.rm = TRUE)

# Use pretty() to get visually nice contour levels
raw_breaks <- pretty(elev_range, n = 10)

# Keep only breaks within the actual elevation range
contour_breaks <- raw_breaks[raw_breaks >= elev_range[1] & raw_breaks <= elev_range[2]]

# Ensure unique, sorted, and at least 2 levels
contour_breaks <- sort(unique(contour_breaks))
if (length(contour_breaks) < 2) {
  contour_breaks <- c(elev_range[1], elev_range[1] + 1)
}

# For the colour scale limits we can still round to a nice upper bound
max_elev <- ceiling(elev_range[2] / 100) * 100

xlim_lt <- c(lon_min_ext, lon_max_ext)
ylim_lt <- c(lat_min_ext, lat_max_ext)

#==================== 6b. QA PLOT: FULL GRID (TILES + CONTOURS, LEGACY THEME) =====================

p_full <- ggplot() +
  # Smooth surface via tiles
  geom_tile(
    data = pred_grid,
    aes(x = longitude, y = latitude, fill = elevation_dem)
  ) +
  # Contour lines from full grid
  geom_contour(
    data = pred_grid,
    aes(x = longitude, y = latitude, z = elevation_dem),
    breaks   = contour_breaks,
    colour   = "grey30",
    alpha    = 0.7,
    linewidth = 0.4
  ) +
  # Labelled contours
  # geom_text_contour(
  #   data  = pred_grid,
  #   aes(x = longitude, y = latitude, z = elevation_dem),
  #   breaks = contour_breaks,
  #   stroke = 0.2,
  #   size   = 2.5,
  #   skip   = 0,
  #   colour = "grey20"
  # ) +
  # Lithuania boundary
  geom_sf(
    data   = lithuania_sf,
    colour = "black",
    fill   = NA,
    linewidth = 0.5
  ) +
  coord_sf(xlim = xlim_lt, ylim = ylim_lt) +
  # Original colour scheme & legend style
  scale_fill_gradientn(
    colours = c("#124B10", "#e1bb33", "#7E482B"),
    limits  = c(elev_range[1], max_elev),
    name    = "m",
    breaks  = c(0, 100, 200, 300),
    guide   = guide_colorbar(
      barwidth      = 20,
      barheight     = 0.5,
      title.position = "top",
      title.hjust    = 0.5,
      label.hjust    = 0.5
    )
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    legend.position    = "bottom",
    panel.border       = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    legend.box.spacing = unit(0.5, "lines"),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks         = element_line(colour = "black")
  )

ggsave(
  filename = file.path(PLOTS_DIR, "Figure3a_DEM_Elevation_300x300_Grid.png"),
  plot     = p_full,
  width    = 10,
  height   = 9,
  dpi      = 450,
  bg       = "white"
)

#==================== 7. SAVE FULL GRID (NOT CLIPPED TO LITHUANIA) =====================

write.csv(pred_grid, FULL_GRID_OUT_FILE, row.names = FALSE)
cat(paste0("Full grid saved → ", FULL_GRID_OUT_FILE, "\n"))

#==================== 8. CLIP GRID TO LITHUANIA & SAVE =====================

grid_sf <- st_as_sf(
  pred_grid,
  coords = c("longitude", "latitude"),
  crs    = 4326
)

grid_clip_sf <- st_intersection(grid_sf, lithuania_sf)

coords <- st_coordinates(grid_clip_sf)

grid_clip <- grid_clip_sf |>
  mutate(
    longitude = coords[, 1],
    latitude  = coords[, 2]
  ) |>
  st_drop_geometry()

cat(paste0("Grid cells within Lithuania: ",
           nrow(grid_clip), "\n"))

#==================== 8b. QA PLOT: CLIPPED GRID (TILES + CONTOURS, LEGACY THEME) =====================

p_clip <- ggplot() +
  geom_tile(
    data = grid_clip,
    aes(x = longitude, y = latitude, fill = elevation_dem)
  ) +
  geom_contour(
    data = pred_grid,   # contours from full grid for continuous lines
    aes(x = longitude, y = latitude, z = elevation_dem),
    breaks   = contour_breaks,
    colour   = "grey30",
    alpha    = 0.7,
    linewidth = 0.4
  ) +
  # geom_text_contour(
  #   data  = pred_grid,
  #   aes(x = longitude, y = latitude, z = elevation_dem),
  #   breaks = contour_breaks,
  #   stroke = 0.2,
  #   size   = 2.5,
  #   skip   = 0,
  #   colour = "grey20"
  # ) +
  geom_sf(
    data   = lithuania_sf,
    colour = "black",
    fill   = NA,
    linewidth = 0.5
  ) +
  coord_sf(xlim = xlim_lt, ylim = ylim_lt) +
  scale_fill_gradientn(
    colours = c("#124B10", "#e1bb33", "#7E482B"),
    limits  = c(elev_range[1], max_elev),
    name    = "m",
    breaks  = c(0, 100, 200, 300),
    guide   = guide_colorbar(
      barwidth      = 20,
      barheight     = 0.5,
      title.position = "top",
      title.hjust    = 0.5,
      label.hjust    = 0.5
    )
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    legend.position    = "bottom",
    panel.border       = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    legend.box.spacing = unit(0.5, "lines"),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks         = element_line(colour = "black")
  )

ggsave(
  filename = file.path(PLOTS_DIR, "Figure3b_DEM_Elevation_300x300_Grid_Clipped_Lithuania.png"),
  plot     = p_clip,
  width    = 10,
  height   = 9,
  dpi      = 450,
  bg       = "white"
)

#==================== 9. WRITE CLIPPED GRID =====================

write.csv(grid_clip, CLIPPED_GRID_OUT_FILE, row.names = FALSE)
cat(paste0("Clipped grid saved → ", CLIPPED_GRID_OUT_FILE, "\n"))

#==================== 10. CLEAN UP WORKSPACE =====================
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
