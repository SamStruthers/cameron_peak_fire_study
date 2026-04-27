library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(here)
library(ggpattern)
library(patchwork)
library(nhdplusTools)
library(elevatr)
library(terra)
library(ggnewscale)  

sf::sf_use_s2(FALSE)

# Map styling helper functions

# Reusable blue water style
layer_water <- function(data) {
  geom_sf(data = data, fill = "#2c68e8", color = "#2c68e8", alpha = 1)
}

# Common annotation layers (scale bar + north arrow)
layers_annotations <- list(
  annotation_scale(location = "br", width_hint = 0.25, text_cex = 0.9),
  annotation_north_arrow(
    location    = "tr",
    which_north = "true",
    style       = north_arrow_fancy_orienteering(),
    height      = unit(1.2, "cm"),
    width       = unit(1.2, "cm")
  )
)

# Common annotation layers (scale bar only)
layers_annotation_simple <- list(
  annotation_scale(location = "br", width_hint = 0.25, text_cex = 0.9)
)

# Common theme
theme_map <- function() {
  list(
    theme_bw(base_size = 13),
    theme(
      axis.title       = element_blank(),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      legend.position  = "none"
    )
  )
}

# compute padded xlim/ylim from an sf object
padded_limits <- function(sf_obj, x_pad = 0.01, y_pad = 0.005) {
  bb <- st_bbox(sf_obj)
  list(
    xlim = c(bb[["xmin"]] - x_pad, bb[["xmax"]] + x_pad),
    ylim = c(bb[["ymin"]] - y_pad, bb[["ymax"]] + y_pad)
  )
}

# Helper: fetch and prep a DEM data frame for a given xlim/ylim
# z controls resolution (higher = finer; 10 is good for detail maps, 8 for inset)
# fact controls aggregation (higher = faster/blockier render)
get_dem <- function(xlim, ylim, z = 10, fact = 3, crs = 4326) {
  aoi <- st_sf(id = 1, geometry = st_as_sfc(st_bbox(
    c(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
    crs = crs
  )))
  r <- elevatr::get_elev_raster(locations = aoi, z = z, clip = "locations") %>%
    terra::rast() %>%
    terra::aggregate(fact = fact)
  df           <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df)[3] <- "elev_m"
  df
}

# Reusable DEM raster layer (grey hillshade background)
# Must be followed by new_scale_fill() before any subsequent fill layer
layer_dem <- function(df_dem, alph = 0.4) {
  list(
    geom_raster(data = df_dem, aes(x = x, y = y, fill = elev_m), alpha = alph),
    scale_fill_gradient(low = "white", high = "grey20", na.value = "white", guide = "none"),
    new_scale_fill()
  )
}

# Reusable watershed basin fill
layer_basin <- function(data) {
  geom_sf(data = data, fill = "#2c68e8", color = "darkblue", alpha = 0.12)
}

# BAER burn severity raster layer (light to dark orange by class)
# Values: 1 = Low, 2 = Moderate, 3 = High; all others -> NA -> transparent
baer_colors <- c(
  "Very Low" = "#fef4e8",
  "Low"      = "#fdd9b0",
  "Moderate" = "orange",
  "High"     = "#e8722a"
)

layer_baer <- function(df_baer) {
  list(
    scale_fill_manual(values   = baer_colors,
                      na.value = "transparent",
                      name     = "Burn Severity",
                      drop     = FALSE),
    geom_sf(data = cpf_fire, fill = NA, color = "sienna", linewidth = 1),
    geom_raster(data = df_baer, aes(x = x, y = y, fill = burn_class))
  )
}

layer_points <- function(data, size = 6) {
  list(
    geom_sf(data = data %>% mutate(`Sampling Site` = ifelse(location_type == "Reservoir", "Reservoir", "Stream")),
            aes(shape = `Sampling Site`),
            size = size,
            fill = "black",
            color = "white",
            stroke = 0.6),
    scale_shape_manual(values = c("Reservoir" = 21, "Stream" = 24),
                       guide = "none")
  )
}

# Load in data

flowlines <- st_read(here("data/raw/spatial/res_study/flowlines.shp")) %>%
  st_transform(4326)

cpf_fire <- st_read(here("data/raw/spatial/res_study/cpf_boundary.shp")) %>%
  st_transform(4326)

all_waterbodies <- st_read(here("data/raw/spatial/res_study/study_waterbodies.shp")) %>%
  st_transform(4326)

meta <- read_csv(here("data/raw/chem/ross_clp_chem/v2025.11.14/data/metadata/location_metadata.csv")) %>%
  filter(site_code %in% c("BEAV", "BMD", "BRNR", "CBRI", "CBRR", "CHD", "COMI", "COMO", "COMR",
                          "HORI", "HORO", "HORR", "JOEI", "JOER", "JWC", "LNGO", "LNGR", 
                          "PJW", "PNF", "PSF", "PTRO", "PTRR", "SFM", "PBD", "PBR", "SLEP")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

df_baer <- terra::rast(here("data/raw/spatial/res_study/cameron_peak_co4060910587920200813_sbs/cameron_peak_co4060910587920200813_sbs.tif")) %>%
  terra::project("EPSG:4326") %>%
  terra::crop(cpf_fire, mask = TRUE) %>%
  as.data.frame(xy = TRUE) %>%
  mutate(burn_class = factor(
    case_when(
      Layer_1 == 1 ~ "Very Low",
      Layer_1 == 2 ~ "Low",
      Layer_1 == 3 ~ "Moderate",
      Layer_1 == 4 ~ "High",
      TRUE              ~ "Very Low"
    ),
    levels = c("Very Low", "Low", "Moderate", "High")
  ))

# dems... don't actually use but keeping in case

# Temporary limits computed upfront so all DEMs can be fetched together.
# Final limit objects are (re)assigned in each map section below.
.lims_main <- padded_limits(
  all_waterbodies %>% filter(site_code %in% c("LNGR", "PTRR", "JOER", "BRNR", "CBRR"))
)
.sites_como <- meta %>%
  filter(site_code %in% c("BEAV", "SFM")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
.lims_como <- padded_limits(
  st_union(all_waterbodies %>% filter(site_code %in% c("COMR", "HORR")), .sites_como)
)
.bb_fire <- st_bbox(st_union(cpf_fire))
.pad_x   <- 0.10 * (.bb_fire[["xmax"]] - .bb_fire[["xmin"]])
.pad_y   <- 0.10 * (.bb_fire[["ymax"]] - .bb_fire[["ymin"]])
.lims_inset <- list(
  xlim = c(.bb_fire[["xmin"]] - .pad_x, .bb_fire[["xmax"]] + .pad_x),
  ylim = c(.bb_fire[["ymin"]] - .pad_y, .bb_fire[["ymax"]] + .pad_y)
)

df_dem_main  <- get_dem(.lims_main$xlim,  .lims_main$ylim,  z = 10, fact = 1)
df_dem_como  <- get_dem(.lims_como$xlim,  .lims_como$ylim,  z = 10, fact = 1)
df_dem_inset <- get_dem(.lims_inset$xlim, .lims_inset$ylim, z = 8,  fact = 3)


# Chambers complex

waterbodies_main <- all_waterbodies %>% filter(!site_code %in% c("HORR", "COMR"))
lims_main        <- padded_limits(waterbodies_main)

p_main <- ggplot() +
  # layer_dem(df_dem_main) +
  layer_baer(df_baer) +
  layer_water(waterbodies_main) +
  layer_water(flowlines) +
  layers_annotation_simple +
  layer_points(meta, size = 3) +
  coord_sf(xlim = lims_main$xlim, ylim = lims_main$ylim, expand = FALSE) +
  theme_map() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA))

# Comanche hourglass complex

waterbodies_como <- all_waterbodies %>% filter(site_code %in% c("COMR", "HORR"))
sites_como       <- meta %>%
  filter(site_code %in% c("BEAV")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

lims_como <- padded_limits(st_union(waterbodies_como, sites_como))

nhd_como <- nhdplusTools::get_nhdphr(
  AOI    = waterbodies_como %>% summarize() %>% st_buffer(0.01),
  type   = "networknhdflowline",
  buffer = 3
) %>%
  filter(nhdplusid %in% c("23001900089883", "23001900206218", "23001900000008"))

p_como <- ggplot() +
  # layer_dem(df_dem_como) +
  layer_baer(df_baer) +
  geom_sf(data = cpf_fire, fill = NA, color = "sienna", linewidth = 0.5) +
  layer_water(waterbodies_como) +
  layer_water(nhd_como) +
  layer_water(flowlines) +
  layers_annotation_simple +
  layer_points(meta, size = 3) +
  coord_sf(xlim = lims_como$xlim, ylim = lims_como$ylim, expand = FALSE) +
  theme_map() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA))

# Full watershed
inset_xlim <- .lims_inset$xlim
inset_ylim <- .lims_inset$ylim

bbox_poly <- st_as_sfc(st_bbox(
  c(xmin = inset_xlim[1], xmax = inset_xlim[2],
    ymin = inset_ylim[1], ymax = inset_ylim[2]),
  crs = st_crs(cpf_fire)
))

# NHD rivs for whole WS
nhd_inset <- nhdplusTools::get_nhdplus(AOI = bbox_poly)

rivers_inset <- nhd_inset %>%
  filter(
    gnis_name %in% c(
      "La Poudre Pass Creek",
      "South Fork Cache la Poudre River",
      "North Fork Cache La Poudre River",
      "Beaver Creek",
      "Joe Wright Creek",
      "Cache la Poudre River"
    ) | comid == 2899995
  )


p_inset <- ggplot() +
  # layer_dem(df_dem_inset) +
  geom_sf(data = st_read("data/raw/spatial/res_study/cpf_boundary.shp") %>% st_transform(4326), fill = NA, color= "red") +
  layer_baer(df_baer) +
  geom_sf(data = rivers_inset, fill = "#2c68e8", color = "#2c68e8") +
  geom_sf(data = nhd_inset,    fill = "#2c68e8", color = "#2c68e8", alpha = 0.3) +
  layer_water(all_waterbodies) +
  layers_annotations +
  # Study-area bounding boxes
  geom_rect(aes(xmin = lims_main$xlim[1], xmax = lims_main$xlim[2],
                ymin = lims_main$ylim[1], ymax = lims_main$ylim[2]),
            fill = NA, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = lims_como$xlim[1], xmax = lims_como$xlim[2],
                ymin = lims_como$ylim[1], ymax = lims_como$ylim[2]),
            fill = NA, color = "black", linewidth = 1) +
  layer_points(meta, size = 3) + #%>% filter(!site_code %in% c("BEAV", "BMD", "CBRI", "CHD", "COMI", "COMO", "HORI", "HORO", "JOEI", "LNGO", "PTRO")), size = 3) +
  coord_sf(xlim = inset_xlim, ylim = inset_ylim, expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA))

layout_mat <- "
AB
AC
"

p_final <- (p_main + p_inset + p_como) +
  plot_layout(design = layout_mat,
              widths = c(1.3, 1.55),   # right column is wider
              heights = c(1.8, 0.5),   # BIG CLP row, very small COMO row
              guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.spacing.x = unit(0.35, "cm")) &
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    byrow = TRUE,
    order = 1),
    shape = guide_legend(title.position = "top",
                         title.hjust = 0.5,
                         nrow = 1,
                         byrow = TRUE,
                         order = 2,
                         override.aes = list(size = 4)))

p_final
