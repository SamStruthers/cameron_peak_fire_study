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
  "Very Low" = "#037d75",
  "Low"      = "#66CDCD",
  "Moderate" = "#FFE820",
  "High"     = "#FF0000"
)

layer_baer <- function(df_baer) {
  list(
    scale_fill_manual(values   = baer_colors,
                      na.value = "transparent",
                      name     = "Soil Burn Severity",
                      drop     = FALSE),
    geom_sf(data = cpf_fire, fill = NA, color = "black", linewidth = 1),
    geom_raster(data = df_baer, aes(x = x, y = y, fill = burn_class))
  )
}

layer_points <- function(data, size = 6) {
  list(
    geom_sf(data = data %>% mutate(`Sampling Site` = case_when(location_type == "Reservoir" ~  "Reservoir",
                                                               site_code == "PBD" ~ "Canyon Mouth", 
                                                               TRUE ~ "Stream")),
            aes(shape = `Sampling Site`, size = `Sampling Site`),
            fill = "black",
            color = "white",
            stroke = 0.6),
    scale_shape_manual(values = c("Reservoir" = 21, "Stream" = 24, "Canyon Mouth" = 23),
                       guide = "none"), 
    scale_size_manual(values = c("Reservoir" = size, "Stream" = size, "Canyon Mouth" = size + 1),
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

pbd_tb_ws <- st_read(here("data/raw/spatial/res_study/cpf_watersheds.shp"))%>%
  filter(site_code == "PBD")

flowlines_tb <- nhdplusTools::get_nhdplus(AOI = pbd_tb_ws)

ggplot() +
  geom_sf(data = pbd_tb_ws, fill = "lightblue", color = "blue") +
  geom_sf(data = flowlines_tb, fill = "lightblue", color = "blue") +
  geom_sf(data = cpf_fire, fill = NA, color = "black", linewidth = 1) +
  theme_void()

meta <- read_csv(here("data/raw/chem/ross_clp_chem/v2025.11.14/data/metadata/location_metadata.csv")) %>%
  filter(site_code %in% c("BEAV", "BMD", "BRNR", "CBRI", "CBRR", "CHD", "COMI", "COMO", "COMR",
                          "HORI", "HORO", "HORR", "JOEI", "JOER", "JWC", "LNGO", "LNGR", 
                          "PJW", "PNF", "PSF", "PTRO", "PTRR", "SFM", "PBD", "PBR", "PFAL")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

baer_raw <- terra::rast(here("data/raw/spatial/res_study/cameron_peak_co4060910587920200813_sbs/cameron_peak_co4060910587920200813_sbs.tif"))

target_crs <- sf::st_crs(4326)$wkt

df_baer <- terra::project(baer_raw, target_crs) %>%
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
  layer_water(flowlines_tb) +
  layers_annotation_simple +
  layer_points(meta, size = 3) +
  coord_sf(xlim = lims_main$xlim, ylim = lims_main$ylim, expand = FALSE) +
  theme_map() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA))


#Joe Wright Creek Only


waterbodies_main <- all_waterbodies %>% filter(!site_code %in% c("HORR", "COMR", "LNGR", "PTRR"))
jwc_sites <- meta %>%
  filter(site_code %in% c("JOEI", "JOER", "CBRI","CBRR", "CHD", "JWC")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)
lims_main        <- padded_limits(jwc_sites)
flowlines_tb_jwc <- flowlines_tb%>%
  filter(gnis_name %in% c("Joe Wright Creek"))

ggplot() +
  # layer_dem(df_dem_main) +
  layer_baer(df_baer) +
  geom_sf(data = flowlines_tb_jwc, color = "#002EA3", linewidth = 2.5) +
  #geom_sf(data = flowlines_tb_jwc, color = "#002EA3", linewidth = 1) +
  
  layer_water(waterbodies_main) +
  layer_water(flowlines %>% filter(gnis_nm != "Joe Wright Creek")) +
  layers_annotation_simple +
  layer_points(jwc_sites, size = 6) +
  coord_sf(xlim = lims_main$xlim, ylim = lims_main$ylim, expand = FALSE) +
  theme_map() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA), 
        legend.position = "none")
ggsave(here("docs/cpf_reservoir/paper_draft/presentation/jwc_map.png"), width = 6, height = 10, dpi = 300)
#"#002EA3" "#E70870" "#256BF5" "#745CFB" "#1E4D2B" "#56104E"


# Comanche hourglass complex

waterbodies_como <- all_waterbodies %>% filter(site_code %in% c("COMR", "HORR"))
sites_como       <- meta %>%
  filter(site_code %in% c("BEAV")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

lims_como <- padded_limits(st_union(waterbodies_como, sites_como))

nhd_como <- nhdplusTools::get_nhdphr(
  ids  = c("23001900089883", "23001900206218", "23001900000008"),
  type = "networknhdflowline"
)

p_como <- ggplot() +
  # layer_dem(df_dem_como) +
  layer_baer(df_baer) +
  geom_sf(data = cpf_fire, fill = NA, color = "black", linewidth = 0.5) +
  layer_water(waterbodies_como) +
  layer_water(nhd_como) +
  layer_water(flowlines_tb) +
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

# delinate the watershed from PBD upwards 
outlet <- meta %>%
  filter(site_code == "PBD")

# Snap to nearest flowline and get COMID
start_comid <- discover_nhdplus_id(outlet)

# Then delineate
basin <- get_nldi_basin(nldi_feature = list(
  featureSource = "comid",
  featureID     = as.character(start_comid), 
  simplify = F
))
# get me the difference between pbd_tb_ws and the basin to get the upper portion of the watershed that isn't in the pbd_tb_ws layer
trans_basin <- st_transform(basin, st_crs(3857))
trans_pbd_tb <- st_transform(pbd_tb_ws, st_crs(3857))

basin_valid    <- st_make_valid(st_snap(trans_basin, trans_pbd_tb, tolerance = 0.01))
pbd_tb_valid   <- st_make_valid(st_snap(trans_pbd_tb, trans_basin, tolerance = 0.01))

clip_tb <- st_difference(
  pbd_tb_valid,
  st_union(basin_valid)
)


ggplot(clip_tb) +
  geom_sf(fill = "lightblue", color = NA) +
  theme_void()
p_inset <- ggplot() +
  # layer_dem(df_dem_inset) +
  geom_sf(data = st_read("data/raw/spatial/res_study/cpf_boundary.shp") %>% st_transform(4326), fill = "red", alpha = 0.2, color= "black") +
  geom_sf(data = clip_tb, fill = "grey",  alpha = 0.4) +
  #layer_baer(df_baer) +
  geom_sf(data = rivers_inset, fill = "#2c68e8", color = "#2c68e8", linewidth = 1) +
  geom_sf(data = flowlines_tb,    fill = "#2c68e8", color = "#2c68e8", alpha = 0.3) +
  layer_water(all_waterbodies) +
  geom_sf(data = basin, fill = NA, color = "darkblue", linewidth = 0.8) +
  layers_annotations +
  # Study-area bounding boxes
  geom_rect(aes(xmin = lims_main$xlim[1], xmax = lims_main$xlim[2],
                ymin = lims_main$ylim[1], ymax = lims_main$ylim[2]),
            fill = NA, color = "black", linewidth = 1) +
  geom_rect(aes(xmin = lims_como$xlim[1], xmax = lims_como$xlim[2],
                ymin = lims_como$ylim[1], ymax = lims_como$ylim[2]),
            fill = NA, color = "black", linewidth = 1) +
  layer_points(meta%>%filter(location_type %in% c("Stream","Mainstem") & site_code != "BEAV"), size = 3) + #%>% filter(!site_code %in% c("BEAV", "BMD", "CBRI", "CHD", "COMI", "COMO", "HORI", "HORO", "JOEI", "LNGO", "PTRO")), size = 3) +
  coord_sf(xlim = inset_xlim, ylim = inset_ylim, expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA))

p_inset
# Regional Map
colorado <- tigris::states(cb = TRUE) %>%
  filter(NAME == "Colorado") %>%
  st_transform(4326)
huc8 <- get_huc(id = "10190007", type = "huc08") #%>% st_transform(3857)
huc8_bbox <- st_bbox(st_buffer(huc8 %>% st_transform(4326), 0.2))
flowlines_reg <- nhdplusTools::get_nhdplus(AOI = huc8)


p_regional <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 7, progress = "none") +
  geom_sf(data = colorado, fill = NA, color = "grey40", linewidth = 0.4) +
  geom_sf(data = clip_tb, fill = "grey",  alpha = 0.6) +
  geom_sf(data = huc8, fill = "#a8c8e8", color = "#2171b5",
          linewidth = 0.5, alpha = 0.6) +
  geom_sf(
    data = flowlines_reg %>%
      st_transform(4326)%>%
      filter(gnis_name %in% c("Cache la Poudre River",
                              "North Fork Cache La Poudre River",
                              "South Fork Cache la Poudre River")),
    color = "#5a9fff", linewidth = 0.8, alpha = 0.95
  ) +
  geom_sf(data = bbox_poly, fill = NA, color = "black",
          linewidth = 1, linetype = "solid") +
  #layers_annotations +
  theme_void() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.5),
    legend.position = "none"
  )


layout_mat <- "
ABB
ABB
ACC
"

# gsave each plot separately to ensure optimal resolution, then combine with patchwork for final export
ggsave(filename = "docs/cpf_reservoir/paper_draft/p_regional.jpg", width = 6, height = 6, dpi = 300,plot =  p_regional)
ggsave(filename = "docs/cpf_reservoir/paper_draft/p_main.jpg", width = 6, height = 6, dpi = 300,plot =  p_main)
ggsave(filename = "docs/cpf_reservoir/paper_draft/p_inset.jpg", width = 8, height = 6, dpi = 300,plot =  p_inset)
ggsave(filename = "docs/cpf_reservoir/paper_draft/p_como.jpg", width = 6, height = 6, dpi = 300,plot = p_como)


p_final <- ( p_main + p_inset + p_como) +
  plot_layout(design = layout_mat,
              widths  = c(0.6, 1.3, 1.55),
              heights = c(1.0, 1.0, 0.5),
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
ggsave(here("docs/cpf_reservoir/paper_draft/cpf_maps.png"), p_final, width = 12, height = 8, dpi = 300)





## Transbasin maps

#load transbasin watersheds


list.files()

m_ditch <- read_sf("data/raw/spatial/res_study/m_ditch.kml") %>%
  st_transform(4326)
l_tunnel <- read_sf("data/raw/spatial/res_study/l_tunnel.kml") %>%
  st_transform(4326)
g_ditch <- read_sf("data/raw/spatial/res_study/g_ditch.kml") %>%
  st_transform(4326)

tb_ws <- read_sf("data/raw/spatial/res_study/trans_basin_watersheds.shp") %>%
  st_transform(4326)

#get any flowlines inside of tb_ws and then clip to fit the watershed boundaries

m_ditch_flowlines <- nhdplusTools::get_nhdplus(AOI = st_union(tb_ws %>% filter(site_name == "Michigan Ditch")))%>%
  st_transform(4326) %>%
  st_crop(st_union(tb_ws %>% filter(site_name == "Michigan Ditch")))

g_ditch_flowlines <- nhdplusTools::get_nhdplus(AOI = st_union(tb_ws %>% filter(site_name == "Grand Ditch")))%>%
  st_transform(4326) %>%
  st_intersection(st_union(tb_ws %>% filter(site_name == "Grand Ditch")))
        
l_tunnel_flowlines <- nhdplusTools::get_nhdplus(AOI = st_union(tb_ws %>% filter(site_name == "Laramie River Tunnel")))%>%
  st_transform(4326) %>%
  st_intersection(st_union(tb_ws %>% filter(site_name == "Laramie River Tunnel")))
g_ditch_flowlines <- nhdplusTools::get_nhdplus(AOI = st_union(tb_ws %>% filter(site_name == "Grand Ditch")))%>%
  st_transform(4326) %>%
  st_intersection(st_union(tb_ws %>% filter(site_name == "Grand Ditch")))
ggplot() +
  geom_sf(data = g_ditch_flowlines)+
  geom_sf(data = tb_ws %>% filter(site_name == "Grand Ditch"), fill = "lightblue", color = "blue", alpha = 0.4)
  

clp_flowlines <- nhdplusTools::get_nhdplus(AOI = st_union(pbd_tb_ws)) %>%
  filter(gnis_name %in% c("Cache la Poudre River",
                          "La Poudre Pass Creek",
                          "Joe Wright Creek"))

#make a bounding box around all elements other than clp flowlines
all_elements <- rbind(
  st_union(m_ditch, m_ditch_flowlines),
  st_union(g_ditch, g_ditch_flowlines),
  st_union(l_tunnel, l_tunnel_flowlines)
)
# 1. Keep the exact numeric bbox limit values
tb_bbox <- st_bbox(all_elements) %>% 
  st_as_sfc() %>% 
  st_transform(3857)%>%
  st_buffer(dist = 5000) %>%  # Adjust this number to change your padding size
  st_bbox()%>%
  st_transform(4326)

tb_bbox <- st_read(here("data/raw/spatial/res_study/tb_bbox.kml")) %>%
  st_transform(4326)%>%
  st_bbox()
# 2. Extract the CRS from the elements you measured
target_crs <- st_crs(all_elements)


library(ggrepel) # Make sure this is loaded

# Create a clean labeling dataframe using the center points of your features
infra_labels <- tibble(
  label = c("Michigan Ditch", "Grand Ditch", "Laramie River Tunnel"),
  geometry = c(st_geometry(m_ditch), st_geometry(g_ditch), st_geometry(l_tunnel))
) %>% 
  st_as_sf() %>% 
  st_point_on_surface() %>% 
  # Extract the geometry coordinates into separate X and Y numeric columns
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  )


# Define your professional hex color palette vectors
tb_colors <- c(
  "Michigan Ditch"          = "#e6a15c",  # Warm Ochre
  "Grand Ditch"             = "#5c8a77",  # Sage Green
  "Laramie River Tunnel"    = "#965a5a"   # Dusty Terracotta
)

hydro_blue <- "#7b9acc" # Consistent, professional stream/waterbody color
infra_dark <- "#2b2b2b" # Clean charcoal for infrastructure vectors

ggplot() +
  # 1. Background Context: HUC8 Boundary (Muted & pulled back)
  geom_sf(data = huc8, fill = "#f0f4f8", color = "#9aa5b1",
          linewidth = 0.4, alpha = 0.7) +
  
  # 2. Key Focus: Transbasin Watersheds
  geom_sf(data = tb_ws, aes(fill = site_name, color = site_name), alpha = 0.6, linewidth = 0.3) +
  scale_color_manual(values = tb_colors) +
  scale_fill_manual(values = tb_colors) +
  
  # 3. Hydro Networks (Unified color scale, no competing bright blues/reds)
  geom_sf(data = clp_flowlines, color = hydro_blue, linewidth = 0.3) +
  geom_sf(data = m_ditch_flowlines, color = hydro_blue, linewidth = 0.4) +
  geom_sf(data = g_ditch_flowlines, color = hydro_blue, linewidth = 0.4) +
  geom_sf(data = l_tunnel_flowlines, color = hydro_blue, linewidth = 0.4) +
  geom_sf(data = waterbodies_main, fill = hydro_blue, color = NA) +
  
  # 4. Infrastructure Features (Delineated in high-contrast charcoal)
  geom_sf(data = m_ditch, color = infra_dark, linewidth = 0.7) +
  geom_sf(data = g_ditch, color = infra_dark, linewidth = 0.7) +
  geom_sf(data = l_tunnel, color = infra_dark, linewidth = 1.2) +
  # geom_label_repel(
  #   data = infra_labels,
  #   aes(x = X, y = Y, label = label),
  #   fontface = "bold",
  #   size = 3.2,
  #   color = "#1f2d3d",
  #   fill = alpha("white", 0.85),
  #   box.padding = 0.5,           
  #   point.padding = 0.2,
  #   segment.color = "#47525d",   
  #   segment.size = 0.4,
  #   force = 3
  # ) +
  # 5. Spatial Constraints & Polish
  coord_sf(
    xlim = c(tb_bbox["xmin"], tb_bbox["xmax"]), 
    ylim = c(tb_bbox["ymin"], tb_bbox["ymax"]), 
    crs = target_crs,
    expand = FALSE
  ) +
  labs(
    title = "Transbasin Diversions in the Cache la Poudre River Watershed", 
    color = "Transbasin Watershed", 
    fill = "Transbasin Watershed"
  ) +
  layers_annotations +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14, color = "#1f2d3d"),
    legend.title = element_text(face = "bold", size = 10, color = "#1f2d3d"),
    legend.position = "right"
  )
ggsave(here("docs/cpf_reservoir/paper_draft/tb_map.png"), width = 10, height = 10, dpi = 300)


### Map for figure 7 mainstem sites
lims_jwc <- padded_limits(meta %>% filter(site_code %in% c("JOEI", "JOER", "CBRI","CBRR", "CHD", "JWC")))

mainstem <- rivers_inset %>% filter(gnis_name %in% c("Cache la Poudre River"))

p_fig_seven <- ggplot() +
  # layer_dem(df_dem_inset) +
  geom_sf(data = st_read("data/raw/spatial/res_study/cpf_boundary.shp") %>% st_transform(4326), fill = "red", alpha = 0.2, color= "black") +
  geom_sf(data = clip_tb, fill = "grey",  alpha = 0.4) +
  #layer_baer(df_baer) +
  geom_sf(data = mainstem, fill = "#2c68e8", color = "#2c68e8", linewidth = 1) +
  geom_sf(data = rivers_inset, fill = "#2c68e8", color = "#2c68e8", linewidth = 0.7) +
  geom_sf(data = flowlines_tb,    fill = "#2c68e8", color = "#2c68e8", alpha = 0.3) +
  layer_water(all_waterbodies) +
  geom_sf(data = basin, fill = NA, color = "darkblue", linewidth = 0.8) +
  layers_annotations +
  # Study-area bounding boxes
  geom_rect(aes(xmin = lims_jwc$xlim[1], xmax = lims_jwc$xlim[2],
                ymin = lims_jwc$ylim[1], ymax = lims_jwc$ylim[2]),
            fill = NA, color = "darkgrey", linewidth = 1) +
  layer_points(meta%>%filter(location_type %in% c("Stream","Mainstem") & site_code != "BEAV"), size = 3) + #%>% filter(!site_code %in% c("BEAV", "BMD", "CBRI", "CHD", "COMI", "COMO", "HORI", "HORO", "JOEI", "LNGO", "PTRO")), size = 3) +
  coord_sf(xlim = inset_xlim, ylim = inset_ylim, expand = FALSE) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.background = element_rect(fill = "white", color = NA))

p_fig_seven
ggsave(filename = "docs/cpf_reservoir/paper_draft/p_fig_seven.jpg", width = 8, height = 6, dpi = 500,plot =  p_fig_seven)

