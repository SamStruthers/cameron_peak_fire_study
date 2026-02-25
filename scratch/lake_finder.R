
install.packages("osmdata")
library(osmdata)


all_clp_lakes <- opq(bbox = "Colorado") %>%
  add_osm_feature(key = 'natural', value = "water") %>%
  osmdata_sf()


poly <- all_clp_lakes[["osm_polygons"]]%>%
  select(name, osm_id)%>%
  filter(name %in% c("Chambers Lake", "Barnes Meadow Reservoir","Joe Wright Reservoir", "Peterson Lake",
                     "Comanche Reservoir", "Hourglass Reservoir",
                     "Seaman Reservoir"))%>%
  filter(!(name == "Comanche Reservoir" & osm_id != 60474057))%>%
  filter(!(name == "Peterson Lake" & osm_id !=55275713))


multipoly <- all_clp_lakes[["osm_multipolygons"]]%>%
  #filter(!is.na(name))%>%
  filter(name %in% c("Chambers Lake", "Barnes Meadow Reservoir", "Long Draw Reservoir", "Joe Wright Reservoir", "Peterson Lake",
                     "Comanche Reservoir", "Hourglass Reservoir",
                     "Seaman Reservoir"))%>%
  select(name, osm_id)

mapview::mapview(poly, zcol = "osm_id")+
  mapview::mapview(multipoly, zcol = "osm_id")

cpf_water_bodies <- bind_rows(poly, multipoly)%>%
  st_transform(4326)%>%
  mutate(site_code = case_when(
    name == "Chambers Lake" ~ "CBRR",
    name == "Barnes Meadow Reservoir" ~ "BRNR",
    name == "Joe Wright Reservoir" ~ "JOER",
    name == "Peterson Lake" ~ "PTRR",
    name == "Comanche Reservoir" ~ "COMR",
    name == "Hourglass Reservoir" ~ "HORR",
    name == "Long Draw Reservoir" ~ "LNGR"))%>%
  select(-osm_id)

mapview::mapview(cpf_water_bodies, zcol = "site_code")


st_write(cpf_water_bodies,"data/spatial/study_water_bodies/study_water_bodies.shp")

check <- st_read("data/spatial/study_water_bodies/study_water_bodies.shp")
mapview::mapview(check)

st_is_valid(check)

waterbody_files <- list.files("data/spatial/all_study_waterbodies/", full.names = T, pattern = ".shp")
