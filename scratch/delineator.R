
library(tidyverse)
library(sf)
library(nhdplusTools)

sf::sf_use_s2(FALSE)

general_dir <- "data/baer_soil_burn_severity/"

# Download some NHDPlusHR Data
hr_data <- nhdplusTools::download_nhdplushr(general_dir, 1019)
hr_flowlines <- nhdplusTools::get_nhdplushr(hr_data, layers = c("NHDFlowline"))
hr_catchments <- nhdplusTools::get_nhdplushr(hr_data, layers = c("NHDPlusCatchment"))

# Change to your directory
dir <- "data/baer_soil_burn_severity/cpf_sites_sbs.shp"

#read in sites
sites <- sf::st_read(dir) %>%
  #dplyr::filter(grepl("Reservoir|Inflow", Site_Nm)) %>%
  #filter(site_cd=="PBD") %>%
  sf::st_transform(4269)

start_index <- get_flowline_index(hr_flowlines$NHDFlowline,
                                  sites,
                                  search_radius = 10)

watersheds <-  map(start_index$COMID, function(x){
  
  ids <- get_UT(hr_flowlines$NHDFlowline, x)
  
  ws <- hr_catchments[[1]] %>%
    filter(FEATUREID %in% ids) %>%
    filter(FEATUREID != 23001900058842) %>%
    summarize() %>%
    nngeo::st_remove_holes() %>%
    sf::st_transform(26913)
  
  return(ws)
})

mapview::mapview(watersheds)


